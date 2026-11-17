## 012b  BUILD TIDY FRAME                step 012 (extract), substep b
## Turn the API's per-chunk JSON into the analysis frame: one row per
## filing x statement x line x period, with the amount in ACTUAL DOLLARS.
##
## Input :  extract/extracted.jsonl   (what 012a writes: one JSON object per
##                                     chunk - the extraction plus the chunk's
##                                     own metadata)
## Output:  extract/scf_text_lines.parquet   the frame
##          extract/scf_text_foot.parquet    the footing check, one row per
##                                           filing x statement x period
##          extract/scf_text_lines.csv       a readable copy
##
## WHY THE RESCALING HAPPENS HERE AND NOT IN THE MODEL. The model returns the
## number as printed plus the scale word. Multiplying in R means a misread
## multiplier is a one-line fix instead of a re-run of the whole extraction, and
## the arithmetic is auditable. `value_printed` is always kept beside `amount`.
##
## WHY THE FOOTING CHECK IS HERE. A printed statement foots - the filer typeset
## it and the auditor signed it. So a transcription that foots is almost
## certainly correct, which makes this the free validation that lets a cheap
## model do the extraction. It is also the escalation trigger: send the failures
## to a stronger model, nothing else.

suppressMessages({library(jsonlite); library(dplyr); library(tidyr)
                  library(arrow); library(stringr)})

## Point TIDY_IN / TIDY_OUT at a scratch copy to dry-run the frame on a handful
## of extractions without writing into the real output folder.
SP  <- Sys.getenv("TIDY_OUT",
         paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/extract/"))
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")))
IN  <- Sys.getenv("TIDY_IN", paste0(SP, "extracted.jsonl"))
if (!file.exists(IN)) stop("no ", IN, " - run 012a-extract-statements.py fetch first")
cat("reading", IN, "\n")

## ---- scale words to multipliers -------------------------------------------
## `ones` and a missing scale are NOT the same thing. A statement that states no
## multiplier is usually in whole dollars, but "usually" is not good enough to
## multiply by, so a missing scale yields amount = NA and a flag. Decide what to
## do with those as a group rather than silently assuming.
MULT <- c(ones = 1, thousands = 1e3, millions = 1e6, billions = 1e9)

STMT <- c(cash_flow = "SCF", income_statement = "IS", balance_sheet = "BS")

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- read ------------------------------------------------------------------
raw <- lapply(readLines(IN, warn = FALSE), function(l) {
  if (!nzchar(trimws(l))) return(NULL)
  tryCatch(fromJSON(l, simplifyVector = FALSE), error = function(e) NULL)
})
raw <- raw[!vapply(raw, is.null, logical(1))]
cat("chunks read:", length(raw), "\n")

## 012a may nest the model's answer under `result`; accept either shape.
pick <- function(r, k) {
  if (!is.null(r[[k]])) return(r[[k]])
  if (!is.null(r$result) && !is.null(r$result[[k]])) return(r$result[[k]])
  NULL
}
meta <- function(r, k) { v <- r[[k]]; if (is.null(v)) NA else v[[1]] }

## ---- expand one chunk to long ---------------------------------------------
one <- function(r) {
  if (!isTRUE(pick(r, "found"))) return(NULL)
  rows <- pick(r, "rows"); cols <- pick(r, "columns")
  if (!length(rows) || !length(cols)) return(NULL)
  nc <- length(cols)

  sc  <- pick(r, "scale")
  scw <- if (is.null(sc)) NA_character_ else as.character(sc)
  m   <- if (is.na(scw)) NA_real_ else unname(MULT[scw])

  ch <- vapply(cols, function(k) as.character(k$h %||% ""), character(1))
  ce <- vapply(cols, function(k) if (is.null(k$end)) NA_character_
                                 else as.character(k$end), character(1))
  cm <- vapply(cols, function(k) if (is.null(k$months)) NA_integer_
                                 else as.integer(k$months), integer(1))

  out <- vector("list", length(rows))
  for (i in seq_along(rows)) {
    rr <- rows[[i]]
    v  <- rr$v
    ## A row whose v is the wrong length is a transcription fault, not something
    ## to paper over: pad/truncate so the frame stays rectangular, and flag it.
    bad <- length(v) != nc
    if (length(v) < nc) v <- c(v, vector("list", nc - length(v)))
    if (length(v) > nc) v <- v[seq_len(nc)]
    vals <- vapply(v, function(z) if (is.null(z)) NA_real_ else as.numeric(z),
                   numeric(1))
    ## per-share / share-count / ratio rows are printed unscaled already
    noscale <- isTRUE(rr$x)
    out[[i]] <- data.frame(
      line_order    = as.integer(rr$n %||% i),
      line_label    = as.character(rr$l %||% ""),
      col_order     = seq_len(nc),
      col_heading   = ch,
      period_end    = ce,
      period_months = cm,
      value_printed = vals,
      unscaled_row  = noscale,
      len_mismatch  = bad,
      stringsAsFactors = FALSE)
  }
  d <- bind_rows(out)
  d$scale  <- scw
  d$mult   <- m
  d$amount <- ifelse(d$unscaled_row, d$value_printed, d$value_printed * m)

  st <- as.character(pick(r, "statement") %||% NA)
  d$statement   <- unname(STMT[st])
  d$stmt_raw    <- st
  d$wrdsfname   <- meta(r, "wrdsfname")
  d$cik         <- meta(r, "cik")
  d$coname      <- meta(r, "coname")
  d$form        <- meta(r, "form")
  d$fdate       <- meta(r, "fdate")
  d$rdate       <- meta(r, "rdate")
  d
}

`%||%` <- function(a, b) if (is.null(a)) b else a

L <- bind_rows(lapply(raw, one))
if (!nrow(L))
  stop("no usable extractions - every record had found=FALSE or no rows. ",
       "Check that the input really is 012a's output and not the bare ",
       "model answers without their chunk metadata.")
cat("long rows:", nrow(L), " filings:", length(unique(L$wrdsfname)),
    "  (records with found=FALSE are excluded by design)\n")

L <- L %>% select(wrdsfname, cik, coname, form, fdate, rdate,
                  statement, stmt_raw, line_order, line_label,
                  col_order, col_heading, period_end, period_months,
                  value_printed, scale, mult, unscaled_row, amount,
                  len_mismatch) %>%
  arrange(wrdsfname, statement, line_order, col_order)

## ---- what the frame is missing, stated rather than buried ------------------
cat("\n=== completeness ===\n")
cat(sprintf("scale missing      : %6d rows (%.1f%%) across %d filings\n",
            sum(is.na(L$scale)), 100 * mean(is.na(L$scale)),
            length(unique(L$wrdsfname[is.na(L$scale)]))))
cat(sprintf("period_end missing : %6d rows (%.1f%%)\n",
            sum(is.na(L$period_end)), 100 * mean(is.na(L$period_end))))
cat(sprintf("v-length mismatch  : %6d rows across %d filings\n",
            sum(L$len_mismatch), length(unique(L$wrdsfname[L$len_mismatch]))))
cat(sprintf("amount NA but value present: %6d rows (scale unknown)\n",
            sum(is.na(L$amount) & !is.na(L$value_printed))))

## ---- footing check ---------------------------------------------------------
## Only the cash flow statement has an arithmetic identity this clean:
##   operating + investing + financing + fx = net change in cash
##   beginning + net change = end
## Label wordings vary a lot, so the patterns are deliberately loose and the
## LAST match in a section is taken - the subtotal, not the section header.
PAT <- c(
  op  = "net cash.{0,40}operat|cash (provided|used).{0,30}operat",
  inv = "net cash.{0,40}invest|cash (provided|used).{0,30}invest",
  fin = "net cash.{0,40}financ|cash (provided|used).{0,30}financ",
  fx  = "effect of (exchange|foreign|currency)",
  net = "(net |)(increase|decrease|change).{0,40}in cash",
  beg = "cash.{0,50}beginning",
  end = "cash.{0,50}end of")

grab <- function(lab, val, p) {
  i <- grep(p, tolower(lab), perl = TRUE)
  if (!length(i)) return(NA_real_)
  val[i[length(i)]]
}

foot <- L %>%
  filter(statement == "SCF") %>%
  group_by(wrdsfname, cik, coname, form, rdate, col_order, period_end,
           period_months, mult) %>%
  summarise(
    op  = grab(line_label, amount, PAT[["op"]]),
    inv = grab(line_label, amount, PAT[["inv"]]),
    fin = grab(line_label, amount, PAT[["fin"]]),
    fx  = grab(line_label, amount, PAT[["fx"]]),
    net = grab(line_label, amount, PAT[["net"]]),
    beg = grab(line_label, amount, PAT[["beg"]]),
    end = grab(line_label, amount, PAT[["end"]]),
    n_lines = n(), .groups = "drop") %>%
  mutate(
    sum_sections = rowSums(cbind(op, inv, fin, coalesce(fx, 0)), na.rm = FALSE),
    diff_net     = sum_sections - net,
    diff_roll    = (beg + net) - end,
    ## A tolerance, because a filer computes a subtotal on exact figures and
    ## then rounds, so rounded components need not sum to the rounded subtotal.
    ## ONE PRINTED UNIT is the natural tolerance - $1 for a statement in ones,
    ## but $1,000,000 for one in millions. A flat $1 would fail most of the
    ## large filers for nothing.
    tol          = coalesce(mult, 1),
    foots_net    = !is.na(diff_net)  & abs(diff_net)  <= tol,
    foots_roll   = !is.na(diff_roll) & abs(diff_roll) <= tol,
    testable_net = !is.na(sum_sections) & !is.na(net),
    verdict      = case_when(
      !testable_net                 ~ "not testable",
      foots_net & foots_roll        ~ "foots",
      foots_net                     ~ "sections foot, roll-forward does not",
      TRUE                          ~ "FAILS"))

cat("\n=== cash flow footing, by period-column ===\n")
print(table(foot$verdict))
cat(sprintf("\nescalation rate (FAILS or not testable): %.1f%% of %d columns\n",
            100 * mean(foot$verdict != "foots"), nrow(foot)))

esc <- foot %>% filter(verdict != "foots") %>%
  distinct(wrdsfname) %>% pull(wrdsfname)
cat(sprintf("filings to escalate to a stronger model: %d of %d (%.1f%%)\n",
            length(esc), length(unique(foot$wrdsfname)),
            100 * length(esc) / max(1, length(unique(foot$wrdsfname)))))

## ---- write -----------------------------------------------------------------
write_parquet(L,    paste0(SP, "scf_text_lines.parquet"))
write_parquet(foot, paste0(SP, "scf_text_foot.parquet"))
write.csv(L, paste0(SP, "scf_text_lines.csv"), row.names = FALSE, na = "")
writeLines(esc, paste0(SP, "escalate.txt"))

cat("\nwrote scf_text_lines.parquet (", nrow(L), " rows ), ",
    "scf_text_foot.parquet (", nrow(foot), " rows ), escalate.txt\n", sep = "")
