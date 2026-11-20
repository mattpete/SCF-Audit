## 001h  BUILD TIDY FRAME                step 001, substep h
## Turn the API's per-chunk JSON into the analysis frame: one row per
## filing x statement x line x period, with the amount in ACTUAL DOLLARS.
##
## Input :  extract/extracted.jsonl   (what legacy/extract-statements writes: one JSON object per
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
if (!file.exists(IN)) stop("no ", IN, " - run legacy/extract-statements.py fetch first")
cat("reading", IN, "\n")

## ---- scale words to multipliers -------------------------------------------
## `ones` and a missing scale are NOT the same thing. A statement that states no
## multiplier is usually in whole dollars, but "usually" is not good enough to
## multiply by, so a missing scale yields amount = NA and a flag. Decide what to
## do with those as a group rather than silently assuming.
MULT <- c(ones = 1, thousands = 1e3, millions = 1e6, billions = 1e9)
## A STATEMENT THAT NAMES NO MULTIPLIER IS IN WHOLE DOLLARS.
## The comment above says a missing scale yields NA and a flag, pending a
## decision. Decided 2026-11-19 (Matt), after confirming it against CH Real
## Estate II's 10-Q, which states no multiplier and prints cash of $32,677.
## Four of the ten filings in the first in-harness run name no multiplier, all
## of them small filers, and leaving amount NA drops exactly those from any
## analysis. The model still reports what the page says, so `scale` stays NA
## and `scale_assumed_ones` records where this rule was applied - the decision
## is in the code, visible and reversible, not buried in the extraction.
MULT_DEFAULT <- 1

STMT <- c(cash_flow = "SCF", income_statement = "IS", balance_sheet = "BS")

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- read ------------------------------------------------------------------
raw <- lapply(readLines(IN, warn = FALSE), function(l) {
  if (!nzchar(trimws(l))) return(NULL)
  tryCatch(fromJSON(l, simplifyVector = FALSE), error = function(e) NULL)
})
raw <- raw[!vapply(raw, is.null, logical(1))]
cat("chunks read:", length(raw), "\n")

## legacy/extract-statements may nest the model's answer under `result`; accept either shape.
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
  m   <- if (is.na(scw)) MULT_DEFAULT else unname(MULT[scw])
  assumed_ones <- is.na(scw)

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
    ## `u` - the row printed fewer numbers than there are columns, so the model
    ## filled v from the left and the placement is unverified. Resolved below
    ## by footing, never by the model. See resolve_unplaced().
    unres <- isTRUE(rr$u)
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
      col_unplaced  = unres,
      stringsAsFactors = FALSE)
  }
  d <- bind_rows(out)
  d$scale  <- scw
  d$mult   <- m
  d$scale_assumed_ones <- assumed_ones
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

## ---- PLACE THE UNPLACED VALUES BY FOOTING ---------------------------------
## In the cell-per-line layout a blank cell prints nothing, so a row whose
## figure appears in only one of two columns arrives as a label and a single
## number with nothing on the page saying which column it is. Dover Downs'
## 10-Q financing section is the case:
##
##     Repurchase of common stock            (35,042
##     Proceeds from stock options exercised     349
##     Other                                     (21
##
## Three rows, two columns, one number each. The first two are 2006 and the
## third is 2005, and the only evidence for that is the section's own
## subtotal: 147,325 - 115,300 - 2,591 - 35,042 + 349 + 50 = -5,209, which is
## what the statement prints, and the 2005 column only reaches -7,190 if the
## (21) goes there.
##
## The model is forbidden to do that sum - it marks the row `u` and fills from
## the left. The arithmetic happens here, where it can be read and checked.
## For every assignment of the printed values to columns that keeps their
## left-to-right order, the section is footed against its own subtotal; if
## exactly one assignment foots, it is taken. If none or several do, the
## values stay where the model put them and the rows stay flagged, because a
## guess that cannot be checked is worse than a flag that can.
## THE SUBTOTALS EACH STATEMENT FOOTS TO.
##
## Cash flow and balance sheet both work the same way - the lines above a
## subtotal SUM to it, with the printed sign being the arithmetic sign. So the
## same resolver serves both; only the labels differ.
##
## THE INCOME STATEMENT NEEDS A SIGN FOR EVERY LINE AS WELL. Its expenses are
## printed POSITIVE and subtracted - Biomet prints net sales 2,698.0, cost of
## sales 819.9 and gross profit 1,878.1 - so the components do not sum to the
## subtotal, they NET to it, and the page does not say which lines are added.
## But each line is either added or subtracted, so the sign is one bit per
## line and can be solved for alongside the placement (Matt, 2026-11-17).
##
## The protection is the same in all three cases and it is the uniqueness
## requirement, not the statement type: a placement is taken only when exactly
## one combination foots every column. Searching signs as well means more
## combinations and so more ties, and a tie leaves the row flagged. That is the
## intended behaviour - the resolver declines more often on income statements
## rather than guessing more often.
## A SUBTOTAL PATTERN MUST BE ANCHORED, AND MUST ALLOW PUNCTUATION.
## Both of these were found the first time the resolver ran on real filings
## rather than on hand-built tests, which used tidy labels (Matt, 2026-11-19):
##
##  1. unanchored, "net cash ... provided ... operating" also matches
##     "Adjustments to reconcile net earnings to net cash provided by
##     operating activities:" - a COMPONENT row, and boilerplate in nearly
##     every indirect-method statement. The section walk then treated that row
##     as the subtotal, so the operating section was three rows long with a
##     null subtotal and the resolver bailed out silently.
##
##  2. "provided|used" had to follow "net cash " immediately, so Bristol-Myers'
##     "Net Cash (Used in)/Provided by Investing Activities" matched nothing
##     and its investing and financing sections were never resolvable.
##
## Anchoring to ^net cash fixes the first; allowing parentheses, commas and
## slashes in between fixes the second. On Bristol-Myers this takes the
## resolver from 0 of 18 cells placed to 18 of 18, all correct.
SUBTOT <- list(
  SCF = c(op  = "^net cash (flows? )?[a-z (),/]*(provided|used|from)[a-z (),/]*operat",
          inv = "^net cash (flows? )?[a-z (),/]*(provided|used|from)[a-z (),/]*invest",
          fin = "^net cash (flows? )?[a-z (),/]*(provided|used|from)[a-z (),/]*financ"),
  ## Each balance-sheet subtotal closes the run of lines above it, exactly as a
  ## cash flow section subtotal does. Ordered as printed so the section walk
  ## assigns each line to the first subtotal at or after it.
  BS  = c(tca = "^total current assets",
          ta  = "^total assets",
          tcl = "^total current liabilities",
          tl  = "^total liabilities$|^total liabilities[^a]",
          teq = "^total (share|stock)holders.? (equity|deficit)|^total equity",
          tle = "^total liabilities and"),
  IS  = c(trev = "^(total|net) (revenues?|sales)",
          gp   = "^gross (profit|margin)",
          toe  = "^total (operating |costs? and )?(costs?|expenses?)",
          oi   = "^(total )?operating (income|loss|profit)",
          pbt  = "^(income|loss|earnings).{0,30}before.{0,20}tax",
          ni   = "^net (income|loss|earnings)"))
## Which statements need a sign solved per line as well as a placement.
SIGNED <- c(SCF = FALSE, BS = FALSE, IS = TRUE)
FOOT_TOL <- 1       # printed units, so $1k in a thousands statement
## Refuse to search beyond this many combinations. Signs double the space per
## component line, so an income-statement block of more than about ten lines
## is declined rather than searched - and a block that big is also the one
## where a tie is most likely anyway.
MAX_COMB <- 20000

resolve_unplaced <- function(d, stmt) {
  if (!any(d$col_unplaced)) return(d)
  SB <- SUBTOT[[stmt]]
  if (is.null(SB)) return(d)
  nc <- max(d$col_order)
  if (nc < 2) return(d)
  w <- d[order(d$line_order, d$col_order), ]
  ord <- unique(w$line_order)
  lab <- tolower(trimws(w$line_label[match(ord, w$line_order)]))
  ## section boundaries: a subtotal closes its own section, so each line takes
  ## the first subtotal at or after it. Same rule as 001a uses on the XBRL side.
  sub <- rep(NA_character_, length(ord))
  for (k in names(SB)) sub[grepl(SB[[k]], lab)] <- k
  sec <- rep(NA_character_, length(ord))
  nxt <- NA_character_
  for (i in rev(seq_along(ord))) {
    if (!is.na(sub[i])) nxt <- sub[i]
    sec[i] <- nxt
  }
  for (k in names(SB)) {
    si <- which(sec == k & is.na(sub))                     # components
    ti <- which(sec == k & !is.na(sub))                     # the subtotal
    if (!length(ti) || !length(si)) next
    tot <- w$value_printed[w$line_order == ord[ti[1]]]
    if (any(is.na(tot))) next
    amb <- si[vapply(si, function(i)
      any(w$col_unplaced[w$line_order == ord[i]]), logical(1))]
    if (!length(amb)) next
    fix <- setdiff(si, amb)
    ## Where the subtotal is a running one rather than a sum of the block, the
    ## block starts from the PREVIOUS subtotal, not from zero. An income
    ## statement's "Operating income" follows "Gross profit"; a balance sheet's
    ## "Total assets" follows "Total current assets". Taking the nearest
    ## subtotal above the block as the opening balance covers both, and for a
    ## block with nothing above it the opening balance is zero.
    pv <- rep(0, nc)
    above <- which(!is.na(sub) & seq_along(ord) < min(si))
    if (isTRUE(SIGNED[[stmt]]) && length(above)) {
      p <- w$value_printed[w$line_order == ord[max(above)]]
      if (!any(is.na(p))) pv <- p
    }
    sgn <- isTRUE(SIGNED[[stmt]])
    ## FIXED lines: known values, unknown signs only where the statement nets.
    fv <- lapply(fix, function(i) {
      v <- w$value_printed[w$line_order == ord[i]]
      ifelse(is.na(v), 0, v)
    })
    ## AMBIGUOUS lines: the printed values, to be placed in some increasing
    ## choice of columns, with a sign each where the statement nets.
    vals <- lapply(amb, function(i) {
      v <- w$value_printed[w$line_order == ord[i]]
      v[!is.na(v)]
    })
    opts <- lapply(vals, function(v) combn(nc, length(v), simplify = FALSE))

    ## ---- STEP 1: PIN THE SIGNS ON THE CLEAN COLUMNS --------------------
    ## A line's sign is the same in every column, and most columns have no
    ## missing value at all - Biomet's four-column income statement has two
    ## clean ones. Those columns determine the signs on their own:
    ##
    ##   col 1:  1,878.1 - 1,042.3 - 106.6 - 372.6 = 356.6
    ##   col 4:    146.5 -   194.2 -  34.0 -   0.5 = -82.2
    ##
    ## and no other sign pattern satisfies both. So the signs come out of the
    ## clean columns rather than out of a search over the whole block, which
    ## is what made the brute-force cap bind (Matt, 2026-11-17).
    ##
    ## The convention is tried first and almost always holds: within a block
    ## closed by a running subtotal the components SUBTRACT, and within one
    ## closed by a "total X" they ADD. Only if neither verifies does this fall
    ## back to enumerating sign patterns, and only then does the cap apply.
    ## Which columns are clean is not known in advance - any column could be
    ## the one a missing value belongs to. But a clean column is exactly a
    ## column the FIXED lines already foot, so the right sign vector is the one
    ## that foots the most columns on the fixed lines alone, and the columns it
    ## misses are the ones awaiting a missing value.
    sv_for <- function(sv) {
      b <- pv
      for (q in seq_along(fix)) b <- b + sv[q] * fv[[q]]
      b
    }
    nf <- length(fix)
    cand <- if (!sgn) list(rep(1, nf)) else {
      cc <- list(rep(-1, nf), rep(1, nf))        # the convention, both ways
      ok <- vapply(cc, function(sv) sum(abs(sv_for(sv) - tot) <= FOOT_TOL) > 0, logical(1))
      if (any(ok)) cc[ok] else {
        if (2^nf > MAX_COMB) list() else
          lapply(seq_len(2^nf) - 1, function(z)
            ifelse(bitwAnd(bitwShiftR(z, seq_len(nf) - 1), 1L) == 1L, 1, -1))
      }
    }
    if (!length(cand)) next
    ## Narrow to the sign vectors that foot the most columns on the fixed lines
    ## - but only where signs were actually unknown, and only when at least one
    ## column IS clean. Dover Downs' financing section is the counter-example:
    ## both of its columns are waiting on a missing value, so no column foots on
    ## the fixed lines and there is nothing to narrow. Bailing out there took
    ## the cash flow statement from 100% resolved to 0%.
    if (sgn && length(cand) > 1) {
      nfoot <- vapply(cand, function(sv)
        sum(abs(sv_for(sv) - tot) <= FOOT_TOL), integer(1))
      if (max(nfoot) > 0) cand <- cand[nfoot == max(nfoot)]
    }

    ## ---- STEP 2: CLOSE THE GAP IN EVERY OTHER COLUMN -------------------
    ## With the signs fixed, each column has one number missing from it and the
    ## gap says what that number must be. No search over placements.
    pgrid <- expand.grid(lapply(opts, seq_along), KEEP.OUT.ATTRS = FALSE)
    hit <- NULL
    for (sv0 in cand) {
      sv <- c(sv0, rep(sv0[1], length(amb)))     # ambiguous rows share the sign
      if (!sgn) sv <- rep(1, nf + length(amb))
      b <- sv_for(sv0)
      for (g in seq_len(nrow(pgrid))) {
        s <- b
        for (j in seq_along(amb)) {
          cc2 <- opts[[j]][[pgrid[g, j]]]
          s[cc2] <- s[cc2] + sv[nf + j] * vals[[j]]
        }
        if (all(abs(s - tot) <= FOOT_TOL)) {
          if (!is.null(hit)) { hit <- NA; break }            # a tie: give up
          hit <- list(g = g)
        }
      }
      if (length(hit) == 1 && is.na(hit[[1]])) break
    }
    if (is.null(hit) || (length(hit) == 1 && is.na(hit[[1]]))) next
    g <- hit$g
    for (j in seq_along(amb)) {
      cc <- opts[[j]][[pgrid[g, j]]]
      nv <- rep(NA_real_, nc); nv[cc] <- vals[[j]]
      sel <- d$line_order == ord[amb[j]]
      d$value_printed[sel][order(d$col_order[sel])] <- nv
      d$col_placed_by_footing[sel] <- TRUE
      d$col_unplaced[sel] <- FALSE
    }
  }
  d
}

L <- bind_rows(lapply(raw, one))
if (!nrow(L))
  stop("no usable extractions - every record had found=FALSE or no rows. ",
       "Check that the input really is legacy/extract-statements's output and not the bare ",
       "model answers without their chunk metadata.")
cat("long rows:", nrow(L), " filings:", length(unique(L$wrdsfname)),
    "  (records with found=FALSE are excluded by design)\n")

## ---- resolve the unplaced values, then rescale ----------------------------
## Runs before `amount` is used for anything, because moving a value between
## columns changes which period it belongs to.
if (!"col_unplaced" %in% names(L)) L$col_unplaced <- FALSE
L$col_unplaced[is.na(L$col_unplaced)] <- FALSE
L$col_placed_by_footing <- FALSE
n_amb0 <- sum(L$col_unplaced)
if (n_amb0) {
  L <- bind_rows(lapply(split(L, paste(L$wrdsfname, L$statement)), function(d) {
    st <- unique(d$statement)
    if (!any(d$col_unplaced) || length(st) != 1 || !st %in% names(SUBTOT)) return(d)
    tryCatch(resolve_unplaced(d, st), error = function(e) d)
  }))
  ## amount is derived from value_printed, so recompute it after the move
  L$amount <- ifelse(L$unscaled_row, L$value_printed, L$value_printed * L$mult)
}
cat(sprintf("unplaced cells: %d  resolved by footing: %d (%.1f%%)  still flagged: %d\n",
            n_amb0, sum(L$col_placed_by_footing),
            if (n_amb0) 100 * sum(L$col_placed_by_footing) / n_amb0 else 0,
            sum(L$col_unplaced)))

L <- L %>% select(wrdsfname, cik, coname, form, fdate, rdate,
                  statement, stmt_raw, line_order, line_label,
                  col_order, col_heading, period_end, period_months,
                  value_printed, scale, mult, scale_assumed_ones,
                  unscaled_row, amount,
                  col_unplaced, col_placed_by_footing,
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
    ## THE CHECK RUNS ON THE PRINTED FIGURES, NOT ON SCALED DOLLARS.
    ## Footing is scale-invariant: if the printed numbers add up, so do the
    ## dollars. Running it on `amount` meant that a filing stating no
    ## multiplier - where amount is NA by design - could not be tested at all,
    ## and those are exactly the small filers whose statements most need
    ## checking. Three of the ten filings in the first in-harness run were
    ## lost this way, 6 of 18 period-columns (Matt, 2026-11-19).
    ##
    ## The dollar values are kept beside them, so a reader still sees the
    ## section totals in the units the rest of the frame uses.
    op  = grab(line_label, value_printed, PAT[["op"]]),
    inv = grab(line_label, value_printed, PAT[["inv"]]),
    fin = grab(line_label, value_printed, PAT[["fin"]]),
    fx  = grab(line_label, value_printed, PAT[["fx"]]),
    net = grab(line_label, value_printed, PAT[["net"]]),
    beg = grab(line_label, value_printed, PAT[["beg"]]),
    end = grab(line_label, value_printed, PAT[["end"]]),
    op_usd  = grab(line_label, amount, PAT[["op"]]),
    inv_usd = grab(line_label, amount, PAT[["inv"]]),
    fin_usd = grab(line_label, amount, PAT[["fin"]]),
    n_lines = n(), .groups = "drop") %>%
  mutate(
    sum_sections = rowSums(cbind(op, inv, fin, coalesce(fx, 0)), na.rm = FALSE),
    diff_net     = sum_sections - net,
    diff_roll    = (beg + net) - end,
    ## A tolerance, because a filer computes a subtotal on exact figures and
    ## then rounds, so rounded components need not sum to the rounded
    ## subtotal. On PRINTED figures that tolerance is one printed unit,
    ## whatever the statement's scale happens to be.
    tol          = 1,
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
