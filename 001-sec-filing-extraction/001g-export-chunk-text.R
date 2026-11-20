## ============================================================================
## 001g  EXPORT EVERY CANDIDATE, FLAGGED       step 001, substep g
## The counterpart to legacy/export-chunk-text-approach-a for approach B. Instead of sending the one block the
## locator chose, send every block that passed the structure test and let the
## model decide which belongs to the company on this filing row.
## In : cache/chunks_anch.rds (001d)
## Out: extract/candidates.jsonl, cache/candtext.rds
## ============================================================================
## WHY SEND SEVERAL (Matt, 2026-11-17). A combined 10-K holds two or more
## companies' statements, and WRDS indexes the same document once per
## registrant, so one accession is drawn as several filing rows and each row
## wants ITS OWN company's numbers:
##
##   000010/100517/0001193125-15-056493.txt  United Continental Holdings  NI 1,132
##   000031/319687/0001193125-15-056493.txt  UNITED AIRLINES, INC.        NI 1,114
##
## Measured on wrds_forms, 10-K/10-Q since 2002: 36,008 of 732,299 filing rows
## (4.92%) are combined filings, and one accession can carry up to 12
## registrants. This is the one error class whose extracted numbers foot
## perfectly while belonging to the wrong company, so it cannot be left to a
## ranking rule that gets it right most of the time.
##
## Cost of sending all of them rather than one: 1.2x the input tokens (median
## 358 lines per filing against 308). Most slots are not ambiguous at all -
## 976 of 1,117 filing x statement slots have exactly one passing candidate.
##
## The model is given, per candidate: the registrant name printed above the
## block, how many other cash flow statements sit between it and the anchor,
## and the company name and CIK on this filing row. The first two are evidence
## about which entity the block belongs to; the third is the answer it has to
## match. Nothing is discarded here - 001d ranks, this ships the whole list.
suppressMessages({library(DBI); library(RPostgres); library(keyring)
                  library(ssh); library(jsonlite)})
## PILOT names the run's cache directory, as in 001b/c/f/h/i. ONLY_N pulls the
## text for a random N filings instead of all of them, which is what a review
## sample needs - the full pull is a long remote job for text nobody will read.
PILOT  <- Sys.getenv("PILOT", "pilot-2026-11")
ONLY_N <- as.integer(Sys.getenv("ONLY_N", "0"))
SP  <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT, "/cache/")
OUT <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT, "/extract/")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")))

## Which locator's candidates to ship. "anch" = 001d (approach B),
## "hdr" = 001f (approach C, cut at page headers).
IN  <- Sys.getenv("IN_CHUNKS", "chunks_hdr.rds")
C <- readRDS(paste0(SP, IN))
cat("reading", IN, "
")
for (cl in c("reg_name","cf_titles_between","sec_lo","sec_hi","form","set","miss"))
  if (!cl %in% names(C)) C[[cl]] <- NA
cat("candidates:", nrow(C), " filings:", length(unique(C$wrdsfname)), "\n")
if (ONLY_N > 0) {
  set.seed(as.integer(Sys.getenv("ONLY_SEED", "20261119")))
  fs <- sample(unique(C$wrdsfname), min(ONLY_N, length(unique(C$wrdsfname))))
  C <- C[C$wrdsfname %in% fs, ]
  cat("ONLY_N set: keeping", length(fs), "filings,", nrow(C), "candidates\n")
}

## WHICH CANDIDATES SHIP. Everything that passed the structure test, plus
## rank 1 whatever its state - otherwise a slot where nothing passed would be
## sent nothing at all, and a block that failed the test is still worth a look
## when it is the only block there is.
C$ship <- C$complete | C$rank == 1
D <- C[C$ship, ]
cat("shipping:", nrow(D), sprintf("(%.0f%% of candidates)\n", 100*nrow(D)/nrow(C)))
D$n_cand_shipped <- as.integer(table(paste(D$wrdsfname, D$kind))[paste(D$wrdsfname, D$kind)])
cat("\ncandidates shipped per filing x statement:\n")
print(table(D$n_cand_shipped[!duplicated(paste(D$wrdsfname, D$kind))]))

## ---- the company name and CIK on THIS filing row -------------------------
wrds <- dbConnect(Postgres(), host = 'wrds-pgdata.wharton.upenn.edu', port = 9737,
                  user = key_get("wrds-user"), password = key_get("wrds-password"),
                  sslmode = 'require', dbname = 'wrds')
fi <- dbGetQuery(wrds, paste0(
  "SELECT wrdsfname, cik, coname, form AS form_wrds, fdate, rdate FROM wrdssec.wrds_forms",
  " WHERE wrdsfname IN ('", paste(unique(D$wrdsfname), collapse = "','"), "')"))
dbDisconnect(wrds)
D <- merge(D, fi, by = "wrdsfname", all.x = TRUE)
cat("\nfiling rows with a coname:", sum(!is.na(D$coname)), "of", nrow(D), "\n")

## ---- does the printed name match the row's company? ----------------------
## Reported, never used to drop a candidate. Comparison is deliberately crude -
## strip punctuation, legal suffixes and WRDS's /STATE/ tags, then ask whether
## either name contains the other. A loose match that picks the WRONG
## registrant is worse than no match at all, which is why the model gets both
## names rather than this flag alone.
norm <- function(s) {
  s <- tolower(ifelse(is.na(s), "", s))
  s <- gsub("/[a-z]{2}/|/de/|/new/", " ", s)
  s <- gsub("[^a-z ]", " ", s)
  s <- gsub("\\b(inc|corp|corporation|company|co|holdings|holding|group|the|lp|llc|plc|ltd|trust|partners|incorporated)\\b", " ", s)
  trimws(gsub("[[:space:]]+", " ", s))
}
a <- norm(D$reg_name); b <- norm(D$coname)
D$name_match <- ifelse(!nzchar(a) | !nzchar(b), NA,
                       mapply(function(x, y) grepl(x, y, fixed = TRUE) ||
                                              grepl(y, x, fixed = TRUE), a, b))
cat("\nregistrant name line found above the block:",
    sprintf("%.1f%%\n", 100*mean(!is.na(D$reg_name))))
cat("of those, matches the row's company name:",
    sprintf("%.1f%%\n", 100*mean(D$name_match, na.rm = TRUE)))
cat("\ncandidates with another registrant's cash flow statement in between:",
    sprintf("%.1f%%\n", 100*mean(D$cf_titles_between > 0)))

## ---- pull the text -------------------------------------------------------
remote <- '
R=/wrds/sec/wrds_clean_filings
while IFS="~" read FF K A B; do
  p="$R/$FF"
  [ -f "$p" ] || continue
  b=$(sed -n "${A},${B}p" "$p" | base64 -w0)
  printf "%s~%s~%s~%s\n" "$FF" "$K" "$A" "$b"
done < /tmp/ag_$$.txt
rm -f /tmp/ag_$$.txt'

decode <- function(x) {
  if (!nzchar(x)) return("")
  t <- rawToChar(jsonlite::base64_dec(x))
  if (!validUTF8(t)) t <- iconv(t, "latin1", "UTF-8", sub = " ")
  t
}

BS  <- 100
idx <- split(seq_len(nrow(D)), ceiling(seq_len(nrow(D)) / BS))
acc <- character(0)
for (bi in seq_along(idx)) {
  q <- D[idx[[bi]], ]
  r <- try({
    s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                     passwd = key_get("wrds-password"))
    cmd <- paste0("cat > /tmp/ag_$$.txt <<'XEOF'\n",
                  paste(paste(q$wrdsfname, q$kind, q$start, q$end, sep = "~"),
                        collapse = "\n"), "\nXEOF\n", remote)
    o <- rawToChar(ssh_exec_internal(s, cmd, error = FALSE)$stdout)
    ssh_disconnect(s); o }, silent = TRUE)
  if (inherits(r, "try-error")) { cat("  batch", bi, "FAILED - rerun\n"); next }
  acc <- c(acc, r)
  cat(sprintf("  batch %d/%d ok\n", bi, length(idx))); flush.console()
}

ln <- unlist(strsplit(paste(acc, collapse = "\n"), "\n", fixed = TRUE))
ln <- ln[nzchar(ln)]
p  <- strsplit(ln, "~", fixed = TRUE)
got <- data.frame(wrdsfname = vapply(p, `[`, character(1), 1),
                  kind      = vapply(p, `[`, character(1), 2),
                  start     = as.integer(vapply(p, `[`, character(1), 3)),
                  text      = vapply(p, function(z) decode(z[4]), character(1)),
                  stringsAsFactors = FALSE)
D <- merge(D, got, by = c("wrdsfname", "kind", "start"), all.x = TRUE)
cat("\ntext resolved for", sum(!is.na(D$text) & nzchar(D$text)), "of", nrow(D), "candidates\n")

## ---- drop page furniture -------------------------------------------------
## Same list as legacy/export-chunk-text-approach-a. The notes reference and the Table of Contents line are
## removed from what the MODEL reads while remaining available as BOUNDARY
## markers in 001d - 001d reads the marker file, not this text.
FURNITURE <- paste0(
  "^(back to |return to )?table of contents$|^index$",
  "|^see (the )?(accompanying|notes)|^the accompanying notes",
  "|^see notes to|^accompanying notes to",
  "|^[-(]?continued[-)]?$",
  "|^<page>|^page [0-9]+$|^f-[0-9]+$",
  "|^function |^if \\(e\\.|^\\} else \\{|do not remove this comment",
  "|^</?(type|sequence|filename|description|text)>")

strip_furniture <- function(txt) {
  if (is.na(txt)) return(NA_character_)
  v <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  k <- !grepl(FURNITURE, gsub("[[:space:]]+", " ", tolower(trimws(v))), perl = TRUE)
  paste(v[k], collapse = "\n")
}
D$text  <- vapply(D$text, strip_furniture, character(1), USE.NAMES = FALSE)
D$chars <- nchar(D$text)

## ---- write -------------------------------------------------------------
keep <- c("wrdsfname","cik","coname","form_wrds","fdate","rdate",
          "kind","rank","n_cand_shipped","start","end","len","nnum",
          "how","how_end","titled","complete","miss",
          "reg_name","name_match","cf_titles_between","sec_lo","sec_hi",
          "chars","text")
D <- D[order(D$wrdsfname, D$kind, D$rank), intersect(keep, names(D))]

f <- file(paste0(OUT, "candidates.jsonl"), "wb")
for (i in seq_len(nrow(D)))
  writeLines(toJSON(as.list(D[i, ]), auto_unbox = TRUE, null = "null"), f, useBytes = TRUE)
close(f)
saveRDS(D, paste0(SP, "candtext.rds"))

cat("\nwrote", paste0(OUT, "candidates.jsonl"), "-",
    round(sum(D$chars, na.rm = TRUE)/1e6, 1), "MB\n")
amb <- unique(D[D$n_cand_shipped > 1, c("wrdsfname","kind")])
cat("slots the model has to choose in:", nrow(amb),
    sprintf("(%.1f%% of %d)\n", 100*nrow(amb)/nrow(unique(D[, c("wrdsfname","kind")])),
            nrow(unique(D[, c("wrdsfname","kind")]))))
cat("median input tokens per filing: ~",
    round(median(tapply(D$chars, D$wrdsfname, sum), na.rm = TRUE)/3.5), "\n")
