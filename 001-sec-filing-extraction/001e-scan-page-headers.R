## ============================================================================
## 001e  SCAN FOR STATEMENT PAGE HEADERS      step 001, substep e
## A second marker scan, run on JOINED LINE PAIRS, that looks for the header
## block printed at the top of every statement page rather than for the
## statements' contents.
## In : wrdssec.wrds_forms (the 500-filing pilot list, via cache/pick_anch.rds)
## Out: cache/markers_hdr.rds
## ============================================================================
## WHY (Matt, 2026-11-17). 001b looks for CONTENT landmarks - net income, total
## assets, earnings per share, the three cash flow sections - and that is what
## keeps failing:
##
##   EPIQ Systems    prints "NET" and "INCOME" on separate lines, so no
##                   single-line pattern can see its net income at all
##   Genlyte Group   the same
##   United Airlines labels no total on either page of its balance sheet, and
##                   puts the column years on the heading line
##   Biomet          wholly owned, so there is no EPS to close on
##
## Reading four filings line by line shows the invariant is somewhere else. At
## the top of EVERY statement page, all three filings print the same block:
##
##   EPIQ 10-Q                  SPX 10-K                  Intl Bancshares 10-Q
##   ------------------------   -----------------------   ---------------------
##   480| 1                     3231| 52                  467| 4
##   482| Table of Contents     3233| SPX Corporation     469| INTERNATIONAL
##   484| EPIQ SYSTEMS, INC.    3235| Consolidated         470| BANCSHARES ... SUBS
##   486| CONDENSED                   Statements of Oper   472| Consolidated
##   487| CONSOLIDATED BAL SH   3239| 2004 2003 2002       473| Statements of Income
##   489| (in thousands, exc                              475| (Dollars
##   492| March 31, 2009                                   476| in Thousands, exc
##   494| December 31, 2008                                478| Three Months Ended
##   498| ASSETS                                           480| March 31,
##
## page number, company name, statement title, scale, period header, column
## years - six elements inside twenty lines, and a footnote table never has
## them. So the boundary is the HEADER BLOCK, and a statement runs from one
## header to the next. Nothing about the statement's own contents is needed,
## which is exactly what the four failures above have in common.
##
## TWO THINGS THIS SCAN DOES DIFFERENTLY FROM 001b:
##
## 1. IT GREPS JOINED LINE PAIRS. Every element above splits mid-phrase -
##    "CONDENSED" / "CONSOLIDATED BALANCE SHEETS", "INTERNATIONAL" /
##    "BANCSHARES CORPORATION", "(Dollars" / "in Thousands", "Three Months
##    Ended" / "March 31,". The remote side pastes each line to the one after
##    it, so a split element appears whole on one row. A row's reported line
##    number is the FIRST of the pair, so a match is accurate to +/- 1 line.
##    Single-line matches are not lost: a phrase on one line still appears in
##    that line's joined row.
##
## 2. IT DOES NOT CHASE THE PAGE NUMBER. A bare "471" is a page number in the
##    gap between statements and an amount inside one, and nothing in the text
##    separates them - EPIQ line 368 is the amount 471. Page numbers are
##    therefore not used; the title plus the period header carry the signature.
suppressMessages({library(DBI); library(RPostgres); library(keyring); library(ssh)})
## PILOT selects which run this is. Default is the original pilot; set it to
## run the same chain over a different sample WITHOUT overwriting the original
## cache, which holds the only record of which filings the README numbers refer
## to and cannot be regenerated (Matt, 2026-11-18).
PILOT <- Sys.getenv("PILOT", "pilot-2026-11")
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT, "/cache/")
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")))

## SCAN THE WHOLE SAMPLE, NOT WHAT 001d RESOLVED.
##
## This used to read pick_anch.rds, 001d's picks, which meant approach C only
## ever saw filings where approach B had already found a cash flow statement.
## On the fresh 500-filing holdout 001d found an anchor for 478, so 22 filings
## (4.4%) were never scanned for page headers at all and could not be located
## by any means - a loss of data to a hardcoded input rather than to anything
## about the filings.
##
## C does not need B any more. Window placement uses the span of the titles
## themselves, and run selection falls back to counting distinct statement
## kinds, so a filing with no anchor is still locatable (Matt, 2026-11-18).
fl <- sort(unique(c(readRDS(paste0(SP, "dev_d.rds"))$wrdsfname,
                    readRDS(paste0(SP, "loc_d.rds"))$wrdsfname)))
cat("filings to scan:", length(fl), "  (the whole sample)\n")
## 001d's picks remain the ANCHOR where one exists; they just no longer decide
## which filings get scanned.
if (file.exists(paste0(SP, "pick_anch.rds"))) {
  pk <- readRDS(paste0(SP, "pick_anch.rds"))
  cat("of which 001d resolved a cash flow anchor for:",
      length(unique(pk$wrdsfname)), "\n")
}

S <- "[[:space:]]+"
## An OPTIONAL space. Norwood Financial's Q3 2002 10-Q heads the page
## "Consolidated Statements of Cashflows (Unaudited)" - one word - so a
## required space missed it and the filing shipped with no cash flow
## statement, the statement this whole project is about (Matt, 2026-11-18).
S0 <- "[[:space:]]*"
W <- "[a-z, '&-]{0,40}"
PAT <- paste0(
  ## ---- statement titles ------------------------------------------------
  "statements?", S, "of", S, W, "cash", S0, "flows?",
  "|statements?", S, "of", S, W, "(income|operations|earnings)",
  ## THE NOUN CAN COME FIRST. DuPont's Q2 2004 10-Q heads the page
  ## "Consolidated Income Statements (Note 1)" - no "of" anywhere in it - so
  ## every pattern above missed it and the filing shipped with no income
  ## statement at all. The line never even reached the marker file, which is
  ## why it could not be diagnosed from our own data (Matt, 2026-11-18).
  "|(income|earnings|operations)", S, "statements?",
  "|cash", S0, "flows?", S, "statements?",
  "|statements?", S, "of", S, W, "comprehensive", S, "(income|loss)",
  "|statements?", S, "of", S, W, "financial", S, "(position|condition)",
  "|balance", S, "sheets?",
  "|statements?", S, "of", S, W, "(stock|share)holders",
  "|statements?", S, "of", S, W, "changes", S, "in",
  ## ---- the PERIOD HEADER, the other half of the signature ---------------
  ## A statement always says what period it covers. A footnote table does not
  ## reprint it, which is what makes the pair discriminating.
  "|(three|six|nine|twelve|thirteen|twenty|twenty.six|thirty|thirty.nine",
  "|forty|fifty|fifty.two|fifty.three)", S, "(months?|weeks?)", S, "ended",
  "|(years?|quarters?|periods?|months?|weeks?)", S, "ended",
  "|as", S, "of", S, "(january|february|march|april|may|june|july|august",
  "|september|october|november|december)",
  "|(january|february|march|april|may|june|july|august|september|october",
  "|november|december)", S, "[0-9]{1,2},",
  ## ---- the COLUMN YEARS, on a line of their own -------------------------
  "|^", S, "?(19|20)[0-9]{2}(", S, "(19|20)[0-9]{2})*", S, "?$",
  ## ---- scale -----------------------------------------------------------
  "|in", S, "(thousands|millions|billions)",
  "|dollars", S, "in",
  "|\\(in", S, "[a-z]+",
  "|(thousands|millions|billions)\\)",
  ## ---- section brackets ------------------------------------------------
  ## The 10-Q and 10-K forms name the section. Both appear TWICE - once in the
  ## table of contents and once for real - so this marks candidates, not the
  ## answer; 001f tells them apart by what follows.
  "|item", S, "(1|8)[^a-z]{0,4}financial", S, "statements",
  "|notes", S, "to", S, "[a-z ]{0,40}financial", S, "statements",
  "|table", S, "of", S, "contents",
  ## ---- a continuation page ---------------------------------------------
  ## "Consolidated Statements of Income, continued (Unaudited)" - Intl
  ## Bancshares runs its income statement across two pages this way, and
  ## without this the second page reads as a separate statement.
  "|,", S, "?continued")

## ---- the company name line, RUN SEPARATELY OVER SHORT LINES ONLY ---------
## No digits in the class, or every sentence ending "...of the Company."
## comes back. "subsidiaries" is here because the name is usually printed
## as "X CORPORATION AND SUBSIDIARIES".
##
## THIS RULE IS NOT IN PAT, AND MUST NOT BE PUT BACK (measured 2026-11-19).
## Inside PAT it cost 93% of the entire scan: grep tests a bounded run
## ({3,70}) anchored at BOTH ends against every line, including the 170-600
## character wrapped prose paragraphs that cannot possibly match it, and that
## forces grep off its fast matcher for the whole pattern. Removing this one
## branch took 001e from 3.50 to 0.055 s/filing; de-bounding the OTHER caps
## ({0,40}) saved nothing at all, so this branch alone is the cost.
##
## It cannot simply be dropped or de-bounded: the 70-character cap IS the
## semantics. Without it, prose lines ending "...on the financial position of
## Alcoa" match too (measured: 980 extra rows per 100 filings).
##
## So the rule is unchanged and runs against short lines only. The guard is
## PROVABLY lossless, not just empirically: this pattern can match at most
## 70 chars of run + 11 ("corporation") + 1 ("[.,]") = 82 characters of
## non-space content, so a 90-character cap on the space-and-tab-trimmed
## length is strictly more generous than the rule itself. Tabs are trimmed as
## well as spaces because S is [[:space:]], not " ".
##
## The "^[0-9]+:" prefix is there because awk prepends NR":" - grep -n cannot
## be used once awk has filtered the stream, or the numbers would count only
## surviving lines. Measured: 3.502 -> 0.071 s/filing (49.5x) with the marker
## set identical on 100 filings, 121,181 rows, 0 gained and 0 lost.
NAME_PAT <- paste0(
  "^[0-9]+:", S, "?[a-z .,&'/-]{3,70}(inc|corp|corporation|company|holdings",
  "|subsidiaries|lp|llc|plc|trust|partners)[.,]?", S, "?$")

## LC_ALL=C ON EVERY GREP. The filings are plain ASCII, but in the login
## shell's UTF-8 locale "grep -i" case-folds per character as multibyte, which
## cost a measured 4.5x on this pattern for nothing. Verified byte-identical:
## 121,181 marker rows either way, not one line different in either direction.
##
## The join is done by awk rather than "tail -n +2 > /tmp && paste", which
## wrote a full copy of every filing to the login node's /tmp. That copy was
## free on time (storage measured at 0.004 s/filing) but it is 414 KB per
## filing of avoidable write on a node shared with ~260 users.
remote <- paste0('
R=/wrds/sec/wrds_clean_filings
P=', shQuote(PAT, type = "sh"), '
N=', shQuote(NAME_PAT, type = "sh"), '
while read FF; do
  p="$R/$FF"
  [ -f "$p" ] || continue
  n=$(wc -l < "$p" | tr -dc 0-9)
  echo "#F|$FF|$n"
  awk \'{ if (NR>1) print pv" "$0; pv=$0 } END { print pv }\' "$p" > /tmp/j_$$
  LC_ALL=C grep -niE "$P" /tmp/j_$$ | cut -c1-170 | tr -d "|" | sed "s/:/|/" | sed "s/^/M|/"
  awk \'{ t=$0; sub(/^[ \\t]+/,"",t); sub(/[ \\t]+$/,"",t); if (length(t)<=90) print NR":"$0 }\' /tmp/j_$$ | LC_ALL=C grep -iE "$N" | cut -c1-170 | tr -d "|" | sed "s/:/|/" | sed "s/^/M|/"
  rm -f /tmp/j_$$
done < /tmp/hd_$$.txt
rm -f /tmp/hd_$$.txt')

BS  <- 100
idx <- split(seq_along(fl), ceiling(seq_along(fl) / BS))
acc <- character(0)
for (b in seq_along(idx)) {
  r <- try({
    s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                     passwd = key_get("wrds-password"))
    cmd <- paste0("cat > /tmp/hd_$$.txt <<'XEOF'\n",
                  paste(fl[idx[[b]]], collapse = "\n"), "\nXEOF\n", remote)
    o <- rawToChar(ssh_exec_internal(s, cmd, error = FALSE)$stdout)
    ssh_disconnect(s); o }, silent = TRUE)
  if (inherits(r, "try-error")) { cat("batch", b, "FAILED\n"); next }
  acc <- c(acc, r); cat(sprintf("batch %d/%d ok\n", b, length(idx))); flush.console()
}

ln  <- unlist(strsplit(paste(acc, collapse = "\n"), "\n", fixed = TRUE))
cur <- NA_character_; curn <- NA_integer_
ff  <- character(length(ln)); nl <- integer(length(ln))
lno <- integer(length(ln)); txt <- character(length(ln)); keep <- logical(length(ln))
for (i in seq_along(ln)) {
  x <- ln[i]
  if (startsWith(x, "#F|")) {
    p <- strsplit(x, "|", fixed = TRUE)[[1]]; cur <- p[2]; curn <- as.integer(p[3]); next }
  if (!startsWith(x, "M|")) next
  p <- strsplit(x, "|", fixed = TRUE)[[1]]; if (length(p) < 3) next
  ff[i] <- cur; nl[i] <- curn
  lno[i] <- suppressWarnings(as.integer(p[2]))
  txt[i] <- tolower(paste(p[-(1:2)], collapse = " ")); keep[i] <- !is.na(lno[i])
}
M <- data.frame(wrdsfname = ff[keep], nlines = nl[keep], line = lno[keep],
                text = gsub("[[:space:]]+", " ", txt[keep]), stringsAsFactors = FALSE)
M <- M[!is.na(M$wrdsfname) & nzchar(M$wrdsfname), ]
## DEDUPE, because the company name rule now runs as a SECOND grep. A line
## that satisfies both PAT and NAME_PAT - "SPX Corporation" sitting under a
## statement title, say - was emitted once by the single grep and is emitted
## twice by two. 001f re-sorts by line everywhere it matters and its flags are
## built with any(), so a duplicate row is harmless there, but dropping it
## here keeps the marker file identical to what the old single-grep produced.
M <- M[!duplicated(M[c("wrdsfname", "line", "text")]), ]

## ---- REFUSE TO SAVE A COLLAPSED RESULT ----------------------------------
## Same guard as 001b, for the same reason: one bad character in the pattern
## turns into an empty marker file that fails two scripts later with no error
## to trace.
got <- length(unique(M$wrdsfname))
out <- paste0(SP, "markers_hdr.rds")
cat("\nrows:", nrow(M), " filings:", got, "of", length(fl), "\n")
if (nrow(M) == 0 || got < length(fl) / 2)
  stop(sprintf("scan found markers for only %d of %d filings (%d rows) - NOT saving to %s",
               got, length(fl), nrow(M), out), call. = FALSE)
saveRDS(M, out)
cat("saved", out, "\n")
