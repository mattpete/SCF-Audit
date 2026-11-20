## ============================================================================
## 001b  SCAN STATEMENT MARKERS           step 001, substep b
## One remote grep pass per filing: every statement title, subtotal, roll-
## forward and SCALE line, with its line number. The scale patterns are the
## reason a chunk can be padded up far enough to include the multiplier.
## In : cache/dev_d.rds, cache/loc_d.rds     (ORPHAN - see README, nothing
##                                            in this repo writes them)
## Out: cache/markers4_raw.rds
## ============================================================================
## PASS 1e: add the markers needed for a COMPLETENESS test on each statement.
suppressMessages({library(keyring); library(ssh)})
## PILOT selects which run this is. Default is the original pilot; set it to
## run the same chain over a different sample WITHOUT overwriting the original
## cache, which holds the only record of which filings the README numbers refer
## to and cannot be regenerated (Matt, 2026-11-18).
PILOT <- Sys.getenv("PILOT", "pilot-2026-11")
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT, "/cache/")
dev <- readRDS(paste0(SP, "dev_d.rds")); tst <- readRDS(paste0(SP, "loc_d.rds"))
d <- unique(rbind(cbind(dev[, c("wrdsfname","form")], set="dev"),
                  cbind(tst[, c("wrdsfname","form")], set="test")))
d <- d[!duplicated(d$wrdsfname), ]

S <- "[[:space:]]+"; W <- ".{0,40}"
## An OPTIONAL space, because Norwood Financial writes "Cashflows" as one word
## and a required space missed its cash flow statement entirely. Same fix as
## in 001e (Matt, 2026-11-18).
S0 <- "[[:space:]]*"
PAT <- paste0(
  ## titles
  "statements?", S, "of", S, W, "cash", S0, "flows?",
  "|statements?", S, "of", S, W, "(income|operations|earnings)",
  "|statements?", S, "of", S, W, "financial", S, "(position|condition)",
  "|statements?", S, "of", S, W, "comprehensive", S, "income",
  "|balance", S, "sheets?",
  ## THE EQUITY STATEMENT TITLE. Not a statement we extract, but it is printed
  ## between the income statement and the cash flow statement in most filings,
  ## so without a marker for it the locator cannot see that boundary and pulls
  ## the preceding statement into the cash flow block. Donnkenny's 10-K is the
  ## case: its cash flow block opened on "Consolidated Statements of
  ## Stockholders' Equity" and ran through to the cash flow statement below.
  "|statements?", S, "of", S, W, "(share|stock)holders.{0,3}", S, "?(equity|deficit)",
  "|statements?", S, "of", S, W, "changes", S, "in", S, W, "equity",
  ## cash-flow section headers (the three sections)
  "|cash", S, "flows?", W, "(operating|investing|financing)",
  "|cash", S, "(used|provided)", W, "(operating|investing|financing)",
  ## ...and the BARE section header. Plenty of filers print just "OPERATING
  ## ACTIVITIES:" with no "cash flows from" in front of it, and with only the
  ## two patterns above those lines were never captured at all - so no marker
  ## existed for them and legacy/build-rank-candidates could not tell a whole statement from one
  ## missing a section. Measured: the investing landmark was absent from 107
  ## otherwise-sound cash flow spans for exactly this reason.
  "|(operating|investing|financing)", S, "activities",
  ## the reconciliation line, which is the top of the operating section: it
  ## sits directly under the income starting point and above the first
  ## adjustment. Present in 79% of filings, and the best available anchor
  ## where the section header is phrased unusually.
  "|adjustments?", S, "to", S, "reconcile",
  ## cash-flow terminal
  "|cash", W, "end", S, "of",
  "|end", S, "of", S, "(the", S, ")?(period|year|quarter)",
  "|net", S, "(increase|decrease|change)", W, "cash",
  "|supplemental", S, "(disclosure|cash|information)",
  ## income-statement content
  "|(net|total)", S, "(sales|revenues?)",
  ## ...and the BARE revenue line. Requiring "net" or "total" in front missed
  ## every filer that prints simply "Revenue", "Revenues", "Sales" or
  ## "Revenue, net", which is common. Bloomia Holdings is the case: its real
  ## income statement was found and then rejected for having no revenue
  ## landmark, because the landmark had never been captured.
  "|^", S, "?(revenues?|sales)", S, "?[,:]?", S, "?$",
  "|(revenues?|sales)", S, "?,", S, "net",
  ## the revenue line WITH its amounts on the same line. Anchoring the bare
  ## pattern to the end of the line only catches a label printed alone, so
  ## "Revenue 1,234 5,678" was never captured - which is why Clean Energy
  ## Technologies' real income statement was found and then rejected for
  ## having no revenue landmark. Requiring a digit, $ or ( straight after the
  ## word keeps prose like "Revenue Recognition" out.
  ##
  ## The bracket is ordered "(" before "$" ON PURPOSE. This pattern is shipped
  ## to a POSIX shell on WRDS, and R's shQuote() defaults to type = "cmd" on
  ## Windows, which wraps the pattern in DOUBLE quotes - so a "$(" sequence
  ## anywhere in it becomes command substitution and the remote shell executes
  ## part of the regex. The symptom is silent and total: grep returns nothing
  ## for every filing, 001b saves an EMPTY marker file over the good one, and
  ## the next script reports zero rows with no error anywhere. shQuote(type =
  ## "sh") below is the actual fix; this ordering is belt and braces.
  "|^", S, "?(revenues?|sales)", S, "?[,:]?", S, "?[0-9($]",
  ## NOT "revenue from contracts with customers": that matches the ASC 606
  ## reference in the accounting-policies footnote and the XBRL element
  ## labels ("revenue from contract with customer [abstract]"), and it picked
  ## Clean Energy's footnote as its income statement. Measured: exactly 3
  ## filings in 500 had it as their only revenue landmark, against this cost.
  ## "profit" belongs with income/loss/earnings - Clean Energy's bottom line
  ## reads "Net profit / (loss)", so leaving it out lost the landmark.
  "|net", S, "(income|loss|earnings|profit)",
  "|(income|loss)", S, "before", S, W, "tax",
  ## FINANCIAL FIRMS. A bank's income statement has no "net sales" and no
  ## "total revenues" - it opens with interest income - so the revenue
  ## landmark was unfindable for the whole sector and 505 income-statement
  ## spans failed on it. Financials are IN SCOPE for this track (Matt,
  ## 2026-11-17), so these are first-class patterns, not edge cases.
  "|(total|net)", S, "interest", S, "(income|expense|revenue)",
  "|interest", S, "and", S, "(dividend|fee)", S, W, "income",
  "|(total|net)", S, "(premiums?|revenues?", S, "and", S, "other", S, "income)",
  "|premiums?", S, "(earned|written)",
  "|(total|net)", S, "(interest|non.?interest)", S, "income", S, "after",
  "|provision", S, "for", S, W, "(loan|credit|lease)", S, W, "loss",
  "|per", S, "(common", S, "|basic", S, "|diluted", S, ")?share",
  "|earnings", S, "per", S, "share",
  ## balance-sheet content
  "|total", S, "assets",
  "|total", S, "current", S, "(assets|liabilities)",
  "|total", S, "(liabilities|equity|shareholders|stockholders|capitalization)",
  ## A bank's balance sheet has no current/non-current split at all, so
  ## "total current assets" never appears and the top-of-statement anchor was
  ## missing for financials. Deposits are the landmark that replaces it.
  "|total", S, "deposits",
  ## THE TWO SECTION HEADINGS, on their own line. These are the only
  ## layout-independent landmarks the balance sheet has, because a filer can
  ## print the TOTAL with no label at all: Amerisafe's 10-K shows total assets
  ## as "$ 1,148,509 $ 1,117,923" on a line of its own, the HTML label cell
  ## being empty. No word pattern can reach that line, so its real balance
  ## sheet produced no candidate and the locator was left choosing among
  ## footnote tables. Every balance sheet has these headings.
  ## The heading may carry its column years on the same line - United prints
  ## "ASSETS 2014 2013" and "LIABILITIES AND STOCKHOLDERS' EQUITY 2014 2013" -
  ## so the anchored end-of-line form never reached them and neither side of
  ## its balance sheet had a single findable landmark. Trailing figures are
  ## allowed; trailing words are not.
  "|^", S, "?assets", S, "?:?", S, "?[0-9 ,.$()-]*$",
  "|^", S, "?liabilities(", S, "and", S, W, "equity)?", S, "?:?", S, "?[0-9 ,.$()-]*$",
  ## RESTATEMENT RECONCILIATION COLUMNS. A footnote that reconciles a restated
  ## statement is captioned with the statement's own name and carries its
  ## totals, so it satisfies every structural landmark - Amerisafe's "As
  ## Reported / Adjustments / As Adjusted" table was chosen as its balance
  ## sheet. These column captions are what a primary statement never has, and
  ## catching them matters beyond this one case: for THIS project the
  ## restatement footnote is the single most dangerous false positive, since
  ## it is where the revisions we are trying to measure are described.
  "|as", S, "(previously", S, ")?(reported|adjusted|restated|revised)",
  "|previously", S, "reported",
  "|effect", S, "of", S, W, "(restatement|adjustment|revision|correction)",
  ## CONSOLIDATING SCHEDULES. A guarantor / non-guarantor note and a Schedule I
  ## parent-only statement are captioned with the statement's own name and are
  ## complete statements, so no structural test can reject them - and the
  ## guarantor note carries all THREE statements next to each other, which
  ## satisfies the adjacency rule as well. Biomet's 10-K is the case: its
  ## chosen balance sheet was Note 13's "Biomet, Inc. | Guarantors |
  ## Non-Guarantors | Eliminations | Total" schedule.
  ##
  ## The column captions are the tell. A primary consolidated statement never
  ## has an eliminations column, and never splits itself by guarantor status.
  ## This is the same failure as picking the wrong co-registrant, and the
  ## numbers it yields look right and will foot.
  "|eliminations?",
  "|non.?guarantors?|guarantors?",
  "|condensed", S, "consolidating",
  "|parent", S, "(company", S, ")?only|parent.only",
  "|schedule", S, "i+", S, "?[-(]",
  "|(cash", S, "and", S, "(cash", S, "equivalents|due", S, "from)|due", S, "from", S, "banks)",
  ## THE STATEMENT SEPARATOR, and the page break. Every primary statement is
  ## followed by a line referring the reader to the notes - Biomet prints "The
  ## accompanying notes are a part of the consolidated financial statements."
  ## after each of its four - and in the cleaned text the printed page break
  ## arrives as a bare "Table of Contents" line. Together these BRACKET the
  ## statements section explicitly, which is stronger than any window width:
  ## the statements are the blocks BETWEEN the separators.
  ##
  ## Added for 001d, which anchors on the cash flow statement (the one we
  ## locate most reliably) and then looks for the other two inside that
  ## section. legacy/build-rank-candidates does not read these and is unaffected.
  ## Words get between "accompanying" and "notes": United Continental writes
  ## "The accompanying COMBINED Notes to Consolidated Financial Statements are
  ## an integral part of these statements.", which an adjacent-words pattern
  ## misses entirely - so United Airlines, Inc.'s own income statement (no EPS,
  ## being a wholly-owned subsidiary) had no close marker AND no separator, and
  ## produced no candidate at all.
  "|accompanying.{0,30}notes?",
  "|see", S, "notes?", S, "to",
  ## The same sentence without the word "notes" in reach. This clause is what
  ## actually marks the end of a printed statement.
  "|integral", S, "part", S, "of", S, W, "statements?",
  "|^", S, "?table", S, "of", S, "contents",
  ## THE REGISTRANT NAME LINE. A combined filing holds two or more companies'
  ## statements and WRDS indexes it once per registrant, so the same document
  ## is drawn as several rows and each row wants ITS OWN company's statements.
  ## United's 10-K is the case: one accession, two rows -
  ##   000010/100517/...  United Continental Holdings, Inc.  net income 1,132
  ##   000031/319687/...  UNITED AIRLINES, INC.              net income 1,114
  ## and the name is printed on its own line directly above each statement
  ## title. 4.92% of filing rows since 2002 are combined filings (36,008 of
  ## 732,299), and this is the one error class whose numbers foot perfectly
  ## while belonging to the wrong company.
  ##
  ## No digits in the character class, or every prose sentence ending in
  ## "...of the Company." comes back too.
  "|^", S, "?[a-z .,&'/-]{3,60}(inc|corp|corporation|company|holdings|lp|llc|plc|trust|partners)[.,]?", S, "?$",
  ## SCALE. Without this the chunk cannot be padded up far enough to include the
  ## multiplier, and every extracted number comes out 1,000x or 1,000,000x wrong.
  ## Measured: NONE of four hand-checked chunks contained their scale line, because
  ## PAD_UP=6 lines is nothing in a cell-per-line layout where the title, the
  ## scale and each column date each sit on their own line.
  "|in", S, "(thousands|millions|billions)",
  "|(thousands|millions|billions)", S, "of", S, "(u[.]?s[.]?", S, ")?dollars",
  "|except", S, W, "per", S, "share",
  ## The HTML-to-text conversion wraps a header cell mid-phrase, so "(Dollars In
  ## Thousands)" arrives as "(Dollars In" then "Thousands)" on the next line and
  ## a per-line grep sees neither. First Financial Service Corp 10-Q is the case.
  ## These two fragments catch each half; the chunk then reaches the whole
  ## header and the model reads it as printed.
  "|dollars", S, "in",
  "|\\((thousands|millions|billions)\\)",
  "|(thousands|millions|billions)\\)")

s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                 passwd = key_get("wrds-password"))
remote <- paste0('
R=/wrds/sec/wrds_clean_filings
P=', shQuote(PAT, type = "sh"), '
while read FF; do
  p="$R/$FF"
  [ -f "$p" ] || continue
  n=$(wc -l < "$p" | tr -dc 0-9)
  echo "#F|$FF|$n"
  grep -niE "$P" "$p" | cut -c1-120 | tr -d "|" | sed "s/:/|/" | sed "s/^/M|/"
done < /tmp/mk_$$.txt
rm -f /tmp/mk_$$.txt')

BS <- 125
fl <- d$wrdsfname
idx <- split(seq_along(fl), ceiling(seq_along(fl)/BS))
acc <- character(0)
for (b in seq_along(idx)) {
  r <- try({
    s2 <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                      passwd = key_get("wrds-password"))
    cmd <- paste0("cat > /tmp/mk_$$.txt <<'XEOF'\n",
                  paste(fl[idx[[b]]], collapse="\n"), "\nXEOF\n", remote)
    o <- rawToChar(ssh_exec_internal(s2, cmd, error=FALSE)$stdout)
    ssh_disconnect(s2); o }, silent=TRUE)
  if (inherits(r, "try-error")) { cat("batch", b, "FAILED\n"); next }
  acc <- c(acc, r); cat(sprintf("batch %d/%d ok\n", b, length(idx))); flush.console()
}
try(ssh_disconnect(s), silent=TRUE)

ln <- unlist(strsplit(paste(acc, collapse="\n"), "\n", fixed=TRUE))
cur <- NA_character_; curn <- NA_integer_
ff <- character(length(ln)); nl <- integer(length(ln))
lno <- integer(length(ln)); txt <- character(length(ln)); keep <- logical(length(ln))
for (i in seq_along(ln)) {
  x <- ln[i]
  if (startsWith(x, "#F|")) {
    p <- strsplit(x, "|", fixed=TRUE)[[1]]; cur <- p[2]; curn <- as.integer(p[3]); next }
  if (!startsWith(x, "M|")) next
  p <- strsplit(x, "|", fixed=TRUE)[[1]]; if (length(p) < 3) next
  ff[i] <- cur; nl[i] <- curn
  lno[i] <- suppressWarnings(as.integer(p[2]))
  txt[i] <- tolower(paste(p[-(1:2)], collapse=" ")); keep[i] <- !is.na(lno[i])
}
M <- data.frame(wrdsfname=ff[keep], nlines=nl[keep], line=lno[keep],
                text=gsub("[[:space:]]+", " ", txt[keep]), stringsAsFactors=FALSE)
M <- M[!is.na(M$wrdsfname) & nzchar(M$wrdsfname), ]
M <- merge(M, d, by="wrdsfname")
## ---- REFUSE TO SAVE A COLLAPSED RESULT -----------------------------------
## This script used to save unconditionally, and that turned one bad character
## in the pattern into a destroyed cache: the remote grep matched nothing for
## every filing, an empty marker file replaced the good one, and the failure
## surfaced two scripts later as "0 rows" with no error to trace. A run that
## finds markers for fewer than half the filings it asked about has failed,
## whatever the exit status said, so it stops here with the old file intact.
got <- length(unique(M$wrdsfname))
cat("\nrows:", nrow(M), " filings:", got, "of", nrow(d), "\n")
out <- paste0(SP, "markers4_raw.rds")
if (nrow(M) == 0 || got < nrow(d) / 2) {
  stop(sprintf(paste0("markers collapsed: %d rows for %d of %d filings. ",
                      "NOT saving - %s is unchanged.\n",
                      "  Check the grep pattern reached the remote shell intact: ",
                      "a '$(' sequence in PAT becomes command substitution if the ",
                      "pattern is double-quoted (see the shQuote note above)."),
               nrow(M), got, nrow(d), out), call. = FALSE)
}
saveRDS(M, out)
cat("saved", out, "\n")
