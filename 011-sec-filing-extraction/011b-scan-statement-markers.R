## ============================================================================
## 011b  SCAN STATEMENT MARKERS           step 011 (locate), substep b
## One remote grep pass per filing: every statement title, subtotal, roll-
## forward and SCALE line, with its line number. The scale patterns are the
## reason a chunk can be padded up far enough to include the multiplier.
## In : cache/dev_d.rds, cache/loc_d.rds     (ORPHAN - see README, nothing
##                                            in this repo writes them)
## Out: cache/markers4_raw.rds
## ============================================================================
## PASS 1e: add the markers needed for a COMPLETENESS test on each statement.
suppressMessages({library(keyring); library(ssh)})
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
dev <- readRDS(paste0(SP, "dev_d.rds")); tst <- readRDS(paste0(SP, "loc_d.rds"))
d <- unique(rbind(cbind(dev[, c("wrdsfname","form")], set="dev"),
                  cbind(tst[, c("wrdsfname","form")], set="test")))
d <- d[!duplicated(d$wrdsfname), ]

S <- "[[:space:]]+"; W <- ".{0,40}"
PAT <- paste0(
  ## titles
  "statements?", S, "of", S, W, "cash", S, "flows?",
  "|statements?", S, "of", S, W, "(income|operations|earnings)",
  "|statements?", S, "of", S, W, "financial", S, "(position|condition)",
  "|statements?", S, "of", S, W, "comprehensive", S, "income",
  "|balance", S, "sheets?",
  ## cash-flow section headers (the three sections)
  "|cash", S, "flows?", W, "(operating|investing|financing)",
  "|cash", S, "(used|provided)", W, "(operating|investing|financing)",
  ## cash-flow terminal
  "|cash", W, "end", S, "of",
  "|end", S, "of", S, "(the", S, ")?(period|year|quarter)",
  "|net", S, "(increase|decrease|change)", W, "cash",
  "|supplemental", S, "(disclosure|cash|information)",
  ## income-statement content
  "|(net|total)", S, "(sales|revenues?)",
  "|net", S, "(income|loss|earnings)",
  "|(income|loss)", S, "before", S, W, "tax",
  "|per", S, "(common", S, "|basic", S, "|diluted", S, ")?share",
  "|earnings", S, "per", S, "share",
  ## balance-sheet content
  "|total", S, "assets",
  "|total", S, "current", S, "(assets|liabilities)",
  "|total", S, "(liabilities|equity|shareholders|stockholders|capitalization)",
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
P=', shQuote(PAT), '
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
saveRDS(M, paste0(SP, "markers4_raw.rds"))
cat("\nrows:", nrow(M), " filings:", length(unique(M$wrdsfname)), "\n")
