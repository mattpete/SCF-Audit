## SMOKE TEST for 011e: pull a handful of chunks and write them as readable .txt
## so the located statements can be eyeballed against the real filings.
## Uses the SAME remote transport as 011e, so it tests the real path.

suppressMessages({library(DBI); library(RPostgres); library(keyring)
                  library(ssh); library(jsonlite)})

SP  <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
OUT <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/inspect/")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")))

pk <- readRDS(paste0(SP, "pick9.rds"))

## ---- pick 6 filings deliberately spread across form and chunk length,
## not at random, so the sample includes a short and a long statement.
cf <- pk[pk$kind == "cf", ]
cf <- cf[order(cf$len), ]
pickers <- c(
  head(cf$wrdsfname[cf$form == "10-K"], 1),                     # shortest 10-K cf
  cf$wrdsfname[cf$form == "10-K"][ceiling(sum(cf$form=="10-K")/2) ],  # median 10-K
  tail(cf$wrdsfname[cf$form == "10-K"], 1),                     # longest 10-K
  head(cf$wrdsfname[cf$form == "10-Q"], 1),
  cf$wrdsfname[cf$form == "10-Q"][ceiling(sum(cf$form=="10-Q")/2) ],
  tail(cf$wrdsfname[cf$form == "10-Q"], 1))
pickers <- unique(pickers[!is.na(pickers)])
q <- pk[pk$wrdsfname %in% pickers, ]
q <- q[order(q$wrdsfname, q$kind), ]
cat("filings:", length(pickers), " chunks:", nrow(q), "\n")
print(q[, c("kind","form","start","end","len","how","titled","complete","nreq")])

## ---- metadata for readable filenames
wrds <- dbConnect(Postgres(), host = 'wrds-pgdata.wharton.upenn.edu', port = 9737,
                  user = key_get("wrds-user"), password = key_get("wrds-password"),
                  sslmode = 'require', dbname = 'wrds')
fi <- dbGetQuery(wrds, sprintf(
  "SELECT wrdsfname, cik, coname, form, fdate, rdate FROM wrdssec.wrds_forms
   WHERE wrdsfname IN (%s)",
  paste0("'", unique(q$wrdsfname), "'", collapse = ",")))
dbDisconnect(wrds)
cat("metadata rows:", nrow(fi), "\n")

## ---- same remote pipeline as 011e (single line per command, see its note)
remote <- '
R=/wrds/sec/wrds_clean_filings
while IFS="~" read FF KIND A B; do
  p="$R/$FF"
  [ -f "$p" ] || continue
  b=$(sed -n "${A},${B}p" "$p" | base64 -w0)
  printf "%s~%s~%s\n" "$FF" "$KIND" "$b"
done < /tmp/sm_$$.txt
rm -f /tmp/sm_$$.txt'

s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                 passwd = key_get("wrds-password"))
cmd <- paste0("cat > /tmp/sm_$$.txt <<'XEOF'\n",
              paste(paste(q$wrdsfname, q$kind, q$start, q$end, sep = "~"),
                    collapse = "\n"),
              "\nXEOF\n", remote)
o <- rawToChar(ssh_exec_internal(s, cmd, error = FALSE)$stdout)
ssh_disconnect(s)

ln <- unlist(strsplit(o, "\n", fixed = TRUE))
ln <- ln[grepl("~", ln, fixed = TRUE)]
ff <- sub("~.*$", "", ln)
kk <- sub("~.*$", "", sub("^[^~]*~", "", ln))
bb <- sub("^[^~]*~[^~]*~", "", ln)
cat("lines returned:", length(ln), " kinds:", paste(unique(kk), collapse=","), "\n")

decode <- function(x) {
  if (!nzchar(x)) return("")
  t <- rawToChar(jsonlite::base64_dec(x))
  if (!validUTF8(t)) t <- iconv(t, "latin1", "UTF-8", sub = " ")
  t
}
txt <- vapply(bb, decode, character(1), USE.NAMES = FALSE)

D <- data.frame(wrdsfname = ff, kind = kk, text = txt, stringsAsFactors = FALSE)
D <- D[nzchar(D$wrdsfname) & nzchar(D$text), ]
D <- merge(D, q[, c("wrdsfname","kind","start","end","len","how","titled","complete")],
           by = c("wrdsfname","kind"), all.x = TRUE)
D <- merge(D, fi, by = "wrdsfname", all.x = TRUE)
D$chars <- nchar(D$text)
cat("\nchunks with text:", nrow(D), "of", nrow(q), "\n")
print(D[, c("coname","form","rdate","kind","len","chars","how","titled","complete")])

## ---- one .txt per chunk, plus one combined file for scrolling
safe <- function(x) gsub("[^A-Za-z0-9]+", "-", substr(x, 1, 28))
kindlab <- c(cf = "CASH-FLOW", is = "INCOME", bs = "BALANCE-SHEET")
unlink(list.files(OUT, full.names = TRUE))
allf <- file(paste0(OUT, "_ALL-CHUNKS.txt"), "wb")
D <- D[order(D$coname, D$rdate, D$kind), ]
for (i in seq_len(nrow(D))) {
  r <- D[i, ]
  hdr <- c(strrep("=", 78),
    sprintf("%s  |  %s  |  period %s  |  filed %s", r$coname, r$form, r$rdate, r$fdate),
    sprintf("statement: %s   cik %s", kindlab[[r$kind]], r$cik),
    sprintf("located by: %s   titled: %s   complete: %s", r$how, r$titled, r$complete),
    sprintf("source lines %d-%d (%d lines, %d chars)", r$start, r$end, r$len, r$chars),
    sprintf("file: %s", r$wrdsfname),
    strrep("=", 78), "")
  nm <- sprintf("%s_%s_%s_%s.txt", safe(r$coname), r$form, r$rdate, r$kind)
  f <- file(paste0(OUT, nm), "wb")
  writeLines(c(hdr, r$text), f, useBytes = TRUE); close(f)
  writeLines(c(hdr, r$text, "", ""), allf, useBytes = TRUE)
}
close(allf)
cat("\nwrote", nrow(D), "files +  _ALL-CHUNKS.txt  to\n ", OUT, "\n")
