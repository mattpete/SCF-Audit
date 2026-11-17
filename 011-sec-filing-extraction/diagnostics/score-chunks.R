## PASS 3b: score chunks on the server, in batches with a fresh connection each time.
suppressMessages({library(keyring); library(ssh)})
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
q <- readRDS(paste0(SP, "q_chunks3.rds"))
q <- q[order(q$wrdsfname), ]

remote <- '
R=/wrds/sec/wrds_clean_filings
B=/tmp/ck_$$.txt
while IFS="~" read FF KIND A B2 PAT; do
  p="$R/$FF"
  [ -f "$p" ] || continue
  sed -n "${A},${B2}p" "$p" > $B
  k=$(grep -oE "$PAT" $B 2>/dev/null | sed "s/[^0-9.]//g" | sort -u | wc -l | tr -dc 0-9)
  nn=$(grep -coE "[0-9][0-9,]{2,}" $B | tr -dc 0-9)
  nl=$(wc -l < $B | tr -dc 0-9)
  printf "%s|%s|%s|%s|%s|%s\n" "$FF" "$KIND" "$A" "${k:-0}" "${nn:-0}" "${nl:-0}"
done < /tmp/ckin_$$.txt
rm -f /tmp/ckin_$$.txt $B'

BS <- 500
idx <- split(seq_len(nrow(q)), ceiling(seq_len(nrow(q)) / BS))
acc <- character(0)
for (b in seq_along(idx)) {
  qq <- q[idx[[b]], ]
  ok <- FALSE
  for (try in 1:3) {
    r <- try({
      s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                       passwd = key_get("wrds-password"))
      cmd <- paste0("cat > /tmp/ckin_$$.txt <<'XEOF'\n",
        paste(paste(qq$wrdsfname, qq$kind, qq$start, qq$end, qq$pat, sep="~"),
              collapse="\n"), "\nXEOF\n", remote)
      out <- rawToChar(ssh_exec_internal(s, cmd, error=FALSE)$stdout)
      ssh_disconnect(s)
      out
    }, silent = TRUE)
    if (!inherits(r, "try-error")) { acc <- c(acc, r); ok <- TRUE; break }
  }
  cat(sprintf("batch %d/%d  rows %d  %s\n", b, length(idx), nrow(qq),
              if (ok) "ok" else "FAILED")); flush.console()
}
res <- paste(acc, collapse = "\n")
ln <- strsplit(res, "\n", fixed=TRUE)[[1]]; ln <- ln[grepl("\\|", ln)]
p3 <- do.call(rbind, strsplit(ln, "|", fixed=TRUE))
sc <- data.frame(wrdsfname=p3[,1], kind=p3[,2], start=as.integer(p3[,3]),
                 hits=as.integer(p3[,4]), nnum=as.integer(p3[,5]),
                 nl=as.integer(p3[,6]), stringsAsFactors=FALSE)
sc <- merge(sc, q[, c("wrdsfname","kind","start","end","len","strong","form","set","pat")],
            by=c("wrdsfname","kind","start"))
sc$dens <- sc$nnum / pmax(sc$nl, 1)
sc$scorable <- sc$pat != "ZZZNOMATCHZZZ"
saveRDS(sc, paste0(SP, "scored3.rds"))
cat("\nscored rows:", nrow(sc), "of", nrow(q), "\n")
