## Actual characters in the chosen chunks (the real driver of token cost).
suppressMessages({library(keyring); library(ssh)})
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
pk <- readRDS(paste0(SP, "pick9.rds"))
pk <- pk[order(pk$wrdsfname), ]
cat("chunks:", nrow(pk), "\n")

remote <- '
R=/wrds/sec/wrds_clean_filings
while IFS="~" read FF KIND A B; do
  p="$R/$FF"
  [ -f "$p" ] || continue
  # characters, and characters after collapsing runs of whitespace
  raw=$(sed -n "${A},${B}p" "$p" | wc -c | tr -dc 0-9)
  sq=$(sed -n "${A},${B}p" "$p" | tr -s " \t" " " | sed "/^ *$/d" | wc -c | tr -dc 0-9)
  printf "%s|%s|%s|%s\n" "$FF" "$KIND" "${raw:-0}" "${sq:-0}"
done < /tmp/by_$$.txt
rm -f /tmp/by_$$.txt'

BS <- 400
idx <- split(seq_len(nrow(pk)), ceiling(seq_len(nrow(pk))/BS))
acc <- character(0)
for (b in seq_along(idx)) {
  q <- pk[idx[[b]], ]
  r <- try({
    s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                     passwd = key_get("wrds-password"))
    cmd <- paste0("cat > /tmp/by_$$.txt <<'XEOF'\n",
      paste(paste(q$wrdsfname, q$kind, q$start, q$end, sep="~"), collapse="\n"),
      "\nXEOF\n", remote)
    o <- rawToChar(ssh_exec_internal(s, cmd, error=FALSE)$stdout)
    ssh_disconnect(s); o }, silent=TRUE)
  if (inherits(r, "try-error")) { cat("batch", b, "FAILED\n"); next }
  acc <- c(acc, r); cat(sprintf("batch %d/%d ok\n", b, length(idx))); flush.console()
}
ln <- unlist(strsplit(paste(acc, collapse="\n"), "\n", fixed=TRUE))
ln <- ln[grepl("\\|", ln)]
p <- do.call(rbind, strsplit(ln, "|", fixed=TRUE))
z <- data.frame(wrdsfname=p[,1], kind=p[,2],
                raw=as.numeric(p[,3]), sq=as.numeric(p[,4]), stringsAsFactors=FALSE)
z <- merge(z, unique(pk[, c("wrdsfname","form")]), by="wrdsfname")
saveRDS(z, paste0(SP, "bytes.rds"))

cat("\n=== characters per chunk ===\n")
for (ty in c("cf","is","bs")) {
  x <- z[z$kind == ty, ]
  cat(sprintf("%-3s n=%4d  raw: med %5.0f  mean %5.0f   |  whitespace-collapsed: med %5.0f  mean %5.0f\n",
      ty, nrow(x), median(x$raw), mean(x$raw), median(x$sq), mean(x$sq)))
}
pf <- aggregate(cbind(raw, sq) ~ wrdsfname, z, sum)
cat(sprintf("\nper FILING, all three statements: raw med %.0f mean %.0f | collapsed med %.0f mean %.0f chars\n",
    median(pf$raw), mean(pf$raw), median(pf$sq), mean(pf$sq)))
pfm <- merge(pf, unique(pk[, c("wrdsfname","form")]), by="wrdsfname")
cat("\nby form (collapsed chars per filing):\n")
print(round(tapply(pfm$sq, pfm$form, mean)))
