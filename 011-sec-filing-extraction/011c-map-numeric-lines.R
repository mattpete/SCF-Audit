## ============================================================================
## 011c  MAP NUMERIC LINES                 step 011 (locate), substep c
## Line numbers of every line holding a statement-like number, per filing.
## 011d uses this to tell a table apart from prose. Runs BEFORE 011d; the
## letters now follow the dependency order (they used to not).
## In : cache/markers3.rds   (ORPHAN - see README, nothing in this repo
##                            writes it)
## Out: cache/numlines.rds
## ============================================================================
## One pass per filing: line numbers that contain a statement-like number.
suppressMessages({library(keyring); library(ssh)})
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
M <- readRDS(paste0(SP, "markers3.rds"))
ff <- unique(M$wrdsfname)
cat("filings:", length(ff), "\n")

remote <- '
R=/wrds/sec/wrds_clean_filings
while read FF; do
  p="$R/$FF"
  [ -f "$p" ] || continue
  printf "#%s " "$FF"
  grep -nE "[0-9][0-9,.]{2,}" "$p" | cut -d: -f1 | tr "\n" " "
  echo ""
done < /tmp/dn_$$.txt
rm -f /tmp/dn_$$.txt'

BS <- 125
idx <- split(seq_along(ff), ceiling(seq_along(ff)/BS))
acc <- character(0)
for (b in seq_along(idx)) {
  fb <- ff[idx[[b]]]
  r <- try({
    s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                     passwd = key_get("wrds-password"))
    cmd <- paste0("cat > /tmp/dn_$$.txt <<'XEOF'\n", paste(fb, collapse="\n"),
                  "\nXEOF\n", remote)
    o <- rawToChar(ssh_exec_internal(s, cmd, error=FALSE)$stdout)
    ssh_disconnect(s); o }, silent=TRUE)
  if (inherits(r, "try-error")) { cat("batch", b, "FAILED\n"); next }
  acc <- c(acc, r)
  cat(sprintf("batch %d/%d ok\n", b, length(idx))); flush.console()
}
ln <- unlist(strsplit(paste(acc, collapse="\n"), "\n", fixed=TRUE))
ln <- ln[startsWith(ln, "#")]
NUM <- list()
for (x in ln) {
  sp <- strsplit(sub("^#", "", trimws(x)), " +")[[1]]
  if (length(sp) < 2) next
  NUM[[sp[1]]] <- as.integer(sp[-1])
}
cat("\nfilings with numeric map:", length(NUM), "\n")
cat("median numeric lines per filing:",
    median(vapply(NUM, length, numeric(1))), "\n")
saveRDS(NUM, paste0(SP, "numlines.rds"))
