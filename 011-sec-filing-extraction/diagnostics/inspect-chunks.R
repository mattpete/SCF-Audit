## Eyeball the finally-chosen cash-flow chunks: header present? runs to the end?
suppressMessages({library(keyring); library(ssh)})
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
pk <- readRDS(paste0(SP, "pick7.rds"))
d <- pk[pk$kind == "cf", ]
set.seed(11); d <- d[sample(nrow(d), 5), ]

s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                 passwd = key_get("wrds-password"))
remote <- '
R=/wrds/sec/wrds_clean_filings
while IFS="~" read FF A B; do
  echo "################ $FF   lines $A-$B"
  echo "--- FIRST 10 non-blank ---"
  sed -n "${A},${B}p" "$R/$FF" | grep -v "^[[:space:]]*$" | head -10 | cut -c1-80
  echo "--- LAST 6 non-blank ---"
  sed -n "${A},${B}p" "$R/$FF" | grep -v "^[[:space:]]*$" | tail -6 | cut -c1-80
done < /tmp/ey_$$.txt
rm -f /tmp/ey_$$.txt'
cmd <- paste0("cat > /tmp/ey_$$.txt <<'XEOF'\n",
  paste(paste(d$wrdsfname, d$start, d$end, sep="~"), collapse="\n"), "\nXEOF\n", remote)
out <- rawToChar(ssh_exec_internal(s, cmd, error=FALSE)$stdout)
ssh_disconnect(s)
writeLines(out, paste0(SP, "eyeball2.txt"))
cat(out)
