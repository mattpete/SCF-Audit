## ============================================================================
## DRAW THE FILING SAMPLE  -  SUPERSEDED by the inventory (was 011a)
## Draws the 500-filing sample (set.seed(42) below) and probes whether each
## filing even has statement headings in its WRDS clean text.
## In : WRDS wrdssec.wrds_forms
## Out: NOTHING IS SAVED. The sample the rest of the chain runs on lives in
##      cache/dev_d.rds and cache/loc_d.rds, written in an interactive
##      session that no longer exists. Re-running this does NOT regenerate
##      them and the draw is not reproducible. See README, "the seed".
## ============================================================================
suppressMessages({library(DBI); library(RPostgres); library(keyring); library(ssh)})
wrds <- dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu', port=9737,
                  user=key_get("wrds-user"), password=key_get("wrds-password"),
                  sslmode='require', dbname='wrds')
set.seed(42)
d <- dbGetQuery(wrds, "SELECT form, wrdsfname FROM wrdssec.wrds_forms
  WHERE form IN ('10-K','10-Q') AND fdate >= '2002-01-01' AND wrdsfname IS NOT NULL
  ORDER BY random() LIMIT 400")
dbDisconnect(wrds)
cat("sampled:", nrow(d), " 10-K:", sum(d$form=="10-K"), " 10-Q:", sum(d$form=="10-Q"), "\n")

s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                 passwd = key_get("wrds-password"))

# stream the list in via stdin of a cat, then loop
lines <- paste(d$form, d$wrdsfname, collapse = "\n")
cmd <- paste0("cat > /tmp/scf_sample_$$.txt <<'XEOF'\n", lines, "\nXEOF\n",
  'R=/wrds/sec/wrds_clean_filings
while read form f; do
  p="$R/$f"
  if [ ! -f "$p" ]; then echo "$form MISSING 0 0"; continue; fi
  is=$(grep -ciE "statements? of (income|operations|earnings)" "$p")
  cf=$(grep -ciE "statements? of cash flow" "$p")
  echo "$form OK $is $cf"
done < /tmp/scf_sample_$$.txt
rm -f /tmp/scf_sample_$$.txt')

res <- rawToChar(ssh_exec_internal(s, cmd, error = FALSE)$stdout)
ssh_disconnect(s)

r <- read.table(text = res, col.names = c("form","status","is_hits","cf_hits"),
                stringsAsFactors = FALSE)
cat("\n=== clean file present ===\n"); print(table(r$form, r$status))
ok <- r[r$status == "OK", ]
cat("\n=== income statement heading ===\n")
print(table(ok$form, ifelse(ok$is_hits > 0, "found", "NOT FOUND")))
cat("\n=== cash flow heading ===\n")
print(table(ok$form, ifelse(ok$cf_hits > 0, "found", "NOT FOUND")))
cat("\n=== cash-flow heading hits per filing (where >0) ===\n")
print(table(pmin(ok$cf_hits[ok$cf_hits > 0], 6)))
