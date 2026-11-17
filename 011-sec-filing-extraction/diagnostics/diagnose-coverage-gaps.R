## Which filings still have no chunk, and why - all three statements.
suppressMessages({library(DBI); library(RPostgres); library(keyring)})
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
M <- readRDS(paste0(SP, "markers3.rds"))
C <- readRDS(paste0(SP, "chunks3.rds"))
allf <- unique(M[, c("wrdsfname","form","set","nlines")])

STARTS <- list(cf=c("t_cf","cf_sec"), is="t_is", bs="t_bs")
ENDS   <- list(cf=c("ecf_s","ecf_w"), is="eis_s", bs=c("ebs_s","ebs_w"))

cnt <- function(w, tags) sum(M$tag[M$wrdsfname == w] %in% tags)
rows <- list()
for (ty in c("cf","is","bs")) {
  got <- unique(C$wrdsfname[C$kind == ty])
  bad <- allf[!allf$wrdsfname %in% got, ]
  if (!nrow(bad)) next
  bad$kind <- ty
  bad$nst <- vapply(bad$wrdsfname, cnt, numeric(1), STARTS[[ty]])
  bad$nen <- vapply(bad$wrdsfname, cnt, numeric(1), ENDS[[ty]])
  bad$nmark <- vapply(bad$wrdsfname, function(w) sum(M$wrdsfname == w), numeric(1))
  bad$why <- with(bad,
    ifelse(nmark == 0,        "no statement text in clean file",
    ifelse(nst == 0 & nen == 0, "neither start nor end marker",
    ifelse(nst == 0,          "no START marker",
    ifelse(nen == 0,          "no END marker",
                              "both exist, >700 lines apart")))))
  rows[[ty]] <- bad
}
B <- do.call(rbind, rows)
cat("=== remaining gaps by statement and cause ===\n")
print(table(B$why, B$kind))
cat("\n=== by form ===\n")
print(table(B$kind, B$form))

wrds <- dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu', port=9737,
                  user=key_get("wrds-user"), password=key_get("wrds-password"),
                  sslmode='require', dbname='wrds')
info <- dbGetQuery(wrds, sprintf("SELECT wrdsfname, coname, fdate FROM wrdssec.wrds_forms
  WHERE wrdsfname IN (%s)", paste0("'", unique(B$wrdsfname), "'", collapse=",")))
dbDisconnect(wrds)
B <- merge(B, info, by="wrdsfname", all.x=TRUE)
saveRDS(B, paste0(SP, "gaps.rds"))

for (ty in c("cf","is","bs")) {
  x <- B[B$kind == ty, ]
  cat(sprintf("\n===== %s : %d filings =====\n", ty, nrow(x)))
  print(x[order(x$why, x$coname), c("coname","form","nlines","nst","nen","nmark","why")],
        row.names=FALSE)
}
