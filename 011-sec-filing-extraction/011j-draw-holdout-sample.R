## ============================================================================
## 011j  DRAW A FRESH HOLDOUT SAMPLE       step 011 (locate), substep j
##
## WHY THIS EXISTS. Every locator fix through 2026-11-18 was derived from a
## filing inside the original 500, and six of the eleven cases sit in the half
## that was meant to be held out (Horton, the run-selection case, Trinity
## Place, Biomet, United, Genlyte). So the coverage figure that chain now
## reports is fitted, and the original holdout is spent as a test of it.
##
## This draws a NEW sample, excluding every filing in the original one, and
## writes it into its own cache directory so the whole chain can be re-run
## over it without touching the original. The original `dev_d.rds` and
## `loc_d.rds` are the only record of which filings the README's numbers refer
## to and cannot be regenerated, so nothing here writes near them.
##
## In : WRDS wrdssec.wrds_forms, plus the original sample (to exclude it)
## Out: {SEC_DATA_PATH}/{PILOT_OUT}/cache/dev_d.rds   the fresh sample
##      {SEC_DATA_PATH}/{PILOT_OUT}/cache/loc_d.rds   empty, so 011b's
##                                                    rbind(dev, tst) still
##                                                    works unchanged
##
## THEN RUN, with PILOT set to the same directory:
##   011b (content markers, remote)  ->  011c (numeric lines, remote)
##   011f (cash flow anchors)        ->  011h (header markers, remote)
##   011i (the locator)
##
## The draw is NOT reproducible, for the same reason the original was not:
## ORDER BY random() executes on the Postgres server, which never sees R's
## seed. That is accepted here - this sample is a test of the locator before
## it runs on the whole population, and the population will contain these
## filings anyway (Matt, 2026-11-18).
## ============================================================================
suppressMessages({library(DBI); library(RPostgres); library(keyring)})

N         <- as.integer(Sys.getenv("HOLDOUT_N", "500"))
PILOT_IN  <- Sys.getenv("PILOT_IN",  "pilot-2026-11")
PILOT_OUT <- Sys.getenv("PILOT_OUT", "holdout-2026-11")
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")), PILOT_OUT != PILOT_IN)

IN  <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT_IN,  "/cache/")
OUT <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT_OUT, "/cache/")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

## the filings to EXCLUDE: the original sample, both halves
seen <- unique(c(readRDS(paste0(IN, "dev_d.rds"))$wrdsfname,
                 readRDS(paste0(IN, "loc_d.rds"))$wrdsfname))
cat("original sample to exclude:", length(seen), "filings\n")

wrds <- dbConnect(Postgres(), host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737, user = key_get("wrds-user"),
                  password = key_get("wrds-password"),
                  sslmode = "require", dbname = "wrds")
## Over-draw, then remove the originals locally and trim to N. Excluding them
## in SQL would mean shipping 500 strings into the query for no gain.
q <- paste0("SELECT form, coname, cik, fdate, rdate, wrdsfname
  FROM wrdssec.wrds_forms
  WHERE form IN ('10-K','10-Q') AND fdate >= '2002-01-01'
    AND wrdsfname IS NOT NULL
  ORDER BY random() LIMIT ", N + 200)
d <- dbGetQuery(wrds, q)
dbDisconnect(wrds)
cat("drawn:", nrow(d), "\n")

d <- d[!d$wrdsfname %in% seen, ]
cat("after removing the original sample:", nrow(d), "\n")
d <- head(d, N)

cat("\nholdout sample:", nrow(d),
    " 10-K:", sum(d$form == "10-K"), " 10-Q:", sum(d$form == "10-Q"), "\n")
cat("fdate range:", format(min(d$fdate)), "to", format(max(d$fdate)), "\n")
cat("distinct CIKs:", length(unique(d$cik)), "\n")

## 011b reads dev_d.rds and loc_d.rds and binds them with cbind(), which fails
## on a zero-row frame ("arguments imply differing number of rows: 0, 1"). So
## the sample is split in half as the original was, rather than parked entirely
## in dev_d with an empty loc_d.
##
## The split carries no meaning here. The locator is already tuned, so this
## whole sample is the holdout; the halves exist only to keep the file shapes
## the chain expects.
keep <- c("form", "coname", "rdate", "wrdsfname")
h <- seq_len(floor(nrow(d) / 2))
saveRDS(d[h, keep],  paste0(OUT, "dev_d.rds"))
saveRDS(d[-h, keep], paste0(OUT, "loc_d.rds"))
saveRDS(d, paste0(OUT, "holdout_full.rds"))
cat("\nwrote", paste0(OUT, "dev_d.rds"), "\n")
cat("next:  PILOT=", PILOT_OUT, " Rscript 011b-scan-statement-markers.R\n", sep = "")
