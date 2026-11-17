## PASS 3a: build + cache the Compustat anchor patterns per chunk.
suppressMessages({library(DBI); library(RPostgres); library(keyring)})
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
C <- readRDS(paste0(SP, "chunks3.rds")); K <- C

wrds <- dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu', port=9737,
                  user=key_get("wrds-user"), password=key_get("wrds-password"),
                  sslmode='require', dbname='wrds')
fi <- dbGetQuery(wrds, sprintf("SELECT cik, rdate, wrdsfname, form FROM wrdssec.wrds_forms
  WHERE wrdsfname IN (%s)", paste0("'", unique(K$wrdsfname), "'", collapse=",")))
ann <- dbGetQuery(wrds, "SELECT cik, datadate, ni, ib, txt, epspx, epsfx, xint,
    oancf, ivncf, fincf, chech, dpc, at, che, seq, ceq
  FROM comp.funda WHERE indfmt='INDL' AND datafmt='STD' AND popsrc='D' AND consol='C'
    AND datadate >= '2001-01-01' AND cik IS NOT NULL")
qtr <- dbGetQuery(wrds, "SELECT cik, datadate, niq AS ni, ibq AS ib, txtq AS txt,
    epspxq AS epspx, epsfxq AS epsfx, xintq AS xint, oancfy AS oancf, ivncfy AS ivncf,
    fincfy AS fincf, chechy AS chech, dpcy AS dpc, atq AS at, cheq AS che,
    seqq AS seq, ceqq AS ceq
  FROM comp.fundq WHERE indfmt='INDL' AND datafmt='STD' AND popsrc='D' AND consol='C'
    AND datadate >= '2001-01-01' AND cik IS NOT NULL")
dbDisconnect(wrds)

jn <- function(f, cs) { o <- merge(f, cs, by="cik")
  o <- o[abs(as.numeric(as.Date(o$rdate) - as.Date(o$datadate))) <= 7, ]
  o[!duplicated(o$wrdsfname), ] }
m <- rbind(jn(fi[fi$form=="10-K", ], ann), jn(fi[fi$form=="10-Q", ], qtr))

SETS <- list(cf=c("oancf","ivncf","fincf","chech","dpc","che"),
             is=c("ni","ib","txt","epspx","epsfx","xint"),
             bs=c("at","che","seq","ceq"))
fbig <- function(x) { if (is.na(x)) return(character(0)); v <- abs(x); o <- character(0)
  if (v >= 1)    o <- c(o, formatC(round(v),      format="d", big.mark=","))
  if (v >= .001) o <- c(o, formatC(round(v*1000), format="d", big.mark=","))
  o[nchar(gsub(",", "", o)) >= 3] }
feps <- function(x) { if (is.na(x) || abs(x) < .01) return(character(0))
  gsub("[.]", "[.]", formatC(abs(x), format="f", digits=2)) }
patfor <- function(i, items) {
  p <- unlist(lapply(items, function(a)
        if (a %in% c("epspx","epsfx")) feps(m[[a]][i]) else fbig(m[[a]][i])))
  paste(unique(p[nzchar(p)]), collapse="|") }
pm <- data.frame(wrdsfname=m$wrdsfname,
  cf=vapply(seq_len(nrow(m)), patfor, character(1), SETS$cf),
  is=vapply(seq_len(nrow(m)), patfor, character(1), SETS$is),
  bs=vapply(seq_len(nrow(m)), patfor, character(1), SETS$bs), stringsAsFactors=FALSE)
long <- do.call(rbind, lapply(c("cf","is","bs"), function(k)
  data.frame(wrdsfname=pm$wrdsfname, kind=k, pat=pm[[k]], stringsAsFactors=FALSE)))
q <- merge(K, long, by=c("wrdsfname","kind"), all.x=TRUE)
q$pat[is.na(q$pat) | !nzchar(q$pat)] <- "ZZZNOMATCHZZZ"
saveRDS(q, paste0(SP, "q_chunks3.rds"))
cat("chunks:", nrow(q), " with anchor pattern:", sum(q$pat != "ZZZNOMATCHZZZ"),
    " filings joined:", nrow(m), "of", length(unique(K$wrdsfname)), "\n")
