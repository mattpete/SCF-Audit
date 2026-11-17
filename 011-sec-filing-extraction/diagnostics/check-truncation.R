## For chosen chunks that are not complete: which concept is missing, and would
## extending the end fix it?
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
M  <- readRDS(paste0(SP, "markers4_raw.rds"))
pk <- readRDS(paste0(SP, "pick8.rds"))
t <- M$text
FL <- data.frame(
  cf_op  = grepl("cash (flows?|used|provided).{0,40}operating", t) & !grepl("net cash|^net ", t),
  cf_inv = grepl("cash (flows?|used|provided).{0,40}investing", t),
  cf_fin = grepl("cash (flows?|used|provided).{0,40}financing", t),
  cf_end = grepl("cash.{0,40}end of|end of (the )?(period|year|quarter)|net (increase|decrease|change).{0,20}cash", t),
  is_ni  = grepl("net (income|loss|earnings)", t),
  is_eps = grepl("per (common |basic |diluted )?share|earnings per share", t) &
           !grepl("par value|stated value|no par", t),
  bs_ta  = grepl("total assets", t))
M <- cbind(M[, c("wrdsfname","line")], FL)

REQ <- list(cf=c("cf_op","cf_inv","cf_fin","cf_end"), is=c("is_ni","is_eps"), bs="bs_ta")
report <- function(ty, extra = 0) {
  p <- pk[pk$kind == ty, ]
  miss <- matrix(FALSE, nrow(p), length(REQ[[ty]]),
                 dimnames = list(NULL, REQ[[ty]]))
  for (i in seq_len(nrow(p))) {
    x <- M[M$wrdsfname == p$wrdsfname[i] &
           M$line >= p$start[i] & M$line <= p$end[i] + extra, ]
    for (j in seq_along(REQ[[ty]]))
      miss[i, j] <- !any(x[[REQ[[ty]][j]]])
  }
  cat(sprintf("%-3s extra=%4d  complete %.1f%%   missing: %s\n", ty, extra,
      100*mean(rowSums(miss) == 0),
      paste(sprintf("%s %.0f%%", colnames(miss), 100*colMeans(miss)), collapse="  ")))
}
cat("=== as chosen ===\n")
for (ty in c("cf","is","bs")) report(ty, 0)
cat("\n=== if the end is pushed out ===\n")
for (ex in c(50, 150, 300)) { for (ty in c("cf","is","bs")) report(ty, ex); cat("\n") }
