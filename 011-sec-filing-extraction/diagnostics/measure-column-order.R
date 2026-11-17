## How often is the column order descending (current year first)?
## Pull the year sequence from each chosen cash-flow chunk's header.
suppressMessages({library(DBI); library(RPostgres); library(keyring); library(ssh)})
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
pk <- readRDS(paste0(SP, "pick9.rds"))
d  <- pk[pk$kind == "cf", c("wrdsfname","start","end","form")]
cat("chunks:", nrow(d), "\n")

remote <- '
R=/wrds/sec/wrds_clean_filings
while IFS="~" read FF A B; do
  p="$R/$FF"
  [ -f "$p" ] || continue
  # first 25 non-blank lines of the chunk = the header region
  h=$(sed -n "${A},$((A+40))p" "$p" | grep -v "^[[:space:]]*$" | head -25 | grep -oE "(19|20)[0-9][0-9]" | tr "\n" " ")
  printf "%s@%s\n" "$FF" "$h"
done < /tmp/yo_$$.txt
rm -f /tmp/yo_$$.txt'

BS <- 250
idx <- split(seq_len(nrow(d)), ceiling(seq_len(nrow(d))/BS))
acc <- character(0)
for (b in seq_along(idx)) {
  q <- d[idx[[b]], ]
  r <- try({
    s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                     passwd = key_get("wrds-password"))
    cmd <- paste0("cat > /tmp/yo_$$.txt <<'XEOF'\n",
      paste(paste(q$wrdsfname, q$start, q$end, sep="~"), collapse="\n"), "\nXEOF\n", remote)
    o <- rawToChar(ssh_exec_internal(s, cmd, error=FALSE)$stdout); ssh_disconnect(s); o
  }, silent=TRUE)
  if (inherits(r, "try-error")) { cat("batch", b, "FAILED\n"); next }
  acc <- c(acc, r); cat(sprintf("batch %d/%d ok\n", b, length(idx))); flush.console()
}

ln <- unlist(strsplit(paste(acc, collapse="\n"), "\n", fixed=TRUE))
ln <- ln[grepl("@", ln, fixed = TRUE)]
ff <- sub("@.*$", "", ln)
yy <- trimws(sub("^[^@]*@", "", ln))
z <- data.frame(wrdsfname = ff, yrs = yy, stringsAsFactors = FALSE)
z <- z[nzchar(z$wrdsfname), ]
cat("parsed rows:", nrow(z), " with any year text:", sum(nzchar(z$yrs)), "\n")
z <- merge(z, d[, c("wrdsfname","form")], by = "wrdsfname")

## the filing's own fiscal year, to know which year SHOULD be first
wrds <- dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu', port=9737,
                  user=key_get("wrds-user"), password=key_get("wrds-password"),
                  sslmode='require', dbname='wrds')
fi <- dbGetQuery(wrds, sprintf("SELECT wrdsfname, rdate FROM wrdssec.wrds_forms
  WHERE wrdsfname IN (%s)", paste0("'", z$wrdsfname, "'", collapse=",")))
dbDisconnect(wrds)
z <- merge(z, fi, by="wrdsfname", all.x=TRUE)
z$fy <- as.integer(format(as.Date(z$rdate), "%Y"))

parse_seq <- function(s, fy) {
  v <- suppressWarnings(as.integer(strsplit(s, " +")[[1]]))
  v <- v[!is.na(v) & v >= 1990 & v <= 2027]
  if (!length(v)) return(NULL)
  v <- v[c(TRUE, diff(v) != 0)]                 # drop immediate repeats
  if (!is.na(fy)) v <- v[v >= fy - 4 & v <= fy + 1]   # plausible period years
  v <- v[c(TRUE, diff(v) != 0)]
  v
}
res <- lapply(seq_len(nrow(z)), function(i) parse_seq(z$yrs[i], z$fy[i]))
z$n   <- vapply(res, length, integer(1))
z$dir <- vapply(res, function(v) {
  if (length(v) < 2) return(NA_character_)
  dd <- diff(v)
  if (all(dd < 0)) "descending" else if (all(dd > 0)) "ascending" else "mixed"
}, character(1))
z$first_is_fy <- vapply(seq_along(res), function(i)
  if (!length(res[[i]]) || is.na(z$fy[i])) NA else res[[i]][1] >= z$fy[i] - 1, logical(1))

cat("\n=== could a year sequence be read from the header? ===\n")
print(table(ifelse(z$n >= 2, "yes (2+ years)", ifelse(z$n == 1, "one year only", "none")),
            z$form))

cat("\n=== column order, where 2+ years were found ===\n")
x <- z[z$n >= 2, ]
print(table(x$dir, x$form))
cat(sprintf("\ndescending (current year first): %.1f%% of %d\n",
    100*mean(x$dir == "descending"), nrow(x)))

cat("\n=== does the first column match the filing's own year? ===\n")
cat(sprintf("%.1f%%\n", 100*mean(x$first_is_fy, na.rm=TRUE)))

cat("\n=== the non-descending cases ===\n")
odd <- x[x$dir != "descending", ]
if (nrow(odd)) print(head(odd[, c("wrdsfname","form","fy","yrs","dir")], 15),
                     row.names=FALSE) else cat("none\n")
saveRDS(z, paste0(SP, "yearorder.rds"))
