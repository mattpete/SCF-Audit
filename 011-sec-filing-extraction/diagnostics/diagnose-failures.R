## Re-diagnose the 30 cash-flow failures, with the tagging actually applied.
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
M <- readRDS(paste0(SP, "markers2.rds"))
C <- readRDS(paste0(SP, "chunks2.rds"))
b <- readRDS(paste0(SP, "fails.rds"))

## same tagging as mark2b.R
tt <- M$text
M$tag <- NA_character_
M$tag[grepl("total (liabilities|equity|shareholders|stockholders|capitalization)", tt)] <- "ebs_w"
M$tag[grepl("net (increase|decrease|change) in cash", tt)] <- "ecf_w"
M$tag[grepl("total liabilities and|total liabilities, |total equity and liabilities", tt)] <- "ebs_s"
M$tag[grepl("cash.{0,50}(at |,)? ?end of|end of (the )?(period|year|quarter)", tt)] <- "ecf_s"
M$tag[grepl("supplemental (disclosure|cash|information)", tt)] <- "supp"
M$tag[is.na(M$tag) & grepl("per (common |basic |diluted )?share|earnings per share", tt)] <- "eis_s"
M$tag[grepl("statements? of cash flow", tt)] <- "t_cf"
M$tag[grepl("statements? of (income|operations|earnings)", tt)] <- "t_is"
M$tag[grepl("balance sheets?|statements? of financial (position|condition)", tt)] <- "t_bs"
M$tag[grepl("statements? of comprehensive income", tt)] <- "t_ci"
M <- M[!is.na(M$tag), ]
saveRDS(M, paste0(SP, "markers2_tagged.rds"))

f <- function(w) {
  x <- M[M$wrdsfname == w, ]
  c(t_cf=sum(x$tag=="t_cf"), ecf_s=sum(x$tag=="ecf_s"), ecf_w=sum(x$tag=="ecf_w"),
    t_is=sum(x$tag=="t_is"), t_bs=sum(x$tag=="t_bs"), nmark=nrow(x))
}
tg <- as.data.frame(t(vapply(b$wrdsfname, f, numeric(6))))
b2 <- cbind(b[, c("wrdsfname","coname","form","fdate","nlines")], tg)
b2$why <- with(b2,
  ifelse(nmark == 0,              "clean file has no statement text at all",
  ifelse(t_cf == 0,               "no cash-flow TITLE matched",
  ifelse(ecf_s + ecf_w == 0,      "title found, no END marker in filing",
                                  "title + end both exist, pairing window failed"))))
cat("=== corrected failure breakdown (cash flow, n=30) ===\n")
print(table(b2$why, b2$form))
cat("\n")
print(b2[order(b2$why, b2$coname),
         c("coname","form","nlines","t_cf","ecf_s","ecf_w","nmark","why")],
      row.names=FALSE)
saveRDS(b2, paste0(SP, "fails2.rds"))
