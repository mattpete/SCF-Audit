SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
sc <- readRDS(paste0(SP, "scored3.rds"))

## selection uses NO Compustat: strong terminal marker, then numeric density
pick <- do.call(rbind, lapply(split(sc, paste(sc$wrdsfname, sc$kind)), function(x)
  x[order(-x$strong, -x$dens, x$start), ][1, ]))

cat("=== Compustat anchors in the CHOSEN chunk (eval only) ===\n")
for (ty in c("cf","is","bs")) {
  x <- pick[pick$kind == ty & pick$scorable, ]; if (!nrow(x)) next
  cat(sprintf("%-3s n=%3d   >=1 %.1f%%   >=2 %.1f%%   >=3 %.1f%%   median hits %d\n",
      ty, nrow(x), 100*mean(x$hits>=1), 100*mean(x$hits>=2),
      100*mean(x$hits>=3), median(x$hits)))
}

cat("\n=== ceiling: best of ALL closed candidates in the filing ===\n")
for (ty in c("cf","is","bs")) {
  x <- sc[sc$kind == ty & sc$scorable, ]
  b <- tapply(x$hits, x$wrdsfname, max)
  cat(sprintf("%-3s n=%3d   >=1 %.1f%%   >=2 %.1f%%   >=3 %.1f%%\n", ty, length(b),
      100*mean(b>=1), 100*mean(b>=2), 100*mean(b>=3)))
}

cat("\n=== does the no-Compustat rule pick the best-anchored chunk? ===\n")
for (ty in c("cf","is","bs")) {
  x <- sc[sc$kind == ty & sc$scorable, ]
  b <- tapply(x$hits, x$wrdsfname, max)
  pk <- pick[pick$kind == ty & pick$scorable, c("wrdsfname","hits")]
  mm <- merge(pk, data.frame(wrdsfname=names(b), best=as.integer(b)), by="wrdsfname")
  cat(sprintf("%-3s same as best: %.1f%%   (strictly worse: %.1f%%)\n", ty,
      100*mean(mm$hits == mm$best), 100*mean(mm$hits < mm$best)))
}

cat("\n=== 10-K vs 10-Q, chosen chunk, >=2 anchors ===\n")
print(round(100*tapply(pick$hits[pick$scorable] >= 2,
      list(pick$kind[pick$scorable], pick$form[pick$scorable]), mean), 1))

cat("\n=== dev vs held-out, chosen chunk, >=2 anchors ===\n")
print(round(100*tapply(pick$hits[pick$scorable] >= 2,
      list(pick$kind[pick$scorable], pick$set[pick$scorable]), mean), 1))

cat("\n=== chosen chunk profile ===\n")
for (ty in c("cf","is","bs")) {
  x <- pick[pick$kind == ty, ]
  cat(sprintf("%-3s  len med %3.0f   numeric density med %.2f   strong marker %.1f%%\n",
      ty, median(x$len), median(x$dens), 100*mean(x$strong)))
}
saveRDS(pick, paste0(SP, "pick5.rds"))
