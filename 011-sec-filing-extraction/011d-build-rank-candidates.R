## ============================================================================
## 011d  BUILD AND RANK CANDIDATES         step 011 (locate), substep d
## Builds every candidate text block per filing x statement, scores it on
## completeness, and ranks them. Rank 1 is the pick; the rest are the
## fallbacks 011e walks if rank 1 turns out not to be a statement.
## In : cache/markers4_raw.rds (011b), cache/numlines.rds (011c)
## Out: cache/chunks8.rds (all candidates, ranked), cache/pick9.rds (rank 1)
## ============================================================================
## PASS 2e: candidates must be COMPLETE statements; header block included.
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
M   <- readRDS(paste0(SP, "markers4_raw.rds"))
NUM <- readRDS(paste0(SP, "numlines.rds"))
t <- M$text

## ---- concept flags (a line can carry more than one) ----
F <- data.frame(
  ttl_cf = grepl("statements? of .{0,40}cash flows?", t),
  ttl_is = grepl("statements? of .{0,40}(income|operations|earnings)", t),
  ttl_bs = grepl("balance sheets?|statements? of .{0,40}financial (position|condition)", t),
  ttl_ci = grepl("statements? of .{0,40}comprehensive income", t),
  cf_op  = grepl("cash (flows?|used|provided).{0,40}operating", t) & !grepl("net cash|^net ", t),
  cf_inv = grepl("investing activit", t),
  cf_opX = grepl("operating activit", t),
  cf_finX = grepl("financing activit", t),
  cf_fin = grepl("financing activit", t),
  cf_end = grepl("cash.{0,40}end of|end of (the )?(period|year|quarter)|net (increase|decrease|change).{0,20}cash", t),
  cf_sup = grepl("supplemental (disclosure|cash|information)", t),
  is_rev = grepl("(net|total) (sales|revenues?)", t),
  is_ni  = grepl("net (income|loss|earnings)", t),
  is_tax = grepl("(income|loss) before .{0,30}tax", t),
  is_eps = grepl("per (common |basic |diluted )?share|earnings per share", t) & !grepl("par value|stated value|no par", t),
  bs_ta  = grepl("total assets", t),
  bs_tca = grepl("total current (assets|liabilities)", t),
  bs_tle = grepl("total liabilities and|total liabilities, |total equity and liabilities", t),
  bs_teq = grepl("total (equity|shareholders|stockholders|capitalization)", t),
  ## The multiplier line. Needs 011b's PAT to carry the scale patterns, or this
  ## is always FALSE and the chunk silently loses its scale.
  sc_unit = grepl(paste0("in (thousands|millions|billions)",
                         "|(thousands|millions|billions) of (u[.]?s[.]?)?dollars",
                         "|dollars in|\\((thousands|millions|billions)\\)",
                         "|(thousands|millions|billions)\\)"), t))
M <- cbind(M[, c("wrdsfname","nlines","line","form","set")], F)
M <- M[rowSums(F) > 0, ]
cat("marker rows:", nrow(M), " filings:", length(unique(M$wrdsfname)), "\n")

MAXLEN <- 900; MINLEN <- 8; CAP <- 300; PAD_UP <- 12; EXT <- 80
## How far above the title to hunt for the scale line. 6 was never enough: in a
## cell-per-line filing the title, "(In thousands)", and each column date sit on
## separate lines, so the multiplier can be 40+ lines above the first line item.
SCALE_BACK <- 60
REQ <- list(cf = c("cf_opX","cf_inv","cf_finX","cf_end"),
            is = c("is_ni","is_eps"),
            bs = c("bs_ta"))
REQ2 <- list(cf = character(0), is = c("is_rev","is_tax"),
             bs = c("bs_tle","bs_teq","bs_tca"))      # at least one of these too

build <- function(x) {
  x <- x[order(x$line), ]
  L <- function(fl) x$line[x[[fl]]]
  ttl_any <- sort(unique(c(L("ttl_cf"), L("ttl_is"), L("ttl_bs"), L("ttl_ci"))))
  own <- c(cf="ttl_cf", is="ttl_is", bs="ttl_bs")
  endf <- list(cf="cf_end", is="is_eps", bs=c("bs_tle","bs_teq"))
  out <- list()
  for (ty in c("cf","is","bs")) {
    st <- L(own[[ty]])
    if (ty == "cf") st <- sort(unique(c(st, L("cf_op"))))
    if (!length(st)) next
    e <- integer(0)
    for (fl in endf[[ty]]) { v <- L(fl); if (length(v)) { e <- sort(unique(v)); break } }
    for (S0 in sort(unique(st))) {
      cs <- e[e >= S0 + MINLEN & e <= S0 + MAXLEN]
      if (length(cs)) {
        E <- min(cs); more <- cs[cs > E & cs <= E + EXT]
        if (length(more)) E <- max(more)
        if (ty == "cf") { s2 <- L("cf_sup"); s2 <- s2[s2 > E & s2 <= E + EXT]
                          if (length(s2)) E <- max(s2) + 12 }
        how <- "marker"
      } else {
        nx <- ttl_any[ttl_any >= S0 + MINLEN & !(ttl_any %in% L(own[[ty]]))]
        E <- min(c(S0 + CAP, if (length(nx)) min(nx) - 1 else Inf))
        how <- if (length(nx) && min(nx) - 1 < S0 + CAP) "next_title" else "capped"
      }
      ## Pull in the header block, and specifically the SCALE. Start at the
      ## nearest scale marker above the title when there is one, because without
      ## the multiplier every number in the chunk is unusable - a statement in
      ## thousands reading 2,533 is $2.5M, and nothing downstream can tell.
      S  <- max(1, S0 - PAD_UP)
      sc <- L("sc_unit"); sc <- sc[sc <= S0 + 4 & sc >= S0 - SCALE_BACK]
      if (length(sc)) S <- max(1, min(min(sc) - 2, S))
      inw <- x$line >= S & x$line <= E
      nreq <- sum(vapply(REQ[[ty]], function(f) any(x[[f]][inw]), logical(1)))
      ok1 <- nreq == length(REQ[[ty]])
      ok2 <- length(REQ2[[ty]]) == 0 ||
             any(vapply(REQ2[[ty]], function(f) any(x[[f]][inw]), logical(1)))
      out[[length(out)+1]] <- data.frame(kind=ty, start=S, end=E, len=E-S+1,
        how=how, titled=S0 %in% L(own[[ty]]), complete=ok1 && ok2, nreq=nreq + as.integer(ok2))
    }
  }
  if (!length(out)) return(NULL)
  o <- do.call(rbind, out); o <- o[o$len >= MINLEN, ]
  if (!nrow(o)) return(NULL)
  o$wrdsfname <- x$wrdsfname[1]; o$form <- x$form[1]; o$set <- x$set[1]; o
}

C <- do.call(rbind, lapply(split(M, M$wrdsfname), build)); rownames(C) <- NULL
## numeric density, locally
C$nnum <- NA_integer_
for (f in unique(C$wrdsfname)) {
  v <- NUM[[f]]; if (is.null(v)) next
  i <- which(C$wrdsfname == f)
  C$nnum[i] <- vapply(i, function(j) sum(v >= C$start[j] & v <= C$end[j]), integer(1))
}
C$dens <- C$nnum / pmax(C$len, 1)
allf <- length(unique(M$wrdsfname))

cat("\n=== candidates:", nrow(C), " of which complete:", sum(C$complete), "\n")
cat("\n=== COVERAGE: filings with >=1 COMPLETE candidate ===\n")
for (ty in c("cf","is","bs")) {
  x <- C[C$kind == ty, ]
  n <- length(unique(x$wrdsfname))
  cat(sprintf("%-3s : %3d/%3d (%.1f%%)", ty, n, allf, 100*n/allf))
  for (fm in c("10-K","10-Q")) {
    tot <- length(unique(M$wrdsfname[M$form == fm]))
    got <- length(unique(x$wrdsfname[x$form == fm]))
    cat(sprintf("   %s %.1f%%", fm, 100*got/tot))
  }
  cat("\n")
}
cat("\n=== complete candidates per filing x statement ===\n")
for (ty in c("cf","is","bs")) {
  n <- as.integer(table(C$wrdsfname[C$kind == ty & C$complete]))
  if (!length(n)) next
  cat(sprintf("%-3s  median %.0f  p90 %.0f  exactly one %.1f%%\n", ty,
      median(n), quantile(n,.9), 100*mean(n == 1)))
}

## ---- rank the candidates -------------------------------------------------
## `titled` used to lead this sort and it points the WRONG WAY: a
## table-of-contents entry and an MD&A heading both read "Statements of Cash
## Flows". Among the 27 filings whose chosen cash flow block held no statement,
## 77% of the wrong blocks were `titled` against only 49% of the right ones. So
## `titled` is gone from the sort entirely.
##
## RANKING IS NOT WHAT MAKES THIS CORRECT - the validate-and-retry loop in 011e
## is, because it reads the text and walks down `rank` until the block really is
## a statement. Measured on the 27 known-bad filings, the best sort recovered
## only 19 of 27 and no sort recovered all. Ranking is therefore tuned for
## SIZE, not accuracy: the cheapest block that is still plausibly a whole
## statement, so the API bill stays down and the fallback handles the rest.
##
##   rule                            correct   median lines
##   complete, then most figures      19/27     307
##   complete + nnum>=20, smallest    17/27      95   <- chosen
##   complete, then smallest          12/27      77
##
## "complete, then smallest" is cheaper still but picks MD&A summary tables:
## Privia's 15-line "Cash Flows Data" block carries all three subtotals, so it
## reads as complete while holding no line items at all. NNUM_FLOOR keeps those
## out. The floor is a ranking preference, not a filter - at 20 numeric lines it
## keeps 88% of real statements and cuts 56% of the fakes, and the fallback
## recovers the rest.
NNUM_FLOOR <- 20

C <- do.call(rbind, lapply(split(C, paste(C$wrdsfname, C$kind)), function(z) {
  z <- z[order(-z$complete, -(z$nnum >= NNUM_FLOOR), z$len, -z$dens, z$start), ]
  z$rank <- seq_len(nrow(z))
  z }))
pk <- C[C$rank == 1, ]
cat("\n=== chosen chunk ===\n")
for (ty in c("cf","is","bs")) {
  x <- pk[pk$kind == ty, ]
  cat(sprintf("%-3s  len med %3.0f  p90 %3.0f   titled start %.1f%%   complete %.1f%%\n",
      ty, median(x$len), quantile(x$len,.9), 100*mean(x$titled), 100*mean(x$complete)))
}
lp <- tapply(pk$len, pk$wrdsfname, sum)
cat(sprintf("\nlines sent per filing (all 3): med %.0f  p90 %.0f   ~%.0f input tokens\n",
    median(lp), quantile(lp,.9), median(lp)*30/3.5))
cat("\n=== fallback depth available ===\n")
for (ty in c("cf","is","bs")) {
  n <- as.integer(table(C$wrdsfname[C$kind == ty]))
  cat(sprintf("%-3s  candidates per filing: median %.0f  p90 %.0f  only one %.1f%%\n",
      ty, median(n), quantile(n, .9), 100*mean(n == 1)))
}
saveRDS(C, paste0(SP, "chunks8.rds")); saveRDS(pk, paste0(SP, "pick9.rds"))
cat("\nsaved chunks8.rds (all candidates, with rank) / pick9.rds (rank 1)\n")
