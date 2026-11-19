## ============================================================================
## 011f  LOCATE BY ANCHORING ON THE CASH FLOW STATEMENT
##                                          step 011 (locate), substep f
## An ALTERNATIVE to 011d, not a successor. 011d stays in the chain and is
## untouched; this script writes its own outputs so the two can be compared
## filing by filing.
## In : cache/markers4_raw.rds (011b), cache/numlines.rds (011c)
## Out: cache/chunks_anch.rds (candidates), cache/pick_anch.rds (rank 1),
##      cache/anch_vs_011d.rds (the filing-level comparison)
## ============================================================================
## WHY (Matt, 2026-11-17). 011d searches the whole filing three times, once per
## statement, and uses adjacency only as a TIEBREAK that quality tier dominates.
## That cannot be fixed by reweighting, because the failure is not that the
## right block was outranked - it is that the right block was never a candidate.
##
## Biomet's 10-K (FY May 2010) is the case. Its four statements print on
## consecutive pages, which our line numbers reproduce exactly:
##
##     balance sheet            3792-3907     page 78
##     statements of operations 3916-3962     page 79
##     shareholder's equity    ~3972-4058     page 80
##     statements of cash flows 4070-4228     page 81
##
## 011d found the balance sheet and the cash flow statement and then chose, as
## the income statement, the unaudited QUARTERLY RESULTS footnote at 6787 -
## 2,717 lines away. Not because it scored better on adjacency but because the
## real income statement produced NO CANDIDATE AT ALL: Biomet is wholly owned
## by LVB Acquisition, so it reports no EPS, and EPS is 011d's only close
## marker for an income statement. The statement ends at "Net loss".
##
## Measured: 22 of 498 pilot filings have no EPS near any income statement
## title. The genuine class is wholly-owned subsidiaries and partnerships
## filing on public debt - PACCAR Financial says so outright, "earnings per
## share and dividends per share are not reported because the company is a
## wholly owned subsidiary". A Compustat or CRSP screen does NOT remove them:
## Biomet and First Data both carry a CRSP link from their public era.
##
## THE IDEA. Anchoring does not just move adjacency up the priority order - it
## lets us ASK FOR LESS EVIDENCE. 011d has to demand a title AND a top landmark
## AND a bottom landmark, because the block could be anywhere in 15,000 lines.
## Inside a 200-line window after a confirmed cash flow statement, "title +
## revenue line + net income" is overwhelming, because there is nothing else it
## could be. No EPS required. So the ragged cases become findable rather than
## needing a looser rule applied document-wide, which is what would break the
## cases that currently work.
##
## The cash flow statement is the right anchor on the measured evidence - it is
## both the one we locate most often and the one with least to confuse it:
##
##                        cash flow   income   balance
##   located                  96.6%    95.6%     93.4%
##   candidates per filing      2.0      4.2       6.0
##   ...of those, complete      77%      61%       32%
##
## Two candidates per filing and three-quarters already complete: the locator
## is barely choosing anything, because no footnote in a 10-K contains
## "operating activities / investing activities / financing activities". There
## is no miniature cash flow statement in the notes. The income statement has a
## true double - the quarterly results footnote - which is why its failures are
## complete, titled and wrong rather than visibly broken.
##
## BRACKETING, NOT A WINDOW WIDTH. Every primary statement is followed by a
## line sending the reader to the notes ("The accompanying notes are a part of
## the consolidated financial statements."), and the printed page break arrives
## in the cleaned text as a bare "Table of Contents" line. Those separators
## bracket the statements explicitly, so a statement runs from its title to the
## next separator. That is structural rather than a number I picked, it needs
## no close marker, and it is therefore what makes the no-EPS case work.
##
## 011e strips both of these lines as page furniture - that is correct for what
## gets sent to the model and wrong as a reason not to USE them for boundaries.
##
## THE COST, which is real and is why 011d stays. All three picks come to share
## one point of failure. Today three picks 10,000 lines apart is a SIGNAL that
## something broke, and it is how Biomet was caught; anchor them and the triple
## always looks plausible. So this script reports its disagreements with 011d
## rather than replacing it, and that comparison is free - no API calls.
## ============================================================================
PILOT <- Sys.getenv("PILOT", "pilot-2026-11")
SP  <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT, "/cache/")
M   <- readRDS(paste0(SP, "markers4_raw.rds"))
NUM <- readRDS(paste0(SP, "numlines.rds"))
t <- M$text

## ---- flags: identical to 011d, plus the two separator flags --------------
## Deliberately copied rather than factored out. 011d is validated against
## five rounds of hand audit and must not be edited to support an experiment.
PROSE <- paste0(
  "(^|[[:space:]])(we|our|us|their|its|these|those|have|has|had|been|was|were",
  "|is|are|be|consist|consists|recognized|recognised|included|includes",
  "|including|reported|approximate|approximates|reflected|presented|comprise",
  "|comprises|shown|relating|related|refer|see)([[:space:]]|$)",
  "|(in|on|to|at|from|with|of|for)[[:space:]]+the[[:space:]]")

title_like <- function(t, rx) {
  p <- regexpr(rx, t, perl = TRUE)
  ok <- p > 0
  pre <- ifelse(ok, substr(t, 1, pmax(0L, p - 1L)), "")
  at_word_start <- ok & (p == 1L | grepl("[[:space:]]$", pre))
  at_word_start & !grepl(PROSE, pre, perl = TRUE)
}

F <- data.frame(
  ttl_cf = title_like(t, "statements? of .{0,40}cash flows?"),
  ttl_is = title_like(t, "statements? of .{0,40}(income|operations|earnings)"),
  ttl_bs = title_like(t, "balance sheets?|statements? of .{0,40}financial (position|condition)"),
  ttl_ci = title_like(t, "statements? of .{0,40}comprehensive income"),
  ttl_eq = title_like(t, paste0("statements? of .{0,40}(share|stock)holders.{0,3} ?(equity|deficit)",
                             "|statements? of .{0,40}changes in .{0,20}equity")),
  cf_bop = grepl("cash flows? (from|used)|^operating activities|operating activities:", t) &
           grepl("operating", t) & !grepl("[0-9]", t) &
           !grepl("net cash|^net |provided by|used in [a-z]* ?activities", t),
  cf_recon = grepl("adjustments? to reconcile", t),
  cf_opX = grepl("operating activit", t),
  cf_inv = grepl("investing activit", t),
  cf_finX = grepl("financing activit", t),
  cf_end = grepl("cash.{0,40}end of|end of (the )?(period|year|quarter)|net (increase|decrease|change).{0,20}cash", t),
  cf_sup = grepl("supplemental (disclosure|cash|information)", t),
  is_rev = grepl(paste0("(net|total) (sales|revenues?)",
                        "|^ ?(revenues?|sales) ?[,:]? ?($|[0-9$(])",
                        "|(revenues?|sales) ?, net",
                        "|(total|net) interest (income|revenue)",
                        "|interest and (dividend|fee).{0,30}income",
                        "|total revenues? and other income",
                        "|(total|net) premiums?|premiums? (earned|written)"), t),
  is_ni  = grepl("net (income|loss|earnings|profit)", t),
  is_tax = grepl("(income|loss) before .{0,30}tax", t),
  is_eps = grepl("per (common |basic |diluted )?share|earnings per share", t) & !grepl("par value|stated value|no par", t),
  bs_ta  = grepl("total assets", t),
  bs_tca = grepl("total current (assets|liabilities)|total deposits", t),
  bs_tle = grepl("total liabilities and|total liabilities, |total equity and liabilities", t),
  bs_hasset = grepl("^ ?assets ?:? ?[0-9 ,.$()-]*$", t),
  bs_hliab  = grepl("^ ?liabilities( and .{0,40}equity)? ?:? ?[0-9 ,.$()-]*$", t),
  recon = grepl(paste0("as (previously )?(reported|adjusted|restated|revised)",
                       "|previously reported",
                       "|effect of .{0,40}(restatement|adjustment|revision|correction)"), t),
  consolidating = grepl(paste0("eliminations?|non.?guarantors?|guarantors?",
                               "|condensed consolidating",
                               "|parent (company )?only|parent.only",
                               "|schedule i+ ?[-(]"), t),
  bs_teq = grepl("total (equity|shareholders|stockholders|capitalization)", t),
  sc_unit = grepl(paste0("in (thousands|millions|billions)",
                         "|(thousands|millions|billions) of (u[.]?s[.]?)?dollars",
                         "|dollars in|\\((thousands|millions|billions)\\)",
                         "|(thousands|millions|billions)\\)"), t),
  ## ---- NEW: the statement separator and the page break ------------------
  ## The notes reference that closes every primary statement, and the printed
  ## page break. These are the brackets; nothing else in this script is new.
  sep  = grepl(paste0("accompanying.{0,30}notes?",
                      "|see notes? to .{0,40}(financial statements?|consolidated)",
                      "|notes? to (the )?(unaudited )?(condensed )?consolidated financial",
                      "|integral part of .{0,40}statements?"), t),
  pbrk = grepl("^ ?table of contents", t),
  ## THE REGISTRANT NAME LINE, printed directly above each statement title in
  ## a combined filing. Must be a name and nothing else: no digits, no verbs.
  regname = grepl(paste0("^ ?[a-z .,&'/-]{3,60}",
                         "(inc|corp|corporation|company|holdings|lp|llc|plc|trust|partners)",
                         "[.,]? ?$"), t) &
            !grepl(paste0("^ ?(the|a|an|this|these|our|its|see|notes?|table|item|part)[ .]",
                          "|accompanying|integral part"), t))

M <- cbind(M[, c("wrdsfname","nlines","line","form","set","text")], F)
M <- M[rowSums(F) > 0, ]
cat("marker rows:", nrow(M), " filings:", length(unique(M$wrdsfname)), "\n")
cat("separator markers:", sum(M$sep), "  page-break markers:", sum(M$pbrk), "\n")
sepf <- length(unique(M$wrdsfname[M$sep]))
cat(sprintf("filings with >=1 separator: %d (%.1f%%)\n",
            sepf, 100*sepf/length(unique(M$wrdsfname))))

## ---- parameters ----------------------------------------------------------
MAXLEN <- 900; MINLEN <- 8; PAD_UP <- 12; EXT <- 80; SCALE_BACK <- 60
TITLE_REACH <- 250; CLOSE_GAP <- 40; RECON_HEAD <- 25
NUM_WITHIN <- 40; NUM_MIN_AT_TOP <- 3; TITLE_CROWD <- 3; CROWD_WINDOW <- 45
FLOOR_SLACK <- 3; NNUM_MIN <- 8
## How far the statements SECTION may reach above and below the cash flow
## anchor. Measured on 011d's own picks, the income statement sits a median
## 242 lines from the cash flow statement and the balance sheet 283, with 80%
## of each inside 800. 1200 is deliberately generous: inside the section the
## structural test is relaxed, so the section wants to be big enough to hold
## the statements and no bigger.
SEC_REACH <- 1500
## The section always reaches at least this far, even when the filing prints no
## notes reference at all. Set from the measured spread: the income statement
## sits a median 205 lines from the cash flow anchor and the balance sheet 245,
## with the 90th percentile under 860.
MIN_REACH <- 600
## How many separators to walk back through on the way up. Four statements in
## the usual run (balance sheet, income, equity, cash flow) means crossing
## three separators to clear the top of the first one; one more for slack.
SEP_UP <- 3
## Lines of furniture to keep past the closing separator, so the last data row
## is never clipped by a separator that the HTML wrap placed early.
SEP_SLACK <- 4

TTL <- c("ttl_cf","ttl_is","ttl_bs","ttl_ci","ttl_eq")

## STRICT requirements - used for the cash flow ANCHOR, which must be right.
REQ_CF <- list(top = c("ttl_cf","cf_bop","cf_recon"),
               op = "cf_opX", inv = "cf_inv", fin = "cf_finX", close = "cf_end")
## RELAXED requirements - used only INSIDE the section. The dropped landmark
## in each case is the one that is optional in practice:
##   income  : EPS. Absent for wholly-owned subsidiaries and partnerships.
##   balance : total assets as a labelled line. Absent when the filer prints
##             the figure with an empty label cell (Amerisafe).
## What remains is the top, and the statement's defining content line. That is
## thin evidence document-wide and sufficient 200 lines from a located SCF.
REQ_IS <- list(top = c("ttl_is","ttl_ci","is_rev"), ni = "is_ni")
REQ_BS <- list(top = c("ttl_bs","bs_hasset","bs_tca"),
               liab = c("bs_tle","bs_teq","bs_hliab"))

OWN  <- c(cf = "ttl_cf", is = "ttl_is", bs = "ttl_bs")
ANCH <- list(cf = c("cf_bop","cf_recon"), is = "is_rev", bs = "bs_tca")
ENDF <- list(cf = "cf_end", is = "is_eps", bs = c("bs_tle","bs_teq"))
OTHER_CLOSE <- list(cf = c("is_eps","bs_tle","bs_teq"),
                    is = c("cf_end","bs_tle","bs_teq"),
                    bs = c("cf_end","is_eps"))

## ---- span builder --------------------------------------------------------
## Same bracketing as 011d, with one addition: a span may close at the next
## SEPARATOR as well as at a close marker. That is what removes the dependence
## on EPS and on a labelled total.
## PAGE BREAKS DO NOT CLOSE A STATEMENT, and the separator is a FALLBACK.
## Both learned from the first run, which took the first separator of any kind
## below the top:
##
##  1. a statement that prints across two pages carries a page break IN THE
##     MIDDLE of itself, so closing on one cuts it in half. The balance sheet
##     is worst hit because it is the statement most often split (assets on one
##     page, liabilities and equity on the next) - coverage fell to 36.8%.
##     Biomet's fits on one page, which is why the case did not show it.
##  2. preferring a separator over the real close marker moved almost every
##     boundary by a few lines for no gain, which buried the real differences
##     with 011d in noise (405 of 492 cash flow picks read as DIFFERENT at a
##     median of 11 lines).
##
## So: the close marker first, exactly as 011d finds it, and the notes
## reference only when there is no close marker at all. That is the no-EPS
## case and nothing else, which is the whole point - every pick that already
## worked is left alone.
build_span <- function(x, ty, S0, lo, hi, req, nv, ttl_any, own, seps, sep_close) {
  ends <- sort(unique(unlist(lapply(ENDF[[ty]], function(f) x$line[x[[f]]]))))
  oth  <- ttl_any[ttl_any >= S0 + MINLEN & !(ttl_any %in% own)]
  sam  <- own[own >= S0 + MINLEN]
  sam  <- sam[vapply(sam, function(t0) any(ends > S0 & ends < t0), logical(1))]
  nx   <- c(oth, sam)
  lim  <- min(c(S0 + MAXLEN, hi, if (length(nx)) min(nx) - 1 else Inf))
  if (!is.finite(lim) || lim <= S0 + MINLEN) return(NULL)

  cs <- ends[ends >= S0 + MINLEN & ends <= lim]
  how_end <- NA_character_
  if (length(cs)) {
    E <- cs[1]
    for (k in seq_along(cs)[-1]) if (cs[k] - E <= CLOSE_GAP) E <- cs[k] else break
    how_end <- "close"
  } else {
    ## no close marker exists - the no-EPS / unlabelled-total case. The printed
    ## bottom of the statement is the notes reference, so run to it.
    sp <- sep_close[sep_close >= S0 + MINLEN & sep_close <= lim]
    if (length(sp)) { E <- min(min(sp) + SEP_SLACK, lim); how_end <- "separator" }
    else if (ty == "bs") {
      ## LAST RESORT, BALANCE SHEET ONLY. United Continental labels no total on
      ## either page - both end on a bare "$ 37,353" - so there is no line to
      ## find and the statement simply runs to wherever the next one begins.
      ## 011d has this rule; dropping it from 011f is why United produced no
      ## balance sheet at all. It was risky in 011d because it applied across
      ## the whole document; here the span is already inside the section, which
      ## is what makes it safe. An unlabelled total is a balance-sheet habit,
      ## so it stays restricted to the balance sheet.
      E <- lim; how_end <- "boundary"
    } else return(NULL)
  }
  if (ty == "cf") {
    sup <- x$line[x$cf_sup]
    s2 <- sup[sup > E & sup <= min(E + EXT, lim)]
    if (length(s2)) E <- min(max(s2) + 12, lim)
  }
  ## left boundary: header and scale, never back over the previous statement
  prv <- c(ttl_any[ttl_any < S0],
           unlist(lapply(OTHER_CLOSE[[ty]], function(f) { v <- x$line[x[[f]]]; v[v < S0] })),
           seps[seps < S0 - 2])
  floor_ln <- max(c(if (length(prv)) max(prv) + FLOOR_SLACK else 1, lo, 1))
  S <- max(S0 - PAD_UP, floor_ln, 1)
  sc <- x$line[x$sc_unit]
  s3 <- sc[sc <= S0 + 4 & sc >= S0 - SCALE_BACK & sc >= floor_ln]
  if (length(s3)) S <- max(min(min(s3) - 2, S), floor_ln, 1)
  if (E - S + 1 < MINLEN) return(NULL)

  inw <- x$line >= S & x$line <= E
  got <- vapply(req, function(g)
    any(vapply(g, function(f) any(x[[f]][inw]), logical(1))), logical(1))
  hdr <- x$line >= S & x$line <= min(E, S + RECON_HEAD)
  nn  <- if (is.null(nv)) NA_integer_ else sum(nv >= S & nv <= E)
  data.frame(kind = ty, start = S, end = E, len = E - S + 1,
             how = if (S0 %in% own) "title" else "anchor", titled = S0 %in% own,
             how_end = how_end,
             complete = all(got) && !any(x$recon[hdr]) && !any(x$consolidating[hdr]) &&
                        (is.na(nn) || nn >= NNUM_MIN),
             nnum = nn,
             miss = paste(c(names(got)[!got],
                            if (any(x$recon[hdr])) "RECON",
                            if (any(x$consolidating[hdr])) "CONSOLIDATING",
                            if (!is.na(nn) && nn < NNUM_MIN) "figures"), collapse = ","),
             stringsAsFactors = FALSE)
}

tops_for <- function(x, ty, nv, ttl_all, lo = 1, hi = Inf) {
  own <- x$line[x[[OWN[[ty]]]]]
  anc <- sort(unique(unlist(lapply(ANCH[[ty]], function(f) x$line[x[[f]]]))))
  if (ty == "is") anc <- sort(unique(c(anc, x$line[x$ttl_ci])))
  if (length(anc) && length(own))
    anc <- anc[!vapply(anc, function(a) any(own <= a & own >= a - TITLE_REACH), logical(1))]
  tp <- sort(unique(c(own, anc)))
  tp <- tp[tp >= lo & tp <= hi]
  ## a title is only a title if figures follow and titles are not stacked below
  keep <- vapply(tp, function(S0) {
    figs <- is.null(nv) || sum(nv > S0 & nv <= S0 + NUM_WITHIN) >= NUM_MIN_AT_TOP
    crowd <- sum(ttl_all >= S0 - 5 & ttl_all <= S0 + CROWD_WINDOW) >= TITLE_CROWD
    figs && !crowd
  }, logical(1))
  list(tops = tp[keep], own = own)
}

## ---- per filing ----------------------------------------------------------
locate <- function(x) {
  x <- x[order(x$line), ]
  f  <- x$wrdsfname[1]
  nv <- NUM[[f]]
  ttl_all <- sort(unique(unlist(lapply(TTL, function(fl) x$line[x[[fl]]]))))
  seps      <- sort(unique(c(x$line[x$sep], x$line[x$pbrk])))
  sep_close <- sort(unique(x$line[x$sep]))

  ## STEP 1 - the cash flow anchor, strict, chosen on its own merits.
  ## 011d picks the cash flow span jointly with the other two; here it has to
  ## stand alone, which is what the method claims it can do.
  tc <- tops_for(x, "cf", nv, ttl_all)
  cfc <- do.call(rbind, lapply(tc$tops, function(S0)
    build_span(x, "cf", S0, 1, Inf, REQ_CF, nv, ttl_all, tc$own, seps, sep_close)))
  if (!is.null(cfc)) {
    cfc <- cfc[cfc$len >= MINLEN, ]
    cfc <- cfc[order(cfc$end, -cfc$len), ]
    cfc <- cfc[!duplicated(cfc$end), ]
  }
  if (is.null(cfc) || !nrow(cfc)) return(NULL)
  cfc$tier <- ifelse(cfc$complete & cfc$titled, 1L, ifelse(cfc$complete, 2L, 3L))
  ## tier, then the longest - the consolidated statement over a condensed or
  ## parent-only one, which is 011d's dead-heat rule.
  A <- cfc[order(cfc$tier, -cfc$len), ][1, ]

  ## STEP 2 - the statements SECTION around the anchor.
  ##
  ## WALK NOTES REFERENCES, NOT PAGE BREAKS. Page breaks outnumber notes
  ## references six to one in this sample (13,747 rows against 2,302), so
  ## stepping back through "separators" of any kind crossed four PAGE BREAKS -
  ## roughly 100 lines - rather than clearing four statements. The section
  ## collapsed to a median 786 lines and cut the balance sheet out of itself:
  ## coverage fell to 30.6% and 319 filings got no balance sheet candidate at
  ## all. Only the notes reference marks the end of a STATEMENT.
  ##
  ## And the section is bounded below AND above regardless of what the
  ## separators say, because a filing may print none: at least MIN_REACH so
  ## the statements fit, at most SEC_REACH so the relaxed test stays safe.
  up <- sep_close[sep_close < A$start]
  lo <- if (length(up)) min(rev(sort(up))[min(SEP_UP, length(up))], A$start - MIN_REACH)
        else A$start - SEC_REACH
  lo <- max(1, lo, A$start - SEC_REACH)
  dn <- sep_close[sep_close > A$end]
  hi <- if (length(dn)) max(dn[min(2, length(dn))], A$end + MIN_REACH)
        else A$end + SEC_REACH
  hi <- min(hi, A$end + SEC_REACH)

  ## STEP 3 - the other two, RELAXED, inside the section only
  out <- list(A)
  for (ty in c("is", "bs")) {
    tt <- tops_for(x, ty, nv, ttl_all, lo, hi)
    req <- if (ty == "is") REQ_IS else REQ_BS
    z <- do.call(rbind, lapply(tt$tops, function(S0)
      build_span(x, ty, S0, lo, hi, req, nv, ttl_all, tt$own, seps, sep_close)))
    if (is.null(z) || !nrow(z)) next
    z <- z[z$len >= MINLEN, ]
    if (!nrow(z)) next
    z <- z[order(z$end, -z$len), ]; z <- z[!duplicated(z$end), ]
    z$tier <- ifelse(z$complete & z$titled, 1L, ifelse(z$complete, 2L, 3L))
    mid <- (z$start + z$end) / 2
    z <- z[order(z$tier, abs(mid - (A$start + A$end) / 2)), ]
    out[[length(out) + 1]] <- z
  }
  o <- do.call(rbind, out)
  o$wrdsfname <- f; o$form <- x$form[1]; o$set <- x$set[1]
  o$sec_lo <- lo; o$sec_hi <- hi

  ## ---- WHICH REGISTRANT does each span belong to? ------------------------
  ## Two independent signals, because they fail differently. The NAME can
  ## identify the wrong registrant if the match is loose; the BOUNDARY count
  ## can only decline to pick, never mislabel. Both are recorded and neither
  ## is used to drop a candidate here - the model decides, with these in hand.
  nmln <- x$line[x$regname]
  o$reg_name <- vapply(o$start, function(s) {
    v <- nmln[nmln <= s + 6 & nmln >= s - 60]      # the name sits just above
    if (!length(v)) NA_character_ else x$text[match(max(v), x$line)]
  }, character(1))
  ## How many OTHER cash flow statement titles sit between this span and the
  ## anchor. One or more means a different registrant's whole statement set
  ## intervenes, which is what put United Continental Holdings' income
  ## statement (net income 1,132) beside United Airlines, Inc.'s cash flow
  ## statement (net income 1,114) - two companies in one triple.
  cft <- x$line[x$ttl_cf]
  o$cf_titles_between <- vapply(seq_len(nrow(o)), function(i) {
    if (o$kind[i] == "cf") return(0L)
    a <- min(o$start[i], A$start); b <- max(o$end[i], A$end)
    sum(cft > a & cft < b & !(cft >= A$start & cft <= A$end))
  }, integer(1))

  o <- do.call(rbind, lapply(split(o, o$kind), function(w) {
    ## candidates from the anchor's OWN registrant run first, then tier, then
    ## distance. Ranking only - nothing is discarded.
    w <- w[order(w$cf_titles_between > 0, w$tier,
                 abs((w$start + w$end)/2 - (A$start + A$end)/2)), ]
    w$rank <- seq_len(nrow(w)); w }))
  rownames(o) <- NULL
  o
}

C <- do.call(rbind, lapply(split(M, M$wrdsfname), locate)); rownames(C) <- NULL
pk <- C[C$rank == 1, ]
allf <- length(unique(M$wrdsfname))

cat("\n=== ANCHORED LOCATOR ===\n")
cat("filings with a cash flow anchor:", length(unique(C$wrdsfname[C$kind == "cf"])),
    "of", allf, "\n")
cat("\nsection size (lines) around the anchor:\n")
print(summary(pk$sec_hi[pk$kind == "cf"] - pk$sec_lo[pk$kind == "cf"] + 1))

cat("\n=== picks, by statement ===\n")
for (ty in c("cf","is","bs")) {
  z <- pk[pk$kind == ty, ]
  cat(sprintf("%-3s  located %3d (%5.1f%%)  complete %5.1f%%  titled %5.1f%%  med len %3.0f  closed on separator %5.1f%%\n",
      ty, nrow(z), 100*nrow(z)/allf, 100*mean(z$complete), 100*mean(z$titled),
      median(z$len), 100*mean(z$how_end == "separator")))
}

cat("\n=== distance of is/bs pick from the cash flow pick ===\n")
cfm <- setNames((pk$start[pk$kind=="cf"] + pk$end[pk$kind=="cf"])/2, pk$wrdsfname[pk$kind=="cf"])
for (ty in c("is","bs")) {
  z <- pk[pk$kind == ty, ]
  d <- abs((z$start + z$end)/2 - cfm[z$wrdsfname])
  cat(sprintf("%-3s  median %4.0f  p90 %5.0f  max %6.0f   >3000 lines: %.1f%%\n",
      ty, median(d, na.rm=TRUE), quantile(d, .9, na.rm=TRUE), max(d, na.rm=TRUE),
      100*mean(d > 3000, na.rm=TRUE)))
}

## ---- COMPARISON WITH 011d, which is the point of the script --------------
## Skipped on a sample that has not been through 011d - a fresh holdout run
## wants the anchors, not the comparison with the older approach.
if (!file.exists(paste0(SP, "pick9.rds"))) {
  cat("\nno pick9.rds in", SP, "- skipping the 011d comparison\n")
  saveRDS(C,  paste0(SP, "chunks_anch.rds"))
  saveRDS(pk, paste0(SP, "pick_anch.rds"))
  cat("saved chunks_anch.rds and pick_anch.rds\n")
  quit(save = "no")
}
old <- readRDS(paste0(SP, "pick9.rds"))
k <- function(d) paste(d$wrdsfname, d$kind)
cmp <- merge(old[, c("wrdsfname","kind","start","end","len","complete","titled")],
             pk[,  c("wrdsfname","kind","start","end","len","complete","titled","how_end")],
             by = c("wrdsfname","kind"), all = TRUE, suffixes = c(".old",".new"))
## A DIFFERENT BLOCK AND A DIFFERENT BOUNDARY ARE NOT THE SAME DISAGREEMENT,
## and conflating them hides the only comparison that matters. Overlap decides:
## two spans that overlap are the same statement bracketed differently, which
## is a question of a few lines of header or footer. Two spans that do not
## overlap at all are different tables, and one of them is wrong.
ov <- with(cmp, pmin(end.old, end.new) - pmax(start.old, start.new) + 1)
sh <- with(cmp, pmin(len.old, len.new))
cmp$overlap <- ifelse(is.na(ov), NA_real_, pmax(0, ov) / sh)
cmp$status <- with(cmp, ifelse(is.na(start.old) & !is.na(start.new), "011f only",
                        ifelse(!is.na(start.old) & is.na(start.new), "011d only",
                        ifelse(overlap >= 0.9, "same block",
                        ifelse(overlap > 0,    "same, shifted",
                                               "DIFFERENT BLOCK")))))
cat("\n=== 011f vs 011d, per statement ===\n")
print(table(cmp$kind, cmp$status))
cat("\n=== the disagreements that matter: non-overlapping picks ===\n")
dd <- cmp[cmp$status == "DIFFERENT BLOCK" & !is.na(cmp$start.old), ]
if (nrow(dd)) {
  dd$gap <- abs(dd$start.old - dd$start.new)
  print(tapply(dd$gap, dd$kind, summary))
  cat("\nof those, how many did 011d place >3000 lines away:\n")
  print(table(dd$kind[dd$gap > 3000]))
  cat("\nand which side scored COMPLETE on them:\n")
  print(table(kind = dd$kind, d011d = dd$complete.old, f011f = dd$complete.new))
}
cat("\n=== closes that needed the separator (the no-close-marker cases) ===\n")
print(table(pk$kind, pk$how_end))
saveRDS(C,   paste0(SP, "chunks_anch.rds"))
saveRDS(pk,  paste0(SP, "pick_anch.rds"))
saveRDS(cmp, paste0(SP, "anch_vs_011d.rds"))
cat("\nsaved chunks_anch.rds / pick_anch.rds / anch_vs_011d.rds\n")
