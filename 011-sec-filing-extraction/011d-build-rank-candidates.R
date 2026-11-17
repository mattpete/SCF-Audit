## ============================================================================
## 011d  BUILD AND RANK CANDIDATES         step 011 (locate), substep d
## Brackets each statement between its own top and bottom anchor, keeps only
## spans that carry the statement's whole required structure, then picks the
## (bs, is, cf) TRIPLE that sits together in the filing. Rank 1 is the pick;
## the rest are fallbacks 011e can walk.
## In : cache/markers4_raw.rds (011b), cache/numlines.rds (011c)
## Out: cache/chunks8.rds (all candidates, ranked), cache/pick9.rds (rank 1)
## ============================================================================
## PASS 3: STRUCTURE, NOT DENSITY.
##
## WHY THIS WAS REWRITTEN (Matt, 2026-11-17). The previous version generated
## candidate windows and ranked them by "complete, then SMALLEST, then densest",
## on the reasoning that ranking only had to be cheap because a validate-and-
## retry loop in 011e would walk down the ranks until the block really was a
## statement. That loop no longer fires - 011e's validator was loosened - so
## nothing was catching the bad picks, and the cost-minimising sort has a
## systematic bias: a statement's TAIL is shorter and denser than the whole
## statement, because working-capital lines are nothing but numbers. So the
## sort preferred fragments by construction.
##
## Evergy Kansas Central 10-Q, 2021-06-30, is the case that killed it. The
## chosen cash flow block started at "Prepaid expenses and other current
## assets" - no net income, no depreciation, none of the reconciling
## adjustments - and it scored complete TRUE, because completeness tested for
## the three section subtotals and the cash roll-forward, all of which sit in
## the BOTTOM half of the statement. The whole-statement candidate for that
## filing existed and ranked 22nd:
##
##     rank  lines          len  titled
##     1     43982-44030     49  no      <- picked
##     22    43854-44024    171  yes     <- the actual statement
##
## Three rules follow, and none of them is about size:
##
## 1. BRACKET, DO NOT SCORE. A statement has a findable top and a findable
##    bottom. Take everything between them. Length is an OUTPUT, never a
##    criterion, and numeric density is not used at all.
##
## 2. REQUIRE THE WHOLE STRUCTURE, TOP INCLUDED. The completeness test now
##    demands the statement's opening as well as its close, so a tail cannot
##    pass. For the cash flow statement that means the operating section
##    header must be inside the span, not merely the operating subtotal.
##
## 3. THE THREE STATEMENTS ARE ADJACENT, SO PICK THEM TOGETHER. Measured on
##    the 490 filings with all three picks, only 2.2% had them within 200
##    lines of each other and 38.8% were picked more than 5,000 lines apart -
##    the old per-statement ranking threw that constraint away. Choosing the
##    TRIPLE that sits together resolves multi-registrant filings for free:
##    Evergy files one 10-Q holding statements for Evergy Inc, Evergy Metro
##    and Evergy Kansas Central, and each registrant's three statements are
##    together, so the tight triple is one registrant's set.
## ============================================================================
SP  <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
M   <- readRDS(paste0(SP, "markers4_raw.rds"))
NUM <- readRDS(paste0(SP, "numlines.rds"))
t <- M$text

## ---- concept flags (a line can carry more than one) ----
F <- data.frame(
  ttl_cf = grepl("statements? of .{0,40}cash flows?", t),
  ttl_is = grepl("statements? of .{0,40}(income|operations|earnings)", t),
  ttl_bs = grepl("balance sheets?|statements? of .{0,40}financial (position|condition)", t),
  ttl_ci = grepl("statements? of .{0,40}comprehensive income", t),
  ## We never extract the equity statement, but it is printed between the
  ## income statement and the cash flow statement, so it has to be visible as
  ## a BOUNDARY or the block before it gets swallowed (Donnkenny).
  ttl_eq = grepl(paste0("statements? of .{0,40}(share|stock)holders.{0,3} ?(equity|deficit)",
                        "|statements? of .{0,40}changes in .{0,20}equity"), t),
  ## THE OPERATING SECTION BANNER - the top of the cash flow statement proper,
  ## and the landmark a clipped statement is missing.
  ##
  ## It must be separated from the operating SUBTOTAL, which sits at the BOTTOM
  ## of the same section and is worded almost identically. Excluding "net cash"
  ## and "provided by" is not enough: Evergy Kansas Central writes its subtotal
  ## as "Cash Flows from Operating Activities 246.1 211.6", which carries none
  ## of those words, so a word-only rule reads it as the banner and the span
  ## opens at the bottom of the section - the exact bug this rewrite is for.
  ##
  ## A HEADER CARRIES NO FIGURES. That is the discriminator, and it is clean:
  ## on Evergy it flags lines 43874 and 43954 (banners) and rejects 43914 and
  ## 43994 (subtotals), which is right in every case.
  cf_bop = grepl("cash flows? (from|used)|^operating activities|operating activities:", t) &
           grepl("operating", t) & !grepl("[0-9]", t) &
           !grepl("net cash|^net |provided by|used in [a-z]* ?activities", t),
  ## "Adjustments to reconcile net income to net cash provided by operating
  ## activities" is the other top-of-section landmark: it sits immediately
  ## below the income starting point and above the first adjustment. Present in
  ## 79.4% of filings against 74.8% for the banner, and the union is 90.8%.
  cf_recon = grepl("adjustments? to reconcile", t),
  cf_opX = grepl("operating activit", t),
  cf_inv = grepl("investing activit", t),
  cf_finX = grepl("financing activit", t),
  cf_end = grepl("cash.{0,40}end of|end of (the )?(period|year|quarter)|net (increase|decrease|change).{0,20}cash", t),
  cf_sup = grepl("supplemental (disclosure|cash|information)", t),
  ## The revenue line, i.e. the top of the income statement. The second half
  ## is for financial firms, which have no "net sales" or "total revenues"
  ## line to find - a bank opens with interest income, an insurer with
  ## premiums. Financials are in scope on this track, so without these the
  ## top-of-statement landmark is simply unfindable for the whole sector.
  is_rev = grepl(paste0("(net|total) (sales|revenues?)",
                        ## the bare line, and the line carrying its amounts.
                        ## Requiring a digit, $ or ( right after the word is
                        ## what keeps "Revenue Recognition" and the ASC 606
                        ## reference out - see the note in 011b.
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
  ## "total deposits" stands in for the current-asset subtotal on a bank's
  ## balance sheet, which has no current/non-current split at all.
  bs_tca = grepl("total current (assets|liabilities)|total deposits", t),
  bs_tle = grepl("total liabilities and|total liabilities, |total equity and liabilities", t),
  ## the two section headings on a line of their own. The only landmarks that
  ## survive a filer printing its totals with an empty label cell - see the
  ## note in 011b. Anchored, and with no "total" allowed, so that
  ## "Total liabilities and shareholders equity" does NOT satisfy them.
  bs_hasset = grepl("^ ?assets ?:? ?$", t),
  bs_hliab  = grepl("^ ?liabilities( and .{0,40}equity)? ?:? ?$", t),
  ## a restatement reconciliation's column captions
  recon = grepl(paste0("as (previously )?(reported|adjusted|restated|revised)",
                       "|previously reported",
                       "|effect of .{0,40}(restatement|adjustment|revision|correction)"), t),
  bs_teq = grepl("total (equity|shareholders|stockholders|capitalization)", t),
  sc_unit = grepl(paste0("in (thousands|millions|billions)",
                         "|(thousands|millions|billions) of (u[.]?s[.]?)?dollars",
                         "|dollars in|\\((thousands|millions|billions)\\)",
                         "|(thousands|millions|billions)\\)"), t))
M <- cbind(M[, c("wrdsfname","nlines","line","form","set")], F)
M <- M[rowSums(F) > 0, ]
cat("marker rows:", nrow(M), " filings:", length(unique(M$wrdsfname)), "\n")

MAXLEN <- 900; MINLEN <- 8; PAD_UP <- 12; EXT <- 80
## How far above the top anchor to hunt for the scale line. The pull-up is now
## BLOCKED at the previous statement's title, which is what stops a block
## swallowing its neighbour: Donnkenny's cash flow block used to open on the
## Statement of Stockholders' Equity because the pull-up walked straight past
## that statement's title. 53 of 498 blocks (10.6%) had that problem.
SCALE_BACK <- 60
## A content anchor (operating banner / revenue line / current-asset subtotal)
## is only used as a top when the statement's own TITLE is not within this
## many lines above it. Titles are better tops; this is the fallback.
TITLE_REACH <- 250

## REQUIRED STRUCTURE, as a list of GROUPS. Every group must be represented by
## at least one marker inside the span. Each group is one landmark of the
## statement, and crucially the FIRST group in each is the statement's TOP -
## which is what a clipped statement cannot satisfy.
##
##   cash flow : a top (title / operating banner / reconcile line), then the
##               operating, investing and financing sections, then the close.
##   income    : a top (title or the revenue line), down to net income and EPS.
##   balance   : a top (title or the current-asset subtotal), total assets, and
##               a close of total equity or total liabilities-and-equity.
REQ <- list(
  cf = list(top = c("ttl_cf","cf_bop","cf_recon"),
            op  = "cf_opX", inv = "cf_inv", fin = "cf_finX", close = "cf_end"),
  is = list(top = c("ttl_is","is_rev"),
            ni  = "is_ni", close = "is_eps", spine = c("is_rev","is_tax")),
  bs = list(top    = c("ttl_bs","bs_hasset","bs_tca"),
            assets = c("bs_ta","bs_hasset","bs_tca"),
            ## the liability and equity side must be in the span. Either its
            ## closing total, or - where the filer labels no total - its
            ## section heading.
            liab   = c("bs_tle","bs_teq","bs_hliab")))

TTL  <- c("ttl_cf","ttl_is","ttl_bs","ttl_ci","ttl_eq")
## How far the statement's CLOSING BLOCK may run. The close is contiguous -
## net change in cash, then the beginning and ending balances, then the
## supplemental disclosures - so a close marker far below the previous one
## belongs to a different statement, not to this one. Taking the last close
## in range without this test stretched Select Comfort's cash flow block from
## 52 to 836 lines, swallowing everything down to the next statement that
## happened to carry a cash line.
CLOSE_GAP <- 40
## How far into a span to look for restatement-reconciliation column captions.
## Header region only, so a statement that merely references a revision in a
## note further down is not thrown away for it.
RECON_HEAD <- 25
## A TITLE IS ONLY A TITLE IF FIGURES FOLLOW IT. A table of contents lists
## "Consolidated Statements of Income ..." word for word, and so does an MD&A
## heading and an XBRL role definition, so no word pattern can tell them from
## the real heading. What separates them is what comes next: the real heading
## is followed within a few lines by the column dates and the first amounts,
## while a contents entry is followed by more contents entries.
##
## Citizens Holding is the case. Its income statement block opened at line 105
## - the table of contents - and ran 200 lines to the real statement's EPS,
## scoring complete the whole way because the span happened to contain every
## landmark it needed. Requiring figures below the anchor removes the contents
## entry without touching the real heading.
NUM_WITHIN <- 40   # look this far below the anchor
NUM_MIN_AT_TOP <- 3   # and expect at least this many numeric lines
## The figures test alone does not catch Citizens Holding, because its table of
## contents is immediately followed by the balance sheet, so figures DO appear
## below it. The second signature is the one that identifies a contents block
## for what it is: it stacks the statement titles together. Citizens lists
## Condition, Income, Comprehensive Income and Cash Flows inside eight lines.
## Real statements are separated by their own bodies.
TITLE_CROWD <- 3    # this many titles in the window below means a contents list
CROWD_WINDOW <- 45
## HTML-to-text wraps a label away from its amount, so the previous
## statement's last VALUE can sit a line or two below its last label. Without
## this slack the pull-up floor lands on that orphaned amount and the block
## opens on it - Innovate Biopharmaceuticals' "(deficit) $ 13,285,014".
FLOOR_SLACK <- 3
OWN  <- c(cf = "ttl_cf", is = "ttl_is", bs = "ttl_bs")
## Where a span may START when the statement's own title is missing. These are
## tops only - never a subtotal, which is how the old version opened spans at
## the bottom of the operating section.
ANCH <- list(cf = c("cf_bop","cf_recon"), is = "is_rev", bs = "bs_tca")
## The statement's close, as a PRIMARY landmark and a FALLBACK. The split
## matters for the balance sheet: "total assets" is a usable stopping point
## when nothing better exists, but it is NOT the close - assets and
## liabilities often sit on facing pages, so ending there drops the whole
## liability and equity side. Using it as a close also broke the
## continuation test above, which asks whether the statement has closed yet
## and so must be given the real close, not a halfway landmark.
ENDF   <- list(cf = "cf_end", is = "is_eps", bs = c("bs_tle","bs_teq"))
## NOT bs_hliab. The "Liabilities and shareholders equity" heading OPENS the
## second half of the balance sheet - treating it as a close ended Amerisafe's
## span at "Liabilities:" and dropped the entire liability and equity side.
## It belongs in the required structure as proof that side is present, which
## is where it sits in REQ, and nowhere near the close.
ENDF_F <- list(cf = character(0), is = character(0), bs = "bs_ta")
## The OTHER statements' closing lines, used as a floor on how far the header
## pull-up may reach back. Flooring at the previous TITLE is not enough: the
## previous statement's own body sits between its title and ours, so the
## pull-up still swallows its last few lines. Citizens Holding's income
## statement opened on the tail of the balance sheet above it, and Innovate
## Biopharmaceuticals' opened on "(deficit) $ 13,285,014" for the same reason.
OTHER_CLOSE <- list(cf = c("is_eps","bs_tle","bs_teq"),
                    is = c("cf_end","bs_tle","bs_teq"),
                    bs = c("cf_end","is_eps"))

build <- function(x) {
  x <- x[order(x$line), ]
  L <- function(fl) x$line[x[[fl]]]
  nv <- NUM[[x$wrdsfname[1]]]          # numeric line numbers for this filing
  ttl_all <- sort(unique(unlist(lapply(TTL, L))))
  ## is this anchor a real heading, or an entry in a contents list?
  real_top <- function(S0) {
    figs <- is.null(nv) ||               # no numeric map: do not punish it
            sum(nv > S0 & nv <= S0 + NUM_WITHIN) >= NUM_MIN_AT_TOP
    crowd <- sum(ttl_all >= S0 - 5 & ttl_all <= S0 + CROWD_WINDOW) >= TITLE_CROWD
    figs && !crowd
  }
  ttl_any <- sort(unique(unlist(lapply(TTL, L))))
  out <- list()
  for (ty in c("cf","is","bs")) {
    own <- L(OWN[[ty]])
    anc <- sort(unique(unlist(lapply(ANCH[[ty]], L))))
    ## content anchors that are not already covered by a title just above
    if (length(anc) && length(own))
      anc <- anc[!vapply(anc, function(a)
        any(own <= a & own >= a - TITLE_REACH), logical(1))]
    tops <- sort(unique(c(own, anc)))
    tops <- tops[vapply(tops, real_top, logical(1))]
    if (!length(tops)) next

    ends <- sort(unique(unlist(lapply(ENDF[[ty]], L))))
    endf <- sort(unique(unlist(lapply(ENDF_F[[ty]], L))))
    if (!length(ends) && !length(endf)) next
    sup <- L("cf_sup"); sc <- L("sc_unit")

    for (S0 in tops) {
      ## ---- right boundary: never cross into the NEXT statement -----------
      ## A title of a DIFFERENT statement is a hard stop. A title of the SAME
      ## statement is ambiguous: it is either a page-break continuation
      ## ("...Statements of Cash Flows (continued)") or the next registrant's
      ## copy in a combined filing. Stopping at both clipped 265 cash flow
      ## spans mid-statement, which showed up as the investing and financing
      ## sections going missing from spans that plainly had an operating one.
      ## The close marker tells them apart: if the statement has not reached
      ## its close yet, the title is a continuation, so keep going.
      oth <- ttl_any[ttl_any >= S0 + MINLEN & !(ttl_any %in% own)]
      sam <- own[own >= S0 + MINLEN]
      sam <- sam[vapply(sam, function(t0) any(ends > S0 & ends < t0), logical(1))]
      nx  <- c(oth, sam)
      lim <- min(c(S0 + MAXLEN, if (length(nx)) min(nx) - 1 else Inf))
      cs <- ends[ends >= S0 + MINLEN & ends <= lim]
      ## only reach for the halfway landmark when the real close is unavailable
      if (!length(cs)) cs <- endf[endf >= S0 + MINLEN & endf <= lim]
      ## LAST RESORT, BALANCE SHEET ONLY: close at the boundary. When the filer
      ## labels no total at all there is no line to find, so the statement
      ## simply runs to wherever the next statement begins - which the boundary
      ## already knows. Without this, Amerisafe's balance sheet produced no
      ## candidate whatsoever, because it prints total assets as a bare figure.
      ##
      ## Restricted to the balance sheet deliberately. An unlabelled total is a
      ## balance-sheet habit; the cash flow statement closes on "net increase
      ## in cash" and the income statement on EPS, both of which are named.
      ## Allowing it for all three only manufactured low-confidence spans for
      ## filings that had correctly produced none, which showed up as the cash
      ## flow pick's completeness falling from 95.0% to 92.3%.
      if (!length(cs) && ty == "bs" && is.finite(lim) && lim > S0 + MINLEN) cs <- lim
      if (!length(cs)) next
      ## Walk the closing block instead of taking either end of it. The first
      ## close marker alone stops the block short, because "net decrease in
      ## cash" precedes the beginning and ending balances; the last one in
      ## range runs into whatever statement comes next. So start at the first
      ## and extend while the markers stay contiguous.
      E <- cs[1]
      for (k in seq_along(cs)[-1]) {
        if (cs[k] - E <= CLOSE_GAP) E <- cs[k] else break
      }
      if (ty == "cf") {
        s2 <- sup[sup > E & sup <= min(E + EXT, lim)]
        if (length(s2)) E <- min(max(s2) + 12, lim)
      }
      how <- if (S0 %in% own) "title" else "anchor"

      ## ---- left boundary: header and SCALE, but do NOT cross back over
      ## the previous statement's title.
      prv <- c(ttl_any[ttl_any < S0],
               unlist(lapply(OTHER_CLOSE[[ty]], function(f) {
                 v <- L(f); v[v < S0] })))
      floor_ln <- if (length(prv)) max(prv) + FLOOR_SLACK else 1
      S <- max(S0 - PAD_UP, floor_ln, 1)
      s3 <- sc[sc <= S0 + 4 & sc >= S0 - SCALE_BACK & sc >= floor_ln]
      if (length(s3)) S <- max(min(min(s3) - 2, S), floor_ln, 1)

      ## every REQUIRED GROUP must be represented by at least one marker
      inw <- x$line >= S & x$line <= E
      got <- vapply(REQ[[ty]], function(g)
        any(vapply(g, function(f) any(x[[f]][inw]), logical(1))), logical(1))
      ## A RESTATEMENT RECONCILIATION IS NOT THE STATEMENT. Its column
      ## captions sit in the header, so only the top of the span is tested -
      ## a primary statement can legitimately mention a revision further down
      ## in a note reference, and this must not reject it for that.
      is_recon <- any(x$recon[x$line >= S & x$line <= min(E, S + RECON_HEAD)])
      out[[length(out)+1]] <- data.frame(kind=ty, start=S, end=E, len=E-S+1,
        how=how, titled=S0 %in% own, complete=all(got) && !is_recon,
        nreq=sum(got),
        miss=paste(c(names(got)[!got], if (is_recon) "RECON"), collapse=","),
        stringsAsFactors=FALSE)
    }
  }
  if (!length(out)) return(NULL)
  o <- do.call(rbind, out)
  o <- o[o$len >= MINLEN, ]
  if (!nrow(o)) return(NULL)
  ## a top anchor can bracket to the same close twice; keep the widest
  o <- o[order(o$kind, o$end, -o$len), ]
  o <- o[!duplicated(paste(o$kind, o$end)), ]
  o$wrdsfname <- x$wrdsfname[1]; o$form <- x$form[1]; o$set <- x$set[1]
  o
}

C <- do.call(rbind, lapply(split(M, M$wrdsfname), build)); rownames(C) <- NULL

## Numeric lines. Density is NOT used to choose anything any more, but the
## COUNT is a structural requirement: a financial statement has figures in it.
##
## This is what catches the XBRL taxonomy-definition blocks. A filing's XBRL
## section repeats each statement twice - once as a rendering carrying values
## and once as a definition listing element names with no values at all - and
## the definition block satisfies every word-based landmark, because it is
## built from the statement's own line labels. Bloomia Holdings' chosen income
## statement had nnum = 0: a 104-line "income statement" containing not one
## number. Marker patterns can never exclude that, since the words are right.
NNUM_MIN <- 8
C$nnum <- NA_integer_
for (f in unique(C$wrdsfname)) {
  v <- NUM[[f]]; if (is.null(v)) next
  i <- which(C$wrdsfname == f)
  C$nnum[i] <- vapply(i, function(j) sum(v >= C$start[j] & v <= C$end[j]), integer(1))
}
C$dens <- C$nnum / pmax(C$len, 1)
C$has_figures <- is.na(C$nnum) | C$nnum >= NNUM_MIN     # NA = no map, don't punish
C$complete <- C$complete & C$has_figures
C$miss[!C$has_figures] <- ifelse(nzchar(C$miss[!C$has_figures]),
                                 paste0(C$miss[!C$has_figures], ",figures"), "figures")
allf <- length(unique(M$wrdsfname))

cat("\n=== candidates:", nrow(C), " of which complete:", sum(C$complete), "\n")
cat("\n=== COVERAGE: filings with >=1 COMPLETE candidate ===\n")
for (ty in c("cf","is","bs")) {
  x <- C[C$kind == ty & C$complete, ]
  n <- length(unique(x$wrdsfname))
  cat(sprintf("%-3s : %3d/%3d (%.1f%%)", ty, n, allf, 100*n/allf))
  for (fm in c("10-K","10-Q")) {
    tot <- length(unique(M$wrdsfname[M$form == fm]))
    got <- length(unique(x$wrdsfname[x$form == fm]))
    cat(sprintf("   %s %.1f%%", fm, 100*got/tot))
  }
  cat("\n")
}

## ---- WHICH LANDMARK IS MISSING when a span is not complete ---------------
## Printed rather than inferred: a coverage drop has to be attributable to a
## named landmark, or there is no way to tell a real gap in the filing from a
## pattern of ours that does not match how that filer writes the line.
cat("\n=== incomplete spans: which required group is absent ===\n")
for (ty in c("cf","is","bs")) {
  z <- C[C$kind == ty & !C$complete, ]
  if (!nrow(z)) next
  cat(sprintf("%-3s  %d incomplete spans\n", ty, nrow(z)))
  print(head(sort(table(z$miss), decreasing = TRUE), 6))
}
cat("\n=== filings with NO complete span, by statement ===\n")
for (ty in c("cf","is","bs")) {
  has <- unique(C$wrdsfname[C$kind == ty & C$complete])
  none <- setdiff(unique(M$wrdsfname), has)
  cat(sprintf("%-3s  %3d filings\n", ty, length(none)))
}

## ---- TIER, then the TRIPLE -----------------------------------------------
## Quality tier is ordinal and dominates everything: a complete titled span
## beats a complete untitled one, which beats an incomplete one. Only WITHIN
## a tier does position matter, and there the rule is adjacency, not size.
C$tier <- ifelse(C$complete & C$titled, 1L, ifelse(C$complete, 2L, 3L))
C$mid  <- (C$start + C$end) / 2

## For each cash flow candidate, find the best is and bs by (tier, distance)
## and score the resulting triple. The cash flow statement leads because it is
## the one the project is about, and whichever registrant's cash flow statement
## we take, we want ITS income statement and balance sheet beside it.
best_near <- function(z, at) {
  if (!nrow(z)) return(list(i = NA_integer_, tier = 4L, d = 0))
  o <- order(z$tier, abs(z$mid - at))
  list(i = o[1], tier = z$tier[o[1]], d = abs(z$mid[o[1]] - at))
}

choose_triple <- function(z) {
  cf <- z[z$kind == "cf", ]; is <- z[z$kind == "is", ]; bs <- z[z$kind == "bs", ]
  if (!nrow(cf)) {                      # no cash flow span: fall back per kind
    sel <- do.call(rbind, lapply(list(is, bs), function(w)
      if (nrow(w)) w[order(w$tier, -w$len), ][1, ] else NULL))
    return(sel)
  }
  sc <- vapply(seq_len(nrow(cf)), function(j) {
    a <- best_near(is, cf$mid[j]); b <- best_near(bs, cf$mid[j])
    ## tiers dominate; the spread of the triple breaks ties; a longer cash
    ## flow span wins a dead heat (the consolidated statement over a condensed
    ## or parent-only one).
    (cf$tier[j] + a$tier + b$tier) * 1e9 + (a$d + b$d) * 1e3 - cf$len[j]
  }, numeric(1))
  j <- which.min(sc)
  a <- best_near(is, cf$mid[j]); b <- best_near(bs, cf$mid[j])
  rbind(cf[j, ],
        if (!is.na(a$i)) is[a$i, ] else NULL,
        if (!is.na(b$i)) bs[b$i, ] else NULL)
}

sel <- do.call(rbind, lapply(split(C, C$wrdsfname), choose_triple))
sel$chosen <- TRUE
key <- function(d) paste(d$wrdsfname, d$kind, d$start, d$end)
C$chosen <- key(C) %in% key(sel)

## centre of the chosen triple, so the FALLBACKS are also ranked by adjacency
ctr <- tapply(sel$mid, sel$wrdsfname, median)
C$ctr <- as.numeric(ctr[C$wrdsfname])
C <- do.call(rbind, lapply(split(C, paste(C$wrdsfname, C$kind)), function(z) {
  z <- z[order(-z$chosen, z$tier, abs(z$mid - z$ctr), -z$len), ]
  z$rank <- seq_len(nrow(z))
  z }))
rownames(C) <- NULL
pk <- C[C$rank == 1, ]

cat("\n=== chosen chunk ===\n")
for (ty in c("cf","is","bs")) {
  x <- pk[pk$kind == ty, ]
  cat(sprintf("%-3s  len med %3.0f  p90 %3.0f   titled %.1f%%   complete %.1f%%   tier1 %.1f%%\n",
      ty, median(x$len), quantile(x$len,.9), 100*mean(x$titled),
      100*mean(x$complete), 100*mean(x$tier == 1)))
}

## ---- did the adjacency constraint actually bind? -------------------------
w <- reshape(pk[, c("wrdsfname","kind","start","end")], idvar="wrdsfname",
             timevar="kind", direction="wide")
w <- w[complete.cases(w), ]
sp <- pmax(w$end.cf, w$end.is, w$end.bs) - pmin(w$start.cf, w$start.is, w$start.bs)
cat(sprintf("\n=== triple spread (first pick to last), n = %d ===\n", length(sp)))
print(summary(sp))
b <- cut(sp, c(-1, 200, 500, 1000, 5000, 20000, Inf),
         labels = c("<=200","201-500","501-1000","1001-5000","5001-20000",">20000"))
print(data.frame(span = levels(b), n = as.integer(table(b)),
                 pct = sprintf("%.1f%%", 100*as.integer(table(b))/length(sp))),
      row.names = FALSE)

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
