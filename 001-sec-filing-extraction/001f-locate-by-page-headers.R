## ============================================================================
## 001f  LOCATE BY PAGE HEADER             step 001, substep f
## Approach C. Cuts the statements apart at their PAGE HEADERS instead of
## bracketing each one between its own content landmarks.
## In : cache/markers_hdr.rds (001e), cache/numlines.rds (001c)
## Out: cache/chunks_hdr.rds (same shape as 001d's output, so 001g exports it)
## ============================================================================
## THE IDEA IN ONE LINE: a statement runs from its own page header to the next
## page header, so nothing about its contents has to be findable.
##
## Approaches A (legacy/build-rank-candidates) and B (001d) both bracket a statement between a top and
## a bottom landmark drawn from its contents, and both keep failing on filings
## where those landmarks are not there to be found:
##
##   EPIQ / Genlyte   "NET" and "INCOME" on separate lines - no net income
##   United Airlines  no labelled total on either balance sheet page
##   Biomet           wholly owned, so no EPS
##
## Reading those filings line by line shows the top of every statement page
## carries the same block - company name, statement title, scale, period
## header, column years - and a footnote table never does. See the worked
## examples in 001e's header. So:
##
##   1. A HEADER is a statement title with a PERIOD HEADER or COLUMN YEARS
##      close below it. The title alone is not enough: a table of contents
##      lists every title, and footnotes mention them in prose.
##   2. The STATEMENTS SECTION is the longest run of headers that sit close
##      together, because the statements are printed consecutively.
##   3. EACH STATEMENT runs from its header to the next header in the run.
##      That is the whole boundary rule - no close marker, no subtotal, no EPS.
##   4. A page whose title says ", continued" belongs to the statement above
##      it rather than starting a new one.
##
## WHAT THIS GIVES UP. A and B could say "this span contains the three cash
## flow sections, so it is a cash flow statement". C only says "this is the
## block between two headers, and its title says cash flow". If a filer's
## title is wrong or missing, C has nothing to fall back on. The structure
## test is therefore still COMPUTED and reported here, but it no longer
## decides the boundary - it is a quality flag on a block already cut.
PILOT <- Sys.getenv("PILOT", "pilot-2026-11")
SP  <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT, "/cache/")
M   <- readRDS(paste0(SP, "markers_hdr.rds"))
NUM <- readRDS(paste0(SP, "numlines.rds"))
t <- M$text
cat("marker rows:", nrow(M), " filings:", length(unique(M$wrdsfname)), "\n")

## ---- flags ---------------------------------------------------------------
## Matched on JOINED line pairs by 001e, so a title split mid-phrase is whole
## here. No title_like()/PROSE filter is needed to keep footnote prose out,
## because a title only becomes a header when a period line follows it - but
## it is kept anyway, since it costs nothing and removes obvious prose.
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
  (ok & (p == 1L | grepl("[[:space:]]$", pre))) & !grepl(PROSE, pre, perl = TRUE)
}

F <- data.frame(
  ## The noun may come FIRST and the "of" be absent entirely - DuPont heads
  ## its page "Consolidated Income Statements (Note 1)". Matched here as well
  ## as in 001e, since 001f reclassifies what 001e grepped.
  ## "Cashflows" is one word in Norwood Financial's 10-Q, so the space is
  ## optional on both sides of the pattern.
  ttl_cf = title_like(t, "statements? of .{0,40}cash ?flows?|cash ?flows? statements?"),
  ttl_is = title_like(t, paste0("statements? of .{0,40}(income|operations|earnings)",
                                "|(income|earnings|operations) statements?")),
  ttl_bs = title_like(t, "balance sheets?|statements? of .{0,40}financial (position|condition)"),
  ## Comprehensive income is its OWN statement on its OWN page - International
  ## Bancshares prints it between the income statement and the cash flow
  ## statement, and 001d mistook it for the income statement because it was
  ## nearer the anchor. It is a separate kind here, never an income statement,
  ## EXCEPT where the title combines the two ("Statements of Operations and
  ## Comprehensive Income", SPX), which ttl_is already catches.
  ttl_ci = title_like(t, "statements? of .{0,40}comprehensive (income|loss)"),
  ttl_eq = title_like(t, "statements? of .{0,40}((stock|share)holders|changes in)"),
  ## THE PERIOD HEADER - the second half of the signature.
  period = grepl(paste0("(three|six|nine|twelve|thirteen|twenty|thirty|forty|fifty)",
                        "[ -]?(six|two|three|nine)? (months?|weeks?) ended",
                        "|(years?|quarters?|periods?|months?|weeks?) ended",
                        "|as of (january|february|march|april|may|june|july|august",
                        "|september|october|november|december)"), t),
  ## column dates: "March 31, 2009" or a bare year row "2004 2003 2002"
  coldate = grepl(paste0("(january|february|march|april|may|june|july|august",
                         "|september|october|november|december) [0-9]{1,2},"), t),
  colyear = grepl("^ ?(19|20)[0-9]{2}( (19|20)[0-9]{2})* ?$", t),
  scale   = grepl(paste0("in (thousands|millions|billions)|dollars in",
                         "|\\(in [a-z]+|(thousands|millions|billions)\\)"), t),
  coname  = grepl(paste0("^ ?[a-z .,&'/-]{3,70}(inc|corp|corporation|company",
                         "|holdings|subsidiaries|lp|llc|plc|trust|partners)[.,]? ?$"), t),
  item_hd = grepl("item (1|8)[^a-z]{0,4}financial statements", t),
  notes_hd = grepl("notes to [a-z ]{0,40}financial statements", t),
  toc     = grepl("table of contents", t),
  contd   = grepl(", ?continued", t))
M <- cbind(M[, c("wrdsfname", "nlines", "line", "text")], F)
M <- M[rowSums(F) > 0, ]

KIND <- c(ttl_cf = "cf", ttl_is = "is", ttl_bs = "bs", ttl_ci = "ci", ttl_eq = "eq")

## ---- parameters ----------------------------------------------------------
## How far below a title to look for the period header. Measured off the three
## filings read by hand: EPIQ title 486 -> period 492 (6), Intl Bancshares
## 472 -> 478 (6), SPX 3235 -> 3239 (4). Twenty-five is generous.
HDR_WIN <- 25
## Headers nearer than this are the same header, reported more than once by
## the joined-pair grep. A real statement page is never 8 lines long.
HDR_DEDUP <- 8
## Two headers belong to the same run of statements if they are no further
## apart than this. A statement page is 50-250 lines; 600 allows for an audit
## report or an equity statement sitting between two of them.
RUN_GAP <- 600
## SOFT HEADERS: how close the page furniture must sit to a title for it to
## count as a header despite having no period line under it, and how many of
## the three kinds it needs. Two of three keeps notes prose out; WWE's equity
## header has all three (contents marker, company name, scale).
SOFT_NEAR <- 3
SOFT_FURN <- 2
## A run must hold at least this many headers to be the statements section.
## Two is enough: a 10-Q can print the balance sheet and cash flow statement
## with the income statement combined into another.
RUN_MIN <- 2
## Lines of lead-in kept above a header, so the company name and scale come
## with the block.
PAD_UP <- 14
## A block is capped here. Nothing legitimate runs longer, and an uncapped
## block would swallow the notes when the next header is missing.
MAXLEN <- 900
MINLEN <- 8
## A statement has figures in it - the test that rejects an XBRL taxonomy
## definition block, which carries every word of a statement and no numbers.
NNUM_MIN <- 8
## A table of contents lists the titles together. Real statements are
## separated by their own bodies.
## Contents entries sit 6-8 lines apart (EPIQ lists four at 183/190/197/204);
## real statement pages are 30+ lines apart even in a compact 10-Q. A 45-line
## window rejected real headers and cost 14 points of coverage, so it is
## tightened to sit between the two spacings.
CROWD_N <- 3; CROWD_WIN <- 20
## ...and a contents entry is not followed by AMOUNTS. It is followed by a page
## number and the next entry, so a couple of numeric lines is not enough.
FIG_WIN <- 45; FIG_MIN <- 6
## THE FALLBACK WINDOW, for a filing the headers do not fully cover. Reaches
## further UP than down because the cash flow statement prints last of the
## three. Sized off the measured span from the first statement to the last:
## median 588 lines, p90 1,217, max 2,074. 3,000 lines is about 17k tokens.
WIN_UP <- 2200; WIN_DN <- 800
## The widest stretch allowed to count as one statements section when the
## window is placed off the titles. Measured span from the first statement to
## the last is a median 583 lines and p90 1,219, so 3,000 is generous.
WIN_MAX <- 3000

## ---- the structure test, now a QUALITY FLAG rather than the boundary -----
REQ <- list(
  cf = list(top = c("cf_bop", "cf_recon"), op = "cf_opX",
            inv = "cf_inv", fin = "cf_finX", close = "cf_end"),
  is = list(rev = "is_rev", ni = "is_ni"),
  bs = list(assets = c("bs_ta", "bs_hasset", "bs_tca"),
            liab = c("bs_tle", "bs_teq", "bs_hliab")))
## 001e's pattern set is header-focused and does not carry content markers, so
## the content flags are absent from this marker file. The test is therefore
## reported as NA unless the 001b markers are also present, which keeps the
## two scans independent: this script's boundaries never depend on 001b.
HAVE_CONTENT <- FALSE
if (file.exists(paste0(SP, "markers4_raw.rds"))) {
  MB <- readRDS(paste0(SP, "markers4_raw.rds"))
  tb <- MB$text
  CF <- data.frame(
    wrdsfname = MB$wrdsfname, line = MB$line,
    cf_bop = grepl("cash flows? (from|used)|^operating activities|operating activities:", tb) &
             grepl("operating", tb) & !grepl("[0-9]", tb) &
             !grepl("net cash|^net |provided by|used in [a-z]* ?activities", tb),
    cf_recon = grepl("adjustments? to reconcile", tb),
    cf_opX = grepl("operating activit", tb),
    cf_inv = grepl("investing activit", tb),
    cf_finX = grepl("financing activit", tb),
    cf_end = grepl("cash.{0,40}end of|end of (the )?(period|year|quarter)|net (increase|decrease|change).{0,20}cash", tb),
    is_rev = grepl(paste0("(net|total) (sales|revenues?)|^ ?(revenues?|sales) ?[,:]? ?($|[0-9$(])",
                          "|(revenues?|sales) ?, net|(total|net) interest (income|revenue)",
                          "|interest and (dividend|fee).{0,30}income|total revenues? and other income",
                          "|(total|net) premiums?|premiums? (earned|written)"), tb),
    is_ni = grepl("net (income|loss|earnings|profit)", tb),
    bs_ta = grepl("total assets", tb),
    bs_tca = grepl("total current (assets|liabilities)|total deposits", tb),
    bs_tle = grepl("total liabilities and|total liabilities, |total equity and liabilities", tb),
    bs_teq = grepl("total (equity|shareholders|stockholders|capitalization)", tb),
    bs_hasset = grepl("^ ?assets ?:? ?[0-9 ,.$()-]*$", tb),
    bs_hliab = grepl("^ ?liabilities( and .{0,40}equity)? ?:? ?[0-9 ,.$()-]*$", tb),
    stringsAsFactors = FALSE)
  CF <- CF[rowSums(CF[, -(1:2)]) > 0, ]
  CFS <- split(CF, CF$wrdsfname)
  HAVE_CONTENT <- TRUE
  cat("content markers available for the quality flag:", length(CFS), "filings\n")
}

## ---- 001d's cash flow pick, as the anchor -----------------------------
ANCH <- list()
if (file.exists(paste0(SP, "pick_anch.rds"))) {
  P9 <- readRDS(paste0(SP, "pick_anch.rds"))
  P9 <- P9[P9$kind == "cf", ]
  ANCH <- setNames(lapply(seq_len(nrow(P9)), function(i) c(P9$start[i], P9$end[i])),
                   P9$wrdsfname)
  cat("cash flow anchors from 001d:", length(ANCH), "\n")
}

## ---- per filing ----------------------------------------------------------
locate <- function(x) {
  x <- x[order(x$line), ]
  f <- x$wrdsfname[1]; nl <- x$nlines[1]; nv <- NUM[[f]]

  ## A FILING IS NEVER RETURNED EMPTY. Where the headers give nothing to cut
  ## on, the filing still has three statements in it, so it ships as a window.
  ##
  ## THE WINDOW IS CENTRED ON THE TITLES, NOT ON THE ANCHOR. Centring on the
  ## cash flow anchor rescued only 52 of 70 filings, and the 18 misses were not
  ## near misses - CIK 54507's window sat at lines 22,514-25,630 while its
  ## income statement and balance sheet titles are at 270 and 266. The anchor
  ## is exactly what is unreliable on the filings that reach this fallback, so
  ## using it to place the rescue reproduces the failure it is rescuing.
  ##
  ## Instead take the TIGHTEST stretch of the filing holding a title of all
  ## three kinds. That is what a statements section is, it needs no anchor, and
  ## it is measured off the filing in front of us (Matt, 2026-11-18).
  win_span <- function(TLa) {
    if (is.null(TLa) || !nrow(TLa)) return(NULL)
    TLa <- TLa[order(TLa$line), ]
    best <- NULL
    for (i in seq_len(nrow(TLa))) {
      j <- i
      while (j <= nrow(TLa) && TLa$line[j] - TLa$line[i] <= WIN_MAX) {
        if (all(c("cf", "is", "bs") %in% TLa$kind[i:j])) {
          w <- TLa$line[j] - TLa$line[i]
          if (is.null(best) || w < best[3]) best <- c(TLa$line[i], TLa$line[j], w)
          break
        }
        j <- j + 1
      }
    }
    if (is.null(best)) return(NULL)
    best[1:2]
  }
  winonly <- function(reason, TLa = NULL) {
    sp <- win_span(TLa)
    if (!is.null(sp)) {
      ## the last title's own statement runs on past it
      S <- max(1, sp[1] - PAD_UP); E <- min(nl, sp[2] + MAXLEN)
      how2 <- "title span"
    } else {
      A0 <- ANCH[[f]]
      if (is.null(A0)) return(NULL)
      S <- max(1, A0[1] - WIN_UP); E <- min(nl, A0[2] + WIN_DN)
      how2 <- "anchor window"
    }
    nn <- if (is.null(nv)) NA_integer_ else sum(nv >= S & nv <= E)
    data.frame(kind = "win", start = S, end = E, len = E - S + 1,
      pages = NA_integer_, how = how2, how_end = reason,
      titled = FALSE, nnum = nn,
      has_figures = is.na(nn) || nn >= NNUM_MIN,
      complete = NA, miss = "cf,is,bs", wrdsfname = f, nlines = nl,
      d_anchor = 0, rank = 1L, stringsAsFactors = FALSE)
  }

  ## 1. every title, with the kind it names
  tl <- do.call(rbind, lapply(names(KIND), function(k) {
    v <- x$line[x[[k]]]
    if (!length(v)) return(NULL)
    data.frame(line = v, kind = KIND[[k]], stringsAsFactors = FALSE)
  }))
  if (is.null(tl)) return(winonly("no title"))
  ## one title may match two kinds ("Operations and Comprehensive Income");
  ## keep the income-statement reading, which is what such a page is.
  tl <- tl[order(tl$line, match(tl$kind, c("is", "cf", "bs", "ci", "eq"))), ]
  tl <- tl[!duplicated(tl$line), ]

  ## 2. a title becomes a HEADER only if a period header or column dates sit
  ##    just below it, and it is not one of a stack of titles (a contents list)
  per <- sort(unique(c(x$line[x$period], x$line[x$coldate], x$line[x$colyear])))
  tl$hdr <- vapply(tl$line, function(L)
    any(per >= L - 2 & per <= L + HDR_WIN), logical(1))
  ## THE CONTENTS-LIST TEST MUST LOOK BOTH WAYS. Looking only forward lets the
  ## LAST title in a contents list through, because nothing follows it - and in
  ## a 10-Q the last entry is the cash flow statement. EPIQ lists its four
  ## statements at lines 183/190/197/204 with "Three months ended March 31,
  ## 2009 and 2008" under each, so every entry has a period header and reads as
  ## a page header; the cash flow entry at 204 was then chosen as the cash flow
  ## statement, and the real one at 857 was never reached.
  ## ...BUT IT MUST COUNT DISTINCT TITLES, NOT GREP ROWS. 001e greps JOINED
  ## line pairs, so one title is reported twice (sometimes three times). The
  ## collapse below used to run only on the survivors, after this test, so a
  ## title and its own duplicate already counted 2 of the 3 needed and ANY
  ## second title within the window rejected both. D.R. HORTON'S Q3 2006 10-Q
  ## is the case: its balance sheet header at line 191 is textbook - company
  ## name, title, "June 30, September 30,", "2006 2005", "(in millions)" - but
  ## the exhibit index line "Statement of Computation of Ratio of Earnings to
  ## Fixed Charges" sits at 171, exactly CROWD_WIN above it, and matched
  ## ttl_is. That made four rows, the balance sheet was rejected, and the
  ## filing shipped with no balance sheet at all (Matt, 2026-11-18).
  ##
  ## COLLAPSING BY LINE PROXIMITY ALONE WOULD BREAK THE TEST IT IS PART OF. A
  ## contents list prints its entries one per line, so its titles are as close
  ## together as a grep duplicate is - Horton lists all three statements at
  ## 139/141/143. The difference is the KIND: a grep duplicate repeats the
  ## same kind, a contents list steps through different ones. So adjacent rows
  ## are merged only when they agree on kind, which leaves Horton's contents
  ## list as three distinct titles (still rejected, correctly) and its balance
  ## sheet with one neighbour instead of three.
  tl <- tl[order(tl$line), ]
  dl <- if (nrow(tl) == 1) tl$line else tl$line[c(TRUE,
    !(diff(tl$line) <= HDR_DEDUP & tl$kind[-1] == tl$kind[-nrow(tl)]))]
  tl$crowd <- vapply(tl$line, function(L)
    sum(dl >= L - CROWD_WIN & dl <= L + CROWD_WIN), integer(1)) >= CROWD_N
  ## ...and AMOUNTS have to follow. A contents entry is followed by a page
  ## number and the next entry, which is one or two numeric lines; a statement
  ## page is followed by its figures.
  tl$figs <- if (is.null(nv)) TRUE else vapply(tl$line, function(L)
    sum(nv > L & nv <= L + FIG_WIN) >= FIG_MIN, logical(1))
  H <- tl[tl$hdr & !tl$crowd & tl$figs, ]
  ## COLLAPSE NEAR-DUPLICATE HEADERS. 001e greps JOINED line pairs, so a title
  ## on line N is reported on row N-1 (pair N-1,N) and again on row N (pair
  ## N,N+1), and a title split across two lines is reported three times. Each
  ## copy became its own header, so every statement was cut two or three times
  ## and the blocks came out a median 63 lines long - a third of a statement.
  ## Headers closer together than this are one header.
  if (nrow(H) > 1) {
    H <- H[order(H$line), ]
    H <- H[c(TRUE, diff(H$line) > HDR_DEDUP), ]
  }
  if (nrow(H) < RUN_MIN) return(winonly("no header", tl))

  ## 3. group headers into runs of consecutive statement pages, and take the
  ##    run with the most headers. Ties go to the run holding a cash flow
  ##    statement, which is the statement the project is about.
  ## USE 001d's CASH FLOW PICK AS THE ANCHOR WHERE ONE EXISTS. That pick is
  ## the one piece of this pipeline with hand validation behind it - 10 of 10
  ## on Matt's audit, 98.2% coverage - so the run to keep is the run it sits
  ## in, and the block to keep for each statement is the one nearest it.
  ##
  ## Ranking by position instead cost two of the five known cases. Biomet's
  ## 10-K prints tables with page headers of their own BEFORE Item 8, so the
  ## earliest income-statement header was at 3081 and the real statement is at
  ## 3912; United's filing holds two registrants, so the earliest set is the
  ## holding company's and the row we are processing is the subsidiary's.
  A <- ANCH[[f]]

  ## ---- SOFT HEADERS -------------------------------------------------------
  ## A title that fails ONLY the period test is still a page header, and it
  ## still has two jobs to do even though we will not ship it as a block:
  ## it ENDS the block above it, and it keeps the statements section together
  ## across the space it occupies.
  ##
  ## WWE's 10-K is the case. Its equity statement header reads
  ##
  ##   6059  table of contents
  ##   6061  world wrestling entertainment, inc.
  ##   6063  consolidated statements of stockholders equity
  ##   6065  (in thousands, except per share data)
  ##
  ## - four of the six header elements, and no period line anywhere near it,
  ## because an equity statement's columns are ACCOUNTS (Shares, Amount,
  ## Capital), not periods. Discarding it entirely left a 616-line gap, which
  ## exceeded RUN_GAP by sixteen, which stranded the cash flow header in a run
  ## of one and lost the statement. 128 of 488 holdout filings (26.2%) have an
  ## equity title rejected this way (Matt, 2026-11-19).
  ##
  ## WHY NOT JUST DROP RUN_GAP. Tried first: runs merged, nothing new cut the
  ## blocks apart, and ten filings fell off tight blocks onto 6,000-token
  ## windows for no gain in coverage. Soft headers cut as well as join, which
  ## is the half that attempt was missing.
  ##
  ## THE FURNITURE TEST is what keeps prose out. A soft header must carry the
  ## surrounding page furniture - a company name just above, a scale just
  ## below, or a page/contents marker above. Kibush's notes contain "balance
  ## sheet dates. items in the income statement and cash flow statement",
  ## which has none of it and stays excluded.
  cname <- x$line[x$coname]; scal <- x$line[x$scale]
  pmark <- sort(unique(c(x$line[x$toc], x$line[x$item_hd])))
  ## NO FIGURES REQUIREMENT HERE, and the reason is the same statement again.
  ## An equity statement is a WIDE table, so its header band runs forty lines
  ## of one word each - Class A, Class B, Paid-in, Comprehensive, Shares,
  ## Amount, Capital, Deficit, Total - before the first number. WWE's fails
  ## the figures test for exactly the reason it fails the period test, and
  ## requiring figures here meant the soft set never contained it at all.
  ## The furniture test below is what keeps prose out; it does not need help.
  soft <- tl[!tl$hdr & !tl$crowd, ]
  if (nrow(soft)) {
    furn <- vapply(soft$line, function(L)
      any(cname >= L - SOFT_NEAR & cname < L) +
      any(scal  >  L & scal  <= L + SOFT_NEAR) +
      any(pmark >= L - SOFT_NEAR * 2 & pmark < L) >= SOFT_FURN, logical(1))
    soft <- soft[furn, ]
  }
  ## Never let a soft header sit on top of a real one.
  if (nrow(soft)) soft <- soft[vapply(soft$line, function(L)
    !any(abs(H$line - L) <= HDR_DEDUP), logical(1)), ]

  H$run <- cumsum(c(1, vapply(seq_len(nrow(H))[-1], function(i) {
    g <- H$line[i] - H$line[i - 1]
    if (g <= RUN_GAP) return(FALSE)
    ## a soft header inside the gap means the section did not end there
    !any(soft$line > H$line[i - 1] & soft$line < H$line[i])
  }, logical(1))))
  rs <- split(H, H$run)
  ## Only runs big enough to BE a statements section are eligible. Choosing
  ## the nearest run without this test cost 88 filings, because a lone header
  ## can sit closer to the anchor than the real section does.
  elig <- names(rs)[vapply(rs, nrow, integer(1)) >= RUN_MIN]
  if (!length(elig)) elig <- names(rs)
  ## NEARNESS TO THE ANCHOR IS THE LAST TEST, NOT THE FIRST. Picking the run
  ## nearest the anchor asks the wrong question when the anchor is wrong,
  ## because SOME run is always nearest and nothing checks that the anchor is
  ## even in it. CIK 72207's 2007 10-Q is the case: 001d's cash flow pick
  ## lands at 2861-2919, the real statements are the run at 166-781, and a
  ## junk run at 1502-1749 is a thousand lines closer to the bad anchor. The
  ## real run won only because the junk run held one header and was below
  ## RUN_MIN; the moment the crowd fix gave it a second, the junk run took
  ## the filing and the cash flow statement was lost (Matt, 2026-11-18).
  ##
  ## So the tests run in order of how much they actually tell us:
  ##   1. does the run HOLD the anchor - decisive when it does, and it does
  ##      for most filings, since 001d's pick is right ~98% of the time;
  ##   2. how many DIFFERENT statements are in the run - the statements print
  ##      together, so the run with the income statement, balance sheet and
  ##      cash flow statement in it is the statements section; a run of three
  ##      income-statement headers is an exhibit index or a notes table;
  ##   3. does it hold a cash flow statement at all - the statement the
  ##      project is about;
  ##   4. and only then nearness.
  nkind <- vapply(rs[elig], function(z) length(unique(z$kind)), integer(1))
  hascf <- vapply(rs[elig], function(z) any(z$kind == "cf"), logical(1))
  if (!is.null(A)) {
    amid  <- (A[1] + A[2]) / 2
    ## The last header's own statement runs on past it, so a run reaches
    ## MAXLEN beyond its final header before the anchor counts as outside it.
    holds <- vapply(rs[elig], function(z)
      A[2] >= min(z$line) - PAD_UP & A[1] <= max(z$line) + MAXLEN, logical(1))
    d <- vapply(rs[elig], function(z) min(abs(z$line - amid)), numeric(1))
    H <- rs[[elig[order(!holds, -nkind, !hascf, d)[1]]]]
  } else {
    H <- rs[[elig[order(-nkind, !hascf, -vapply(rs[elig], nrow, integer(1)))[1]]]]
  }
  if (nrow(H) < RUN_MIN) return(winonly("no header", tl))

  ## 4. a ", continued" page joins the statement above it
  cd <- x$line[x$contd]
  H$is_contd <- vapply(H$line, function(L) any(cd >= L - 2 & cd <= L + 3), logical(1))
  H$is_contd[1] <- FALSE
  grp <- cumsum(!H$is_contd)
  blocks <- do.call(rbind, lapply(split(H, grp), function(z) {
    data.frame(kind = z$kind[1], hdr = z$line[1], pages = nrow(z),
               stringsAsFactors = FALSE)
  }))
  blocks <- blocks[order(blocks$hdr), ]

  ## 5. each block runs from its header to the next block's header.
  ##    The last block in the run stops at the notes section if one follows,
  ##    else at the run's end plus one page.
  ## A SOFT HEADER ALSO ENDS THE BLOCK ABOVE IT. Without this the balance
  ## sheet block would run through the equity statement to the cash flow
  ## header - covered, but 600 lines instead of 235, which is the merging that
  ## sank the first attempt at this.
  sl <- if (exists("soft") && nrow(soft)) soft$line[
    soft$line > min(blocks$hdr) & soft$line <= max(blocks$hdr) + MAXLEN] else numeric(0)
  nh <- vapply(seq_len(nrow(blocks)), function(i) {
    cands <- c(if (i < nrow(blocks)) blocks$hdr[i + 1] else NA_real_,
               sl[sl > blocks$hdr[i] + MINLEN])
    cands <- cands[!is.na(cands)]
    if (!length(cands)) NA_real_ else min(cands)
  }, numeric(1))
  nt <- x$line[x$notes_hd]; nt <- nt[nt > max(blocks$hdr)]
  tail_end <- if (length(nt)) min(nt) else min(max(blocks$hdr) + 300, nl)
  out <- data.frame()
  for (i in seq_len(nrow(blocks))) {
    S <- max(1, blocks$hdr[i] - PAD_UP)
    E <- if (!is.na(nh[i])) nh[i] - 1 else tail_end
    E <- min(E, S + MAXLEN, nl)
    if (E - S + 1 < MINLEN) next
    nn <- if (is.null(nv)) NA_integer_ else sum(nv >= S & nv <= E)
    out <- rbind(out, data.frame(
      kind = blocks$kind[i], start = S, end = E, len = E - S + 1,
      pages = blocks$pages[i], how = "header",
      how_end = if (!is.na(nh[i])) "next header" else
                if (length(nt)) "notes section" else "run end",
      titled = TRUE, nnum = nn,
      has_figures = is.na(nn) || nn >= NNUM_MIN,
      stringsAsFactors = FALSE))
  }
  if (!nrow(out)) return(winonly("no block", tl))

  ## 6. the structure test, as a FLAG on a block already cut
  out$complete <- NA; out$miss <- NA_character_
  if (HAVE_CONTENT && !is.null(CFS[[f]])) {
    cm <- CFS[[f]]
    for (i in seq_len(nrow(out))) {
      ty <- out$kind[i]
      if (!ty %in% names(REQ)) next
      inw <- cm$line >= out$start[i] & cm$line <= out$end[i]
      got <- vapply(REQ[[ty]], function(g)
        any(vapply(g, function(fl2) any(cm[[fl2]][inw]), logical(1))), logical(1))
      out$complete[i] <- all(got)
      out$miss[i] <- paste(names(got)[!got], collapse = ",")
    }
  }
  out$complete <- out$complete & out$has_figures
  out$wrdsfname <- f; out$nlines <- nl

  ## A COMPREHENSIVE-INCOME STATEMENT IS THE INCOME STATEMENT WHEN THERE IS NO
  ## OTHER ONE. Comprehensive income is kept as its own kind because
  ## International Bancshares prints it as a separate page between the income
  ## statement and the cash flow statement, and treating it as the income
  ## statement made 001d pick the wrong one. But PAYONEER'S Q2 2021 10-Q
  ## prints only "Condensed Consolidated Statements of Comprehensive Loss" -
  ## that single statement IS its income statement, and the filing shipped
  ## without one. So the ci block doubles as the is block exactly when the
  ## filing has no is block of its own, which leaves the Bancshares case
  ## untouched (Matt, 2026-11-18).
  if (!("is" %in% out$kind) && "ci" %in% out$kind) {
    ci <- out[out$kind == "ci", ][1, ]
    ci$kind <- "is"; ci$how <- paste0(ci$how, " (comprehensive income)")
    out <- rbind(out, ci)
  }
  ## RANK BY DISTANCE TO THE CASH FLOW ANCHOR, not by position in the filing.
  ## The three statements print together, so the anchor's neighbours are its
  ## own set - which is what resolves both a 10-K with earlier tables of its
  ## own (Biomet) and a filing holding two registrants (United).
  out <- do.call(rbind, lapply(split(out, out$kind), function(w) {
    if (!is.null(A)) {
      w$d_anchor <- abs((w$start + w$end) / 2 - (A[1] + A[2]) / 2)
      w <- w[order(w$d_anchor), ]
    } else {
      w$d_anchor <- NA_real_
      w <- w[order(w$start), ]
    }
    w$rank <- seq_len(nrow(w)); w }))

  ## 7. NEVER SHIP A FILING SHORT OF ALL THREE STATEMENTS. Every 10-K and 10-Q
  ## contains an income statement, a balance sheet and a cash flow statement,
  ## so a filing missing one HERE is a locator failure, never a filing without
  ## one - and dropping it loses the observation for no reason except cost.
  ## Priced over the population: sending a window for the filings the headers
  ## do not cover adds about 9% to the job, which is not a reason to lose
  ## 13.6% of the sample (Matt, 2026-11-18).
  ##
  ## This also moves the hard part to where it is done better. Deciding which
  ## block is the real income statement is what the regex keeps getting wrong;
  ## given a wide window the model reads the statements directly.
  miss3 <- setdiff(c("cf", "is", "bs"), out$kind)
  if (length(miss3)) {
    wk <- winonly(paste("missing", paste(miss3, collapse = ",")), tl)
    if (!is.null(wk)) { wk$miss <- paste(miss3, collapse = ","); out <- rbind(out, wk) }
  }
  rownames(out) <- NULL
  out
}

C <- do.call(rbind, lapply(split(M, M$wrdsfname), locate)); rownames(C) <- NULL
allf <- length(unique(M$wrdsfname))

## ZERO BLOCKS IS A LEGITIMATE ANSWER, NOT AN ERROR. When no filing in the
## batch yields a block, rbind of all-NULL returns NULL, and the diagnostics
## below died on `!nrow(NULL)` ("invalid argument type"). That never showed up
## while batches were random -- some filing always matched -- and appeared the
## moment a batch was drawn from the filings earlier waves had failed on: 297
## amendments, not one printed statement between them, and the step crashed
## instead of reporting an empty result. A crash there is the worse outcome,
## because the wave then logs NOTHING and re-claims the same filings forever,
## which is the wedge 001m's notext stage exists to prevent. So: write the
## empty frame, say so, and exit 0.
EMPTY_C <- data.frame(
  kind = character(0), start = integer(0), end = integer(0), len = integer(0),
  pages = integer(0), how = character(0), how_end = character(0),
  titled = logical(0), nnum = integer(0), has_figures = logical(0),
  complete = logical(0), miss = character(0), wrdsfname = character(0),
  nlines = integer(0), d_anchor = integer(0), rank = integer(0),
  stringsAsFactors = FALSE)
NO_BLOCKS <- is.null(C) || !nrow(C)
if (NO_BLOCKS) C <- EMPTY_C

cat("\n=== APPROACH C: cut at page headers ===\n")
cat("filings with any block:", length(unique(C$wrdsfname)), "of", allf,
    if (NO_BLOCKS) "- no statement located in any of them\n" else "\n")

if (!NO_BLOCKS) {
cat("\n=== blocks found, by statement ===\n")
for (ty in c("cf", "is", "bs", "ci", "eq")) {
  z <- C[C$kind == ty, ]
  if (!nrow(z)) { cat(sprintf("%-3s  none\n", ty)); next }
  n <- length(unique(z$wrdsfname))
  cat(sprintf("%-3s  %3d filings (%5.1f%%)  blocks %4d  med len %3.0f  p90 %4.0f  multi-page %4.1f%%  passes structure test %s\n",
      ty, n, 100 * n / allf, nrow(z), median(z$len), quantile(z$len, .9),
      100 * mean(z$pages > 1),
      if (all(is.na(z$complete))) "n/a" else sprintf("%5.1f%%", 100 * mean(z$complete, na.rm = TRUE))))
}

cat("\n=== how each block was closed ===\n")
print(table(C$kind, C$how_end))

cat("\n=== the three statements together ===\n")
w <- reshape(C[C$rank == 1 & C$kind %in% c("cf", "is", "bs"),
               c("wrdsfname", "kind", "start", "end")],
             idvar = "wrdsfname", timevar = "kind", direction = "wide")
cat("filings with all three:", sum(complete.cases(w)), sprintf("(%.1f%%)\n",
    100 * sum(complete.cases(w)) / allf))
wc <- w[complete.cases(w), ]
sp <- pmax(wc$end.cf, wc$end.is, wc$end.bs) - pmin(wc$start.cf, wc$start.is, wc$start.bs)
cat(sprintf("span from first to last: median %4.0f  p90 %5.0f  max %6.0f\n",
    median(sp), quantile(sp, .9), max(sp)))
}   ## end if (!NO_BLOCKS) -- the diagnostics above all index into C

saveRDS(C, paste0(SP, "chunks_hdr.rds"))
cat("\nsaved", paste0(SP, "chunks_hdr.rds"),
    sprintf("(%d blocks)\n", nrow(C)))
