## 001h  BUILD TIDY FRAME                step 001, substep h
## Turn the API's per-chunk JSON into the analysis frame: one row per
## filing x statement x line x period, with the amount in ACTUAL DOLLARS.
##
## Input :  extract/extracted.jsonl   (what legacy/extract-statements writes: one JSON object per
##                                     chunk - the extraction plus the chunk's
##                                     own metadata)
## Output:  extract/scf_text_lines.parquet   the frame
##          extract/scf_text_foot.parquet    the footing check, one row per
##                                           filing x statement x period
##          extract/scf_text_lines.csv       a readable copy
##
## WHY THE RESCALING HAPPENS HERE AND NOT IN THE MODEL. The model returns the
## number as printed plus the scale word. Multiplying in R means a misread
## multiplier is a one-line fix instead of a re-run of the whole extraction, and
## the arithmetic is auditable. `value_printed` is always kept beside `amount`.
##
## WHY THE FOOTING CHECK IS HERE. A printed statement foots - the filer typeset
## it and the auditor signed it. So a transcription that foots is almost
## certainly correct, which makes this the free validation that lets a cheap
## model do the extraction. It is also the escalation trigger: send the failures
## to a stronger model, nothing else.

suppressMessages({library(jsonlite); library(dplyr); library(tidyr)
                  library(arrow); library(stringr)})

## Point TIDY_IN / TIDY_OUT at a scratch copy to dry-run the frame on a handful
## of extractions without writing into the real output folder.
SP  <- Sys.getenv("TIDY_OUT",
         paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/extract/"))
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")))
IN  <- Sys.getenv("TIDY_IN", paste0(SP, "extracted.jsonl"))
if (!file.exists(IN)) stop("no ", IN, " - run legacy/extract-statements.py fetch first")
cat("reading", IN, "\n")

## ---- scale words to multipliers -------------------------------------------
## `ones` and a missing scale are NOT the same thing. A statement that states no
## multiplier is usually in whole dollars, but "usually" is not good enough to
## multiply by, so a missing scale yields amount = NA and a flag. Decide what to
## do with those as a group rather than silently assuming.
MULT <- c(ones = 1, thousands = 1e3, millions = 1e6, billions = 1e9)
## A STATEMENT THAT NAMES NO MULTIPLIER IS IN WHOLE DOLLARS.
## The comment above says a missing scale yields NA and a flag, pending a
## decision. Decided 2026-11-19 (Matt), after confirming it against CH Real
## Estate II's 10-Q, which states no multiplier and prints cash of $32,677.
## Four of the ten filings in the first in-harness run name no multiplier, all
## of them small filers, and leaving amount NA drops exactly those from any
## analysis. The model still reports what the page says, so `scale` stays NA
## and `scale_assumed_ones` records where this rule was applied - the decision
## is in the code, visible and reversible, not buried in the extraction.
MULT_DEFAULT <- 1

STMT <- c(cash_flow = "SCF", income_statement = "IS", balance_sheet = "BS")

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- read ------------------------------------------------------------------
raw <- lapply(readLines(IN, warn = FALSE), function(l) {
  if (!nzchar(trimws(l))) return(NULL)
  tryCatch(fromJSON(l, simplifyVector = FALSE), error = function(e) NULL)
})
raw <- raw[!vapply(raw, is.null, logical(1))]
cat("chunks read:", length(raw), "\n")

## legacy/extract-statements may nest the model's answer under `result`; accept either shape.
pick <- function(r, k) {
  if (!is.null(r[[k]])) return(r[[k]])
  if (!is.null(r$result) && !is.null(r$result[[k]])) return(r$result[[k]])
  NULL
}
meta <- function(r, k) { v <- r[[k]]; if (is.null(v)) NA else v[[1]] }

## ---- expand one chunk to long ---------------------------------------------
one <- function(r) {
  if (!isTRUE(pick(r, "found"))) return(NULL)
  rows <- pick(r, "rows"); cols <- pick(r, "columns")
  if (!length(rows) || !length(cols)) return(NULL)
  nc <- length(cols)

  sc  <- pick(r, "scale")
  scw <- if (is.null(sc)) NA_character_ else as.character(sc)
  m   <- if (is.na(scw)) MULT_DEFAULT else unname(MULT[scw])
  assumed_ones <- is.na(scw)

  ch <- vapply(cols, function(k) as.character(k$h %||% ""), character(1))
  ce <- vapply(cols, function(k) if (is.null(k$end)) NA_character_
                                 else as.character(k$end), character(1))
  cm <- vapply(cols, function(k) if (is.null(k$months)) NA_integer_
                                 else as.integer(k$months), integer(1))

  out <- vector("list", length(rows))
  for (i in seq_along(rows)) {
    rr <- rows[[i]]
    v  <- rr$v
    ## A row whose v is the wrong length is a transcription fault, not something
    ## to paper over: pad/truncate so the frame stays rectangular, and flag it.
    bad <- length(v) != nc
    if (length(v) < nc) v <- c(v, vector("list", nc - length(v)))
    if (length(v) > nc) v <- v[seq_len(nc)]
    vals <- vapply(v, function(z) if (is.null(z)) NA_real_ else as.numeric(z),
                   numeric(1))
    ## per-share / share-count / ratio rows are printed unscaled already
    noscale <- isTRUE(rr$x)
    ## `u` - the row printed fewer numbers than there are columns, so the model
    ## filled v from the left and the placement is unverified. Resolved below
    ## by footing, never by the model. See resolve_unplaced().
    unres <- isTRUE(rr$u)
    ## `va` - present only where the row printed MORE numbers than there are
    ## columns. The surplus has nowhere to go in a rectangular frame, so it is
    ## carried as text: a reviewer can see what the page actually held and the
    ## redo list can rebuild the row. Without it those figures are simply
    ## absent, and the only symptom is a section that does not add up with
    ## nothing to say why - which is how 70,000 and (2,953) were lost from an
    ## Ace Hardware 10-K/A before the spec had a rule for this.
    va   <- rr$va
    over <- length(va) > nc
    out[[i]] <- data.frame(
      line_order    = as.integer(rr$n %||% i),
      line_label    = as.character(rr$l %||% ""),
      col_order     = seq_len(nc),
      col_heading   = ch,
      period_end    = ce,
      period_months = cm,
      value_printed = vals,
      unscaled_row  = noscale,
      len_mismatch  = bad,
      col_unplaced  = unres,
      row_overcount = over,
      ## a person reads this column, so a blank cell shows as "-" rather than
      ## as nothing between two pipes
      row_printed   = if (over) paste(vapply(va, function(z)
        if (is.null(z)) "-" else format(as.numeric(z), scientific = FALSE),
        character(1)), collapse = " | ") else NA_character_,
      stringsAsFactors = FALSE)
  }
  d <- bind_rows(out)
  d$scale  <- scw
  d$mult   <- m
  d$scale_assumed_ones <- assumed_ones
  d$amount <- ifelse(d$unscaled_row, d$value_printed, d$value_printed * m)

  st <- as.character(pick(r, "statement") %||% NA)
  d$statement   <- unname(STMT[st])
  d$stmt_raw    <- st
  d$wrdsfname   <- meta(r, "wrdsfname")
  d$cik         <- meta(r, "cik")
  d$coname      <- meta(r, "coname")
  d$form        <- meta(r, "form")
  d$fdate       <- meta(r, "fdate")
  d$rdate       <- meta(r, "rdate")
  d
}

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- PLACE THE UNPLACED VALUES BY FOOTING ---------------------------------
## In the cell-per-line layout a blank cell prints nothing, so a row whose
## figure appears in only one of two columns arrives as a label and a single
## number with nothing on the page saying which column it is. Dover Downs'
## 10-Q financing section is the case:
##
##     Repurchase of common stock            (35,042
##     Proceeds from stock options exercised     349
##     Other                                     (21
##
## Three rows, two columns, one number each. The first two are 2006 and the
## third is 2005, and the only evidence for that is the section's own
## subtotal: 147,325 - 115,300 - 2,591 - 35,042 + 349 + 50 = -5,209, which is
## what the statement prints, and the 2005 column only reaches -7,190 if the
## (21) goes there.
##
## The model is forbidden to do that sum - it marks the row `u` and fills from
## the left. The arithmetic happens here, where it can be read and checked.
## For every assignment of the printed values to columns that keeps their
## left-to-right order, the section is footed against its own subtotal; if
## exactly one assignment foots, it is taken. If none or several do, the
## values stay where the model put them and the rows stay flagged, because a
## guess that cannot be checked is worse than a flag that can.
## THE SUBTOTALS EACH STATEMENT FOOTS TO.
##
## Cash flow and balance sheet both work the same way - the lines above a
## subtotal SUM to it, with the printed sign being the arithmetic sign. So the
## same resolver serves both; only the labels differ.
##
## THE INCOME STATEMENT NEEDS A SIGN FOR EVERY LINE AS WELL. Its expenses are
## printed POSITIVE and subtracted - Biomet prints net sales 2,698.0, cost of
## sales 819.9 and gross profit 1,878.1 - so the components do not sum to the
## subtotal, they NET to it, and the page does not say which lines are added.
## But each line is either added or subtracted, so the sign is one bit per
## line and can be solved for alongside the placement (Matt, 2026-11-17).
##
## The protection is the same in all three cases and it is the uniqueness
## requirement, not the statement type: a placement is taken only when exactly
## one combination foots every column. Searching signs as well means more
## combinations and so more ties, and a tie leaves the row flagged. That is the
## intended behaviour - the resolver declines more often on income statements
## rather than guessing more often.
## A SUBTOTAL PATTERN MUST BE ANCHORED, AND MUST ALLOW PUNCTUATION.
## Both of these were found the first time the resolver ran on real filings
## rather than on hand-built tests, which used tidy labels (Matt, 2026-11-19):
##
##  1. unanchored, "net cash ... provided ... operating" also matches
##     "Adjustments to reconcile net earnings to net cash provided by
##     operating activities:" - a COMPONENT row, and boilerplate in nearly
##     every indirect-method statement. The section walk then treated that row
##     as the subtotal, so the operating section was three rows long with a
##     null subtotal and the resolver bailed out silently.
##
##  2. "provided|used" had to follow "net cash " immediately, so Bristol-Myers'
##     "Net Cash (Used in)/Provided by Investing Activities" matched nothing
##     and its investing and financing sections were never resolvable.
##
## Anchoring to ^net cash fixes the first; allowing parentheses, commas and
## slashes in between fixes the second. On Bristol-Myers this takes the
## resolver from 0 of 18 cells placed to 18 of 18, all correct.
SUBTOT <- list(
  SCF = c(op  = "^net cash (flows? )?[a-z (),/]*(provided|used|from)[a-z (),/]*operat",
          inv = "^net cash (flows? )?[a-z (),/]*(provided|used|from)[a-z (),/]*invest",
          fin = "^net cash (flows? )?[a-z (),/]*(provided|used|from)[a-z (),/]*financ"),
  ## Each balance-sheet subtotal closes the run of lines above it, exactly as a
  ## cash flow section subtotal does. Ordered as printed so the section walk
  ## assigns each line to the first subtotal at or after it.
  BS  = c(tca = "^total current assets",
          ta  = "^total assets",
          tcl = "^total current liabilities",
          tl  = "^total liabilities$|^total liabilities[^a]",
          teq = "^total (share|stock)holders.? (equity|deficit)|^total equity",
          tle = "^total liabilities and"),
  IS  = c(trev = "^(total|net) (revenues?|sales)",
          gp   = "^gross (profit|margin)",
          toe  = "^total (operating |costs? and )?(costs?|expenses?)",
          oi   = "^(total )?operating (income|loss|profit)",
          pbt  = "^(income|loss|earnings).{0,30}before.{0,20}tax",
          ni   = "^net (income|loss|earnings)"))
## Which statements need a sign solved per line as well as a placement.
SIGNED <- c(SCF = FALSE, BS = FALSE, IS = TRUE)
FOOT_TOL <- 1       # printed units, so $1k in a thousands statement
## Refuse to search beyond this many combinations. Signs double the space per
## component line, so an income-statement block of more than about ten lines
## is declined rather than searched - and a block that big is also the one
## where a tie is most likely anyway.
MAX_COMB <- 20000

## ---- the cash roll-forward, used by the resolver AND the footing check -----
## Defined once here so the two cannot disagree about which line is which.
##   beginning cash + net change + any other cash movement = ending cash
ROLL <- c(net = "(net |)(increase|decrease|change).{0,40}in cash",
          ## SPLIT CASH-BALANCE LABELS. Some filers print a "CASH AND CASH
          ## EQUIVALENTS" header and then bare "Beginning of period" / "End of
          ## period" beneath it, so the balance labels contain no cash word at
          ## all - K-Tron International 10-Q 2002-03-30, where requiring "cash"
          ## left beg/end NA and the check then reported a roll-forward failure
          ## on a statement that footed to the penny.
          beg = "cash.{0,50}beginning|^\\(?beginning (of|balance)",
          end = "cash.{0,50}end of|^\\(?end (of|balance)")

## LINES THAT LOOK LIKE THE NET-CHANGE LINE AND ARE NOT IT. Walmart FY2021-FY2024
## prints "Change in cash and cash equivalents reclassified from (to) assets
## held for sale" BELOW the real net-change line, so taking the last match read
## net as 1,848 in every period - the same number each time, which is the tell.
## All 11 Walmart footing failures and all 10 of its untestable columns were
## this one line; the transcriptions themselves were exact.
##
## They are excluded from the SECTION sum and kept in the ROLL-FORWARD, because
## such a line still moves the cash balance:
##   17,788 - 4,802 + 1,848 = 14,834  (Walmart FY2022, exactly).
## Restricted cash is deliberately NOT excluded: after ASU 2016-18 "cash, cash
## equivalents and restricted cash" IS the correct caption.
ROLL_OTH <- "held for sale|reclassif|classified as|discontinued"

## ...and they are matched on their OWN shape, not as an exception to the
## net-change pattern, because the caption is not always a "change in cash"
## sentence. Walmart's FY2021 10-K prints "Cash and cash equivalents
## reclassified as assets held for sale (1,848)", which contains no
## increase/decrease/change wording at all - so keying off the net-change
## pattern missed it and left that one column failing. Requiring a cash word
## AND a qualifier keeps the beginning/ending balance lines out, since neither
## carries a qualifier.
##
## "discontinued" is deliberately NOT a qualifier here, although it IS one in
## ROLL_OTH above. Walmart 2004-2008 prints "Net cash provided by operating
## activities of discontinued operations" INSIDE the operating section, so it
## is already inside that subtotal; adding it to the roll-forward double-counts
## it and broke 15 previously-footing columns. Only a movement printed outside
## the three sections belongs here, and the SUBTOT guard below is the second
## line of defence against exactly that mistake.
ROLL_OTH_PAT <- local({
  q <- "held for sale|reclassif|classified as"
  paste0("(cash|equivalents).{0,80}(", q, ")|(", q, ").{0,40}(cash|equivalents)")
})
## PAT AND NOT LIVE HERE, NOT IN THE FOOTING SECTION BELOW, BECAUSE THE
## RESOLVER NEEDS THEM. resolve_netchange() reads PAT to find the section
## subtotals and it runs long before the footing check. With PAT defined
## further down, every call threw "object 'PAT' not found", the tryCatch
## around it swallowed the error, and the pass silently did nothing - which
## looks exactly like a rule that never matches. Keep definitions above use.

PAT <- c(
  op  = "net cash.{0,40}operat|cash (provided|used).{0,30}operat",
  inv = "net cash.{0,40}invest|cash (provided|used).{0,30}invest",
  fin = "net cash.{0,40}financ|cash (provided|used).{0,30}financ",
  fx  = "effect of (exchange|foreign|currency)",
  ## the roll-forward lines come from ROLL, defined with the resolver, so the
  ## check and the resolver cannot disagree about which line is the net change
  net = ROLL[["net"]], beg = ROLL[["beg"]], end = ROLL[["end"]],
  ## A DISCONTINUED-OPERATIONS BLOCK IS A FOURTH TERM, NOT A SECTION. Abbott
  ## 2004-2008 splits the statement into continuing and discontinued:
  ##   Net Cash From Operating Activities of Continuing Operations    2,578,198
  ##   Net Cash (Used in) Investing Activities of Continuing Ops     -3,273,521
  ##   Net Cash From (Used in) Financing Activities of Continuing Ops    69,560
  ##   Effect of exchange rates                                             852
  ##   Net cash provided by discontinued operations                     861,360
  ##   Net Increase in Cash and Cash Equivalents                        236,449
  ## which foots exactly. But "cash provided by discontinued operations" also
  ## satisfies the operating pattern, so taking the last match read operating
  ## as 861,360 instead of 2,578,198 and put the column 2.6bn out - the same
  ## last-match mistake as Walmart's held-for-sale line, in a new caption.
  ## 16 of Abbott's 25 filings failed on this one line.
  ##
  ## The block's own total comes last, exactly as a section's subtotal does, so
  ## the last match is the total and its components are not double counted.
  disc = "discontinued operation")

## `discontinued` is excluded from op/inv/fin so those pick the CONTINUING
## subtotal, and carried as its own term instead.
NOT <- c(net = ROLL_OTH, beg = ROLL_OTH, end = ROLL_OTH,
         op = "discontinued", inv = "discontinued", fin = "discontinued")

is_section_subtotal <- function(lab)
  Reduce(`|`, lapply(SUBTOT$SCF, function(p) grepl(p, lab, perl = TRUE)))

## THE THIRD IDENTITY: SECTIONS + FX + ANYTHING BELOW THEM = NET CHANGE.
## Abbott's 10-Q for June 2007 prints
##   Net Cash From Operating Activities                              2,595,742
##   Net Cash (Used in) Investing Activities                          -549,639
##   Net Cash (Used in) From Financing Activities                   -1,038,878
##   Effect of exchange rate changes on cash                            10,514
##   Net cash provided by operating activities of discontinued ops       67,152
##   Net Increase (Decrease) in Cash and Cash Equivalents            1,017,739
## and the first four already sum to 1,017,739 - so the 67,152 does NOT belong
## to the 2007 column at all. It is a single printed number on a two-column
## row, filled from the left into the wrong column; the 2006 comparative is
## short by exactly 67,152. The section pass cannot see the line (it sits
## outside every section) and the roll-forward pass cannot either (beg + net
## already closes), so the cell stayed unplaced and the column read as a
## failure. This identity pins it exactly, column by column.
resolve_netchange <- function(d) {
  if (!any(d$col_unplaced)) return(d)
  nc <- max(d$col_order)
  if (nc < 2) return(d)
  w   <- d[order(d$line_order, d$col_order), ]
  ord <- unique(w$line_order)
  lab <- tolower(trimws(w$line_label[match(ord, w$line_order)]))
  vof <- function(i) {
    s <- w$line_order == ord[i]
    w$value_printed[s][order(w$col_order[s])]
  }

  ## the section subtotals: last match per section, discontinued excluded so
  ## these are the continuing or combined subtotals, never a component
  sec <- unlist(lapply(c("op", "inv", "fin"), function(k) {
    i <- grep(PAT[[k]], lab, perl = TRUE)
    i <- i[!grepl("discontinued", lab[i], perl = TRUE)]
    if (length(i)) i[length(i)] else NULL
  }))
  if (!length(sec)) return(d)
  inet <- grep(ROLL[["net"]], lab, perl = TRUE)
  inet <- inet[!grepl(ROLL_OTH, lab[inet], perl = TRUE)]
  if (length(inet) != 1) return(d)
  net <- vof(inet)
  if (any(is.na(net))) return(d)

  tot <- rep(0, nc)
  for (i in sec) { v <- vof(i); if (any(is.na(v))) return(d); tot <- tot + v }
  ifx <- grep(PAT[["fx"]], lab, perl = TRUE)
  if (length(ifx)) {
    v <- vof(ifx[length(ifx)])
    if (!any(is.na(v))) { tot <- tot + v; ifx <- ifx[length(ifx)] } else ifx <- integer()
  }

  ## candidates sit BELOW the last section subtotal and ABOVE the net-change
  ## line; fx and the subtotals themselves are already in `tot`
  lo   <- max(ord[sec]); hi <- ord[inet]
  band <- setdiff(which(ord > lo & ord < hi), c(sec, ifx))
  band <- band[vapply(band, function(i) !all(is.na(vof(i))) ||
    any(w$col_unplaced[w$line_order == ord[i]]), logical(1))]
  amb  <- band[vapply(band, function(i)
    any(w$col_unplaced[w$line_order == ord[i]]), logical(1))]
  ## ONE unplaced line only: with two, several splits of the gap satisfy the
  ## identity and this would be guessing rather than solving
  if (length(amb) != 1) return(d)

  req <- net - tot
  for (i in setdiff(band, amb)) {
    v <- vof(i); if (any(is.na(v))) return(d); req <- req - v
  }

  vals <- vof(amb); vals <- vals[!is.na(vals)]
  cc   <- which(abs(req) > FOOT_TOL)
  if (!length(vals) || length(cc) != length(vals)) return(d)
  if (any(abs(req[cc] - vals) > FOOT_TOL)) return(d)

  nv <- rep(NA_real_, nc); nv[cc] <- vals
  sel <- d$line_order == ord[amb]
  d$value_printed[sel][order(d$col_order[sel])] <- nv
  d$col_placed_by_footing[sel] <- TRUE
  d$col_unplaced[sel] <- FALSE
  d
}

resolve_rollforward <- function(d) {
  if (!any(d$col_unplaced)) return(d)
  nc <- max(d$col_order)
  if (nc < 2) return(d)
  w   <- d[order(d$line_order, d$col_order), ]
  ord <- unique(w$line_order)
  lab <- tolower(trimws(w$line_label[match(ord, w$line_order)]))
  vof <- function(i) w$value_printed[w$line_order == ord[i]][order(w$col_order[w$line_order == ord[i]])]

  ## the three anchors must each resolve to exactly one line and be fully
  ## printed; anything less and the identity cannot pin a cell
  pick <- function(p) {
    i <- grep(p, lab, perl = TRUE)
    i[!grepl(ROLL_OTH, lab[i], perl = TRUE)]
  }
  inet <- pick(ROLL[["net"]]); ibeg <- pick(ROLL[["beg"]])
  iend <- pick(ROLL[["end"]])
  ioth <- grep(ROLL_OTH_PAT, lab, perl = TRUE)
  ioth <- ioth[!is_section_subtotal(lab[ioth])]
  if (length(inet) != 1 || length(ibeg) != 1 || length(iend) != 1) return(d)
  ## ONE unplaced line only. With two, several splits of the gap satisfy the
  ## identity and the resolver would be guessing; the existing section pass
  ## declines on a tie for the same reason, so do the same here.
  amb <- ioth[vapply(ioth, function(i)
    any(w$col_unplaced[w$line_order == ord[i]]), logical(1))]
  if (length(amb) != 1) return(d)
  fixed <- setdiff(ioth, amb)

  net <- vof(inet); beg <- vof(ibeg); end <- vof(iend)
  if (any(is.na(net)) || any(is.na(beg)) || any(is.na(end))) return(d)
  req <- end - beg - net
  for (i in fixed) {
    v <- vof(i); if (any(is.na(v))) return(d); req <- req - v
  }

  ## the printed numbers, in the order printed, must BE the non-zero gaps -
  ## same count, same values, same order. No search, no reordering.
  vals <- vof(amb); vals <- vals[!is.na(vals)]
  cc   <- which(abs(req) > FOOT_TOL)
  if (!length(vals) || length(cc) != length(vals)) return(d)
  if (any(abs(req[cc] - vals) > FOOT_TOL)) return(d)

  nv <- rep(NA_real_, nc); nv[cc] <- vals
  sel <- d$line_order == ord[amb]
  d$value_printed[sel][order(d$col_order[sel])] <- nv
  d$col_placed_by_footing[sel] <- TRUE
  d$col_unplaced[sel] <- FALSE
  d
}

resolve_unplaced <- function(d, stmt) {
  if (!any(d$col_unplaced)) return(d)
  SB <- SUBTOT[[stmt]]
  if (is.null(SB)) return(d)
  nc <- max(d$col_order)
  if (nc < 2) return(d)
  w <- d[order(d$line_order, d$col_order), ]
  ord <- unique(w$line_order)
  lab <- tolower(trimws(w$line_label[match(ord, w$line_order)]))
  ## section boundaries: a subtotal closes its own section, so each line takes
  ## the first subtotal at or after it. Same rule as 001a uses on the XBRL side.
  sub <- rep(NA_character_, length(ord))
  for (k in names(SB)) sub[grepl(SB[[k]], lab)] <- k
  sec <- rep(NA_character_, length(ord))
  nxt <- NA_character_
  for (i in rev(seq_along(ord))) {
    if (!is.na(sub[i])) nxt <- sub[i]
    sec[i] <- nxt
  }
  for (k in names(SB)) {
    si <- which(sec == k & is.na(sub))                     # components
    ti <- which(sec == k & !is.na(sub))                     # the subtotal
    if (!length(ti) || !length(si)) next
    tot <- w$value_printed[w$line_order == ord[ti[1]]]
    if (any(is.na(tot))) next
    amb <- si[vapply(si, function(i)
      any(w$col_unplaced[w$line_order == ord[i]]), logical(1))]
    if (!length(amb)) next
    fix <- setdiff(si, amb)
    ## Where the subtotal is a running one rather than a sum of the block, the
    ## block starts from the PREVIOUS subtotal, not from zero. An income
    ## statement's "Operating income" follows "Gross profit"; a balance sheet's
    ## "Total assets" follows "Total current assets". Taking the nearest
    ## subtotal above the block as the opening balance covers both, and for a
    ## block with nothing above it the opening balance is zero.
    pv <- rep(0, nc)
    above <- which(!is.na(sub) & seq_along(ord) < min(si))
    if (isTRUE(SIGNED[[stmt]]) && length(above)) {
      p <- w$value_printed[w$line_order == ord[max(above)]]
      if (!any(is.na(p))) pv <- p
    }
    sgn <- isTRUE(SIGNED[[stmt]])
    ## FIXED lines: known values, unknown signs only where the statement nets.
    fv <- lapply(fix, function(i) {
      v <- w$value_printed[w$line_order == ord[i]]
      ifelse(is.na(v), 0, v)
    })
    ## AMBIGUOUS lines: the printed values, to be placed in some increasing
    ## choice of columns, with a sign each where the statement nets.
    vals <- lapply(amb, function(i) {
      v <- w$value_printed[w$line_order == ord[i]]
      v[!is.na(v)]
    })
    opts <- lapply(vals, function(v) combn(nc, length(v), simplify = FALSE))

    ## ---- STEP 1: PIN THE SIGNS ON THE CLEAN COLUMNS --------------------
    ## A line's sign is the same in every column, and most columns have no
    ## missing value at all - Biomet's four-column income statement has two
    ## clean ones. Those columns determine the signs on their own:
    ##
    ##   col 1:  1,878.1 - 1,042.3 - 106.6 - 372.6 = 356.6
    ##   col 4:    146.5 -   194.2 -  34.0 -   0.5 = -82.2
    ##
    ## and no other sign pattern satisfies both. So the signs come out of the
    ## clean columns rather than out of a search over the whole block, which
    ## is what made the brute-force cap bind (Matt, 2026-11-17).
    ##
    ## The convention is tried first and almost always holds: within a block
    ## closed by a running subtotal the components SUBTRACT, and within one
    ## closed by a "total X" they ADD. Only if neither verifies does this fall
    ## back to enumerating sign patterns, and only then does the cap apply.
    ## Which columns are clean is not known in advance - any column could be
    ## the one a missing value belongs to. But a clean column is exactly a
    ## column the FIXED lines already foot, so the right sign vector is the one
    ## that foots the most columns on the fixed lines alone, and the columns it
    ## misses are the ones awaiting a missing value.
    sv_for <- function(sv) {
      b <- pv
      for (q in seq_along(fix)) b <- b + sv[q] * fv[[q]]
      b
    }
    nf <- length(fix)
    cand <- if (!sgn) list(rep(1, nf)) else {
      cc <- list(rep(-1, nf), rep(1, nf))        # the convention, both ways
      ok <- vapply(cc, function(sv) sum(abs(sv_for(sv) - tot) <= FOOT_TOL) > 0, logical(1))
      if (any(ok)) cc[ok] else {
        if (2^nf > MAX_COMB) list() else
          lapply(seq_len(2^nf) - 1, function(z)
            ifelse(bitwAnd(bitwShiftR(z, seq_len(nf) - 1), 1L) == 1L, 1, -1))
      }
    }
    if (!length(cand)) next
    ## Narrow to the sign vectors that foot the most columns on the fixed lines
    ## - but only where signs were actually unknown, and only when at least one
    ## column IS clean. Dover Downs' financing section is the counter-example:
    ## both of its columns are waiting on a missing value, so no column foots on
    ## the fixed lines and there is nothing to narrow. Bailing out there took
    ## the cash flow statement from 100% resolved to 0%.
    if (sgn && length(cand) > 1) {
      nfoot <- vapply(cand, function(sv)
        sum(abs(sv_for(sv) - tot) <= FOOT_TOL), integer(1))
      if (max(nfoot) > 0) cand <- cand[nfoot == max(nfoot)]
    }

    ## ---- STEP 2: CLOSE THE GAP IN EVERY OTHER COLUMN -------------------
    ## With the signs fixed, each column has one number missing from it and the
    ## gap says what that number must be. No search over placements.
    pgrid <- expand.grid(lapply(opts, seq_along), KEEP.OUT.ATTRS = FALSE)
    hit <- NULL
    for (sv0 in cand) {
      sv <- c(sv0, rep(sv0[1], length(amb)))     # ambiguous rows share the sign
      if (!sgn) sv <- rep(1, nf + length(amb))
      b <- sv_for(sv0)
      for (g in seq_len(nrow(pgrid))) {
        s <- b
        for (j in seq_along(amb)) {
          cc2 <- opts[[j]][[pgrid[g, j]]]
          s[cc2] <- s[cc2] + sv[nf + j] * vals[[j]]
        }
        if (all(abs(s - tot) <= FOOT_TOL)) {
          if (!is.null(hit)) { hit <- NA; break }            # a tie: give up
          hit <- list(g = g)
        }
      }
      if (length(hit) == 1 && is.na(hit[[1]])) break
    }
    if (is.null(hit) || (length(hit) == 1 && is.na(hit[[1]]))) next
    g <- hit$g
    for (j in seq_along(amb)) {
      cc <- opts[[j]][[pgrid[g, j]]]
      nv <- rep(NA_real_, nc); nv[cc] <- vals[[j]]
      sel <- d$line_order == ord[amb[j]]
      d$value_printed[sel][order(d$col_order[sel])] <- nv
      d$col_placed_by_footing[sel] <- TRUE
      d$col_unplaced[sel] <- FALSE
    }
  }
  d
}

L <- bind_rows(lapply(raw, one))
if (!nrow(L))
  stop("no usable extractions - every record had found=FALSE or no rows. ",
       "Check that the input really is legacy/extract-statements's output and not the bare ",
       "model answers without their chunk metadata.")
cat("long rows:", nrow(L), " filings:", length(unique(L$wrdsfname)),
    "  (records with found=FALSE are excluded by design)\n")

## ---- resolve the unplaced values, then rescale ----------------------------
## Runs before `amount` is used for anything, because moving a value between
## columns changes which period it belongs to.
if (!"col_unplaced" %in% names(L)) L$col_unplaced <- FALSE
L$col_unplaced[is.na(L$col_unplaced)] <- FALSE
L$col_placed_by_footing <- FALSE
n_amb0 <- sum(L$col_unplaced)
if (n_amb0) {
  L <- bind_rows(lapply(split(L, paste(L$wrdsfname, L$statement)), function(d) {
    st <- unique(d$statement)
    if (!any(d$col_unplaced) || length(st) != 1 || !st %in% names(SUBTOT)) return(d)
    d <- tryCatch(resolve_unplaced(d, st), error = function(e) d)
    ## The section pass only sees lines that belong to a section, so a cash
    ## movement printed BELOW the financing subtotal is invisible to it - all 94
    ## of Walmart's unresolved cells were that one held-for-sale line. The
    ## roll-forward identity pins those exactly, so run it second on whatever
    ## the section pass left flagged.
    if (identical(st, "SCF"))
      d <- tryCatch(resolve_rollforward(d), error = function(e) d)
      d <- tryCatch(resolve_netchange(d),   error = function(e) d)
    d
  }))
  ## amount is derived from value_printed, so recompute it after the move
  L$amount <- ifelse(L$unscaled_row, L$value_printed, L$value_printed * L$mult)
}
cat(sprintf("unplaced cells: %d  resolved by footing: %d (%.1f%%)  still flagged: %d\n",
            n_amb0, sum(L$col_placed_by_footing),
            if (n_amb0) 100 * sum(L$col_placed_by_footing) / n_amb0 else 0,
            sum(L$col_unplaced)))

L <- L %>% select(wrdsfname, cik, coname, form, fdate, rdate,
                  statement, stmt_raw, line_order, line_label,
                  col_order, col_heading, period_end, period_months,
                  value_printed, scale, mult, scale_assumed_ones,
                  unscaled_row, amount,
                  col_unplaced, col_placed_by_footing,
                  len_mismatch, row_overcount, row_printed) %>%
  arrange(wrdsfname, statement, line_order, col_order)

## ---- what the frame is missing, stated rather than buried ------------------
cat("\n=== completeness ===\n")
cat(sprintf("scale missing      : %6d rows (%.1f%%) across %d filings\n",
            sum(is.na(L$scale)), 100 * mean(is.na(L$scale)),
            length(unique(L$wrdsfname[is.na(L$scale)]))))
cat(sprintf("period_end missing : %6d rows (%.1f%%)\n",
            sum(is.na(L$period_end)), 100 * mean(is.na(L$period_end))))
cat(sprintf("v-length mismatch  : %6d rows across %d filings\n",
            sum(L$len_mismatch), length(unique(L$wrdsfname[L$len_mismatch]))))
cat(sprintf("amount NA but value present: %6d rows (scale unknown)\n",
            sum(is.na(L$amount) & !is.na(L$value_printed))))
## printed more numbers than there are columns: the surplus is in row_printed,
## not lost, but it usually means the column count was misread - worth looking
## at the filing rather than trusting the row
no <- L %>% filter(row_overcount) %>% distinct(wrdsfname, line_order)
cat(sprintf("rows with surplus numbers: %6d across %d filings%s\n",
            nrow(no), n_distinct(no$wrdsfname),
            if (nrow(no)) " (see row_printed; re-check their column count)" else ""))

## ---- footing check ---------------------------------------------------------
## Only the cash flow statement has an arithmetic identity this clean:
##   operating + investing + financing + fx = net change in cash
##   beginning + net change = end
## Label wordings vary a lot, so the patterns are deliberately loose and the
## LAST match in a section is taken - the subtotal, not the section header.

grab <- function(lab, val, p, not = NA_character_) {
  lo <- tolower(lab)
  i  <- grep(p, lo, perl = TRUE)
  if (!is.na(not)) i <- i[!grepl(not, lo[i], perl = TRUE)]
  if (!length(i)) return(NA_real_)
  val[i[length(i)]]
}

## THE EXCLUDED CASH LINES ARE NOT NOISE - THEY BELONG TO THE ROLL-FORWARD.
## Walmart FY2022 prints
##   Net increase (decrease) in cash ...     -4,802
##   Change in cash reclassified ... held for sale   1,848
##   Beginning of year                       17,788
##   End of year                             14,834
## and 17,788 - 4,802 + 1,848 = 14,834 exactly. So a cash movement that is
## neither one of the three sections nor the net-change line still moves the
## cash balance: drop it from the section sum, keep it in the roll-forward.
## IS THE DISCONTINUED BLOCK A SEPARATE TERM, OR ALREADY INSIDE THE SECTIONS?
## Both presentations exist and they need opposite handling, so this cannot be
## decided from the discontinued line alone. Walmart 2003-2008 prints THREE
## lines per section and the combined total comes last:
##   Net cash provided by operating activities of continuing operations    2,614
##   Net cash provided by operating activities of discontinued operations     39
##   Net cash provided by operating activities                             2,653
## Abbott prints only the continuing subtotal and puts discontinued in its own
## block below the three sections, with no combined line anywhere.
##
## So the test is NOT "does a subtotal mention continuing" - Walmart's does
## too, and gating on that double counted it from 203 of 203 columns down to
## 117. The test is whether an UNQUALIFIED subtotal exists: if one does it
## already includes discontinued, and the block must not be added again.
## DECIDED BY POSITION, NOT BY CAPTION. Two caption rules were tried and both
## broke, because Abbott alone prints three styles:
##   2004  "...Operating Activities of Continuing Operations" + a discontinued
##         block of its own below the sections, with its own total
##   2006  "Net Cash From Operating Activities" - no qualifier at all - and
##         STILL a discontinued line below financing that must be added
##   Walmart "...of continuing operations", "...of discontinued operations",
##         then the combined "Net cash provided by operating activities"
## Gating on "a subtotal says continuing" double counted Walmart (203 -> 117
## columns). Gating on "an unqualified subtotal exists" then missed Abbott 2006,
## whose subtotals are unqualified yet exclude discontinued.
##
## Position settles it, because position is what decides whether a number is
## already inside a subtotal: a discontinued line printed AFTER the last
## section subtotal is a separate term, and one printed before it is a
## component of the section it sits in and is already counted.
grab_disc <- function(ord, lab, val) {
  lo <- tolower(lab)
  si <- unique(unlist(lapply(c("op", "inv", "fin"),
                             function(k) grep(PAT[[k]], lo, perl = TRUE))))
  si <- si[!grepl("discontinued", lo[si], perl = TRUE)]
  if (!length(si)) return(0)
  di <- grep(PAT[["disc"]], lo, perl = TRUE)
  di <- di[ord[di] > max(ord[si])]
  if (!length(di)) return(0)
  ## the block's own total comes last, exactly as a section's subtotal does,
  ## so its components are not double counted
  v <- val[di[which.max(ord[di])]]
  if (is.na(v)) 0 else v
}

grab_oth <- function(lab, val) {
  lo <- tolower(lab)
  i  <- grep(ROLL_OTH_PAT, lo, perl = TRUE)
  i  <- i[!is_section_subtotal(lo[i])]
  if (!length(i)) return(0)
  sum(val[i], na.rm = TRUE)
}

foot <- L %>%
  filter(statement == "SCF") %>%
  group_by(wrdsfname, cik, coname, form, rdate, col_order, period_end,
           period_months, mult) %>%
  summarise(
    ## THE CHECK RUNS ON THE PRINTED FIGURES, NOT ON SCALED DOLLARS.
    ## Footing is scale-invariant: if the printed numbers add up, so do the
    ## dollars. Running it on `amount` meant that a filing stating no
    ## multiplier - where amount is NA by design - could not be tested at all,
    ## and those are exactly the small filers whose statements most need
    ## checking. Three of the ten filings in the first in-harness run were
    ## lost this way, 6 of 18 period-columns (Matt, 2026-11-19).
    ##
    ## The dollar values are kept beside them, so a reader still sees the
    ## section totals in the units the rest of the frame uses.
    op  = grab(line_label, value_printed, PAT[["op"]],  NOT[["op"]]),
    inv = grab(line_label, value_printed, PAT[["inv"]], NOT[["inv"]]),
    fin = grab(line_label, value_printed, PAT[["fin"]], NOT[["fin"]]),
    fx  = grab(line_label, value_printed, PAT[["fx"]]),
    net = grab(line_label, value_printed, PAT[["net"]], NOT[["net"]]),
    beg = grab(line_label, value_printed, PAT[["beg"]], NOT[["beg"]]),
    end = grab(line_label, value_printed, PAT[["end"]], NOT[["end"]]),
    oth = grab_oth(line_label, value_printed),
    disc = grab_disc(line_order, line_label, value_printed),
    op_usd  = grab(line_label, amount, PAT[["op"]],  NOT[["op"]]),
    inv_usd = grab(line_label, amount, PAT[["inv"]], NOT[["inv"]]),
    fin_usd = grab(line_label, amount, PAT[["fin"]], NOT[["fin"]]),
    n_lines = n(), .groups = "drop") %>%
  mutate(
    ## A SECTION THAT IS NOT PRINTED COUNTS AS ZERO, AND THAT IS SAFE. Adams
    ## Resources' 10-Qs print no financing section at all, so demanding all
    ## three left 15 of its 17 filings untestable although operating +
    ## investing equals the printed net change exactly (-9,825 - 3,364 =
    ## -13,189) AND the cash balance rolls forward exactly (36,140 - 13,189 =
    ## 22,951). Two independent confirmations, thrown away over an absence.
    ##
    ## Treating the absence as zero cannot hide a truncated statement: if the
    ## section really was there with a non-zero total, the identity stops
    ## closing and the column is flagged. The only case it silently passes is a
    ## section that is missing AND zero, where zero is the right answer anyway.
    ## At least one section must be found, or there is nothing to test.
    n_sec = (!is.na(op)) + (!is.na(inv)) + (!is.na(fin)),
    ## the discontinued block is added ONLY where the sections are captioned
    ## position decides whether it is already counted; see grab_disc()
    disc_term = coalesce(disc, 0),
    sum_sections = ifelse(n_sec == 0, NA_real_,
                          rowSums(cbind(op, inv, fin, coalesce(fx, 0),
                                        disc_term), na.rm = TRUE)),
    diff_net     = sum_sections - net,
    diff_roll    = (beg + net + coalesce(oth, 0)) - end,
    ## A tolerance, because a filer computes a subtotal on exact figures and
    ## then rounds, so rounded components need not sum to the rounded
    ## subtotal. On PRINTED figures that tolerance is one printed unit,
    ## whatever the statement's scale happens to be.
    tol          = 1,
    foots_net    = !is.na(diff_net)  & abs(diff_net)  <= tol,
    foots_roll   = !is.na(diff_roll) & abs(diff_roll) <= tol,
    ## UNTESTED IS NOT FAILED, AND THE DIFFERENCE COSTS MONEY. The verdict
    ## decides whether a statement is re-read on a more expensive model, so a
    ## column whose roll-forward was never printed must not be reported as a
    ## roll-forward failure. K-Tron's 2002 10-Q footed to the penny and was
    ## called a failure purely because its cash balances carried no cash word.
    testable_net  = !is.na(sum_sections) & !is.na(net),
    testable_roll = !is.na(beg) & !is.na(net) & !is.na(end),
    verdict      = case_when(
      !testable_net           ~ "not testable",
      !foots_net              ~ "FAILS",
      !testable_roll          ~ "foots (no roll-forward printed)",
      foots_roll              ~ "foots",
      TRUE                    ~ "sections foot, roll-forward does not"))

cat("\n=== cash flow footing, by period-column ===\n")
print(table(foot$verdict))

## ESCALATE ONLY AN ARITHMETIC CONTRADICTION. "not testable" means a subtotal
## was never found, which is usually the locator handing over a truncated
## statement (K-Tron FY2001 lost its financing section at a page break) - a
## stronger reader cannot recover text that is not there, so those go to a
## review queue instead of to a paid re-read.
foots_ok <- c("foots", "foots (no roll-forward printed)")
esc <- foot %>% filter(!verdict %in% c(foots_ok, "not testable")) %>%
  distinct(wrdsfname) %>% pull(wrdsfname)
rev <- foot %>% filter(verdict == "not testable") %>%
  distinct(wrdsfname) %>% pull(wrdsfname)
cat(sprintf("\ncolumns that foot: %.1f%% of %d\n",
            100 * mean(foot$verdict %in% foots_ok), nrow(foot)))
cat(sprintf("columns to review (subtotal not found): %d\n",
            sum(foot$verdict == "not testable")))
cat(sprintf("filings to escalate to a stronger model: %d of %d (%.1f%%)\n",
            length(esc), length(unique(foot$wrdsfname)),
            100 * length(esc) / max(1, length(unique(foot$wrdsfname)))))

## ---- write -----------------------------------------------------------------
write_parquet(L,    paste0(SP, "scf_text_lines.parquet"))
write_parquet(foot, paste0(SP, "scf_text_foot.parquet"))
write.csv(L, paste0(SP, "scf_text_lines.csv"), row.names = FALSE, na = "")
writeLines(esc, paste0(SP, "escalate.txt"))
writeLines(rev, paste0(SP, "review.txt"))

cat("\nwrote scf_text_lines.parquet (", nrow(L), " rows ), ",
    "scf_text_foot.parquet (", nrow(foot), " rows ), escalate.txt\n", sep = "")
