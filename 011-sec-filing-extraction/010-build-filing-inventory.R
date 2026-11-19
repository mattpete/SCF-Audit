## 010 -- BUILD THE FILING INVENTORY
##
## Step 1 of five. One row per filing we could ever want to extract, with the
## screens recorded as COLUMNS rather than applied as filters, and a status
## column the collection step fills in.
##
## In : WRDS wrdssec.wrds_forms      (the filing index)
##      WRDS wrdssec.wrds_forms_reg  (one row per registrant per filing; the
##                                    only place the SEC-assigned SIC lives)
## Out: {SEC_DATA_PATH}/inventory/filing_inventory.parquet
##
## Numbered 010 because it runs BEFORE the locate step (011) and the extract
## step (012). It renumbers nothing else.
##
## WHY SCREENS ARE COLUMNS AND NOT A FILTER. The sample definition is not
## settled -- whether banks, insurers and utilities stay is an open research
## decision (Matt, 2026-11-19). A filtered inventory means re-querying WRDS
## and, worse, re-collecting text every time that decision moves. So every
## screen lands as a flag plus a reason string, `in_sample` is derived from
## them at the bottom, and changing your mind is a one-line edit and a re-run
## of THIS script only.
##
## WHY A STATUS COLUMN. Collection is incremental and will be interrupted. The
## inventory is the ledger: step 2 reads the rows where `collected` is FALSE,
## and writes back. A re-run of this script PRESERVES the status of every row
## it has seen before (see Section 7) -- it must never reset progress just
## because the form list or a screen changed.

suppressPackageStartupMessages({
  library(RPostgres); library(DBI); library(keyring)
  library(dplyr); library(arrow); library(glue)
})

## ---------------------------------------------------------------- 1. config

## RUN THIS FROM THE REPO ROOT. .Renviron lives there, so R started in any
## subfolder gets an EMPTY SEC_DATA_PATH and the output lands at the root of
## C: instead. That happened on the first run of this script, 2026-11-19 --
## hence the guard rather than a comment.
SEC <- Sys.getenv("SEC_DATA_PATH")
if (!nzchar(SEC)) stop(
  "SEC_DATA_PATH is empty. Run from the repo root so .Renviron loads, ",
  "or set it in this session.")
OUT_DIR <- paste0(SEC, "/inventory")
## paste0, NOT file.path: SEC_DATA_PATH carries no trailing slash and
## file.path strips it.

## TWO DATE WINDOWS, AND THEY ARE NOT THE SAME THING.
##
## DATE_* bounds the FILING date and exists only to bound the pull. It runs a
## year past the panel on purpose, because an annual report is filed the
## spring after the year it covers. Measured on the first run, 2026-11-19:
## with DATE_MAX at 2023-12-31 the last year of the panel held 1,155 annual
## filings against ~6,600 in a normal year. That was not censoring, it was
## the query clipping the filings that carry the last year's statements.
##
## PERIOD_* bounds the period of report, which is what the panel actually is,
## and it is applied as a FLAG (see Section 4), not a filter.
DATE_MIN   <- "2002-01-01"
DATE_MAX   <- "2026-12-31"
PERIOD_MIN <- "2001-01-01"
PERIOD_MAX <- "2025-12-31"
## PERIOD_MAX stops at the last COMPLETE fiscal year in the index (which runs
## to 2026-11). DATE_MAX runs a year beyond it so the annual reports for that
## year, filed the following spring, are in the pull.
##
## RIGHT-EDGE CENSORING IS A SEPARATE ISSUE and is not handled by narrowing
## this. Detecting a revision needs a LATER filing that re-presents the
## period, so the most recent two years can only ever look clean -- the same
## effect the XBRL panel flags as `era == "censored right edge"`. Those
## filings are still collected, because they are the comparatives that reveal
## revisions in EARLIER periods. Flag the right edge at analysis; do not drop
## it here.
## 2002 is where Audit Analytics coverage and I/B/E/S cash flow forecasts
## begin, which is the whole reason this track exists.
##
## PERIOD_MIN is a year EARLIER than DATE_MIN deliberately: an FY2001 annual
## report is filed in spring 2002, so its period belongs to the panel while
## its filing date sits at the very start of the window. Periods before that
## cannot be studied even when a 2002 amendment mentions them, because the
## ORIGINAL filing was made before 2002 and is not in the collection -- with
## no original there is nothing to compare a revision against.

## FORM LIST, and the three traps in it.
##
## 1. SMALL-BUSINESS FORMS HAVE NO HYPHEN. They are `10KSB` and `10QSB`, not
##    `10-KSB`. A form list built with `form LIKE '10-%'` silently drops
##    109,057 filings from ~8,000 filers, every one of them between 2002 and
##    2009 -- i.e. precisely the pre-XBRL window this track is for. Found
##    2026-11-19 by querying the index instead of assuming.
## 2. `10-K405` is a real annual report. It was the Item 405 delinquency
##    checkbox variant, retired in 2003; 2,168 of them sit in Jan-Apr 2002.
##    `10KSB40` is the same thing for small-business filers.
## 3. TRANSITION REPORTS (`10-KT`, `10-QT`) cover a shortened period after a
##    fiscal-year change. They carry real statements and they are exactly the
##    periods a revision study must not lose, because a fiscal-year change is
##    one of the legitimate reasons comparatives get recast.
##
## DELIBERATELY EXCLUDED: `10-D` (143,330 filings) is an asset-backed
## distribution report, not a financial statement. `10-12B`/`10-12G` are
## registration statements. `NT 10-K` is a notice of late filing.
FORMS <- list(
  annual    = c("10-K", "10-K405", "10-KT", "10KSB", "10KSB40"),
  annual_a  = c("10-K/A", "10-K405/A", "10-KT/A", "10KSB/A", "10KSB40/A"),
  quarter   = c("10-Q", "10-QT", "10QSB"),
  quarter_a = c("10-Q/A", "10-QT/A", "10QSB/A")
)

## SIC CODES TO DROP: entities that are not operating companies, so there is
## no cash flow statement to revise. Measured counts are 2002+ 10-K/10-Q.
##
## Banks (6021/6022/6035/6036), insurers (6311/6321/6331/6411), brokers
## (6211) and real-estate operators (6500/6512/6513/6531) are KEPT. The
## conventional "drop SIC 6000-6999" screen exists because leverage and
## accruals foul up valuation tests, which is not what the descriptive half
## of this paper does, and those filers have ordinary cash flow statements
## where classification matters (Matt, 2026-11-19).
DROP_SIC <- c(
  "6189",  ## asset-backed securities. 9,806 filers -- 28% of the entire
           ## index -- filing a 10-K of servicing reports with NO financial
           ## statements. These are the 5-of-500 holdout misses.
  "6770",  ## blank checks, shells, SPACs. 2,790 filers, near-empty SCF.
  "6798",  ## REITs. 966 filers but 34,535 filings, because the Inc and the
           ## operating LP file separately as two CIKs with near-identical
           ## statements -- double counting, and the main source of the
           ## CIK-to-gvkey mess.
  "6726",  ## investment offices, closed-end funds, BDCs
  "6792",  ## oil royalty traders
  "6794",  ## patent owners and lessors -- NOTE this sweeps in a few real
           ## franchisors; 81 filers, so cheap either way. Flip if wanted.
  "6795",  ## mineral royalty traders
  "6799"   ## "investors, not elsewhere classified"
)

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
OUT <- paste0(OUT_DIR, "/filing_inventory.parquet")

## ------------------------------------------------------------- 2. the query
## The SIC lives on wrds_forms_reg, one row per registrant per filing. Joining
## it to the index on accession ALONE fans a multi-registrant filing out into
## several rows, so the join is on (accession, regcik = cik): the SIC of the
## registrant whose CIK the index row is actually under. 36,044 filings
## (4.9%) carry more than one registrant, so this is not a corner case.
##
## regseq = 1 is the primary registrant; it is kept as a fallback for the
## 25,211 filings (3.4%) where the (accession, cik) pair finds no SIC.

cat("connecting to WRDS...\n")
wrds <- dbConnect(Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu", port = 9737,
  dbname = "wrds", sslmode = "require",
  user = key_get("wrds-user"), password = key_get("wrds-password"))

all_forms <- unlist(FORMS, use.names = FALSE)
in_list   <- paste0("'", all_forms, "'", collapse = ",")

q <- glue("
WITH ix AS (
  SELECT cik, coname, form, accession, wrdsfname, fdate, rdate,
         regcount, fsize_clean
  FROM wrdssec.wrds_forms
  WHERE form IN ({in_list})
    AND fdate BETWEEN '{DATE_MIN}' AND '{DATE_MAX}'
),
own AS (   -- the SIC of this filing's own registrant
  SELECT accession, regcik, MIN(regsic) AS regsic
  FROM wrdssec.wrds_forms_reg GROUP BY accession, regcik
),
pri AS (   -- fallback: the primary registrant on the filing
  SELECT accession, MIN(regsic) AS regsic_pri
  FROM wrdssec.wrds_forms_reg WHERE regseq = 1 GROUP BY accession
)
SELECT ix.*, own.regsic AS regsic_own, pri.regsic_pri
FROM ix
LEFT JOIN own ON own.accession = ix.accession AND own.regcik = ix.cik
LEFT JOIN pri ON pri.accession = ix.accession")

cat("querying the index (expect ~840k rows, a minute or two)...\n")
d <- dbGetQuery(wrds, q)
dbDisconnect(wrds)
cat("  rows returned:", format(nrow(d), big.mark = ","), "\n")

## ------------------------------------------------- 3. identifiers and dates

d <- d |>
  mutate(
    cik = stringr::str_pad(as.character(cik), width = 10, pad = "0"),
    ## 10-digit zero padding is the house convention, so this joins to the
    ## XBRL panel and to Audit Analytics without further thought.
    regsic = dplyr::coalesce(regsic_own, regsic_pri),
    sic2   = substr(regsic, 1, 2),

    form_grp = dplyr::case_when(
      form %in% FORMS$annual    ~ "annual",
      form %in% FORMS$annual_a  ~ "annual",
      form %in% FORMS$quarter   ~ "quarterly",
      form %in% FORMS$quarter_a ~ "quarterly",
      TRUE ~ NA_character_),
    is_amendment = form %in% c(FORMS$annual_a, FORMS$quarter_a),
    is_transition = form %in% c("10-KT", "10-QT", "10-KT/A", "10-QT/A"),
    is_smallbiz   = grepl("SB", form, fixed = TRUE),

    ## rdate is the PERIOD OF REPORT, fdate is when it was filed. The whole
    ## revision design keys on the period, not the filing date -- an original
    ## and the filing that re-presents it share a period and nothing else.
    period_end = rdate,
    fyear      = as.integer(format(rdate, "%Y")),
    filing_lag = as.integer(fdate - rdate),

    multi_registrant = !is.na(regcount) & regcount > 1
  ) |>
  select(-regsic_own, -regsic_pri)

## ------------------------------------------------------------- 4. the gates
## Each gate is one column and one reason string. Nothing is removed here.

d <- d |>
  mutate(
    g_no_period = is.na(period_end),
    ## Without a period there is nothing to anchor a comparison on. Should be
    ## rare; counted in the summary so it stops being a guess.
    g_bad_period = !g_no_period &
      (period_end < as.Date("1995-01-01") | period_end > fdate + 30),
    ## A period after the filing date plus a month is a data error, not a
    ## forecast.
    g_drop_sic = !is.na(regsic) & regsic %in% DROP_SIC,
    g_no_sic   = is.na(regsic),
    ## NOTE g_no_sic does NOT drop the row. 1,647 filers have no SIC on the
    ## index, and dropping them would reintroduce exactly the survivorship
    ## problem that using the index instead of Compustat was meant to avoid:
    ## a firm that delisted early is more likely to be missing a header.
    g_no_file  = is.na(wrdsfname) | wrdsfname == "",
    ## No path on WRDS Cloud means step 2 has nothing to fetch.
    g_period_out = !g_no_period &
      (period_end < as.Date(PERIOD_MIN) | period_end > as.Date(PERIOD_MAX))
    ## The filing window and the panel window are different (see Section 1).
    ## Filing-date filtering alone let 2002-filed amendments of 1986-2000
    ## periods in, and pulled nothing to compare them against.
  ) |>
  mutate(
    drop_reason = dplyr::case_when(
      g_no_file     ~ "no file on WRDS",
      g_no_period   ~ "no period of report",
      g_bad_period  ~ "period date implausible",
      g_period_out  ~ "period outside panel",
      g_drop_sic    ~ paste0("SIC ", regsic),
      TRUE          ~ NA_character_),
    in_sample = is.na(drop_reason)
  )

## ------------------------------------------------- 5. one row per filing to fetch
## Initialised empty; step 2 writes these back. Kept on every row, including
## out-of-sample ones, so that widening the sample later does not need a
## schema change.

## ONE FILING CAN BE SEVERAL ROWS, AND STEP 2 MUST NOT RE-FETCH IT.
## The index carries one row per (accession, registrant CIK), so a co-filed
## submission appears once per registrant. 12,963 accessions are affected and
## one appears 67 times.
##
## NOTE WRDS STORES A SEPARATE COPY PER REGISTRANT, so wrdsfname does NOT
## collapse them -- AMERCO and U-Haul International share accession
## 0000004457-04-000004 but sit at 000000/4457/... and 000000/4458/..., two
## paths, identical content. Grouping on wrdsfname therefore saved nothing
## (measured: 0 rows) and the dedupe has to key on ACCESSION.
##
## `collect_primary` marks one row per accession for step 2 to fetch; the
## other registrants' rows take the same chunk.
##
## OPEN PROBLEM, not solved here: a co-filed submission contains the
## statements of BOTH registrants, so one chunk can hold two income
## statements. That is the International Bancshares / United Airlines case
## already recorded in 011i -- picking which set belongs to which CIK is an
## extraction problem, and `n_registrant_rows > 1` is the flag for it.
## THE PRIMARY ROW MUST BE AN IN-SAMPLE ONE. A co-filed submission can have
## one registrant we keep and one we drop -- a REIT parent (6798, dropped)
## filing with an operating subsidiary (6500, kept) is the common shape. If
## the dropped registrant happens to sort first it becomes the primary, and
## the filing is then never collected although an in-sample row needs it.
## Measured: 24 filings behaved that way before this `arrange` was added.
d <- d |>
  arrange(accession, desc(in_sample), cik) |>
  group_by(accession) |>
  mutate(
    n_registrant_rows = dplyr::n(),
    collect_primary   = dplyr::row_number() == 1
  ) |>
  ungroup()

## -------------------------------------------------- 6. who works on what
## ASSIGNED BY CIK, NOT BY FILING. Two reasons:
##
## 1. Step 5 compares a firm's filings against each other, so a firm split
##    across two people is half-done until both finish. Whole firms keep each
##    person's slice self-contained.
## 2. It must be STABLE as the inventory grows. Assigning by row number would
##    reshuffle everyone's queue the moment a form or a year is added, so
##    assignment is a function of the CIK itself and nothing else.
##
## Each person sets SCF_WHO in their own .Renviron (machine-specific, never
## committed), the same way DATA_PATH and SEC_DATA_PATH work:
##     SCF_WHO = "matt"
PEOPLE <- c("matt", "andrew")
BATCH_N <- 250    ## filings per batch -- a sitting's worth of in-harness work

d <- d |>
  mutate(
    assigned_to = PEOPLE[(as.integer(cik) %% length(PEOPLE)) + 1]
  ) |>
  arrange(assigned_to, cik, period_end, fdate) |>
  group_by(assigned_to) |>
  mutate(batch = ((dplyr::row_number() - 1) %/% BATCH_N) + 1) |>
  ungroup()

## ------------------------------------------------- 7. status, from the logs
## NOBODY WRITES THIS FILE. Status is DERIVED from append-only per-person
## logs, so two people working at once cannot collide.
##
## WHY. The inventory lives on OneDrive. Two machines updating one parquet
## produces a silent conflict copy -- `filing_inventory-DESKTOP-ABC.parquet`
## -- and one person's afternoon disappears with no error. A file that nobody
## writes cannot do that. So step 2 and step 3 append a row to
## `inventory/logs/progress_{SCF_WHO}.csv`, each person touching only their
## own file, and this script reads whatever logs it finds and joins them on.
##
## It also means status is rebuildable: delete the inventory, re-run, and the
## logs restore exactly where everyone was.
LOG_DIR <- paste0(OUT_DIR, "/logs")
dir.create(LOG_DIR, showWarnings = FALSE, recursive = TRUE)

logs <- list.files(LOG_DIR, pattern = "^progress_.*\\.csv$", full.names = TRUE)
if (length(logs)) {
  lg <- dplyr::bind_rows(lapply(logs, function(p)
    readr::read_csv(p, show_col_types = FALSE, progress = FALSE)))
  cat("logs read:", length(logs), "file(s),", format(nrow(lg), big.mark = ","),
      "entries from", paste(sort(unique(lg$who)), collapse = ", "), "\n")
  ## Last entry wins per (accession, cik, stage) -- a re-collected filing
  ## supersedes its earlier entry rather than duplicating it.
  lg <- lg |> arrange(when) |>
    group_by(accession, cik, stage) |> slice_tail(n = 1) |> ungroup()
  coll <- lg |> filter(stage == "collected") |>
    select(accession, cik, collect_date = when, locate_how, n_blocks)
  extr <- lg |> filter(stage == "extracted") |>
    select(accession, cik, extract_date = when)
  d <- d |>
    left_join(coll, by = c("accession", "cik")) |>
    left_join(extr, by = c("accession", "cik")) |>
    mutate(collected = !is.na(collect_date), extracted = !is.na(extract_date))
} else {
  cat("no progress logs yet -- everything is uncollected\n")
  d <- d |> mutate(
    collect_date = as.Date(NA), locate_how = NA_character_,
    n_blocks = NA_integer_, extract_date = as.Date(NA),
    collected = FALSE, extracted = FALSE)
}
write_parquet(d, OUT)
cat("\nwritten:", OUT, "\n")

## ------------------------------------------------------------- 8. summary
## Printed every run, because these are the numbers that decide the budget.

cat("\n================ INVENTORY ================\n")
cat(sprintf("filings            %10s\n", format(nrow(d), big.mark = ",")))
cat(sprintf("filers (CIK)       %10s\n", format(n_distinct(d$cik), big.mark = ",")))
cat(sprintf("period range       %s to %s\n",
            min(d$period_end, na.rm = TRUE), max(d$period_end, na.rm = TRUE)))

cat("\n-- by form group --\n")
print(d |> count(form_grp, is_amendment) |> as.data.frame())

cat("\n-- gates (a filing can fail more than one) --\n")
for (g in grep("^g_", names(d), value = TRUE))
  cat(sprintf("  %-14s %8s\n", sub("^g_", "", g),
              format(sum(d[[g]], na.rm = TRUE), big.mark = ",")))

cat("\n-- the funnel --\n")
cat(sprintf("  all filings      %10s  %7s filers\n",
    format(nrow(d), big.mark = ","), format(n_distinct(d$cik), big.mark = ",")))
ins <- d[d$in_sample, ]
cat(sprintf("  in sample        %10s  %7s filers\n",
    format(nrow(ins), big.mark = ","), format(n_distinct(ins$cik), big.mark = ",")))
cat(sprintf("  distinct filings %10s  (co-registrant copies removed)\n",
    format(sum(ins$collect_primary), big.mark = ",")))
cat(sprintf("  still to collect %10s\n",
    format(sum(ins$collect_primary & !ins$collected), big.mark = ",")))

cat("\n-- what the SIC screen removed --\n")
print(d |> filter(g_drop_sic) |> count(regsic, sort = TRUE) |>
      mutate(filers = sapply(regsic, function(s)
        n_distinct(d$cik[d$regsic == s & !is.na(d$regsic)]))) |> as.data.frame())

cat("\n-- in-sample filings by year of period --\n")
print(ins |> count(fyear, form_grp) |>
      tidyr::pivot_wider(names_from = form_grp, values_from = n) |>
      as.data.frame())

## ============================ Validation ============================
## Ad hoc checks, left in place. Not meant to run unattended.

if (FALSE) {

  inv <- read_parquet(OUT)

  ## 1. (accession, cik) must be unique. If it is not, the carry-over in
  ##    Section 6 fans rows out on the left_join and the inventory grows.
  stopifnot(!any(duplicated(inv[c("accession", "cik")])))

  ## 2. Walmart: 10-Ks and 10-Qs should run the whole panel with no holes.
  inv |> filter(cik == "0000104169") |>
    count(fyear, form_grp) |> print(n = 100)

  ## 3. Akorn is CIK 0000003116. Checking 1070494 by mistake on 2026-11-19
  ##    produced a clean 10-K/10-Q series with no amendments at all and a
  ##    false alarm that the amendment channel had been lost. FY2014 is the
  ##    known restatement, and its 10-Q/A filings must be present -- the
  ##    amendment channel is the disclosed channel, and losing it loses half
  ##    the design.
  inv |> filter(cik == "0000003116", fyear %in% 2011:2015) |>
    select(form, fdate, period_end, is_amendment, in_sample) |>
    arrange(fdate) |> print(n = 100)

  ## 4. the small-business forms are present and are 2002-2009 only
  inv |> filter(is_smallbiz) |> count(fyear) |> print(n = 20)

  ## 5. nothing kept that has no file to fetch
  stopifnot(all(inv$wrdsfname[inv$in_sample] != ""))

  ## 6. eyeball what the no-SIC rows look like -- these are KEPT on purpose
  ##    and are the survivorship-sensitive group
  inv |> filter(g_no_sic) |> count(coname, sort = TRUE) |> head(20) |> print()
}
