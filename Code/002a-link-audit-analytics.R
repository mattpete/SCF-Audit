# =============================================================================
# 002a - Link the SCF filing panel to Audit Analytics
# =============================================================================
# PURPOSE
#   The XBRL filing panel is the MAIN table; Audit Analytics is left-joined onto
#   it. That orientation is the whole point: AA only records a restatement or
#   adjustment once a filer DISCLOSES it, so a period where our pipeline finds an
#   unexplained SCF revision and AA has nothing is a SILENT revision -- invisible
#   to any study that samples from AA.
#
#   An earlier version of this script ran the other way (AA-anchored, XBRL joined
#   on) which can only ever measure how much of AA we can match. It cannot see
#   the silent population at all, because those rows do not exist in AA. The
#   AA-anchored view is retained in Section 8 purely as a coverage diagnostic.
#
# WHAT "DISCLOSED" CAN MEAN
#   Two independent channels, and they are kept separate because a filer can use
#   one without the other:
#     - an AMENDMENT (10-K/A, 10-Q/A) that actually changed the cash flow
#       statement -- detected by 001a from the filings themselves
#     - an AA RECORD whose restated/adjustment window covers the period
#   AA records are further split by whether AA flagged a cash-flow-statement
#   failure specifically. AA can log a restatement for, say, revenue recognition
#   that happens to move the SCF too; the event is disclosed, but the cash-flow
#   nature of it was not identified. `aa_flagged_cash_flow` separates those.
#
# JOIN KEY
#   cik (10-digit, zero padded) AND the filing's period end falling inside the
#   AA restated/adjustment window [begin, end]. AA windows are firm-level date
#   ranges spanning multiple reporting periods, so one AA record legitimately
#   matches several of our filing-periods, and a period can match several AA
#   records. Matches are therefore aggregated to one row per filing-period.
#
# COVERAGE NOTE
#   AA disclosure dates run to 2025-03 (restatements) and 2026-02 (adjustments),
#   while the XBRL panel stops at 2023. So AA is NOT right-censored relative to
#   our periods: a 2023 revision had time to be disclosed and recorded. The
#   reverse censoring does apply at the panel's own right edge -- see the `era`
#   flag, where periods from FY2022 lack later filings to serve as comparatives.
#
# INPUT   {data_path}scf_filing_summary{IN_SUFFIX}.parquet      (001c)
#         {data_path}restatement-data-031425.xlsx               (Audit Analytics)
#         {data_path}adjustment-data-021626.xlsx                (Audit Analytics)
# OUTPUT  {data_path}scf_filing_with_aa{IN_SUFFIX}.parquet
#         {data_path}aa_cleaned.parquet
# =============================================================================


# 1. Setup ---------------------------------------------------------------------
library(dplyr)
library(stringr)
library(arrow)
library(glue)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)

data_path <- Sys.getenv('DATA_PATH')
source("code/000-utilities-functions.R")


# 2. Configuration -------------------------------------------------------------
IN_SUFFIX <- "_full"

RESTATEMENT_FILE <- "restatement-data-031425.xlsx"
ADJUSTMENT_FILE  <- "adjustment-data-021626.xlsx"

# AA's own label for the phenomenon this project is about. The failures field is
# a pipe-delimited list; the cash flow entry reads
#   "Cash flow statement (SFAS 95) classification errors"
CF_FAILURE_PATTERN <- "cash flow statement"

# Headline figures are reported on this subset: sections whose arithmetic we can
# verify, and periods outside the XBRL phase-in era and the censored right edge.
# The saved file keeps every row and flag so other cuts remain possible.


# 3. Clean Audit Analytics restatements ----------------------------------------
# Field derivations follow the previously validated version of this script; see
# git history for the AA-anchored original.
misstate <- read_excel(glue("{data_path}{RESTATEMENT_FILE}"))

misstate2 <- misstate |>
  rename(restatement_key = `Restatement Key`,
         cik = `CIK Code`, ticker = Ticker, com_name = Company,
         filing_time = `Disclosure Accepted`,
         misstate_begin = `Restated Period Begin`,
         misstate_end = `Restated Period Ended`,
         acct_failures = `Accounting Rule (GAAP/FASB) Application Failures`,
         date_8k_402 = `Date of 8-K Item 4.02`) |>
  mutate(cusip = sub('^\\s*="(.*)"$', '\\1', CUSIP),
         cik = str_pad(cik, width = 10, pad = "0"),
         big4 = coalesce(if_else(str_detect(`Auditor - During Restated Period`,
                  "Ernst|KPMG|Pricewaterhouse|Deloitte"), 1, 0), 0L),
         salient_ann = if_else(str_detect(Disclosure, "Press Release"), 1, 0),
         neg_effect = if_else(Effect == "Negative", 1, 0),
         chg_ni = coalesce(`Cumulative Change in Net Income`, 0L),
         materiality = coalesce(chg_ni / coalesce(`H - Assets ($)`, `MR - Assets ($)`), 0L),
         material = if_else(abs(materiality) >= 0.05, 1, 0),
         rev_rec = coalesce(if_else(str_detect(acct_failures, "Revenue recognition"), 1, 0), 0L),
         cfr = coalesce(if_else(str_detect(str_to_lower(acct_failures),
                                           CF_FAILURE_PATTERN), 1, 0), 0L),
         icmw = coalesce(if_else(str_detect(`Other Significant Issues`,
                                            "Material Weakness"), 1, 0), 0L),
         SEC = coalesce(if_else(`SEC Investigation` == "Y", 1, 0), 0L),
         aud_involve = coalesce(if_else(`Auditor Letter - Discussion` == "Y", 1, 0), 0L),
         board_involve = coalesce(if_else(`Board Involvement` == "Y", 1, 0), 0L),
         fraud = if_else(!is.na(`Financial Fraud, Irregularities and Misrepresentations`), 1, 0),
         restate = if_else(!is.na(date_8k_402), 1, 0),
         file_date = date(filing_time),
         misstate_ann_year = year(file_date),
         num_failures = sapply(str_split(replace_na(acct_failures, ""), "\\|"),
                               function(x) sum(x != ""))) |>
  filter(year(file_date) >= 2007, year(file_date) < 2027) |>
  select(restatement_key, cik, ticker, cusip, com_name, file_date, filing_time,
         misstate_begin, misstate_end, big4, salient_ann, neg_effect, chg_ni, cfr,
         num_failures, materiality, material, rev_rec, icmw, SEC, aud_involve,
         board_involve, fraud, date_8k_402, restate, misstate_ann_year, acct_failures)

# duplicates on (cik, file_date): keep the formal restatement, then the most material
misstate3 <- misstate2 |>
  group_by(cik, file_date) |>
  arrange(desc(restate), desc(materiality), .by_group = TRUE) |>
  slice(1) |> ungroup()


# 4. Clean Audit Analytics adjustments -----------------------------------------
adjust <- read_excel(glue("{data_path}{ADJUSTMENT_FILE}"))

adjust2 <- adjust |>
  rename(cik = `CIK Code`, ticker = Ticker, com_name = Company,
         filing_time = `Disclosure Accepted`,
         misstate_begin = `Adjustment Period Begin`,
         misstate_end = `Adjustment Period Ended`,
         acct_failures = `Accounting Rule (GAAP/FASB) Application Failures`) |>
  mutate(cusip = sub('^\\s*="(.*)"$', '\\1', CUSIP),
         cik = str_pad(cik, width = 10, pad = "0"),
         big4 = coalesce(if_else(str_detect(`Auditor - During Restated Period`,
                  "Ernst|KPMG|Pricewaterhouse|Deloitte"), 1, 0), 0L),
         salient_ann = if_else(str_detect(Disclosure, "Press Release"), 1, 0),
         neg_effect = if_else(Effect == "Negative", 1, 0),
         chg_ni = coalesce(`Cumulative Change in Net Income`, 0L),
         materiality = coalesce(chg_ni / coalesce(`H - Assets ($)`, `MR - Assets ($)`), 0L),
         material = if_else(abs(materiality) >= 0.05, 1, 0),
         rev_rec = coalesce(if_else(str_detect(acct_failures, "Revenue recognition"), 1, 0), 0L),
         cfr = coalesce(if_else(str_detect(str_to_lower(acct_failures),
                                           CF_FAILURE_PATTERN), 1, 0), 0L),
         icmw = coalesce(if_else(str_detect(`Other Significant Issues`,
                                            "Material Weakness"), 1, 0), 0L),
         SEC = coalesce(if_else(`SEC Investigation` == "Y", 1, 0), 0L),
         aud_involve = coalesce(if_else(`Auditor Letter - Discussion` == "Y", 1, 0), 0L),
         board_involve = coalesce(if_else(`Board Involvement` == "Y", 1, 0), 0L),
         fraud = if_else(!is.na(`Financial Fraud, Irregularities and Misrepresentations`), 1, 0),
         file_date = date(filing_time),
         misstate_ann_year = year(file_date),
         num_failures = sapply(str_split(replace_na(acct_failures, ""), "\\|"),
                               function(x) sum(x != ""))) |>
  filter(year(file_date) >= 2007, year(file_date) < 2027) |>
  mutate(restatement_key = NA_real_, date_8k_402 = as.POSIXct(NA), restate = 0) |>
  select(restatement_key, cik, ticker, cusip, com_name, file_date, filing_time,
         misstate_begin, misstate_end, big4, salient_ann, neg_effect, chg_ni, cfr,
         num_failures, materiality, material, rev_rec, icmw, SEC, aud_involve,
         board_involve, fraud, date_8k_402, restate, misstate_ann_year, acct_failures)

adjust3 <- adjust2 |>
  group_by(cik, file_date) |>
  arrange(desc(materiality), .by_group = TRUE) |>
  slice(1) |> ungroup()


# 5. Combine into one AA event table -------------------------------------------
# On a (cik, file_date) collision a restatement outranks an adjustment.
aa <- bind_rows(misstate3 |> mutate(aa_source = "restatement"),
                adjust3   |> mutate(aa_source = "adjustment")) |>
  mutate(adjust_flag = if_else(restate == 0, 1, 0)) |>
  arrange(cik, file_date, adjust_flag) |>
  group_by(cik, file_date) |> slice(1) |> ungroup() |>
  select(-adjust_flag) |>
  mutate(misstate_begin = as.Date(misstate_begin),
         misstate_end   = as.Date(misstate_end)) |>
  filter(!is.na(misstate_begin), !is.na(misstate_end))

write_parquet(aa, glue("{data_path}aa_cleaned.parquet"))

cat("AA events after cleaning:", format(nrow(aa), big.mark = ","),
    "| restatements:", sum(aa$aa_source == "restatement"),
    "| adjustments:", sum(aa$aa_source == "adjustment"), "\n")
cat("  flagged as a cash-flow-statement failure:", sum(aa$cfr), "\n")
cat("  formal restatements (8-K Item 4.02):", sum(aa$restate), "\n")


# 6. Left-join AA onto the filing panel ----------------------------------------
fs <- read_parquet(glue("{data_path}scf_filing_summary{IN_SUFFIX}.parquet"))
cat("\nfiling-periods (main table):", format(nrow(fs), big.mark = ","), "\n")

# A period matches an AA event when the company matches and the period end falls
# inside the restated window. One event spans several periods and one period can
# hit several events, so this is intentionally many-to-many and is collapsed
# immediately afterwards.
aa_matches <- fs |>
  select(cik, period_start, period_end, orig_filed) |>
  inner_join(aa, by = join_by(cik, between(period_end, misstate_begin, misstate_end))) |>
  group_by(cik, period_start, period_end) |>
  summarise(
    aa_match            = TRUE,
    aa_n_events         = n(),
    aa_flagged_cash_flow = any(cfr == 1),
    aa_any_formal_restatement = any(restate == 1),
    aa_any_material     = any(material == 1),
    aa_any_fraud        = any(fraud == 1),
    aa_any_icmw         = any(icmw == 1),
    aa_any_sec_inquiry  = any(SEC == 1),
    aa_first_disclosed  = min(file_date),
    aa_last_disclosed   = max(file_date),
    aa_max_abs_chg_ni   = max(abs(as.numeric(chg_ni)), na.rm = TRUE),
    aa_keys             = paste(sort(unique(na.omit(restatement_key))), collapse = "; "),
    aa_sources          = paste(sort(unique(aa_source)), collapse = "+"),
    .groups = "drop") |>
  mutate(aa_keys = if_else(aa_keys == "", NA_character_, aa_keys))

cat("filing-periods matched to at least one AA event:",
    format(nrow(aa_matches), big.mark = ","), "\n")


# 7. Classify the disclosure channel -------------------------------------------
scf_filing_with_aa <- fs |>
  left_join(aa_matches, by = c("cik", "period_start", "period_end")) |>
  mutate(
    aa_match             = coalesce(aa_match, FALSE),
    aa_flagged_cash_flow = coalesce(aa_flagged_cash_flow, FALSE),
    across(c(aa_n_events, aa_any_formal_restatement, aa_any_material,
             aa_any_fraud, aa_any_icmw, aa_any_sec_inquiry),
           ~ coalesce(.x, 0)),
    # how long after the original filing did any disclosure arrive
    aa_days_after_filing = as.numeric(aa_first_disclosed - orig_filed),

    # The population of interest: a section actually moved, and the movement is
    # NOT attributable to an accounting standard.
    revision = any_change & any_unexplained,

    # Was that revision disclosed through either channel?
    disclosure_status = case_when(
      !revision                                   ~ "No unexplained revision",
      aa_match & has_amendment                    ~ "Disclosed: AA + amendment",
      aa_match & !has_amendment                    ~ "Disclosed: AA only",
      !aa_match & has_amendment                    ~ "Disclosed: amendment only",
      TRUE                                         ~ "SILENT: no AA, no amendment"),

    silent_revision = revision & !aa_match & !has_amendment,

    # Narrower still: AA logged the event but never characterised it as a cash
    # flow problem, so the SCF nature of it is absent from AA even though the
    # restatement itself is on record.
    aa_missed_cash_flow_nature = revision & aa_match & !aa_flagged_cash_flow,

    # the analysis subset: verifiable arithmetic, outside the dirty eras
    in_clean_sample = all_testable & era == "clean")

write_parquet_safely <- function(x, path, tries = 6, wait = 5) {
  for (attempt in seq_len(tries)) {
    ok <- tryCatch({ write_parquet(x, path); TRUE },
                   error = function(e) { message("  write failed (", attempt, "/",
                                        tries, "): ", conditionMessage(e)); FALSE })
    if (ok) return(invisible(TRUE))
    Sys.sleep(wait)
  }
  stop("Could not write ", path, " after ", tries, " attempts.")
}
write_parquet_safely(scf_filing_with_aa,
  glue("{data_path}scf_filing_with_aa{IN_SUFFIX}.parquet"))
cat("Saved scf_filing_with_aa:", format(nrow(scf_filing_with_aa), big.mark=","),
    "rows x", ncol(scf_filing_with_aa), "cols\n")


# 8. Results -------------------------------------------------------------------
clean <- scf_filing_with_aa |> filter(in_clean_sample)

cat("\n=== disclosure status, ALL filing-periods ===\n")
print(scf_filing_with_aa |> count(disclosure_status, sort = TRUE) |>
  mutate(pct = round(100*n/sum(n), 1)))

cat("\n=== disclosure status, CLEAN SAMPLE (testable, clean era) ===\n")
print(clean |> count(disclosure_status, sort = TRUE) |>
  mutate(pct = round(100*n/sum(n), 1)))

cat("\n=== among unexplained revisions in the clean sample ===\n")
rev <- clean |> filter(revision)
print(rev |> summarise(
  revisions              = n(),
  silent                 = sum(silent_revision),
  pct_silent             = round(100*mean(silent_revision), 1),
  in_aa                  = sum(aa_match),
  pct_in_aa              = round(100*mean(aa_match), 1),
  aa_flagged_cash_flow   = sum(aa_flagged_cash_flow),
  pct_aa_flagged_cf      = round(100*mean(aa_flagged_cash_flow), 1),
  amendment_only         = sum(!aa_match & has_amendment)))

cat("\n=== does AA catch the CROSS-SECTION reclassifications specifically? ===\n")
print(clean |> filter(revision) |>
  group_by(period_type_adjusted) |>
  summarise(revisions = n(), in_aa = sum(aa_match),
            pct_in_aa = round(100*mean(aa_match), 1),
            aa_called_it_cash_flow = sum(aa_flagged_cash_flow), .groups = "drop"))

cat("\n=== silent revision rate by filer status ===\n")
print(clean |> filter(revision) |> group_by(afs) |>
  summarise(revisions = n(), silent = sum(silent_revision),
            pct_silent = round(100*mean(silent_revision), 1), .groups = "drop") |>
  arrange(desc(revisions)))

cat("\n=== AA-ANCHORED COVERAGE DIAGNOSTIC (the reverse view) ===\n")
cat("Of AA events whose window overlaps our panel, how many match a filing-period?\n")
aa_era <- aa |> filter(misstate_end >= as.Date("2009-01-01"),
                       misstate_end <= as.Date("2023-12-31"))
# aa_era is the LEFT table here, so the period column lives on the right (y$)
aa_hit <- aa_era |>
  inner_join(fs |> select(cik, period_end),
             by = join_by(cik, between(y$period_end, x$misstate_begin, x$misstate_end))) |>
  distinct(cik, file_date)
print(tibble(aa_events_in_era = nrow(aa_era),
             matched_to_a_filing_period = nrow(aa_hit),
             pct = round(100*nrow(aa_hit)/nrow(aa_era), 1)))
cat("(A low rate here is expected: AA covers filers and periods the XBRL panel\n",
    " does not, e.g. pre-XBRL periods and non-10-K/Q filers.)\n")
