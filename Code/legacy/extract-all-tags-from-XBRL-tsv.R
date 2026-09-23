# 1. Setup -----------------------------------------------------------------------
library(dplyr)
library(stringr)
library(arrow)
library(glue)
library(tidyr)
library(lubridate)
library(readr)

raw_data_path <- "D:/XBRL/XBRL Extracted Data/"
data_path     <- "C:/Users/mnp4/Dropbox/Cash Flow Audit Idea/Data/"

# XBRL tags of interest: CF (3 primary + 3 ContinuingOperations fallbacks),
# NI with fallback, Equity with fallback
target_tags <- c(
  "NetCashProvidedByUsedInOperatingActivities",
  "NetCashProvidedByUsedInInvestingActivities",
  "NetCashProvidedByUsedInFinancingActivities",
  "NetCashProvidedByUsedInOperatingActivitiesContinuingOperations",
  "NetCashProvidedByUsedInInvestingActivitiesContinuingOperations",
  "NetCashProvidedByUsedInFinancingActivitiesContinuingOperations",
  "NetIncomeLoss",
  "ProfitLoss",
  "NetIncomeLossAvailableToCommonStockholdersBasic",
  "StockholdersEquity",
  "PartnersCapital",
  "Assets",
  "Liabilities")

# Initial tag_type assignment; all fallbacks are resolved in Section 4
tag_type_map <- c(
  "NetCashProvidedByUsedInOperatingActivities"                                = "Operating",
  "NetCashProvidedByUsedInInvestingActivities"                                = "Investing",
  "NetCashProvidedByUsedInFinancingActivities"                                = "Financing",
  "NetCashProvidedByUsedInOperatingActivitiesContinuingOperations"            = "Operating_fallback",
  "NetCashProvidedByUsedInInvestingActivitiesContinuingOperations"            = "Investing_fallback",
  "NetCashProvidedByUsedInFinancingActivitiesContinuingOperations"            = "Financing_fallback",
  "NetIncomeLoss"                                                              = "NetIncomeLoss",
  "ProfitLoss"                                                                 = "ProfitLoss_fallback",
  "NetIncomeLossAvailableToCommonStockholdersBasic"                           = "NI_fallback2",
  "StockholdersEquity"                                                         = "StockholdersEquity",
  "PartnersCapital"                                                            = "SE_fallback",
  "Assets"                                                                     = "Assets",
  "Liabilities"                                                                = "Liabilities")



# 2. Build period file list ------------------------------------------------------
# Quarterly files: 2009 Q1 through 2020 Q3
# Monthly files: 2020 M10 onwards (SEC switched format mid-2020)
search <- bind_rows(
  expand.grid(year   = as.character(2009:2019),
              period = paste0("Q", 1:4),
              stringsAsFactors = FALSE),
  data.frame(year   = "2020",
             period = c("Q1", "Q2", "Q3", "M10", "M11", "M12"),
             stringsAsFactors = FALSE),
  expand.grid(year   = as.character(2021:2023),
              period = sprintf("M%02d", 1:12),
              stringsAsFactors = FALSE))


# 3. Load, filter, and stack all periods -----------------------------------------
# For each period:
#   - Read num (values) and sub (filing metadata) TSV files
#   - Filter num to target tags; apply quality filters from 002 script
#     (value != "", iprx == 0, uom == "USD")
#   - Resolve parent vs. dimensioned values: prefer dimh == "0x00000000",
#     fall back to dimensioned values when parent is absent for that tag/period
#   - Join sub for cik, form, fp, fy, filed, period-end date
#   - Compute start date from ddate + qtrs (analogous to JSON's explicit start)
#   - Flag current vs. comparative periods (current = ddate == sub$period)

stacked_raw <- vector("list", nrow(search))

for (i in seq_len(nrow(search))) {
  year   <- search$year[i]
  period <- search$period[i]

  num_file <- glue("{raw_data_path}{year}_{period}_num.tsv")
  sub_file <- glue("{raw_data_path}{year}_{period}_sub.tsv")

  if (!file.exists(num_file) || !file.exists(sub_file)) next

  if (i %% 10 == 1) cat("Loading", year, period, "(", i, "of", nrow(search), ")\n")

  num_raw <- tryCatch(
    read_delim(num_file, delim = "\t", show_col_types = FALSE,
               col_types = cols(.default = col_character())),
    error = function(e) { cat("  Error reading num:", year, period, "\n"); NULL }
  )
  sub_raw <- tryCatch(
    read_delim(sub_file, delim = "\t", show_col_types = FALSE,
               col_types = cols(.default = col_character())),
    error = function(e) { cat("  Error reading sub:", year, period, "\n"); NULL }
  )
  if (is.null(num_raw) || is.null(sub_raw)) next


  # --- Filter num to target tags + quality filters ---
  num_filtered <- num_raw |>
    filter(tag %in% target_tags,
           value != "",
           iprx  == "0",
           uom   == "USD") |>
    mutate(value = as.numeric(value),
           ddate = as.integer(ddate),
           qtrs  = as.integer(qtrs)) |>
    filter(!is.na(value), !is.na(ddate), !is.na(qtrs))

  # --- Prepare sub metadata ---
  sub_clean <- sub_raw |>
    select(any_of(c("adsh", "cik", "form", "period", "fy", "fp", "filed"))) |>
    filter(form %in% c("10-K", "10-Q", "10-K/A", "10-Q/A")) |>
    mutate(
      cik    = str_pad(as.character(cik), width = 10, pad = "0"),
      filed  = ymd(filed),
      period = as.integer(period),
      fy     = suppressWarnings(as.integer(fy))
    )


  # --- Resolve parent vs. dimensioned values ---
  # Primary: parent-level rows (dimh == "0x00000000")
  # Fallback: dimensioned rows for (adsh, tag, ddate, qtrs) missing from parent
  num_parent <- num_filtered |> filter(dimh == "0x00000000")

  num_dim_fallback <- num_filtered |>
    filter(dimh != "0x00000000") |>
    anti_join(num_parent, by = c("adsh", "tag", "ddate", "qtrs")) |>
    group_by(adsh, tag, ddate, qtrs) |> 
    slice(1) |>
    ungroup()

  num_resolved <- bind_rows(num_parent, num_dim_fallback) |>
    group_by(adsh, tag, ddate, qtrs) |>
    slice(1) |>
    ungroup() |>
    select(adsh, tag, ddate, qtrs, value)

  # --- Join, compute dates, flag current vs. comparative ---
  tag_joined <- num_resolved |>
    inner_join(sub_clean, by = "adsh") |>
    mutate(
      end               = as.Date(as.character(ddate), "%Y%m%d"),
      # start: period begin approximated from duration (qtrs * 3 months)
      # NA for instantaneous (balance sheet) rows where qtrs == 0
      start             = if_else(qtrs > 0L, end %m-% months(qtrs * 3L), as.Date(NA)),
      # current_period: this row represents the reporting period of the filing
      # (vs. a comparative prior-period row included in the same filing)
      is_current_period = (ddate == period),
      tag_type          = tag_type_map[tag]
    ) |>
    rename(val = value, accn = adsh) |>
    select(cik, accn, tag, tag_type, start, end, val, form, fp, fy, filed,
           is_current_period)

  # --- Identify filings in sub_clean not present in tag_joined ---
  missing_filings <- sub_clean |>
    filter(!adsh %in% tag_joined$accn) |>
    mutate(
      tag      = NA_character_,
      tag_type = NA_character_,
      start    = as.Date(NA),
      end      = as.Date(as.character(period), "%Y%m%d"),
      val      = NA_real_,
      is_current_period = NA
    ) |>
    rename(accn = adsh) |>
    select(cik, accn, tag, tag_type, start, end, val, form, fp, fy, filed, is_current_period)

  result <- bind_rows(tag_joined, missing_filings)
  stacked_raw[[i]] <- result
  gc()
}

all_raw <- bind_rows(stacked_raw)
rm(stacked_raw); gc()

cat("Total rows loaded:", nrow(all_raw), "\n")
cat("Distinct CIKs:", n_distinct(all_raw$cik), "\n")
cat("Distinct filings:", n_distinct(all_raw$accn), "\n")

# Cache: save the stacked raw data to avoid re-running the loop
write_parquet(all_raw, glue("{data_path}xbrl_tsv_raw.parquet"))


# 4. Apply NI and equity fallbacks; split by tag type ----------------------------
# CF:  use primary tags; fall back to ContinuingOperations variant when absent
# NI:  use NetIncomeLoss; fall back to ProfitLoss when absent
# EQ:  use StockholdersEquity; fall back to SE including NCI when absent

# Reload from parquet if starting here (skipping the Section 3 loop)
all_raw <- read_parquet(glue("{data_path}xbrl_tsv_raw.parquet")) |>
  mutate(start = as.Date(start), end = as.Date(end), filed = as.Date(filed),
         fy    = as.integer(fy))

# --- CF fallbacks ---
cfo_primary <- all_raw |> filter(tag == "NetCashProvidedByUsedInOperatingActivities")
cfi_primary <- all_raw |> filter(tag == "NetCashProvidedByUsedInInvestingActivities")
cff_primary <- all_raw |> filter(tag == "NetCashProvidedByUsedInFinancingActivities")

cfo_fallback <- all_raw |>
  filter(tag == "NetCashProvidedByUsedInOperatingActivitiesContinuingOperations") |>
  anti_join(cfo_primary, by = c("cik", "accn", "start", "end")) |>
  mutate(tag_type = "Operating")

cfi_fallback <- all_raw |>
  filter(tag == "NetCashProvidedByUsedInInvestingActivitiesContinuingOperations") |>
  anti_join(cfi_primary, by = c("cik", "accn", "start", "end")) |>
  mutate(tag_type = "Investing")

cff_fallback <- all_raw |>
  filter(tag == "NetCashProvidedByUsedInFinancingActivitiesContinuingOperations") |>
  anti_join(cff_primary, by = c("cik", "accn", "start", "end")) |>
  mutate(tag_type = "Financing")

# --- NI fallback ---
ni_primary <- all_raw |> filter(tag == "NetIncomeLoss")

ni_fallback <- all_raw |>
  filter(tag == "ProfitLoss") |>
  anti_join(ni_primary, by = c("cik", "accn", "start", "end")) |>
  mutate(tag_type = "NetIncomeLoss")

ni_fallback2 <- all_raw |>
  filter(tag == "NetIncomeLossAvailableToCommonStockholdersBasic") |>
  anti_join(ni_primary,  by = c("cik", "accn", "start", "end")) |>
  anti_join(ni_fallback, by = c("cik", "accn", "start", "end")) |>
  mutate(tag_type = "NetIncomeLoss")

# --- EQ fallback ---
eq_primary <- all_raw |> filter(tag == "StockholdersEquity")

eq_fallback <- all_raw |>
  filter(tag == "PartnersCapital") |>
  anti_join(eq_primary, by = c("cik", "accn", "start", "end")) |>
  mutate(tag_type = "StockholdersEquity")

raw <- bind_rows(
  cfo_primary |> mutate(tag_type = "Operating"),
  cfi_primary |> mutate(tag_type = "Investing"),
  cff_primary |> mutate(tag_type = "Financing"),
  cfo_fallback, cfi_fallback, cff_fallback,
  ni_primary  |> mutate(tag_type = "NetIncomeLoss"),
  ni_fallback,
  ni_fallback2,
  eq_primary  |> mutate(tag_type = "StockholdersEquity"),
  eq_fallback
)

cf_raw    <- raw |> filter(tag_type %in% c("Operating", "Investing", "Financing"))
ni_raw    <- raw |> filter(tag_type == "NetIncomeLoss")
eq_raw    <- raw |> filter(tag_type == "StockholdersEquity")
assets_raw <- all_raw |> filter(tag == "Assets")
liab_raw   <- all_raw |> filter(tag == "Liabilities")

cat("CF rows:",  nrow(cf_raw), "| NI rows:", nrow(ni_raw), "| EQ rows:", nrow(eq_raw),
    "| Assets rows:", nrow(assets_raw), "| Liabilities rows:", nrow(liab_raw), "\n")

write_parquet(cf_raw,     glue("{data_path}xbrl_tsv_cf_raw.parquet"))
write_parquet(ni_raw,     glue("{data_path}xbrl_tsv_ni_raw.parquet"))
write_parquet(eq_raw,     glue("{data_path}xbrl_tsv_eq_raw.parquet"))
write_parquet(assets_raw, glue("{data_path}xbrl_tsv_assets_raw.parquet"))
write_parquet(liab_raw,   glue("{data_path}xbrl_tsv_liab_raw.parquet"))

# Reload checkpoints if starting from Section 5+:
# cf_raw     <- read_parquet(glue("{data_path}xbrl_tsv_cf_raw.parquet"))
# ni_raw     <- read_parquet(glue("{data_path}xbrl_tsv_ni_raw.parquet"))
# eq_raw     <- read_parquet(glue("{data_path}xbrl_tsv_eq_raw.parquet"))
# assets_raw <- read_parquet(glue("{data_path}xbrl_tsv_assets_raw.parquet"))
# liab_raw   <- read_parquet(glue("{data_path}xbrl_tsv_liab_raw.parquet"))


# 5. Build SCF dataset (Cash Flows) ----------------------------------------------
# Logic mirrors 001b Section 4.
# "Original" = first filing to report (cik, cf_type, start, end) as current period.
# "Comparative" = later filings that include same (cik, cf_type, start, end) as
#   prior-period data (is_current_period == FALSE).
# "Amendment" = 10-K/A or 10-Q/A filing with same (cik, cf_type, start, end).

cf_annual <- cf_raw |>
  filter(form %in% c("10-K", "10-Q"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, cf_type = tag_type, start, end, val, accn, form, fp, fy, filed,
         is_current_period) |>
  filter(!is.na(start), !is.na(end), !is.na(filed), !is.na(val))

# Deduplicate on (cik, cf_type, start, end, accn) — keeps start as part of key
# so YTD and standalone quarter rows for the same period-end are distinct
cf_annual_dedup <- cf_annual |>
  group_by(cik, cf_type, start, end, accn) |>
  slice(1) |>
  ungroup()

# Original: earliest filing that reported this period as *current* period
cf_original <- cf_annual_dedup |>
  filter(is_current_period) |>
  group_by(cik, cf_type, start, end) |>
  arrange(filed, accn, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  transmute(cik, cf_type, start, end,
            original_val   = val,
            original_accn  = accn,
            original_filed = filed,
            original_fy    = fy,
            original_form  = form)

# Comparatives: later filings that include this period as a prior-period comparison
cf_comparatives <- cf_annual_dedup |>
  filter(!is_current_period) |>
  inner_join(cf_original, by = c("cik", "cf_type", "start", "end")) |>
  filter(filed > original_filed) |>
  mutate(val_changed = val != original_val,
         change_amt  = val - original_val) |>
  arrange(cik, cf_type, start, end, filed, accn) |>
  group_by(cik, cf_type, start, end) |>
  mutate(comp_n = row_number()) |>
  ungroup()

cf_comparatives_wide <- cf_comparatives |>
  filter(comp_n <= 2) |>
  select(cik, cf_type, start, end, comp_n, val, accn, filed, val_changed, change_amt) |>
  pivot_wider(id_cols     = c(cik, cf_type, start, end),
              names_from  = comp_n,
              values_from = c(val, accn, filed, val_changed, change_amt),
              names_glue  = "{.value}_c{comp_n}") |>
  rename(val_p1 = val_c1, val_p2 = val_c2,
         accn_p1 = accn_c1, accn_p2 = accn_c2,
         filed_p1 = filed_c1, filed_p2 = filed_c2,
         change_amt_p1 = change_amt_c1, change_amt_p2 = change_amt_c2) |>
  mutate(changed_p1       = if_else(is.na(val_changed_c1), 0L, as.integer(val_changed_c1)),
         changed_p2       = if_else(is.na(val_changed_c2), 0L, as.integer(val_changed_c2)),
         any_10k_revision = if_else(changed_p1 == 1L | changed_p2 == 1L, 1L, 0L)) |>
  select(-val_changed_c1, -val_changed_c2)

# Amendments: 10-K/A or 10-Q/A filings that changed the value
cf_amend_raw <- cf_raw |>
  filter(form %in% c("10-K/A", "10-Q/A"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, cf_type = tag_type, start, end, val, accn, form, fp, fy, filed) |>
  filter(!is.na(start), !is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, cf_type, start, end, accn) |>
  slice(1) |>
  ungroup()

cf_amend_summary <- cf_amend_raw |>
  inner_join(cf_original, by = c("cik", "cf_type", "start", "end")) |>
  filter(filed > original_filed) |>
  mutate(val_changed = val != original_val,
         change_amt  = val - original_val) |>
  arrange(cik, cf_type, start, end, filed, accn) |>
  group_by(cik, cf_type, start, end) |>
  summarise(has_10ka_after_10k     = 1L,
            n_10ka_after_10k       = n(),
            first_10ka_accn        = first(accn),
            first_10ka_filed       = first(filed),
            first_10ka_fy          = first(fy),
            first_10ka_val         = first(val),
            changed_in_first_10ka  = first(as.integer(val_changed)),
            change_amt_first_10ka  = first(change_amt),
            any_change_in_10ka     = if_else(any(val_changed), 1L, 0L),
            max_abs_change_in_10ka = max(abs(change_amt), na.rm = TRUE),
            .groups = "drop")

scf_section <- cf_original |>
  left_join(cf_comparatives_wide, by = c("cik", "cf_type", "start", "end")) |>
  left_join(cf_amend_summary,     by = c("cik", "cf_type", "start", "end")) |>
  mutate(across(c(changed_p1, changed_p2, any_10k_revision, has_10ka_after_10k,
                  n_10ka_after_10k, changed_in_first_10ka, any_change_in_10ka),
                replace_na, 0L),
         cf_key   = recode(cf_type,
                           "Operating"  = "cfo",
                           "Investing"  = "cfi",
                           "Financing"  = "cff"),
         best_val = case_when(any_change_in_10ka == 1L ~ first_10ka_val,
                              any_10k_revision   == 1L ~ coalesce(val_p2, val_p1),
                              TRUE                     ~ original_val),
         correction_channel = case_when(
           any_change_in_10ka == 1L & any_10k_revision == 1L ~ "Both",
           any_change_in_10ka == 1L ~ paste0(original_form, "/A amendment"),
           any_10k_revision   == 1L ~ paste0(original_form, " revision"),
           TRUE                     ~ "No change"))

section_value_order <- c("original_val", "best_val", "val_p1", "val_p2",
                         "changed_p1", "changed_p2", "any_10k_revision",
                         "change_amt_p1", "change_amt_p2", "has_10ka_after_10k",
                         "n_10ka_after_10k", "first_10ka_val", "changed_in_first_10ka",
                         "any_change_in_10ka", "change_amt_first_10ka", "correction_channel")
section_col_order <- unlist(lapply(c("cfo", "cfi", "cff"),
                                   function(p) paste0(p, "_", section_value_order)))

scf_wide <- scf_section |>
  pivot_wider(id_cols     = c(cik, start, end, original_fy, original_accn,
                              original_filed, original_form),
              names_from  = cf_key,
              values_from = c(original_val, best_val, val_p1, val_p2,
                              changed_p1, changed_p2, any_10k_revision,
                              change_amt_p1, change_amt_p2, has_10ka_after_10k,
                              n_10ka_after_10k, first_10ka_val, changed_in_first_10ka,
                              any_change_in_10ka, change_amt_first_10ka, correction_channel),
              names_glue  = "{cf_key}_{.value}")

scf <- scf_wide |>
  mutate(
    any_small_revision    = as.integer(
      coalesce(cfo_any_10k_revision, 0L) == 1L |
      coalesce(cfi_any_10k_revision, 0L) == 1L |
      coalesce(cff_any_10k_revision, 0L) == 1L),
    any_salient_amendment = as.integer(
      coalesce(cfo_any_change_in_10ka, 0L) == 1L |
      coalesce(cfi_any_change_in_10ka, 0L) == 1L |
      coalesce(cff_any_change_in_10ka, 0L) == 1L),
    correction_route = case_when(
      any_small_revision == 1L & any_salient_amendment == 1L ~ "Both",
      any_salient_amendment == 1L ~ paste0("Salient amendment (", original_form, "/A)"),
      any_small_revision    == 1L ~ paste0("Small revision (subsequent ", original_form, ")"),
      TRUE                        ~ "No correction")
  ) |>
  rename(fy          = original_fy,
         filing_accn = original_accn,
         filing_date = original_filed,
         filing_form = original_form) |>
  select(cik, fy, start, end, filing_accn, filing_date, filing_form,
         any_small_revision, any_salient_amendment, correction_route,
         any_of(section_col_order)) |>
  arrange(cik, fy, start, end)


# 6. Build Net Income dataset ----------------------------------------------------
# Identical logic to Section 5 but without cf_type dimension.
#
# Tag selection: for each (cik, start, end), a single canonical tag is chosen
# from current-period filings using the hierarchy:
#   NetIncomeLoss > ProfitLoss > NetIncomeLossAvailableToCommonStockholdersBasic
# All subsequent comparisons (comparatives, amendments) are restricted to that
# same tag, so revisions are only flagged when the same concept changes value.

ni_annual <- ni_raw |>
  filter(form %in% c("10-K", "10-Q"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, start, end, val, tag, accn, form, fp, fy, filed, is_current_period) |>
  filter(!is.na(start), !is.na(end), !is.na(filed), !is.na(val))

# Determine canonical tag per (cik, start, end) from current-period filings
ni_tag_rank <- c(
  "NetIncomeLoss"                                   = 1L,
  "ProfitLoss"                                      = 2L,
  "NetIncomeLossAvailableToCommonStockholdersBasic" = 3L
)

ni_canonical_tag <- ni_annual |>
  filter(is_current_period) |>
  mutate(tag_rank = ni_tag_rank[tag]) |>
  group_by(cik, start, end) |>
  summarise(canonical_tag = tag[which.min(tag_rank)], .groups = "drop")

# Restrict all filings (current and comparative) to the canonical tag
ni_annual <- ni_annual |>
  inner_join(ni_canonical_tag, by = c("cik", "start", "end")) |>
  filter(tag == canonical_tag)

ni_annual_dedup <- ni_annual |>
  group_by(cik, start, end, accn) |>
  slice(1) |>
  ungroup()

ni_original <- ni_annual_dedup |>
  filter(is_current_period) |>
  group_by(cik, start, end) |>
  arrange(filed, accn, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  transmute(cik, start, end,
            ni_original_val   = val,
            ni_original_accn  = accn,
            ni_original_filed = filed,
            ni_original_fy    = fy,
            ni_original_form  = form,
            ni_canonical_tag  = canonical_tag)

ni_comp_wide <- ni_annual_dedup |>
  filter(!is_current_period) |>
  inner_join(ni_original, by = c("cik", "start", "end")) |>
  filter(filed > ni_original_filed) |>
  mutate(val_changed = val != ni_original_val,
         change_amt  = val - ni_original_val) |>
  arrange(cik, start, end, filed, accn) |>
  group_by(cik, start, end) |>
  mutate(comp_n = row_number()) |>
  ungroup() |>
  filter(comp_n <= 2) |>
  select(cik, start, end, comp_n, val, accn, filed, val_changed, change_amt) |>
  pivot_wider(id_cols     = c(cik, start, end),
              names_from  = comp_n,
              values_from = c(val, accn, filed, val_changed, change_amt),
              names_glue  = "{.value}_c{comp_n}") |>
  rename(ni_val_p1 = val_c1, ni_val_p2 = val_c2,
         ni_change_amt_p1 = change_amt_c1, ni_change_amt_p2 = change_amt_c2) |>
  mutate(ni_any_revision = if_else(
    coalesce(as.integer(val_changed_c1), 0L) == 1L |
    coalesce(as.integer(val_changed_c2), 0L) == 1L, 1L, 0L)) |>
  select(-val_changed_c1, -val_changed_c2, -starts_with("accn_"), -starts_with("filed_"))

ni_amend_summary <- ni_raw |>
  filter(form %in% c("10-K/A", "10-Q/A"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, start, end, val, tag, accn, form, fp, fy, filed) |>
  filter(!is.na(start), !is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, start, end, accn) |>
  slice(1) |>
  ungroup() |>
  inner_join(ni_original, by = c("cik", "start", "end")) |>
  filter(filed > ni_original_filed, tag == ni_canonical_tag) |>
  mutate(val_changed = val != ni_original_val,
         change_amt  = val - ni_original_val) |>
  arrange(cik, start, end, filed, accn) |>
  group_by(cik, start, end) |>
  summarise(ni_has_amendment          = 1L,
            ni_first_amend_val        = first(val),
            ni_change_amt_first_amend = first(change_amt),
            ni_any_change_in_amend    = if_else(any(val_changed), 1L, 0L),
            .groups = "drop")

ni_wide <- ni_original |>
  left_join(ni_comp_wide,     by = c("cik", "start", "end")) |>
  left_join(ni_amend_summary, by = c("cik", "start", "end")) |>
  mutate(ni_any_revision        = replace_na(ni_any_revision,        0L),
         ni_has_amendment       = replace_na(ni_has_amendment,       0L),
         ni_any_change_in_amend = replace_na(ni_any_change_in_amend, 0L),
         ni_best_val            = case_when(
           ni_any_change_in_amend == 1L ~ ni_first_amend_val,
           ni_any_revision        == 1L ~ coalesce(ni_val_p2, ni_val_p1),
           TRUE                         ~ ni_original_val))


# 7. Build Stockholders Equity dataset -------------------------------------------
# Point-in-time (balance sheet) — no start date, no YTD complication.
# Uses is_current_period (ddate == period) rather than current_end trick.

eq_clean <- eq_raw |>
  filter(form %in% c("10-K", "10-Q"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, end, val, accn, form, fp, fy, filed, is_current_period) |>
  filter(!is.na(end), !is.na(filed), !is.na(val)) |>
  # For equity, keep only current-period rows (not comparative prior-year balance sheets)
  filter(is_current_period) |>
  group_by(cik, end, accn) |>
  slice(1) |>
  ungroup()

eq_original <- eq_clean |>
  group_by(cik, end) |>
  arrange(filed, accn, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  transmute(cik, end,
            eq_original_val   = val,
            eq_original_accn  = accn,
            eq_original_filed = filed,
            eq_original_fy    = fy,
            eq_original_form  = form)

eq_revisions <- eq_clean |>
  inner_join(eq_original, by = c("cik", "end")) |>
  filter(filed > eq_original_filed, val != eq_original_val) |>
  arrange(cik, end, filed, accn) |>
  group_by(cik, end) |>
  slice(1) |>
  ungroup() |>
  transmute(cik, end,
            eq_revised_val  = val,
            eq_change_amt   = val - eq_original_val,
            eq_any_revision = 1L)

eq_amend_summary <- eq_raw |>
  filter(form %in% c("10-K/A", "10-Q/A"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, end, val, accn, form, fp, fy, filed) |>
  filter(!is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, end, accn) |>
  slice(1) |>
  ungroup() |>
  inner_join(eq_original, by = c("cik", "end")) |>
  filter(filed > eq_original_filed) |>
  mutate(val_changed = val != eq_original_val,
         change_amt  = val - eq_original_val) |>
  arrange(cik, end, filed, accn) |>
  group_by(cik, end) |>
  summarise(eq_has_amendment          = 1L,
            eq_first_amend_val        = first(val),
            eq_change_amt_first_amend = first(change_amt),
            eq_any_change_in_amend    = if_else(any(val_changed), 1L, 0L),
            .groups = "drop")

eq_wide <- eq_original |>
  left_join(eq_revisions,     by = c("cik", "end")) |>
  left_join(eq_amend_summary, by = c("cik", "end")) |>
  mutate(eq_any_revision        = replace_na(eq_any_revision,        0L),
         eq_has_amendment       = replace_na(eq_has_amendment,       0L),
         eq_any_change_in_amend = replace_na(eq_any_change_in_amend, 0L),
         eq_best_val            = case_when(
           eq_any_change_in_amend == 1L ~ eq_first_amend_val,
           eq_any_revision        == 1L ~ eq_revised_val,
           TRUE                         ~ eq_original_val))


# 8. Build Assets dataset --------------------------------------------------------
# Point-in-time (balance sheet) — mirrors Section 7 (equity).

assets_clean <- assets_raw |>
  filter(form %in% c("10-K", "10-Q"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, end, val, accn, form, fp, fy, filed, is_current_period) |>
  filter(!is.na(end), !is.na(filed), !is.na(val)) |>
  filter(is_current_period) |>
  group_by(cik, end, accn) |>
  slice(1) |>
  ungroup()

assets_original <- assets_clean |>
  group_by(cik, end) |>
  arrange(filed, accn, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  transmute(cik, end,
            assets_original_val   = val,
            assets_original_accn  = accn,
            assets_original_filed = filed,
            assets_original_fy    = fy,
            assets_original_form  = form)

assets_revisions <- assets_clean |>
  inner_join(assets_original, by = c("cik", "end")) |>
  filter(filed > assets_original_filed, val != assets_original_val) |>
  arrange(cik, end, filed, accn) |>
  group_by(cik, end) |>
  slice(1) |>
  ungroup() |>
  transmute(cik, end,
            assets_revised_val  = val,
            assets_change_amt   = val - assets_original_val,
            assets_any_revision = 1L)

assets_amend_summary <- assets_raw |>
  filter(form %in% c("10-K/A", "10-Q/A"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, end, val, accn, form, fp, fy, filed) |>
  filter(!is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, end, accn) |>
  slice(1) |>
  ungroup() |>
  inner_join(assets_original, by = c("cik", "end")) |>
  filter(filed > assets_original_filed) |>
  mutate(val_changed = val != assets_original_val,
         change_amt  = val - assets_original_val) |>
  arrange(cik, end, filed, accn) |>
  group_by(cik, end) |>
  summarise(assets_has_amendment          = 1L,
            assets_first_amend_val        = first(val),
            assets_change_amt_first_amend = first(change_amt),
            assets_any_change_in_amend    = if_else(any(val_changed), 1L, 0L),
            .groups = "drop")

assets_wide <- assets_original |>
  left_join(assets_revisions,     by = c("cik", "end")) |>
  left_join(assets_amend_summary, by = c("cik", "end")) |>
  mutate(assets_any_revision        = replace_na(assets_any_revision,        0L),
         assets_has_amendment       = replace_na(assets_has_amendment,       0L),
         assets_any_change_in_amend = replace_na(assets_any_change_in_amend, 0L),
         assets_best_val            = case_when(
           assets_any_change_in_amend == 1L ~ assets_first_amend_val,
           assets_any_revision        == 1L ~ assets_revised_val,
           TRUE                             ~ assets_original_val))


# 9. Build Liabilities dataset ---------------------------------------------------
# Point-in-time (balance sheet) — mirrors Section 7 (equity).

liab_clean <- liab_raw |>
  filter(form %in% c("10-K", "10-Q"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, end, val, accn, form, fp, fy, filed, is_current_period) |>
  filter(!is.na(end), !is.na(filed), !is.na(val)) |>
  filter(is_current_period) |>
  group_by(cik, end, accn) |>
  slice(1) |>
  ungroup()

liab_original <- liab_clean |>
  group_by(cik, end) |>
  arrange(filed, accn, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  transmute(cik, end,
            liab_original_val   = val,
            liab_original_accn  = accn,
            liab_original_filed = filed,
            liab_original_fy    = fy,
            liab_original_form  = form)

liab_revisions <- liab_clean |>
  inner_join(liab_original, by = c("cik", "end")) |>
  filter(filed > liab_original_filed, val != liab_original_val) |>
  arrange(cik, end, filed, accn) |>
  group_by(cik, end) |>
  slice(1) |>
  ungroup() |>
  transmute(cik, end,
            liab_revised_val  = val,
            liab_change_amt   = val - liab_original_val,
            liab_any_revision = 1L)

liab_amend_summary <- liab_raw |>
  filter(form %in% c("10-K/A", "10-Q/A"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, end, val, accn, form, fp, fy, filed) |>
  filter(!is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, end, accn) |>
  slice(1) |>
  ungroup() |>
  inner_join(liab_original, by = c("cik", "end")) |>
  filter(filed > liab_original_filed) |>
  mutate(val_changed = val != liab_original_val,
         change_amt  = val - liab_original_val) |>
  arrange(cik, end, filed, accn) |>
  group_by(cik, end) |>
  summarise(liab_has_amendment          = 1L,
            liab_first_amend_val        = first(val),
            liab_change_amt_first_amend = first(change_amt),
            liab_any_change_in_amend    = if_else(any(val_changed), 1L, 0L),
            .groups = "drop")

liab_wide <- liab_original |>
  left_join(liab_revisions,     by = c("cik", "end")) |>
  left_join(liab_amend_summary, by = c("cik", "end")) |>
  mutate(liab_any_revision        = replace_na(liab_any_revision,        0L),
         liab_has_amendment       = replace_na(liab_has_amendment,       0L),
         liab_any_change_in_amend = replace_na(liab_any_change_in_amend, 0L),
         liab_best_val            = case_when(
           liab_any_change_in_amend == 1L ~ liab_first_amend_val,
           liab_any_revision        == 1L ~ liab_revised_val,
           TRUE                           ~ liab_original_val))


# 10. Join into one dataset and save ---------------------------------------------
# Same structure as 001b Section 7. One row per (cik, period-end).
# For a given period-end, keep the YTD row (earliest start) before joining equity.

xbrl_all_tsv <- scf |>
  full_join(ni_wide, by = c("cik", "start", "end")) |>
  group_by(cik, end) |>
  filter(is.na(start) | start == min(start, na.rm = TRUE)) |>
  slice(1) |>
  ungroup() |>
  full_join(eq_wide,     by = c("cik", "end")) |>
  full_join(assets_wide, by = c("cik", "end")) |>
  full_join(liab_wide,   by = c("cik", "end")) |>
  mutate(
    fy          = coalesce(fy,          ni_original_fy,     eq_original_fy,
                                        assets_original_fy, liab_original_fy),
    filing_accn = coalesce(filing_accn, ni_original_accn,   eq_original_accn,
                                        assets_original_accn, liab_original_accn),
    filing_date = coalesce(filing_date, ni_original_filed,  eq_original_filed,
                                        assets_original_filed, liab_original_filed),
    filing_form = coalesce(filing_form, ni_original_form,   eq_original_form,
                                        assets_original_form, liab_original_form),

    cfo_change    = cfo_best_val    - cfo_original_val,
    cfi_change    = cfi_best_val    - cfi_original_val,
    cff_change    = cff_best_val    - cff_original_val,
    ni_change     = ni_best_val     - ni_original_val,
    eq_change     = eq_best_val     - eq_original_val,
    assets_change = assets_best_val - assets_original_val,
    liab_change   = liab_best_val   - liab_original_val,

    cfo_correction    = case_when(coalesce(cfo_any_change_in_10ka,    0L) == 1L ~ "Big R",
                                  coalesce(cfo_any_10k_revision,      0L) == 1L ~ "Revision",
                                  TRUE                                           ~ "None"),
    cfi_correction    = case_when(coalesce(cfi_any_change_in_10ka,    0L) == 1L ~ "Big R",
                                  coalesce(cfi_any_10k_revision,      0L) == 1L ~ "Revision",
                                  TRUE                                           ~ "None"),
    cff_correction    = case_when(coalesce(cff_any_change_in_10ka,    0L) == 1L ~ "Big R",
                                  coalesce(cff_any_10k_revision,      0L) == 1L ~ "Revision",
                                  TRUE                                           ~ "None"),
    ni_correction     = case_when(coalesce(ni_any_change_in_amend,    0L) == 1L ~ "Big R",
                                  coalesce(ni_any_revision,           0L) == 1L ~ "Revision",
                                  TRUE                                           ~ "None"),
    eq_correction     = case_when(coalesce(eq_any_change_in_amend,    0L) == 1L ~ "Big R",
                                  coalesce(eq_any_revision,           0L) == 1L ~ "Revision",
                                  TRUE                                           ~ "None"),
    assets_correction = case_when(coalesce(assets_any_change_in_amend, 0L) == 1L ~ "Big R",
                                  coalesce(assets_any_revision,        0L) == 1L ~ "Revision",
                                  TRUE                                            ~ "None"),
    liab_correction   = case_when(coalesce(liab_any_change_in_amend,   0L) == 1L ~ "Big R",
                                  coalesce(liab_any_revision,          0L) == 1L ~ "Revision",
                                  TRUE                                            ~ "None"),

    # Overall indicator: 1 if any tracked account has any correction (revision or Big R)
    any_correction    = as.integer(
      cfo_correction != "None" | cfi_correction != "None" | cff_correction != "None" |
      ni_correction  != "None" | eq_correction  != "None" |
      assets_correction != "None" | liab_correction != "None"),
    any_big_r         = as.integer(
      cfo_correction == "Big R" | cfi_correction == "Big R" | cff_correction == "Big R" |
      ni_correction  == "Big R" | eq_correction  == "Big R" |
      assets_correction == "Big R" | liab_correction == "Big R")
  ) |>
  select(
    cik, filing_accn, filing_date, filing_form, fy, end,
    any_correction, any_big_r,
    cfo_original_val,    cfo_best_val,    cfo_change,    cfo_correction,
    cfi_original_val,    cfi_best_val,    cfi_change,    cfi_correction,
    cff_original_val,    cff_best_val,    cff_change,    cff_correction,
    ni_original_val,     ni_best_val,     ni_change,     ni_correction,    ni_canonical_tag,
    eq_original_val,     eq_best_val,     eq_change,     eq_correction,
    assets_original_val, assets_best_val, assets_change, assets_correction,
    liab_original_val,   liab_best_val,   liab_change,   liab_correction
  ) |>
  arrange(cik, end)

write_parquet(xbrl_all_tsv, glue("{data_path}xbrl_all_tsv.parquet"))
cat("Saved xbrl_all_tsv.parquet:", nrow(xbrl_all_tsv), "rows,",
    ncol(xbrl_all_tsv), "columns\n")



#Validation---------------------------------------------------------------------

xbrl_all_tsv <- read_parquet(glue("{data_path}xbrl_all_tsv.parquet"))


any_correct <- xbrl_all_tsv |> filter(any_correction==1)


honey <- xbrl_all_tsv |> filter(cik=="0000773840")

aceto <- xbrl_all_tsv |> filter(cik=="0000002034")

akorn <- xbrl_all_tsv |> filter(cik=="0000003116")



num <- read_delim(glue("{raw_data_path}2019_Q2_num.tsv"), delim = "\t", show_col_types = FALSE,
                  col_types = cols(.default = col_character()))


test <- num |> filter(adsh == "0000104169-19-000024")



num <- read_delim(glue("{raw_data_path}2014_Q1_num.tsv"), delim = "\t", show_col_types = FALSE,
                  col_types = cols(.default = col_character()))


test <- num |> filter(adsh == "0000104169-14-000019")



ni_miss <- xbrl_all_tsv |> filter(!is.na(cfo_original_val),is.na(ni_original_val))
eq_miss <- xbrl_all_tsv |> filter(!is.na(cfo_original_val),is.na(eq_original_val))
liab_miss <- xbrl_all_tsv |> filter(!is.na(cfo_original_val),is.na(liab_original_val))

"MembersEquity"