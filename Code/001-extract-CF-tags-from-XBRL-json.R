# 1. Setup-----------------------------------------------------------------------
library(jsonlite)
library(dplyr)
library(stringr)
library(arrow)
library(glue)
library(tidyr)


zip_path <- "D:/XBRL/companyfacts.zip"
output_path <- "D:/XBRL/XBRL Cash Flow Tags by CIK/"
data_path <- "C:/Users/mnp4/Dropbox/Cash Flow Audit Idea/Data/"
dir.create(output_path, showWarnings = FALSE)

# 2. Extract cash flow tags from XBRL data ---------------------------------------
# 2.1 Get all CIKs from zip file
zip_contents <- unzip(zip_path, list = TRUE)
json_files <- zip_contents$Name[str_detect(zip_contents$Name, "\\.json$")]
all_ciks <- str_extract(json_files, "\\d+")

cat("Total CIKs in zip file:", length(all_ciks), "\n")

# 2.2 Get already-processed CIKs from output folder
existing_files <- list.files(output_path, pattern = "\\.parquet$")
done_ciks <- str_extract(existing_files, "\\d+")

cat("Already processed:", length(done_ciks), "\n")

# 2.3 Find remaining CIKs
remaining_ciks <- setdiff(all_ciks, done_ciks)
remaining_files <- json_files[all_ciks %in% remaining_ciks]

cat("Remaining to process:", length(remaining_ciks), "\n")

# Exit early if nothing to do
if (length(remaining_files) == 0) {
  cat("All files already processed!\n")
} else {

  temp_dir <- tempdir()

  # Function to extract cash flows
  extract_cashflows <- function(file_path) {
    json_data <- tryCatch(fromJSON(file_path), error = function(e) NULL)
    if (is.null(json_data)) return(NULL)

    gaap <- json_data$facts$`us-gaap`

    get_cf <- function(concept_name, cf_type) {
      cf <- tryCatch(gaap[[concept_name]]$units$USD, error = function(e) NULL)
      if (is.null(cf) || nrow(cf) == 0) return(NULL)
      cf$cf_type <- cf_type
      return(cf)
    }

    bind_rows(get_cf("NetCashProvidedByUsedInOperatingActivities", "Operating"),
              get_cf("NetCashProvidedByUsedInInvestingActivities", "Investing"),
              get_cf("NetCashProvidedByUsedInFinancingActivities", "Financing"))
  }

  # Process remaining companies
  for (i in seq_along(remaining_files)) {
    if (i %% 500 == 0) cat("Processing file", i, "of", length(remaining_files), "\n")

    json_file <- remaining_files[i]
    cik <- str_extract(json_file, "\\d+")

    # Extract, process, save
    unzip(zip_path, files = json_file, exdir = temp_dir, overwrite = TRUE)
    file_path <- file.path(temp_dir, json_file)

    cf_data <- extract_cashflows(file_path)

    if (!is.null(cf_data) && nrow(cf_data) > 0) {
      cf_data$cik <- str_pad(cik, width = 10, pad = "0")
      write_parquet(cf_data, paste0(output_path, "CIK", cik, ".parquet"))
    }

    file.remove(file_path)
  }

  cat("Done! Total files now in output:", length(list.files(output_path, pattern = "\\.parquet$")), "\n")
}


# 3. Compare cash flow tags across filings to identify restatements --------------


# 3.1 Load and combine parquet files
output_path <- "D:/XBRL/XBRL Cash Flow Tags by CIK/"
parquet_files <- list.files(output_path, pattern = "\\.parquet$", full.names = TRUE)
cf_raw <- bind_rows(lapply(parquet_files, read_parquet))

# 3.2 Filter to 10-K/10-Q rows and clean
cf_annual <- cf_raw |>
  filter(form %in% c("10-K", "10-Q"), fp %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, cf_type, start, end, val, accn, form, fp, fy, filed) |>
  mutate(start = as.Date(start),
         end   = as.Date(end),
         filed = as.Date(filed),
         fy    = as.integer(fy)) |>
  filter(!is.na(end), !is.na(filed), !is.na(val))

# 3.3 Identify the current period for each filing
# A 10-K typically reports 3 years of data; a 10-Q reports the current and prior-year quarter.
# The current period is max(end) within that filing; other end dates are comparatives.
cf_annual <- cf_annual |>
  group_by(cik, accn) |>
  mutate(current_end = max(end)) |>
  ungroup() |>
  mutate(is_current_year = end == current_end)

# 3.4 Deduplicate: one row per (cik, cf_type, end, accn)
cf_annual_dedup <- cf_annual |>
  group_by(cik, cf_type, end, accn) |>
  slice(1) |>
  ungroup()

# 3.5 Original values: the current-year row for each (cik, cf_type, end)
# This is the value as first reported in the filing where this was the primary year.
# Periods that only appear as comparatives (e.g. in a company's first-ever 10-K, prior
# years were never separately filed) have no original here and are excluded from
# restatement analysis -- the current year of that first filing IS tracked normally.
cf_original <- cf_annual_dedup |>
  filter(is_current_year) |>
  group_by(cik, cf_type, end) |>
  arrange(filed, accn, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  transmute(cik,
            cf_type,
            end,
            original_val   = val,
            original_accn  = accn,
            original_filed = filed,
            original_fy    = fy,
            original_form  = form)

# 3.6 Comparative reports: non-current-year rows in filings dated AFTER the original
# These are the prior-year comparatives that may differ from the originally reported value.
cf_comparatives <- cf_annual_dedup |>
  filter(!is_current_year) |>
  inner_join(cf_original, by = c("cik", "cf_type", "end")) |>
  filter(filed > original_filed) |>
  mutate(val_changed = val != original_val,
         change_amt  = val - original_val) |>
  arrange(cik, cf_type, end, filed, accn) |>
  group_by(cik, cf_type, end) |>
  mutate(comp_n = row_number()) |>
  ungroup()

# 3.7 Wide view: first two comparative reports per period (val_p1, val_p2)
cf_comparatives_wide <- cf_comparatives |>
  filter(comp_n <= 2) |>
  select(cik, cf_type, end, comp_n, val, accn, filed, val_changed, change_amt) |>
  pivot_wider(id_cols     = c(cik, cf_type, end),
              names_from  = comp_n,
              values_from = c(val, accn, filed, val_changed, change_amt),
              names_glue  = "{.value}_c{comp_n}") |>
  rename(val_p1        = val_c1,        val_p2        = val_c2,
         accn_p1       = accn_c1,       accn_p2       = accn_c2,
         filed_p1      = filed_c1,      filed_p2      = filed_c2,
         change_amt_p1 = change_amt_c1, change_amt_p2 = change_amt_c2) |>
  mutate(changed_p1       = if_else(is.na(val_changed_c1), 0L, as.integer(val_changed_c1)),
         changed_p2       = if_else(is.na(val_changed_c2), 0L, as.integer(val_changed_c2)),
         any_10k_revision = if_else(changed_p1 == 1L | changed_p2 == 1L, 1L, 0L)) |>
  select(-val_changed_c1, -val_changed_c2)

# 3.8 Amendment view: 10-K/A and 10-Q/A filings after the original filing for the same period
cf_amend_raw <- cf_raw |>
  filter(form %in% c("10-K/A", "10-Q/A"), fp %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, cf_type, start, end, val, accn, form, fp, fy, filed) |>
  mutate(start = as.Date(start),
         end   = as.Date(end),
         filed = as.Date(filed),
         fy    = as.integer(fy)) |>
  filter(!is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, cf_type, end, accn) |>
  slice(1) |>
  ungroup()

cf_amend_versions <- cf_amend_raw |>
  inner_join(cf_original, by = c("cik", "cf_type", "end")) |>
  filter(filed > original_filed) |>
  mutate(val_changed = val != original_val,
         change_amt  = val - original_val) |>
  arrange(cik, cf_type, end, filed, accn) |>
  group_by(cik, cf_type, end) |>
  mutate(amend_n = row_number()) |>
  ungroup()

cf_amend_summary <- cf_amend_versions |>
  group_by(cik, cf_type, end) |>
  arrange(amend_n, .by_group = TRUE) |>
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

# 3.9 Combine into one row per (cik, cf_type, end)
scf_section <- cf_original |>
  left_join(cf_comparatives_wide, by = c("cik", "cf_type", "end")) |>
  left_join(cf_amend_summary,     by = c("cik", "cf_type", "end")) |>
  mutate(changed_p1            = replace_na(changed_p1,            0L),
         changed_p2            = replace_na(changed_p2,            0L),
         any_10k_revision      = replace_na(any_10k_revision,      0L),
         has_10ka_after_10k    = replace_na(has_10ka_after_10k,    0L),
         n_10ka_after_10k      = replace_na(n_10ka_after_10k,      0L),
         changed_in_first_10ka = replace_na(changed_in_first_10ka, 0L),
         any_change_in_10ka    = replace_na(any_change_in_10ka,    0L))

# 3.10 Best available value and correction channel per CF type
scf_section <- scf_section |>
  mutate(cf_key   = recode(cf_type, "Operating" = "cfo", "Investing" = "cfi", "Financing" = "cff"),
         best_val = case_when(any_change_in_10ka == 1L ~ first_10ka_val,
                              any_10k_revision   == 1L ~ coalesce(val_p2, val_p1),
                              TRUE                     ~ original_val),
         correction_channel = case_when(
           any_change_in_10ka == 1L & any_10k_revision == 1L ~ "Both",
           any_change_in_10ka == 1L                          ~ paste0(original_form, "/A amendment"),
           any_10k_revision   == 1L                          ~ paste0(original_form, " revision"),
           TRUE                                              ~ "No change"))

# 3.11 Pivot wide: one row per (cik, end) with cfo/cfi/cff columns
section_value_order <- c("original_val", "best_val",
                         "val_p1", "val_p2", "changed_p1", "changed_p2", "any_10k_revision",
                         "change_amt_p1", "change_amt_p2",
                         "has_10ka_after_10k", "n_10ka_after_10k",
                         "first_10ka_val", "changed_in_first_10ka", "any_change_in_10ka",
                         "change_amt_first_10ka", "correction_channel")

section_col_order <- unlist(lapply(c("cfo", "cfi", "cff"), function(prefix) paste0(prefix, "_", section_value_order)))

scf_wide <- scf_section |>
  pivot_wider(id_cols     = c(cik, end, original_fy, original_accn, original_filed, original_form),
              names_from  = cf_key,
              values_from = c(original_val, best_val,
                              val_p1, val_p2, changed_p1, changed_p2, any_10k_revision,
                              change_amt_p1, change_amt_p2,
                              has_10ka_after_10k, n_10ka_after_10k,
                              first_10ka_val, changed_in_first_10ka, any_change_in_10ka,
                              change_amt_first_10ka, correction_channel),
              names_glue  = "{cf_key}_{.value}")

# 3.12 Filing-level correction flags: one row per original filing (10-K or 10-Q)
scf <- scf_wide |>
  mutate(any_small_revision = as.integer(coalesce(cfo_any_10k_revision, 0L) == 1L |
                                         coalesce(cfi_any_10k_revision, 0L) == 1L |
                                         coalesce(cff_any_10k_revision, 0L) == 1L),
         any_salient_amendment = as.integer(coalesce(cfo_any_change_in_10ka, 0L) == 1L |
                                            coalesce(cfi_any_change_in_10ka, 0L) == 1L |
                                            coalesce(cff_any_change_in_10ka, 0L) == 1L),
         correction_route = case_when(
           any_small_revision == 1L & any_salient_amendment == 1L ~ "Both",
           any_salient_amendment == 1L ~ paste0("Salient amendment (", original_form, "/A)"),
           any_small_revision    == 1L ~ paste0("Small revision (subsequent ", original_form, ")"),
           TRUE                        ~ "No correction")) |>
  rename(fy = original_fy, filing_accn = original_accn, filing_date = original_filed,
         filing_form = original_form) |>
  select(cik, fy, end, filing_accn, filing_date, filing_form,
         any_small_revision, any_salient_amendment, correction_route,
         any_of(section_col_order)) |>
  arrange(cik, fy, end)

# 3.13 Save down
write_parquet(scf,glue("{data_path}xbrl_scf.parquet"))

# Example checks
# View(scf)
# scf |> count(correction_route)


# 4.1 Summary statistics by filing year-------------------------------------------
scf_summary_by_filing_year <- scf |>
  filter(!is.na(fy)) |>
  group_by(fy) |>
  summarise(n_filings                  = n_distinct(filing_accn),
            n_changed_small_revision   = n_distinct(filing_accn[any_small_revision   == 1L]),
            n_changed_amended_filing   = n_distinct(filing_accn[any_salient_amendment == 1L]),
            pct_changed_small_revision = n_changed_small_revision / n_filings,
            pct_changed_amended_filing = n_changed_amended_filing / n_filings,
            .groups = "drop") |>
  arrange(fy)

# 3.14 Error distribution by correction channel and CF type
# Compute per-row change amounts and % of original for each channel.
# Excludes rows where original_val == 0 (pct would be undefined).
cf_errors_long <- scf_section |>
  filter(correction_channel != "No change", original_val != 0) |>
  mutate(
    # revision channel: change from original to the most recent comparative
    revision_change_amt = if_else(any_10k_revision == 1L,
                                  coalesce(val_p2, val_p1) - original_val,
                                  NA_real_),
    revision_pct = revision_change_amt / abs(original_val),
    # amendment channel: change in the first amendment
    amendment_change_amt = if_else(any_change_in_10ka == 1L, change_amt_first_10ka, NA_real_),
    amendment_pct = amendment_change_amt / abs(original_val))

# Frequency: how often each CF type is corrected, by channel
cf_error_freq <- bind_rows(cf_errors_long |>
    filter(any_10k_revision == 1L) |>
    group_by(cf_key) |>
    summarise(correction_type = "revision",   n_errors = n(), .groups = "drop"),
  cf_errors_long |>
    filter(any_change_in_10ka == 1L) |>
    group_by(cf_key) |>
    summarise(correction_type = "amendment", n_errors = n(), .groups = "drop")) |>
  left_join(scf_section |>
    group_by(cf_key) |>
    summarise(n_total = n(), .groups = "drop"),
    by = "cf_key") |>
  mutate(pct_of_filings = n_errors / n_total) |>
  arrange(correction_type, cf_key)

# Distribution: size of error as % of original value, by channel and CF type
cf_error_dist <- bind_rows(cf_errors_long |>
    filter(any_10k_revision == 1L, !is.na(revision_pct), is.finite(revision_pct)) |>
    group_by(cf_key) |>
    summarise(correction_type  = "revision",
              n                = n(),
              mean_abs_pct     = mean(abs(revision_pct)),
              median_abs_pct   = median(abs(revision_pct)),
              p25_abs_pct      = quantile(abs(revision_pct), 0.25),
              p75_abs_pct      = quantile(abs(revision_pct), 0.75),
              p95_abs_pct      = quantile(abs(revision_pct), 0.95),
              .groups = "drop"),
  cf_errors_long |>
    filter(any_change_in_10ka == 1L, !is.na(amendment_pct), is.finite(amendment_pct)) |>
    group_by(cf_key) |>
    summarise(correction_type  = "amendment",
              n                = n(),
              mean_abs_pct     = mean(abs(amendment_pct)),
              median_abs_pct   = median(abs(amendment_pct)),
              p25_abs_pct      = quantile(abs(amendment_pct), 0.25),
              p75_abs_pct      = quantile(abs(amendment_pct), 0.75),
              p95_abs_pct      = quantile(abs(amendment_pct), 0.95),
              .groups = "drop")) |>
  arrange(correction_type, cf_key)

# View(cf_error_freq)
# View(cf_error_dist)

scf |> group_by(lubridate::year(filing_date)) |> summarise(obs=n())
test <- scf |> mutate(year=lubridate::year(filing_date)) |> group_by(cik,year) |> mutate(obs=n()) |> filter(obs==3,year==2022)
