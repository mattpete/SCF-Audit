# 1. Setup -----------------------------------------------------------------------
library(jsonlite)
library(dplyr)
library(stringr)
library(arrow)
library(glue)
library(tidyr)

zip_path   <- "D:/XBRL/companyfacts.zip"
output_dir <- "D:/XBRL/XBRL Tags by CIK/"
data_path  <- "C:/Users/mnp4/Dropbox/Cash Flow Audit Idea/Data/"

dir.create(output_dir, showWarnings = FALSE)


# 2. Extract tags from XBRL data -------------------------------------------------
# Each JSON is opened once. CF, NI, and equity rows are stacked into a single
# file per CIK, distinguished by a tag_type column.

# 2.1 Get all CIKs
zip_contents <- unzip(zip_path, list = TRUE)
json_files   <- zip_contents$Name[grepl("[.]json$", zip_contents$Name)]
all_ciks     <- str_extract(json_files, "\\d+")

cat("Total CIKs in zip file:", length(all_ciks), "\n")

done_ciks       <- str_extract(list.files(output_dir, pattern = "[.]parquet$"), "\\d+")
remaining_ciks  <- setdiff(all_ciks, done_ciks)
remaining_files <- json_files[all_ciks %in% remaining_ciks]

cat("Already processed:", length(done_ciks), "\n")
cat("Remaining to process:", length(remaining_ciks), "\n")

if (length(remaining_files) == 0) {
  cat("All files already processed!\n")
} else {

  temp_dir <- tempdir()

  extract_all_tags <- function(file_path) {
    json_data <- tryCatch(fromJSON(file_path), error = function(e) NULL)
    if (is.null(json_data)) return(NULL)

    gaap <- json_data$facts$`us-gaap`

    get_usd <- function(concept) {
      x <- tryCatch(gaap[[concept]]$units$USD, error = function(e) NULL)
      if (is.null(x) || nrow(x) == 0) return(NULL)
      as.data.frame(x)
    }

    # Cash flows
    cfo <- get_usd("NetCashProvidedByUsedInOperatingActivities")
    cfi <- get_usd("NetCashProvidedByUsedInInvestingActivities")
    cff <- get_usd("NetCashProvidedByUsedInFinancingActivities")
    if (!is.null(cfo)) cfo$tag_type <- "Operating"
    if (!is.null(cfi)) cfi$tag_type <- "Investing"
    if (!is.null(cff)) cff$tag_type <- "Financing"

    # Net income: NetIncomeLoss with ProfitLoss as fallback
    ni <- get_usd("NetIncomeLoss")
    if (is.null(ni)) ni <- get_usd("ProfitLoss")
    if (!is.null(ni)) ni$tag_type <- "NetIncomeLoss"

    # Stockholders equity: StockholdersEquity with noncontrolling-interest version as fallback
    eq <- get_usd("StockholdersEquity")
    if (is.null(eq)) eq <- get_usd("StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest")
    if (!is.null(eq)) eq$tag_type <- "StockholdersEquity"

    result <- bind_rows(cfo, cfi, cff, ni, eq)
    if (nrow(result) == 0) return(NULL)

    # Ensure start column exists for equity rows (balance sheet has no start date)
    if (!"start" %in% names(result)) result$start <- NA_character_
    result$start <- as.character(result$start)

    result
  }

  for (i in seq_along(remaining_files)) {
    if (i %% 500 == 0) cat("Processing file", i, "of", length(remaining_files), "\n")

    json_file <- remaining_files[i]
    cik       <- str_extract(json_file, "\\d+")

    unzip(zip_path, files = json_file, exdir = temp_dir, overwrite = TRUE)
    result <- extract_all_tags(file.path(temp_dir, json_file))

    if (!is.null(result)) {
      result$cik <- str_pad(cik, width = 10, pad = "0")
      write_parquet(result, paste0(output_dir, "CIK", cik, ".parquet"))
    }

    file.remove(file.path(temp_dir, json_file))
  }

  cat("Done! Total files in output:", length(list.files(output_dir, pattern = "[.]parquet$")), "\n")
}


# 3. Load and split by tag type --------------------------------------------------

raw <- bind_rows(lapply(list.files(output_dir, pattern = "[.]parquet$", full.names = TRUE), read_parquet)) |>
  mutate(start = as.Date(start),
         end   = as.Date(end),
         filed = as.Date(filed),
         fy    = as.integer(fy))

cf_raw <- raw |> filter(tag_type %in% c("Operating", "Investing", "Financing"))
ni_raw <- raw |> filter(tag_type == "NetIncomeLoss")
eq_raw <- raw |> filter(tag_type == "StockholdersEquity")


# 4. Build SCF dataset (Cash Flows) ----------------------------------------------

cf_annual <- cf_raw |>
  filter(form %in% c("10-K", "10-Q"), fp %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, cf_type = tag_type, start, end, val, accn, form, fp, fy, filed) |>
  filter(!is.na(start), !is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, accn) |>
  mutate(current_end = max(end)) |>
  ungroup() |>
  mutate(is_current_year = end == current_end)

# Deduplicate on (cik, cf_type, start, end, accn) — start is now part of the key
# so YTD and standalone quarter rows are kept as distinct periods
cf_annual_dedup <- cf_annual |>
  group_by(cik, cf_type, start, end, accn) |>
  slice(1) |>
  ungroup()

cf_original <- cf_annual_dedup |>
  filter(is_current_year) |>
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

cf_comparatives <- cf_annual_dedup |>
  filter(!is_current_year) |>
  inner_join(cf_original, by = c("cik", "cf_type", "start", "end")) |>
  filter(filed > original_filed) |>
  mutate(val_changed = val != original_val, change_amt = val - original_val) |>
  arrange(cik, cf_type, start, end, filed, accn) |>
  group_by(cik, cf_type, start, end) |>
  mutate(comp_n = row_number()) |>
  ungroup()

cf_comparatives_wide <- cf_comparatives |>
  filter(comp_n <= 2) |>
  select(cik, cf_type, start, end, comp_n, val, accn, filed, val_changed, change_amt) |>
  pivot_wider(id_cols = c(cik, cf_type, start, end), names_from = comp_n,
              values_from = c(val, accn, filed, val_changed, change_amt),
              names_glue = "{.value}_c{comp_n}") |>
  rename(val_p1 = val_c1, val_p2 = val_c2, accn_p1 = accn_c1, accn_p2 = accn_c2,
         filed_p1 = filed_c1, filed_p2 = filed_c2,
         change_amt_p1 = change_amt_c1, change_amt_p2 = change_amt_c2) |>
  mutate(changed_p1       = if_else(is.na(val_changed_c1), 0L, as.integer(val_changed_c1)),
         changed_p2       = if_else(is.na(val_changed_c2), 0L, as.integer(val_changed_c2)),
         any_10k_revision = if_else(changed_p1 == 1L | changed_p2 == 1L, 1L, 0L)) |>
  select(-val_changed_c1, -val_changed_c2)

cf_amend_raw <- cf_raw |>
  filter(form %in% c("10-K/A", "10-Q/A"), fp %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, cf_type = tag_type, start, end, val, accn, form, fp, fy, filed) |>
  filter(!is.na(start), !is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, cf_type, start, end, accn) |>
  slice(1) |>
  ungroup()

cf_amend_summary <- cf_amend_raw |>
  inner_join(cf_original, by = c("cik", "cf_type", "start", "end")) |>
  filter(filed > original_filed) |>
  mutate(val_changed = val != original_val, change_amt = val - original_val) |>
  arrange(cik, cf_type, start, end, filed, accn) |>
  group_by(cik, cf_type, start, end) |>
  mutate(amend_n = row_number()) |>
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
                  n_10ka_after_10k, changed_in_first_10ka, any_change_in_10ka), replace_na, 0L),
         cf_key = recode(cf_type, "Operating" = "cfo", "Investing" = "cfi", "Financing" = "cff"),
         best_val = case_when(any_change_in_10ka == 1L ~ first_10ka_val,
                              any_10k_revision   == 1L ~ coalesce(val_p2, val_p1),
                              TRUE                     ~ original_val),
         correction_channel = case_when(
           any_change_in_10ka == 1L & any_10k_revision == 1L ~ "Both",
           any_change_in_10ka == 1L ~ paste0(original_form, "/A amendment"),
           any_10k_revision   == 1L ~ paste0(original_form, " revision"),
           TRUE                     ~ "No change"))

section_value_order <- c("original_val", "best_val", "val_p1", "val_p2", "changed_p1", "changed_p2",
                         "any_10k_revision", "change_amt_p1", "change_amt_p2", "has_10ka_after_10k",
                         "n_10ka_after_10k", "first_10ka_val", "changed_in_first_10ka",
                         "any_change_in_10ka", "change_amt_first_10ka", "correction_channel")
section_col_order <- unlist(lapply(c("cfo", "cfi", "cff"), function(p) paste0(p, "_", section_value_order)))

scf_wide <- scf_section |>
  pivot_wider(id_cols     = c(cik, start, end, original_fy, original_accn, original_filed, original_form),
              names_from  = cf_key,
              values_from = c(original_val, best_val, val_p1, val_p2, changed_p1, changed_p2,
                              any_10k_revision, change_amt_p1, change_amt_p2, has_10ka_after_10k,
                              n_10ka_after_10k, first_10ka_val, changed_in_first_10ka,
                              any_change_in_10ka, change_amt_first_10ka, correction_channel),
              names_glue = "{cf_key}_{.value}")

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
  select(cik, fy, start, end, filing_accn, filing_date, filing_form,
         any_small_revision, any_salient_amendment, correction_route,
         any_of(section_col_order)) |>
  arrange(cik, fy, start, end)


# 5. Build Net Income dataset ----------------------------------------------------
# Flow variable (YTD in 10-Qs) — same logic as cash flows.

ni_annual <- ni_raw |>
  filter(form %in% c("10-K", "10-Q"), fp %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, start, end, val, accn, form, fp, fy, filed) |>
  filter(!is.na(start), !is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, accn) |>
  mutate(current_end = max(end)) |>
  ungroup() |>
  mutate(is_current_year = end == current_end)

ni_annual_dedup <- ni_annual |>
  group_by(cik, start, end, accn) |>
  slice(1) |>
  ungroup()

ni_original <- ni_annual_dedup |>
  filter(is_current_year) |>
  group_by(cik, start, end) |>
  arrange(filed, accn, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  transmute(cik, start, end,
            ni_original_val   = val,
            ni_original_accn  = accn,
            ni_original_filed = filed,
            ni_original_fy    = fy,
            ni_original_form  = form)

ni_comp_wide <- ni_annual_dedup |>
  filter(!is_current_year) |>
  inner_join(ni_original, by = c("cik", "start", "end")) |>
  filter(filed > ni_original_filed) |>
  mutate(val_changed = val != ni_original_val, change_amt = val - ni_original_val) |>
  arrange(cik, start, end, filed, accn) |>
  group_by(cik, start, end) |>
  mutate(comp_n = row_number()) |>
  ungroup() |>
  filter(comp_n <= 2) |>
  select(cik, start, end, comp_n, val, accn, filed, val_changed, change_amt) |>
  pivot_wider(id_cols = c(cik, start, end), names_from = comp_n,
              values_from = c(val, accn, filed, val_changed, change_amt),
              names_glue = "{.value}_c{comp_n}") |>
  rename(ni_val_p1 = val_c1, ni_val_p2 = val_c2,
         ni_change_amt_p1 = change_amt_c1, ni_change_amt_p2 = change_amt_c2) |>
  mutate(ni_any_revision = if_else(
    coalesce(as.integer(val_changed_c1), 0L) == 1L | coalesce(as.integer(val_changed_c2), 0L) == 1L,
    1L, 0L)) |>
  select(-val_changed_c1, -val_changed_c2, -starts_with("accn_"), -starts_with("filed_"))

ni_amend_summary <- ni_raw |>
  filter(form %in% c("10-K/A", "10-Q/A"), fp %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, start, end, val, accn, form, fp, fy, filed) |>
  filter(!is.na(start), !is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, start, end, accn) |>
  slice(1) |>
  ungroup() |>
  inner_join(ni_original, by = c("cik", "start", "end")) |>
  filter(filed > ni_original_filed) |>
  mutate(val_changed = val != ni_original_val, change_amt = val - ni_original_val) |>
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
         ni_best_val = case_when(
           ni_any_change_in_amend == 1L ~ ni_first_amend_val,
           ni_any_revision        == 1L ~ coalesce(ni_val_p2, ni_val_p1),
           TRUE                         ~ ni_original_val))


# 6. Build Stockholders Equity dataset -------------------------------------------
# Point-in-time (balance sheet) — no start date, no YTD issue.

eq_clean <- eq_raw |>
  filter(form %in% c("10-K", "10-Q"), fp %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, end, val, accn, form, fp, fy, filed) |>
  filter(!is.na(end), !is.na(filed), !is.na(val)) |>
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
  filter(form %in% c("10-K/A", "10-Q/A"), fp %in% c("FY", "Q1", "Q2", "Q3")) |>
  select(cik, end, val, accn, form, fp, fy, filed) |>
  filter(!is.na(end), !is.na(filed), !is.na(val)) |>
  group_by(cik, end, accn) |>
  slice(1) |>
  ungroup() |>
  inner_join(eq_original, by = c("cik", "end")) |>
  filter(filed > eq_original_filed) |>
  mutate(val_changed = val != eq_original_val, change_amt = val - eq_original_val) |>
  arrange(cik, end, filed, accn) |>
  group_by(cik, end) |>
  summarise(eq_has_amendment          = 1L,
            eq_first_amend_val        = first(val),
            eq_change_amt_first_amend = first(change_amt),
            eq_any_change_in_amend    = if_else(any(val_changed), 1L, 0L),
            .groups = "drop")

eq_wide <- eq_original |>
  left_join(eq_revisions,    by = c("cik", "end")) |>
  left_join(eq_amend_summary, by = c("cik", "end")) |>
  mutate(eq_any_revision        = replace_na(eq_any_revision,        0L),
         eq_has_amendment       = replace_na(eq_has_amendment,       0L),
         eq_any_change_in_amend = replace_na(eq_any_change_in_amend, 0L),
         eq_best_val = case_when(
           eq_any_change_in_amend == 1L ~ eq_first_amend_val,
           eq_any_revision        == 1L ~ eq_revised_val,
           TRUE                         ~ eq_original_val))


# 7. Join into one dataset and save ----------------------------------------------
# All three datasets key on (cik, end): one row per company-period.
# Identifiers are coalesced across tag types in case a period has NI/equity
# but no cash flow data (or vice versa).

xbrl_all <- scf |>
  full_join(ni_wide, by = c("cik", "start", "end")) |>
  # Filter to YTD periods only before joining equity (which has no start date).
  # For a given (cik, end), the YTD row has the earliest start; standalone quarters
  # and any duplicates are dropped here.
  group_by(cik, end) |>
  filter(is.na(start) | start == min(start, na.rm = TRUE)) |>
  slice(1) |>
  ungroup() |>
  full_join(eq_wide, by = c("cik", "end")) |>
  mutate(
    # Coalesce identifiers across tag types
    fy          = coalesce(fy,          ni_original_fy,    eq_original_fy),
    filing_accn = coalesce(filing_accn, ni_original_accn,  eq_original_accn),
    filing_date = coalesce(filing_date, ni_original_filed, eq_original_filed),
    filing_form = coalesce(filing_form, ni_original_form,  eq_original_form),

    # Change amounts (best - original)
    cfo_change = cfo_best_val - cfo_original_val,
    cfi_change = cfi_best_val - cfi_original_val,
    cff_change = cff_best_val - cff_original_val,
    ni_change  = ni_best_val  - ni_original_val,
    eq_change  = eq_best_val  - eq_original_val,

    # Correction type per variable: Big R (amendment) > Revision > None
    cfo_correction = case_when(coalesce(cfo_any_change_in_10ka, 0L) == 1L ~ "Big R",
                               coalesce(cfo_any_10k_revision,   0L) == 1L ~ "Revision",
                               TRUE                                        ~ "None"),
    cfi_correction = case_when(coalesce(cfi_any_change_in_10ka, 0L) == 1L ~ "Big R",
                               coalesce(cfi_any_10k_revision,   0L) == 1L ~ "Revision",
                               TRUE                                        ~ "None"),
    cff_correction = case_when(coalesce(cff_any_change_in_10ka, 0L) == 1L ~ "Big R",
                               coalesce(cff_any_10k_revision,   0L) == 1L ~ "Revision",
                               TRUE                                        ~ "None"),
    ni_correction  = case_when(coalesce(ni_any_change_in_amend, 0L) == 1L ~ "Big R",
                               coalesce(ni_any_revision,        0L) == 1L ~ "Revision",
                               TRUE                                        ~ "None"),
    eq_correction  = case_when(coalesce(eq_any_change_in_amend, 0L) == 1L ~ "Big R",
                               coalesce(eq_any_revision,        0L) == 1L ~ "Revision",
                               TRUE                                        ~ "None")
  ) |>
  select(
    cik, filing_accn, filing_date, filing_form, fy, end,
    cfo_original_val, cfo_best_val, cfo_change, cfo_correction,
    cfi_original_val, cfi_best_val, cfi_change, cfi_correction,
    cff_original_val, cff_best_val, cff_change, cff_correction,
    ni_original_val,  ni_best_val,  ni_change,  ni_correction,
    eq_original_val,  eq_best_val,  eq_change,  eq_correction
  ) |>
  arrange(cik, end)

write_parquet(xbrl_all, glue("{data_path}xbrl_all.parquet"))
cat("Saved xbrl_all.parquet:", nrow(xbrl_all), "rows,", ncol(xbrl_all), "columns\n")


# SCRATCH: Explore all XBRL tags for a given CIK ---------------------------------
# Produces a long-format dataframe with every us-gaap tag reported by this company,
# optionally filtered to one or more specific filings (accession numbers).
# Use this to find the right tag name when a value is unexpectedly missing.

explore_cik  <- "0001126328"   # <-- CIK to inspect
filter_accns <- NULL           # <-- set to a character vector of accession numbers
                               #     to limit to specific filings, e.g.:
                               #     c("0001193125-14-069574", "0001193125-15-069574")
                               #     or leave NULL to return all filings

# Load and extract JSON
zip_contents <- unzip(zip_path, list = TRUE)
json_files   <- zip_contents$Name[grepl("[.]json$", zip_contents$Name)]
json_file    <- json_files[grepl(paste0("CIK", explore_cik, "[.]json$"), json_files)]

temp_dir <- tempdir()
unzip(zip_path, files = json_file, exdir = temp_dir, overwrite = TRUE)
raw_json <- fromJSON(file.path(temp_dir, json_file))
gaap     <- raw_json$facts$`us-gaap`

# Flatten every us-gaap tag into one long dataframe: one row per tag × filing × period
all_tags <- bind_rows(lapply(names(gaap), function(tag) {
  x <- tryCatch(gaap[[tag]]$units$USD, error = function(e) NULL)
  if (is.null(x) || nrow(x) == 0) return(NULL)
  x <- x |>
    select(any_of(c("start", "end", "val", "accn", "form", "fp", "fy", "filed"))) |>
    mutate(across(everything(), as.character))
  x$tag <- tag
  x
})) |>
  mutate(start = as.Date(start), end = as.Date(end),
         filed = as.Date(filed), val = as.numeric(val)) |>
  filter(form %in% c("10-K", "10-Q", "10-K/A", "10-Q/A")) |>
  select(tag, accn, form, fp, fy, filed, start, end, val) |>
  arrange(filed, tag, end)

# Optionally filter to specific accession numbers
if (!is.null(filter_accns)) {
  all_tags <- all_tags |> filter(accn %in% filter_accns)
}

cat("CIK:", explore_cik, " | Total tag-period rows:", nrow(all_tags),
    " | Distinct tags:", n_distinct(all_tags$tag),
    " | Distinct filings:", n_distinct(all_tags$accn), "\n\n")

# Print — View() is easier for manual inspection in RStudio
print(all_tags, n = 50, width = Inf)
# View(all_tags)   # uncomment to open in RStudio viewer

pfg_xbrl_rev <- xbrl_all |> filter(cik=="0001126328")


xbrl_all |> 
  mutate(year=year(filing_date),
         miss_cfo = if_else(is.na(cfo_original_val),1,0),
         miss_ni = if_else(is.na(ni_original_val),1,0)) |> 
  group_by(year) |> 
  summarise(obs=n(),
            miss_cfo=sum(miss_cfo),
            miss_ni=sum(miss_ni)) 
