# 1. Setup -----------------------------------------------------------------------
library(dplyr)
library(stringr)
library(arrow)
library(glue)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)


setwd("C:/Users/mnp4/Dropbox/Cash Flow Audit Idea/Code")
raw_data_path <- "D:/XBRL/XBRL Extracted Data/"
data_path     <- "C:/Users/mnp4/Dropbox/Cash Flow Audit Idea/Data/"

# 2. Load GAAP Taxonomy ----------------------------------------------------------
taxonomy_raw <- read_excel(glue("{data_path}GAAP_Taxonomy.xlsx"), sheet = "Presentation")

taxonomy_clean <- taxonomy_raw |> 
  filter(definition %in% c("152200 - Statement - Statement of Cash Flows",
                           "152201 - Statement - Statement of Cash Flows, Additional Cash Flow Elements",
                           "152205 - Statement - Statement of Cash Flows, Supplemental Disclosures",
                           "160000 - Statement - Statement of Cash Flows, Deposit Based Operations",
                           "164000 - Statement - Statement of Cash Flows, Insurance Based Operations",
                           "168400 - Statement - Statement of Cash Flows, Securities Based Operations",
                           "170000 - Statement - Statement of Cash Flows, Real Estate, Including REITs",
                           "172600 - Statement - Statement of Cash Flows, Direct Method Operating Activities"))


# 3. Build period file list -------------------------------------------------------
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


# 4. Load, filter, and stack all periods -----------------------------------------
target_tags <- taxonomy_clean$name

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
      across(any_of("fy"), ~ suppressWarnings(as.integer(.)))
    ) |>
    (\(df) if (!"fy" %in% names(df)) mutate(df, fy = NA_integer_) else df)()

  # --- Resolve parent vs. dimensioned values ---
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
      start             = if_else(qtrs > 0L, end %m-% months(qtrs * 3L), as.Date(NA)),
      is_current_period = (ddate == period)
    ) |>
    rename(val = value, accn = adsh) |>
    select(cik, accn, tag, start, end, val, form, fp, fy, filed, is_current_period)

  stacked_raw[[i]] <- tag_joined
  gc()
}

all_raw <- bind_rows(stacked_raw)
rm(stacked_raw); gc()

cat("Total rows loaded:", nrow(all_raw), "\n")
cat("Distinct CIKs:", n_distinct(all_raw$cik), "\n")
cat("Distinct tags:", n_distinct(all_raw$tag), "\n")

write_parquet(all_raw, glue("{data_path}xbrl_tsv_scf_tags_raw.parquet"))


# 5. Build tag-level restatement dataset -----------------------------------------

# Reload from parquet if starting here (skipping the Section 4 loop)
all_raw <- read_parquet(glue("{data_path}xbrl_tsv_scf_tags_raw.parquet")) |>
  mutate(start = as.Date(start), end = as.Date(end), filed = as.Date(filed),
         across(any_of("fy"), as.integer)) |>
  (\(df) if (!"fy" %in% names(df)) mutate(df, fy = NA_integer_) else df)()

# Filter to annual/quarterly periods; drop instantaneous rows (qtrs == 0 → start NA)
tag_annual <- all_raw |>
  filter(form %in% c("10-K", "10-Q", "10-K/A", "10-Q/A"),
         fp   %in% c("FY", "Q1", "Q2", "Q3")) |>
  filter(!is.na(start), !is.na(end), !is.na(filed), !is.na(val))

# Deduplicate on (cik, tag, start, end, accn)
tag_annual_dedup <- tag_annual |>
  group_by(cik, tag, start, end, accn) |>
  slice(1) |>
  ungroup()

# Original: earliest current-period regular filing for each (cik, tag, start, end)
tag_original <- tag_annual_dedup |>
  filter(is_current_period, form %in% c("10-K", "10-Q")) |>
  group_by(cik, tag, start, end) |>
  arrange(filed, accn, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  transmute(cik, tag, start, end,
            filing_accn  = accn,
            filing_date  = filed,
            filing_form  = form,
            fy,
            original_val = val)

# Comparatives: non-current-period rows in regular filings filed after original
tag_comp_wide <- tag_annual_dedup |>
  filter(!is_current_period, form %in% c("10-K", "10-Q")) |>
  inner_join(tag_original, by = c("cik", "tag", "start", "end")) |>
  filter(filed > filing_date) |>
  mutate(change_amt = val - original_val) |>
  arrange(cik, tag, start, end, filed, accn) |>
  group_by(cik, tag, start, end) |>
  mutate(comp_n = row_number()) |>
  ungroup() |>
  filter(comp_n <= 2) |>
  select(cik, tag, start, end, comp_n, val, change_amt) |>
  pivot_wider(id_cols     = c(cik, tag, start, end),
              names_from  = comp_n,
              values_from = c(val, change_amt),
              names_glue  = "{.value}_p{comp_n}") |>
  (\(df) {
    for (col in setdiff(c("val_p1", "change_amt_p1", "val_p2", "change_amt_p2"), names(df))) {
      df[[col]] <- NA_real_
    }
    df
  })()

# Amendments: 10-K/A or 10-Q/A filings filed after original
tag_amend_summary <- tag_annual_dedup |>
  filter(form %in% c("10-K/A", "10-Q/A")) |>
  inner_join(tag_original, by = c("cik", "tag", "start", "end")) |>
  filter(filed > filing_date) |>
  mutate(val_changed = val != original_val,
         change_amt  = val - original_val) |>
  arrange(cik, tag, start, end, filed, accn) |>
  group_by(cik, tag, start, end) |>
  summarise(any_amendment       = 1L,
            first_amend_val     = first(val),
            change_amt_amend    = first(change_amt),
            any_change_in_amend = if_else(any(val_changed), 1L, 0L),
            .groups = "drop")

# Disaggregation: tags appearing in comparative filings that were never the
# current-period tag in the original filing for that (cik, start, end)
orig_period_date <- tag_original |>
  group_by(cik, start, end) |>
  summarise(min_filing_date = min(filing_date), .groups = "drop")

new_comp_tags <- tag_annual_dedup |>
  filter(!is_current_period, form %in% c("10-K", "10-Q")) |>
  inner_join(orig_period_date, by = c("cik", "start", "end")) |>
  filter(filed > min_filing_date) |>
  anti_join(tag_original, by = c("cik", "tag", "start", "end")) |>
  group_by(cik, tag, start, end) |>
  arrange(filed, accn, .by_group = TRUE) |>
  slice(1) |>
  ungroup()

period_new_tags <- new_comp_tags |>
  group_by(cik, start, end) |>
  summarise(
    has_new_tags     = TRUE,
    n_new_tags       = n_distinct(tag),
    sum_new_tag_vals = sum(val, na.rm = TRUE),
    .groups = "drop"
  )

# Period-level filing metadata for borrowing into new-tag rows
period_original_meta <- tag_original |>
  group_by(cik, start, end) |>
  arrange(filing_date, filing_accn, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  select(cik, start, end, filing_accn, filing_date, filing_form, fy)

# Join and compute correction indicators for originally-filed tags
scf_tags <- tag_original |>
  left_join(tag_comp_wide,     by = c("cik", "tag", "start", "end")) |>
  left_join(tag_amend_summary, by = c("cik", "tag", "start", "end")) |>
  left_join(period_new_tags,   by = c("cik", "start", "end")) |>
  mutate(
    is_sign_flip        = coalesce(coalesce(val_p2, val_p1) == -original_val, FALSE),
    any_revision        = as.integer(
      (coalesce(change_amt_p1 != 0, FALSE) |
       coalesce(change_amt_p2 != 0, FALSE)) &
      !is_sign_flip),
    any_amendment       = replace_na(any_amendment,       0L),
    any_change_in_amend = replace_na(any_change_in_amend, 0L),
    has_new_tags        = coalesce(has_new_tags, FALSE),
    likely_disaggregation = (
      any_revision == 1L &
      has_new_tags &
      coalesce(change_amt_p1, 0) < 0
    ),
    best_val = case_when(
      any_change_in_amend == 1L ~ first_amend_val,
      likely_disaggregation     ~ original_val,
      any_revision        == 1L ~ coalesce(val_p2, val_p1),
      TRUE                      ~ original_val),
    correction = case_when(
      any_change_in_amend == 1L ~ "Amendment",
      likely_disaggregation     ~ "Reclassification",
      is_sign_flip              ~ "Sign Flip",
      any_revision        == 1L ~ "Revision",
      TRUE                      ~ "None"),
    change_amt = best_val - original_val
  ) |>
  select(cik, filing_accn, filing_date, filing_form, fy, start, end, tag,
         original_val, best_val, change_amt,
         has_new_tags, likely_disaggregation, is_sign_flip, correction)

# Rows for newly disaggregated tags: appear in comparative but not in original filing.
# original_val is NA; val_p1 carries the first comparative value.
new_tag_rows <- new_comp_tags |>
  select(-any_of("fy")) |>
  left_join(period_original_meta, by = c("cik", "start", "end")) |>
  transmute(
    cik, filing_accn, filing_date, filing_form, fy, start, end, tag,
    original_val          = 0,
    best_val              = val,
    change_amt            = val,
    has_new_tags          = TRUE,
    likely_disaggregation = FALSE,
    is_sign_flip          = FALSE,
    correction            = "Disaggregated"
  )

scf_tags <- bind_rows(scf_tags, new_tag_rows) |>
  arrange(cik, end, tag)


# 6. Save ------------------------------------------------------------------------
write_parquet(scf_tags, glue("{data_path}xbrl_scf_tags.parquet"))
cat("Saved xbrl_scf_tags.parquet:", nrow(scf_tags), "rows,",
    n_distinct(scf_tags$cik), "distinct CIKs,",
    n_distinct(scf_tags$tag), "distinct tags\n")

data <- read_parquet(glue("{data_path}xbrl_scf_tags.parquet"))

test <- data |> filter(cik=="0000104169",end=='2011-07-31')


test_xbrl <- read_tsv(glue("{raw_data_path}2012_Q3_num.tsv")) |> 
  filter(adsh=="0000104169-12-000014")
