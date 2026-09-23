# =============================================================================
# 001c - One-row-per-filing summary, built for HUMAN AUDIT
# =============================================================================
# PURPOSE
#   Collapse the pipeline down to one row per filing-period, carrying everything
#   needed to check a case against the actual SEC filing by hand: who filed it,
#   what the three section totals were as originally filed, what they became,
#   how much of the change is attributable to an accounting standard, what is
#   left unexplained, and exactly which XBRL tags moved.
#
#   The point is to make the pipeline falsifiable. Pull a random sample of rows
#   flagged as changed and a random sample flagged as unchanged, open the two
#   filings named in each row, and see whether the summary tells the truth.
#   Ready-made samples are written for precisely that (Section 8).
#
# GRAIN
#   One row per (cik, period start, period end) -- i.e. per period, anchored on
#   the filing that first reported it. That is one row per filing 99% of the
#   time: 267,282 periods come from 264,477 filings, but 2,769 filings anchor 2
#   or 3 periods (typically a 10-K covering both its annual period and a Q4).
#   `n_periods_this_filing` flags those so they are not mistaken for duplicates.
#
# UNITS
#   All numeric amounts are in RAW DOLLARS, exactly as tagged in XBRL, so they
#   can be tied to a filing without rescaling. Filings themselves present in
#   thousands or millions, so divide accordingly. The one exception is the
#   `*_changed_tags` strings, which use $ thousands to stay readable.
#
# INPUT   {data_path}scf_section_adjusted{IN_SUFFIX}.parquet   (001b)
#         {data_path}scf_line_changes_flagged{IN_SUFFIX}.parquet (001b)
#         {data_path}scf_period_summary{IN_SUFFIX}.parquet      (001a)
#         {raw_data_path}*_sub.parquet   (company name and fiscal period label)
# OUTPUT  {data_path}scf_filing_summary{IN_SUFFIX}.parquet
#         {data_path}audit_sample_changed.csv
#         {data_path}audit_sample_unchanged.csv
# =============================================================================


# 1. Setup ---------------------------------------------------------------------
library(dplyr)
library(stringr)
library(arrow)
library(glue)
library(tidyr)
library(lubridate)
library(readr)

raw_data_path <- Sys.getenv('XBRL_DATA_PATH')
data_path     <- Sys.getenv('DATA_PATH')
source("code/000-utilities-functions.R")


# 2. Configuration -------------------------------------------------------------
IN_SUFFIX <- "_full"

# rows per hand-audit sample, and the seed so the same sample can be re-drawn
AUDIT_N    <- 120
AUDIT_SEED <- 20260922

# A section counts as changed when the movement clears this, in dollars.
# Matches 001b's floor: filings round to thousands, so less is noise.
MATERIALITY_FLOOR <- 1000


# 3. Company name and fiscal period label --------------------------------------
# Neither survives 001a (which carries only cik/sic/afs), so both are rebuilt
# from the raw sub files. `name` is per filing rather than per company, because
# companies rename themselves and the filing's own label is what an auditor will
# see on EDGAR.
search <- bind_rows(
  expand.grid(year = as.character(2009:2019), period = paste0("Q", 1:4),
              stringsAsFactors = FALSE),
  data.frame(year = "2020", period = c("Q1", "Q2", "Q3", "M10", "M11", "M12"),
             stringsAsFactors = FALSE),
  expand.grid(year = as.character(2021:2023), period = sprintf("M%02d", 1:12),
              stringsAsFactors = FALSE))

filing_meta <- bind_rows(lapply(seq_len(nrow(search)), function(i) {
  f <- glue("{raw_data_path}{search$year[i]}_{search$period[i]}_sub.parquet")
  if (!file.exists(f)) return(NULL)
  read_parquet(f, col_select = c("adsh", "name", "fp", "fy", "form", "period")) |>
    filter(form %in% c("10-K", "10-Q", "10-K/A", "10-Q/A"))
})) |>
  transmute(accn = adsh, company_name = name, fp,
            sub_fy = suppressWarnings(as.integer(fy)),
            filing_period = suppressWarnings(as.integer(period))) |>
  distinct(accn, .keep_all = TRUE)

cat("filing metadata rows:", format(nrow(filing_meta), big.mark = ","), "\n")


# 4. Section figures, pivoted to one row per period ----------------------------
sa <- read_parquet(glue("{data_path}scf_section_adjusted{IN_SUFFIX}.parquet"))

# `subtotal_orig` is the section as first filed; `subtotal_difference` is
# as-filed minus latest, so the latest presentation is orig - difference.
sect <- sa |>
  filter(section %in% c("operating", "investing", "financing")) |>
  transmute(
    cik, start, end, section,
    as_filed    = subtotal_orig,
    latest      = subtotal_orig - subtotal_difference,
    change      = subtotal_difference,
    std         = std_applied,
    unexplained = unexplained_difference,
    changed     = abs(subtotal_difference) >= MATERIALITY_FLOOR,
    n_tags      = n_changed,
    testable    = recon_testable,
    ambiguous   = section_ambiguous,
    verdict     = section_verdict) |>
  mutate(sect = recode(section, operating = "cfo", investing = "cfi",
                       financing = "cff")) |>
  select(-section) |>
  pivot_wider(id_cols = c(cik, start, end), names_from = sect,
              values_from = c(as_filed, latest, change, std, unexplained,
                              changed, n_tags, testable, ambiguous, verdict),
              names_glue = "{sect}_{.value}")


# 5. Which tags actually moved, as readable strings ----------------------------
# One string per section listing every changed line, its before/after amounts in
# $ thousands, and which standard (if any) it was attributed to -- so an auditor
# can go straight to those lines in the filing instead of hunting.
lf <- read_parquet(glue("{data_path}scf_line_changes_flagged{IN_SUFFIX}.parquet"),
       col_select = c("cik","start","end","section","tag","is_subtotal","orig_val",
                      "comp_val","difference","change_type","is_standard_driven",
                      "standard","is_tag_substitution"))

tag_strings <- lf |>
  filter(!is_subtotal, section %in% c("operating","investing","financing"),
         coalesce(change_type, "Unchanged") != "Unchanged") |>
  mutate(entry = paste0(
      tag, " [", round(orig_val/1e3), " -> ", round(comp_val/1e3), "]",
      if_else(coalesce(is_standard_driven, FALSE),
              paste0(" {", coalesce(standard, "?"), "}"), ""),
      if_else(coalesce(is_tag_substitution, FALSE), " {rename}", ""))) |>
  arrange(cik, start, end, section, desc(abs(difference))) |>
  group_by(cik, start, end, section) |>
  summarise(tags_changed = paste(entry, collapse = "; "), .groups = "drop") |>
  mutate(sect = recode(section, operating = "cfo", investing = "cfi",
                       financing = "cff")) |>
  select(-section) |>
  pivot_wider(id_cols = c(cik, start, end), names_from = sect,
              values_from = tags_changed, names_glue = "{sect}_changed_tags")


# 6. Assemble ------------------------------------------------------------------
anchors <- sa |>
  distinct(cik, start, end, orig_accn, orig_filed, orig_form, orig_fy,
           comp_accn, comp_filed, amend_accn, amend_filed)

ps  <- read_parquet(glue("{data_path}scf_period_summary{IN_SUFFIX}.parquet")) |>
  select(cik, start, end, sic, afs, period_type, correction_channel,
         n_amendments, has_amendment)
pa  <- read_parquet(glue("{data_path}scf_period_adjusted{IN_SUFFIX}.parquet")) |>
  select(cik, start, end, period_type_adjusted, standards_involved, era,
         net_unexplained)

# EDGAR filing index, so a row can be checked without constructing a URL
edgar_url <- function(cik, accn) {
  if_else(is.na(accn), NA_character_,
    paste0("https://www.sec.gov/Archives/edgar/data/",
           as.integer(cik), "/", str_remove_all(accn, "-"), "/", accn, "-index.htm"))
}

scf_filing_summary <- anchors |>
  left_join(sect,        by = c("cik","start","end")) |>
  left_join(tag_strings, by = c("cik","start","end")) |>
  left_join(ps,          by = c("cik","start","end")) |>
  left_join(pa,          by = c("cik","start","end")) |>
  left_join(filing_meta, by = c("orig_accn" = "accn")) |>
  group_by(orig_accn) |>
  mutate(n_periods_this_filing = n()) |>
  ungroup() |>
  mutate(
    period_days   = as.numeric(end - start),
    period_length = case_when(period_days > 300 ~ "annual",
                              period_days > 200 ~ "9 months",
                              period_days > 120 ~ "6 months",
                              TRUE              ~ "3 months"),
    across(c(cfo_changed, cfi_changed, cff_changed), ~ coalesce(.x, FALSE)),
    any_change      = cfo_changed | cfi_changed | cff_changed,
    n_sections_changed = cfo_changed + cfi_changed + cff_changed,
    n_tags_changed  = coalesce(cfo_n_tags,0) + coalesce(cfi_n_tags,0) +
                      coalesce(cff_n_tags,0),
    all_testable    = coalesce(cfo_testable,FALSE) & coalesce(cfi_testable,FALSE) &
                      coalesce(cff_testable,FALSE),
    any_ambiguous   = coalesce(cfo_ambiguous,FALSE) | coalesce(cfi_ambiguous,FALSE) |
                      coalesce(cff_ambiguous,FALSE),
    any_unexplained = abs(coalesce(cfo_unexplained,0)) >= MATERIALITY_FLOOR |
                      abs(coalesce(cfi_unexplained,0)) >= MATERIALITY_FLOOR |
                      abs(coalesce(cff_unexplained,0)) >= MATERIALITY_FLOOR,
    orig_url  = edgar_url(cik, orig_accn),
    comp_url  = edgar_url(cik, comp_accn),
    amend_url = edgar_url(cik, amend_accn)) |>
  select(
    # who and when
    cik, company_name, fy = orig_fy, fp, period_length,
    period_start = start, period_end = end,
    orig_form, orig_filed, orig_accn, orig_url,
    comp_filed, comp_accn, comp_url,
    amend_filed, amend_accn, amend_url,
    n_periods_this_filing,
    # the headline flags
    any_change, n_sections_changed, n_tags_changed, any_unexplained,
    period_type, period_type_adjusted, correction_channel, standards_involved,
    # operating
    cfo_as_filed, cfo_latest, cfo_change, cfo_std, cfo_unexplained,
    cfo_changed, cfo_n_tags, cfo_verdict, cfo_changed_tags,
    # investing
    cfi_as_filed, cfi_latest, cfi_change, cfi_std, cfi_unexplained,
    cfi_changed, cfi_n_tags, cfi_verdict, cfi_changed_tags,
    # financing
    cff_as_filed, cff_latest, cff_change, cff_std, cff_unexplained,
    cff_changed, cff_n_tags, cff_verdict, cff_changed_tags,
    # scope / data quality
    all_testable, any_ambiguous, era, sic, afs, has_amendment, n_amendments,
    net_unexplained) |>
  arrange(cik, period_end)


# 7. Save ----------------------------------------------------------------------
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

write_parquet_safely(scf_filing_summary,
  glue("{data_path}scf_filing_summary{IN_SUFFIX}.parquet"))
cat("\nSaved scf_filing_summary:", format(nrow(scf_filing_summary), big.mark=","),
    "rows x", ncol(scf_filing_summary), "cols\n")


# 8. Hand-audit samples --------------------------------------------------------
# Two CSVs to open in Excel. Restricted to sections that are testable and to the
# clean era, because a case that fails the pipeline's own quality gates is not a
# fair test of whether the summary is accurate -- audit those separately if the
# gates themselves are in question.
audit_pool <- scf_filing_summary |> filter(all_testable, era == "clean")

set.seed(AUDIT_SEED)
sample_changed <- audit_pool |> filter(any_change) |> slice_sample(n = AUDIT_N)
set.seed(AUDIT_SEED)
sample_unchanged <- audit_pool |> filter(!any_change) |> slice_sample(n = AUDIT_N)

write_csv(sample_changed,   glue("{data_path}audit_sample_changed.csv"))
write_csv(sample_unchanged, glue("{data_path}audit_sample_unchanged.csv"))

cat("Audit samples written (n =", AUDIT_N, "each, seed", AUDIT_SEED, "):\n")
cat("  audit_sample_changed.csv    -- expect the named tags to differ between the two filings\n")
cat("  audit_sample_unchanged.csv  -- expect all three section totals identical in both\n")
cat("  pool: ", format(nrow(audit_pool), big.mark=","), " testable, clean-era periods (",
    format(sum(audit_pool$any_change), big.mark=","), " changed / ",
    format(sum(!audit_pool$any_change), big.mark=","), " unchanged)\n", sep="")


# 9. Summary ------------------------------------------------------------------
cat("\n=== filings by whether any section changed ===\n")
print(scf_filing_summary |> count(any_change) |> mutate(pct = round(100*n/sum(n),1)))

cat("\n=== sections changed per filing ===\n")
print(scf_filing_summary |> count(n_sections_changed) |> mutate(pct = round(100*n/sum(n),1)))

cat("\n=== does an unexplained residual survive the standards adjustment? ===\n")
print(scf_filing_summary |> count(any_change, any_unexplained))

cat("\n=== coverage of the identity fields an auditor needs ===\n")
print(scf_filing_summary |> summarise(
  missing_company_name = sum(is.na(company_name)),
  missing_fp           = sum(is.na(fp)),
  missing_orig_url     = sum(is.na(orig_url)),
  missing_comp_url     = sum(is.na(comp_url)),
  filings_with_multiple_periods = sum(n_periods_this_filing > 1)))

cat("\n=== example row (widest changed case) ===\n")
ex <- scf_filing_summary |> filter(any_change, all_testable) |>
  slice_max(n_tags_changed, n = 1, with_ties = FALSE)
print(as.data.frame(t(ex |> select(company_name, fy, fp, period_end, orig_form,
  orig_accn, comp_accn, cfo_as_filed, cfo_latest, cfo_change, cfo_std,
  cfo_unexplained, cfo_verdict, period_type, period_type_adjusted))))
cat("\noperating tags changed:\n"); cat(str_wrap(ex$cfo_changed_tags, 110), "\n")
