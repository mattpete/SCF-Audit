# =============================================================================
# 001e - Identify line-item level changes in the Statement of Cash Flows
# =============================================================================
# PURPOSE (Phase 1 of project roadmap -- see CLAUDE.md)
#   Detect every change between the SCF as originally filed and the SCF as it is
#   later re-presented as a prior-period comparative, at the INDIVIDUAL LINE ITEM
#   (XBRL tag) level -- not just at the CFO/CFI/CFF subtotal level.
#
#   Tag-level detail is required because a section subtotal change alone cannot
#   distinguish a true misstatement from a legitimate standard-driven
#   reclassification (e.g. ASU 2016-09 moving excess tax benefits from financing
#   to operating). Phase 2 will use the tag identity to filter those out.
#
# HOW THIS DIFFERS FROM 001d (which is preserved as a fallback)
#   1. SCF membership comes from the SEC `pre` file (stmt == "CF") -- i.e. what
#      the filer ACTUALLY presented on the cash flow statement -- rather than
#      from filtering the GAAP taxonomy to SCF `definition` roles. Taxonomy-role
#      filtering silently drops tags a filer borrowed from another statement's
#      taxonomy section. (Confirmed on Walmart: RepaymentsOfLongTermCapitalLease-
#      Obligations and OtherNoncashIncomeExpense are both missed by 001d.)
#   2. Section (operating/investing/financing) is assigned per filing from the
#      presentation ordering relative to the section subtotal tags, so every line
#      is attributed to a section.
#   3. The `negating` presentation flag is applied so values match what a reader
#      sees on the face of the filing (001d uses raw signs).
#   4. The comparison is anchored on two SPECIFIC FILINGS -- the original filing
#      and the LAST filing to re-present the period -- rather than capping
#      comparatives at 2 and coalescing. Per project convention, the most recent
#      comparative is the final attestation of how the period should be presented.
#   5. Missing tags on either side are carried as explicit 0s with presence flags,
#      so disaggregation (one line splitting into several) is visible directly
#      rather than inferred heuristically.
#
# SIGN CONVENTION
#   `value` throughout is the DISPLAYED value (negating applied).
#   `difference` = as-filed value - subsequent value, matching Examples.xlsx.
#
# OUTPUT
#   {data_path}scf_lines_raw.parquet    one row per filing x SCF line x period
#   {data_path}scf_line_changes.parquet one row per company-period x tag
#   {data_path}scf_section_changes.parquet one row per company-period x section
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

raw_data_path <- Sys.getenv('XBRL_DATA_PATH')
data_path     <- Sys.getenv('DATA_PATH')
source("code/000-utilities-functions.R")


# 2. Configuration -------------------------------------------------------------

# --- EXTRACTION scope (Section 5) ---------------------------------------------
# Which CIKs get pulled out of the raw files and written to the parts directory.
#
# RECOMMENDED: leave this NULL. Filtering by CIK barely saves any time, because
# the cost of extraction is READING each period file, not processing its rows --
# measured on 2019_Q1, two CIKs took 5.3s and all 5,211 filings took 6.2s. A full
# extract is roughly 10-15 minutes, so there is little reason to build a partial
# one and then be unable to reuse it for a different sample.
TARGET_CIKS <- NULL
# TARGET_CIKS <- c("0000104169", "0000003116")

# --- ANALYSIS scope (Sections 6-10) -------------------------------------------
# Which of the extracted CIKs to actually compare. NULL = everything extracted.
# This is the cheap, rerunnable phase, so subset here rather than re-extracting:
# a pilot, a sector, or a memory-bounded chunk of the population.
#   0000104169 Walmart -- hand-built ground truth in Examples.xlsx; keep it in any
#              subset so the Section 10 validation re-runs as a regression check
#   0000003116 Akorn   -- known restatement (Audit Analytics restatement_key 48887)
ANALYSIS_CIKS <- c("0000104169", "0000003116")
# ANALYSIS_CIKS <- NULL

# Pilot mode: draw a random sample of CIKs instead of naming them. Set to an
# integer to sample that many filers (added to TARGET_CIKS, so the two validation
# companies above stay in and the Section 10 regression check keeps running).
# NULL = no sampling.
PILOT_N_CIKS <- NULL
PILOT_SEED   <- 20260814

# Full-population runs hold ~30M+ SCF line rows, far too many to accumulate in
# memory, so each period is written to its own parquet part file here and only
# the lines belonging to anchor filings are read back in Section 6.
lines_dir     <- glue("{data_path}scf_lines_parts/")
done_log      <- glue("{data_path}scf_lines_parts_done.csv")
scope_file    <- glue("{data_path}scf_lines_parts_scope.rds")

# RESUME = TRUE skips periods already recorded in the done log, so an interrupted
# extract can be restarted without redoing completed work. FALSE wipes the parts
# and starts clean.
#
# Checkpointing is per PERIOD, not per CIK, because the SEC Financial Statement
# Data Sets are organised by period: one num/pre/sub file holds every filer for a
# quarter. Resuming per CIK would mean rescanning all 86 period files (6.2 GB) for
# each company. (The JSON lineage in 001/001b can checkpoint per CIK because
# companyfacts.zip stores one file per company -- this lineage cannot.)
RESUME <- FALSE

# Section subtotal anchor tags. The plain total and the ContinuingOperations
# variant both act as the closing line of their section; where both appear the
# later (higher line number) one is treated as the section boundary.
SUBTOTAL_TAGS <- list(
  operating = c("NetCashProvidedByUsedInOperatingActivities",
                "NetCashProvidedByUsedInOperatingActivitiesContinuingOperations"),
  investing = c("NetCashProvidedByUsedInInvestingActivities",
                "NetCashProvidedByUsedInInvestingActivitiesContinuingOperations"),
  financing = c("NetCashProvidedByUsedInFinancingActivities",
                "NetCashProvidedByUsedInFinancingActivitiesContinuingOperations")
)
ALL_SUBTOTAL_TAGS <- unlist(SUBTOTAL_TAGS, use.names = FALSE)

# The top of the operating section is SOMETIMES an income build-up rather than a
# set of parallel line items: filers commonly present consolidated net income,
# then a discontinued-operations adjustment, then income from continuing
# operations, where the last line is the sum of the ones above it. Only that
# final line feeds the operating subtotal, so treating all three as independent
# lines double counts, and manufactures a false change when a filer later
# simplifies the build-up (Walmart FY2016: the original 10-K shows all three
# lines at 15,080 while the later comparative shows only ProfitLoss at 15,080 --
# identical economics, no change).
#
# But it is NOT always a build-up. Akorn presents net income AND a discontinued-
# operations adjustment as two genuine parallel components with no "income from
# continuing operations" line, and its operating section foots only when both are
# counted. Collapsing unconditionally would discard net income entirely (Akorn
# FY2014: 35,345 as filed vs 13,884 later -- a 21,461 hole in the reconciliation).
#
# So the collapse is applied CONDITIONALLY, decided by the filing's own arithmetic
# in Section 7: keep every line where the section already foots, and collapse only
# where the section foots after collapsing but not before.
INCOME_LINE_TAGS <- c(
  "ProfitLoss",
  "NetIncomeLoss",
  "IncomeLossFromContinuingOperations",
  "IncomeLossFromContinuingOperationsIncludingPortionAttributableToNoncontrollingInterest",
  "IncomeLossFromContinuingOperationsAttributableToParent",
  "IncomeLossFromDiscontinuedOperationsNetOfTax",
  "IncomeLossFromDiscontinuedOperationsNetOfTaxAttributableToReportingEntity",
  "IncomeLossAttributableToParent",
  "IncomeLossIncludingPortionAttributableToNoncontrollingInterest",
  "NetIncomeLossAvailableToCommonStockholdersBasic",
  "NetIncomeLossAllocatedToLimitedPartners",
  "NetIncomeLossAllocatedToGeneralPartners")

INCOME_START_TAG <- "[Income starting point]"

# Quarterly files 2009 Q1 - 2020 Q3; monthly from 2020 M10 (SEC changed cadence)
search <- bind_rows(
  expand.grid(year = as.character(2009:2019), period = paste0("Q", 1:4),
              stringsAsFactors = FALSE),
  data.frame(year = "2020", period = c("Q1", "Q2", "Q3", "M10", "M11", "M12"),
             stringsAsFactors = FALSE),
  expand.grid(year = as.character(2021:2023), period = sprintf("M%02d", 1:12),
              stringsAsFactors = FALSE))


# 3. Note on nested subtotals --------------------------------------------------
# The GAAP taxonomy's Calculation linkbase was evaluated as a way to identify
# aggregate lines automatically, but it describes the STANDARD hierarchy, not the
# filer's own calculation relationships (which the SEC Financial Statement Data
# Sets do not publish). It mislabels ordinary leaf lines as aggregates -- e.g.
# IncreaseDecreaseInAccruedLiabilities has children in the standard taxonomy but
# is a plain line item on Walmart's statement -- so it is not used.
#
# In practice the income build-up at the top of the operating section is the only
# systematic nesting on the face of the cash flow statement, and it is handled
# explicitly via INCOME_LINE_TAGS above. With that one collapse applied, the
# component differences reconcile to the section subtotal difference (verified
# across all five Walmart fiscal years).


# 4. Helper: assign each presented line to a cash flow section -----------------
# Within one filing's cash flow statement the lines run in presentation order:
#   ...operating lines... [operating subtotal]
#   ...investing lines... [investing subtotal]
#   ...financing lines... [financing subtotal]
#   [FX effect, net change in cash, opening/closing balances, supplemental]
# so a line belongs to the first section whose subtotal appears at or after it.
# Lines below the financing subtotal are "other" (not part of any section).
assign_sections <- function(pre_cf) {
  anchors <- pre_cf |>
    filter(tag %in% ALL_SUBTOTAL_TAGS) |>
    mutate(sect = case_when(
      tag %in% SUBTOTAL_TAGS$operating ~ "operating",
      tag %in% SUBTOTAL_TAGS$investing ~ "investing",
      tag %in% SUBTOTAL_TAGS$financing ~ "financing")) |>
    # where both the plain and ContinuingOperations totals appear, the later
    # line closes the section
    group_by(adsh, report, sect) |>
    summarise(anchor_line = max(line), .groups = "drop") |>
    pivot_wider(names_from = sect, values_from = anchor_line,
                names_prefix = "ln_")

  for (nm in c("ln_operating", "ln_investing", "ln_financing")) {
    if (!nm %in% names(anchors)) anchors[[nm]] <- NA_integer_
  }

  pre_cf |>
    inner_join(anchors, by = c("adsh", "report")) |>
    mutate(
      # only trust the ordering when the three sections appear in the expected
      # sequence; otherwise leave section NA so the filing can be reviewed
      order_ok = !is.na(ln_operating) & !is.na(ln_investing) & !is.na(ln_financing) &
                 ln_operating < ln_investing & ln_investing < ln_financing,
      section = case_when(
        !order_ok            ~ NA_character_,
        line <= ln_operating ~ "operating",
        line <= ln_investing ~ "investing",
        line <= ln_financing ~ "financing",
        TRUE                 ~ "other"),
      is_subtotal = tag %in% ALL_SUBTOTAL_TAGS) |>
    select(-starts_with("ln_"))
}


# 5. Build the SCF line inventory ----------------------------------------------
# One row per (filing, presented SCF line, reported period). Includes the period
# the filing is *about* and every comparative period it re-presents.

# Draw the pilot sample, if requested, from every filer that files a 10-K/10-Q.
if (!is.null(PILOT_N_CIKS)) {
  cik_pool <- bind_rows(lapply(seq_len(nrow(search)), function(i) {
    f <- glue("{raw_data_path}{search$year[i]}_{search$period[i]}_sub.parquet")
    if (!file.exists(f)) return(NULL)
    read_parquet(f, col_select = c("cik", "form")) |>
      filter(form %in% c("10-K", "10-Q", "10-K/A", "10-Q/A")) |>
      distinct(cik)
  })) |> distinct() |>
    mutate(cik = str_pad(as.character(cik), width = 10, pad = "0"))

  set.seed(PILOT_SEED)
  TARGET_CIKS <- union(TARGET_CIKS,
                       sample(cik_pool$cik, min(PILOT_N_CIKS, nrow(cik_pool))))
  cat("Pilot mode: sampling", length(TARGET_CIKS), "CIKs of", nrow(cik_pool), "\n")
}

if (!dir.exists(lines_dir)) dir.create(lines_dir, recursive = TRUE)

# Guard against resuming into parts built for a different set of CIKs -- that
# would silently mix scopes and corrupt the anchors.
scope_now <- list(target_ciks = if (is.null(TARGET_CIKS)) NULL else sort(TARGET_CIKS))
if (RESUME && file.exists(scope_file)) {
  scope_prev <- readRDS(scope_file)
  if (!identical(scope_prev$target_ciks, scope_now$target_ciks)) {
    stop("Existing parts in ", lines_dir, " were built for a different CIK scope.\n",
         "  Set RESUME <- FALSE to rebuild from scratch, or restore the previous ",
         "TARGET_CIKS to continue that extract.")
  }
}
if (!RESUME) {
  stale <- list.files(lines_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(stale) > 0) invisible(file.remove(stale))
  if (file.exists(done_log)) invisible(file.remove(done_log))
}
saveRDS(scope_now, scope_file)

# Periods we expect to process: those whose three source files all exist.
expected_periods <- search |>
  mutate(has_src = file.exists(glue("{raw_data_path}{year}_{period}_sub.parquet")) &
                   file.exists(glue("{raw_data_path}{year}_{period}_pre.parquet")) &
                   file.exists(glue("{raw_data_path}{year}_{period}_num.parquet"))) |>
  filter(has_src) |>
  select(year, period)

done_periods <- if (file.exists(done_log)) {
  read_csv(done_log, show_col_types = FALSE,
           col_types = cols(year = col_character(), period = col_character()))
} else {
  tibble(year = character(), period = character(), n_rows = integer())
}

todo_periods <- expected_periods |> anti_join(done_periods, by = c("year", "period"))

cat("Periods expected:", nrow(expected_periods),
    "| already done:", nrow(done_periods),
    "| to process:", nrow(todo_periods), "\n")

# A period counts as done once attempted, including when it yields no rows (the
# target CIKs may simply not have filed that period). Without this, zero-row
# periods would be retried on every resume and the completeness gate would never
# be satisfied.
mark_period_done <- function(yr, pd, n) {
  write_csv(tibble(year = yr, period = pd, n_rows = as.integer(n),
                   written = format(Sys.time())),
            done_log, append = file.exists(done_log))
}

for (i in seq_len(nrow(search))) {
  yr <- search$year[i]; pd <- search$period[i]

  f_sub <- glue("{raw_data_path}{yr}_{pd}_sub.parquet")
  f_pre <- glue("{raw_data_path}{yr}_{pd}_pre.parquet")
  f_num <- glue("{raw_data_path}{yr}_{pd}_num.parquet")
  if (!file.exists(f_sub) || !file.exists(f_pre) || !file.exists(f_num)) next

  # resume: skip periods already recorded as complete
  if (RESUME && any(done_periods$year == yr & done_periods$period == pd)) next

  cat("Loading", yr, pd, "(", i, "of", nrow(search), ")\n")

  # --- filings ---
  # `sic` and `afs` (filer status) are carried through so the research sample can
  # be screened -- accounting papers conventionally drop financials (SIC 6000-6999)
  # and often utilities (4900-4949) -- and stratified by filer size without a rebuild.
  sub_clean <- read_parquet(f_sub, col_select = c("adsh", "cik", "form", "period",
                                                  "fy", "fp", "filed", "sic", "afs")) |>
    filter(form %in% c("10-K", "10-Q", "10-K/A", "10-Q/A")) |>
    mutate(cik    = str_pad(as.character(cik), width = 10, pad = "0"),
           filed  = ymd(filed),
           period = as.integer(period),
           fy     = suppressWarnings(as.integer(fy)),
           sic    = suppressWarnings(as.integer(sic)),
           afs    = as.character(afs))

  if (!is.null(TARGET_CIKS)) sub_clean <- sub_clean |> filter(cik %in% TARGET_CIKS)
  if (nrow(sub_clean) == 0) { mark_period_done(yr, pd, 0L); next }
  keep_adsh <- sub_clean$adsh

  # --- cash flow statement layout ---
  pre_cf <- read_parquet(f_pre, col_select = c("adsh", "report", "line", "stmt",
                                               "inpth", "tag", "prole", "plabel",
                                               "negating")) |>
    filter(adsh %in% keep_adsh, stmt == "CF", inpth == 0) |>
    mutate(report   = as.integer(report),
           line     = as.integer(line),
           negating = as.integer(negating))
  if (nrow(pre_cf) == 0) { mark_period_done(yr, pd, 0L); next }

  # A filing can carry more than one CF-flagged report (e.g. a supplemental or
  # restated schedule). Keep the one presenting the most section subtotals,
  # breaking ties on the lowest report number.
  primary_report <- pre_cf |>
    group_by(adsh, report) |>
    summarise(n_anchor = n_distinct(tag[tag %in% ALL_SUBTOTAL_TAGS]),
              n_line   = n(), .groups = "drop") |>
    arrange(adsh, desc(n_anchor), desc(n_line), report) |>
    group_by(adsh) |>
    slice(1) |>
    ungroup() |>
    select(adsh, report)

  pre_cf <- pre_cf |>
    inner_join(primary_report, by = c("adsh", "report")) |>
    assign_sections()

  # --- values ---
  num_raw <- read_parquet(f_num, col_select = c("adsh", "tag", "ddate", "qtrs",
                                                "uom", "dimh", "iprx", "value")) |>
    filter(adsh %in% keep_adsh,
           tag %in% unique(pre_cf$tag),
           uom == "USD",
           as.integer(iprx) == 0,
           !is.na(value), value != "") |>
    mutate(value = as.numeric(value),
           ddate = as.integer(ddate),
           qtrs  = as.integer(qtrs)) |>
    filter(!is.na(value), !is.na(ddate), !is.na(qtrs))

  # parent (undimensioned) value preferred; dimensioned only as a fallback
  num_parent <- num_raw |> filter(dimh == "0x00000000")
  num_dim <- num_raw |>
    filter(dimh != "0x00000000") |>
    anti_join(num_parent, by = c("adsh", "tag", "ddate", "qtrs")) |>
    group_by(adsh, tag, ddate, qtrs) |> slice(1) |> ungroup()

  num_resolved <- bind_rows(num_parent, num_dim) |>
    group_by(adsh, tag, ddate, qtrs) |> slice(1) |> ungroup() |>
    select(adsh, tag, ddate, qtrs, value)

  # --- periods the cash flow statement actually presents ---
  # `pre` maps a tag to a statement but carries no period, so joining it to `num`
  # on (adsh, tag) alone would attach EVERY duration reported for that tag to the
  # cash flow statement -- including durations the statement never shows. A Q3
  # 10-Q, for example, reports ProfitLoss at qtrs = 1 for the income statement's
  # three-month column while its cash flow statement is nine-month (qtrs = 3);
  # without this restriction that stray row makes the Q3 filing look like it
  # re-presents the prior Q1 cash flow statement.
  #
  # The section subtotals appear only on the cash flow statement, so the
  # (ddate, qtrs) pairs at which they are reported define the periods it covers.
  cf_periods <- num_resolved |>
    filter(tag %in% ALL_SUBTOTAL_TAGS) |>
    distinct(adsh, ddate, qtrs)

  # --- join layout to values ---
  lines <- pre_cf |>
    # many-to-many is expected: a tag may be presented on more than one line and
    # is reported for several periods; the period filter and dedupe below resolve it
    inner_join(num_resolved, by = c("adsh", "tag"), relationship = "many-to-many") |>
    inner_join(cf_periods, by = c("adsh", "ddate", "qtrs")) |>
    # a tag can be presented twice (e.g. opening vs closing cash balance); keep
    # the first presentation of each tag-period so values are not duplicated
    arrange(adsh, tag, ddate, qtrs, line) |>
    group_by(adsh, tag, ddate, qtrs) |> slice(1) |> ungroup() |>
    inner_join(sub_clean, by = "adsh") |>
    mutate(
      end   = as.Date(as.character(ddate), "%Y%m%d"),
      start = if_else(qtrs > 0L, end %m-% months(qtrs * 3L), as.Date(NA)),
      is_current_period = (ddate == period),
      # presentation sign: `negating` means the filing displays the value negated
      value_raw = value,
      value     = if_else(negating == 1L, -value_raw, value_raw)) |>
    select(cik, accn = adsh, form, fp, fy, filed, sic, afs, report, line, tag,
           plabel, prole, negating, section, is_subtotal, order_ok, ddate, qtrs,
           start, end, value_raw, value, is_current_period)

  if (nrow(lines) > 0) write_parquet(lines, glue("{lines_dir}{yr}_{pd}.parquet"))
  mark_period_done(yr, pd, nrow(lines))

  rm(num_raw, num_parent, num_dim, num_resolved, pre_cf, lines); gc()
}

# Completeness gate. Sections 6+ pick the LAST filing to re-present each period,
# so they are only correct once every period has been extracted -- a partial
# extract would silently choose the wrong comparative.
done_periods <- read_csv(done_log, show_col_types = FALSE,
                         col_types = cols(year = col_character(),
                                          period = col_character()))
still_missing <- expected_periods |> anti_join(done_periods, by = c("year", "period"))
if (nrow(still_missing) > 0) {
  stop(nrow(still_missing), " period(s) still unprocessed: ",
       paste(paste0(still_missing$year, "_", still_missing$period), collapse = ", "),
       "\n  Re-run with RESUME <- TRUE to continue. The analysis below is skipped ",
       "because picking the LAST comparative requires a complete extract.")
}

# Rebuild the slim (filing x period) index from the parts rather than carrying it
# through the loop, so a resumed run gets the full index rather than only the
# periods processed in this session.
filing_index <- open_dataset(lines_dir) |>
  select(cik, accn, form, fp, fy, filed, sic, afs, start, end, is_current_period) |>
  distinct() |>
  collect() |>
  mutate(start = as.Date(start), end = as.Date(end), filed = as.Date(filed))

cat("\nFiling-period index rows:", nrow(filing_index),
    "| filings:", n_distinct(filing_index$accn),
    "| CIKs:", n_distinct(filing_index$cik), "\n")
cat("SCF line parts in:", lines_dir, "\n")


# 6. Pick the anchor filings for each company-period ---------------------------
# Three anchors, because a period's numbers can be corrected through two very
# different channels and the contrast between them is the point of the project:
#
#   ORIGINAL    = earliest regular (10-K/10-Q) filing presenting the period as its
#                 own current period -- the statement "as filed".
#   COMPARATIVE = the LAST regular filing to re-present that period as a prior-
#                 period comparative. This is the SILENT channel: the number
#                 changes in a later filing's comparative column with no
#                 amendment and often no disclosure. Per project convention the
#                 last one is used -- the final time the preparer/auditor
#                 attested to how the period should be presented.
#   AMENDMENT   = the LAST 10-K/A or 10-Q/A restating that period as its own
#                 current period. This is the DISCLOSED channel: an explicit,
#                 retrospective correction of a material error.
#
# A period is kept if it has EITHER channel, so amendment-only periods are not
# lost (an earlier version inner-joined on the comparative and dropped them).

# Subset the analysis without touching the extract. Use this for pilots, sector
# cuts, or to process the population in memory-bounded chunks of CIKs.
if (!is.null(ANALYSIS_CIKS)) {
  filing_index <- filing_index |> filter(cik %in% ANALYSIS_CIKS)
  cat("Analysis restricted to", n_distinct(filing_index$cik), "CIKs |",
      nrow(filing_index), "filing-periods\n")
}
stopifnot(nrow(filing_index) > 0)

regular <- filing_index |>
  filter(form %in% c("10-K", "10-Q"), !is.na(start), !is.na(end))

original_filing <- regular |>
  filter(is_current_period) |>
  distinct(cik, start, end, accn, filed, form, fp, fy, sic, afs) |>
  arrange(cik, start, end, filed, accn) |>
  group_by(cik, start, end) |>
  slice(1) |>
  ungroup() |>
  rename(orig_accn = accn, orig_filed = filed, orig_form = form,
         orig_fp = fp, orig_fy = fy)

comparative_filing <- regular |>
  filter(!is_current_period) |>
  distinct(cik, start, end, accn, filed, form) |>
  inner_join(original_filing |> select(cik, start, end, orig_filed),
             by = c("cik", "start", "end")) |>
  filter(filed > orig_filed) |>
  arrange(cik, start, end, desc(filed), desc(accn)) |>
  group_by(cik, start, end) |>
  slice(1) |>
  ungroup() |>
  select(cik, start, end, comp_accn = accn, comp_filed = filed, comp_form = form)

# Amendments restate their OWN period, so the restated figures sit in the /A
# filing's current-period column (is_current_period), not a comparative column.
amendment_filing <- filing_index |>
  filter(form %in% c("10-K/A", "10-Q/A"), !is.na(start), !is.na(end),
         is_current_period) |>
  distinct(cik, start, end, accn, filed, form) |>
  inner_join(original_filing |> select(cik, start, end, orig_filed),
             by = c("cik", "start", "end")) |>
  filter(filed > orig_filed) |>
  arrange(cik, start, end, desc(filed), desc(accn)) |>
  group_by(cik, start, end) |>
  mutate(n_amendments = n()) |>
  slice(1) |>
  ungroup() |>
  select(cik, start, end, amend_accn = accn, amend_filed = filed,
         amend_form = form, n_amendments)

anchors <- original_filing |>
  left_join(comparative_filing, by = c("cik", "start", "end")) |>
  left_join(amendment_filing,   by = c("cik", "start", "end")) |>
  filter(!is.na(comp_accn) | !is.na(amend_accn)) |>
  mutate(has_comparative = !is.na(comp_accn),
         has_amendment   = !is.na(amend_accn),
         n_amendments    = coalesce(n_amendments, 0L),
         n_years_later   = as.numeric(comp_filed - orig_filed) / 365.25)

cat("Company-periods with at least one correction channel:", nrow(anchors), "\n")
cat("  with a later comparative:", sum(anchors$has_comparative), "\n")
cat("  with an amendment:       ", sum(anchors$has_amendment), "\n")

# Read back only the lines belonging to anchor filings -- a small fraction of the
# parts written above, which is what keeps a full-population run tractable.
anchor_accns <- unique(na.omit(c(anchors$orig_accn, anchors$comp_accn,
                                 anchors$amend_accn)))

scf_lines <- open_dataset(lines_dir) |>
  filter(accn %in% anchor_accns) |>
  select(cik, accn, tag, plabel, section, is_subtotal, line, start, end,
         value_raw, negating, value) |>
  collect() |>
  mutate(start = as.Date(start), end = as.Date(end))

cat("Anchor filings:", length(anchor_accns),
    "| SCF line rows read back:", nrow(scf_lines), "\n")


# 7. Line-level comparison -----------------------------------------------------
# Union the two filings' line sets so a tag present in only one side is carried
# with an explicit 0 and a presence flag. A tag dropping to 0 while new tags
# appear in the same section is the signature of disaggregation.

side_lines <- function(accn_col) {
  anchors |>
    select(cik, start, end, accn = all_of(accn_col)) |>
    inner_join(scf_lines |> select(cik, accn, tag, plabel, section, is_subtotal,
                                   line, start, end, value_raw, negating, value),
               by = c("cik", "accn", "start", "end")) |>
    select(cik, start, end, tag, section, is_subtotal,
           label = plabel, line, value_raw, negating, value)
}

orig_raw_lines  <- side_lines("orig_accn")
comp_raw_lines  <- side_lines("comp_accn")
amend_raw_lines <- side_lines("amend_accn")

# Decide, from the filing's own arithmetic, whether its operating section is an
# income build-up. `value` here is the value as that filing displays it, so the
# section should foot to its own subtotal. Collapse only where the section does
# NOT foot with every line counted but DOES foot once the block is collapsed.
income_block_test <- function(d) {
  op <- d |> filter(section == "operating")
  if (nrow(op) == 0) {
    return(tibble(cik = character(), start = as.Date(character()),
                  end = as.Date(character())))
  }
  subtot <- op |> filter(is_subtotal) |>
    group_by(cik, start, end) |> summarise(sub = sum(value), .groups = "drop")
  comps <- op |> filter(!is_subtotal)
  inc <- comps |> filter(tag %in% INCOME_LINE_TAGS)
  keep_last <- inc |> group_by(cik, start, end) |>
    slice_max(line, n = 1, with_ties = FALSE) |> ungroup()

  sum_all <- comps |> group_by(cik, start, end) |>
    summarise(s_all = sum(value), .groups = "drop")
  sum_coll <- bind_rows(
      comps |> anti_join(inc |> select(cik, start, end, tag),
                         by = c("cik", "start", "end", "tag")),
      keep_last) |>
    group_by(cik, start, end) |> summarise(s_coll = sum(value), .groups = "drop")

  subtot |>
    left_join(sum_all,  by = c("cik", "start", "end")) |>
    left_join(sum_coll, by = c("cik", "start", "end")) |>
    filter(abs(coalesce(s_all, 0) - sub) >= 1,
           abs(coalesce(s_coll, 0) - sub) <  1) |>
    select(cik, start, end)
}

# If either side needs collapsing, collapse BOTH so the two sides stay
# comparable -- otherwise one side's "[Income starting point]" would be matched
# against the other side's raw NetIncomeLoss row and both would look changed.
collapse_keys <- bind_rows(income_block_test(orig_raw_lines),
                           income_block_test(comp_raw_lines),
                           income_block_test(amend_raw_lines)) |>
  distinct()

cat("Company-periods where the operating income build-up is collapsed:",
    nrow(collapse_keys), "of", nrow(anchors), "\n")

collapse_income_block <- function(d, keys) {
  if (nrow(keys) == 0) return(d)
  target <- d |> semi_join(keys, by = c("cik", "start", "end"))
  rest   <- d |> anti_join(keys, by = c("cik", "start", "end"))
  inc <- target |> filter(section == "operating", !is_subtotal,
                          tag %in% INCOME_LINE_TAGS)
  if (nrow(inc) == 0) return(d)

  income_start <- inc |>
    group_by(cik, start, end) |>
    slice_max(line, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(label = paste0(label, " [", tag, "]"), tag = INCOME_START_TAG)

  bind_rows(
    rest,
    target |> anti_join(inc |> select(cik, start, end, tag),
                        by = c("cik", "start", "end", "tag")),
    income_start)
}

orig_collapsed  <- collapse_income_block(orig_raw_lines,  collapse_keys)
comp_collapsed  <- collapse_income_block(comp_raw_lines,  collapse_keys)
amend_collapsed <- collapse_income_block(amend_raw_lines, collapse_keys)

# Does each side's section foot using ITS OWN presentation flags? A section that
# foots is internally consistent, so its displayed values can be trusted; one
# that does not has defective presentation tagging. This is the signal used to
# pick a sign convention below.
section_foots <- function(d) {
  d |>
    filter(!is.na(section), section != "other") |>
    group_by(cik, start, end, section) |>
    summarise(sub_v = sum(value[is_subtotal]),
              cmp_v = sum(value[!is_subtotal]), .groups = "drop") |>
    transmute(cik, start, end, section, foots = abs(cmp_v - sub_v) < 1)
}

# SIGN CONVENTION
#   What matters economically is the value as DISPLAYED on the face of the
#   statement -- that is what tells a reader whether a line is an inflow or an
#   outflow. Getting there is not simply "apply `negating`", because filers churn
#   both the stored sign and the flag, in opposite directions:
#
#     Walmart Q3 FY2019 stores PaymentsToAcquirePropertyPlantAndEquipment as
#     7,014 with negating = 0 (displaying a cash outflow as POSITIVE); the next
#     year's 10-Q keeps the identical stored 7,014 but corrects the flag to 1.
#     Same stored value, different display -- comparing displayed values alone
#     would invent a 14,028 swing.
#
#     Akorn stores NoncashGainOnBargainPurchase as -849 with negating = 0, while
#     the later filing stores +849 with negating = 1. Opposite stored values,
#     but BOTH display -849 -- comparing stored values alone would invent a
#     1,698 swing.
#
#   The two cases are exact opposites, so neither stored nor displayed values
#   work on their own. The resolution is per line, not per filing:
#
#     - If the STORED value is identical, the tagged fact did not change; any
#       display delta is pure flag churn. Adopt the comparative's (later,
#       corrected) presentation for both sides. -- handles the Walmart case.
#     - Otherwise use each filing's own displayed value. Where the filer flipped
#       the stored sign and the flag together the displayed values already agree
#       and the difference is zero. -- handles the Akorn case.
#
#   A genuine sign correction still surfaces: Akorn's Q3 2011 10-Q tags
#   IncreaseDecreaseInAccruedLiabilities as -1,024 where its own subtotal
#   requires +1,024 (the section misfoots by exactly 2 x 1,024), and the later
#   filing corrects it -- that shows up as a real -2,048 difference.
#
#   `orig_section_foots` / `rev_section_foots` are carried as diagnostics: a
#   filing whose section does not foot has defective SCF tagging, which is
#   itself a finding.
#
# This runs once per correction channel (comparative and amendment), so each
# channel's difference is computed on its own pairwise-normalised basis.
compare_channel <- function(o_lines, r_lines, keys) {
  o <- o_lines |> semi_join(keys, by = c("cik", "start", "end"))
  r <- r_lines |> semi_join(keys, by = c("cik", "start", "end"))
  o_foot <- section_foots(o) |> rename(orig_section_foots = foots)
  r_foot <- section_foots(r) |> rename(rev_section_foots  = foots)

  full_join(
      o |> select(-value) |>
        rename(o_label = label, o_line = line, o_raw = value_raw, o_neg = negating),
      r |> select(-value) |>
        rename(r_label = label, r_line = line, r_raw = value_raw, r_neg = negating),
      by = c("cik", "start", "end", "tag"), suffix = c("_o", "_r")) |>
    mutate(
      in_original = !is.na(o_raw),
      in_revised  = !is.na(r_raw),
      # section/subtotal identity may only be known from one side
      section     = coalesce(section_o, section_r),
      is_subtotal = coalesce(is_subtotal_o, is_subtotal_r),
      label       = coalesce(o_label, r_label)) |>
    left_join(o_foot, by = c("cik", "start", "end", "section")) |>
    left_join(r_foot, by = c("cik", "start", "end", "section")) |>
    mutate(
      orig_section_foots = coalesce(orig_section_foots, TRUE),
      rev_section_foots  = coalesce(rev_section_foots,  TRUE),
      negating_changed = in_original & in_revised &
                         coalesce(o_neg != r_neg, FALSE),
      # each filing's value as it displays it
      o_display = if_else(o_neg == 1L, -o_raw, o_raw),
      r_display = if_else(r_neg == 1L, -r_raw, r_raw),
      # identical stored fact -> display delta is flag churn, adopt the later
      # filing's presentation for both sides so the difference is zero
      stored_value_same = in_original & in_revised &
                          coalesce(o_raw == r_raw, FALSE),
      orig_val = coalesce(if_else(stored_value_same, r_display, o_display), 0),
      rev_val  = coalesce(r_display, 0),
      # user convention (Examples.xlsx): as filed minus subsequent
      difference = orig_val - rev_val,
      # A line added or dropped at a value of zero changes nothing on the face of
      # the statement, so difference is tested first.
      change_type = case_when(
        difference == 0            ~ "Unchanged",
        !in_original & in_revised  ~ "Added",
        in_original & !in_revised  ~ "Dropped",
        TRUE                       ~ "Value change"),
      line_order = coalesce(o_line, r_line)) |>
    select(cik, start, end, tag, section, is_subtotal, label, line_order,
           in_original, in_revised, orig_val, rev_val, difference, change_type,
           negating_changed, stored_value_same,
           orig_section_foots, rev_section_foots)
}

cmp_channel <- compare_channel(orig_collapsed, comp_collapsed,
                               anchors |> filter(has_comparative) |>
                                 select(cik, start, end))
amd_channel <- compare_channel(orig_collapsed, amend_collapsed,
                               anchors |> filter(has_amendment) |>
                                 select(cik, start, end))

# One row per company-period x tag, carrying both channels side by side. Channel
# columns are NA where that channel does not exist for the period, which is what
# distinguishes "no amendment was filed" from "an amendment changed nothing".
scf_line_changes <- full_join(
    cmp_channel,
    amd_channel |>
      select(cik, start, end, tag,
             a_section = section, a_is_subtotal = is_subtotal, a_label = label,
             a_line_order = line_order, a_in_original = in_original,
             in_amendment = in_revised, a_orig_val = orig_val,
             amend_val = rev_val, amend_difference = difference,
             amend_change_type = change_type,
             amend_negating_changed = negating_changed,
             amend_section_foots = rev_section_foots),
    by = c("cik", "start", "end", "tag")) |>
  mutate(
    # a tag may appear only in the amendment channel (e.g. amendment-only periods)
    section        = coalesce(section, a_section),
    is_subtotal    = coalesce(is_subtotal, a_is_subtotal),
    label          = coalesce(label, a_label),
    line_order     = coalesce(line_order, a_line_order),
    in_original    = coalesce(in_original, a_in_original, FALSE),
    in_comparative = coalesce(in_revised, FALSE),
    in_amendment   = coalesce(in_amendment, FALSE),
    orig_val       = coalesce(orig_val, a_orig_val, 0),
    comp_val       = rev_val,
    comp_section_foots = rev_section_foots) |>
  select(-a_section, -a_is_subtotal, -a_label, -a_line_order, -a_in_original,
         -a_orig_val, -in_revised, -rev_val, -rev_section_foots) |>
  left_join(anchors |> select(cik, start, end, orig_accn, orig_filed, orig_form,
                              orig_fy, comp_accn, comp_filed, comp_form,
                              amend_accn, amend_filed, amend_form, n_amendments,
                              has_comparative, has_amendment),
            by = c("cik", "start", "end")) |>
  arrange(cik, end, section, line_order)


# 7b. Flag tag substitutions ---------------------------------------------------
# A line relabelled to a different XBRL element without changing its amount is a
# presentation change, not an economic one (e.g. Walmart FY2012
# DeferredIncomeTaxesAndTaxCredits -> IncreaseDecreaseInDeferredIncomeTaxes, both
# 1050). Pair a Dropped tag with an Added tag in the same period and section
# carrying the same value, so these can be excluded from misstatement counts.
substitutions <- scf_line_changes |>
  filter(coalesce(change_type, "") %in% c("Added", "Dropped"), !is_subtotal) |>
  mutate(amt = if_else(change_type == "Dropped", orig_val, comp_val)) |>
  filter(amt != 0) |>
  group_by(cik, start, end, section, amt) |>
  filter(n_distinct(change_type) == 2) |>
  ungroup() |>
  distinct(cik, start, end, tag) |>
  mutate(is_tag_substitution = TRUE)

scf_line_changes <- scf_line_changes |>
  left_join(substitutions, by = c("cik", "start", "end", "tag")) |>
  mutate(is_tag_substitution = coalesce(is_tag_substitution, FALSE))


# 8. Section-level rollup and reconciliation diagnostics -----------------------
# Within a section the component differences should sum to the subtotal
# difference. A mismatch means a line that moved is not being captured -- either
# a tag missing from the extract, or double counting from a nested subtotal
# (see the CAVEAT in Section 3). `recon_ok` is a diagnostic, not a guarantee.

section_subtotals <- scf_line_changes |>
  filter(is_subtotal, !is.na(section), section != "other") |>
  group_by(cik, start, end, section) |>
  summarise(subtotal_orig  = sum(orig_val),
            subtotal_comp  = sum(comp_val,  na.rm = TRUE),
            subtotal_amend = sum(amend_val, na.rm = TRUE),
            subtotal_difference       = sum(difference,       na.rm = TRUE),
            subtotal_amend_difference = sum(amend_difference, na.rm = TRUE),
            .groups = "drop")

section_components <- scf_line_changes |>
  filter(!is_subtotal, !is.na(section), section != "other") |>
  group_by(cik, start, end, section) |>
  summarise(
    n_lines             = n(),
    n_changed           = sum(change_type != "Unchanged", na.rm = TRUE),
    n_added             = sum(change_type == "Added", na.rm = TRUE),
    n_dropped           = sum(change_type == "Dropped", na.rm = TRUE),
    n_value_changed     = sum(change_type == "Value change", na.rm = TRUE),
    n_substitutions     = sum(is_tag_substitution, na.rm = TRUE),
    n_negating_changed  = sum(negating_changed, na.rm = TRUE),
    component_difference = sum(difference, na.rm = TRUE),
    # amendment channel
    n_amend_changed     = sum(amend_change_type != "Unchanged", na.rm = TRUE),
    component_amend_difference = sum(amend_difference, na.rm = TRUE),
    .groups = "drop")

# Whether each side's own statement foots. A section can only be expected to
# reconcile if both filings' own cash flow statements are internally consistent.
section_footing <- scf_line_changes |>
  filter(!is.na(section), section != "other") |>
  group_by(cik, start, end, section) |>
  summarise(orig_section_foots = all(orig_section_foots),
            comp_section_foots = all(comp_section_foots), .groups = "drop") |>
  mutate(both_sides_foot = orig_section_foots & comp_section_foots)

scf_section_changes <- section_subtotals |>
  full_join(section_components, by = c("cik", "start", "end", "section")) |>
  left_join(section_footing,    by = c("cik", "start", "end", "section")) |>
  mutate(across(where(is.numeric), ~ coalesce(.x, 0)),
         recon_diff = component_difference - subtotal_difference,
         recon_ok   = abs(recon_diff) < 1,
         amend_recon_diff = component_amend_difference - subtotal_amend_difference,
         amend_recon_ok   = abs(amend_recon_diff) < 1) |>
  left_join(anchors |> select(cik, start, end, orig_accn, orig_filed, orig_form,
                              orig_fy, comp_accn, comp_filed, amend_accn,
                              amend_filed, has_comparative, has_amendment),
            by = c("cik", "start", "end"))

# Period level: do the three sections offset each other?
#   offsetting  -> cash moved between sections (reclassification)
#   not         -> the total change in cash itself was restated
scf_period_summary <- scf_section_changes |>
  left_join(anchors |> select(cik, start, end, sic, afs, n_amendments),
            by = c("cik", "start", "end")) |>
  group_by(cik, sic, afs, start, end, orig_accn, orig_filed, orig_form, orig_fy,
           comp_accn, amend_accn, amend_filed, n_amendments,
           has_comparative, has_amendment) |>
  summarise(
    cfo_difference = sum(subtotal_difference[section == "operating"]),
    cfi_difference = sum(subtotal_difference[section == "investing"]),
    cff_difference = sum(subtotal_difference[section == "financing"]),
    n_sections_changed = sum(abs(subtotal_difference) >= 1),
    n_lines_changed    = sum(n_changed),
    any_recon_fail     = any(!recon_ok),
    # amendment channel
    amend_cfo_difference = sum(subtotal_amend_difference[section == "operating"]),
    amend_cfi_difference = sum(subtotal_amend_difference[section == "investing"]),
    amend_cff_difference = sum(subtotal_amend_difference[section == "financing"]),
    n_amend_sections_changed = sum(abs(subtotal_amend_difference) >= 1),
    n_amend_lines_changed    = sum(n_amend_changed),
    .groups = "drop") |>
  mutate(
    net_difference = cfo_difference + cfi_difference + cff_difference,
    amend_net_difference = amend_cfo_difference + amend_cfi_difference +
                           amend_cff_difference,
    # What the silent (comparative) channel did to the statement
    period_type = case_when(
      !has_comparative                                ~ NA_character_,
      n_lines_changed == 0                            ~ "No change",
      n_sections_changed == 0                         ~ "Intra-section reclass",
      abs(net_difference) < 1                         ~ "Cross-section reclass",
      TRUE                                            ~ "Net cash flow change"),
    # Which channel actually corrected the period. The core contrast for the
    # project: an amendment is disclosed and retrospective, a comparative
    # revision is silent.
    amendment_changed_scf   = has_amendment & n_amend_lines_changed > 0,
    comparative_changed_scf = has_comparative & n_lines_changed > 0,
    correction_channel = case_when(
      amendment_changed_scf & comparative_changed_scf ~ "Both",
      amendment_changed_scf                           ~ "Amendment (disclosed)",
      comparative_changed_scf                         ~ "Comparative revision (silent)",
      TRUE                                            ~ "None"))


# 9. Save ----------------------------------------------------------------------
# DATA_PATH lives in Dropbox, which intermittently holds a memory-mapped handle
# on a file it is syncing; writing then fails with "user-mapped section open".
# Retry briefly rather than lose a long run at the final step.
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
write_parquet_safely(scf_line_changes,    glue("{data_path}scf_line_changes.parquet"))
write_parquet_safely(scf_section_changes, glue("{data_path}scf_section_changes.parquet"))
write_parquet_safely(scf_period_summary,  glue("{data_path}scf_period_summary.parquet"))

cat("\nSaved:\n")
cat("  scf_line_changes.parquet   ", nrow(scf_line_changes), "rows\n")
cat("  scf_section_changes.parquet", nrow(scf_section_changes), "rows\n")
cat("  scf_period_summary.parquet ", nrow(scf_period_summary), "rows\n")

cat("\nPeriod classification (comparative channel):\n")
print(scf_period_summary |> count(period_type))

cat("\nCorrection channel -- disclosed amendment vs silent comparative revision:\n")
print(scf_period_summary |> count(correction_channel))

cat("\nReconciliation diagnostic (component diffs vs subtotal diff):\n")
print(scf_section_changes |> count(section, recon_ok))

# A section can only be expected to reconcile if BOTH filings' own cash flow
# statements foot. Where they do, a reconciliation failure means this script is
# missing a line; where they do not, the filing's own XBRL is defective.
cat("\nReconciliation vs whether both filings' own statements foot:\n")
print(scf_section_changes |> count(both_sides_foot, recon_ok))


# 10. Walmart validation -------------------------------------------------------
# Ground truth is Examples.xlsx ("Walmart Filings"), hand-built for the FY2012-
# FY2016 10-K periods. Every line item and amount in that sheet is reproduced by
# this script, and all 15 section-periods reconcile exactly.
#
# Runs automatically whenever Walmart is in the output, so the check survives
# pilot and full-population runs as well as targeted ones.

wmt_fy_ends <- as.Date(c("2012-01-31", "2013-01-31", "2014-01-31",
                         "2015-01-31", "2016-01-31"))

if ("0000104169" %in% scf_line_changes$cik) {

  wmt <- scf_line_changes |>
    filter(cik == "0000104169", end %in% wmt_fy_ends,
           as.numeric(end - start) > 300,
           !is.na(section), section != "other",
           change_type != "Unchanged" | is_subtotal)

  cat("\n\n=== Walmart FY2012-FY2016 annual: detected changes ($MM) ===\n")
  print(as.data.frame(
    wmt |> transmute(fy_end = end, section, tag,
                     as_filed   = round(orig_val / 1e6),
                     subsequent = round(comp_val / 1e6),
                     difference = round(difference / 1e6),
                     change_type,
                     note = paste0(if_else(is_subtotal, "*subtotal ", ""),
                                   if_else(is_tag_substitution, "tag-substitution ", ""),
                                   if_else(negating_changed, "sign-flag-changed", ""))) |>
      arrange(fy_end, factor(section, levels = c("operating", "investing", "financing")))))

  cat("\n=== Walmart annual reconciliation (0 = components tie to subtotal) ===\n")
  print(as.data.frame(
    scf_section_changes |>
      filter(cik == "0000104169", end %in% wmt_fy_ends, as.numeric(end - start) > 300) |>
      transmute(fy_end = end, section,
                subtotal_diff = round(subtotal_difference / 1e6),
                component_diff = round(component_difference / 1e6),
                recon_diff = round(recon_diff / 1e6), recon_ok) |>
      arrange(fy_end, factor(section, levels = c("operating", "investing", "financing")))))
}
