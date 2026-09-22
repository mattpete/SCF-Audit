# =============================================================================
# 002a - Separate standard-driven SCF changes from genuine revisions
# =============================================================================
# PURPOSE (Phase 2 of project roadmap -- see CLAUDE.md)
#   001e detects every change between the SCF as originally filed and as later
#   re-presented. Some of those changes are not errors at all: the FASB required
#   them. This script attributes the attributable ones to specific accounting
#   standards, sums the amount each standard accounts for within a section, and
#   subtracts it -- leaving an UNEXPLAINED residual per section that is the
#   candidate revision.
#
#     unexplained_difference = subtotal_difference - standard_difference
#
#   A section whose entire movement is explained by an ASU is not evidence of
#   anything; a section with a material unexplained residual is.
#
# WHY ATTRIBUTION IS PER-STANDARD AND PER-IMPACT, NOT ONE BOOLEAN
#   The four standards that matter here threaten DIFFERENT outcome variables
#   (measured on the full population -- see the table in CLAUDE.md):
#     - ASU 2016-09 moves cash BETWEEN sections (32.6% of affected periods are
#       cross-section reclassifications, triple any other standard). It is the
#       direct threat to a reclassification result.
#     - ASU 2016-18 changes the TOTAL change in cash (62.4% net-change), not its
#       classification.
#     - ASC 606 and ASC 842 are predominantly INTRA-section (59% / 71%). They
#       contaminate tag-level descriptive counts far more than section results.
#   So each flagged row carries which standard and what kind of impact, letting
#   an exclusion be applied per outcome variable rather than discarding periods
#   that are harmless to the test at hand.
#
# WHY THE WINDOWS ARE NOT THE EFFECTIVE DATES
#   Adoption generally restates the COMPARATIVES, and 001e compares a period as
#   originally filed against that same period as shown in the latest filing. So
#   the signal lands on the periods whose comparatives were restated -- typically
#   the years BEFORE adoption, not the adoption year. Excess tax benefits spike
#   in FY2015, a year before ASU 2016-09 was issued, because firms adopting in
#   FY2017 retrospectively restated FY2015-16. Windows below therefore run from
#   roughly (adoption - 2) and are anchored on OBSERVED spike years, with the
#   nominal effective date given for reference.
#
# INPUT   {data_path}scf_line_changes{IN_SUFFIX}.parquet
#         {data_path}scf_section_changes{IN_SUFFIX}.parquet
# OUTPUT  {data_path}scf_line_changes_flagged{IN_SUFFIX}.parquet
#         {data_path}scf_section_adjusted{IN_SUFFIX}.parquet
#         {data_path}scf_period_adjusted{IN_SUFFIX}.parquet
# =============================================================================


# 1. Setup ---------------------------------------------------------------------
library(dplyr)
library(stringr)
library(arrow)
library(glue)
library(tidyr)
library(lubridate)
library(readr)

data_path <- Sys.getenv('DATA_PATH')
source("code/000-utilities-functions.R")


# 2. Configuration -------------------------------------------------------------

# Which 001e outputs to read. "_full" is the full-population build; "" is a
# targeted build (e.g. the Walmart/Akorn validation pair).
IN_SUFFIX <- "_full"

# A section's movement counts as "materially unexplained" when the residual left
# after removing standard-driven amounts exceeds this, in dollars. XBRL amounts
# are exact, but filings are rounded to thousands, so anything under one
# reporting unit is noise.
MATERIALITY_FLOOR <- 1000

# Periods at or before this fiscal year sit in the XBRL phase-in era, when tag
# churn reflects filers being onboarded to XBRL and the taxonomy being cleaned
# up rather than any accounting change (FY2011 taxes/impairment, FY2012 stock
# options/business combinations show as large spikes). Flagged SEPARATELY from
# FASB-driven changes, not lumped in: "the taxonomy changed" is a weaker
# exclusion than "the FASB required this", and the two should be reportable
# apart. Whether to exclude these at all is still an open decision.
EARLY_XBRL_FY_MAX <- 2012

# The panel's right edge is censored: the extract stops at 2023_M12, so recent
# periods have fewer later filings available to serve as comparatives. Tags
# appear to "die" in 2022 (ProceedsFromIssuanceInitialPublicOffering -58%,
# PrivatePlacement -57%) purely for this reason. Periods at or after this year
# should be treated as incompletely observed rather than unchanged.
CENSORED_FY_MIN <- 2022


# 3. Standards reference table -------------------------------------------------
# One row per standard. `tag_pattern` is a regex matched against the XBRL tag
# name; `fy_from`/`fy_to` bound the fiscal years in which a change to such a tag
# is treated as standard-driven; `impact` records which outcome variable the
# standard threatens, as measured on the full population.
#
# -----------------------------------------------------------------------------
# ASU 2016-09  Improvements to Employee Share-Based Payment Accounting
#   WHAT IT DID   Excess tax benefits from share-based compensation had been
#                 split out: shown as an operating outflow and an equal
#                 financing inflow. The ASU eliminated that split, recognising
#                 the benefit in income tax expense and leaving it in operating.
#   SCF IMPACT    Cash moves BETWEEN sections -- financing to operating -- with
#                 no change to the total change in cash. This is the classic
#                 confound for a reclassification result, and the one standard
#                 here that genuinely relocates cash.
#   EFFECTIVE     Fiscal years beginning after 2016-12-15 (FY2017 for calendar
#                 filers); early adoption permitted, and widely taken in 2016.
#   OBSERVED      Change rate peaks FY2015-FY2016 (85% of affected sections move
#                 in FY2016), collapsing after FY2017 as the tags are retired
#                 (1,298 users in FY2016 -> 42 by FY2020).
#   MEASURED      7,103 firm-periods; section moves 58.8% of the time and the tag
#                 is the SOLE driver (median share of the movement = 1.00);
#                 32.6% of affected periods are cross-section.
# -----------------------------------------------------------------------------
# ASU 2016-18  Restricted Cash (Statement of Cash Flows, Topic 230)
#   WHAT IT DID   Required restricted cash and restricted cash equivalents to be
#                 included in the beginning and ending cash totals. Transfers
#                 between restricted and unrestricted cash therefore stopped
#                 being investing (or operating) activities at all.
#   SCF IMPACT    Changes the TOTAL change in cash, because the definition of
#                 the cash being reconciled widened. Also removes lines from
#                 investing. Threatens net-change measures more than
#                 classification measures.
#   EFFECTIVE     Fiscal years beginning after 2017-12-15 (FY2018); retrospective
#                 application required.
#   OBSERVED      Rate climbs to 0.79 by FY2017, then the tags are abandoned
#                 (IncreaseDecreaseInRestrictedCash users -80% in FY2018).
#   MEASURED      9,836 firm-periods; section moves 61.9%, median share 1.00;
#                 62.4% of affected periods are net-cash-flow changes.
# -----------------------------------------------------------------------------
# ASC 606 / ASU 2014-09  Revenue from Contracts with Customers
#   WHAT IT DID   Replaced the deferred-revenue model with contract assets and
#                 contract liabilities, and renamed the corresponding working
#                 capital lines.
#   SCF IMPACT    Mostly a RENAME within operating: deferred revenue becomes
#                 contract liability, both operating working-capital lines. It
#                 is the largest source of tag churn in the data, but it rarely
#                 moves a section subtotal.
#   EFFECTIVE     Fiscal years beginning after 2017-12-15 for public filers
#                 (FY2018); full or modified retrospective transition.
#   OBSERVED      Deferred-revenue change rate 0.26 -> 0.67 by FY2017;
#                 IncreaseDecreaseInContractWithCustomerLiability born from 1
#                 user to 1,099. Largest single wave detected.
#   MEASURED      15,586 firm-periods, but the section moves only 31.8% of the
#                 time and the tag explains just half of it (median share 0.50);
#                 59.4% of affected periods are intra-section.
# -----------------------------------------------------------------------------
# ASC 842 / ASU 2016-02  Leases
#   WHAT IT DID   Brought operating leases onto the balance sheet and renamed
#                 capital leases to finance leases, introducing right-of-use
#                 asset amortisation to the operating reconciliation.
#   SCF IMPACT    Largely terminology at the section level -- finance-lease
#                 principal sits in financing before and after, operating lease
#                 payments remain operating. But it touches more firm-periods
#                 than any other standard here, so its ABSOLUTE contamination is
#                 the largest despite the low per-change rate.
#   EFFECTIVE     Fiscal years beginning after 2018-12-15 (FY2019); modified
#                 retrospective, and many filers did not restate comparatives.
#   OBSERVED      "lease" is the single biggest concept spike in the dataset
#                 (1,070 excess firms across 1,400 tags in FY2020);
#                 RepaymentsOfLongTermCapitalLeaseObligations drains 651 -> 252
#                 users while FinanceLeasePrincipalPayments grows 1 -> 713. The
#                 spike sits AFTER adoption, unlike the others, consistent with
#                 comparatives not being restated on transition.
#   MEASURED      29,560 firm-periods (4x ASU 2016-09); section moves only 25.2%
#                 but that still yields the most threatening periods (8,618);
#                 70.8% intra-section.
#   CAVEAT        The pattern "Lease" is deliberately broad (3,163 tags) and is
#                 relied on being paired with a narrow window. It will catch
#                 ordinary lease-line revisions inside FY2018-2021, so it is the
#                 most over-inclusive entry in this table -- tighten it first if
#                 the residual looks suspiciously small.
# -----------------------------------------------------------------------------
# ASU 2016-01  Financial Instruments -- Recognition and Measurement
#   WHAT IT DID   Split available-for-sale securities into debt and equity
#                 classes, with new tags carrying explicit Debt/Equity suffixes.
#   SCF IMPACT    A rename within investing; both old and new lines are investing
#                 activities.
#   EFFECTIVE     Fiscal years beginning after 2017-12-15 (FY2018).
#   OBSERVED      PaymentsToAcquireAvailableForSaleSecuritiesDebt and
#                 ProceedsFromSaleOfAvailableForSaleSecuritiesDebt appear in
#                 FY2017 (206 and 135 excess firms).
#   NOTE          Attribution here is inferred from the tag-name split and the
#                 timing, not from reading transition guidance, so it is the
#                 least certain entry. It is included because leaving a known
#                 FY2017 wave unattributed would push it into the residual.
# -----------------------------------------------------------------------------
# ASU 2016-15  Classification of Certain Cash Receipts and Cash Payments
#   DELIBERATELY ABSENT. It targeted eight specific classification issues and
#   would be the most obvious candidate for this table, but the detector found
#   NO wave in any of its tags: debt extinguishment costs, equity-method
#   distributions, life-insurance proceeds and divestiture receipts all run flat
#   or declining through the adoption window. Either the affected populations are
#   too small to register or the reclassifications land in tags that do not move.
#   Adding it on theory alone would remove genuine revisions with no evidence.
# -----------------------------------------------------------------------------

# Windows are set TIGHTLY on the observed spike, not generously around the
# effective date. Two constraints bound how early a window can start:
#   - a standard cannot explain a change in a period that precedes its issuance;
#   - a period only gets restated by an adoption-year filing if it still appears
#     as a comparative in it, and a 10-K presents three years of cash flows. So
#     an FY2017 adopter can restate FY2015-16, but not FY2014.
# Slack here is expensive: a wide window swallows genuine restatements that
# happen to touch the same tags (see the Akorn note below).
STANDARDS <- tribble(
  ~standard,     ~source, ~impact,         ~fy_from, ~fy_to, ~effective_fy, ~tag_pattern,
  "ASU 2016-09", "FASB",  "cross_section",    2015L,  2017L,        2017L,
    "^ExcessTaxBenefitFromShareBasedCompensation",
  "ASU 2016-18", "FASB",  "net_change",       2016L,  2018L,        2018L,
    "RestrictedCash",
  "ASC 606",     "FASB",  "intra_section",    2016L,  2019L,        2018L,
    "ContractWithCustomer|ContractLiability|DeferredRevenue",
  "ASC 842",     "FASB",  "intra_section",    2018L,  2021L,        2019L,
    "Lease",
  "ASU 2016-01", "FASB",  "intra_section",    2016L,  2019L,        2018L,
    "AvailableForSaleSecurities"
)

# IMPORTANT -- patterns must match BOTH SIDES of a rename.
# A standard that renames a line produces two offsetting changes: the old tag
# falls to zero and the new tag appears. If the pattern catches only one side,
# the other stays unattributed and the pair no longer nets to zero, so
# subtracting the attributed amount MANUFACTURES a residual in a section that
# never moved. This is why ASU 2016-01 matches "AvailableForSaleSecurities"
# rather than only the new "...Debt"/"...Equity" variants, and why the ASC 606
# pattern lists DeferredRevenue alongside the contract-liability tags. The cost
# is breadth -- these patterns also catch ordinary revisions to those lines
# inside the window -- which is the deliberate trade against the windows being
# narrow.

# Order matters: a tag matching more than one pattern is attributed to the first
# row that matches, so the more specific patterns are listed above "Lease".
STANDARDS <- STANDARDS |> mutate(priority = row_number())

cat("Standards in reference table:", nrow(STANDARDS), "\n")
print(STANDARDS |> select(standard, impact, fy_from, fy_to, effective_fy))


# 4. Load 001e output ----------------------------------------------------------
lc <- read_parquet(glue("{data_path}scf_line_changes{IN_SUFFIX}.parquet"))
sc <- read_parquet(glue("{data_path}scf_section_changes{IN_SUFFIX}.parquet"))

cat("\nline-change rows:", format(nrow(lc), big.mark = ","),
    "| sections:", format(nrow(sc), big.mark = ","), "\n")

# fiscal year of the period being examined; orig_fy is the filer's own label,
# falling back to the calendar year of the period end
lc <- lc |> mutate(fy = coalesce(orig_fy, year(end)))


# 5. Attribute changed line items to standards ---------------------------------
# Only COMPONENT lines are attributed. Subtotal rows are the thing being
# explained, so attributing them would double count.
components <- lc |>
  filter(!is_subtotal, !is.na(section), section != "other")

# every (component row) x (standard whose pattern AND fiscal-year window match
matches <- bind_rows(lapply(seq_len(nrow(STANDARDS)), function(i) {
  s <- STANDARDS[i, ]
  components |>
    filter(str_detect(tag, s$tag_pattern), fy >= s$fy_from, fy <= s$fy_to) |>
    transmute(cik, start, end, section, tag,
              standard = s$standard, source = s$source, impact = s$impact,
              priority = s$priority)
})) |>
  # a tag matching several patterns goes to the highest-priority (most specific)
  arrange(cik, start, end, section, tag, priority) |>
  distinct(cik, start, end, section, tag, .keep_all = TRUE) |>
  select(-priority)

# OVERRIDE: never attribute to a standard where the filer itself disclosed a
# correction for that period.
#   Tag x window attribution cannot tell an adoption apart from a restatement
#   that happens to touch the same tags inside the window. An amendment is
#   independent evidence that something was actually wrong, so it wins.
#   The case that forced this: Akorn FY2014 (Audit Analytics restatement_key
#   48887) is a documented restatement whose excess tax benefit legs move
#   -9,193 / +9,193. With a window starting in FY2014 the financing section came
#   back "explained by standard" with a zero residual -- a real misstatement
#   excluded by a standard that had not yet been issued. Tightening the window
#   fixes that specific case; this override is the general guard.
#   Amendments are rare (7,740 of 267,288 periods) so the cost is negligible.
disclosed_periods <- lc |>
  filter(coalesce(amend_change_type, "Unchanged") != "Unchanged") |>
  distinct(cik, start, end) |>
  mutate(has_disclosed_correction = TRUE)

lc_flagged <- lc |>
  left_join(matches, by = c("cik", "start", "end", "section", "tag")) |>
  left_join(disclosed_periods, by = c("cik", "start", "end")) |>
  mutate(
    has_disclosed_correction = coalesce(has_disclosed_correction, FALSE),
    standard_overridden = !is.na(standard) & has_disclosed_correction,
    is_standard_driven = !is.na(standard) & !has_disclosed_correction &
                         coalesce(change_type, "Unchanged") != "Unchanged",
    # a change inside the XBRL phase-in era, or in the censored right edge, is
    # not attributable to a standard but is not clean evidence either
    era = case_when(fy <= EARLY_XBRL_FY_MAX ~ "early XBRL phase-in",
                    fy >= CENSORED_FY_MIN   ~ "censored right edge",
                    TRUE                    ~ "clean"))

cat("\n=== component changes attributed to a standard ===\n")
print(lc_flagged |>
  filter(!is_subtotal, coalesce(change_type, "Unchanged") != "Unchanged") |>
  mutate(standard = coalesce(standard, "(unattributed)")) |>
  count(standard, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)))


# 6. Adjust each section for the standard-driven amount ------------------------
# std_difference       amount of the section's movement attributable to standards
# unexplained_difference  what is left once that is removed -- the candidate revision
section_std <- lc_flagged |>
  filter(!is_subtotal, !is.na(section), section != "other") |>
  group_by(cik, start, end, section) |>
  summarise(
    std_difference   = sum(difference[is_standard_driven], na.rm = TRUE),
    other_difference = sum(difference[!is_standard_driven], na.rm = TRUE),
    n_std_changed    = sum(is_standard_driven, na.rm = TRUE),
    n_other_changed  = sum(!is_standard_driven &
                             coalesce(change_type, "Unchanged") != "Unchanged", na.rm = TRUE),
    standards        = paste(sort(unique(na.omit(standard[is_standard_driven]))),
                             collapse = " + "),
    std_impacts      = paste(sort(unique(na.omit(impact[is_standard_driven]))),
                             collapse = " + "),
    .groups = "drop") |>
  mutate(standards   = if_else(standards == "", NA_character_, standards),
         std_impacts = if_else(std_impacts == "", NA_character_, std_impacts))

scf_section_adjusted <- sc |>
  left_join(section_std, by = c("cik", "start", "end", "section")) |>
  mutate(
    across(c(std_difference, other_difference, n_std_changed, n_other_changed),
           ~ coalesce(.x, 0)),
    # THE ADJUSTMENT MUST ONLY EVER EXPLAIN MOVEMENT AWAY, NEVER CREATE IT.
    #   A naive `subtotal_difference - std_difference` fails that test. Where a
    #   standard-driven line moved +X and an unattributed line moved -X, the
    #   subtotal never budged, yet subtracting the attributed +X leaves a
    #   residual of -X -- inventing movement in a section that was flat. On the
    #   full population this pushed 12,592 periods from "intra-section reclass"
    #   to "net cash flow change", the opposite of what removing a confound
    #   should do.
    #   So the attributed amount is applied only in the direction of the observed
    #   movement and capped at its size: a standard can account for up to all of
    #   what happened and no more.
    std_applied = if_else(sign(std_difference) == sign(subtotal_difference),
                          sign(subtotal_difference) *
                            pmin(abs(std_difference), abs(subtotal_difference)),
                          0),
    # the residual the standards do not account for
    unexplained_difference = subtotal_difference - std_applied,
    any_standard  = n_std_changed > 0,
    # did removing the standard amount materially change the picture?
    subtotal_material    = abs(subtotal_difference)       >= MATERIALITY_FLOOR,
    unexplained_material = abs(unexplained_difference)    >= MATERIALITY_FLOOR,
    std_explains_all     = subtotal_material & !unexplained_material,
    section_verdict = case_when(
      !recon_testable      ~ "not testable",
      !subtotal_material   ~ "no movement",
      std_explains_all     ~ "explained by standard",
      any_standard         ~ "partly standard, residual remains",
      TRUE                 ~ "unexplained (candidate revision)"))


# 7. Period-level rollup -------------------------------------------------------
# Reclassify each period on the UNEXPLAINED amounts, so the classification
# reflects what is left after the FASB's own requirements are removed.
scf_period_adjusted <- scf_section_adjusted |>
  group_by(cik, start, end) |>
  summarise(
    cfo_unexplained = sum(unexplained_difference[section == "operating"]),
    cfi_unexplained = sum(unexplained_difference[section == "investing"]),
    cff_unexplained = sum(unexplained_difference[section == "financing"]),
    cfo_std = sum(std_difference[section == "operating"]),
    cfi_std = sum(std_difference[section == "investing"]),
    cff_std = sum(std_difference[section == "financing"]),
    standards_involved = paste(sort(unique(na.omit(unlist(str_split(standards, " \\+ "))))),
                               collapse = " + "),
    n_std_changed_lines   = sum(n_std_changed),
    n_other_changed_lines = sum(n_other_changed),
    n_sections_testable = sum(recon_testable, na.rm = TRUE),
    all_sections_testable = all(recon_testable, na.rm = TRUE),
    .groups = "drop")

# carry the era label (one row per period; `era` is a function of fy alone)
period_era <- lc_flagged |> distinct(cik, start, end, era, fy)
scf_period_adjusted <- scf_period_adjusted |>
  left_join(period_era, by = c("cik", "start", "end")) |>
  mutate(
    net_unexplained = cfo_unexplained + cfi_unexplained + cff_unexplained,
    n_sections_moved_unexplained =
      (abs(cfo_unexplained) >= MATERIALITY_FLOOR) +
      (abs(cfi_unexplained) >= MATERIALITY_FLOOR) +
      (abs(cff_unexplained) >= MATERIALITY_FLOOR),
    any_standard = standards_involved != "",
    standards_involved = if_else(standards_involved == "", NA_character_, standards_involved),
    # Same four-way taxonomy as 001e's period_type, recomputed on the amounts
    # left after standards are removed. The intra-section category is preserved
    # (lines moved but no subtotal did) so this stays comparable to 001e; it is
    # keyed on UNATTRIBUTED line changes, so a period whose only line movement
    # was a standard-driven rename correctly lands in "No change".
    period_type_adjusted = case_when(
      n_sections_moved_unexplained == 0 & n_other_changed_lines == 0 ~ "No change",
      n_sections_moved_unexplained == 0                     ~ "Intra-section reclass",
      abs(net_unexplained) < MATERIALITY_FLOOR              ~ "Cross-section reclass",
      TRUE                                                  ~ "Net cash flow change"))


# 8. Save ----------------------------------------------------------------------
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

write_parquet_safely(lc_flagged,
  glue("{data_path}scf_line_changes_flagged{IN_SUFFIX}.parquet"))
write_parquet_safely(scf_section_adjusted,
  glue("{data_path}scf_section_adjusted{IN_SUFFIX}.parquet"))
write_parquet_safely(scf_period_adjusted,
  glue("{data_path}scf_period_adjusted{IN_SUFFIX}.parquet"))

cat("\nSaved:\n")
cat("  scf_line_changes_flagged  ", format(nrow(lc_flagged), big.mark=","), "rows\n")
cat("  scf_section_adjusted      ", format(nrow(scf_section_adjusted), big.mark=","), "rows\n")
cat("  scf_period_adjusted       ", format(nrow(scf_period_adjusted), big.mark=","), "rows\n")


# 9. Diagnostics ---------------------------------------------------------------
cat("\n=== section verdict (all sections) ===\n")
print(scf_section_adjusted |> count(section_verdict, sort = TRUE) |>
  mutate(pct = round(100*n/sum(n), 1)))

cat("\n=== among TESTABLE sections that actually moved ===\n")
print(scf_section_adjusted |>
  filter(recon_testable, subtotal_material) |>
  summarise(sections = n(),
            explained_by_standard = sum(std_explains_all),
            pct_explained = round(100*mean(std_explains_all), 1),
            partly_standard = sum(any_standard & !std_explains_all),
            fully_unexplained = sum(!any_standard)))

# NOTE ON MEASURING "how much do standards absorb"
#   Comparing sum|unexplained| against sum|subtotal| is NOT a valid absorption
#   metric and will read as negative. The decomposition
#       standard_difference + other_difference = subtotal_difference
#   is exact (verified by the sanity check below), but the two parts routinely
#   have OPPOSITE signs: a section can take +100 from a standard and -100 from
#   another cause, netting to a subtotal move of zero. Then |other| = 100 while
#   |subtotal| = 0, and any ratio of absolute sums goes haywire. That is a
#   property of the data, not an error.
#   The meaningful question is per section: does removing the standard-driven
#   amount leave the section immaterial?
cat("\n=== do standards explain the movement? (testable sections that moved) ===\n")
print(scf_section_adjusted |>
  filter(recon_testable, subtotal_material) |> group_by(section) |>
  summarise(sections = n(),
            explained_by_standard = sum(std_explains_all),
            pct_explained = round(100*mean(std_explains_all), 1),
            standard_involved = sum(any_standard),
            residual_exceeds_move = sum(abs(unexplained_difference) >
                                        abs(subtotal_difference)),
            .groups = "drop"))

# Where a standard IS involved, how much of the movement does it leave behind?
cat("\n=== residual as a share of the original movement, where a standard applies ===\n")
print(scf_section_adjusted |>
  filter(recon_testable, subtotal_material, any_standard) |>
  mutate(ratio = abs(unexplained_difference)/abs(subtotal_difference)) |>
  group_by(section) |>
  summarise(sections = n(),
            median_residual_share = round(median(ratio), 2),
            pct_residual_over_1 = round(100*mean(ratio > 1), 1),
            .groups = "drop"))

# Sanity checks on the adjustment itself.
cat("\n=== sanity checks ===\n")
print(scf_section_adjusted |> summarise(
  sections = n(),
  # the adjustment must never enlarge apparent movement
  residual_exceeds_move = sum(abs(unexplained_difference) >
                              abs(subtotal_difference) + 1),
  # nor flip its direction
  sign_flipped = sum(sign(unexplained_difference) != sign(subtotal_difference) &
                     abs(unexplained_difference) >= 1),
  # a flat section must stay flat
  flat_made_to_move = sum(!subtotal_material &
                          abs(unexplained_difference) >= MATERIALITY_FLOOR),
  # raw decomposition still exact where the section reconciles
  decomp_exact = sum(recon_ok &
                     abs((std_difference + other_difference) -
                         subtotal_difference) < 1)))

cat("\n=== period classification: before (001e) vs after removing standards ===\n")
ps_orig <- read_parquet(glue("{data_path}scf_period_summary{IN_SUFFIX}.parquet")) |>
  select(cik, start, end, period_type)
print(scf_period_adjusted |> left_join(ps_orig, by = c("cik","start","end")) |>
  mutate(period_type = coalesce(period_type, "(none)")) |>
  count(period_type, period_type_adjusted) |>
  pivot_wider(names_from = period_type_adjusted, values_from = n, values_fill = 0L))

cat("\n=== periods whose classification CHANGED once standards were removed ===\n")
print(scf_period_adjusted |> left_join(ps_orig, by = c("cik","start","end")) |>
  filter(!is.na(period_type), period_type != period_type_adjusted) |>
  count(from = period_type, to = period_type_adjusted, sort = TRUE))

cat("\n=== era breakdown (early-XBRL and censored periods are not clean evidence) ===\n")
print(scf_period_adjusted |> count(era) |> mutate(pct = round(100*n/sum(n), 1)))
