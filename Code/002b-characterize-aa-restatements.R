# =============================================================================
# 002b - Characterise what each Audit Analytics restatement did to the SCF
# =============================================================================
# PURPOSE
#   002a is panel-anchored and asks "was our revision disclosed?". This script
#   runs the other way on purpose: the AA event is the unit, and for each one we
#   ask what the cash flow statement actually did. Not just "did AA flag it" but
#   the signed change in each section, and whether the movement stayed INSIDE a
#   section (a reclassification that leaves the subtotal untouched), crossed
#   between sections, or changed net cash.
#
#   The two scripts answer different questions and neither replaces the other:
#     002a  of OUR revisions, how many did AA never record?        (silence)
#     002b  of AA's restatements, what did the SCF do, and did AA
#           characterise that correctly?                           (accuracy)
#
#   Known and accepted limitation: AA only exists where a filer disclosed, so
#   this file is silent on the silent population by construction. It is the
#   accuracy check on the disclosed channel, not a prevalence estimate.
#
# MOST AA RESTATEMENTS HAVE NOTHING TO DO WITH THE CASH FLOW STATEMENT
#   This is the single easiest way to misread this script, so the reporting is
#   built around it. AA's GAAP failures field is a pipe-delimited list of
#   labels, and exactly one of them names this statement -- "Cash flow
#   statement (SFAS 95) classification errors" -- carried here as `cfr`. It
#   appears on 1,772 of 17,703 events, about 10%. The rest are debt and warrant
#   issues (3,829), tax (2,402), revenue recognition (2,054), accruals, expense
#   recording and so on.
#
#   So pooling every AA event and reporting what the SCF did is NOT a statement
#   about cash flow restatements. It mostly measures background tagging churn,
#   because line-level change is the norm in this panel: ~78% of ALL panel
#   periods show some line-level change and ~17% show a section-level change,
#   with no restatement involved. Against that baseline a pooled figure near
#   50% says almost nothing.
#
#   Therefore:
#     PRIMARY population  = events where AA itself named the cash flow
#                           statement (`cfr == 1`). Only here are AA and this
#                           pipeline talking about the same thing.
#     CONTROL population  = events where AA named some other failure. Useful
#                           for exactly two things: (a) a contrast that shows
#                           the `cfr` flag has real content, and (b) the
#                           under-characterisation claim -- AA logged the
#                           restatement and never called it cash flow, yet a
#                           section moved. That second claim only works when
#                           the movement is compared against the panel
#                           baseline, never asserted from its level alone.
#
# THE TWO HARD PARTS, BOTH FROM THE GRAIN MISMATCH
#
#   (1) ONE AA RECORD SPANS MANY OF OUR PERIODS.
#       An AA window is a firm-level date range -- median 364 days, a quarter at
#       the 25th percentile, over 5 years at the 99th. Window bounds land on
#       fiscal boundaries (begins cluster in months 1/4/7/10, ends in 3/6/9/12),
#       so a window maps onto a set of reporting periods rather than cutting
#       across them. We expand each event to the periods it covers, characterise
#       each, and roll back up to the event.
#
#   (2) OUR SCF FIGURES ARE YTD, SO THEY NEST AND MUST NOT BE SUMMED.
#       Within one fiscal year the panel holds Q1 (3 months), Q2 (6), Q3 (9) and
#       FY (12) -- roughly 67k periods at each length. These are cumulative, not
#       additive: Q3 CONTAINS Q1. Summing the four counts the same dollars up to
#       four times and inflates every magnitude.
#
#       Fix: within each (cik, fiscal year) keep the SINGLE LONGEST period the
#       window covers -- the "annual basis". That is one non-overlapping
#       observation per fiscal year, and it is the most inclusive view of that
#       year the window actually reaches. If a window ends in June the 12-month
#       period is not covered and the 6-month Q2 YTD is correctly chosen instead.
#       Only then is it safe to sum across fiscal years.
#
#       We do NOT difference YTD figures into discrete quarters. It is possible
#       (Q3 minus Q2) but every differenced quarter inherits the error of two
#       filings, and a window aligned to quarter ends gains nothing from it. The
#       annual basis loses within-year timing and keeps magnitudes right, which
#       is the correct trade for this question.
#
#       Consequence worth stating: `n_fy_covered` counts fiscal years, not
#       filings. An event spanning 3 years has 3 annual-basis rows, not 12.
#
# WINDOW CONTAINMENT - and why the obvious "strict" rule is WRONG here
#   The rule used is `period_end` inside [misstate_begin, misstate_end], the
#   same as 002a, so the two scripts stay comparable.
#
#   The tempting alternative -- also require period_start >= misstate_begin, so
#   no part of the figure predates the window -- was built, measured and
#   REJECTED. It does not do what it appears to do on YTD data. A YTD period
#   always starts at the fiscal year start by construction, so that condition
#   does not test whether the figure is contaminated; it only asks whether the
#   AA window happens to open on a fiscal year boundary. For a window opening in
#   April, every YTD period of that year starts in January, so the whole year is
#   discarded even though the window covers Q2-Q4. Measured cost: 12,971 annual
#   rows fall to 4,802 (-63%), and 4,112 of the 17,703 windows open in months
#   4/7/10, i.e. mid-year. The rule selects on window alignment, not data
#   quality.
#
#   What is really unavoidable: with YTD figures you cannot isolate Apr-Jun
#   without differencing, so where a window opens mid-year the matched figure
#   does include some unrestated months. That is a genuine limitation and it is
#   carried as a FLAG (`fy_fully_in_window`, and `n_fy_fully_covered` on the
#   event) plus a sensitivity cut in section 7, not as a filter that silently
#   drops two thirds of the data.
#
# EVENT-LEVEL VERDICT IS A SEVERITY RANK, NOT A SUM
#   Summing signed changes across fiscal years lets offsetting years cancel:
#   CFO +500k in FY2014 and -500k in FY2015 sums to zero and would read as "no
#   change" although both years moved. So the event verdict is the most severe
#   per-year verdict (net change > cross-section > intra-section > none), and
#   the dollar sums are carried separately as magnitudes only.
#
# INPUT   {data_path}aa_cleaned.parquet                    (002a section 5)
#         {data_path}scf_filing_summary{IN_SUFFIX}.parquet (001c)
# OUTPUT  {data_path}aa_events_scf_characterised.parquet   one row per AA event
#         {data_path}aa_event_periods.parquet              event x fiscal year
# =============================================================================


# 1. Setup ---------------------------------------------------------------------
library(dplyr)
library(stringr)
library(arrow)
library(glue)
library(tidyr)
library(lubridate)
library(readr)

data_path <- Sys.getenv("DATA_PATH")
options(scipen = 999, width = 200)


# 2. Configuration -------------------------------------------------------------
IN_SUFFIX <- "_full"

# Same floor as 001b, for the same reason: filings round to thousands, so
# anything under $1,000 is rounding noise rather than a revision. Absolute, not
# scaled to firm size -- see the limitations section of CLAUDE.md.
MATERIALITY_FLOOR <- 1000

# Severity order for collapsing per-year verdicts up to the event.
VERDICT_RANK <- c("No change"             = 0L,
                  "Intra-section reclass" = 1L,
                  "Cross-section reclass" = 2L,
                  "Net cash flow change"  = 3L)


# 3. Load ----------------------------------------------------------------------
aa <- read_parquet(glue("{data_path}aa_cleaned.parquet")) |>
  mutate(aa_event_id = row_number(),
         win_days = as.numeric(misstate_end - misstate_begin))

fs <- read_parquet(glue("{data_path}scf_filing_summary{IN_SUFFIX}.parquet"))

cat("AA events:", format(nrow(aa), big.mark = ","),
    "| filing-periods:", format(nrow(fs), big.mark = ","), "\n")

# The panel's reach. An AA window outside this can never match, and that is a
# coverage fact about the panel rather than a failure to detect anything.
panel_min  <- min(fs$period_end, na.rm = TRUE)
panel_max  <- max(fs$period_end, na.rm = TRUE)
panel_ciks <- unique(fs$cik)
cat("panel covers", format(panel_min), "to", format(panel_max),
    "across", format(length(panel_ciks), big.mark = ","), "CIKs\n")


# 4. Expand each AA event to the filing-periods its window covers --------------
# Deliberately many-to-many: one event covers several periods and one period can
# fall inside several events' windows.
per <- fs |>
  select(cik, fy, fp, period_start, period_end, period_length,
         any_change, any_unexplained, n_tags_changed, n_sections_changed,
         period_type, period_type_adjusted, all_testable, era, afs, sic,
         cfo_as_filed, cfi_as_filed, cff_as_filed,
         cfo_change, cfi_change, cff_change,
         cfo_unexplained, cfi_unexplained, cff_unexplained,
         cfo_changed_tags, cfi_changed_tags, cff_changed_tags,
         has_amendment, company_name, orig_url)

aa_keys <- aa |>
  select(aa_event_id, cik, restatement_key, com_name, aa_source, file_date,
         misstate_begin, misstate_end, win_days, cfr, restate, material,
         materiality, chg_ni, fraud, icmw, rev_rec, num_failures, acct_failures)

matched <- aa_keys |>
  inner_join(per, by = "cik", relationship = "many-to-many") |>
  filter(period_end >= misstate_begin, period_end <= misstate_end) |>
  # Quality flag, NOT a filter -- see the containment note in the header. TRUE
  # means the window also covers the start of this fiscal year, so none of the
  # YTD figure predates the restated window.
  mutate(fy_fully_in_window = period_start >= misstate_begin)

cat("\nevent x period pairs:", format(nrow(matched), big.mark = ","),
    "| of which the window also covers the fiscal year start:",
    format(sum(matched$fy_fully_in_window), big.mark = ","), "\n")


# 5. Collapse YTD nesting: one row per (event, fiscal year) --------------------
# THE critical step. Keep the longest covered period per fiscal year so the
# retained rows do not overlap and can legitimately be summed.
annual_basis <- function(d) {
  d |>
    group_by(aa_event_id, cik, fy) |>
    arrange(desc(period_length), desc(period_end), .by_group = TRUE) |>
    slice(1) |>
    ungroup()
}

ab <- annual_basis(matched)

cat("annual-basis rows:", format(nrow(ab), big.mark = ","),
    "| of which the window covers the full fiscal year:",
    format(sum(ab$fy_fully_in_window), big.mark = ","), "\n")

cat("\n=== which YTD length got picked as the annual basis ===\n")
print(ab |> count(period_length, fp) |>
  pivot_wider(names_from = fp, values_from = n, values_fill = 0L) |>
  as.data.frame())
cat("(annual = the full year was covered; 3/6/9 months = the window ended",
    "mid-year,\n so the most inclusive covered view of that year is a YTD",
    "stub)\n")

write_parquet(ab, glue("{data_path}aa_event_periods.parquet"))


# 6. Roll up to one row per AA event -------------------------------------------
roll_up <- function(d) {
  d |>
    # Raw `period_type` is NA for 2,181 panel periods (0.8%): those with an
    # amendment or no channel at all but no COMPARATIVE to compare against, so
    # 001a never computed the taxonomy. Every one has any_change == FALSE, so
    # falling back to the adjusted label (always "No change" for these) is
    # faithful rather than a guess. Left as NA it would propagate through max()
    # and silently NA out the verdict for 5.9% of matched events.
    mutate(v_rank     = unname(VERDICT_RANK[coalesce(period_type,
                                                     period_type_adjusted)]),
           v_rank_adj = unname(VERDICT_RANK[period_type_adjusted])) |>
    group_by(aa_event_id) |>
    summarise(
      n_fy_covered     = n(),
      n_fy_fully_covered = sum(fy_fully_in_window),
      fy_first         = min(fy), fy_last = max(fy),
      # TWO different notions of "changed", and the distinction is central to
      # this project rather than a technicality. 001c's `any_change` is TRUE
      # only when a section SUBTOTAL moved, so on its own it is blind to a
      # within-section reclassification -- exactly the phenomenon the paper is
      # about. Line-level change is therefore the inclusive measure and the one
      # `scf_detected` is built from; the section measure is kept beside it.
      n_fy_section_changed = sum(any_change),
      n_fy_line_changed    = sum(coalesce(n_tags_changed, 0L) > 0),
      n_fy_changed         = sum(any_change),
      n_fy_changed_full    = sum(any_change & fy_fully_in_window),
      n_fy_unexplained = sum(any_unexplained),
      n_fy_testable    = sum(all_testable),
      n_fy_clean_era   = sum(era == "clean"),
      n_tags_changed   = sum(n_tags_changed, na.rm = TRUE),

      # magnitudes, summed over non-overlapping fiscal years
      cfo_change = sum(cfo_change, na.rm = TRUE),
      cfi_change = sum(cfi_change, na.rm = TRUE),
      cff_change = sum(cff_change, na.rm = TRUE),
      cfo_unexplained = sum(cfo_unexplained, na.rm = TRUE),
      cfi_unexplained = sum(cfi_unexplained, na.rm = TRUE),
      cff_unexplained = sum(cff_unexplained, na.rm = TRUE),
      cfo_as_filed = sum(cfo_as_filed, na.rm = TRUE),

      # sum of absolute values: survives offsetting years, unlike the signed sum
      abs_section_change = sum(abs(cfo_change) + abs(cfi_change) +
                               abs(cff_change), na.rm = TRUE),
      max_abs_section_change = max(pmax(abs(cfo_change), abs(cfi_change),
                                        abs(cff_change), na.rm = TRUE)),

      # severity rank, immune to signed cancellation across years
      worst_rank     = max(v_rank, na.rm = TRUE),
      worst_rank_adj = max(v_rank_adj, na.rm = TRUE),
      # how often the standards layer claimed an AA-disclosed period. 001b's
      # amendment override does NOT protect these (it keys on 10-K/A and 10-Q/A
      # only, and runs before the AA link exists), so a genuine AA-disclosed
      # error can be attributed to a standard -- the Akorn failure mode via the
      # AA channel. Quantified in section 7 rather than assumed harmless.
      n_fy_standard_claimed = sum(any_change & !any_unexplained),
      changed_tags = paste(na.omit(c(cfo_changed_tags, cfi_changed_tags,
                                     cff_changed_tags)), collapse = " | "),
      any_amendment = any(has_amendment),
      company_name  = first(company_name),
      example_url   = first(orig_url),
      .groups = "drop") |>
    mutate(
      net_change   = cfo_change + cfi_change + cff_change,
      # PRIMARY verdict is on the RAW taxonomy, deliberately. An AA record is a
      # disclosed correction, and 001b's own rule is that a disclosed correction
      # beats standard attribution -- subtracting a standard here would erase
      # the very event AA recorded. The adjusted verdict is kept alongside so
      # the effect of that choice is visible.
      scf_verdict          = names(VERDICT_RANK)[worst_rank + 1L],
      scf_verdict_adjusted = names(VERDICT_RANK)[worst_rank_adj + 1L],
      # Derived FROM the verdict so the two can never disagree. Anything other
      # than "No change" means at least one line moved in at least one covered
      # fiscal year, including a reclassification that left every subtotal
      # intact.
      scf_detected      = scf_verdict != "No change",
      scf_section_moved = n_fy_section_changed > 0,
      scf_unexplained   = n_fy_unexplained > 0,
      # relative size of the move, where a denominator exists
      cfo_pct_change = if_else(abs(cfo_as_filed) > 0,
                               cfo_change / abs(cfo_as_filed), NA_real_)) |>
    select(-worst_rank, -worst_rank_adj)
}

ev <- roll_up(ab)

# Keep every AA event, including those matching no panel period at all.
events <- aa_keys |>
  left_join(ev, by = "aa_event_id") |>
  mutate(
    matched_panel = !is.na(n_fy_covered),
    across(c(n_fy_covered, n_fy_fully_covered, n_fy_changed, n_fy_section_changed, n_fy_line_changed,
             n_fy_changed_full, n_fy_unexplained, n_fy_testable,
             n_fy_clean_era, n_tags_changed, n_fy_standard_claimed),
           ~ coalesce(.x, 0L)),
    scf_detected      = coalesce(scf_detected, FALSE),
    scf_section_moved = coalesce(scf_section_moved, FALSE),
    scf_unexplained = coalesce(scf_unexplained, FALSE),

    # Why an event has no match. Separates a panel coverage gap from a genuine
    # "AA says restated, our SCF shows nothing".
    no_match_reason = case_when(
      matched_panel              ~ NA_character_,
      misstate_end   < panel_min ~ "window ends before panel starts",
      misstate_begin > panel_max ~ "window begins after panel ends",
      !cik %in% panel_ciks       ~ "CIK not in XBRL panel",
      TRUE                       ~ "CIK and era present, no period in window"),

    # The accuracy 2x2 this script exists to produce.
    aa_vs_us = case_when(
      !matched_panel           ~ "unmatched",
      cfr == 1 &  scf_detected ~ "AA flagged CF, we see a change",
      cfr == 1 & !scf_detected ~ "AA flagged CF, we see nothing",
      cfr == 0 &  scf_detected ~ "AA silent on CF, we see a change",
      TRUE                     ~ "AA silent on CF, we see nothing"))

write_parquet_safely <- function(x, path, tries = 6, wait = 5) {
  for (attempt in seq_len(tries)) {
    ok <- tryCatch({ write_parquet(x, path); TRUE },
                   error = function(e) { message("  write failed (", attempt,
                                        "/", tries, "): ",
                                        conditionMessage(e)); FALSE })
    if (ok) return(invisible(TRUE))
    Sys.sleep(wait)
  }
  stop("Could not write ", path, " after ", tries, " attempts.")
}
write_parquet_safely(events,
  glue("{data_path}aa_events_scf_characterised.parquet"))
cat("\nSaved aa_events_scf_characterised:", format(nrow(events), big.mark = ","),
    "rows x", ncol(events), "cols\n")


# 7. Results -------------------------------------------------------------------
cat("\n=== panel coverage of AA events ===\n")
print(events |> count(matched_panel) |> mutate(pct = round(100 * n / sum(n), 1)))
cat("\nwhy unmatched:\n")
print(events |> filter(!matched_panel) |> count(no_match_reason, sort = TRUE))

# Reconcile against 002a's reverse diagnostic, which restricted to AA windows
# ending inside the panel era before computing a match rate.
cat("\n=== coverage restricted to AA windows ending inside the panel era ===\n")
print(events |>
  filter(misstate_end >= panel_min, misstate_end <= panel_max) |>
  summarise(aa_events_in_era = n(),
            matched = sum(matched_panel),
            pct = round(100 * mean(matched_panel), 1)))

cat("\n=== sensitivity: all covered fiscal years vs only fully covered ones ===\n")
print(events |> filter(matched_panel) |> summarise(
  events            = n(),
  fy_covered        = sum(n_fy_covered),
  fy_fully_covered  = sum(n_fy_fully_covered),
  changed_all       = sum(n_fy_changed),
  changed_full_only = sum(n_fy_changed_full),
  pct_changed_all   = round(100 * sum(n_fy_changed) / sum(n_fy_covered), 1),
  pct_changed_full  = round(100 * sum(n_fy_changed_full) /
                            sum(n_fy_fully_covered), 1)))
cat("(if these two rates agree, mid-year window openings are not driving the",
    "result)\n")

m <- events |> filter(matched_panel)

cat("\n###############################################################\n")
cat("# PRIMARY: events where AA ITSELF named the cash flow statement\n")
cat("###############################################################\n")
m_cf  <- m |> filter(cfr == 1)
m_oth <- m |> filter(cfr == 0)
cat("cash-flow-labelled matched events:", nrow(m_cf),
    "| other-failure matched events:", nrow(m_oth), "\n")

cat("\n=== WHAT DID THE SCF DO -- AA's cash flow events vs the rest ===\n")
cmp <- bind_rows(
  m_cf  |> count(scf_verdict) |> mutate(grp = "AA named cash flow"),
  m_oth |> count(scf_verdict) |> mutate(grp = "AA named something else")) |>
  group_by(grp) |> mutate(pct = round(100 * n / sum(n), 1)) |> ungroup() |>
  select(scf_verdict, grp, pct) |>
  pivot_wider(names_from = grp, values_from = pct, values_fill = 0)
print(as.data.frame(cmp))
cat("Read the CONTRAST. Cross-section reclassification is the discriminating\n",
    "category: it is what a cash-flow classification error looks like, and it\n",
    "should be far commoner on the left than the right.\n")

cat("\n=== the same thing as counts, cash-flow events only ===\n")
print(m_cf |> count(scf_verdict, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)))

cat("\n=== pooled over ALL matched events (kept only as a warning) ===\n")
print(m |> count(scf_verdict, sort = TRUE) |> mutate(pct = round(100 * n / sum(n), 1)))
cat("Do NOT quote this pooled table. 90% of AA events never named the cash\n",
    "flow statement, and the panel baseline for line-level change is ~78%, so\n",
    "most of what appears here is background churn rather than the event.\n")

cat("\n=== every AA failure label vs what the SCF did (per fiscal year) ===\n")
# Unnest the pipe-delimited failures field at the annual-basis grain so each
# label can be scored against the same panel baseline. Labels overlap: an event
# with 3 labels contributes to 3 rows, which is correct for "what does the SCF
# do when AA mentions X".
lab <- ab |>
  filter(!is.na(acct_failures)) |>
  mutate(f = str_split(acct_failures, fixed("|"))) |>
  unnest(f) |>
  mutate(f = str_trim(f)) |>
  filter(f != "") |>
  group_by(f) |>
  summarise(fiscal_years = n(),
            pct_section_changed = round(100 * mean(any_change), 1),
            pct_cross_or_net = round(100 * mean(period_type %in%
              c("Cross-section reclass", "Net cash flow change"), na.rm = TRUE), 1),
            .groups = "drop") |>
  filter(fiscal_years >= 100) |>
  arrange(desc(pct_section_changed))
print(as.data.frame(lab))
cat("\nPanel baseline for comparison: ",
    round(100 * mean(fs$any_change), 1), "% of all filing-periods show a",
    " section-level change.\n", sep = "")
cat("If the cash flow label does not sit at or near the top of this table, the\n",
    "detection logic is not measuring what it claims to measure.\n")

# THE BASE RATE. Without this, "we detect a change in 95% of AA's cash-flow
# events" is close to meaningless, because line-level change is the norm in this
# panel rather than the exception. Compared per FISCAL YEAR, not per event, so
# multi-year AA windows do not get an unfair advantage.
cat("\n=== BASE RATE: AA-matched fiscal years vs the whole panel ===\n")
panel_rates <- fs |> summarise(
  basis = "all panel filing-periods", n = n(),
  pct_line_changed    = round(100 * mean(coalesce(period_type,
                                period_type_adjusted) != "No change"), 1),
  pct_section_changed = round(100 * mean(any_change), 1))
ab_rates <- ab |> summarise(
  basis = "fiscal years inside an AA window", n = n(),
  pct_line_changed    = round(100 * mean(coalesce(period_type,
                                period_type_adjusted) != "No change"), 1),
  pct_section_changed = round(100 * mean(any_change), 1))
ab_oth_rates <- ab |> filter(cfr == 0) |> summarise(
  basis = "fiscal years in an AA window, OTHER failure only", n = n(),
  pct_line_changed    = round(100 * mean(coalesce(period_type,
                                period_type_adjusted) != "No change"), 1),
  pct_section_changed = round(100 * mean(any_change), 1))
ab_cf_rates <- ab |> filter(cfr == 1) |> summarise(
  basis = "fiscal years in an AA CASH-FLOW-flagged window", n = n(),
  pct_line_changed    = round(100 * mean(coalesce(period_type,
                                period_type_adjusted) != "No change"), 1),
  pct_section_changed = round(100 * mean(any_change), 1))
print(bind_rows(panel_rates, ab_rates, ab_oth_rates, ab_cf_rates) |>
        as.data.frame())
cat("Read the LIFT over the panel baseline, not the level. Line-level change is\n",
    "the norm in this panel, so only the excess is evidence of anything.\n")

cat("\n=== line-level vs section-level detection, split by AA's own label ===\n")
# Split, not pooled. Pooled, `lines_only_no_subtotal` reads as 4,617 and looks
# like the project's phenomenon, but it is dominated by the control group, whose
# intra-section rate (51.8%) is close to ordinary panel churn. Only the
# cash-flow-labelled row is a statement about cash flow restatements.
print(bind_rows(
  m_cf  |> mutate(grp = "AA named cash flow"),
  m_oth |> mutate(grp = "AA named something else")) |>
  group_by(grp) |> summarise(
    events                 = n(),
    pct_line_moved         = round(100 * mean(scf_detected), 1),
    pct_subtotal_moved     = round(100 * mean(scf_section_moved), 1),
    lines_only_no_subtotal = sum(scf_detected & !scf_section_moved),
    pct_lines_only         = round(100 * mean(scf_detected &
                                              !scf_section_moved), 1),
    .groups = "drop") |> as.data.frame())

cat("\n=== THE ACCURACY 2x2: AA's cash-flow flag vs what we detect ===\n")
print(m |> count(aa_vs_us, sort = TRUE) |> mutate(pct = round(100 * n / sum(n), 1)))

cat("\n=== cross-tab: AA cash-flow flag x our verdict ===\n")
print(m |> mutate(aa_cf = if_else(cfr == 1, "AA: cash flow", "AA: not cash flow")) |>
  count(aa_cf, scf_verdict) |>
  pivot_wider(names_from = aa_cf, values_from = n, values_fill = 0L) |>
  as.data.frame())

cat("\n=== among events AA flagged as a cash-flow failure ===\n")
print(m |> filter(cfr == 1) |> summarise(
  events                      = n(),
  we_detect_a_change          = sum(scf_detected),
  pct_detected                = round(100 * mean(scf_detected), 1),
  unexplained_after_standards = sum(scf_unexplained),
  intra_only    = sum(scf_verdict == "Intra-section reclass", na.rm = TRUE),
  cross_section = sum(scf_verdict == "Cross-section reclass", na.rm = TRUE),
  net_change    = sum(scf_verdict == "Net cash flow change", na.rm = TRUE),
  no_change     = sum(scf_verdict == "No change", na.rm = TRUE)))

cat("\n=== how often did the standards layer claim an AA-DISCLOSED period? ===\n")
cat("001b's override protects amendment-disclosed periods only, so these were",
    "unprotected.\n")
print(m |> filter(n_fy_changed > 0) |> summarise(
  events_with_a_change   = n(),
  fy_changed             = sum(n_fy_changed),
  fy_fully_claimed_by_std = sum(n_fy_standard_claimed),
  pct = round(100 * sum(n_fy_standard_claimed) / sum(n_fy_changed), 1)))

cat("\n=== raw vs standards-adjusted verdict (how much the layer moves this) ===\n")
print(m |> count(scf_verdict, scf_verdict_adjusted) |>
  pivot_wider(names_from = scf_verdict_adjusted, values_from = n,
              values_fill = 0L) |> as.data.frame())

cat("\n=== VALIDATION TARGET: AA flagged cash flow, we detect nothing ===\n")
miss <- m |> filter(cfr == 1, !scf_detected)
cat("n =", nrow(miss), "of", sum(m$cfr == 1), "CF-flagged matched events\n")
print(miss |> summarise(
  median_fy_covered = median(n_fy_covered),
  any_fy_testable   = sum(n_fy_testable > 0),
  any_fy_clean_era  = sum(n_fy_clean_era > 0),
  had_an_amendment  = sum(coalesce(any_amendment, FALSE)),
  formal_8k_402     = sum(restate == 1),
  median_win_days   = median(win_days)))
cat("\nwindow length of the misses:\n")
print(miss |> mutate(win = cut(win_days, c(-1, 95, 200, 400, 800, 1e5),
        labels = c("<=1 qtr", "2 qtrs", "~1 yr", "1-2 yr", "2+ yr"))) |>
  count(win))

cat("\n=== the reverse: AA never called it cash flow, but a SUBTOTAL moved ===\n")
# Restricted to section-level movement on purpose. At line level this cell holds
# 7,003 events, but most are within-section reclassifications that AA could
# reasonably decline to call a cash-flow-statement failure. Requiring a subtotal
# to have moved is the version of the claim that survives that objection.
print(m |> filter(cfr == 0, scf_section_moved) |> summarise(
  events          = n(),
  median_abs_move = median(abs_section_change),
  p90_abs_move    = quantile(abs_section_change, .9),
  cross_or_net    = sum(scf_verdict %in% c("Cross-section reclass",
                                           "Net cash flow change"))))

cat("\n=== does the SCF move scale with AA's reported change in net income? ===\n")
nz <- m |> filter(scf_detected, abs(chg_ni) > 0, abs_section_change > 0)
cat("events with both non-zero:", nrow(nz), "\n")
if (nrow(nz) > 30) {
  cat("Spearman rho |chg_ni| vs |section move|:",
      round(cor(abs(as.numeric(nz$chg_ni)), nz$abs_section_change,
                method = "spearman"), 3), "\n")
}

cat("\n=== formal 8-K Item 4.02 restatements only (the most serious tier) ===\n")
print(m |> filter(restate == 1) |> count(scf_verdict, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)))

cat("\n=== window length vs our ability to see a change ===\n")
# `pct_detected` is an EVENT-level rate, so it rises mechanically with window
# length: a longer window covers more fiscal years and so gets more chances to
# contain a change. `pct_fy_changed` normalises by fiscal years covered and is
# the comparable figure across window lengths.
print(m |> mutate(win = cut(win_days, c(-1, 95, 200, 400, 800, 1e5),
        labels = c("<=1 qtr", "2 qtrs", "~1 yr", "1-2 yr", "2+ yr"))) |>
  group_by(win) |> summarise(events = n(), fy_covered = sum(n_fy_covered),
            pct_detected = round(100 * mean(scf_detected), 1),
            pct_fy_changed = round(100 * sum(n_fy_changed) /
                                   sum(n_fy_covered), 1), .groups = "drop"))


# 8. Hand-validation samples ---------------------------------------------------
# Two targeted samples, both drawn from the cells where our result and AA's
# characterisation disagree. These are the cases where opening the filing
# actually settles something, unlike the agreeing cells.
set.seed(20260922)
val_cols <- c("restatement_key", "cik", "company_name", "com_name", "aa_source",
              "file_date", "misstate_begin", "misstate_end", "fy_first",
              "fy_last", "n_fy_covered", "n_fy_changed", "scf_verdict",
              "scf_verdict_adjusted", "cfo_change", "cfi_change", "cff_change",
              "net_change", "abs_section_change", "restate", "material",
              "chg_ni", "acct_failures", "changed_tags", "example_url")

# (a) AA says cash flow, we see nothing -> is this OUR false negative?
s_miss <- m |> filter(cfr == 1, !scf_detected) |>
  slice_sample(n = min(60, sum(m$cfr == 1 & !m$scf_detected))) |>
  select(any_of(val_cols))

# (b) AA recorded the restatement but never called it cash flow, yet a section
#     moved materially -> did AA under-characterise it?
s_uncharacterised <- m |>
  filter(cfr == 0, scf_detected,
         scf_verdict %in% c("Cross-section reclass", "Net cash flow change")) |>
  slice_sample(n = 60) |>
  select(any_of(val_cols))

readr::write_csv(s_miss,
  glue("{data_path}aa_validate_cf_flagged_no_change.csv"))
readr::write_csv(s_uncharacterised,
  glue("{data_path}aa_validate_section_moved_not_flagged.csv"))
cat("\nWrote validation samples:", nrow(s_miss), "+",
    nrow(s_uncharacterised), "cases\n")
