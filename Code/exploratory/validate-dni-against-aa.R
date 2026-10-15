# SCRATCH: validate the pipeline's measurement logic by reproducing AA's
# hand-coded "Cumulative Change in Net Income" from our own XBRL comparison.
#
# THE IDEA (Matt's, and it is the best validation available here):
#   Net income is measured by EXACTLY the same machinery as CFO -- same
#   original-vs-comparative anchoring, same sign resolution, same YTD annual
#   basis, same window aggregation. The only difference is which line we read.
#   AA independently hand-codes the cumulative change in net income for each
#   restatement. So if our dNI reproduces AA's chg_ni, the machinery is right,
#   and dCFO inherits that credibility. That is population-scale convergent
#   validity, which is far stronger evidence than hand-auditing 30 cases.
#
# Net income needs no new extraction: it is the top line of an indirect-method
# SCF, so it is already in the extract (98.2% of company-periods).
#
# Nothing is written to DATA_PATH.

suppressMessages({
  library(dplyr); library(arrow); library(glue); library(readxl)
  library(stringr); library(tidyr)
})
dp <- Sys.getenv("DATA_PATH"); options(width = 200, scipen = 999)

# ---- 1. our net income change, per company-period --------------------------
# Prefer NetIncomeLoss (attributable to parent, which is what AA's "net income"
# means) over ProfitLoss (includes noncontrolling interests). Taking both and
# ranking avoids dropping the ~38% of filings that only tag ProfitLoss.
NI_PRIORITY <- c("NetIncomeLoss" = 1L, "ProfitLoss" = 2L,
                 "[Income starting point]" = 3L)

lc <- read_parquet(glue("{dp}scf_line_changes_full.parquet"),
                   col_select = c("cik", "start", "end", "tag", "orig_val",
                                  "comp_val", "amend_val", "difference",
                                  "change_type", "in_original", "in_comparative"))

ni <- lc |>
  filter(tag %in% names(NI_PRIORITY)) |>
  mutate(pri = unname(NI_PRIORITY[tag])) |>
  group_by(cik, start, end) |>
  arrange(pri, .by_group = TRUE) |> slice(1) |> ungroup() |>
  transmute(cik, start, end, ni_tag = tag,
            ni_orig = orig_val, ni_comp = comp_val,
            ni_diff = difference, ni_change_type = change_type)

cat("company-periods with a usable income line:",
    format(nrow(ni), big.mark = ","), "\n")
print(ni |> count(ni_tag) |> mutate(pct = round(100*n/sum(n), 1)) |>
        as.data.frame())

# ---- 2. ROUNDING CHECK, which matters for dCFO too -------------------------
# A filer can re-present the same figure at coarser precision. AAR Corp shows
# ProfitLoss 69,826,000 -> 69,800,000: a 26,000 "change" that is pure rounding.
# Test: does the later value equal the original rounded to some coarser unit?
cat("\n####### ROUNDING ARTEFACT CHECK #######\n")
rnd <- ni |> filter(ni_change_type == "Value change",
                    !is.na(ni_orig), !is.na(ni_comp), ni_orig != 0) |>
  mutate(
    is_round = (ni_comp == round(ni_orig / 1e2) * 1e2) |
               (ni_comp == round(ni_orig / 1e3) * 1e3) |
               (ni_comp == round(ni_orig / 1e4) * 1e4) |
               (ni_comp == round(ni_orig / 1e5) * 1e5) |
               (ni_comp == round(ni_orig / 1e6) * 1e6),
    rel = abs(ni_diff) / abs(ni_orig))
cat("NI 'value change' periods:", format(nrow(rnd), big.mark = ","), "\n")
cat("explainable as pure rounding of the original:",
    format(sum(rnd$is_round), big.mark = ","),
    sprintf("(%.1f%%)", 100 * mean(rnd$is_round)), "\n")
cat("\nrelative size of the change, rounding vs real:\n")
print(rnd |> group_by(is_round) |> summarise(n = n(),
  p50_rel = signif(median(rel), 3), p90_rel = signif(quantile(rel, .9), 3),
  .groups = "drop") |> as.data.frame())
cat("\nIMPLICATION: if a similar share of SCF 'changes' is rounding, the silent\n",
    "revision count is inflated. Worth testing on CFO directly.\n")

# ---- 3. attach to the AA annual basis and aggregate ------------------------
ab <- read_parquet(glue("{dp}aa_event_periods.parquet"))
cat("\nannual-basis rows:", format(nrow(ab), big.mark = ","), "\n")

ab2 <- ab |>
  left_join(ni, by = c("cik", "period_start" = "start", "period_end" = "end"))
cat("of which matched to an income line:",
    format(sum(!is.na(ab2$ni_tag)), big.mark = ","),
    sprintf("(%.1f%%)", 100 * mean(!is.na(ab2$ni_tag))), "\n")

ev_ni <- ab2 |>
  group_by(aa_event_id) |>
  summarise(n_fy = n(),
            n_fy_ni = sum(!is.na(ni_tag)),
            our_dNI  = sum(ni_diff, na.rm = TRUE),
            our_dCFO = sum(cfo_change, na.rm = TRUE),
            fy_first = min(fy), fy_last = max(fy),
            all_annual = all(period_length == "annual"),
            .groups = "drop")

ev <- read_parquet(glue("{dp}aa_events_scf_characterised.parquet")) |>
  filter(matched_panel, aa_source == "restatement") |>
  select(aa_event_id, cik, com_name, file_date, chg_ni, cfr, restate,
         misstate_begin, misstate_end, acct_failures) |>
  inner_join(ev_ni, by = "aa_event_id") |>
  mutate(aa_dNI = as.numeric(chg_ni))

# ---- 4. the comparison ------------------------------------------------------
# Our convention: difference = as-filed MINUS later. So an originally
# OVERSTATED income gives a POSITIVE our_dNI. AA reports the change TO net
# income, so an overstatement restated downward is NEGATIVE. Expect
# our_dNI ~ -aa_dNI. Tested rather than assumed.
cat("\n\n####### 4. DOES OUR dNI REPRODUCE AA's chg_ni? #######\n")
cmp <- ev |> filter(aa_dNI != 0, our_dNI != 0, n_fy_ni > 0)
cat("events with both non-zero:", nrow(cmp), "\n")

cat("\n-- correlation (tests the sign convention too) --\n")
cat("Pearson  our_dNI vs aa_dNI :", round(cor(cmp$our_dNI, cmp$aa_dNI), 3), "\n")
cat("Spearman our_dNI vs aa_dNI :",
    round(cor(cmp$our_dNI, cmp$aa_dNI, method = "spearman"), 3), "\n")
cat("Pearson  |our| vs |aa|     :",
    round(cor(abs(cmp$our_dNI), abs(cmp$aa_dNI)), 3), "\n")
cat("Spearman |our| vs |aa|     :",
    round(cor(abs(cmp$our_dNI), abs(cmp$aa_dNI), method = "spearman"), 3), "\n")

cat("\n-- sign agreement, under the expected flip (our = -aa) --\n")
cmp <- cmp |> mutate(sign_ok = sign(our_dNI) == -sign(aa_dNI))
cat("agree:", sum(cmp$sign_ok), sprintf("(%.1f%%)", 100*mean(cmp$sign_ok)), "\n")

cat("\n-- magnitude agreement --\n")
cmp <- cmp |> mutate(ratio = abs(our_dNI) / abs(aa_dNI))
print(cmp |> summarise(
  within_10pct = sum(ratio > .9  & ratio < 1.1),
  within_25pct = sum(ratio > .75 & ratio < 1.25),
  within_2x    = sum(ratio > .5  & ratio < 2),
  n = n()) |> mutate(across(starts_with("within"),
                            ~ paste0(.x, sprintf(" (%.1f%%)", 100*.x/n)))) |>
  as.data.frame())
cat("\nratio distribution |our| / |AA|:\n")
print(round(quantile(cmp$ratio, c(.05,.25,.5,.75,.95)), 3))

cat("\n-- same, restricted to events covering only ANNUAL periods --\n")
ca <- cmp |> filter(all_annual)
cat("n =", nrow(ca), "| Spearman |our| vs |aa| :",
    round(cor(abs(ca$our_dNI), abs(ca$aa_dNI), method = "spearman"), 3),
    "| within 25%:", sum(ca$ratio > .75 & ca$ratio < 1.25),
    sprintf("(%.1f%%)", 100*mean(ca$ratio > .75 & ca$ratio < 1.25)), "\n")

cat("\n-- single-fiscal-year events only (cleanest: no cumulation at all) --\n")
c1 <- cmp |> filter(n_fy == 1)
cat("n =", nrow(c1), "| Spearman |our| vs |aa| :",
    round(cor(abs(c1$our_dNI), abs(c1$aa_dNI), method = "spearman"), 3),
    "| within 25%:", sum(c1$ratio > .75 & c1$ratio < 1.25),
    sprintf("(%.1f%%)", 100*mean(c1$ratio > .75 & c1$ratio < 1.25)), "\n")

cat("\n-- detection: AA says income changed. do we see ANY NI change? --\n")
det <- ev |> filter(aa_dNI != 0, n_fy_ni > 0)
cat("AA-quantified events with an income line:", nrow(det),
    "| we see a non-zero dNI:", sum(det$our_dNI != 0),
    sprintf("(%.1f%%)", 100*mean(det$our_dNI != 0)), "\n")

cat("\n-- worked examples: largest AA income changes --\n")
print(cmp |> arrange(desc(abs(aa_dNI))) |>
  transmute(com_name = str_sub(com_name, 1, 28), fy_first, fy_last, n_fy,
            aa_dNI, our_dNI, ratio = round(ratio, 2), sign_ok) |>
  head(12) |> as.data.frame())
