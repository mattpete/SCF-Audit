# Follow-ups to the dNI validation:
#   (A) Is the AA mismatch driven by WINDOW LENGTH? Hypothesis: a 10-K
#       re-presents only 2-3 prior years, so for a 5-year restatement window
#       the earliest years never appear in any later comparative and we
#       structurally cannot see their correction. If so the ratio |ours|/|AA|
#       should fall monotonically in the number of fiscal years covered, while
#       single-year events match exactly.
#   (B) How much of the CFO change is pure ROUNDING, as 16.6% of the NI
#       "value changes" turned out to be?

suppressMessages({
  library(dplyr); library(arrow); library(glue); library(stringr); library(tidyr)
})
dp <- Sys.getenv("DATA_PATH"); options(width = 200, scipen = 999)

NI_PRIORITY <- c("NetIncomeLoss" = 1L, "ProfitLoss" = 2L,
                 "[Income starting point]" = 3L)
lc <- read_parquet(glue("{dp}scf_line_changes_full.parquet"),
                   col_select = c("cik","start","end","tag","orig_val","comp_val",
                                  "difference","change_type","is_subtotal","section"))

ni <- lc |> filter(tag %in% names(NI_PRIORITY)) |>
  mutate(pri = unname(NI_PRIORITY[tag])) |>
  group_by(cik, start, end) |> arrange(pri, .by_group = TRUE) |> slice(1) |>
  ungroup() |> transmute(cik, start, end, ni_diff = difference)

ab <- read_parquet(glue("{dp}aa_event_periods.parquet")) |>
  left_join(ni, by = c("cik", "period_start" = "start", "period_end" = "end"))

ev <- read_parquet(glue("{dp}aa_events_scf_characterised.parquet")) |>
  filter(matched_panel, aa_source == "restatement") |>
  select(aa_event_id, com_name, chg_ni, win_days)

cmp <- ab |> group_by(aa_event_id) |>
  summarise(n_fy = n(), our_dNI = sum(ni_diff, na.rm = TRUE),
            n_fy_ni = sum(!is.na(ni_diff)), .groups = "drop") |>
  inner_join(ev, by = "aa_event_id") |>
  mutate(aa_dNI = as.numeric(chg_ni)) |>
  filter(aa_dNI != 0, our_dNI != 0, n_fy_ni > 0) |>
  mutate(ratio = abs(our_dNI) / abs(aa_dNI))

cat("####### (A) IS THE MISMATCH A WINDOW-LENGTH PROBLEM? #######\n")
cat("n =", nrow(cmp), "\n\n")
print(cmp |> group_by(n_fy = pmin(n_fy, 6)) |>
  summarise(events = n(),
            median_ratio = round(median(ratio), 3),
            pct_within_25 = round(100*mean(ratio > .75 & ratio < 1.25), 1),
            pct_we_understate = round(100*mean(ratio < 0.9), 1),
            .groups = "drop") |>
  rename(`fiscal yrs covered (6=6+)` = n_fy) |> as.data.frame())

cat("\nSame cut by AA window length in days:\n")
print(cmp |> mutate(win = cut(win_days, c(-1, 200, 400, 800, 1200, 1e5),
        labels = c("<=2 qtrs","~1 yr","1-2 yr","2-3 yr","3+ yr"))) |>
  group_by(win) |> summarise(events = n(),
    median_ratio = round(median(ratio), 3),
    pct_within_25 = round(100*mean(ratio > .75 & ratio < 1.25), 1),
    .groups = "drop") |> as.data.frame())

cat("\nINTERPRETATION: a monotone fall in median_ratio means our measurement is\n",
    "correct where observable but TRUNCATED by the 2-3 year comparative reach\n",
    "of a 10-K. That is a coverage limit, not a logic error -- and it applies\n",
    "to dCFO identically, so long-window magnitudes are understated.\n")

cat("\n\n####### (B) HOW MUCH OF THE CFO CHANGE IS PURE ROUNDING? #######\n")
# Same test as for NI, on the three section subtotals.
SUB <- c("NetCashProvidedByUsedInOperatingActivities",
         "NetCashProvidedByUsedInInvestingActivities",
         "NetCashProvidedByUsedInFinancingActivities",
         "NetCashProvidedByUsedInOperatingActivitiesContinuingOperations",
         "NetCashProvidedByUsedInInvestingActivitiesContinuingOperations",
         "NetCashProvidedByUsedInFinancingActivitiesContinuingOperations")
cf <- lc |> filter(tag %in% SUB, change_type == "Value change",
                   !is.na(orig_val), !is.na(comp_val), orig_val != 0) |>
  mutate(is_round = (comp_val == round(orig_val/1e2)*1e2) |
                    (comp_val == round(orig_val/1e3)*1e3) |
                    (comp_val == round(orig_val/1e4)*1e4) |
                    (comp_val == round(orig_val/1e5)*1e5) |
                    (comp_val == round(orig_val/1e6)*1e6),
         rel = abs(difference)/abs(orig_val))
cat("section-subtotal 'value change' rows:", format(nrow(cf), big.mark=","), "\n")
cat("explainable as pure rounding:", format(sum(cf$is_round), big.mark=","),
    sprintf("(%.1f%%)", 100*mean(cf$is_round)), "\n")
print(cf |> group_by(is_round) |> summarise(n = n(),
  p50_rel = signif(median(rel), 3), p90_rel = signif(quantile(rel,.9), 3),
  p50_abs = signif(median(abs(difference)), 3), .groups="drop") |> as.data.frame())

cat("\n--- how many would a RELATIVE threshold remove that the $1,000 floor keeps? ---\n")
for (t in c(0.0001, 0.001, 0.01)) {
  cat(sprintf("  rel change < %.2f%% : %6d rows (%.1f%%) -- of which %.1f%% are rounding\n",
    100*t, sum(cf$rel < t), 100*mean(cf$rel < t),
    100*mean(cf$is_round[cf$rel < t])))
}

cat("\n--- all line rows, not just subtotals ---\n")
al <- lc |> filter(change_type == "Value change", !is.na(orig_val),
                   !is.na(comp_val), orig_val != 0) |>
  mutate(is_round = (comp_val == round(orig_val/1e2)*1e2) |
                    (comp_val == round(orig_val/1e3)*1e3) |
                    (comp_val == round(orig_val/1e4)*1e4) |
                    (comp_val == round(orig_val/1e5)*1e5) |
                    (comp_val == round(orig_val/1e6)*1e6))
cat("rows:", format(nrow(al), big.mark=","),
    "| pure rounding:", format(sum(al$is_round), big.mark=","),
    sprintf("(%.1f%%)", 100*mean(al$is_round)), "\n")
