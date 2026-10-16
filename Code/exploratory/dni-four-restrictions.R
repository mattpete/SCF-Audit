# Tightened version of the dNI-to-AA check: isolate the cases where our figure
# and AA's SHOULD be the same quantity, and see whether they are.
#
# The point is to separate two explanations for the dispersion in the loose
# comparison (44% within 25%):
#   (a) our comparison logic is noisy, or
#   (b) the two numbers are different constructs in the loose sample.
# If the exact-match rate rises sharply as the constructs are forced to align,
# it is (b) and the logic is validated. If it stays flat, it is (a).
#
# Four restrictions, reported alone AND cumulatively so it is visible which one
# does the work:
#   R1 single-fiscal-year AA window  -- no cumulation on either side
#   R2 annual (12-month) basis       -- not a YTD stub against AA's full year
#   R3 NetIncomeLoss tagged          -- parent-only, matching AA's "net income"
#   R4 NI line on both sides         -- a true value change, not Added/Dropped

suppressMessages({library(dplyr); library(arrow); library(glue); library(stringr)})
dp <- Sys.getenv("DATA_PATH"); options(width = 200, scipen = 999)

NI_PRIORITY <- c("NetIncomeLoss" = 1L, "ProfitLoss" = 2L,
                 "[Income starting point]" = 3L)

lc <- read_parquet(glue("{dp}scf_line_changes_full.parquet"),
       col_select = c("cik","start","end","tag","orig_val","comp_val","difference",
                      "change_type","in_original","in_comparative"))

ni <- lc |> filter(tag %in% names(NI_PRIORITY)) |>
  mutate(pri = unname(NI_PRIORITY[tag])) |>
  group_by(cik, start, end) |> arrange(pri, .by_group = TRUE) |> slice(1) |>
  ungroup() |>
  transmute(cik, start, end, ni_tag = tag, ni_diff = difference,
            ni_both_sides = coalesce(in_original, FALSE) &
                            coalesce(in_comparative, FALSE))

ab <- read_parquet(glue("{dp}aa_event_periods.parquet")) |>
  left_join(ni, by = c("cik","period_start"="start","period_end"="end"))

ev_base <- read_parquet(glue("{dp}aa_events_scf_characterised.parquet")) |>
  filter(matched_panel, aa_source == "restatement") |>
  select(aa_event_id, com_name, chg_ni, win_days)

ev <- ab |> group_by(aa_event_id) |>
  summarise(n_fy = n(),
            n_fy_ni = sum(!is.na(ni_diff)),
            our_dNI = sum(ni_diff, na.rm = TRUE),
            all_annual = all(period_length == "annual"),
            all_netincomeloss = all(ni_tag == "NetIncomeLoss", na.rm = TRUE),
            all_both_sides = all(ni_both_sides, na.rm = TRUE),
            .groups = "drop") |>
  inner_join(ev_base, by = "aa_event_id") |>
  mutate(aa_dNI = as.numeric(chg_ni)) |>
  filter(aa_dNI != 0, our_dNI != 0, n_fy_ni > 0) |>
  mutate(ratio = abs(our_dNI) / abs(aa_dNI),
         sign_ok = sign(our_dNI) == -sign(aa_dNI),
         # "exact" = agrees to within 0.5%, which is tighter than any rounding
         # difference between the two sources could produce
         exact   = ratio > 0.995 & ratio < 1.005,
         w10     = ratio > 0.90  & ratio < 1.10,
         w25     = ratio > 0.75  & ratio < 1.25,
         R1 = n_fy == 1,
         R2 = all_annual,
         R3 = all_netincomeloss,
         R4 = all_both_sides)

sm <- function(d, lab) tibble(
  subset = lab, n = nrow(d),
  median_ratio = if (nrow(d)) round(median(d$ratio), 3) else NA,
  pct_exact = if (nrow(d)) round(100*mean(d$exact), 1) else NA,
  pct_w10   = if (nrow(d)) round(100*mean(d$w10), 1) else NA,
  pct_w25   = if (nrow(d)) round(100*mean(d$w25), 1) else NA,
  pct_sign  = if (nrow(d)) round(100*mean(d$sign_ok), 1) else NA)

cat("############ BASELINE (the loose comparison) ############\n")
print(sm(ev, "all events, both non-zero") |> as.data.frame())

cat("\n############ EACH RESTRICTION ALONE ############\n")
print(bind_rows(
  sm(ev, "none (baseline)"),
  sm(ev |> filter(R1), "R1 single fiscal year only"),
  sm(ev |> filter(R2), "R2 annual basis only"),
  sm(ev |> filter(R3), "R3 NetIncomeLoss only"),
  sm(ev |> filter(R4), "R4 both sides only")) |> as.data.frame())

cat("\n############ CUMULATIVE, IN ORDER ############\n")
print(bind_rows(
  sm(ev, "none"),
  sm(ev |> filter(R1), "+R1 single fiscal year"),
  sm(ev |> filter(R1, R2), "+R2 annual basis"),
  sm(ev |> filter(R1, R2, R3), "+R3 NetIncomeLoss"),
  sm(ev |> filter(R1, R2, R3, R4), "+R4 both sides")) |> as.data.frame())

cat("\n############ WHICH RESTRICTION DOES THE WORK? ############\n")
cat("Marginal gain in pct_w25 from ADDING each restriction last:\n")
full <- ev |> filter(R1, R2, R3, R4)
for (drop in c("R1","R2","R3","R4")) {
  keep <- setdiff(c("R1","R2","R3","R4"), drop)
  d3 <- ev
  for (k in keep) d3 <- d3[d3[[k]], ]
  cat(sprintf("  without %s: n=%4d pct_w25=%5.1f   |  with all four: n=%4d pct_w25=%5.1f   -> %s adds %+5.1f pp\n",
      drop, nrow(d3), 100*mean(d3$w25), nrow(full), 100*mean(full$w25),
      drop, 100*mean(full$w25) - 100*mean(d3$w25)))
}

cat("\n############ THE TIGHT SUBSET, IN DETAIL ############\n")
cat("n =", nrow(full), "\n")
print(round(quantile(full$ratio, c(.05,.1,.25,.5,.75,.9,.95)), 3))
cat("\nexact (within 0.5%):", sum(full$exact),
    sprintf("(%.1f%%)", 100*mean(full$exact)), "\n")
cat("within 25%:", sum(full$w25), sprintf("(%.1f%%)", 100*mean(full$w25)), "\n")
cat("sign agrees:", sum(full$sign_ok), sprintf("(%.1f%%)", 100*mean(full$sign_ok)), "\n")

cat("\n-- the misses in the tight subset, largest AA figures --\n")
print(full |> filter(!w25) |> arrange(desc(abs(aa_dNI))) |>
  transmute(com_name = str_sub(com_name, 1, 30), aa_dNI, our_dNI,
            ratio = round(ratio, 2), sign_ok) |> head(12) |> as.data.frame())

cat("\n-- a sample of the exact matches, for eyeballing --\n")
print(full |> filter(exact) |> arrange(desc(abs(aa_dNI))) |>
  transmute(com_name = str_sub(com_name, 1, 30), aa_dNI, our_dNI,
            ratio = round(ratio, 3)) |> head(10) |> as.data.frame())
