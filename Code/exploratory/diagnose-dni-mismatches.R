# WHY don't the rest tie? Test specific explanations, in priority order, and
# count how many cases each one accounts for.
#
# Hypotheses, most plausible first:
#   H1 AMENDMENT CHANNEL. AA codes the restated figure as the filer disclosed
#      it, which for a formal restatement is the 10-K/A. Our ni_diff uses the
#      COMPARATIVE (orig - comp). Those are different numbers whenever both
#      channels exist and disagree. Test: does orig - amend match AA better?
#   H2 SCALE. Ratio is a clean power of ten (BestGofer was exactly 1000x).
#   H3 INCOMPLETE COVERAGE. We lack a net income line for some fiscal years the
#      AA window covers, so our sum is over fewer years than AA cumulated.
#   H4 WINDOW REACHES OUTSIDE THE PANEL. AA's window starts before our data or
#      implies more fiscal years than we matched.
#   H5 NONCONTROLLING INTERESTS. Gap equals the NCI line (ProfitLoss includes
#      NCI, NetIncomeLoss does not).
#   H6 DISCONTINUED OPERATIONS. Gap equals the discontinued-ops line.
#   H7 OVERLAPPING AA EVENTS. Another AA event for the same firm covers some of
#      the same periods, so AA's chg_ni refers to a different slice.
#   H8 SIGN ONLY. Magnitude ties but direction does not.

suppressMessages({library(dplyr); library(arrow); library(glue); library(stringr); library(tidyr)})
dp <- Sys.getenv("DATA_PATH"); options(width = 210, scipen = 999)

NI_PRI <- c("NetIncomeLoss" = 1L, "ProfitLoss" = 2L, "[Income starting point]" = 3L)
NCI <- "NetIncomeLossAttributableToNoncontrollingInterest"
DISC <- c("IncomeLossFromDiscontinuedOperationsNetOfTax",
          "IncomeLossFromDiscontinuedOperationsNetOfTaxAttributableToReportingEntity")

lc <- read_parquet(glue("{dp}scf_line_changes_full.parquet"),
       col_select = c("cik","start","end","tag","orig_val","comp_val","amend_val",
                      "difference","amend_difference","in_original","in_comparative",
                      "in_amendment"))

ni <- lc |> filter(tag %in% names(NI_PRI)) |>
  mutate(pri = unname(NI_PRI[tag])) |>
  group_by(cik, start, end) |> arrange(pri, .by_group = TRUE) |> slice(1) |> ungroup() |>
  transmute(cik, start, end, ni_tag = tag,
            ni_diff = difference, ni_amend_diff = amend_difference,
            ni_has_amend = coalesce(in_amendment, FALSE),
            ni_both = coalesce(in_original, FALSE) & coalesce(in_comparative, FALSE))

nci <- lc |> filter(tag == NCI) |>
  group_by(cik, start, end) |> summarise(nci_diff = sum(difference, na.rm=TRUE), .groups="drop")
dsc <- lc |> filter(tag %in% DISC) |>
  group_by(cik, start, end) |> summarise(disc_diff = sum(difference, na.rm=TRUE), .groups="drop")

ab <- read_parquet(glue("{dp}aa_event_periods.parquet")) |>
  left_join(ni,  by = c("cik","period_start"="start","period_end"="end")) |>
  left_join(nci, by = c("cik","period_start"="start","period_end"="end")) |>
  left_join(dsc, by = c("cik","period_start"="start","period_end"="end"))

evb <- read_parquet(glue("{dp}aa_events_scf_characterised.parquet")) |>
  filter(matched_panel, aa_source == "restatement") |>
  select(aa_event_id, cik, com_name, chg_ni, win_days, misstate_begin, misstate_end)

# other AA events overlapping the same firm-periods
ov <- evb |> count(cik, name = "n_events_this_firm")

ev <- ab |> group_by(aa_event_id) |>
  summarise(n_fy = n(), n_fy_ni = sum(!is.na(ni_diff)),
            our_comp  = sum(ni_diff, na.rm = TRUE),
            our_amend = sum(coalesce(ni_amend_diff, ni_diff), na.rm = TRUE),
            any_amend = any(ni_has_amend, na.rm = TRUE),
            all_both  = all(ni_both, na.rm = TRUE),
            nci_sum = sum(nci_diff, na.rm = TRUE),
            disc_sum = sum(disc_diff, na.rm = TRUE),
            all_nil = all(ni_tag == "NetIncomeLoss", na.rm = TRUE),
            .groups = "drop") |>
  inner_join(evb, by = "aa_event_id") |>
  left_join(ov, by = "cik") |>
  mutate(aa = as.numeric(chg_ni)) |>
  filter(aa != 0, our_comp != 0, n_fy_ni > 0) |>
  # fiscal years AA's window spans, vs how many we actually have NI for
  mutate(fy_span = pmax(1, round(win_days / 365)),
         ratio_comp  = abs(our_comp)  / abs(aa),
         ratio_amend = abs(our_amend) / abs(aa),
         tie_comp  = ratio_comp  > .75 & ratio_comp  < 1.25,
         tie_amend = ratio_amend > .75 & ratio_amend < 1.25)

cat("events in comparison:", nrow(ev), "\n")
cat("tie on COMPARATIVE basis :", sum(ev$tie_comp),
    sprintf("(%.1f%%)", 100*mean(ev$tie_comp)), "\n")
cat("tie on AMENDMENT basis   :", sum(ev$tie_amend),
    sprintf("(%.1f%%)", 100*mean(ev$tie_amend)), "\n")
cat("tie on EITHER            :", sum(ev$tie_comp | ev$tie_amend),
    sprintf("(%.1f%%)", 100*mean(ev$tie_comp | ev$tie_amend)), "\n")

cat("\n### H1: does the amendment basis fix cases the comparative misses? ###\n")
print(ev |> count(tie_comp, tie_amend) |> as.data.frame())
cat("among events that HAVE an amendment (n =", sum(ev$any_amend), "):\n")
print(ev |> filter(any_amend) |> summarise(
  n = n(), tie_comp = sum(tie_comp), tie_amend = sum(tie_amend),
  pct_comp = round(100*mean(tie_comp),1), pct_amend = round(100*mean(tie_amend),1)) |>
  as.data.frame())

# ---------- classify the misses ----------
miss <- ev |> filter(!tie_comp)
cat("\n\n#################### CLASSIFYING", nrow(miss), "MISSES ####################\n")

pow10 <- function(r) { l <- log10(r); abs(l - round(l)) < 0.02 & round(l) != 0 }
near <- function(a, b, tol = 0.25) abs(a - b) <= tol * pmax(abs(a), abs(b), 1)

miss <- miss |> mutate(
  gap = abs(our_comp) - abs(aa),
  reason = case_when(
    tie_amend                              ~ "H1 amendment basis ties instead",
    pow10(ratio_comp)                      ~ "H2 scale error (power of 10)",
    n_fy_ni < n_fy                         ~ "H3 missing NI for some covered years",
    fy_span > n_fy_ni                       ~ "H4 AA window spans more years than we have",
    near(abs(gap), abs(nci_sum)) & nci_sum != 0 ~ "H5 noncontrolling interests",
    near(abs(gap), abs(disc_sum)) & disc_sum != 0 ~ "H6 discontinued operations",
    n_events_this_firm > 1                 ~ "H7 firm has multiple AA events",
    ratio_comp > .75 & ratio_comp < 1.25   ~ "H8 sign only",
    TRUE                                   ~ "unexplained"))

cat("\n### dominant reason (first match wins, in the order above) ###\n")
print(miss |> count(reason, sort = TRUE) |>
  mutate(pct_of_misses = round(100*n/sum(n),1),
         pct_of_all = round(100*n/nrow(ev),1)) |> as.data.frame())

cat("\n### how many misses would EACH explanation cover on its own? ###\n")
print(tibble(
  explanation = c("H1 amendment ties","H2 power of 10","H3 missing NI years",
                  "H4 window longer than our coverage","H5 NCI","H6 disc ops",
                  "H7 multiple AA events","H8 sign only"),
  n = c(sum(miss$tie_amend), sum(pow10(miss$ratio_comp)),
        sum(miss$n_fy_ni < miss$n_fy), sum(miss$fy_span > miss$n_fy_ni),
        sum(near(abs(miss$gap), abs(miss$nci_sum)) & miss$nci_sum != 0),
        sum(near(abs(miss$gap), abs(miss$disc_sum)) & miss$disc_sum != 0),
        sum(miss$n_events_this_firm > 1),
        sum(miss$ratio_comp > .75 & miss$ratio_comp < 1.25))) |>
  mutate(pct_of_misses = round(100*n/nrow(miss),1)) |> arrange(desc(n)) |>
  as.data.frame())

cat("\n### the power-of-10 cases in detail ###\n")
p <- miss |> filter(pow10(ratio_comp))
cat("n =", nrow(p), "\n")
if (nrow(p)) print(p |> mutate(factor = round(10^round(log10(ratio_comp)))) |>
  count(factor) |> as.data.frame())

cat("\n### unexplained misses: what do they look like? ###\n")
u <- miss |> filter(reason == "unexplained")
cat("n =", nrow(u), "\n")
if (nrow(u)) {
  print(round(quantile(u$ratio_comp, c(.05,.25,.5,.75,.95)), 3))
  print(u |> arrange(desc(abs(aa))) |>
    transmute(com_name = str_sub(com_name,1,28), n_fy, n_fy_ni, fy_span,
              aa, our_comp, ratio = round(ratio_comp,2), all_nil) |>
    head(15) |> as.data.frame())
}

cat("\n### if we take the BEST of the two bases, where do we land? ###\n")
ev2 <- ev |> mutate(best = pmin(abs(ratio_comp - 1), abs(ratio_amend - 1)),
                    tie_best = tie_comp | tie_amend)
print(ev2 |> summarise(
  n = n(),
  tie_comp_only = sum(tie_comp),  pct_comp = round(100*mean(tie_comp),1),
  tie_best = sum(tie_best), pct_best = round(100*mean(tie_best),1)) |> as.data.frame())
