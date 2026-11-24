## ============================================================================
## 001k  FOOTING STATUS, ONE ROW PER FILING               step 001, substep k
## Reads everything filed so far and says, per filing, whether its cash flow
## statement adds up - and if not, which of the two different problems it has.
## In : rows-foot/   the per-column footing check 001h writes
## Out: extract-status/footing_status.csv   one row per filing, every batch
##      extract-status/scan_queue.csv       the ones needing a human or a re-read
## ============================================================================
## WHY A SEPARATE SCRIPT. The footing check runs per batch, so its escalate /
## review lists only ever describe the batch just filed. This rebuilds the
## whole picture from the accumulated store, so progress can be read at any
## time without re-running anything, and a filing fixed in a later batch stops
## appearing in the queue.
##
## THE TWO PROBLEMS ARE NOT THE SAME AND MUST NOT SHARE A QUEUE.
##   arithmetic  the sections do not sum to the printed net change, or the cash
##               balance does not roll forward. The reader got a number or a
##               column wrong, so re-transcribing on a stronger model is worth
##               paying for.
##   no_subtotal a section subtotal was never found. Usually the located text
##               is cut short at a page break (K-Tron FY2001 lost its financing
##               section that way), and no reader can recover text that is not
##               there, so this wants a human looking at the filing - NOT a
##               paid re-read.
## Measured on the 91 Walmart statements the detection was built against: 203
## of 203 period-columns foot and neither queue has an entry, so anything
## landing in either one is worth taking seriously rather than shrugging at.
## ============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(arrow); library(readr)
})

SD <- Sys.getenv("SEC_DATA_PATH"); stopifnot(nzchar(SD))
FD <- paste0(SD, "/rows-foot")
OD <- paste0(SD, "/extract-status")
if (!dir.exists(FD)) stop("nothing filed yet: no ", FD)
dir.create(OD, recursive = TRUE, showWarnings = FALSE)

F <- open_dataset(FD) |> collect()
OK <- c("foots", "foots (no roll-forward printed)")

## ---- one row per filing ----------------------------------------------------
## A filing is only "ok" when EVERY period-column it prints adds up. A 10-K
## shows three years side by side and a wrong cell sits in just one of them, so
## taking the best column would hide exactly the error being looked for.
st <- F |>
  mutate(col_ok = verdict %in% OK,
         col_arith = !verdict %in% c(OK, "not testable"),
         col_nosub = verdict == "not testable") |>
  group_by(accession, cik, coname, form, fdate, rdate, year) |>
  summarise(n_cols      = n(),
            n_ok        = sum(col_ok),
            n_arith     = sum(col_arith),
            n_nosub     = sum(col_nosub),
            worst_diff  = suppressWarnings(max(abs(diff_net), na.rm = TRUE)),
            .groups = "drop") |>
  mutate(worst_diff = ifelse(is.finite(worst_diff), worst_diff, NA_real_),
         ## arithmetic outranks a missing subtotal: if any column of the filing
         ## contradicts itself the whole filing is worth re-reading, whatever
         ## the other columns did
         status = case_when(n_arith > 0 ~ "rescan_arithmetic",
                            n_nosub > 0 ~ "check_no_subtotal",
                            TRUE        ~ "ok"),
         pct_ok = round(100 * n_ok / n_cols, 1))

write_csv(st, paste0(OD, "/footing_status.csv"))

q <- st |> filter(status != "ok") |> arrange(status, cik, fdate)
write_csv(q, paste0(OD, "/scan_queue.csv"))

## ---- report ----------------------------------------------------------------
cat("\n=== filings filed so far:", nrow(st), " period-columns:", sum(st$n_cols), "===\n\n")
print(table(st$status))
cat(sprintf("\ncolumns that add up: %d of %d (%.1f%%)\n",
            sum(st$n_ok), sum(st$n_cols), 100 * sum(st$n_ok) / sum(st$n_cols)))
cat(sprintf("filings clean       : %d of %d (%.1f%%)\n",
            sum(st$status == "ok"), nrow(st),
            100 * sum(st$status == "ok") / nrow(st)))

cat("\n--- by filing year ---\n")
print(as.data.frame(st |> group_by(year) |>
  summarise(filings = n(), clean = sum(status == "ok"),
            pct = round(100 * clean / filings, 1), .groups = "drop")),
  row.names = FALSE)

if (nrow(q)) {
  cat("\n--- scan queue (", nrow(q), " filings ) ---\n", sep = "")
  print(as.data.frame(q |> select(status, cik, coname, form, fdate,
                                  n_cols, n_ok, worst_diff)), row.names = FALSE)
} else {
  cat("\nscan queue empty - every filing filed so far adds up.\n")
}
cat("\nwrote", paste0(OD, "/footing_status.csv"), "and scan_queue.csv\n")
