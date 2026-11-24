## ============================================================================
## 001l  STAGE A RE-TRANSCRIPTION WORKLIST                step 001, substep l
## Turns the scan queue into "redo these exact files, in these batches", so a
## stronger model re-reads only the statements that did not add up.
## In : extract-status/scan_queue.csv   written by 001k
##      extract-batches/*/manifest.csv  to map a filing back to its c###.txt
## Out: extract-status/rescan_worklist.csv   batch, file, filing, why
##      prints a ready-to-paste instruction per batch
## ============================================================================
## WHY A REDO OVERWRITES IN PLACE, AND MUST. The line store is partitioned by
## year and each batch writes files named after itself, so filing a *new* batch
## containing corrected versions would leave the old rows sitting beside them
## and the filing would appear twice. Instead the better model overwrites
## res/c###.json inside the ORIGINAL batch directory, and 001j is re-run for
## that whole batch: it rebuilds every row from the res files as they now
## stand and overwrites the same parquet names. One version of a filing, always.
##
## WHICH FAILURES ARE WORTH A STRONGER MODEL. Only status rescan_arithmetic -
## the statement contradicts itself, so a number or a column was read wrong.
## check_no_subtotal is excluded by default: the located text is usually cut
## short at a page break and no reader can recover text that is not there.
## Pass INCLUDE_NOSUB=1 to stage those too, for a human to look at.
## ============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr)
})

SD <- Sys.getenv("SEC_DATA_PATH"); stopifnot(nzchar(SD))
OD <- paste0(SD, "/extract-status")
QF <- paste0(OD, "/scan_queue.csv")
if (!file.exists(QF)) stop("no scan queue yet - run 001k first")
INCLUDE_NOSUB <- nzchar(Sys.getenv("INCLUDE_NOSUB"))

q <- read_csv(QF, show_col_types = FALSE,
              col_types = cols(cik = col_character(), .default = col_guess()))
if (!INCLUDE_NOSUB) q <- q |> filter(status == "rescan_arithmetic")
if (!nrow(q)) { cat("nothing to re-transcribe.\n"); quit(status = 0) }

## ---- map each filing back to the batch and file it came from ---------------
mans <- list.files(paste0(SD, "/extract-batches"), pattern = "^manifest[.]csv$",
                   recursive = TRUE, full.names = TRUE)
if (!length(mans)) stop("no batch manifests under ", SD, "/extract-batches")
man <- bind_rows(lapply(mans, function(f) {
  read_csv(f, show_col_types = FALSE,
           col_types = cols(cik = col_character(), .default = col_guess())) |>
    mutate(batch = basename(dirname(f))) |>
    select(batch, idx, accession, cik, coname, form, fdate, chars)
}))

w <- q |>
  select(accession, status, n_cols, n_ok, worst_diff) |>
  inner_join(man, by = "accession") |>
  mutate(file = sprintf("c%03d.txt", idx),
         res  = sprintf("res/c%03d.json", idx)) |>
  arrange(batch, idx)

missing <- setdiff(q$accession, man$accession)
write_csv(w, paste0(OD, "/rescan_worklist.csv"))

## ---- report ----------------------------------------------------------------
cat("\n=== ", nrow(w), " statements to re-transcribe, in ",
    n_distinct(w$batch), " batch(es) ===\n\n", sep = "")
print(as.data.frame(w |> select(batch, file, coname, form, fdate,
                                n_cols, n_ok, worst_diff, chars)), row.names = FALSE)
if (length(missing))
  cat("\nNOT staged - no manifest holds these accessions (batch deleted?):",
      paste(missing, collapse = ", "), "\n")

cat("\n", strrep("=", 70), "\n", sep = "")
cat("HAND THIS TO A STRONGER MODEL, one block per batch:\n")
for (b in unique(w$batch)) {
  f <- w$file[w$batch == b]
  cat("\n--- batch ", b, " (", length(f), " files) ---\n", sep = "")
  cat("Re-transcribe ONLY these files in\n")
  cat("  ", SD, "/extract-batches/", b, "/\n", sep = "")
  cat("  ", paste(f, collapse = ", "), "\n", sep = "")
  cat("Follow prompt/system-prompt.txt and prompt/output-schema.json exactly,\n")
  cat("and OVERWRITE the existing res/c###.json for each one.\n")
  cat("Each of these statements failed its own arithmetic, so the likely error\n")
  cat("is a mistyped number, a missed line, or a value put in the wrong column\n")
  cat("on a row that printed fewer numbers than there are columns.\n")
}
cat("\nThen re-file the whole batch, which rebuilds every row from res/:\n")
for (b in unique(w$batch))
  cat("  BATCH=", b, " Rscript 001-sec-filing-extraction/001j-file-extract-batch.R\n", sep = "")
cat("\nwrote ", OD, "/rescan_worklist.csv\n", sep = "")
