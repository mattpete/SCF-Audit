## ============================================================================
## 001i  CLAIM A BATCH TO TRANSCRIBE                     step 001, substep i
## Picks your next N uncollected-but-not-yet-extracted filings and writes each
## located statement out as its own text file for a model to read in harness.
## In : chunktext/ (what 001g filed), inventory/ (what 001a built)
## Out: extract-batches/{batch}/c###.txt  one file per statement
##      extract-batches/{batch}/manifest.csv  idx -> filing metadata
##      extract-batches/{batch}/res/         empty; the model writes here
## ============================================================================
## WHY THE MODEL NEVER PICKS ITS OWN WORK. Two people run this at once. If the
## model chose filings it would choose the same ones twice, and nothing would
## stop it logging work it had not done. So claiming is a script, logging is a
## script (001j), and the model only ever reads c###.txt and writes res/c###.json.
## That is also what makes two different people's runs comparable: the inputs
## are picked the same way regardless of who is driving.
##
## WHY ONE FILE PER STATEMENT. A batch read as one blob blows the context and
## gets truncated silently in the middle of a number. One file per statement
## means the model reads a bounded thing, and a missing res/c###.json is a
## visible gap rather than a quietly short answer.
##
## CONFIG (env or edit here)
##   BATCH_N    statements per batch        (default 25)
##   KIND       which statement to pull     (default "cf")
##   BATCH      batch name                  (default extract-{who}-{timestamp})
## ============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(arrow); library(readr)
})
source("001-sec-filing-extraction/000-inventory-helpers.R")

BATCH_N <- as.integer(Sys.getenv("BATCH_N", "25"))
KIND    <- Sys.getenv("KIND", "cf")
WHO     <- who_am_i()
BATCH   <- Sys.getenv("BATCH", paste0("extract-", WHO, "-",
                                      format(Sys.time(), "%Y%m%d-%H%M%S")))
SD  <- Sys.getenv("SEC_DATA_PATH"); stopifnot(nzchar(SD))
DIR <- paste0(SD, "/extract-batches/", BATCH)

## ---- 1. whose turn, and which filings -------------------------------------
## inv_todo already filters to filings that are collected but not extracted,
## and to this person's CIK assignment, so two people never claim the same row.
todo <- inv_todo("extract", n = BATCH_N * 3, mine_only = TRUE)
if (!nrow(todo)) { cat("nothing left to extract for", WHO, "\n"); quit(status = 0) }

## ---- 2. the located statements for those filings --------------------------
## rank 1 only: the locator's best candidate. A filing whose rank-1 block is
## wrong shows up as found=FALSE in the transcription and gets re-queued, which
## is cheaper than transcribing every candidate up front.
ct <- open_dataset(paste0(SD, "/chunktext")) |>
  filter(accession %in% todo$accession, kind == KIND, rank == 1) |>
  select(wrdsfname, accession, cik, coname, form_wrds, fdate, rdate, chars, text) |>
  collect() |>
  arrange(cik, fdate) |>
  head(BATCH_N)

if (!nrow(ct)) { cat("no", KIND, "blocks for the claimed filings\n"); quit(status = 0) }

## ---- 3. write them out ----------------------------------------------------
dir.create(paste0(DIR, "/res"), recursive = TRUE, showWarnings = FALSE)
ct$idx <- seq_len(nrow(ct))
for (i in seq_len(nrow(ct))) {
  r <- ct[i, ]
  con <- file(sprintf("%s/c%03d.txt", DIR, r$idx), "w", encoding = "UTF-8")
  writeLines(c(sprintf("CHUNK %d | WANTED=%s | %s | %s | fdate=%s | rdate=%s",
                       r$idx, KIND, r$form_wrds, r$coname,
                       format(r$fdate), format(r$rdate)),
               "", r$text), con)
  close(con)
}

## the manifest carries every identifier, so the model never retypes metadata
## and a transcription cannot be filed against the wrong filing
write_csv(ct |> select(idx, wrdsfname, accession, cik, coname,
                       form = form_wrds, fdate, rdate, chars),
          paste0(DIR, "/manifest.csv"))

cat("batch:", BATCH, "\n")
cat("wrote", nrow(ct), KIND, "statements to", DIR, "\n")
cat("ciks:", n_distinct(ct$cik), " fdate:", format(min(ct$fdate)), "to",
    format(max(ct$fdate)), "\n")
cat("chars: median", median(ct$chars), " max", max(ct$chars), "\n\n")
cat("Now transcribe them: see 001-sec-filing-extraction/prompt/EXTRACT-RUNBOOK.md\n")
cat("Then file them:  BATCH=", BATCH, " Rscript 001-sec-filing-extraction/001j-file-extract-batch.R\n", sep = "")
