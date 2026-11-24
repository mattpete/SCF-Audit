## ============================================================================
## 001j  FILE A TRANSCRIBED BATCH                        step 001, substep j
## Turns the model's per-statement JSON into tidy rows, appends them to the
## line-item store, and records the filings as extracted.
## In : extract-batches/{batch}/res/c###.json  + manifest.csv
## Out: rows/year=YYYY/{batch}-0.parquet       the line-item dataset
##      rows-foot/year=YYYY/{batch}-0.parquet  the footing check
##      inventory/logs/progress_{who}.csv      stage = extracted
## ============================================================================
## WHY THE SCRIPT LOGS AND NOT THE MODEL. inv_log is the record of what has
## been done; if a model could call it, work could be marked done that was
## never transcribed and nothing downstream would ever notice. So only filings
## with a res/c###.json that parses, and that 001h turned into rows, get logged.
## A statement the model skipped stays unlogged and comes back in a later batch.
##
## WHY found=FALSE IS NOT AN ERROR. The locator sometimes hands over the wrong
## block. The model is told to say so rather than transcribe the wrong
## statement, and those filings are deliberately NOT logged, so they are
## re-offered later with a better candidate. See 001h, which drops them.
##
## PARTITIONED BY YEAR, SORTED BY CIK. Year of filing, matching chunktext/, so
## a batch only ever adds files and never rewrites a partition. Rows are
## arranged by cik before writing so the row-group min/max statistics are tight
## and a one-company query reads a handful of row groups instead of the lot.
##
## CONFIG
##   BATCH   which batch to file  (required)
## ============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(arrow); library(readr); library(jsonlite)
})
source("001-sec-filing-extraction/000-inventory-helpers.R")

BATCH <- Sys.getenv("BATCH")
if (!nzchar(BATCH)) stop("set BATCH to the batch name 001i printed")
SD  <- Sys.getenv("SEC_DATA_PATH"); stopifnot(nzchar(SD))
DIR <- paste0(SD, "/extract-batches/", BATCH)
if (!dir.exists(DIR)) stop("no such batch: ", DIR)

man <- read_csv(paste0(DIR, "/manifest.csv"), show_col_types = FALSE,
                col_types = cols(cik = col_character(), .default = col_guess()))

## ---- 1. assemble the model's JSON into one jsonl, metadata from manifest ---
fs <- list.files(paste0(DIR, "/res"), pattern = "^c[0-9]+[.]json$", full.names = TRUE)
cat("transcribed:", length(fs), "of", nrow(man), "\n")
if (!length(fs)) stop("nothing in ", DIR, "/res -- transcribe the batch first")

jl <- paste0(DIR, "/extracted.jsonl")
con <- file(jl, "w", encoding = "UTF-8"); n_ok <- 0L; bad <- character()
for (f in sort(fs)) {
  i <- as.integer(sub("^c0*", "", sub("[.]json$", "", basename(f))))
  r <- man[man$idx == i, ]
  if (nrow(r) != 1) { bad <- c(bad, basename(f)); next }
  res <- try(fromJSON(f, simplifyVector = FALSE), silent = TRUE)
  if (inherits(res, "try-error")) { bad <- c(bad, basename(f)); next }
  writeLines(toJSON(list(result = res, wrdsfname = r$wrdsfname, cik = r$cik,
                         coname = r$coname, form = r$form,
                         fdate = format(r$fdate), rdate = format(r$rdate)),
                    auto_unbox = TRUE, null = "null", na = "null"), con)
  n_ok <- n_ok + 1L
}
close(con)
if (length(bad)) cat("UNREADABLE, not filed:", paste(bad, collapse = ", "), "\n")
cat("assembled", n_ok, "records ->", jl, "\n\n")

## ---- 2. 001h turns them into tidy rows + the footing check -----------------
## Called as a subprocess so this script and 001h cannot disagree about the
## frame: there is one implementation of the rescaling and the footing test.
RS <- file.path(R.home("bin"), "Rscript")
Sys.setenv(TIDY_IN = jl, TIDY_OUT = paste0(DIR, "/"))
st <- system2(RS, args = shQuote("001-sec-filing-extraction/001h-build-tidy-frame.R"),
              stdout = "", stderr = "")
if (!identical(st, 0L)) stop("001h failed (exit ", st, ") -- nothing logged")

L <- read_parquet(paste0(DIR, "/scf_text_lines.parquet"))
F <- read_parquet(paste0(DIR, "/scf_text_foot.parquet"))
if (!nrow(L)) stop("001h produced no rows -- nothing logged")

## ---- 3. append to the store, partitioned by year, sorted by cik ------------
## 001h carries wrdsfname but not accession, and accession is what the
## inventory and the progress logs key on, so derive it once here and use the
## same derivation for the rows, the footing table and the logging below.
acc_of <- function(x) sub("[.]txt$", "", basename(x))
L <- L |> mutate(accession = acc_of(wrdsfname),
                 year = as.integer(format(as.Date(fdate), "%Y"))) |>
  arrange(cik, fdate, statement, line_order, col_order)
write_dataset(L, paste0(SD, "/rows"), partitioning = "year", format = "parquet",
              basename_template = paste0(BATCH, "-{i}.parquet"),
              existing_data_behavior = "overwrite")

## the footing table carries rdate but not fdate, so take the filing date from
## the lines -- both stores must partition on the same year or a batch lands in
## two different partitions and "all of 2014" means two different things
if (nrow(F)) {
  F <- F |> left_join(L |> distinct(wrdsfname, fdate), by = "wrdsfname") |>
    mutate(accession = acc_of(wrdsfname),
           year = as.integer(format(as.Date(fdate), "%Y"))) |> arrange(cik, fdate)
  write_dataset(F, paste0(SD, "/rows-foot"), partitioning = "year", format = "parquet",
                basename_template = paste0(BATCH, "-{i}.parquet"),
                existing_data_behavior = "overwrite")
}

## ---- 4. log only what actually produced rows ------------------------------
done <- man |> filter(accession %in% unique(L$accession))
inv_log(done |> select(accession, cik), "extracted", note = BATCH)

skipped <- man |> filter(!accession %in% done$accession)
cat("\n", strrep("-", 60), "\n", sep = "")
cat("filed   ", nrow(done), "filings,", nrow(L), "line rows ->", paste0(SD, "/rows"), "\n")
cat("logged  ", nrow(done), "as extracted in progress_", who_am_i(), ".csv\n", sep = "")
if (nrow(skipped)) {
  cat("NOT logged (re-offered in a later batch):", nrow(skipped), "\n")
  print(as.data.frame(skipped |> select(idx, accession, form, fdate)), row.names = FALSE)
}
if (nrow(F)) {
  cat("\nfooting:\n"); print(table(F$verdict))
  ## TWO DIFFERENT OUTCOMES, TWO DIFFERENT ACTIONS. An arithmetic
  ## contradiction means the reader got a number or a column wrong, so a
  ## stronger model is worth paying for. A subtotal that was never found
  ## usually means the located text is truncated (K-Tron FY2001 lost its
  ## financing section at a page break) and no reader can recover text that is
  ## not there, so those go to a queue for a human to look at the filing.
  ok  <- c("foots", "foots (no roll-forward printed)")
  esc <- F |> filter(!verdict %in% c(ok, "not testable")) |> distinct(accession)
  rev <- F |> filter(verdict == "not testable") |> distinct(accession)
  if (nrow(esc)) {
    cat("\nDOES NOT ADD UP -- re-transcribe on a stronger model (runbook step 5):\n")
    print(as.data.frame(esc), row.names = FALSE)
  }
  if (nrow(rev)) {
    cat("\nSUBTOTAL NOT FOUND -- check the filing by hand; the located text is\n",
        "probably cut short. Do NOT pay for a re-read:\n", sep = "")
    print(as.data.frame(rev), row.names = FALSE)
  }
}
