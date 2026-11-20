## 000 -- INVENTORY HELPERS
##
## source("001-sec-filing-extraction/000-inventory-helpers.R")
##
## Everything two people need to work through the inventory at the same time
## without stepping on each other. Four functions:
##
##   inv()                  the inventory, as 001a last built it
##   inv_todo(stage, n)     your next n filings that still need doing
##   inv_log(rows, stage)   record that you did them -- APPENDS TO YOUR FILE
##   inv_progress()         who has done what
##
## THE ONE RULE: inv_log writes only to progress_{SCF_WHO}.csv. Nobody ever
## writes the inventory parquet, and nobody ever writes anyone else's log, so
## there is no file two machines can both edit. OneDrive's failure mode here
## is silent -- it makes a second copy named after the machine and one
## person's work vanishes without an error -- so this is not a style
## preference, it is the whole reason the logs exist.
##
## Re-run 001a whenever you want the inventory to reflect the logs. The logs
## are the record; the parquet is a cached join of them.

suppressPackageStartupMessages({
  library(dplyr); library(arrow); library(readr)
})

.sec_dir <- function() {
  s <- Sys.getenv("SEC_DATA_PATH")
  if (!nzchar(s)) stop("SEC_DATA_PATH is empty -- run from the repo root so ",
                       ".Renviron loads.")
  paste0(s, "/inventory")
}

## WHO AM I. Set SCF_WHO in your own .Renviron, e.g. SCF_WHO = "matt".
## Deliberately NOT defaulted to the OS username: a wrong default would write
## into a log nobody is reading and the work would look undone.
who_am_i <- function() {
  w <- Sys.getenv("SCF_WHO")
  if (!nzchar(w)) stop(
    "SCF_WHO is not set. Add a line to your .Renviron at the repo root:\n",
    '  SCF_WHO = "matt"      (or "andrew")\n',
    "then restart R. This names your progress log and decides which filings ",
    "are yours.")
  w
}

inv <- function() {
  p <- paste0(.sec_dir(), "/filing_inventory.parquet")
  if (!file.exists(p)) stop("No inventory at ", p,
    " -- run 001a-build-filing-inventory.R first.")
  read_parquet(p)
}

## YOUR NEXT N FILINGS.
## Defaults to your own assignment, in batch order, in sample, one row per
## accession (co-registrant copies excluded -- same text, fetched once).
##
## `stage = "collect"` gives filings with no chunk text yet.
## `stage = "extract"` gives filings whose text is collected but not yet
## turned into JSON, which is the in-harness step.
inv_todo <- function(stage = c("collect", "extract"), n = 250,
                     mine_only = TRUE, who = NULL) {
  stage <- match.arg(stage)
  w <- if (is.null(who)) who_am_i() else who
  d <- inv() |> filter(in_sample, collect_primary)
  if (mine_only) d <- d |> filter(assigned_to == w)
  d <- if (stage == "collect") d |> filter(!collected)
       else                    d |> filter(collected, !extracted)
  d |> arrange(batch, cik, period_end) |> head(n)
}

## RECORD WHAT YOU DID. `rows` is a frame carrying at least accession and cik
## -- pass the frame inv_todo gave you, or a subset of it.
##
## Appends; never rewrites. An accession logged twice is fine: 001a keeps the
## latest entry per (accession, cik, stage).
inv_log <- function(rows, stage = c("collected", "extracted"),
                    locate_how = NA_character_, n_blocks = NA_integer_,
                    note = NA_character_) {
  stage <- match.arg(stage)
  stopifnot(all(c("accession", "cik") %in% names(rows)))
  w <- who_am_i()
  dir.create(paste0(.sec_dir(), "/logs"), showWarnings = FALSE, recursive = TRUE)
  p <- paste0(.sec_dir(), "/logs/progress_", w, ".csv")
  new <- tibble(
    accession  = rows$accession,
    cik        = rows$cik,
    who        = w,
    stage      = stage,
    when       = Sys.Date(),
    locate_how = if (length(locate_how) == nrow(rows)) locate_how
                 else rep(locate_how, nrow(rows)),
    n_blocks   = if (length(n_blocks) == nrow(rows)) n_blocks
                 else rep(n_blocks, nrow(rows)),
    note       = if (length(note) == nrow(rows)) note else rep(note, nrow(rows))
  )
  ## append = file exists; write_csv handles the header either way
  write_csv(new, p, append = file.exists(p))
  cat("logged", nrow(new), stage, "to", basename(p), "\n")
  invisible(new)
}

## WHERE EVERYONE IS.
inv_progress <- function() {
  d <- inv()
  tot <- d |> filter(in_sample, collect_primary)
  cat(sprintf("in sample, one row per filing: %s\n",
      format(nrow(tot), big.mark = ",")))
  ## NOTE the output columns are n_collected / n_extracted, NOT collected.
  ## summarise() evaluates in order, so naming the count `collected` makes
  ## the next line's mean(collected) read the COUNT, not the flag -- which
  ## printed "1000.0%" the first time this ran.
  print(tot |> group_by(assigned_to) |>
    summarise(filings = dplyr::n(), batches = max(batch),
              pct_collected = sprintf("%.2f%%", 100 * mean(collected)),
              n_collected = sum(collected), n_extracted = sum(extracted),
              .groups = "drop") |> as.data.frame())
  lg <- list.files(paste0(.sec_dir(), "/logs"), pattern = "\\.csv$",
                   full.names = TRUE)
  if (length(lg)) {
    all <- bind_rows(lapply(lg, read_csv, show_col_types = FALSE,
                            progress = FALSE))
    cat("\nlog entries by person and day:\n")
    print(all |> count(who, stage, when) |> arrange(desc(when)) |>
          head(10) |> as.data.frame())
  }
  invisible(NULL)
}

## ============================ Validation ============================
if (FALSE) {

  ## 1. assignment is stable: re-running 001a must not move a CIK between
  ##    people, because it is a function of the CIK and nothing else.
  d <- inv()
  stopifnot(all(d$assigned_to ==
    c("matt", "andrew")[(as.integer(d$cik) %% 2) + 1]))

  ## 2. the split is roughly even and whole firms stay together
  d |> count(assigned_to)
  d |> group_by(cik) |> summarise(n = n_distinct(assigned_to)) |>
    count(n)   ## must be 1 for every CIK

  ## 3. a round trip: claim ten, log them, rebuild, confirm they read back
  ##    as collected. Run 001a again between the log and the check.
  b <- inv_todo("collect", n = 10)
  inv_log(b, "collected", locate_how = "test", n_blocks = 3L)
  ## source 001a here, then:
  ## inv() |> filter(accession %in% b$accession) |> count(collected)
}
