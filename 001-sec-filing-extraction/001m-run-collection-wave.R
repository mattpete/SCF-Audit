## ============================================================================
## 001m  RUN ONE COLLECTION WAVE                          step 001, substep m
## Claims N filings from the inventory, scans / locates / pulls their statement
## text, files the text into the permanent year-partitioned store, and records
## the filings as collected.
## In : inventory/filing_inventory.parquet + inventory/logs/progress_*.csv
## Out: chunktext/year=YYYY/<wave>-part-N.parquet   (permanent, accumulates)
##      waves/<wave>/cache/chunks_hdr.rds           (the line ranges, kept)
##      inventory/logs/progress_{SCF_WHO}.csv       (appended: stage collected)
## ============================================================================
## WHO DOES WHAT (Matt, 2026-11-20). Matt collects the text for EVERY filing -
## so this script defaults to MINE_ONLY=FALSE and ignores the inventory's
## per-person assignment. The assignment still governs the SECOND stage, where
## Matt and Andrew split the in-harness parsing; that uses
## inv_todo("extract") with its own mine_only default of TRUE and is untouched
## by anything here.
##
## WHY WAVES RATHER THAN ONE RUN. 780,364 filings is ~34 hours of WRDS work
## over SSH or ~12 on the grid, and the locate step has never been run on more
## than 500 filings, so its behaviour at scale is unknown. A wave is restartable
## (the progress log is the record, so an interrupted wave simply re-claims the
## filings it did not finish), bounded in disk, and gives something to look at
## early. There is no "scanned" stage on purpose: a filing is collected or it is
## not, so a wave whose scans finished but whose pull failed is re-scanned,
## which at 3x on the grid is cheap and keeps the bookkeeping honest.
##
## CONFIG
##   WAVE_N     filings to claim                      (default 25000)
##   WAVE       wave name, used in file names         (default wave-YYYYMMDD-HHMM)
##   USE_GRID   "FALSE" to run the SSH scans instead  (default grid)
##   MINE_ONLY  "TRUE" to claim only your assignment  (default FALSE, see above)
##   WAVE_KEEP  "1" keeps the wave's markers          (default: deleted)
##   DRY_RUN    "1" claims and reports, runs nothing
##
## The markers are deleted at the end by default: 31 KB per filing, rebuildable,
## and they would otherwise sync to every collaborator. The line ranges
## (chunks_hdr.rds, ~96 bytes per filing) are kept, because they are the record
## of what was found and are cheap.
## ============================================================================
suppressMessages({library(arrow); library(dplyr); library(readr)})
source("001-sec-filing-extraction/000-inventory-helpers.R")

WAVE_N    <- as.integer(Sys.getenv("WAVE_N", "25000"))
WAVE      <- Sys.getenv("WAVE", format(Sys.time(), "wave-%Y%m%d-%H%M"))
USE_GRID  <- !identical(toupper(Sys.getenv("USE_GRID", "TRUE")), "FALSE")
MINE_ONLY <- identical(toupper(Sys.getenv("MINE_ONLY", "FALSE")), "TRUE")
WAVE_KEEP <- identical(Sys.getenv("WAVE_KEEP", ""), "1")
DRY_RUN   <- identical(Sys.getenv("DRY_RUN", ""), "1")

SD    <- Sys.getenv("SEC_DATA_PATH")
stopifnot(nzchar(SD))
PILOT <- paste0("waves/", WAVE)
SP    <- paste0(SD, "/", PILOT, "/cache/")
CHUNK <- paste0(SD, "/chunktext")
RS    <- file.path(R.home("bin"), "Rscript")
t_all <- Sys.time()

cat(strrep("=", 70), "\n")
cat(sprintf("WAVE %s   %s filings   %s   %s\n", WAVE, format(WAVE_N, big.mark = ","),
            if (USE_GRID) "grid" else "SSH",
            if (MINE_ONLY) paste0("mine only (", who_am_i(), ")") else "all assignments"))
cat(strrep("=", 70), "\n")

## ---- 1. claim the work -----------------------------------------------------
## inv_todo() derives `collected` from the progress logs, so this never hands
## back a filing somebody has already done - which is what makes a wave safe
## to re-run after an interruption without tracking anything itself.
## ...EXCEPT that inv_todo() reads `collected` from the inventory PARQUET, and
## the parquet is a cached join of the logs that only refreshes when 001a is
## re-run. So two waves back to back would claim the SAME filings - measured:
## after this wave logged 299, the next claim of 300 overlapped it by 299.
## That is fine for the hand-driven workflow the helpers were written for, and
## fatal for an automated wave, so the logs are subtracted here directly. Every
## person's log is read, not just this user's, because Matt collects everything
## and a filing collected by either of them is collected.
##
## BOTH FINISHED STAGES COUNT. "collected" is text filed; "notext" is scanned
## and empty. Subtracting only the first is what wedged the first full run:
## filings with no printed statement stayed in the queue, 24,572 of them piled
## up at the head of the claim, and every wave re-scanned all of them to find
## ~500 collectible ones -- 18 minutes a wave, decaying ~3% each time, with
## 70,398 never-attempted filings stuck behind them. RETRY_NOTEXT=1 ignores
## the notext side, for re-attempting a written-off set after a locator change.
logged_done <- function(stages = c("collected", "notext")) {
  lg <- list.files(paste0(SD, "/inventory/logs"), pattern = "^progress_.*\\.csv$",
                   full.names = TRUE)
  if (!length(lg)) return(character(0))
  out <- unlist(lapply(lg, function(f) {
    d <- try(read_csv(f, col_types = cols(.default = col_character())), silent = TRUE)
    if (inherits(d, "try-error") || !all(c("accession", "stage") %in% names(d)))
      return(character(0))
    d$accession[d$stage %in% stages]
  }))
  unique(out)
}

## the full uncollected list, not WAVE_N of it: the exclusion below removes an
## unknown number, and over-fetching some fixed multiple would quietly return a
## short wave once the logs grew past it
rows <- inv_todo("collect", n = 10e6, mine_only = MINE_ONLY)
RETRY_NOTEXT <- identical(Sys.getenv("RETRY_NOTEXT"), "1")
done <- logged_done(if (RETRY_NOTEXT) "collected" else c("collected", "notext"))
if (RETRY_NOTEXT) cat("RETRY_NOTEXT=1: re-claiming filings previously scanned as empty\n")
if (length(done)) {
  before <- nrow(rows)
  rows <- rows |> filter(!accession %in% done)
  cat(sprintf("excluded %s already finished in the progress logs%s\n",
              format(before - nrow(rows), big.mark = ","),
              if (RETRY_NOTEXT) " (collected only)" else " (collected or no statement)"))
}
rows <- head(rows, WAVE_N)
if (nrow(rows) == 0) { cat("\nNothing left to collect.\n"); quit(status = 0) }
cat(sprintf("\nclaimed %s filings, %s distinct CIKs, fdate %s to %s\n",
            format(nrow(rows), big.mark = ","), format(n_distinct(rows$cik), big.mark = ","),
            min(rows$fdate), max(rows$fdate)))
print(rows |> count(form_grp, is_amendment) |> as.data.frame())

if (DRY_RUN) { cat("\nDRY_RUN - claimed and reported, nothing run.\n"); quit(status = 0) }

dir.create(SP, recursive = TRUE, showWarnings = FALSE)
lst <- paste0(SP, "filings.txt")
con <- file(lst, "wb"); writeLines(sort(unique(rows$wrdsfname)), con, sep = "\n"); close(con)
## unix newlines deliberately - a CRLF list makes every remote path "not found"
## and the scan returns nothing with no error (see 000-grid-helpers.R)

## ---- 2. run the chain ------------------------------------------------------
## Subprocesses rather than source(): each step is already a standalone script
## driven by PILOT and FILE_LIST, they communicate through files anyway, and a
## failure in one cannot leave a half-built object in this session.
## system2()'s `env` argument IS IGNORED ON WINDOWS - documented, and silent.
## Passing PILOT and FILE_LIST that way sent them nowhere, the child ran with
## its defaults (the 500-filing pilot cache) and died instantly. Setting them
## in THIS process instead works everywhere, because a child inherits the
## parent's environment.
Sys.setenv(PILOT = PILOT, FILE_LIST = lst)

step <- function(label, script, env = character(0)) {
  cat(sprintf("\n---- %-22s %s\n", label, script))
  t0 <- Sys.time()
  if (length(env)) do.call(Sys.setenv, as.list(setNames(
    sub("^[^=]+=", "", env), sub("=.*$", "", env))))
  st <- system2(RS, args = shQuote(paste0("001-sec-filing-extraction/", script)),
                stdout = "", stderr = "")
  el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat(sprintf("---- %-22s %s  (%.1f min)\n", label,
              if (identical(st, 0L)) "ok" else paste("FAILED, exit", st), el))
  if (!identical(st, 0L))
    stop(label, " failed - wave ", WAVE, " not logged, nothing is marked ",
         "collected, so re-running this script re-claims the same filings.",
         call. = FALSE)
  invisible(el)
}

g <- function(a, b) if (USE_GRID) a else b
step("markers",      g("001b-grid-scan-statement-markers.R", "001b-scan-statement-markers.R"))
step("numeric lines", g("001c-grid-map-numeric-lines.R",     "001c-map-numeric-lines.R"))
step("page headers",  g("001e-grid-scan-page-headers.R",     "001e-scan-page-headers.R"))
step("locate",        "001f-locate-by-page-headers.R")

## A WAVE THAT LOCATES NOTHING IS FINISHED, NOT FAILED. It happens once the
## queue reaches a run of filings with no printed statement in them -- a batch
## of 297 amendments produced zero blocks between them. There is nothing to
## pull, and the two steps below would stop() on the empty frame, which would
## abort before anything was logged and leave the same filings to be re-claimed
## on the next wave, forever. So record the whole claim as scanned-and-empty
## and finish cleanly.
CH <- readRDS(paste0(SP, "chunks_hdr.rds"))
if (!nrow(CH)) {
  cat("\nno statement located in any of the", nrow(rows),
      "filings claimed - recording them as scanned-and-empty\n")
  inv_log(rows, "notext", note = WAVE)
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(sprintf("WAVE %s DONE in %.1f min\n", WAVE,
              as.numeric(difftime(Sys.time(), t_all, units = "mins"))))
  cat(sprintf("  collected   0 of %s claimed\n", format(nrow(rows), big.mark = ",")))
  cat(sprintf("  no text     %s  (logged as scanned-and-empty, not re-claimed)\n",
              format(nrow(rows), big.mark = ",")))
  quit(status = 0)
}

step("pull text",     "001g-export-chunk-text.R")

## ---- 3. file the text into the permanent store -----------------------------
D <- readRDS(paste0(SP, "candtext.rds"))
D <- D[!is.na(D$text) & nzchar(D$text), ]
if (nrow(D) == 0)
  stop("no statement text came back for any filing - NOT logging anything.",
       call. = FALSE)

## period_end is the research-relevant date and candtext does not carry it, so
## it comes off the claimed rows; the partition is its year, which is how a
## collaborator can take one year rather than the whole store.
D <- D |>
  left_join(rows |> select(wrdsfname, accession, period_end, fyear, regsic),
            by = "wrdsfname") |>
  mutate(wave = WAVE,
         year = ifelse(is.na(fyear), as.integer(format(fdate, "%Y")), as.integer(fyear)))

## basename_template carries the wave name so each wave adds files BESIDE the
## existing ones instead of replacing a partition - repeated waves accumulate.
##
## existing_data_behavior MUST be "overwrite", which despite the name only
## replaces files whose names collide, leaving every other file in the
## partition alone. NOT "delete_matching", which would empty the whole
## year= partition and destroy every earlier wave's text in it. (This arrow
## build rejects "overwrite_or_ignore" outright, which is how the distinction
## came up.)
write_dataset(D, CHUNK, partitioning = "year", format = "parquet",
              compression = "zstd",
              basename_template = paste0(WAVE, "-part-{i}.parquet"),
              existing_data_behavior = "overwrite")
cat(sprintf("\nfiled %s blocks for %s filings into %s (partitioned by year)\n",
            format(nrow(D), big.mark = ","),
            format(n_distinct(D$wrdsfname), big.mark = ","), CHUNK))

## ---- 4. record what was collected ------------------------------------------
## TWO LOGS, both written: `collected` for filings that produced statement
## text, `notext` for the rest. Those used to be left unlogged "to be retried
## in a later wave", on the assumption that empties were a small residue (193
## paper filings carry a header and no document). They are not a small
## residue -- 24,572 of them accumulated at the head of the queue and were
## re-scanned on every wave of a 59-hour run, which is what the notext stage
## fixes. A `notext` entry is recoverable: it names the wave, and RETRY_NOTEXT=1
## re-claims them.
C <- readRDS(paste0(SP, "chunks_hdr.rds"))
per <- C |> group_by(wrdsfname) |>
  summarise(n_blocks = n(),
            all3 = all(c("cf", "is", "bs") %in% kind),
            win  = any(kind == "win"), .groups = "drop") |>
  mutate(locate_how = ifelse(all3, "all3", ifelse(win, "window", "partial")))

## DROP the inventory's OWN locate_how / n_blocks before joining. The inventory
## carries both as derived columns (001a computes them from the logs), so a
## plain join suffixes them to .x/.y, `ok$locate_how` comes back NULL, and
## inv_log() silently records NA for every row - which is how this wave first
## wrote 299 log entries with no locate_how in them.
ok <- rows |> filter(wrdsfname %in% unique(D$wrdsfname)) |>
  select(-any_of(c("locate_how", "n_blocks"))) |>
  left_join(per, by = "wrdsfname")
stopifnot(all(c("locate_how", "n_blocks", "accession", "cik") %in% names(ok)))
miss <- nrow(rows) - nrow(ok)
inv_log(ok, "collected", locate_how = ok$locate_how, n_blocks = ok$n_blocks,
        note = WAVE)

## RECORD THE EMPTIES TOO. Every claimed filing reached all five steps by the
## time this line runs -- any scan that came back short aborts the wave before
## here (grid_run() errors on a missing slice, and each step is checked), so a
## filing with no block is one the locator genuinely found no statement in,
## not one nothing looked at. Writing that down is what lets the queue move
## past it; leaving it unlogged is the bug this replaces, which cost 59 hours
## and left 70,398 filings unreached. Recoverable either way: the entry names
## this wave, and RETRY_NOTEXT=1 re-claims them.
bad <- rows |> filter(!accession %in% ok$accession)
if (nrow(bad)) inv_log(bad, "notext", note = WAVE)

## ---- 5. tidy up ------------------------------------------------------------
## Three classes of leftover, and only one is worth keeping:
##   markers / numeric map   31 KB per filing, rebuildable from the raw files
##   candtext + candidates   ~20 KB per filing, and a DUPLICATE of what was
##                           just written to chunktext/ - at full scale that is
##                           15 GB of the same text stored twice, syncing to
##                           every collaborator, which is the whole thing this
##                           store exists to avoid
##   chunks_hdr.rds          ~70 bytes per filing, the record of WHAT WAS FOUND
##                           and not reconstructible without re-scanning
## So the first two go and the third stays. WAVE_KEEP=1 keeps everything, for
## debugging a wave that behaved oddly.
if (!WAVE_KEEP) {
  n0 <- sum(file.size(list.files(paste0(SD, "/", PILOT), recursive = TRUE,
                                 full.names = TRUE)), na.rm = TRUE)
  for (f in c("markers4_raw.rds", "markers_hdr.rds", "numlines.rds", "candtext.rds"))
    if (file.exists(paste0(SP, f))) unlink(paste0(SP, f))
  unlink(paste0(SD, "/", PILOT, "/extract"), recursive = TRUE)
  n1 <- sum(file.size(list.files(paste0(SD, "/", PILOT), recursive = TRUE,
                                 full.names = TRUE)), na.rm = TRUE)
  cat(sprintf("cleaned up %.1f MB of rebuildable and duplicated intermediates; kept %.0f KB (the line ranges)\n",
              (n0 - n1) / 1e6, n1 / 1024))
}

cat("\n", strrep("=", 70), "\n", sep = "")
cat(sprintf("WAVE %s DONE in %.1f min\n", WAVE,
            as.numeric(difftime(Sys.time(), t_all, units = "mins"))))
cat(sprintf("  collected   %s of %s claimed (%.1f%%)\n",
            format(nrow(ok), big.mark = ","), format(nrow(rows), big.mark = ","),
            100 * nrow(ok) / nrow(rows)))
cat(sprintf("  no text     %s  (logged as scanned-and-empty, not re-claimed)\n",
            format(miss, big.mark = ",")))
print(ok |> count(locate_how) |> mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))) |>
      as.data.frame())
cat("\nRe-run 001a-build-filing-inventory.R to fold this into the inventory,\n",
    "then inv_progress() to see where things stand.\n", sep = "")
