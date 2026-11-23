## ============================================================================
## 001n  RUN WAVES UNTIL THE INVENTORY IS COLLECTED      step 001, substep n
## Calls 001m over and over until nothing is left to collect.
## In : whatever 001m needs
## Out: whatever 001m writes, plus waves/run-log.csv
## ============================================================================
## WHY A LOOP RATHER THAN ONE BIG WAVE. The whole population in a single wave
## would hold ~833 MILLION marker rows in memory at once - measured 114 bytes
## per row, so **91 GB** - which is most of a 127 GB machine and would likely
## take it down. A 25,000-filing wave is 2.9 GB. The wave size is therefore not
## just about restartability, it is the thing that keeps this inside the
## machine, and 001n exists so that bound does not have to be managed by hand.
##
## WALL TIME IS THE REAL COST, AND IT IS NOT SMALL. Budget for the full
## 780,364 filings, from measured per-filing rates:
##
##   001b  markers        grid   0.033 s/filing    7.2 h
##   001c  numeric lines  grid   ~0.018            4.0 h
##   001e  page headers   grid   0.033             7.2 h
##   001f  locate         LOCAL  0.034             7.4 h
##   001g  pull text      SSH    0.084            18.2 h
##                                              -------
##                                                ~44 h
##
## So roughly **two days of the laptop being on and connected**, not the 12
## hours the grid figure alone suggests - because only the three scans are
## parallelised. The pull is the single largest piece at 18 hours and is still
## one held SSH connection; porting it to the grid is the obvious next win and
## would take the total to about 32 hours.
##
## WHAT THE FIRST FULL RUN ACTUALLY COST, and the bug it exposed. 117 waves,
## 58.9 h, 685,394 of 780,364 collected -- then it wedged. 001m used to leave a
## filing it found no statement in UNLOGGED, "retried in a later wave", which
## was fine while such filings were rare and transient. They are not: 24,572 of
## them (85% amendments, which amend one item without reprinting the financials)
## piled up at the head of the claim and were re-scanned on EVERY wave, so the
## frontier advanced ~500 filings per 18-minute wave instead of ~24,000 per 90,
## decaying ~3% a wave. Behind that wall sat 70,398 filings nothing had ever
## looked at -- the whole top CIK decile, 9.5% collected against ~96%
## everywhere else, which read as a coverage problem with modern filings and
## was nothing of the kind. 001m now logs them as `notext` and both stages
## count as finished here, so the queue cannot wedge again.
##
## SAFE TO INTERRUPT AT ANY POINT. Every wave is independently resumable: 001m
## logs nothing until a wave's text is actually filed, and it subtracts the
## progress logs directly rather than trusting the inventory parquet. So
## killing this mid-run loses at most the wave in flight, and re-running picks
## up exactly where it stopped. There is no state here that needs cleaning up.
##
## CONFIG
##   WAVE_N      filings per wave            (default 25000, ~2.9 GB of markers)
##   MAX_WAVES   stop after this many        (default 0 = until done)
##   MAX_FAILS   consecutive failures before giving up      (default 3)
##   USE_GRID    "FALSE" for the SSH scans   (default grid)
## ============================================================================
suppressMessages({library(dplyr); library(readr)})
source("001-sec-filing-extraction/000-inventory-helpers.R")

WAVE_N    <- as.integer(Sys.getenv("WAVE_N", "25000"))
MAX_WAVES <- as.integer(Sys.getenv("MAX_WAVES", "0"))
MAX_FAILS <- as.integer(Sys.getenv("MAX_FAILS", "3"))
SD  <- Sys.getenv("SEC_DATA_PATH"); stopifnot(nzchar(SD))
RS  <- file.path(R.home("bin"), "Rscript")
RUN <- paste0(SD, "/waves/run-log.csv")
dir.create(paste0(SD, "/waves"), recursive = TRUE, showWarnings = FALSE)

## THE RUN LOG GAINED A COLUMN, so an old one has to move aside rather than be
## appended to. write_csv(append = TRUE) does not check the header: it would
## write 8 values under the 7-column header of a pre-notext run and every row
## after would be silently misaligned. Same failure inv_log() now guards
## against, same remedy -- keep the old file, start a new one.
RUN_COLS <- c("wave", "started", "minutes", "exit", "collected", "no_text",
              "done_after", "left_after")
if (file.exists(RUN)) {
  hdr <- names(readr::read_csv(RUN, n_max = 0, show_col_types = FALSE))
  if (!identical(hdr, RUN_COLS)) {
    bak <- sub("[.]csv$", paste0("-", format(Sys.Date(), "%Y%m%d"), ".csv"), RUN)
    file.rename(RUN, bak)
    cat("run log schema changed; previous log kept at", basename(bak), "\n")
  }
}

## how much is left, read from the logs rather than the inventory parquet, for
## the same reason 001m does it: the parquet only refreshes when 001a re-runs
## A filing is FINISHED when it is either collected or logged notext (scanned,
## no printed statement). Counting only the collected ones here would leave
## `left` permanently above zero and loop until MAX_FAILS, so both stages
## count towards done and `collected` is reported separately.
remaining <- function() {
  lg <- list.files(paste0(SD, "/inventory/logs"), pattern = "^progress_.*\\.csv$",
                   full.names = TRUE)
  st <- bind_rows(lapply(lg, function(f) {
    d <- try(read_csv(f, col_types = cols(.default = col_character())), silent = TRUE)
    if (inherits(d, "try-error") || !all(c("accession", "stage") %in% names(d)))
      return(NULL)
    d[c("accession", "stage")]
  }))
  got  <- unique(st$accession[st$stage == "collected"])
  none <- setdiff(unique(st$accession[st$stage == "notext"]), got)
  tot  <- inv() |> filter(in_sample, collect_primary) |> nrow()
  fin  <- length(got) + length(none)
  list(total = tot, done = fin, collected = length(got), no_text = length(none),
       left = tot - fin)
}

r0 <- remaining()
cat(strrep("=", 70), "\n")
cat(sprintf("COLLECT ALL   %s filings total, %s done, %s to go\n",
            format(r0$total, big.mark = ","), format(r0$done, big.mark = ","),
            format(r0$left, big.mark = ",")))
cat(sprintf("              of those done: %s collected, %s scanned with no statement in them\n",
            format(r0$collected, big.mark = ","), format(r0$no_text, big.mark = ",")))
cat(sprintf("%s per wave => about %d waves, roughly %.0f h at the measured rate\n",
            format(WAVE_N, big.mark = ","), ceiling(r0$left / WAVE_N),
            r0$left * 0.202 / 3600))
cat("Safe to interrupt: a wave logs nothing until its text is filed.\n")
cat(strrep("=", 70), "\n")

i <- 0L; fails <- 0L; t_all <- Sys.time()
repeat {
  r <- remaining()
  if (r$left <= 0) { cat("\nNothing left to collect.\n"); break }
  if (MAX_WAVES > 0 && i >= MAX_WAVES) {
    cat(sprintf("\nStopping: MAX_WAVES=%d reached, %s still to go.\n",
                MAX_WAVES, format(r$left, big.mark = ","))); break }
  if (fails >= MAX_FAILS) {
    cat(sprintf("\nStopping: %d waves failed in a row. Nothing is in a bad ",
                "state - read the error above, fix it, and re-run.\n", fails)); break }

  i <- i + 1L
  w <- format(Sys.time(), "wave-%Y%m%d-%H%M%S")
  cat(sprintf("\n%s\nWAVE %d  (%s)  %s of %s remaining\n%s\n", strrep("-", 70),
              i, w, format(min(WAVE_N, r$left), big.mark = ","),
              format(r$left, big.mark = ","), strrep("-", 70)))
  t0 <- Sys.time()
  Sys.setenv(WAVE = w, WAVE_N = as.character(WAVE_N))
  st <- system2(RS, args = shQuote("001-sec-filing-extraction/001m-run-collection-wave.R"),
                stdout = "", stderr = "")
  el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  r2 <- remaining()
  ## `got` is PROGRESS, not collection: a wave that finds nothing collectible
  ## in its 25,000 has still moved the queue forward by writing them off, and
  ## counting only collections would call that a failure and trip MAX_FAILS
  ## while the run was working correctly. The two are reported separately.
  got  <- r2$done - r$done
  coll <- r2$collected - r$collected

  write_csv(tibble(wave = w, started = format(t0), minutes = round(el, 1),
                   exit = as.integer(st), collected = coll, no_text = got - coll,
                   done_after = r2$done, left_after = r2$left),
            RUN, append = file.exists(RUN))

  if (identical(st, 0L) && got > 0) {
    fails <- 0L
    cat(sprintf("\nwave %d ok in %.0f min: +%s collected, +%s empty  |  %s left  |  eta %.1f h\n",
                i, el, format(coll, big.mark = ","), format(got - coll, big.mark = ","),
                format(r2$left, big.mark = ","), r2$left / got * el / 60))
  } else {
    fails <- fails + 1L
    cat(sprintf("\nwave %d FAILED (exit %s, +%d done). %d in a row.\n",
                i, st, got, fails))
    ## a failed wave logged nothing, so the next pass re-claims the same
    ## filings - which is right when the cause was transient (a dropped SSH
    ## connection, a grid node going away) and a loop when it is not, hence
    ## MAX_FAILS
  }
}

cat("\n", strrep("=", 70), "\n", sep = "")
r <- remaining()
cat(sprintf("DONE after %d waves in %.1f h\n",
            i, as.numeric(difftime(Sys.time(), t_all, units = "hours"))))
cat(sprintf("  collected        %10s of %s (%.1f%%)\n",
            format(r$collected, big.mark = ","), format(r$total, big.mark = ","),
            100 * r$collected / r$total))
cat(sprintf("  no statement     %10s  (scanned, nothing in them to collect)\n",
            format(r$no_text, big.mark = ",")))
cat(sprintf("  still to collect %10s\n", format(r$left, big.mark = ",")))
cat("run log:", RUN, "\n")
cat("Re-run 001a-build-filing-inventory.R to fold the logs into the inventory.\n")
