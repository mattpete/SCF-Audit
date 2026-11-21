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

## how much is left, read from the logs rather than the inventory parquet, for
## the same reason 001m does it: the parquet only refreshes when 001a re-runs
remaining <- function() {
  lg <- list.files(paste0(SD, "/inventory/logs"), pattern = "^progress_.*\\.csv$",
                   full.names = TRUE)
  done <- unique(unlist(lapply(lg, function(f) {
    d <- try(read_csv(f, col_types = cols(.default = col_character())), silent = TRUE)
    if (inherits(d, "try-error") || !all(c("accession", "stage") %in% names(d)))
      return(character(0))
    d$accession[d$stage == "collected"]
  })))
  tot <- inv() |> filter(in_sample, collect_primary) |> nrow()
  list(total = tot, done = length(done), left = tot - length(done))
}

r0 <- remaining()
cat(strrep("=", 70), "\n")
cat(sprintf("COLLECT ALL   %s filings total, %s already done, %s to go\n",
            format(r0$total, big.mark = ","), format(r0$done, big.mark = ","),
            format(r0$left, big.mark = ",")))
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
  got <- r2$done - r$done

  write_csv(tibble(wave = w, started = format(t0), minutes = round(el, 1),
                   exit = as.integer(st), collected = got,
                   done_after = r2$done, left_after = r2$left),
            RUN, append = file.exists(RUN))

  if (identical(st, 0L) && got > 0) {
    fails <- 0L
    done_min <- as.numeric(difftime(Sys.time(), t_all, units = "mins"))
    cat(sprintf("\nwave %d ok: +%s collected in %.0f min  |  %s done, %s left  |  eta %.1f h\n",
                i, format(got, big.mark = ","), el, format(r2$done, big.mark = ","),
                format(r2$left, big.mark = ","),
                if (got > 0) r2$left / got * el / 60 else NA_real_))
  } else {
    fails <- fails + 1L
    cat(sprintf("\nwave %d FAILED (exit %s, +%d collected). %d in a row.\n",
                i, st, got, fails))
    ## a failed wave logged nothing, so the next pass re-claims the same
    ## filings - which is right when the cause was transient (a dropped SSH
    ## connection, a grid node going away) and a loop when it is not, hence
    ## MAX_FAILS
  }
}

cat("\n", strrep("=", 70), "\n", sep = "")
r <- remaining()
cat(sprintf("DONE after %d waves in %.1f h: %s of %s collected (%.1f%%), %s left\n",
            i, as.numeric(difftime(Sys.time(), t_all, units = "hours")),
            format(r$done, big.mark = ","), format(r$total, big.mark = ","),
            100 * r$done / r$total, format(r$left, big.mark = ",")))
cat("run log:", RUN, "\n")
cat("Re-run 001a-build-filing-inventory.R to fold the logs into the inventory.\n")
