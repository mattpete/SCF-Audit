## ============================================================================
## 001b-grid  SCAN STATEMENT MARKERS, ON THE WRDS GRID    step 001, substep b
## Same scan as 001b-scan-statement-markers.R, run as a 100-slice SGE array
## job instead of over one held SSH connection.
## In : cache/dev_d.rds, cache/loc_d.rds  (or FILE_LIST, see below)
## Out: cache/markers4_raw{OUT_SUFFIX}.rds
## ============================================================================
## 001b IS STILL THE AUTHORITATIVE SCRIPT. The patterns are not copied here -
## they are read out of it at run time by grid_patterns(), so there is exactly
## one definition of PAT and NAME_PAT and it lives in 001b. Keep 001b for a
## small run, a debug session, or whenever the grid is busy; this script exists
## only to make a full-population pass take hours instead of days.
##
## Config, all via environment variables so nothing here needs editing:
##   PILOT       which cache directory          (default pilot-2026-11)
##   FILE_LIST   an .rds/.txt of wrdsfnames to scan instead of dev_d/loc_d
##   OUT_SUFFIX  appended to the output name    (use for verification runs)
##   GRID_TASKS  slices                         (default 100)
##   GRID_SLOTS  concurrent tasks               (default 10, the WRDS cap)
##   GRID_KEEP   "1" leaves the job dir on scratch for inspection
## ============================================================================
source("001-sec-filing-extraction/000-grid-helpers.R")

PILOT <- Sys.getenv("PILOT", "pilot-2026-11")
SP    <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT, "/cache/")
SUF   <- Sys.getenv("OUT_SUFFIX", "")
SRC   <- "001-sec-filing-extraction/001b-scan-statement-markers.R"
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")))

## ---- the filing list -------------------------------------------------------
## FILE_LIST is how a real collection wave is driven: hand it the wrdsfnames
## from inv_todo() rather than the pilot's two hand-drawn cache files. Falling
## back to dev_d/loc_d keeps this script a drop-in twin of 001b for the
## existing samples.
FILE_LIST <- Sys.getenv("FILE_LIST", "")
if (nzchar(FILE_LIST)) {
  d <- if (grepl("\\.rds$", FILE_LIST)) readRDS(FILE_LIST) else readLines(FILE_LIST)
  if (is.data.frame(d)) d <- d$wrdsfname
  d <- data.frame(wrdsfname = unique(as.character(d)), form = NA_character_,
                  set = "list", stringsAsFactors = FALSE)
  cat("filing list from", FILE_LIST, "\n")
} else {
  dev <- readRDS(paste0(SP, "dev_d.rds")); tst <- readRDS(paste0(SP, "loc_d.rds"))
  d <- unique(rbind(cbind(dev[, c("wrdsfname", "form")], set = "dev"),
                    cbind(tst[, c("wrdsfname", "form")], set = "test")))
  d <- d[!duplicated(d$wrdsfname), ]
}
cat("filings to scan:", nrow(d), "\n")

## ---- the patterns, read out of 001b ----------------------------------------
P <- grid_patterns(SRC)
cat(sprintf("patterns read from %s: PAT %d chars, NAME_PAT %d chars\n",
            basename(SRC), nchar(P$PAT), nchar(P$NAME_PAT)))

## ---- run -------------------------------------------------------------------
acc <- grid_run(d$wrdsfname, P$remote, jobname = "scfmk",
                keep = identical(Sys.getenv("GRID_KEEP"), "1"))

M <- grid_parse_markers(acc)
M <- merge(M, d, by = "wrdsfname")
grid_save_markers(M, paste0(SP, "markers4_raw", SUF, ".rds"), nrow(d))
