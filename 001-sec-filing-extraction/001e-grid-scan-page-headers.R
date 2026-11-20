## ============================================================================
## 001e-grid  SCAN FOR STATEMENT PAGE HEADERS, ON THE WRDS GRID
##                                                        step 001, substep e
## Same scan as 001e-scan-page-headers.R, run as a 100-slice SGE array job.
## In : cache/dev_d.rds, cache/loc_d.rds  (or FILE_LIST)
## Out: cache/markers_hdr{OUT_SUFFIX}.rds
## ============================================================================
## 001e IS STILL THE AUTHORITATIVE SCRIPT - its PAT and NAME_PAT are read out
## of it at run time, so there is one definition and it lives there. See the
## header of 001b-grid for the full config list and the reasoning.
##
## Note this scan carries the awk line-pair join, which writes its own /tmp
## scratch file. On the grid that write lands on the task node's local disk
## rather than the login node shared with ~260 users, which is strictly better
## than where it used to go.
## ============================================================================
source("001-sec-filing-extraction/000-grid-helpers.R")

PILOT <- Sys.getenv("PILOT", "pilot-2026-11")
SP    <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT, "/cache/")
SUF   <- Sys.getenv("OUT_SUFFIX", "")
SRC   <- "001-sec-filing-extraction/001e-scan-page-headers.R"
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")))

## ---- the filing list -------------------------------------------------------
FILE_LIST <- Sys.getenv("FILE_LIST", "")
if (nzchar(FILE_LIST)) {
  fl <- if (grepl("\\.rds$", FILE_LIST)) readRDS(FILE_LIST) else readLines(FILE_LIST)
  if (is.data.frame(fl)) fl <- fl$wrdsfname
  fl <- sort(unique(as.character(fl)))
  cat("filing list from", FILE_LIST, "\n")
} else {
  fl <- sort(unique(c(readRDS(paste0(SP, "dev_d.rds"))$wrdsfname,
                      readRDS(paste0(SP, "loc_d.rds"))$wrdsfname)))
}
cat("filings to scan:", length(fl), "\n")

## ---- the patterns, read out of 001e ----------------------------------------
P <- grid_patterns(SRC)
cat(sprintf("patterns read from %s: PAT %d chars, NAME_PAT %d chars\n",
            basename(SRC), nchar(P$PAT), nchar(P$NAME_PAT)))

## ---- run -------------------------------------------------------------------
acc <- grid_run(fl, P$remote, jobname = "scfhdr",
                keep = identical(Sys.getenv("GRID_KEEP"), "1"))

M <- grid_parse_markers(acc)
grid_save_markers(M, paste0(SP, "markers_hdr", SUF, ".rds"), length(fl))
