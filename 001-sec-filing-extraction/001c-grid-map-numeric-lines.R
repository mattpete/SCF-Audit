## ============================================================================
## 001c-grid  MAP NUMERIC LINES, ON THE WRDS GRID         step 001, substep c
## Same scan as 001c-map-numeric-lines.R, run as an SGE array job.
## In : cache/markers4_raw.rds  (or FILE_LIST)
## Out: cache/numlines{OUT_SUFFIX}.rds
## ============================================================================
## The cheapest of the three scans by a wide margin - 0.018 s/filing over SSH
## against 0.078 and 0.094 - because its pattern is tiny, it has no -i, and it
## returns line NUMBERS rather than line text. Four hours for the full
## population rather than seventeen or twenty, so this is the one where the
## grid matters least. It is here for completeness and so a wave can be run
## with one mechanism rather than two.
##
## 001c remains authoritative; its remote body is read out of it at run time.
## See the header of 001b-grid for the config list.
##
## Output format differs from the marker scans - one line per filing,
## "#<wrdsfname> <lineno> <lineno> ..." - so it has its own parse rather than
## grid_parse_markers().
## ============================================================================
source("001-sec-filing-extraction/000-grid-helpers.R")

PILOT <- Sys.getenv("PILOT", "pilot-2026-11")
SP    <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT, "/cache/")
SUF   <- Sys.getenv("OUT_SUFFIX", "")
SRC   <- "001-sec-filing-extraction/001c-map-numeric-lines.R"
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")))

## ---- the filing list -------------------------------------------------------
FILE_LIST <- Sys.getenv("FILE_LIST", "")
if (nzchar(FILE_LIST)) {
  ff <- if (grepl("\\.rds$", FILE_LIST)) readRDS(FILE_LIST) else readLines(FILE_LIST)
  if (is.data.frame(ff)) ff <- ff$wrdsfname
  ff <- unique(as.character(ff))
  cat("filing list from", FILE_LIST, "\n")
} else {
  ## markers3.rds is an early intermediate a fresh sample never produces, so
  ## fall back to what 001b writes - same rule as the SSH version
  mf <- paste0(SP, "markers3.rds")
  if (!file.exists(mf)) mf <- paste0(SP, "markers4_raw.rds")
  ff <- unique(readRDS(mf)$wrdsfname)
}
cat("filings:", length(ff), "\n")

## ---- the remote body, read out of 001c -------------------------------------
P <- grid_patterns(SRC)

## ---- run -------------------------------------------------------------------
acc <- grid_run(ff, P$remote, jobname = "scfnum",
                keep = identical(Sys.getenv("GRID_KEEP"), "1"))

## ---- parse (mirrors the loop at the foot of 001c) --------------------------
## No paste(collapse) - strsplit() is element-wise and the lines arrive already
## split, so concatenating them first only risks R's 2^31-1 byte string limit.
## That limit is not hypothetical: the same construct in grid_parse_markers()
## killed a 25,000-filing wave outright. One line per filing here rather than
## one per marker, so it has more headroom, but the fix costs nothing.
ln <- if (any(grepl("\n", acc, fixed = TRUE)))
        unlist(strsplit(acc, "\n", fixed = TRUE)) else acc
ln <- ln[startsWith(ln, "#")]
NUM <- list()
for (x in ln) {
  sp <- strsplit(sub("^#", "", trimws(x)), " +")[[1]]
  if (length(sp) < 2) next
  NUM[[sp[1]]] <- as.integer(sp[-1])
}
cat("\nfilings with numeric map:", length(NUM), "of", length(ff), "\n")
cat("median numeric lines per filing:",
    median(vapply(NUM, length, numeric(1))), "\n")

## same collapse guard as the marker scans: a pattern that reached the node
## broken returns nothing for every filing, and saving that would destroy the
## cache silently
out <- paste0(SP, "numlines", SUF, ".rds")
if (length(NUM) == 0 || length(NUM) < length(ff) / 2)
  stop(sprintf("numeric map collapsed: %d of %d filings. NOT saving - %s unchanged.",
               length(NUM), length(ff), out), call. = FALSE)
saveRDS(NUM, out)
cat("saved", out, "\n")
