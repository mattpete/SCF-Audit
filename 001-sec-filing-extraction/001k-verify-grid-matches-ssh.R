## ============================================================================
## 001k  VERIFY THE GRID SCANS MATCH THE SSH SCANS        step 001, substep k
## In : cache/markers4_raw.rds + markers4_raw_grid.rds
##      cache/markers_hdr.rds  + markers_hdr_grid.rds
##      cache/numlines.rds     + numlines_grid.rds
## Out: nothing. Prints a verdict and exits non-zero if anything differs.
## ============================================================================
## WHY. The grid scripts read PAT and NAME_PAT out of the SSH scripts, so the
## patterns cannot drift - but three things still can: the marker parser
## (grid_parse_markers is a second copy of a loop that could not be read out of
## the originals), grid_body()'s rewriting of the list temp file, and the
## round-robin slicing, which changes the ORDER output arrives in. The last one
## matters: both scans dedupe and 001f re-sorts by line, so order is supposed
## to be irrelevant, and this script is where that assumption gets tested
## rather than assumed.
##
## HOW TO RUN IT. Produce both halves on the same sample, then compare:
##
##   PILOT=holdout-2026-11 Rscript 001-sec-filing-extraction/001b-scan-statement-markers.R
##   PILOT=holdout-2026-11 OUT_SUFFIX=_grid Rscript 001-sec-filing-extraction/001b-grid-scan-statement-markers.R
##   ... same for 001c and 001e ...
##   PILOT=holdout-2026-11 Rscript 001-sec-filing-extraction/001k-verify-grid-matches-ssh.R
##
## The SSH halves already exist for holdout-2026-11, so in practice only the
## _grid halves need running.
##
## A PASS HERE IS THE WHOLE LICENCE TO USE THE GRID SCRIPTS. Run it again after
## touching any pattern, either scan's parse loop, or the helpers.
## ============================================================================
PILOT <- Sys.getenv("PILOT", "holdout-2026-11")
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/", PILOT, "/cache/")
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")))
cat("comparing in", SP, "\n\n")

fails <- character(0)
note  <- function(...) cat(sprintf(...))

## ---- the two marker tables -------------------------------------------------
## Keyed on filing + line + text, which is the whole content of a marker row.
## Order is deliberately NOT compared: the grid slices round-robin so rows
## arrive interleaved, and both scans dedupe afterwards. If a difference shows
## up here it is a real difference in WHAT was found, not in what order.
cmp_markers <- function(nm) {
  a <- paste0(SP, nm, ".rds"); b <- paste0(SP, nm, "_grid.rds")
  if (!file.exists(a) || !file.exists(b)) {
    note("%-16s SKIP  (missing %s)\n", nm,
         paste(basename(c(a, b)[!file.exists(c(a, b))]), collapse = " + "))
    return(invisible(NULL))
  }
  A <- readRDS(a); B <- readRDS(b)
  k <- c("wrdsfname", "line", "text")
  ka <- do.call(paste, c(A[k], sep = "|"))
  kb <- do.call(paste, c(B[k], sep = "|"))
  lost <- setdiff(ka, kb); gain <- setdiff(kb, ka)
  ok <- length(lost) == 0 && length(gain) == 0
  note("%-16s ssh %7d rows / %4d filings   grid %7d rows / %4d filings\n",
       nm, nrow(A), length(unique(A$wrdsfname)), nrow(B), length(unique(B$wrdsfname)))
  note("%-16s %s   lost %d   gained %d\n", "",
       if (ok) "IDENTICAL" else "*** DIFFERS ***", length(lost), length(gain))
  ## also check the duplicate count matches - a dedupe that fired differently
  ## would leave the key sets equal but the row counts apart
  if (ok && nrow(A) != nrow(B)) {
    note("%-16s *** same keys but %d vs %d rows: duplicate handling differs\n",
         "", nrow(A), nrow(B))
    ok <- FALSE
  }
  if (!ok) {
    fails <<- c(fails, nm)
    if (length(lost)) { note("  only in SSH (first 5):\n"); cat("   ", head(lost, 5), sep = "\n    ") }
    if (length(gain)) { note("\n  only in GRID (first 5):\n"); cat("   ", head(gain, 5), sep = "\n    ") }
    cat("\n")
  }
  cat("\n")
}

cmp_markers("markers4_raw")
cmp_markers("markers_hdr")

## ---- the numeric map -------------------------------------------------------
## A named list of integer vectors, one per filing, so this is an exact
## identical() on the sorted-by-name version - the line numbers within a
## filing must match element for element.
local({
  a <- paste0(SP, "numlines.rds"); b <- paste0(SP, "numlines_grid.rds")
  if (!file.exists(a) || !file.exists(b)) {
    note("%-16s SKIP  (missing %s)\n", "numlines",
         paste(basename(c(a, b)[!file.exists(c(a, b))]), collapse = " + "))
    return(invisible(NULL))
  }
  A <- readRDS(a); B <- readRDS(b)
  note("%-16s ssh %d filings   grid %d filings\n", "numlines", length(A), length(B))
  on <- sort(names(A)); bn <- sort(names(B))
  if (!identical(on, bn)) {
    note("%-16s *** DIFFERS *** filing sets differ: ssh-only %d, grid-only %d\n",
         "", length(setdiff(on, bn)), length(setdiff(bn, on)))
    fails <<- c(fails, "numlines"); return(invisible(NULL))
  }
  bad <- names(A)[!vapply(on, function(n) identical(A[[n]], B[[n]]), logical(1))]
  if (length(bad)) {
    note("%-16s *** DIFFERS *** %d filings with different line numbers\n", "", length(bad))
    note("  e.g. %s: ssh %d lines, grid %d lines\n",
         bad[1], length(A[[bad[1]]]), length(B[[bad[1]]]))
    fails <<- c(fails, "numlines")
  } else {
    note("%-16s IDENTICAL   (all %d filings, line for line)\n", "", length(A))
  }
  cat("\n")
})

## ---- verdict ---------------------------------------------------------------
cat(strrep("-", 70), "\n")
if (length(fails)) {
  cat("FAILED:", paste(unique(fails), collapse = ", "), "\n")
  cat("Do NOT use the grid scripts for a real wave until this passes.\n")
  quit(status = 1)
}
cat("PASS - the grid path reproduces the SSH path exactly.\n")
