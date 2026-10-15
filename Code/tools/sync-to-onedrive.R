# =============================================================================
# tools/sync-to-onedrive.R - stage every file the project needs onto OneDrive
# =============================================================================
# PURPOSE
#   OneDrive is the single source of truth for data shared between collaborators.
#   This script copies the files the pipeline actually reads out of their two
#   original homes and into one OneDrive `Data` folder, preserving the subfolder
#   layout the scripts expect.
#
#   It is a one-way stage (originals -> OneDrive), re-runnable, and never deletes
#   anything at the destination. Run it again after regenerating outputs and it
#   copies only what changed.
#
# PATHS - all three come from .Renviron, nothing is hard-coded:
#   DATA_PATH      destination. The OneDrive `Data` folder. Also what every
#                  pipeline script reads from once staging is done.
#   XBRL_DATA_PATH source for the raw SEC files (currently the D: drive).
#   DROPBOX_PATH   source for processed data and the licensed inputs.
#
# RESULTING LAYOUT
#   {DATA_PATH}
#       restatement-data-*.xlsx, adjustment-data-*.xlsx     licensed inputs
#       scf_*.parquet, audit_sample_*.csv                   pipeline outputs
#       scf_lines_parts/                                    extract checkpoint
#       comp_*.parquet, crsp_*.parquet                      for the eventual sample
#       XBRL/XBRL Extracted Data/                           the SEC parquet files
#
# WHAT IS DELIBERATELY NOT COPIED
#   - the 80 GB of `.tsv` originals in XBRL Extracted Data. The pipeline reads
#     only the parquet versions. Keep the .tsv on the external drive as the
#     archive; there is no reason to sync them to a shared folder.
#   - `*_tag.parquet` (2.4 GB). No live script reads it.
#   - the other folders under D:/XBRL (companyfacts.zip, the per-CIK outputs of
#     deleted scripts, the duckdb files). Nothing in the repo references them.
#
# USAGE
#   Run with DRY_RUN = TRUE first (the default) to see the plan and the byte
#   count, then set it to FALSE and re-run to copy. Expect roughly 11.7 GB.
# =============================================================================


# 1. Setup ---------------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(glue)
})

SRC_XBRL    <- Sys.getenv("XBRL_DATA_PATH")
SRC_DROPBOX <- Sys.getenv("DROPBOX_PATH")
DEST        <- Sys.getenv("DATA_PATH")

stopifnot(
  "XBRL_DATA_PATH is not set in .Renviron" = nzchar(SRC_XBRL),
  "DROPBOX_PATH is not set in .Renviron"   = nzchar(SRC_DROPBOX),
  "DATA_PATH is not set in .Renviron"      = nzchar(DEST))
for (p in c(SRC_XBRL, SRC_DROPBOX, DEST)) {
  if (!dir.exists(p)) stop("path does not exist: ", p)
}

# DATA_PATH carries a trailing slash by convention, so strip it before building
# child paths or file.path() yields a doubled separator.
DEST      <- sub("[/\\\\]+$", "", DEST)
DEST_XBRL <- file.path(DEST, "XBRL", "XBRL Extracted Data")


# 2. Configuration -------------------------------------------------------------

# TRUE prints the plan and stops. Set FALSE to actually copy.
DRY_RUN <- TRUE

# Which tiers to stage. Drop any you do not want.
#   xbrl       the SEC parquet files the extract reads               ~9.2 GB
#   required   licensed Audit Analytics inputs; NOT regenerable      ~9 MB
#   checkpoint the extract checkpoint; saves a ~33 min re-extract    ~451 MB
#   outputs    pipeline results and the hand-audit samples           ~494 MB
#   future     Compustat/CRSP. No current script reads these, but
#              they are needed for the eventual regression sample    ~1.5 GB
TIERS <- c("xbrl", "required", "checkpoint", "outputs", "future")

# A file already at the destination with the same size is treated as present.
# Size rather than mtime: cloud sync rewrites timestamps, so mtime produces
# spurious re-copies.
COMPARE_BY_SIZE_ONLY <- TRUE


# 3. Build the manifest --------------------------------------------------------
# One row per file: where it comes from, where it goes, which tier it belongs to.

manifest <- list()

add_files <- function(tier, src_dir, files, dest_dir) {
  if (!length(files)) return(invisible(NULL))
  manifest[[length(manifest) + 1]] <<- tibble(
    tier = tier,
    name = files,
    src  = file.path(src_dir, files),
    dest = file.path(dest_dir, files))
}

# --- the SEC parquet files the extract actually reads ---
if ("xbrl" %in% TIERS) {
  raw_files <- c(
    list.files(SRC_XBRL, pattern = "_num[.]parquet$"),
    list.files(SRC_XBRL, pattern = "_pre[.]parquet$"),
    list.files(SRC_XBRL, pattern = "_sub[.]parquet$"))
  # cal.parquet is the one orphan quarter of the per-filing CALCULATION linkbase
  # (adsh / ptag / ctag / negative). No script reads it yet, but it is the
  # authoritative fix for the nested-subtotal heuristic in 001a, so it travels.
  if (file.exists(file.path(SRC_XBRL, "cal.parquet"))) {
    raw_files <- c(raw_files, "cal.parquet")
  }
  add_files("xbrl", SRC_XBRL, raw_files, DEST_XBRL)
}

# --- licensed inputs: the only files here that cannot be regenerated ---
if ("required" %in% TIERS) {
  aa <- list.files(SRC_DROPBOX,
                   pattern = "^(restatement|adjustment)-data-.*[.]xlsx$")
  if (!length(aa)) {
    warning("No Audit Analytics exports found in DROPBOX_PATH. ",
            "002a cannot run without them.")
  }
  add_files("required", SRC_DROPBOX, aa, DEST)
}

# --- extract checkpoint: the parts directory plus its two bookkeeping files ---
if ("checkpoint" %in% TIERS) {
  parts <- list.files(file.path(SRC_DROPBOX, "scf_lines_parts"),
                      pattern = "[.]parquet$")
  add_files("checkpoint", file.path(SRC_DROPBOX, "scf_lines_parts"), parts,
            file.path(DEST, "scf_lines_parts"))
  add_files("checkpoint", SRC_DROPBOX,
            intersect(c("scf_lines_parts_done.csv", "scf_lines_parts_scope.rds"),
                      list.files(SRC_DROPBOX)), DEST)
}

# --- pipeline outputs and the hand-audit samples ---
if ("outputs" %in% TIERS) {
  outs <- c(list.files(SRC_DROPBOX, pattern = "^scf_.*_full[.]parquet$"),
            list.files(SRC_DROPBOX, pattern = "^aa_cleaned[.]parquet$"),
            list.files(SRC_DROPBOX, pattern = "^audit_sample_.*[.]csv$"))
  add_files("outputs", SRC_DROPBOX, outs, DEST)
}

# --- not yet used by any script, but needed for the regression sample ---
if ("future" %in% TIERS) {
  fut <- list.files(SRC_DROPBOX, pattern = "^(comp_|crsp_).*[.]parquet$")
  add_files("future", SRC_DROPBOX, fut, DEST)
}

plan <- bind_rows(manifest)
if (!nrow(plan)) stop("Manifest is empty - check TIERS and the source paths.")

plan <- plan |>
  mutate(
    src_size  = file.size(src),
    dest_size = file.size(dest),
    action = case_when(
      is.na(src_size)                                  ~ "MISSING at source",
      !is.na(dest_size) & dest_size == src_size        ~ "skip (already there)",
      !is.na(dest_size)                                ~ "re-copy (size differs)",
      TRUE                                             ~ "copy"))


# 4. Report the plan -----------------------------------------------------------
gb <- function(b) sprintf("%7.2f GB", sum(b, na.rm = TRUE) / 1024^3)

cat("SOURCES\n")
cat("  XBRL     ", SRC_XBRL, "\n")
cat("  Dropbox  ", SRC_DROPBOX, "\n")
cat("DESTINATION\n")
cat("  ", DEST, "\n")
cat("  ", DEST_XBRL, "\n\n")

cat("PLAN BY TIER\n")
print(plan |> group_by(tier) |>
  summarise(files = n(),
            size = gb(src_size),
            to_copy = sum(action %in% c("copy", "re-copy (size differs)")),
            already_there = sum(action == "skip (already there)"),
            missing = sum(action == "MISSING at source"),
            .groups = "drop") |> as.data.frame())

to_do <- plan |> filter(action %in% c("copy", "re-copy (size differs)"))
cat("\nTOTAL TO TRANSFER:", nrow(to_do), "files,", gb(to_do$src_size), "\n")

miss <- plan |> filter(action == "MISSING at source")
if (nrow(miss)) {
  cat("\nMISSING AT SOURCE (will be skipped):\n")
  print(as.data.frame(miss |> select(tier, name)))
}

if (DRY_RUN) {
  cat("\nDRY_RUN is TRUE - nothing copied.",
      "\nSet DRY_RUN <- FALSE and re-run to transfer.\n")
} else {

  # 5. Copy --------------------------------------------------------------------
  for (d in unique(dirname(to_do$dest))) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  cat("\nCopying", nrow(to_do), "files...\n")
  t0 <- Sys.time(); done_bytes <- 0; failed <- character(0)

  for (i in seq_len(nrow(to_do))) {
    r <- to_do[i, ]
    ok <- file.copy(r$src, r$dest, overwrite = TRUE, copy.date = TRUE)
    if (!ok) failed <- c(failed, r$name)
    done_bytes <- done_bytes + r$src_size
    # progress every 25 files, and after anything over 200 MB
    if (i %% 25 == 0 || r$src_size > 200e6 || i == nrow(to_do)) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      cat(sprintf("  %4d/%d  %6.2f GB  %5.1f min  (%s)\n",
                  i, nrow(to_do), done_bytes/1024^3, el, r$name))
    }
  }

  # 6. Verify ------------------------------------------------------------------
  cat("\nVERIFYING\n")
  check <- plan |>
    filter(action != "MISSING at source") |>
    mutate(now = file.size(dest),
           ok = !is.na(now) & now == src_size)
  print(check |> group_by(tier) |>
    summarise(files = n(), verified = sum(ok), mismatched = sum(!ok),
              .groups = "drop") |> as.data.frame())

  if (length(failed)) {
    cat("\nFAILED TO COPY (", length(failed), "):\n", sep = "")
    print(head(failed, 20))
  }
  bad <- check |> filter(!ok)
  if (nrow(bad)) {
    cat("\nSIZE MISMATCH AFTER COPY (", nrow(bad), "):\n", sep = "")
    print(as.data.frame(head(bad |> select(tier, name, src_size, now), 20)))
  } else {
    cat("\nAll staged files verified by size.\n")
  }

  cat("\nNEXT: point .Renviron's XBRL reads at the OneDrive copy, or leave\n",
      "XBRL_DATA_PATH on D: for speed - the pipeline scripts read\n",
      "{DATA_PATH}XBRL/XBRL Extracted Data/ once updated.\n")
}


# 7. Informational: what is at the destination that this script did not stage ---
# Never deleted, only reported, so stale files can be pruned by hand.
if (!DRY_RUN) {
  staged <- basename(plan$dest[dirname(plan$dest) == normalizePath(DEST, "/", FALSE)])
  present <- setdiff(list.files(DEST), c(staged, "XBRL", "scf_lines_parts"))
  if (length(present)) {
    cat("\nAt the destination but not staged by this script (left untouched):\n")
    print(head(present, 30))
  }
}
