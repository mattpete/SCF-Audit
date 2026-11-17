## ============================================================================
## 011e  EXPORT CHUNK TEXT                 step 011 (locate), substep e
## Pulls the chosen block text off WRDS. This is the handoff to step 012.
## In : cache/chunks8.rds (011d)
## Out: extract/chunks.jsonl   <- what the model reads
##      cache/chunktext.rds, cache/chunk_unresolved.rds
## ============================================================================
## Pull the actual text of every chosen chunk off WRDS Cloud and write it to a
## JSONL file that the Python extraction runner reads. This is the handoff
## between the R/WRDS half of the project and the API half.
##
## Input :  cache/chunks8.rds      (ALL candidates per filing x statement, ranked)
## Output:  extract/chunks.jsonl   (one JSON object per chunk, with the text)
##          cache/chunktext.rds    (the same, as an R object)
##          cache/chunk_unresolved.rds  (anything still not a statement)
##
## Run this BEFORE 012a-extract-statements.py. It needs no API key.
## Roughly 1,500 chunks, ~18 MB of base64 over SSH, a few minutes.
##
## WHY THIS VALIDATES AND RETRIES. Every 10-K and 10-Q contains a cash flow
## statement, so a chunk that holds no statement is a bug, not a miss rate.
## Measured on all 498 picked cash flow chunks: 27 held no statement (a table of
## contents, MD&A prose, or the MD&A "Cash Flows Data" summary table), and for 24
## of those 27 a CORRECT block was sitting in the candidate list, out-ranked.
## Reranking alone recovered only 19 of 27, so the fix is not a better sort -
## it is to check the text we actually got and walk down the ranked candidates
## until the block really is a statement. Ranking (011d) just makes rank 1 right
## more often, which keeps the number of round trips down.
##
## Transport note: chunk text contains newlines, so it cannot be sent back
## line-delimited. The remote side base64-encodes each chunk to one line.
## Keep every remote pipeline on a SINGLE line - a backslash line-continuation
## inside this single-quoted string gets mangled on its way through SSH and
## silently returns zero rows.

suppressMessages({library(DBI); library(RPostgres); library(keyring)
                  library(ssh); library(jsonlite)})

SP  <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
OUT <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/extract/")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
stopifnot(nzchar(Sys.getenv("SEC_DATA_PATH")))

## How deep to walk the candidate list. 8 left 15 slots stranded at the ceiling
## while they still had candidates below; the unresolved slots carry a median of
## 4 candidates and a p90 of 12, so 12 reaches almost all of them. Cost is only
## the retries, which run on a handful of slots.
MAX_RANK <- 12

C <- readRDS(paste0(SP, "chunks8.rds"))
if (!"rank" %in% names(C))
  stop("chunks8.rds has no `rank` column - re-run 011d-build-rank-candidates.R")
C$key <- paste(C$wrdsfname, C$kind)
cat("candidate blocks:", nrow(C), " filing x statement slots:",
    length(unique(C$key)), "\n")

## ---- the filing's own period end date, needed later to assign columns to years
wrds <- dbConnect(Postgres(), host = 'wrds-pgdata.wharton.upenn.edu', port = 9737,
                  user = key_get("wrds-user"), password = key_get("wrds-password"),
                  sslmode = 'require', dbname = 'wrds')
fi <- dbGetQuery(wrds, sprintf(
  "SELECT wrdsfname, cik, coname, form, fdate, rdate FROM wrdssec.wrds_forms
   WHERE wrdsfname IN (%s)",
  paste0("'", unique(C$wrdsfname), "'", collapse = ",")))
dbDisconnect(wrds)
cat("metadata rows:", nrow(fi), "\n")

## ---- pull the text, base64 one line per chunk
remote <- '
R=/wrds/sec/wrds_clean_filings
while IFS="~" read FF KIND A B; do
  p="$R/$FF"
  [ -f "$p" ] || continue
  b=$(sed -n "${A},${B}p" "$p" | base64 -w0)
  printf "%s~%s~%s\n" "$FF" "$KIND" "$b"
done < /tmp/ex_$$.txt
rm -f /tmp/ex_$$.txt'

decode <- function(x) {
  if (!nzchar(x)) return("")
  t <- rawToChar(jsonlite::base64_dec(x))
  if (!validUTF8(t)) t <- iconv(t, "latin1", "UTF-8", sub = " ")
  t
}

## Pull a set of candidate rows and return wrdsfname / kind / text.
pull_chunks <- function(z, label = "") {
  BS  <- 100
  idx <- split(seq_len(nrow(z)), ceiling(seq_len(nrow(z)) / BS))
  acc <- character(0)
  for (b in seq_along(idx)) {
    q <- z[idx[[b]], ]
    r <- try({
      s <- ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
                       passwd = key_get("wrds-password"))
      cmd <- paste0("cat > /tmp/ex_$$.txt <<'XEOF'\n",
                    paste(paste(q$wrdsfname, q$kind, q$start, q$end, sep = "~"),
                          collapse = "\n"),
                    "\nXEOF\n", remote)
      o <- rawToChar(ssh_exec_internal(s, cmd, error = FALSE)$stdout)
      ssh_disconnect(s); o
    }, silent = TRUE)
    if (inherits(r, "try-error")) { cat("  batch", b, "FAILED - rerun\n"); next }
    acc <- c(acc, r)
    cat(sprintf("  %sbatch %d/%d ok\n", label, b, length(idx))); flush.console()
  }
  ln <- unlist(strsplit(paste(acc, collapse = "\n"), "\n", fixed = TRUE))
  ln <- ln[grepl("~", ln, fixed = TRUE)]
  ff <- sub("~.*$", "", ln)
  kk <- sub("~.*$", "", sub("^[^~]*~", "", ln))  ## two subs, no backreference (CLAUDE.md heredoc trap)
  bb <- sub("^[^~]*~[^~]*~", "", ln)
  d  <- data.frame(wrdsfname = ff, kind = kk,
                   text = vapply(bb, decode, character(1), USE.NAMES = FALSE),
                   stringsAsFactors = FALSE)
  d[nzchar(d$wrdsfname) & nzchar(d$text), ]
}

## ---- is there anything here worth sending to the model? -------------------
## HISTORY, because three versions of this were tried and all three were wrong,
## and the fourth answer is to stop trying:
##
##   v1 wanted a label and a number on the SAME line. ~40% of filings put each
##      number on its own line, so Meta Financial's complete statement scored 3.
##   v2 wanted 6 word-only lines instead. Johnson & Johnson puts labels and
##      numbers on the same line, so it had almost none.
##   v3 rejected junk instead of demanding proof - a contents page, MD&A prose,
##      or the MD&A "Cash Flows Data" table - but every one of those tests
##      needed "and almost no figures" to avoid rejecting real statements, and
##      the threshold (nnum < 40) let long junk through.
##
## No regex separates a printed statement from a page that merely looks like one;
## reading the text is what settles it, and the model reads the text. So this is
## now only a cheap "is it worth an API call" gate, and correctness lives in the
## model's own `found` / `statement` fields plus the footing check in 012b.
is_statement <- function(tx, kind) {
  v  <- strsplit(tx, "\n", fixed = TRUE)[[1]]
  v  <- v[nzchar(trimws(v))]
  ## figures ANYWHERE in the block, so the count is layout-blind
  nnum <- length(unlist(regmatches(
    v, gregexpr("\\(?\\$?[0-9][0-9,]*(\\.[0-9]+)?\\)?", v))))

  ## DECISION (Matt, Nov 2026): do NOT reject on a line/figure count. The old
  ## gates were `toc` (3+ statement titles AND nnum < 40) and `prose`
  ## (mean words > 9 AND nnum < 40). Both hinged on that arbitrary 40, and it
  ## let long junk straight through: of ten picked cash flow chunks read by
  ## hand, three were an exhibit index, a contents page and a footnote, each
  ## carrying well over 40 numbers. Tuning the number trades one error for the
  ## other. The model judges instead - it returned found=false on every junk
  ## block tested - so the only gate kept here is "has some numbers at all",
  ## which costs a wasted API call rather than a wrong row.
  ##
  ## Revisit only if the wasted calls turn out to cost real money. They are
  ## ~2-3k tokens each, so a 10% junk rate on the full population is a few
  ## tens of dollars.
  nnum >= 8
}

## ---- walk the ranked candidates until every slot holds a statement --------
want <- unique(C$key)
got  <- list()
for (rk in seq_len(MAX_RANK)) {
  z <- C[C$rank == rk & C$key %in% want, ]
  if (!nrow(z)) break
  cat(sprintf("\nrank %d: pulling %d candidate(s)\n", rk, nrow(z)))
  d <- pull_chunks(z, label = sprintf("r%d ", rk))
  if (!nrow(d)) next
  d$key <- paste(d$wrdsfname, d$kind)
  d <- d[!duplicated(d$key), ]
  d$ok <- mapply(is_statement, d$text, d$kind)
  d$rank_used <- rk
  keep <- d[d$ok, ]
  if (nrow(keep)) got[[length(got) + 1]] <- keep
  want <- setdiff(want, keep$key)
  cat(sprintf("  validated %d, still unresolved %d\n", nrow(keep), length(want)))
  if (!length(want)) break
}

D <- do.call(rbind, got)
cat("\n==================== resolution ====================\n")
cat("slots resolved to a real statement:", nrow(D), "of",
    length(unique(C$key)), "\n")
cat("rank actually used:\n"); print(table(D$rank_used))
if (length(want)) {
  cat("\nSTILL UNRESOLVED:", length(want), "\n")
  u <- C[C$key %in% want & C$rank == 1, ]
  print(u[, c("wrdsfname", "kind", "form", "start", "end", "len", "how")],
        row.names = FALSE)
  saveRDS(u, paste0(SP, "chunk_unresolved.rds"))
  cat("saved cache/chunk_unresolved.rds - these need a look by hand\n")
} else {
  cat("\nevery slot resolved.\n")
  unlink(paste0(SP, "chunk_unresolved.rds"))
}

## ---- attach the chunk bounds and the filing metadata
D$key <- NULL; D$ok <- NULL
D <- merge(D, C[, c("wrdsfname", "kind", "rank", "start", "end", "len",
                    "how", "titled", "complete", "nnum", "dens")],
           by.x = c("wrdsfname", "kind", "rank_used"),
           by.y = c("wrdsfname", "kind", "rank"), all.x = TRUE)
D <- merge(D, fi, by = "wrdsfname", all.x = TRUE)

## ---- drop PAGE FURNITURE from the block text -----------------------------
## HTML filings put a "Table of Contents" back-link at the top of every page,
## so it lands INSIDE a statement wherever the statement crosses a page
## boundary, not only between statements (Matt, 2026-11-17). The same is true
## of the notes reference, the page number, and the "(continued)" marker, and
## XBRL-rendered filings carry the viewer's JavaScript at the top of the block.
##
## None of it is statement content, and the string cannot be used as a
## boundary signal precisely because it appears mid-statement - so it is
## removed from the text rather than reasoned about.
##
## Measured on 1,412 blocks before this was added: 2,797 furniture lines, in
## 1,084 blocks (77%). The contents back-link alone appeared in 560 blocks and
## 16% of its occurrences were in the middle of the block, confirming the
## mid-statement case. Safety check on the same data: ZERO furniture lines
## carried a figure with a comma or a dollar sign, so nothing with a value in
## it is at risk. The patterns are anchored to whole lines for that reason -
## in particular there is deliberately no bare-number rule, because a lone
## "3" is as likely to be an amount as a page number.
FURNITURE <- paste0(
  "^(back to |return to )?table of contents$|^index$",
  "|^see (the )?(accompanying|notes)|^the accompanying notes",
  "|^see notes to|^accompanying notes to",
  "|^[-(]?continued[-)]?$",
  "|^<page>|^page [0-9]+$|^f-[0-9]+$",
  "|^function |^if \\(e\\.|^\\} else \\{|do not remove this comment",
  "|^</?(type|sequence|filename|description|text)>")

strip_furniture <- function(txt) {
  v <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  k <- !grepl(FURNITURE, gsub("[[:space:]]+", " ", tolower(trimws(v))), perl = TRUE)
  paste(v[k], collapse = "\n")
}
n_before <- sum(lengths(strsplit(D$text, "\n", fixed = TRUE)))
D$text   <- vapply(D$text, strip_furniture, character(1), USE.NAMES = FALSE)
n_after  <- sum(lengths(strsplit(D$text, "\n", fixed = TRUE)))
cat(sprintf("\npage furniture removed: %d of %d lines (%.1f%%)\n",
            n_before - n_after, n_before, 100 * (n_before - n_after) / n_before))

## NOTE: `len` and `nnum` describe the ORIGINAL line range in the filing and
## are deliberately left alone, so a block can still be found again on WRDS by
## its line numbers. Only `chars` is recomputed, below, from the trimmed text.
D$chars <- nchar(D$text)

cat("\nchars  median", median(D$chars), " mean", round(mean(D$chars)),
    " p99", quantile(D$chars, .99), " max", max(D$chars), "\n")
print(table(D$kind, D$form))

## ---- write JSONL, one object per line
f <- file(paste0(OUT, "chunks.jsonl"), "wb")
for (i in seq_len(nrow(D))) {
  writeLines(jsonlite::toJSON(as.list(D[i, ]), auto_unbox = TRUE, null = "null"),
             f, useBytes = TRUE)
}
close(f)
saveRDS(D, paste0(SP, "chunktext.rds"))
cat("\nwrote", paste0(OUT, "chunks.jsonl"), "-",
    round(sum(D$chars) / 1e6, 1), "MB of statement text\n")
