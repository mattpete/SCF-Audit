## ============================================================================
## 000-grid-helpers.R   RUN THE REMOTE SCANS ON THE WRDS GRID
## Shared machinery for 001b-grid / 001c-grid / 001e-grid. Sourced, never run.
## ============================================================================
## WHY THIS EXISTS. The SSH scans (001b/001c/001e) hold one connection open and
## stream grep output back down it, one batch of ~100 filings at a time. That
## is one CPU and a laptop that must stay awake: measured 0.078 (001b), 0.018
## (001c) and 0.094 (001e) s/filing, which is ~2.5 days for the 780,364-filing
## inventory and ~2.1 days for the 660,715 that survive a Compustat screen.
##
## The grid runs the same greps on up to 10 machines at once, detached. Nothing
## about the patterns or the output format changes - see "NO SECOND COPY OF THE
## PATTERNS" below, which is the whole reason this file is shaped as it is.
##
## MEASURED WRDS FACTS (checked 2026-11-20, not assumed - re-check if a run
## behaves unexpectedly, because these are policy and can change):
##
##   all.q                892 slots, 801 free at the time of checking
##   per-user cap         10 slots on all.q  ("limit users {*} queues {all.q}
##                        to slots=10" in qconf -srqs). This is a HARD cap, not
##                        a courtesy limit - it is why GRID_SLOTS is 10 and why
##                        raising it does nothing except queue tasks.
##   runtime              s_rt 168:00:00 (7 days), h_rt INFINITY. A long task
##                        will not be killed.
##   array tasks          max_aj_tasks 75000, so task count is not a constraint.
##   home quota           10 GB, 3.85 used -> job output must NOT go to ~
##   scratch quota        500 GB, 0 used, SHARED with all of UA
##   grid node access     VERIFIED: a grid node reads
##                        /wrds/sec/wrds_clean_filings directly and runs our
##                        grep. This was the real unknown (grid nodes are
##                        separate hosts from the login node) and it works -
##                        tested on three real filings, correct line counts and
##                        hit counts returned.
##
## ----------------------------------------------------------------------------
## THE SPEEDUP IS ~3x, NOT 10x. MEASURED 2026-11-20 - READ THIS BEFORE
## PROMISING ANYONE A NUMBER.
##
## The prediction was 10x, on the reasoning that reading is ~5% of the work and
## grepping ~95% (0.004 s/filing to read against 0.08 to scan), so ten tasks
## should scale almost linearly. That reasoning was WRONG. Benchmarked on
## 001e, all on the same filings, markers byte-identical every time:
##
##   path                       filings  tasks   wall   s/filing   vs SSH
##   SSH, login node                500      -     38s     0.0757    1.0x
##   grid, ONE task                 500      1     82s     0.1644    0.46x
##   grid, 10 tasks               5,000     10    143s     0.0287    2.6x
##   grid, 100 tasks              5,000    100    164s     0.0328    2.3x
##
## Two findings, and both are about the filesystem rather than the CPU:
##
## 1. ONE GRID TASK IS SLOWER THAN THE LOGIN NODE - 0.164 against 0.0757
##    s/filing, and ~0.124 once the ~20s queue start is taken off. So a grid
##    node reads /wrds/sec more slowly than the login node does, and every task
##    starts at a 1.6x disadvantage.
##
## 2. TEN TASKS AT ONCE HALVE EACH OTHER'S THROUGHPUT. 500 filings take 62s of
##    work in one task alone and ~123s per task when ten run together. The
##    archive is shared NFS and ten readers contend on it.
##
## Net: 10 slots x (1/1.6 node penalty) x (1/2 contention) = ~3x, which is what
## the end-to-end numbers show. MORE PARALLELISM DOES NOT HELP - 100 tasks came
## out slower than 10, and the 10-slot cap is therefore NOT the binding
## constraint. The shared filesystem is.
##
## What this means for a full pass over 660,715 filings (Compustat-screened):
##   001b  ~14 h SSH  ->  ~5 h grid
##   001c  ~3 h  SSH  ->  ~1 h grid
##   001e  ~17 h SSH  ->  ~6 h grid
## So roughly 34 hours becomes roughly 12, detached, rather than 2.5 days tied
## to a laptop. Worth having, but it is 3x and not the order of magnitude the
## design was pitched on.
##
## NOT TRIED, AND DELIBERATELY NOT TRIED: ssdwork.q has 300 free slots and is
## absent from the `users {*}` 10-slot rule, so it would probably permit more
## concurrency. Using another queue to get around a published per-user cap is
## exactly the kind of thing that gets an account noticed, and the bottleneck
## is the shared archive anyway, so more slots would not buy much. Ask WRDS
## before going near it.
##
## ----------------------------------------------------------------------------
## NO SECOND COPY OF THE PATTERNS.
##
## The patterns in 001b and 001e are the single most expensive thing in this
## folder - every one of their branches is there because a named filing broke
## the simpler version, and both carry a rule whose character cap IS its
## semantics. Copying them into a grid script would create two copies that
## drift silently, and a drifted pattern does not error: it quietly returns a
## different marker set, and the damage surfaces three scripts later.
##
## So `grid_patterns()` READS THEM OUT OF THE ORIGINAL SCRIPT. It parses the
## file and evaluates only the assignments that build the patterns, in file
## order, in a bare environment. The original scripts are untouched and remain
## authoritative; a change to a pattern there is picked up here on the next run
## with no edit to this file.
## ============================================================================
suppressMessages({library(ssh); library(keyring)})

GRID_SLOTS <- as.integer(Sys.getenv("GRID_SLOTS", "10"))   # concurrent tasks (cap: 10)
GRID_TASKS <- as.integer(Sys.getenv("GRID_TASKS", "100"))  # slices to cut the list into
GRID_ROOT  <- Sys.getenv("GRID_ROOT", "/scratch/ua/mattpeterson-scf")

## MORE TASKS THAN SLOTS, ON PURPOSE. 100 slices running 10 at a time beats 10
## slices of 10x the size: a slow slice cannot hold up the whole run, a failure
## costs 1% of the work instead of 10%, and progress is visible as tasks finish.
## -tc holds concurrency at GRID_SLOTS so we never have more than our share
## pending in the queue.

## WRITE UNIX LINE ENDINGS, ALWAYS. writeLines() on Windows opens a text-mode
## connection and translates "\n" to CRLF, and SGE refuses a job script with
## CRLF outright ("Unable to run job: Windows-style CRLF newlines"). The list
## files would be worse than that - they would not error, they would silently
## append "\r" to every filing path and every single file would be "not found",
## giving an empty scan with no failure anywhere. So every file staged to WRDS
## goes through here (measured: this is exactly how the first smoke test failed).
write_unix <- function(x, path) {
  con <- file(path, "wb"); on.exit(close(con))
  writeLines(x, con, sep = "\n", useBytes = TRUE)
}

wrds_open <- function() {
  ssh_connect(paste0(key_get("wrds-user"), "@wrds-cloud.wharton.upenn.edu"),
              passwd = key_get("wrds-password"))
}

sh <- function(s, cmd) {
  o <- ssh_exec_internal(s, cmd, error = FALSE)
  list(out = rawToChar(o$stdout), err = rawToChar(o$stderr), status = o$status)
}

## ---- read the patterns out of the original script --------------------------
## Evaluates ONLY assignments whose target is in `want`, in file order, so
## PAT is defined before the `remote` string that shQuote()s it. Everything
## else in the script - library(), readRDS(), ssh_connect(), the batch loop -
## is never evaluated, so sourcing has no side effects and needs no data.
grid_patterns <- function(path, want = c("S", "S0", "W", "PAT", "NAME_PAT", "remote")) {
  stopifnot(file.exists(path))
  e <- new.env(parent = globalenv())
  for (x in as.list(parse(path))) {
    if (!is.call(x) || !identical(as.character(x[[1]])[1], "<-")) next
    nm <- as.character(x[[2]])
    if (length(nm) != 1L || !nm %in% want) next
    eval(x, envir = e)
  }
  got <- intersect(want, ls(e))
  miss <- setdiff(c("remote"), got)
  if (length(miss))
    stop("could not read ", paste(miss, collapse = ", "), " out of ", basename(path),
         " - has the script been restructured? (grid_patterns reads assignments ",
         "by name, so renaming `remote` breaks it)", call. = FALSE)
  mget(got, envir = e)
}

## ---- turn an SSH remote body into a grid task body -------------------------
## The SSH bodies read their filing list from a temp file the login shell
## wrote, e.g. `done < /tmp/mk_$$.txt`, and delete it afterwards. On the grid
## the list is staged per task, so those two references are retargeted at
## $LIST and the delete is dropped.
##
## The pattern below matches only the LIST temp files (they end .txt) and
## deliberately NOT 001e's `/tmp/j_$$`, which is the awk line-pair join's own
## scratch file and must stay a real temp file. On a grid node that write goes
## to the node's local disk rather than the shared login node, which is where
## it always belonged.
grid_body <- function(remote) {
  b <- gsub("/tmp/[A-Za-z]+_\\$\\$\\.txt", "\"$LIST\"", remote)
  b <- gsub('(?m)^[ \\t]*rm -f "\\$LIST"[ \\t]*$', "", b, perl = TRUE)
  if (grepl("/tmp/[A-Za-z]+_\\$\\$\\.txt", b))
    stop("a list temp file survived rewriting - check grid_body()", call. = FALSE)
  b
}

## ---- stage, submit, wait, fetch --------------------------------------------
## One call does the lot. Returns the scan output as a character vector of
## lines, in the SAME order and format the SSH version produces, so each
## grid script can hand it to the original's parser unchanged.
grid_run <- function(files, remote, jobname,
                     slots = GRID_SLOTS, tasks = GRID_TASKS,
                     keep = FALSE, poll = 20) {
  stopifnot(length(files) > 0, nzchar(jobname))
  tasks <- max(1L, min(as.integer(tasks), length(files)))
  jdir  <- paste0(GRID_ROOT, "/", jobname)
  body  <- grid_body(remote)

  s <- wrds_open(); on.exit(try(ssh_disconnect(s), silent = TRUE), add = TRUE)

  ## a stale job directory would let an old task's output be collected as this
  ## run's, which is the worst possible silent failure, so it is cleared first
  sh(s, paste0("rm -rf ", shQuote(jdir), " && mkdir -p ", shQuote(jdir)))

  ## --- slices. Round-robin, not contiguous blocks: filing size varies by two
  ## orders of magnitude and is correlated with position in a CIK-sorted list,
  ## so contiguous blocks give wildly uneven tasks and the run waits on one.
  grp <- (seq_along(files) - 1L) %% tasks + 1L
  tmp <- tempfile("scfgrid"); dir.create(tmp)
  for (i in seq_len(tasks)) {
    f <- files[grp == i]
    write_unix(f, file.path(tmp, sprintf("list_%03d.txt", i)))
  }
  cat(sprintf("staging %d filings in %d slices -> %s\n", length(files), tasks, jdir))
  for (i in seq_len(tasks))
    scp_upload(s, file.path(tmp, sprintf("list_%03d.txt", i)), to = jdir, verbose = FALSE)

  ## --- the task script. The scan body goes inside a function so its whole
  ## output can be piped through gzip in one go; a done-marker is written only
  ## on success, which is what makes a partial run resumable and is also the
  ## only honest signal that a task finished (SGE's exit status does not
  ## distinguish "grep found nothing" from "the node died").
  job <- c(
    "#!/bin/bash",
    "#$ -S /bin/bash",
    paste0("JDIR=", shQuote(jdir)),
    'T=$(printf %03d ${SGE_TASK_ID:-1})',
    'LIST=$JDIR/list_$T.txt',
    'OUT=$JDIR/out_$T.txt.gz',
    '[ -f "$LIST" ] || { echo "no list for task $T" >&2; exit 1; }',
    "scan() {", body, "}",
    'scan | gzip -c > "$OUT" || exit 1',
    'echo "$(hostname) $(date +%s)" > "$JDIR/done_$T"')
  jf <- file.path(tmp, "task.sh")
  write_unix(job, jf)
  scp_upload(s, jf, to = jdir, verbose = FALSE)

  ## --- submit. -tc holds us at `slots` concurrent tasks whatever the queue
  ## would allow, so the 10-slot cap is respected by us rather than enforced
  ## against us. Output/error files are per task and go to the job dir, not ~.
  q <- sh(s, paste0(
    "cd ", shQuote(jdir), " && chmod +x task.sh && ",
    "qsub -q all.q -cwd -b n -S /bin/bash -N ", shQuote(jobname),
    " -t 1-", tasks, " -tc ", slots,
    " -o ", shQuote(jdir), " -e ", shQuote(jdir), " task.sh 2>&1"))
  cat(q$out)
  jid <- sub(".*job-array ([0-9]+).*", "\\1", gsub("\n", " ", q$out))
  if (!grepl("^[0-9]+$", jid))
    stop("qsub did not return a job id:\n", q$out, q$err, call. = FALSE)

  ## --- wait. Poll the done-markers rather than qstat: a task that dies
  ## leaves the queue but leaves no marker, which is exactly what we need to
  ## know, and counting files costs nothing.
  t0 <- Sys.time(); last <- -1L
  repeat {
    r <- sh(s, paste0("ls ", shQuote(jdir), "/done_* 2>/dev/null | wc -l; ",
                      "qstat -t 2>/dev/null | grep -c ", shQuote(jobname)))
    v <- suppressWarnings(as.integer(strsplit(trimws(r$out), "\\s+")[[1]]))
    ndone <- if (length(v) >= 1 && !is.na(v[1])) v[1] else 0L
    nq    <- if (length(v) >= 2 && !is.na(v[2])) v[2] else 0L
    if (ndone != last) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      cat(sprintf("  %3d/%d tasks done, %d still in queue  [%.0fs]\n",
                  ndone, tasks, nq, el)); flush.console()
      last <- ndone
    }
    if (ndone >= tasks) break
    if (nq == 0L && ndone < tasks) {
      cat(sprintf("\n!! queue is empty but only %d of %d tasks finished\n", ndone, tasks))
      break
    }
    Sys.sleep(poll)
  }
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  ## --- which slices are missing. Named, so a resubmit can target them.
  dn <- sh(s, paste0("ls ", shQuote(jdir), " 2>/dev/null | grep '^done_' | sed 's/done_//'"))
  have <- sort(as.integer(strsplit(trimws(dn$out), "\\s+")[[1]]))
  have <- have[!is.na(have)]
  gone <- setdiff(seq_len(tasks), have)
  if (length(gone)) {
    cat("\nFAILED TASKS:", paste(gone, collapse = ", "), "\n")
    cat("their stderr:\n")
    for (i in head(gone, 3)) {
      r <- sh(s, paste0("cat ", shQuote(jdir), "/", jobname, ".e*.", i, " 2>/dev/null | head -5"))
      cat("  task", i, ":", substr(r$out, 1, 400), "\n")
    }
  }

  ## --- fetch. Download the gzipped slices and decompress locally; the
  ## markers have to reach R to be parsed either way, and compressed they are
  ## roughly a sixth of the size on the wire.
  dl <- file.path(tmp, "dl"); dir.create(dl)
  acc <- character(0); nb <- 0
  for (i in have) {
    rf <- sprintf("%s/out_%03d.txt.gz", jdir, i)
    lf <- file.path(dl, sprintf("out_%03d.txt.gz", i))
    ok <- try(scp_download(s, rf, to = dl, verbose = FALSE), silent = TRUE)
    if (inherits(ok, "try-error") || !file.exists(lf)) {
      cat("  could not fetch slice", i, "\n"); next }
    nb <- nb + file.size(lf)
    acc <- c(acc, readLines(gzfile(lf), warn = FALSE))
  }

  cat(sprintf("\n%d tasks, %d/%d slices collected, %.1f MB compressed, %.0fs wall\n",
              tasks, length(have), tasks, nb / 1e6, el))
  cat(sprintf("rate: %.4f s/filing  (SSH version: 001b 0.078, 001c 0.018, 001e 0.094)\n",
              el / max(1, length(files))))

  if (!keep) sh(s, paste0("rm -rf ", shQuote(jdir)))
  else cat("job dir kept at", jdir, "\n")
  unlink(tmp, recursive = TRUE)

  attr(acc, "tasks_missing") <- gone
  attr(acc, "elapsed") <- el
  acc
}

## ---- the marker parser -----------------------------------------------------
## MIRRORS the identical loops at the foot of 001b and 001e. This one could not
## be read out of the originals the way the patterns are - it is a for-loop, not
## an assignment - so it IS a second copy, and the guard against drift is
## 001k-verify-grid-matches-ssh.R, which compares the two paths' output row for
## row. Run it after changing either side.
##
## Format produced by the scan body, per filing:
##   #F|<wrdsfname>|<total lines>     one per filing
##   M|<line no>|<text>               one per marker hit
## VECTORISED AND CHUNKED, because the obvious transcription of the originals'
## loop does not survive a real wave. Two separate failures, both measured on
## the first 25,000-filing run:
##
## 1. `paste(acc, collapse = "\n")` DIED: "result would exceed 2^31-1 bytes".
##    25,000 filings yield ~27M marker lines, ~2.5 GB of text, and R cannot
##    hold one string that long. The paste was pointless anyway - the SSH
##    scripts need it because each element of `acc` is a whole batch's stdout,
##    but here the lines arrive already split by readLines(), so pasting them
##    together and splitting them again was a round trip that only served to
##    materialise the thing that broke. strsplit() is element-wise, so both
##    shapes are handled without ever concatenating.
##
## 2. The per-line for-loop would not have finished either: it calls strsplit()
##    once per line and allocates five full-length vectors, on 27M lines.
##
## So the filing each marker belongs to is resolved with a cumsum over the
## "#F|" header rows - a running count of headers seen is exactly an index into
## the header list - and the fields come off with vectorised sub(). Chunking
## caps peak memory; `carry` passes the filing active at a chunk boundary into
## the next chunk, which is the one thing the chunking could otherwise break.
##
## Verified against the SSH output by 001k after this rewrite.
grid_parse_markers <- function(acc, chunk = 2e6) {
  ln <- if (any(grepl("\n", acc, fixed = TRUE)))
          unlist(strsplit(acc, "\n", fixed = TRUE)) else acc
  rm(acc)
  n <- length(ln)
  if (n == 0) return(data.frame(wrdsfname = character(0), nlines = integer(0),
                                line = integer(0), text = character(0),
                                stringsAsFactors = FALSE))
  out <- list(); k <- 0L
  carry_f <- NA_character_; carry_n <- NA_integer_
  for (lo in seq(1L, n, by = chunk)) {
    hi <- min(lo + chunk - 1L, n); k <- k + 1L
    x <- ln[lo:hi]
    isF <- startsWith(x, "#F|"); isM <- startsWith(x, "M|")
    ## "#F|<wrdsfname>|<nlines>" - the path never contains a pipe, and the
        ## marker text cannot either because the remote body runs tr -d "|"
    fh <- x[isF]
    fname <- sub("[|][0-9]+$", "", substring(fh, 4L))
    fn    <- suppressWarnings(as.integer(sub("^.*[|]", "", fh)))
    ## 0 = "before the first header in this chunk", i.e. the carried filing
    grp <- cumsum(isF)
    xm  <- x[isM]
    lno <- suppressWarnings(as.integer(sub("[|].*$", "", substring(xm, 3L))))
    out[[k]] <- data.frame(
      wrdsfname = c(carry_f, fname)[grp[isM] + 1L],
      nlines    = c(carry_n, fn)[grp[isM] + 1L],
      line      = lno,
      text      = gsub("[[:space:]]+", " ", tolower(sub("^M[|][0-9]+[|]", "", xm))),
      stringsAsFactors = FALSE)
    if (length(fh)) { carry_f <- fname[length(fname)]; carry_n <- fn[length(fn)] }
    rm(x, isF, isM, fh, fname, fn, grp, xm, lno)
  }
  rm(ln)
  M <- if (length(out) == 1L) out[[1]] else do.call(rbind, out)
  M <- M[!is.na(M$line) & !is.na(M$wrdsfname) & nzchar(M$wrdsfname), ]
  ## dedupe, because the company-name rule runs as a second grep and a line
  ## satisfying both patterns is emitted twice (see the note in 001b/001e)
  M[!duplicated(M[c("wrdsfname", "line", "text")]), ]
}

## ---- refuse to save a collapsed result -------------------------------------
## The same guard the SSH scripts carry, and for the same reason: one bad
## character in a pattern used to turn into a destroyed cache, because the
## remote grep matched nothing, an empty file replaced the good one, and the
## failure surfaced two scripts later as "0 rows" with nothing to trace.
grid_save_markers <- function(M, out, n_asked) {
  got <- length(unique(M$wrdsfname))
  cat("\nrows:", nrow(M), " filings:", got, "of", n_asked, "\n")
  if (nrow(M) == 0 || got < n_asked / 2)
    stop(sprintf(paste0("markers collapsed: %d rows for %d of %d filings. ",
                        "NOT saving - %s is unchanged.\n",
                        "  Check the pattern reached the grid node intact, and ",
                        "that no task failed (grid_run() names failed slices)."),
                 nrow(M), got, n_asked, out), call. = FALSE)
  saveRDS(M, out)
  cat("saved", out, "\n")
  invisible(M)
}

## ---- housekeeping ----------------------------------------------------------
grid_status <- function() {
  s <- wrds_open(); on.exit(try(ssh_disconnect(s), silent = TRUE))
  r <- sh(s, paste0(
    "echo '--- my jobs ---'; qstat 2>&1 | head -20; ",
    "echo '--- my job dirs ---'; du -sh ", shQuote(GRID_ROOT), "/* 2>/dev/null; ",
    "echo '--- quota ---'; quota 2>&1 | head -8"))
  cat(r$out)
  invisible(NULL)
}

grid_clean <- function(confirm = FALSE) {
  if (!confirm) { cat("grid_clean(confirm = TRUE) removes", GRID_ROOT, "\n"); return(invisible()) }
  s <- wrds_open(); on.exit(try(ssh_disconnect(s), silent = TRUE))
  cat(sh(s, paste0("rm -rf ", shQuote(GRID_ROOT), " && echo removed ", GRID_ROOT))$out)
}
