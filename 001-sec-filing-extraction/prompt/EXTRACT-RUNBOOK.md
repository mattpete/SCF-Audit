# Transcribing a batch in harness

Paste the block under **THE PROMPT** into a fresh Claude Code session at the
repo root. Nothing else. It is written so that Matt's run and Andrew's run
produce the same rows from the same statement.

## Why it is shaped this way

The model does **one** thing: read `c###.txt`, write `res/c###.json`. It does
not choose which filings to do, does not write parquet, does not log progress.
Those are `001i` and `001j`, because:

- two people run at once, and a script claiming work cannot double-claim;
- work must not be markable as done unless rows actually came out of it;
- the rescaling and the footing test have one implementation (`001h`), so two
  people's numbers cannot drift apart.

Everything the model decides is pinned by `prompt/system-prompt.txt` and
`prompt/output-schema.json`. **Those two files are the contract — the prompt
below tells the model to follow them verbatim and not to improvise.** If a
transcription rule needs to change, change it there, not in the runbook, or
the two of us stop producing comparable output.

## Model

**Use Haiku** (`/model claude-haiku-5-5`). Transcription against a fixed schema
is the cheapest thing in this project and there are ~780k statements to get
through; paying Sonnet or Opus rates for typing numbers off a page wastes the
budget we need for everything else.

Escalate **only** the filings the filing step reports under **DOES NOT ADD UP**,
by re-running step 4 for those `c###.txt` on `/model claude-sonnet-5-5`. The
arithmetic not closing is the one case where a better reader is worth the money.

Filings reported under **SUBTOTAL NOT FOUND** are a different problem and must
**not** be escalated: the located text is usually cut short at a page break, so
a stronger model has nothing more to read. Those go to a human to check against
the filing.

On the 91 Walmart cash flow statements this gives **203 of 203 period-columns
footing and zero escalations**, so a filing turning up in either list is worth
taking seriously rather than treating as routine noise.

## The loop

```bash
# 1. claim 25 statements (or BATCH_N=50, etc.)
Rscript 001-sec-filing-extraction/001i-claim-extract-batch.R

# 2. transcribe them in harness -- paste THE PROMPT below, with the batch name

# 3. file them: builds rows, appends to rows/, logs you as having done them
BATCH=extract-matt-20261123-162000 Rscript 001-sec-filing-extraction/001j-file-extract-batch.R
```

**Re-run `001a-build-filing-inventory.R` before claiming your next batch.** Not
optional, and not only when you feel like it. Step 1 picks work from the
inventory parquet, which is a cached join: the progress logs are the source of
truth, but the cache does not know about them until it is rebuilt. Claim twice
without rebuilding and you are handed **the identical filings**, which is how
the second 500-statement wave came back with a 500 of 500 overlap with the
first. The filing step now refuses to store a filing under two batch names, so
the mistake costs transcription tokens rather than a corrupted dataset — but
the tokens are the expensive part. Confirm it worked by checking that
`inv_progress()` shows a non-zero `n_extracted` for you.

Repeat as tokens allow. `inv_progress()` shows where both of you are.

A filing the model marked `found: false`, or skipped, is **not** logged — it
comes back in a later batch. That is deliberate; see the header of `001j`.

---

## THE PROMPT

> You are transcribing cash flow statements from SEC filing text into JSON. This
> is a bulk data-entry task with a fixed contract. Follow it exactly; do not
> improvise, and do not optimise or restructure anything.
>
> **Batch:** `<PASTE THE BATCH NAME 001i PRINTED>`
> **Batch directory:** `$SEC_DATA_PATH/extract-batches/<BATCH>/`
>
> **Step 1.** Read these two files in full and follow them as the specification
> for every transcription you produce. They are the contract; this prompt does
> not restate their rules and does not override them:
> - `001-sec-filing-extraction/prompt/system-prompt.txt`
> - `001-sec-filing-extraction/prompt/output-schema.json`
>
> **Step 2.** Read `manifest.csv` in the batch directory to see how many
> statements there are. Do not retype anything from it — the filing identifiers
> are attached later by script.
>
> **Step 3.** For each `c001.txt` … `cNNN.txt` in the batch directory, in order:
> read the file, then write `res/cNNN.json` in the same batch directory
> containing **only** the JSON object defined by `output-schema.json` — no
> wrapper, no commentary, no markdown fences.
>
> - The statement wanted is named on the file's first line (`WANTED=cf` means
>   the cash flow statement). If that statement is not in the text, return
>   `found: false` with `statement` set to whatever the text actually is. Do not
>   substitute a different statement.
> - The text may start mid-sentence above the statement and continue into the
>   notes below it. Transcribe only the statement itself.
> - Use the Write tool for each JSON file. Do not use shell heredocs: they
>   mangle escapes in this environment.
> - Work in groups of about 3 files, then continue. Do not read the whole batch
>   at once; large reads get truncated silently in the middle of a number.
>
> **Step 4.** Before writing each file, check — do not eyeball it: every row's
> `v` is exactly as long as `columns`, and `n` runs 1,2,3,… with no gaps or
> repeats. If a row printed MORE numbers than there are columns, re-count the
> columns and check whether two rows ran together before anything else; only
> then keep the first `columns` numbers, set `u` true, and list every printed
> number in `va`. Never drop a printed figure and never write such a row as all
> nulls — a dropped figure is invisible downstream, surfacing only as a section
> that fails to add up, with nothing to say why.
>
> **Step 5.** When every `c###.txt` has a `res/c###.json`, stop and say so.
> Report how many you wrote and list any you returned `found: false` for, with
> the reason. Do not run any other script, do not write parquet, and do not
> touch the inventory or the progress logs — `001j` does all of that.
>
> Do not summarise the statements, do not comment on what the numbers mean, and
> do not compute or check any totals beyond the two structural checks in step 4.
