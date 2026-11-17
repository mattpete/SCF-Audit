# 011 — Locating financial statements in SEC filings

Pilot run 2026-11-13/14. Developed in a different repo and copied here; the data
moved to its own OneDrive folder on 2026-11-15, so read **Where the data lives**
before running anything.

This is the single reference for this track. `SEC-FILING-EXTRACTION-NOTES.md`,
which used to sit at the repo root and covered the same pilot, was merged into
this file on 2026-11-15 and deleted. `CLAUDE.md` carries only a short pointer
here, deliberately, because it loads into context every session and these
parameter sweeps do not belong there.

---

## START HERE

### The research question

Measure **revisions between amounts as originally filed and the prior-period
comparatives shown in the following filing**, and use that to test the coverage
of **Audit Analytics** — which can only capture revisions a firm actually
*discloses*. Earlier XBRL work by the author found a substantial number of
revisions absent from AA because they were never disclosed, concentrated in the
statement of cash flows.

Scope decisions already made, not up for re-litigation:

- Full line items **and** subtotals, not just the three cash flow subtotals
- Both 10-Ks and 10-Qs
- 2002 onward — set by Audit Analytics coverage and IBES cash flow forecasts
- **No XBRL.** The author's position: the XBRL data is too messy for this.
- **Not aggregator data.** Individual printed line items are the point.
- Python intended for the extraction/API stage; this pilot is R because that is
  what the WRDS access pattern already used
- Matching to Audit Analytics is later work

Because each 10-K shows three years and each 10-Q shows comparatives, a given
fiscal period appears in up to three filings. The unit of analysis is a
**pair or triple** of (filing, period) observations, which has not been built
yet.

### What this folder establishes

Step 1 only: that the income statement, balance sheet and statement of cash
flows can be **located** inside filings held on WRDS Cloud, without EDGAR
scraping and without XBRL, tightly enough to be affordable to send to an LLM.

| | coverage | 10-K | 10-Q | chosen-chunk median lines |
|---|---|---|---|---|
| cash flow | 99.8% | 100% | 99.7% | 103 |
| income statement | 99.2% | 100% | 98.9% | 77 |
| balance sheet | 99.8% | 100% | 99.7% | 89 |

~4,000 input tokens per filing for all three statements. Full-population cost
~$700 on Haiku with the Batch API. Details below.

### The next step, unambiguously

**Run the extraction pilot.** It needs an Anthropic API key, which the author
did not have during this session. Approximately **$1 on Haiku 5.5 for 500
filings**, and well under a cent for the 60-chunk warm-up.

Everything needed is now written. In order:

```sh
# 1. pull the chunk text off WRDS Cloud (no API key needed, a few minutes)
Rscript 011e-export-chunk-text.R

# 2. project the cost locally, still no API key
python 012a-extract-statements.py estimate
python 012a-extract-statements.py estimate --limit 60

# 3. the warm-up: 60 chunks, spread across statement and form type
python 012a-extract-statements.py submit --limit 60
python 012a-extract-statements.py status      # most batches end within the hour
python 012a-extract-statements.py fetch       # writes extracted.jsonl + real cost

# 4. read the warm-up, fix the prompt, then the full 1,500
python 012a-extract-statements.py submit
```

The prompt itself is `prompt/system-prompt.txt` and `prompt/output-schema.json`.
**Read `prompt/DESIGN.md` before changing either** — it sets out why
the model transcribes and never interprets, what the output shape is, and the
five known weaknesses of the prompt as written. Edit those two files, not the
Python.

`cache/pick9.rds` already holds the chosen chunk boundaries for all 500 sampled
filings, and `011e` turns them into `extract/chunks.jsonl`.

It answers two open questions at once:

1. Can a model read the numbers out of these chunks correctly?
2. Were the chunks right in the first place? If the model reports "there is no
   cash flow statement here," that is a selection failure — a cleaner signal
   than any of the structural proxies below.

Do not spend more effort tuning the regexes before running this. Structural
refinement plateaued; three independent structural measures all land near the
same place and disagree with each other about *which* chunk is right, and that
disagreement cannot be resolved without either the model or hand-labelled
ground truth.

### The research-design question nobody has addressed

**Most differences between an original filing and the next filing's comparative
are not revisions.** They also arise from:

- Discontinued operations reclassification
- Retrospective combinations (poolings, certain acquisitions)
- Line-item relabelling, splitting and recombination
- Change in presentation currency
- Fiscal year changes
- Retrospective adoption of a new standard — **ASU 2016-18 on restricted cash
  hit the statement of cash flows specifically** and moved subtotals for
  thousands of firms at once

That last one matters most here. A mandated retrospective change is not an
error, not a restatement, and correctly absent from Audit Analytics. Counting
it would turn the paper's headline — "AA misses many SCF revisions" — partly
into an artefact. **A definition of what counts as a revision is needed before
the extraction scales**, and it is free to work on now. This is the item most
likely to be expensive if deferred.

Related and also unstarted: matching line items across filings, through
relabelling, splits and merges. Harder than the extraction.

---

## Where the data lives

**Moved to OneDrive 2026-11-15**, into its own folder with its own environment
variable, so this work shares no storage and no path setting with the XBRL
pipeline in `Code/`. The Dropbox originals were copied, not deleted.

```
{SEC_DATA_PATH}/
    pilot-2026-11/
        cache/     49 .rds files  <-- the irreplaceable part, see the sample note below
        scripts/   84 .R files, as originally written
        logs/      27 console captures
        extract/   created by 011e: chunks.jsonl, batch-state.json, extracted.jsonl
        ledger.db  the SQLite inventory - designed, not built yet
        rows/      extracted line items, parquet partitioned by fiscal year
        README.md  copy of this file as of the move
    walmart/       the 50-block worked example plus manifest.csv
```

`SEC_DATA_PATH` is set in `.Renviron` at the repo root, **with no trailing
slash**:

```
SEC_DATA_PATH = "C:/Users/<you>/OneDrive - The University of Alabama/SCF Audit/SEC Extraction"
```

Scripts here resolve it as:

```r
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
```

**Use `paste0`, not `file.path`.** `file.path()` strips a trailing slash --
`file.path("a", "b/cache/")` returns `"a/b/cache"` -- so the `paste0(SP, "x.rds")`
calls throughout these scripts silently produced `cachex.rds` and failed to open.
That was how the scripts arrived in the move; it is fixed, and it is easy to
reintroduce by "tidying" a path back to `file.path`.

`DATA_PATH` still points at the XBRL pipeline's own folder and is not used by
anything here. Check both before running:

```r
Sys.getenv("SEC_DATA_PATH")   # this folder's data
Sys.getenv("DATA_PATH")       # the XBRL pipeline, unrelated
```

The Python stage reads `SEC_DATA_PATH` too, but `.Renviron` is an R file and the
shell never sees it, so `012a-extract-statements.py` parses `.Renviron` itself. Run it
from the repo root, or export the variable.

### 2. Credentials

WRDS credentials come from the OS keyring under exactly these names:

```r
keyring::key_get("wrds-user")
keyring::key_get("wrds-password")
```

Nothing is hardcoded — this was checked. Note for context: an earlier part of
this project hardcoded and committed the WRDS password, and it remains readable
in commits `ac8e9f9` and `93e3f60` **of the original repo**. Rotation was
advised and may not have happened.

Watch out: some older scripts elsewhere in the project use `key_get("wrds user")`
and `key_get("wrds pw")` — with a space, no hyphen. Those names are wrong and
will fail.

### 3. Packages

`DBI`, `RPostgres`, `keyring`, `ssh`. `arrow` for the parquet work. All were
installed and working on this machine.

### 4. WRDS access

| | |
|---|---|
| Postgres | `wrds-pgdata.wharton.upenn.edu:9737`, dbname `wrds`, `sslmode='require'` |
| Cloud (SSH) | `wrds-cloud.wharton.upenn.edu`, no Duo prompt |

Readable: `wrdssec.wrds_forms` (238,420 10-Ks, 1993-11-29 to 2026-11-12),
`comp.funda`, `comp.fundq`, `wrdssec_secsa` (bag-of-words — single words with
counts, no order, no phrases, useless for this).

**Permission denied** (do not waste time retrying): `wrds_xbrl_fundamental`,
`comp_snapshot`, `compsnap`, `comp_pit`, `comph`, `wrdssec_premium`.

**One exception worth an email.** Compustat Snapshot (`wrds_csa_unrestated`) is
purpose-built for exactly this question: 579 columns covering the full income
statement and SCF item set, `first_date` / `last_date` vintage keys, one row per
vintage. It would replace much of this folder. Only the *sample* schema
(`compsamp_snapshot`) is readable here. Ask the librarian before concluding it is
unavailable, because "permission denied for schema" is also what a subscribed
product returns when the account is merely unprovisioned.

Also settled, so it is not retried: `wrdssec_secsa` bag-of-words is
`(accession, word, noccur)` — no order, no phrases, no table structure. Fine for
"does this filing mention free cash flow", useless for recovering a statement,
because the numbers are not attached to anything.

### 5. Filing text on WRDS Cloud

| Path | Contents |
|---|---|
| `/wrds/sec/archives/<wrdsfname>` | raw submission, mean ~9 MB (10-K) |
| `/wrds/sec/wrds_clean_filings/<wrdsfname>` | cleaned text, mean ~0.45 MB |

`wrdsfname` comes from `wrdssec.wrds_forms`. 400/400 sampled filings present.

**No EDGAR scraping.** The author was previously banned from EDGAR on an old
IP address. Everything comes from WRDS; this is settled and should not be
revisited.

Population from 2002 onward: **192,510 10-Ks and 532,242 10-Qs** (724,752).

---

## READ THIS: the sample is not reproducible

Every percentage here refers to a **500-filing random sample** of
`wrdssec.wrds_forms` (10-K and 10-Q, `fdate >= 2002-01-01`); 147 10-K and
353 10-Q, split into dev and held-out halves of 250 each.

`011a-draw-filing-sample.R` has `set.seed(42)` immediately before:

```sql
... ORDER BY random() LIMIT 400
```

`set.seed` seeds R's generator. `random()` runs on the Postgres server, which
never sees it. **The seed does nothing** — re-running draws a different sample.

So `cache/dev_d.rds` and `cache/loc_d.rds` are not a cache of a reproducible
draw. They are the only record of which filings every number here refers to.
**Do not delete them, and do not re-run `011a` expecting the same sample.**

For future sampling, hash server-side instead:

```sql
ORDER BY md5(wrdsfname || 'seed42')
```

---

## The method, as it ended up

Three signals. **Compustat is used only to evaluate, never to select.**

1. **Flexible title match** — any words between the keywords, any whitespace:
   `statements?[[:space:]]+of[[:space:]]+.{0,40}cash[[:space:]]+flows?`.
   Biggest single coverage gain. The earlier fixed-phrase version missed
   "Statements of **Consolidated** Cash Flows" (Kaiser Aluminum) and titles with
   doubled spaces. Income statement and balance sheet titles also match
   "statement of operations" and "statement of financial position".

2. **A terminal marker** gives the chunk a real end — "Total liabilities and
   equity" (balance sheet), "Cash and cash equivalents at end of period" (cash
   flow), the per-share lines (income statement). Where none is found, the chunk
   ends at the next statement title, or at a 300-line cap.

3. **Numeric density** ranks the survivors, with a secondary score for how many
   expected components the chunk contains (operating / investing / financing /
   end-of-period; net income and per-share; total assets) — **as a ranking
   signal, not a gate.** Gating on completeness cost 15–30 points of coverage.

A titled start is preferred over a section-header start, and the chunk begins
**6 lines above** the start marker. That matters more than it sounds: the header
block carries the **scale** ("in thousands") and the **period column dates**,
both sit above the first line item, and without them every number is either
wrong by 1,000× or unassignable to a year.

Two traps found by reading actual text, both fixed, both worth not
reintroducing:

- **Density alone picks fragments.** It is a ratio, so a 13-line MD&A summary of
  the three subtotals beats the real 120-line statement. One pick started at
  "Cash flows from financing activities," mid-statement.
- **Section-header patterns match subtotal lines.** "Net cash provided by
  operating activities" is the *subtotal at the end* of the operating section,
  not the section header. Anchoring on it starts the chunk too low and loses the
  header. But the exclusion must stay loose for *completeness testing*, because
  some firms genuinely title the section "Net Cash Flows From (To) Operating
  Activities" (HNI Corporation). Strict pattern for anchoring, loose pattern for
  testing presence.

### Compustat check (evaluation only)

Share of chosen chunks containing at least two distinct known Compustat values:
cash flow 79%, balance sheet 75%, income statement 54%. Dev and held-out halves
agree within 2 points, so this is not an artefact of tuning.

It is a weak proxy: it matches numbers anywhere in the chunk without reference
to a line label, and items that coincide (net income = income before
extraordinary items) collapse to one match, which penalises the income
statement specifically. Treat it as a smell test.

**Never use Compustat as a rejection test.** Compustat holds the *restated*
figure. When a firm revises, the original filing and Compustat disagree — which
is exactly the population this study is about. An anchor match used as a filter
would systematically discard the treatment group.

Anchors that work (printed as single line items): `oancf`, `ivncf`, `fincf`,
`chech`, `dpc`, `che`, `ni`, `ib`, `txt`, `epspx`, `epsfx`, `xint`, `at`,
`seq`, `ceq`. **Revenue does not work** — firms disaggregate it and Compustat
re-aggregates, so the total never appears in the filing.

---

## What is actually verified

Five cash flow chunks read by hand (`logs/eyeball2.txt`), plus all 25 Walmart
10-Ks in `{SEC_DATA_PATH}/walmart/`. Four of the five were clean —
title, units, period dates, running through the end-of-period line. The fifth
exposed the subtotal-anchoring bug described above.

Everything else is pattern-matching checking its own work.

Extraction accuracy is **completely unmeasured**. No API calls have been made,
no tokens spent.

The extraction scripts are also unrun, and more weakly checked than the rest of
this folder:

- `011e-export-chunk-text.R` HAS now been run (Nov 2026): 1,488 of 1,491
  filing x statement pairs resolved to a block, 1,483 of them at rank 1. The
  base64 transport works. 3 are unresolved, all income statements in 10-Qs.
- `012a-extract-statements.py` has **never been run or even syntax-checked**: there is no
  Python interpreter installed on this machine (`python` resolves to the
  Windows Store stub). Expect to fix a typo or two on first run. `estimate`
  needs no API key, so it is the cheapest way to shake those out.
- The SDK calls follow the current documented shapes — `messages.batches`,
  `output_config.format` for structured outputs, `output_config.effort`. If
  `output_config` raises a `TypeError`, the installed `anthropic` is too old.

---

## The ~10% of 10-Ks with no locatable statement are not failures

All 20 misses in a 200-filing 10-K sample were asset-backed securities trusts —
auto receivables, mortgage and securitization vehicles (Household Auto
Receivables, GSAA Home Equity Trust, BMW Vehicle Owner Trust, Wells Fargo
Commercial Mortgage). They file under Regulation AB and **contain no financial
statements at all**: median 7,462 words against 54,308 where the locator
succeeded, and 19 of 20 mention incorporation by reference.

None are in Compustat, so they drop out at the CIK to GVKEY join. **Sample from
the Compustat universe rather than raw EDGAR and they never appear** — so do not
tune the locator against them.

The residual non-trust misses are Exhibit 13 incorporation by reference, unusual
statement titles, and financial-sector filers whose statements carry different
names.

---

## Known remaining defects

- Income statement is weakest on every measure.
- ~18% of chosen cash flow chunks lack one of the four expected components.
  **Not truncation** — pushing the end out 300 lines changes nothing
  (`diagnostics/check-truncation.R`). Residual label vocabulary.
- 10-Q cleaned text has ~2× the line count of a 10-K at half the bytes per line
  (24 vs 45), because of the one-value-per-line layout. Every threshold here is
  in **lines**, so each is effectively half as wide on those filings. Scaling by
  bytes-per-line is the obvious fix and was never done.
- One filing (Huntmountain Resources, 418 lines) has no statement text in the
  cleaned file at all. Possibly a partial clean-filing record.
- **Unsolved and dangerous:** Schedule I parent-only condensed statements,
  co-registrant statements in combined filings, restatement reconciliations in
  the notes, and press-release exhibits all pass every structural test, because
  they *are* grouped complete statements. Picking the parent-only set gives
  wrong numbers that look right and will even foot correctly.
- Auditor-report proximity was measured (85.1% of chosen 10-K blocks fall after
  the auditor's report, median +480 lines) but never used as a filter.
- **Fiscal year-end changes** produce transition periods that will not align
  across filings. Match on period start and end dates, never on fiscal year
  labels.
- Roughly **40% of located blocks are cell-per-line** rather than row-per-line —
  label split across lines, currency symbols and closing parens on their own
  lines. Walmart alone contains both forms (FY2016 cell-per-line, FY2024
  row-per-line), so it cannot be bracketed by a date rule. This is the main
  argument for LLM extraction over a parser, and it is why footing can only run
  *after* extraction: a subtotal cannot be computed from text that will not
  parse.

---

## Columns, periods, and the one thing the model must not decide

**Never assume three columns.** A 10-K presents three years, so a given fiscal
year appears in up to three consecutive filings and the comparison is triples
rather than pairs — which is what distinguishes "revised once and settled" from
"revised twice". But smaller reporting companies present only **two** years, so
the column count varies with filer status and must be read from the header, not
assumed.

Measured on all 498 cash-flow chunks (`diagnostics/measure-column-order.R`,
`cache/yearorder.rds`):

| | |
|---|---|
| header states the period years explicitly | 488 of 498 |
| states two or more years | 473 |
| **first column is the filing's own fiscal year** | **98.7%** |
| clearly descending (current year first) | 76.7% |
| clearly ascending | 1.5% |
| "mixed" | 21.8% |

**The 1.5% ascending is why the model is never allowed to assign a year to a
column.** If we assumed "first column is current year" we would be wrong on
roughly 7 filings in 500 and could not tell which. A wrong column assignment
does not look wrong — it manufactures a revision where there is none, which is
precisely the thing this paper claims to measure.

So the contract is: the model returns the column headings **verbatim**, in
printed order, and period assignment happens in code against a string we can
read and re-read. Same for the scale: the model returns the word ("thousands")
and never applies it. Applying a scale wrongly is a 1,000× error on every
number in the filing.

The 21.8% "mixed" is mostly not ambiguity. It is **repeated** descending
headers, which is what a four-column 10-Q looks like:

```
                          Three Months Ended        Nine Months Ended
                             September 30,            September 30,
                          2022        2021         2022        2021
Net income                 980         612        2,533       1,404
```

All four columns are real and different. The prompt keeps all four, unmerged,
in printed order; choosing the year-to-date pair (what the following year's
comparative actually restates) is a code decision made by reading the heading
strings. Three things therefore happen downstream, not in the model:

1. **Count check** — every row's values must be as long as `columns`. A
   four-column statement with a two-value row is caught immediately instead of
   silently mis-assigned. `012a` records this as `n_bad_rows`.
2. **Period assignment** — parse "Nine Months Ended September 30, 2022".
3. **Column selection** — keep year-to-date, drop quarter-to-date, by label.

---

## Cost

Measured: ~4,000 input and ~3,000 output tokens per filing (mean; the median is
about half that — banks and insurers have very long statements).

**Both figures are probably low, for two reasons found after they were
written:**

- The token counts came from a chars/4 rule. Claude Haiku 5.5 uses a newer
  tokenizer that counts about **30% more tokens** for the same text, so input
  is nearer 5,000 than 4,000.
- **Haiku 5.5 thinks by default, at effort `medium`, and thinking tokens are
  billed as output.** Output is 79% of the bill. The 3,000-output figure
  assumes no thinking at all. `012a` sends `output_config: {"effort": "low"}`
  for this reason — a bigger cost lever than the choice of model, and one that
  was not in the table below. Do not raise effort without re-measuring.

The 60-chunk warm-up replaces all of this guesswork with the real number:
`fetch` prints actual tokens, actual dollars, and the implied full-corpus cost.

| model | full population | with Batch API (50% off) |
|---|---|---|
| Haiku 5.5 | ~$1,400 | **~$700** |
| Sonnet 5.5 | ~$27,500 | ~$13,750 |
| Opus 5.5 | ~$55,000 | ~$27,500 |

500-filing pilot: **~$1 Haiku**, ~$19 Sonnet, ~$38 Opus.

Output tokens cost 5× input on every model, so the output schema matters more
than the input — compact JSON, short field names, no pretty-printing.

Tiered approach (as in the Owen and Davidson workshop paper the author is
drawing on): cheap model on everything, escalate only filings that fail a
footing check to a stronger model. At ~15% escalation the full population lands
under $3,000 batch.

Use `count_tokens` to get exact figures for free before spending anything on
generation.

---

## Footing checks (free validation, not yet implemented)

Section items should sum to their subtotal. Verified by hand on Walmart FY2024:

```
16,270 + 11,853 + 3,193 − 175 + 2,642 − 797 + 2,017 + 2,515 − 1,324 − 468 = 35,726  ✓
```

Footing **cannot pre-screen** the text. ~41% of filings use a cell-per-line
layout — label fragments, `$` and `)` each on their own line — so a subtotal
cannot be parsed before extraction. Footing has to run *after* the model
returns structured rows.

It does solve a problem that pure text cannot: **orphan sparse rows.** Cleaned
text collapses column alignment, so a row populated in only one year arrives
with no indication of which year. Footing both resolves and assigns them:

```
648,125 − 490,142 − 130,971 = 27,012   ✓
 27,012 −   2,137 −   3,027 = 21,848   ✓  (FY2024 pretax, no 2,410)
 25,942 −   1,836 −   2,410 − 3,000 = 18,696  ✓  (FY2022 pretax, with 2,410)
```

---

## Storage

Statement blocks belong in **parquet** — one row per filing × statement with the
text as a column, partitioned by year — not loose `.txt` files. At full scale
that is ~2.2 million files; Dropbox sync and `list.files()` both degrade on file
*count*, not bytes.

Compression is not the reason (zstd 5.3×, plain tar.gz 5.0× — parquet wins by
about 4%). The reasons are file count, a typed schema instead of filename regex,
and reading metadata without touching the text column. 12.5 GB loose becomes
~2.4 GB.

Trade-off: **parquet costs you `grep`.** The plan to harvest restatement
reconciliation tables by searching for "previously reported" across the blocks
is much easier against plain text. Consider keeping a text copy of the ~6% that
contain restatement language.

---

## Keeping this updatable: the ledger

**Designed, not built.** No ledger exists yet. Everything below is the design
to implement; `012d-build-ledger.R` is the script that would do it.

### Why this is needed before scaling

Nothing in this folder yet distinguishes *not yet attempted* from *attempted
and nothing found*. Both show up as an absent row. At 499 filings that is
survivable. At 725k filings, with ~15–20k new 10-K and 10-Q filings arriving
every quarter, it is fatal: there is no way to run only the new ones, no way to
resume after an interrupted batch, and no way to re-extract under a changed
prompt without either redoing everything or silently mixing two prompt versions
in one dataset.

So the pipeline needs a record of what has been done, to what, under which
rules.

### Two stores, different jobs

| | holds | format | mutable? |
|---|---|---|---|
| **ledger** | what has been done to each filing | SQLite | yes, updated in place |
| **extracted data** | the line items themselves | Parquet, partitioned by fiscal year | no, append-only |

SQLite for the ledger because the ledger is small (~2.2M rows for the whole
corpus) and needs *upserts* — mark this one submitted, mark that one stale.
That is exactly what `.rds` cannot do: an `.rds` is rewritten whole, so a crash
mid-write loses the file, and two processes cannot both touch it. "What is still
pending" becomes a one-line query instead of a script. `RSQLite` is the only
new dependency.

Parquet for the data because that is where the volume is — roughly 650M
label × column cells over the full corpus — and because partitioning by fiscal
year means a quarterly update **writes new files and never rewrites old ones**.
This is the honest answer to the earlier parquet question: it does little for
the ledger and a great deal for the extracted rows.

### Tables

```sql
CREATE TABLE filings (            -- one row per filing
  wrdsfname   TEXT PRIMARY KEY,
  cik         INTEGER, coname TEXT,
  form        TEXT,               -- 10-K, 10-Q, 10-K/A ... keep /A separate
  fdate       TEXT, rdate TEXT,   -- filed date, period end
  fy          INTEGER, fq INTEGER,
  scan_status TEXT,               -- pending | scanned | failed
  scan_ver    TEXT,               -- rules version of 011b
  scanned_at  TEXT
);

CREATE TABLE statements (         -- one row per filing x statement
  wrdsfname   TEXT, kind TEXT,    -- kind: cf | is | bs
  start       INTEGER, end INTEGER, len INTEGER,
  titled      INTEGER, nreq INTEGER, dens REAL,
  locate_status TEXT,             -- pending | located | not_found | ambiguous
  locate_ver  TEXT,               -- rules version of 011c
  text_sha    TEXT,               -- hash of the pulled text
  ext_status  TEXT,               -- pending | submitted | succeeded | failed | stale
  prompt_ver  TEXT,               -- hash of system-prompt.txt + output-schema.json
  model       TEXT, effort TEXT,
  batch_id    TEXT, custom_id TEXT,
  submitted_at TEXT, fetched_at TEXT,
  tokens_in   INTEGER, tokens_out INTEGER, cost REAL,
  stop_reason TEXT,
  found       INTEGER, statement_reported TEXT, scale TEXT,
  n_cols      INTEGER, n_rows INTEGER, n_bad_rows INTEGER, n_seq_ok INTEGER,
  val_status  TEXT,               -- unchecked | passed | failed
  val_note    TEXT,
  PRIMARY KEY (wrdsfname, kind)
);
```

The two things that make this work and are easy to leave out:

1. **`not_found` is a real status, distinct from `pending`.** A filing with no
   locatable cash flow statement has been *answered*. Re-running must not
   retry it, and the coverage statistics must count it.
2. **Every stage records the version of the rules that produced it.**
   `locate_ver`, `prompt_ver`, `model`, `effort`. Without these the dataset
   becomes a mixture of prompt versions with no way to tell which row came from
   which — and no way to answer a referee asking whether the result is robust
   to the extraction method.

### The update loop

Each step filters on status, which is what makes it both incremental and
resumable. Interrupt any step and re-run it; it picks up where it stopped.

```
1. INSERT new filings:  wrds_forms rows with form in (10-K,10-Q,...)
                        and fdate > the ledger's max, not already present
                        -> scan_status = 'pending'
2. 011b marker scan     WHERE scan_status = 'pending'
3. 011c selection       -> statements rows, locate_status located / not_found
4. 011e export text     WHERE locate_status='located' AND ext_status='pending'
                           -> also sets text_sha
5. 012a submit          same filter; writes batch_id, ext_status='submitted'
6. 012a fetch           WHERE ext_status='submitted'
                           -> 'succeeded' / 'failed', usage, diagnostics
                           -> rows appended to parquet, partitioned by fy
7. validation           WHERE ext_status='succeeded' AND val_status='unchecked'
```

### What makes a row stale

Re-extraction is driven by comparison, not by memory:

```sql
UPDATE statements SET ext_status = 'stale'
 WHERE ext_status = 'succeeded' AND prompt_ver <> :current_prompt_ver;
```

Same for `locate_ver` when the regexes change, which cascades: a re-located
chunk has new boundaries, so its extraction is stale too. Old parquet files are
**not** deleted — they carry their `prompt_ver`, so two versions can be compared
directly. That comparison is worth having for its own sake: re-extracting a few
hundred statements under a changed prompt and diffing the numbers is the
cheapest available measure of extraction error.

### Known gaps in this design

- **The seed.** The existing 499-filing sample cannot be regenerated (see the
  seed warning above). Seed the ledger from `dev_d.rds` / `loc_d.rds` as they
  are and let the population grow additively. Do not try to recreate it.
- **WRDS re-cleaning.** `/wrds/sec/wrds_clean_filings/` can be regenerated
  upstream, which would silently invalidate every stored line number. That is
  what `text_sha` is for — but nothing currently checks it, and a changed hash
  needs a decision (re-locate, or trust the old boundaries).
- **Amendments.** A 10-K/A is often exactly where a restatement surfaces, so
  `/A` filings must stay as their own rows and never be deduplicated against
  the original. Whether an amendment counts as "the following filing" for the
  revision test is a research-design question, not a plumbing one.
- **Firms that disappear.** A filing that exists in the ledger and later cannot
  be read on WRDS should be marked, not deleted.

---

## Scripts in this folder

**Numbering convention, matching `Code/` in the parent repo: the number is the
step, the letter is the substep, and the letters run in dependency order.**

| step | what it does |
|---|---|
| **011** | locate the printed statement inside a filing's text |
| **012** | turn that text into structured rows |

Validation substeps come last within their step (`011f`, `012c`). Anything that
is not a step has no number and lives in `diagnostics/`.

### Step 011 — locate the statement

| script | runs | does |
|---|---|---|
| `011a-draw-filing-sample.R` | once | draws the 500-filing sample and probes heading coverage — **saves nothing, see the orphan warning below** |
| `011b-scan-statement-markers.R` | yes | one remote grep per filing: every title, subtotal, roll-forward and scale line, with line numbers → `markers4_raw.rds` |
| `011c-map-numeric-lines.R` | yes | line numbers of every numeric line per filing → `numlines.rds` |
| `011d-build-rank-candidates.R` | yes | builds candidate blocks, scores completeness, ranks → `chunks8.rds` (all, ranked) + `pick9.rds` (rank 1) |
| `011e-export-chunk-text.R` | yes | pulls the chosen text off WRDS → **`extract/chunks.jsonl`**, the handoff to step 012 |
| `011f-export-for-review.pl` | on demand | **VALIDATION.** Writes the located text as human-readable `.txt` with an index, one file per filing × statement |

`011b` and `011c` are independent of each other; both feed `011d`.

### Step 012 — extract the numbers

| script | runs | does |
|---|---|---|
| `012a-extract-statements.py` | **never run** | Batch API runner: `estimate` / `submit` / `status` / `fetch` → `extract/extracted.jsonl` |
| `012b-build-tidy-frame.R` | yes, on test data | one row per filing × statement × line × period, amounts in **actual dollars** → `scf_text_lines.parquet` + the footing check |
| `012c-validate-tidy-frame.R` | — | **not written.** The hand-validation substep |
| `012d-build-ledger.R` | — | **not written.** The inventory/update design in the ledger section above |

Supporting files, not steps:

| file | does |
|---|---|
| `prompt/system-prompt.txt` | the instructions actually sent to the model |
| `prompt/output-schema.json` | the schema the response is constrained to |
| `prompt/DESIGN.md` | why the prompt is shaped this way — **read before changing either of the above.** Written before the Nov 2026 prompt revision, so it predates the column-object format, the per-row `x` flag and the precision-is-not-a-multiplier rule |
| `requirements.txt` | `pip install -U anthropic` |

### `diagnostics/` — not part of any step

One-off scripts written while building the locator. Nothing in the chain reads
their output, and none of them has to run for the pipeline to work. Kept
because each one answered a question that is recorded in this README, and the
measurement would otherwise have to be redone to check a claim.

| script | was | answered |
|---|---|---|
| `compustat-anchor-patterns.R` | `mark3a3.R` | per-filing anchor value patterns |
| `score-chunks.R` | `mark3b3.R` | scores chunks on the server, batched |
| `evaluate-selection.R` | `mark5.R` | the selection evaluation tables |
| `measure-token-size.R` | `bytes.R` | measured characters per chunk |
| `measure-column-order.R` | `yearorder.R` | column order and period years in the header |
| `estimate-api-cost.R` | `cost1.R` | cost arithmetic — **read its warning header** |
| `diagnose-coverage-gaps.R` | `gap1.R` | which filings have no chunk, and why |
| `diagnose-failures.R` | `fail3.R` | failure causes, per statement |
| `check-truncation.R` | `trunc.R` | is the missing component truncation or vocabulary |
| `inspect-chunks.R` | `eye2.R` | prints chunk head and tail for hand-reading |
| `smoke-test-export.R` | new | pulls a few chunks through the real remote transport |

### THE CHAIN CANNOT BE RE-RUN FROM SCRATCH

Three inputs are **orphans** — every live script that reads them exists, but
nothing in this repo writes them:

| orphan | read by | what it is |
|---|---|---|
| `dev_d.rds`, `loc_d.rds` | `011b` | the 500-filing sample, dev and held-out |
| `markers3.rds` | `011c` | an earlier marker pass |

They were written in interactive sessions that no longer exist. `011a` draws a
sample but saves nothing, so re-running it does not regenerate them, and
`set.seed(42)` does not make the draw reproducible because the surrounding query
has changed. **Do not delete these three files.** Rebuilding the chain from
nothing means first writing the two steps that produce them.

`011c-map-numeric-lines.R` is the most reusable piece: it returns the position of
every numeric line per filing, which lets **any** boundary rule be scored
locally with no further SSH. Re-tuning boundaries does not require going back to
the server.


### Cache files worth knowing

| file | contents |
|---|---|
| `dev_d.rds`, `loc_d.rds` | **the irreplaceable sample** (dev / held-out) |
| `pick9.rds` | **the chosen chunk per filing × statement — the pilot's input** |
| `chunks8.rds` | the full candidate set before selection |
| `markers4_raw.rds` | every title and marker line with line numbers |
| `numlines.rds` | numeric line positions, 499 filings |
| `bytes.rds` | measured characters per chosen chunk |
| `yearorder.rds` | column order / period years per cash-flow chunk |
| `chunktext.rds` | the pulled chunk text, written by `011e` (not yet run) |
| `scored3.rds` | Compustat anchor scores |
| `gaps.rds`, `fails2.rds` | coverage failures with diagnosed causes |
| `headings.rds`, `neigh.rds`, `anchor2.rds` | superseded adjacency / anchor approaches |

---

## Environment gotchas that cost time

- **`scp_upload` fails against `~`** on WRDS Cloud ("No such file or directory"
  — the tilde is not expanded). Stream input lists through a quoted heredoc to
  `/tmp/` instead. Every script here does that.
- **A single SSH exec over more than a few hundred filings gets disconnected.**
  Batch and reconnect per batch, with a retry (`diagnostics/score-chunks.R`, `011c`, `diagnostics/measure-token-size.R`).
- **Write R scripts containing backslashes with a file-writing tool, not a bash
  heredoc.** A heredoc eats one level: `"\\.parquet$"` arrives as
  `"\.parquet$"`, silently breaking the regex.
- **`awk` programs do not survive the R-string → shell-heredoc → shell-quoting
  path.** Use `grep | cut | tr` instead. One pass silently returned zero rows
  because of this.
- **`Rscript -e '<multi-line script>'` segfaulted** on this machine. Write the
  script to a file and run the file.
- **Redirect script output to a file rather than piping to `tail`.** A pipe
  buffers, so a long-running job appears to produce nothing.
- On this machine R is at `/c/Program Files/R/R-4.5.3/bin/Rscript.exe`.
  `Rscript` is not on `PATH` in the Bash tool.
- `wrdssec.wrds_forms` has a column named **`form`**, not `form_type`.

---

## Still unstarted

In rough order of value:

1. **The API pilot.** ~$1, and the scripts for it are written and waiting —
   see *The next step, unambiguously*. Needs an API key and a Python install.
   Everything else is downstream of it.
2. **A definition of revision vs reclassification.** Free, and the item most
   likely to be expensive if deferred. See above.
3. **The ledger** (`012d-build-ledger.R`, designed above, not written). Needed
   before the extraction scales past the pilot, not after — retro-fitting
   status and version columns onto a dataset already extracted means
   re-extracting it. Cheap to build now: one SQLite file, one script, no API.
4. **The pair/triple frame** — SQL join of `wrds_forms` to Compustat on CIK,
   self-joined on firm × period, to size the project and produce the sampling
   frame. Offered several times, never started.
5. **Cross-filing line-item matching.** Harder than extraction, and the
   place where line order (`n`) earns its cost.
6. **Restatement reconciliation tables** as a free self-labelling validation
   set — ~6% of blocks contain "previously reported" / "as restated".
7. **The Audit Analytics baseline** — counts by year, SCF-affecting, Big R vs
   little r. The denominator the paper's claim is measured against.
8. **Scale the marker scan to the full population** — ~725k filings, no money,
   roughly a day of runtime, less on the WRDS grid. Turns a 499-filing sample
   into a real frame and exposes systematic failures a sample cannot show.
9. Fix the 10-Q line-vs-bytes threshold problem.
10. Implement auditor-report proximity as a filter.

## Working notes

The author's stated preferences, learned the hard way during this session:
increment in small steps and report briefly; do not produce long write-ups; do
not start work without being asked; and do not re-explain decisions already
made.
