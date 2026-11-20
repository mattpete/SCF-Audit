# 001 — Locating financial statements in SEC filings

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

### The five steps, and where each one stands

Agreed structure, Matt, 2026-11-19. Scripts are numbered by step.

| step | what it does | script | state |
|---|---|---|---|
| **1** | build the inventory: every filing we could want, with the screens as columns | `001a-build-filing-inventory.R` | **built and run.** 913,756 rows, 780,364 filings to fetch |
| **2** | pull the chunk text off WRDS for the uncollected part of the inventory | `001g` (pull) + `001e`/`001f` (locate) | locating works, 96.6% on a fresh holdout. Not yet wired to the inventory |
| **3** | turn chunk text into JSON line items — **in harness, not an API** | none; done by hand in a session | run once, on 10 filings. 1,890 rows, 15 of 18 cash flow columns footed |
| **4** | JSON into a tidy parquet | `001h-build-tidy-frame.R` | built; produced the frame Matt reviewed 2026-11-16 |
| **5** | compare filings to find revised line items | none yet — **but see below** | not started |

**Step 5 is mostly already written, in the other track.** `Code/001a-extract-scf-lines-and-changes.R`
holds the original-vs-last-comparative anchoring, the per-line sign resolution,
the section assignment and the reconciliation gate, and every one of those
rules exists because a specific filing broke the simpler version. If step 4's
output is shaped like that script's `scf_lines_parts/`, step 5 is a schema
mapping rather than a rewrite.

That is reusing the **code**, not merging the **data**. Stitching this track's
pre-2009 measurements onto the XBRL panel's post-2009 ones would put a
measurement break in the middle of the panel; this track covers the whole
2002–2025 period on its own and stands as an independent second measurement.
Settled 2026-11-19.

### If you are picking this up (Andrew, start here)

Three things before anything runs:

1. **`.Renviron` at the repo root** needs `SEC_DATA_PATH` (no trailing slash —
   these scripts build paths with `paste0`) and `SCF_WHO`, which is your name
   in lower case and decides which filings are yours:
   ```
   SEC_DATA_PATH = "C:/Users/<you>/OneDrive - .../SCF Audit/SEC Extraction"
   SCF_WHO = "andrew"
   ```
2. **Run scripts from the repo root**, not from this folder. `.Renviron` lives
   at the root, so R started here gets an empty `SEC_DATA_PATH` and writes to
   the root of your C: drive. There is a guard that stops it now.
3. **WRDS credentials** come from the OS keyring as `keyring::key_get("wrds-user")`
   and `keyring::key_get("wrds-password")`. Older scripts elsewhere in the repo
   use `"wrds user"` / `"wrds pw"` with a space; those are wrong.

Then:

```r
source("001-sec-filing-extraction/000-inventory-helpers.R")
inv_progress()                      # who has done what
b <- inv_todo("collect", n = 250)   # your next batch, nobody else's
```

### Two people, one inventory, no collisions

The inventory is on OneDrive, and **two machines writing one parquet produces a
silent conflict copy** — `filing_inventory-DESKTOP-ABC.parquet` — with no error
and one person's afternoon gone. So:

- **Nobody writes the inventory.** It is a cached join, rebuilt by re-running
  `001a`.
- Progress lives in **append-only per-person logs**,
  `inventory/logs/progress_{SCF_WHO}.csv`. You write only your own file.
- `001a` reads every log it finds and derives `collected` / `extracted` from
  them. Delete the inventory and rebuild: the logs restore exactly where
  everyone was.
- **Filings are assigned by CIK**, not by row, so (a) a firm is never split
  between two people — step 5 needs all of a firm's filings together — and
  (b) assignment does not reshuffle when the inventory grows, because it is a
  function of the CIK and nothing else. Currently 393,026 to matt and 387,338
  to andrew, in batches of 250.
- Chunk output follows the same rule: write `chunks_2014_matt_01.parquet`, not
  into a shared year file. `arrow::open_dataset()` reads a folder as one table.

### Storage, measured rather than guessed

From the 488-filing holdout, at 37.6 bytes per line (measured on real exported
chunks, not estimated):

| what ships | lines per filing | per filing | 780,364 filings |
|---|---|---|---|
| best block per statement | 679 | 25.6 KB | **18.6 GB** |
| every candidate block | 1,020 | 38.4 KB | 27.9 GB |

gzip is a measured 32.5% on these files, so **~6 GB stored compressed**.

**Do not store it as 780,000 loose text files.** OneDrive syncs file by file
and every collaborator downloads all of them; `scf_lines_parts/` in the other
track is already 451 MB across 85 files and is a known nuisance. Put the chunk
text in a column in parquet, partitioned by year — about 31,000 filings and
260 MB per year, 25 files.

Keep a **working folder of loose `.txt`** for the batch you are about to hand
to a model, though. That is what `holdout-2026-11/extract/chunktext/` is, and
it is how the step-3 extraction is actually done.

Disk is not the constraint. Those same 679 lines per filing are the tokens
step 3 pays for, which is where the ~$7,000 full-population figure comes from —
so shipping every candidate block instead of the best one costs +50% on
extraction, not just on disk. That is the argument for locator work.

### What this folder established first

That the three statements can be **located** inside filings held on WRDS Cloud,
without EDGAR scraping and without XBRL, tightly enough to be affordable.
Measured on a fresh 500-filing holdout drawn to exclude the filings the rules
were tuned on: **483 covered (96.6%)**, 483 of 488 excluding asset-backed
trusts (99.0%), 3 genuine misses on ordinary operating companies. The per-statement
and per-form detail is under "Measured" below.

### The extraction is done in harness, in batches

**Not through the paid Batch API.** Matt's decision, 2026-11-19: the text is
handed to a model inside a working session, a few hundred filings at a time,
which also gives a transcription to check the chunks against. The Python
`legacy/extract-statements.py` path and the cost projections below are kept for
reference, not as the plan.

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
        extract/   created by legacy/export-chunk-text-approach-a: chunks.jsonl, batch-state.json, extracted.jsonl
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
shell never sees it, so `legacy/extract-statements.py` parses `.Renviron` itself. Run it
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

## Step 1: the inventory (`001a-build-filing-inventory.R`)

One row per **(accession, registrant CIK)** for every annual and quarterly
report since 2002, written to `{SEC_DATA_PATH}/inventory/filing_inventory.parquet`.
Built from two WRDS tables: `wrdssec.wrds_forms` (the filing index) and
`wrdssec.wrds_forms_reg`, which is one row per registrant per filing and the
only place the SEC-assigned SIC appears.

| | filings | filers |
|---|---|---|
| all annual + quarterly reports, period 2001–2025 | 913,756 | 36,997 |
| in sample | 799,913 | 24,065 |
| distinct filings to fetch | **780,364** | |

### Screens are columns, not filters

Every screen lands as a flag plus a `drop_reason` string, and `in_sample` is
derived from them. The sample definition is **not settled** — whether banks,
insurers and utilities stay is an open research decision — and a filtered
inventory would mean re-collecting text every time that decision moved.
Changing your mind is a one-line edit and a re-run of this script only.

### Four things the index had to settle, not assume

**Small-business forms have no hyphen.** They are `10KSB` and `10QSB`. A form
list built with `form LIKE '10-%'` silently drops **109,057 filings from ~8,000
filers**, every one of them between 2002 and 2009 — precisely the pre-XBRL
window this track exists to reach. `10-K405` and `10KSB40` are also real annual
reports (the Item 405 delinquency variant, retired in 2003), and transition
reports `10-KT` / `10-QT` carry real statements for a shortened year, which is
one of the legitimate reasons comparatives get recast. Deliberately excluded:
`10-D` (143,330 filings, an asset-backed distribution report), `10-12B`/`10-12G`
(registration statements), `NT 10-K` (a notice of late filing).

**SIC 6000–6999 is too blunt.** It bundles banks and insurers, which have
ordinary cash flow statements where classification matters, with entities that
are not operating companies at all. What is actually in there:

| | filers | filings |
|---|---|---|
| **6189 asset-backed trusts** | **9,914** | 24,918 |
| **6770 blank checks / shells / SPACs** | 3,434 | 33,770 |
| 6798 REITs | 783 | 31,481 |
| banks (6021/6022/6035/6036) | 1,612 | 54,959 |

Asset-backed trusts are **27% of the entire filer list** and file a 10-K of
servicing reports with no financial statements — they are the five of 500
holdout filings with nothing locatable. Dropped: 6189, 6770, 6798, 6726, 6792,
6794, 6795, 6799. Kept: banks, insurers, brokers, real-estate operators,
utilities. Note this is a cleanup, **not a cost lever** — junk filers file twice
and vanish, so dropping 12,000 of them removes only 11% of filings.

**The filing window and the panel window are different.** `DATE_*` bounds the
filing date and exists only to bound the pull; `PERIOD_*` bounds the period of
report and is what the panel actually is. Two measured failures from conflating
them: bounding only the filing date let 2002-filed amendments of 1986–2000
periods in, with no original filing in the collection to compare against; and
cutting the filing date at the end of the panel clipped the annual reports that
*carry* the last year's statements, which are filed the following spring —
1,155 annual filings in that year against ~6,600 in a normal one.

Right-edge censoring is a separate problem that narrowing the window does not
fix. Detecting a revision needs a later filing to re-present the period, so the
most recent two years can only ever look clean — the same effect the XBRL panel
flags as `era == "censored right edge"`. Those filings are still collected,
because they are the comparatives that reveal revisions in earlier periods.

**Accession alone is not unique, and `wrdsfname` does not fix it.** A co-filed
submission has one index row per registrant: AMERCO and U-Haul International
share accession `0000004457-04-000004`. 12,963 accessions are affected and one
appears 67 times. WRDS stores a **separate copy of the text per registrant**
(`000000/4457/...` and `000000/4458/...`, identical content), so grouping on the
filename collapses nothing — measured, zero rows. Therefore:

- the status carry-over keys on `(accession, cik)`
- `collect_primary` dedupes on `accession`, avoiding 19,031 duplicate fetches
- `collect_primary` must fall on an **in-sample** row. A REIT parent (6798,
  dropped) co-filing with an operating subsidiary (6500, kept) otherwise makes
  the dropped registrant primary and the filing is never collected although an
  in-sample row needs it. That cost 458 filings before the sort was fixed.

**Open problem, not solved:** a co-filed submission contains the statements of
*both* registrants, so one chunk can hold two income statements. That is the
International Bancshares / United Airlines case already recorded in `001f`.
`n_registrant_rows > 1` is the flag for it.

### Verified

- Walmart runs 1 annual + 3 quarterly every year from 2002 to 2023.
- Akorn is CIK **0000003116**, and all its amendments are present: the Q1 2012
  10-Q/A, the Q2 2014 10-Q/A, the Q2 and Q3 refilings of April 2015, and the
  FY2014 10-K/A. The amendment channel is the disclosed channel; losing it
  would lose half the design. (Checking CIK 1070494 by mistake produced a clean
  series with no amendments at all and a false alarm — the CIK is worth
  double-checking before believing a result like that.)
- `(accession, cik)` is unique; assignment is a pure function of the CIK; no
  firm is split between two people.
- A log round trip works: claim ten, log them, rebuild, and the collect queue
  skips them while the extract queue offers them.

## READ THIS: the sample is not reproducible

Every percentage here refers to a **500-filing random sample** of
`wrdssec.wrds_forms` (10-K and 10-Q, `fdate >= 2002-01-01`); 147 10-K and
353 10-Q, split into dev and held-out halves of 250 each.

`legacy/draw-filing-sample.R` has `set.seed(42)` immediately before:

```sql
... ORDER BY random() LIMIT 400
```

`set.seed` seeds R's generator. `random()` runs on the Postgres server, which
never sees it. **The seed does nothing** — re-running draws a different sample.

So `cache/dev_d.rds` and `cache/loc_d.rds` are not a cache of a reproducible
draw. They are the only record of which filings every number here refers to.
**Do not delete them, and do not re-run `legacy/draw-filing-sample` expecting the same sample.**

For future sampling, hash server-side instead:

```sql
ORDER BY md5(wrdsfname || 'seed42')
```

---

## The method, as it ended up

**Rewritten 2026-11-17 after Matt hand-audited four rounds of ten blocks.** The
previous design — generate candidate windows, then rank them by "complete, then
shortest, then most number-dense" — is gone. The section below describes what
replaced it; the old approach is in git history and is summarised at the end of
this section so it does not get reinvented.

**Compustat is used only to evaluate, never to select.**

### The principle: a statement has a findable top and a findable bottom

Bracket each statement between its own landmarks and take everything in
between. **Length and numeric density are not used to choose anything.** They
are recorded, and that is all.

This is the whole of it, and it matters because the thing it replaced had a
systematic bias. A statement's *tail* is shorter than the whole statement and
denser in numbers, because working-capital lines are nothing but figures. So
"shortest and densest" preferred fragments by construction.

Evergy Kansas Central's Q2 2021 10-Q is the case that killed the old design.
Its chosen cash flow block began at "Prepaid expenses and other current assets"
— no net income, no depreciation, none of the reconciling adjustments — and it
scored *complete*, because completeness tested for the three section subtotals
and the cash roll-forward, all of which sit in the bottom half of the statement.
The whole-statement candidate existed and ranked 22nd.

### Required structure, as groups

Each statement must show every landmark below inside the span; a group is
satisfied by any one of its members. The **first group in each is the top**,
which is what a clipped statement cannot produce.

| statement | top | middle | close |
|---|---|---|---|
| cash flow | title, operating-section banner, or "adjustments to reconcile" | operating, investing and financing sections | net change in cash / end of period |
| income | title or the revenue line | net income | per-share lines |
| balance | title, `Assets` heading, or total current assets | total assets, or the `Assets` heading | total liabilities and equity, or the `Liabilities and ...equity` heading |

### A header carries no figures

The operating-section **banner** and the operating **subtotal** are worded
almost identically, and telling them apart is what keeps a span from opening at
the bottom of the section. Excluding "net cash" and "provided by" is not enough:
Evergy writes its subtotal as `Cash Flows from Operating Activities 246.1 211.6`,
which contains none of those words.

The discriminator is that **a header carries no digits**. On Evergy it accepts
lines 43874 and 43954 (banners) and rejects 43914 and 43994 (subtotals), which
is correct in every case.

One known cost, carried over from the old notes: a few firms genuinely title
the section "Net Cash Flows From (To) Operating Activities" (HNI Corporation),
and the banner rule excludes "net cash", so their section header is not
accepted as a top. The title and the "adjustments to reconcile" line are the
other two members of the cash flow top group, and one of them covers those
filings — which is why the top is a group of three rather than one pattern.

### A statement contains figures

`nnum >= 8` is a hard requirement, not a preference. This is what rejects the
**XBRL taxonomy-definition blocks**: a filing's XBRL section carries each
statement twice, once as a rendering with values and once as a definition
listing element names with none, and the definition block satisfies every
word-based landmark because it is built from the statement's own labels.
Bloomia Holdings' chosen income statement had **nnum = 0** — 104 lines of
"income statement" containing not one number. No marker pattern can ever
exclude that, because the words are right.

### A title is only a title if figures follow it

A table of contents lists "Consolidated Statements of Income" word for word, and
so does an MD&A heading and an XBRL role definition. Two tests, because the
first alone is not enough:

- at least 3 numeric lines within 40 lines below the anchor; and
- fewer than 3 statement titles within the 45 lines below it.

Citizens Holding needed the second: its contents list is immediately followed by
the balance sheet, so figures *do* appear below it, and its income statement
block opened at the contents list and ran 200 lines to the real statement's EPS,
scoring complete the whole way. A contents list stacks the titles together;
real statements are separated by their own bodies.

### The three statements are adjacent, so pick them together

The balance sheet, income statement and cash flow statement sit next to each
other in a filing. The old per-statement ranking threw that away: measured on
the 490 filings that had all three picks, only **2.2%** had them within 200
lines of each other and **38.8%** were picked more than 5,000 lines apart.

Selection is now joint. The cash flow candidate leads, each income and balance
sheet candidate is scored on quality tier first and distance from it second, and
the triple with the best total wins. Quality dominates; distance breaks ties; a
longer cash flow span wins a dead heat, which prefers a consolidated statement
over a condensed or parent-only one.

This resolves combined filings for free. Evergy files one 10-Q holding
statements for Evergy Inc, Evergy Metro and Evergy Kansas Central, and each
registrant's three statements are together, so the tight triple is one
registrant's set.

| triple spread | before | after |
|---|---|---|
| within 1,000 lines | 39.3% | 69.8% |
| more than 5,000 lines apart | 38.8% | 5.6% |
| worst case | 54,327 lines | 21,672 |

### Boundaries, and the two that were wrong

**Right edge.** A title of a *different* statement is a hard stop. A title of
the *same* statement is ambiguous — either a page-break continuation
("...(continued)") or the next registrant's copy — and the close marker tells
them apart: if the statement has not reached its close yet, the title is a
continuation, so keep going. Stopping at both clipped 265 cash flow spans
mid-statement, which surfaced as the investing and financing sections going
missing from spans that plainly had an operating one.

**The close itself** is walked, not taken from either end. The first close
marker alone stops short, because "net decrease in cash" precedes the beginning
and ending balances; the last one in range runs into the next statement. So
start at the first and extend while the markers stay contiguous (`CLOSE_GAP`,
40 lines). Taking the last close in range stretched Select Comfort's cash flow
block from 52 lines to 836.

**Left edge.** The span opens above the top anchor to capture the header, which
carries the scale and the period dates. That pull-up is floored at the previous
statement's **close**, not its title — flooring at the title still swallows the
previous statement's last lines, which is how Citizens Holding's income
statement opened on the tail of the balance sheet. `FLOOR_SLACK` of 3 lines
clears a value orphaned from its label by the HTML-to-text wrap (Innovate
Biopharmaceuticals' `(deficit) $ 13,285,014`).

The equity statement is tracked as a boundary for this reason alone. We never
extract it, but it is printed between the income statement and the cash flow
statement, and without a pattern for its title it is invisible — which is how
Donnkenny's cash flow block came to open on the Statement of Stockholders'
Equity.

### A balance sheet may label no total at all

Amerisafe's 10-K prints total assets as `$ 1,148,509 $ 1,117,923` on a line of
its own, the HTML label cell being empty. No word pattern can reach that line,
so its real balance sheet produced **no candidate whatsoever** and the locator
was left choosing among footnote tables.

Two things follow. The `Assets` and `Liabilities and ...equity` **section
headings** are grepped, anchored and with "total" disallowed, because they are
the only layout-independent landmarks a balance sheet has. And the balance sheet
— *only* the balance sheet — may close at the boundary when no total is
labelled anywhere. An unlabelled total is a balance-sheet habit; the cash flow
statement closes on "net increase in cash" and the income statement on EPS, both
named. Allowing the boundary close for all three only manufactured
low-confidence spans for filings that had correctly produced none.

Note the heading that opens the liability side is **not** a close. Listing it as
one ended Amerisafe's span at "Liabilities:" and dropped the entire liability
and equity side.

### The restatement footnote is the dangerous false positive

A footnote reconciling a restated statement is captioned with the statement's
own name and carries its totals, so it satisfies every structural landmark.
Amerisafe's `As Reported / Adjustments / As Adjusted` table was chosen as its
balance sheet, on 7 line items.

The column captions are what a primary statement never has, and they are now
detected in the span's header region: `as (previously) reported/adjusted/
restated/revised`, `previously reported`, `effect of ... restatement`. Header
region only, so a statement that merely references a revision further down in a
note is not thrown away for it.

**For this project specifically this is the worst possible false positive**,
because the restatement footnote is exactly where the revisions we are trying to
measure get described.

### Page furniture is removed from the text, not reasoned about

HTML filings put a "Table of Contents" back-link at the top of every page, so it
lands *inside* a statement wherever the statement crosses a page boundary — not
only between statements. The string therefore cannot be used as a boundary
signal at all. Same for the notes reference, the page number and "(continued)",
and XBRL-rendered filings carry the viewer's JavaScript at the top of the block.

`legacy/export-chunk-text-approach-a` strips all of it. Measured on 1,412 blocks: 2,938 furniture lines across
1,084 blocks (77%), the back-link alone in 560 blocks with 16% of its
occurrences mid-block. Safety check on the same data: **zero** furniture lines
carried a figure with a comma or a dollar sign. The patterns are anchored to
whole lines for that reason, and there is deliberately **no bare-number rule**,
because a lone "3" is as likely to be an amount as a page number.

`len` and `nnum` still describe the original line range, so any block can be
pulled off WRDS again by its line numbers.

### Patterns that had to exist, and one that had to go

Every one of these came from a block Matt read and rejected.

| landmark | why it was missing |
|---|---|
| bare `OPERATING ACTIVITIES:` | only "cash flows from operating..." was grepped, so 107 spans "lacked" a section that was there |
| `total/net interest income`, premiums, deposits | a bank has no "net sales" and no "total revenues"; the income-statement top was unfindable for the whole sector, failing 505 spans |
| `Revenue 1,234` with amounts inline | the bare pattern was anchored to a label printed alone |
| `Net profit / (loss)` | income, loss and earnings were matched; profit was not |
| `Assets` / `Liabilities and ...equity` headings | see the unlabelled-total case above |
| ~~`revenue from contracts with customers`~~ | **removed.** It matches the ASC 606 reference in prose and the XBRL element labels (`revenue from contract with customer [abstract]`), and it picked Clean Energy Technologies' accounting-policies footnote as its income statement. Exactly 3 filings in 500 had it as their only revenue landmark. |

### What the old design was, so it is not reinvented

Three signals: a flexible title match, a terminal marker, and **numeric density
to rank the survivors**, with completeness as a ranking preference rather than a
gate. The stated rationale was that ranking only had to be cheap, because a
validate-and-retry loop in `legacy/export-chunk-text-approach-a` would walk down the ranks until the block
really was a statement — so the sort was tuned for *size*, to keep the API bill
down.

Both halves of that failed. `legacy/export-chunk-text-approach-a`'s validator was later loosened and the retry
loop stopped firing, so nothing caught the bad picks; and gating on completeness
had been rejected for costing "15–30 points of coverage" against a completeness
test that could not see a tail, which made the coverage it was protecting partly
illusory. The honest comparison is below: coverage is *lower* and correctness is
much higher, and the 99.8% was never real.

| | old | new |
|---|---|---|
| cash flow block located | 99.8% | 91.2% |
| cash flow block structurally complete | 84.3%\* | 94.8% |
| cash flow block starts at a title | 32.5% | 97.9% |
| income block complete | — | 86.4% |
| balance sheet block complete | — | 97.6% |

\* measured by a test that could not detect a clipped statement.

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

### Hand audit, Matt, 2026-11-17 — the only real evidence here

Four rounds of ten located blocks, drawn at random across all three statements,
printed exactly as the model would receive them and read one by one. Files are
in `{SEC_DATA_PATH}/pilot-2026-11/REVIEW/locator-audit-2026-11-17/`, each with
a scoring sheet. Verdicts: **GOOD** (the whole statement), **EXTRA** (the
statement plus material dragged in with it), **CUT** (right statement, clipped),
**WRONG** (not that statement at all). Only WRONG means coverage is overstated.

| round | verdict | the defect it found |
|---|---|---|
| 1 | 8 good, 1 extra, 1 wrong | density ranking picks statement tails (Evergy); pull-up swallows the previous statement (Citizens Holding, Innovate) |
| 2 | 9 good, 1 wrong | XBRL taxonomy-definition block with nnum = 0, and a bare "Revenue" line that was never grepped (Bloomia) |
| 3 | 9 good, 1 wrong | the ASC 606 reference in prose matched as a revenue line, picking an accounting-policies footnote (Clean Energy) |
| 4 | **10 good** | — |

**Read the rate honestly.** Rounds 1 to 3 tested code that no longer exists, so
the only clean read on the current design is round 4. Ten cases with no failures
puts the 95% upper bound on the error rate at about 26%. That is encouraging,
not established, and the obvious next move is more rounds of ten: the yield has
been roughly one real defect per ten blocks, and every one has had a different
root cause.

Each defect is written up in "The method, as it ended up" beside the rule that
now prevents it. **Before loosening any of those rules, find the named case.**

Earlier, weaker checks that still stand: five cash flow chunks read by hand
(`logs/eyeball2.txt`) and all 25 Walmart 10-Ks in `{SEC_DATA_PATH}/walmart/`.

Everything else is pattern-matching checking its own work.

Extraction accuracy is **completely unmeasured**. No API calls have been made,
no tokens spent.

The extraction scripts are also unrun, and more weakly checked than the rest of
this folder:

- `legacy/export-chunk-text-approach-a.R` HAS now been run (latest: 2026-11-17, after the
  locator rewrite): 1,422 of 1,425 filing x statement slots resolved to a
  block. Fewer slots than the old design offered (1,491) because the structural
  test refuses to emit a span that is missing a landmark, where the old one
  emitted a fragment and scored it complete. `legacy/export-chunk-text-approach-a` also now strips page
  furniture from the text — see the method section. The
  base64 transport works. 3 are unresolved, all income statements in 10-Qs.
- `legacy/extract-statements.py` has **never been run or even syntax-checked**: there is no
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

**Reproduced independently on a fresh sample, 2026-11-18.** 12 of the 17 misses
on the 500-filing holdout below were asset-backed trusts, with no statement
title of any kind anywhere in the document — Household Automotive Trust, WaMu
Mortgage Pass Through, GS Mortgage Securities, Santander Drive Auto
Receivables, RASC, CWABS, JP Morgan Commercial Mortgage and more. Two others
were a royalty trust (San Juan Basin, which presents "Statements of
Distributable Income" and "Assets, Liabilities and Trust Corpus") and a
commodity trading partnership (Nestor Partners). The same conclusion holds:
these are entity types that present no conventional statements, not locator
failures, and the Compustat screen removes them.

---

## Approach C: cut the statements apart at their page headers (`001e` + `001f`)

Three locators now exist for the same job and all three are live. **A** is
`legacy/build-rank-candidates`: search the whole filing once per statement, require that statement's
full structure, use adjacency as a tiebreak. **B** is `001d-locate-anchored.R`:
find the cash flow statement under strict rules, then bracket a section around
it and look for the other two inside with the requirements relaxed. **C** is
`001f`, below, and it is the one that works best.

**The page-header invariant.** Every statement page prints the same six things
within about twenty lines: a page number, the company name, the statement
title, the scale, the period header, and the column years. Footnote tables
never carry all of that — above all they do not reprint the period. So a title
only becomes a header when a period line or column dates sit just below it, and
**a statement runs from its header to the next header.** No content landmark is
needed, which is what frees C from the structure requirements that made A and B
miss filings with no EPS line or no labelled totals.

Two gates remove the impostors. A contents list prints its entries stacked
together, so a title crowded by other titles is rejected; and a contents entry
is followed by a page number rather than figures, so a title without at least
six numeric lines below it is rejected.

### Four fixes from four filings, 2026-11-18

Every one came from a single filing that broke the obvious version. The case is
named in the comment beside each rule in `001f`; do not simplify without
reading it.

| fix | the filing that exposed it |
|---|---|
| the crowding test counted **grep rows, not distinct titles** — and 001e greps joined line pairs, so one title counts twice. Any second title within the window then rejected both | **D.R. Horton** Q3 2006. A textbook balance sheet header at line 191, killed by an exhibit-index line, "Statement of Computation of Ratio of Earnings to Fixed Charges", sitting exactly 20 lines above it |
| run selection preferred the run **nearest** the anchor. Some run is always nearest, and nothing checked the anchor was in it | **CIK 72207** 2007. 001d's anchor landed at 2861–2919, the statements are at 166–781, and a junk run was a thousand lines closer. The right run won only because the junk run was below `RUN_MIN` |
| `Income Statements` — the noun first, no "of" anywhere | **DuPont** Q2 2004 |
| `Cashflows` as one word | **Norwood Financial** Q3 2002. The most alarming of the four: the cash flow statement, invisible for want of a space |

Run selection now asks, in order: does the run **hold** the anchor; how many
**different** statements are in it; does it hold a cash flow statement at all;
and only then, how near is it.

### A header with no period line still ends the block above it, 2026-11-19

The gates above decide whether a title is a header. But "is this a header" was
answering two different questions at once, and they need different answers:

- **ship this block?** — strict. All three gates.
- **does a block END here, and does the section CONTINUE?** — lenient. A title
  that fails the period and figures tests is still page furniture, and it still
  occupies the space between two statements.

**World Wrestling Entertainment's** FY2015 10-K is the case. Its equity
statement header reads

```
6059  table of contents
6061  world wrestling entertainment, inc.
6063  consolidated statements of stockholders equity
6065  (in thousands, except per share data)
```

— four of the six header elements, and no period line anywhere near it, because
an equity statement's columns are **accounts** (Shares, Amount, Capital), not
periods. Discarding it left a 616-line gap, sixteen lines over `RUN_GAP`, which
stranded the cash flow header in a run of one and lost the statement. The
mechanism is not rare: **128 of 488 holdout filings (26.2%)** have an equity
title rejected for want of a period line, and 119 (24.4%) show the full WWE
pattern.

A **soft header** is a title that fails the period gate but carries the
surrounding page furniture — a company name just above, a scale just below, or
a page/contents marker above, two of the three. It is never shipped as a block.
It does two things only: a gap containing one does not end the run, and a block
above one ends there.

Two things that look like simplifications and are not:

- **Just widening `RUN_GAP`** was tried first. Runs merged, nothing new cut the
  blocks apart, and ten filings fell off tight blocks onto 6,000-token windows
  for no gain in coverage. The joining half alone is worse than neither half.
- **Requiring figures under a soft header** was in the first build and is why
  it did not fix WWE. An equity statement is a *wide* table, so its header band
  runs forty lines of one word each — Class A, Paid-in, Comprehensive, Shares,
  Amount, Capital, Deficit, Total — before the first number. It fails the
  figures test for the same reason it fails the period test. The furniture test
  carries it alone; Kibush's notes prose ("balance sheet dates. items in the
  income statement and cash flow statement") has no furniture and stays out.

Measured on the holdout: coverage unchanged at 483, **tight blocks 402 → 411**,
windows 81 → 72. On the tuned sample all three went 426 → 432. It also fixed
one of the five hand-validated cases — **EPIQ Systems**' income statement now
comes out at 245–485 against a hand-verified 257–478, where it had been
reporting a footnote table at 1292–1315 since approach A.

### A filing is never shipped short

Every 10-K and 10-Q contains all three statements, so a filing missing one here
is a locator failure and dropping it loses the observation for nothing but
cost. Priced over the population, sending more text for the filings the headers
do not cover adds about 9% to the job — not a reason to lose 13.6% of the
sample (Matt, 2026-11-18).

So where the headers do not yield all three, the filing ships as a **window**:
the tightest stretch of the filing holding a title of all three kinds, which is
what a statements section is. Centring that window on the cash flow anchor was
tried first and rescued only 52 of 70 — the anchor is exactly what is
unreliable on the filings that reach this fallback, so using it to place the
rescue reproduces the failure it is rescuing. Title spans took it to 66 of 70
*and* made the windows smaller, a median 924 lines against 2,650.

This also moves the hard part to where it is done better. Choosing which block
is the real income statement is what the regex keeps getting wrong; given a
wide window the model reads the statements directly.

A comprehensive-income statement counts as the income statement **only when the
filing prints no separate one** — Payoneer Q2 2021 prints just "Condensed
Consolidated Statements of Comprehensive Loss", while International Bancshares
prints both and conflating them is what made B pick the wrong one.

### Measured, 2026-11-19

Denominator is the full 500-filing sample in each column, **not** the filings
the locator returned something for. A filing that came back empty is a miss.

| | tuned sample | **fresh holdout** |
|---|---|---|
| covered: all three cut, or in a window | 490 (98.0%) | **483 (96.6%)** |
| excluding asset-backed trusts | — | **483 of 488 (99.0%)** |
| all three cut as tight blocks | 432 (86.4%) | **411 (82.2%)** |
| shipped as a window instead | 58 | 72 |
| genuine misses on ordinary operating companies | — | **3 of 500** |

**The holdout is the number to trust.** Every fix above was derived from a
filing inside the original 500, and six of the eleven cases sit in the half
that was meant to be held out, so that sample's figure is fitted. `001i` draws
a fresh 500 excluding the original one; see "Running a second sample" below.

**Coverage is no longer the thing header work moves.** It has come back exactly
483 under four different variants of the header logic, because the window
backstop absorbs whatever the headers fail to cut. Further tuning changes
*which* filings get a tight block, and therefore cost — not how many filings
are usable.

The three real misses are Churchill Downs Q1 2007 (no income statement title
anywhere), Provident Bankshares Q1 2002 (no balance sheet title) and one other.

**What this does NOT establish.** It says the statements are present in what we
send. It says nothing about whether the right one is extracted — that choice is
now deliberately the model's, and it is exactly where the hand audit found
errors before (3 of 10 wrong under B). No extraction has ever run.

### Running a second sample without destroying the first

`001b`, `001c`, `001d`, `001e` and `001f` all take **`PILOT`**, an environment
variable naming the cache directory under `SEC_DATA_PATH`; it defaults to
`pilot-2026-11`. Without it a second run overwrites `dev_d.rds` and
`loc_d.rds`, which are the only record of which filings every number in this
file refers to and cannot be regenerated.

```sh
Rscript 001i-draw-holdout-sample.R                 # writes holdout-2026-11/
export PILOT=holdout-2026-11
Rscript 001b-scan-statement-markers.R              # remote, ~25 min
Rscript 001c-map-numeric-lines.R                   # remote
Rscript 001d-locate-anchored.R                     # the cash flow anchors
Rscript 001e-scan-page-headers.R                   # remote, ~26 min
Rscript 001f-locate-by-page-headers.R
```

`001d` skips its comparison with `legacy/build-rank-candidates` when there is no `pick9.rds`, so a
fresh sample does not need `legacy/build-rank-candidates` run first.

**`001e` scans the whole sample, not `001d`'s picks.** It used to read
`pick_anch.rds`, which meant C only ever saw filings where B had already found
a cash flow statement — 22 of the holdout's 500 were never scanned at all and
could not be located by any means. C does not need B any more: window placement
uses title spans and run selection falls back to counting distinct kinds.

---

## Known remaining defects

- ~~**Two files are called `011f`.**~~ **FIXED 2026-11-19** by the renumbering:
  the review exporter is now `001j-export-for-review.pl` (validation, so it
  sorts last) and approach B is `001d-locate-anchored.R`. Working notes written
  before that date use the old `010`/`011`/`012` numbers; the mapping is in
  *Scripts in this folder*.

- Income statement is still weakest on every measure: located for 82.8% of
  filings against 91.2% cash flow and 91.4% balance sheet, and complete on
  86.4% of chosen blocks against 94.8% and 97.6%. The bank and insurer patterns
  (interest income, premiums) lifted it from 73.1%, so the residual is probably
  more of the same — label vocabulary, not a design fault.
- 5.2% of chosen cash flow blocks still fail the structural test, down from an
  apparent 15.7% but against a much stricter test. `miss` on
  `chunks8.rds` names the absent landmark per span, and
  `diagnostics/check-truncation.R` established the old version of this was
  **not** truncation — pushing the end out 300 lines changed nothing.
- 10-Q cleaned text has ~2× the line count of a 10-K at half the bytes per line
  (24 vs 45), because of the one-value-per-line layout. Every threshold here is
  in **lines**, so each is effectively half as wide on those filings. Scaling by
  bytes-per-line is the obvious fix and was never done.
- One filing (Huntmountain Resources, 418 lines) has no statement text in the
  cleaned file at all. Possibly a partial clean-filing record.
- **Still dangerous, now partly handled.** Schedule I parent-only condensed
  statements, co-registrant statements in combined filings, restatement
  reconciliations in the notes, and press-release exhibits all pass every
  structural test, because they *are* complete statements. Picking the
  parent-only set gives wrong numbers that look right and will even foot.
  - *Restatement reconciliations:* now detected by their column captions
    ("As Reported / Adjustments / As Adjusted"). See the method section.
  - *Co-registrant filings:* the adjacency rule picks one registrant's set
    rather than mixing them, which is a real improvement. But **which**
    registrant is unresolved. Evergy's 10-Q holds five cash flow statements and
    only some titles name the registrant ("— Evergy Metro, Inc"), so the block
    is a whole statement of *some* registrant in the family. For a filing with
    one CIK that is a silent wrong-entity risk, and it is unmeasured.
  - *Schedule I and press-release exhibits:* untouched.
- **Nothing reconciles the three statements against each other yet.** Net income
  appears on both the income statement and the top of the cash flow statement,
  and cash appears on both the balance sheet and the foot of the cash flow
  statement. Those two cross-checks would catch a wrong-entity or wrong-period
  pick for free, and they need no API calls. See the footing section.
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
   silently mis-assigned. `legacy/extract-statements` records this as `n_bad_rows`.
2. **Period assignment** — parse "Nine Months Ended September 30, 2022".
3. **Column selection** — keep year-to-date, drop quarter-to-date, by label.

### A row with fewer numbers than columns, and why the model must not place it

The count check above has a legitimate exception, and it is the sharpest case
of "the model must not decide". In the WRDS text conversion every cell sits on
its own line and a **blank cell prints nothing at all**, so a row with a figure
in only one of several columns arrives as a label and one number, with nothing
on the page saying which column it belongs to. Dover Downs' Q2 2006 cash flow
statement has four such rows.

A model asked to place them will add the section up and work out where they
must go. That is exactly the wrong division of labour: it is arithmetic on
numbers that may have been misread, performed where we cannot check it.

So the contract, added 2026-11-17: the model **fills from the left, pads with
null, and sets `u` true** on that row. Placement happens in `001h` by footing,
against the printed subtotal, and only where **exactly one** arrangement foots
every column. A tie leaves the row flagged, because a guess that cannot be
checked is worse than a flag that can.

- **Cash flow and balance sheet** work identically — the components sum to the
  subtotal and the printed sign is the arithmetic sign.
- **The income statement also needs a sign per line.** Expenses print positive
  and are subtracted, so the components *net* to the subtotal and the page does
  not say which lines add. Signs are shared across columns, so the columns with
  nothing missing pin them: Biomet's FY2010 statement has four columns, two of
  them clean, and those two determine the signs outright. The convention is
  tried first (components subtract under a running subtotal, add under a
  "total X"); enumeration is only a fallback, which is why `MAX_COMB` rarely
  binds.
- **A section with no clean column still resolves** when only one sign vector
  is possible. Dover Downs' financing section is the case — both its columns
  await a value — and bailing out there took the cash flow statement from 100%
  resolved to 0%.

Verified on six cases: Dover Downs (2 columns, 4 ambiguous rows, 8/8 placed
correctly), a three-column cash flow where one row prints two numbers of three
and neither row's truth is column 1 (6/6), Biomet's real income statement (4
columns, signs unknown, 8/8), a synthetic balance sheet, a mixed-sign income
statement block where interest expense subtracts while interest income adds so
the convention foots nothing and the sign search has to run (3/3), and a
genuine tie, which correctly placed nothing.

New frame columns: `col_unplaced`, `col_placed_by_footing`. `amount` is
recomputed after any move.

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
  assumes no thinking at all. `legacy/extract-statements` sends `output_config: {"effort": "low"}`
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

**Now measured, not projected — see *Storage, measured rather than guessed* at
the top: 18.6 GB raw, ~6 GB compressed for 780,364 filings.** This section's
reasoning still holds and is kept for the detail.


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

**SUPERSEDED, 2026-11-19.** The inventory plus per-person append-only logs
(`001a` / `000`) does this job, in parquet and CSV that everything else already
reads, so the separate SQLite ledger below was not built. Two parts of the
design below still apply and were carried over: status must be recorded per
filing before extraction scales, not retro-fitted, and a row needs to record
*which* version of the prompt and locator produced it. The rest is kept for
the reasoning.


Everything below is the original design;
`001-ledger-build-ledger.R` is the script that would have done it.

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
  scan_ver    TEXT,               -- rules version of 001b
  scanned_at  TEXT
);

CREATE TABLE statements (         -- one row per filing x statement
  wrdsfname   TEXT, kind TEXT,    -- kind: cf | is | bs
  start       INTEGER, end INTEGER, len INTEGER,
  titled      INTEGER, nreq INTEGER, dens REAL,
  locate_status TEXT,             -- pending | located | not_found | ambiguous
  locate_ver  TEXT,               -- rules version of 001c
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
2. 001b marker scan     WHERE scan_status = 'pending'
3. 001c selection       -> statements rows, locate_status located / not_found
4. legacy/export-chunk-text-approach-a export text     WHERE locate_status='located' AND ext_status='pending'
                           -> also sets text_sha
5. legacy/extract-statements submit          same filter; writes batch_id, ext_status='submitted'
6. legacy/extract-statements fetch           WHERE ext_status='submitted'
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

**One step, lettered in run order.** This folder is step `001` of the project —
the first major stage — and the letters say what to run and when. Renumbered
2026-11-19 from the old `010`/`011`/`012` scheme, which had grown two files
called `011f` and gave letters to superseded approaches.

`000-` is sourced, not run in sequence, matching `Code/000-utilities-functions.R`
in the parent repo. Superseded approaches live in `legacy/` with their numbers
dropped, again matching `Code/legacy/`. Anything that is not part of the chain
has no number and lives in `diagnostics/`.

A bare `001x` in this file always means a script **in this folder**. The XBRL
pipeline in `Code/` also numbers its scripts `001a`, `001b`, `001c`, so
references to that track are always written with the `Code/` prefix.

**Run every one of these from the repo root**, so `.Renviron` loads.

| script | runs | does |
|---|---|---|
| `000-inventory-helpers.R` | sourced | `inv()`, `inv_todo()`, `inv_log()`, `inv_progress()` — claim a batch and record it without two people writing the same file |
| `001a-build-filing-inventory.R` | on demand | every annual/quarterly report since 2002 from `wrdssec.wrds_forms` + `wrds_forms_reg`; screens as columns; assigns filings by CIK; derives status from the per-person logs → `inventory/filing_inventory.parquet` |
| `001b-scan-statement-markers.R` | yes | one remote grep per filing: every title, section heading, subtotal, roll-forward, scale and restatement-caption line, with line numbers → `markers4_raw.rds` |
| `001c-map-numeric-lines.R` | yes | line numbers of every numeric line per filing → `numlines.rds` |
| `001d-locate-anchored.R` | yes | **approach B.** Finds the cash flow statement standalone, brackets a section around it, locates the other two inside with the requirements relaxed → `chunks_anch.rds`, `pick_anch.rds`. Kept because it is still the source of the cash flow **anchor** that `001f` ranks runs against |
| `001e-scan-page-headers.R` | yes | second remote grep, on **joined line pairs** so a title split mid-phrase matches whole: titles, period headers, column dates, scale, company names, contents markers → `markers_hdr.rds` |
| `001f-locate-by-page-headers.R` | yes | **approach C, the one in use.** Cuts each statement from its page header to the next, falls back to a title-span window so no filing ships short → `chunks_hdr.rds` |
| `001g-export-chunk-text.R` | yes | pulls the located text off WRDS and ships every passing candidate plus rank 1, both company names attached, so the model chooses rather than the regex. `IN_CHUNKS` selects `chunks_anch.rds` or `chunks_hdr.rds` |
| `001h-build-tidy-frame.R` | yes, on test data | one row per filing × statement × line × period, amounts in **actual dollars** → `scf_text_lines.parquet` + the footing check |
| `001i-draw-holdout-sample.R` | on demand | draws a fresh validation sample excluding the original one, into its own `PILOT` cache directory |
| `001j-export-for-review.pl` | on demand | **validation.** Writes the located text as human-readable `.txt` with an index, one file per filing × statement |

Not written yet, in the order they are needed:

| | |
|---|---|
| wiring `001g` to the inventory | read `inv_todo("collect")`, write chunk text to parquet by year, call `inv_log()`. The next build |
| the step-3 extraction | done **in harness**, in batches. No script; the prompt files below are the reference |
| the comparison | mostly `Code/001a-extract-scf-lines-and-changes.R` already — map `001h`'s output onto its `scf_lines_parts/` shape |
| a validation substep for `001h` | hand-check the tidy frame against filings |

### `legacy/` — superseded, kept for reference, not run

| script | was | why it is here |
|---|---|---|
| `legacy/draw-filing-sample.R` | `011a` | drew the original 500-filing sample and probed heading coverage. **Saved nothing** — see the orphan warning below. The inventory replaces it |
| `legacy/build-rank-candidates.R` | `011d` | **approach A.** Brackets each statement top-to-bottom over the whole filing, requires its full structure, picks the adjacent triple → `chunks8.rds`, `pick9.rds`. Replaced by approach C |
| `legacy/export-chunk-text-approach-a.R` | `011e` | exported approach A's chosen text → `extract/chunks.jsonl`. Replaced by `001g` |
| `legacy/extract-statements.py` | `012a` | the paid Batch API runner: `estimate` / `submit` / `status` / `fetch`. **Never run, and the route was dropped** — extraction is done in harness (Matt, 2026-11-19). Kept because its cost estimator and its handling of the output schema are the reference if that ever changes |

### Dependencies worth knowing

`001b` and `001c` are independent of each other and both feed the locators.
**`001c` is not optional** — the figures requirement and the contents-list test
both read `numlines.rds`.

Changing a pattern in `001b` or `001e` means re-running it before a locator
will see the change. Measured on the 500-filing holdout, 2026-11-19: **39s for
`001b`, 47s for `001e`.** `001b` refuses to save if it finds markers for fewer
than half the filings it asked about, because it used to overwrite a good cache
with an empty one when a pattern broke.

### Why the scans are fast, and the one rule that must stay out of `PAT`

Both scans used to take ~3.4 s/filing, which is 24 days for the 780,364-filing
inventory and was the binding constraint on collecting at all. Two fixes,
measured 2026-11-19, took that to ~0.07 s/filing — a combined **49x** for
`001e` and **57x** for `001b` — with the marker files **byte-identical** and the
located line ranges **byte-identical** on the holdout (2,549 blocks, 488
filings, all three statements in 411).

1. **`LC_ALL=C` on every remote grep.** The filings are plain ASCII, but in the
   login shell's UTF-8 locale `grep -i` case-folds per character as multibyte.
   Worth 4.5x for nothing. Verified identical on 121,181 marker rows.
2. **The company-name rule runs as a separate grep over short lines only.**
   Inside `PAT` that single branch cost **93% of the whole scan** — `grep` tests
   its bounded run (`{3,70}`, `{3,60}`) anchored at both ends against every
   line including the 170-600 character wrapped prose paragraphs, which forces
   the entire pattern off `grep`'s fast matcher. Removing it alone took `001e`
   from 3.50 to 0.055 s/filing; de-bounding the *other* caps (`{0,40}`) saved
   nothing measurable, so this one branch is the entire cost.

**Do not fold `NAME_PAT` back into `PAT`, and do not "simplify" it by dropping
the cap.** The character cap IS the semantics: it is what separates
`THE ALLIED DEFENSE GROUP, INC.` from a prose sentence ending `...on the
financial position of Alcoa`. De-bounding it was tried and measured at **980
extra marker rows per 100 filings.** The rule is therefore unchanged; only the
stream it sees is narrowed, by an `awk` guard on the space-and-tab-trimmed line
length. That guard is **provably** lossless rather than merely tested: the
pattern can match at most 70 + 11 (`corporation`) + 1 (`[.,]`) = 82 characters
of non-space content, so the 90-character cap is strictly more generous than
the rule itself.

Two things that are NOT the bottleneck, so do not optimise them: reading the
files (measured 0.004 s/filing — 55 MB off `/wrds/sec` in 0.4s) and shipping
the markers back (identical timing with the output discarded remotely). An
earlier plan to merge the three scans into one pass was abandoned for exactly
this reason — re-reading a filing is free, so merging saves almost nothing.

Because the name rule is a second `grep`, a line satisfying both patterns comes
back twice, so both scripts now dedupe on `(wrdsfname, line, text)` after
parsing. `001f` re-sorts by line everywhere it matters and builds its flags
with `any()`, so a duplicate would have been harmless there, but the dedupe
keeps the marker files identical to the old single-grep output.

Supporting files, not steps:

| file | does |
|---|---|
| `prompt/system-prompt.txt` | the instructions sent to the model |
| `prompt/output-schema.json` | the schema the response is constrained to |
| `prompt/DESIGN.md` | why the prompt is shaped this way — **read before changing either of the above.** Written before the Nov 2026 prompt revision, so it predates the column-object format, the per-row `x` flag and the precision-is-not-a-multiplier rule |
| `requirements.txt` | `pip install -U anthropic`. Only the dropped Python route needs it |

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
| `dev_d.rds`, `loc_d.rds` | `001b` | the 500-filing sample, dev and held-out |
| `markers3.rds` | `001c` | an earlier marker pass |

They were written in interactive sessions that no longer exist. `legacy/draw-filing-sample` draws a
sample but saves nothing, so re-running it does not regenerate them, and
`set.seed(42)` does not make the draw reproducible because the surrounding query
has changed. **Do not delete these three files.** Rebuilding the chain from
nothing means first writing the two steps that produce them.

`001c-map-numeric-lines.R` is the most reusable piece: it returns the position of
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
| `chunktext.rds` | the pulled chunk text, written by `legacy/export-chunk-text-approach-a` (not yet run) |
| `scored3.rds` | Compustat anchor scores |
| `gaps.rds`, `fails2.rds` | coverage failures with diagnosed causes |
| `headings.rds`, `neigh.rds`, `anchor2.rds` | superseded adjacency / anchor approaches |

---

## Environment gotchas that cost time

- **`shQuote()` on Windows quotes for cmd.exe, not for the remote shell.** Its
  default is `type = "cmd"`, which wraps the string in **double** quotes — and
  everything in this folder ships that string to a POSIX shell on WRDS Cloud.
  A `$(` sequence anywhere in a grep pattern then becomes command substitution
  and the remote shell executes part of the regex. Always pass
  `shQuote(x, type = "sh")`.
  The failure is silent and total: grep matched nothing for all 500 filings,
  `001b` wrote an empty marker file over the good one, and it surfaced two
  scripts later as "0 rows" with no error to trace. It had worked for months
  only because no pattern had ever contained `$(`. Adding one character class
  (`[0-9$(]`) was enough to break it. `001b` now refuses to save a collapsed
  result, and orders that class `[0-9($]` as well.
- **`scp_upload` fails against `~`** on WRDS Cloud ("No such file or directory"
  — the tilde is not expanded). Stream input lists through a quoted heredoc to
  `/tmp/` instead. Every script here does that.
- **A single SSH exec over more than a few hundred filings gets disconnected.**
  Batch and reconnect per batch, with a retry (`diagnostics/score-chunks.R`, `001c`, `diagnostics/measure-token-size.R`).
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

## Roadmap

Rewritten 2026-11-19, after step 1 was built and the API route was dropped.
In order of value.

1. **Wire step 2 to the inventory.** `001g` currently takes a sample frame; it
   should take `inv_todo("collect")`, write the chunk text to parquet
   partitioned by year, and call `inv_log()`. That is the only thing standing
   between the inventory and collection running at scale. Nothing else here is
   blocked on money or on an API key.
2. **A definition of revision vs reclassification.** Free to work on now, and
   the item most likely to be expensive if deferred — see *The research-design
   question nobody has addressed*. The XBRL track's `Code/001b` already carries a
   working version of this (five standards, windows set from observed spikes,
   plus a rule that a disclosed amendment always beats attribution), so this is
   adaptation rather than invention.
3. **Step 5, the comparison.** Map step 4's parquet onto the shape of
   `Code/001a`'s `scf_lines_parts/` and run that script's anchoring logic
   against it. Reuses the rules that each cost a counter-example to find.
4. **In-harness extraction at scale**, in batches, logged. Run once on 10
   filings: 1,890 rows, 15 of 18 cash flow period-columns footed, no failures.
   The next question is whether accuracy holds on filings nobody hand-checked
   first.
5. **Cross-filing line-item matching** — through relabelling, splits and
   merges. Harder than the extraction, and where line order (`n`) earns its
   cost. Note the other track's `plabel` work is the same problem.
6. **The co-registrant problem.** A co-filed submission holds two registrants'
   statements in one chunk (`n_registrant_rows > 1`, 12,963 accessions). Which
   set belongs to which CIK is unresolved, and it is the International
   Bancshares / United Airlines failure in `001f`.
7. **Restatement reconciliation tables** as a free self-labelling validation
   set — ~6% of blocks contain "previously reported" / "as restated".
8. **The Audit Analytics baseline** — counts by year, SCF-affecting, Big R vs
   little r. The denominator the paper's claim is measured against. Much of
   this now exists in the other track's `002a`/`002b`.
9. Fix the 10-Q line-vs-bytes threshold problem.
10. Implement auditor-report proximity as a filter.

**Dropped:** the paid Batch API pilot. Extraction is done in harness (Matt,
2026-11-19). `legacy/extract-statements.py` and the cost projections are kept for
reference. Also dropped: the separate `001-ledger-build-ledger.R` SQLite design — the
inventory plus per-person logs does that job, in the format everything else
already reads.

## Working notes

The author's stated preferences, learned the hard way during this session:
increment in small steps and report briefly; do not produce long write-ups; do
not start work without being asked; and do not re-explain decisions already
made.
