# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

This is an academic accounting/finance research project (R), studying how SEC filers correct/restate the Statement of Cash Flows (SCF) and related financial statement line items (Net Income, Stockholders' Equity, Assets, Liabilities) across 10-K/10-Q filings, revisions, and amendments (10-K/A, 10-Q/A). It builds firm-period panels from SEC XBRL data and cross-references them against Audit Analytics restatement/adjustment data.

There is no application code, package, or test suite — this is a linear sequence of R data-processing scripts run interactively in RStudio (`SCF-Audit.Rproj`).

### Research question / hypothesis

The core question: do auditors and preparers scrutinize the SCF with the same rigor as the balance sheet and income statement?

The tension motivating the project: classification errors are conventionally treated as immaterial, and that's often defensible for the balance sheet or income statement, where classification doesn't change bottom-line totals. But for the SCF, classification *is* the statement's entire value proposition — moving an item between operating/investing/financing changes nothing about net income or total assets, yet it's the exact distinction users of the SCF rely on. A materiality framework built for the other two statements may therefore be the wrong lens for the SCF.

The hypothesis: there are many undisclosed SCF reclassifications, and where issues are disclosed, they're often characterized as immaterial even when they are numerically material by conventional thresholds.

This is why `001a` tracks the **disclosed** channel (10-K/A, 10-Q/A amendments) separately from the **silent** channel (a figure quietly changing in a later filing's comparative column), and why it works at the individual line-item level: a reclassification recharacterizes *where* a cash flow appears without moving the reported total, so a standard "did net income change" restatement screen misses it entirely. Akorn's Q2/Q3 2014 10-Q/A is the pattern in miniature — a formally disclosed amendment that restated several operating line items while leaving CFO exactly unchanged.

### The headline descriptive claim

The punchiest available result is at the **statement level**: what proportion of cash flow statements carry a revision that was never disclosed — the figures changed in a later filing's comparative column with no amendment, so the event never enters Audit Analytics at all. That is direct evidence of how prevalent undisclosed SCF revision is, and it is what the `correction_channel` silent-vs-disclosed split exists to support.

Note this claim is **robust to the tag-versus-line-item caveat below**, because it is binary per statement — did this SCF change, and was it disclosed? It does not require knowing how many individual line items moved.

### Measurement caveat: tags proxy for line items (OPEN — deliberately not settled)

`001a` observes changes in XBRL **tags**, which only proxy for changes in **line items**. A filer can retag the same line with a different element and nothing economic changes — `IncreaseDecreaseInDeferredRevenue` → `IncreaseDecreaseInContractWithCustomerLiability` under ASC 606, or `RepaymentsOfLongTermCapitalLeaseObligations` → `FinanceLeasePrincipalPayments` under ASC 842. The slippage runs one way: **tag-level counts overstate line-item changes.**

Scope it correctly rather than caveating everything. It affects descriptive *tag-level* counts ("N line items were revised"); it does **not** affect the section-level CFO/CFI/CFF measures that carry the actual tests, because a rename inside a section nets to zero and leaves the subtotal untouched — which the reconciliation check verifies directly. Section-level outcomes are therefore primary; tag-level detail is diagnostic (identifying *why* a section moved, so ASU-driven causes can be excluded).

**Left open on purpose.** Three routes to tightening it are preserved and need no re-extraction:
- `is_tag_substitution` already flags equal-value renames, so any tag-level count can be reported with and without them — a one-line robustness check.
- `plabel` (the filer's own line label) is carried through the extract; label similarity would separate a relabel from a genuine presentation change.
- `GAAP_Taxonomy.xlsx` (the `Elements` and `References` sheets, never yet opened) and the per-period `tag.tsv` files may carry FASB deprecation/replacement data. If so, renames of standard elements become an authoritative lookup rather than a fuzzy match, leaving only company-specific extension tags to heuristics.

## Commands

- Open `SCF-Audit.Rproj` in RStudio and run scripts by sourcing them or executing line-by-line — there is no CLI build/lint/test entrypoint.
- Required env vars, set in a local (gitignored) `.Renviron` at the repo root:
  - `DATA_PATH` — processed data folder (parquet outputs, taxonomy/restatement inputs)
  - `XBRL_DATA_PATH` — raw SEC XBRL Financial Statement Data Sets folder (quarterly/monthly `_num.tsv` / `_sub.tsv`)
  - These are machine-specific (differ between personal/work computers) and never committed.
- `Code/000-utilities-functions.R` is sourced by downstream scripts via `source("code/000-utilities-functions.R")` — run scripts from the repo root so this relative path resolves.
- No automated tests exist. Scripts self-validate with ad hoc `filter()`/`View()` checks against known CIKs, left in place at the bottom of each script under headers like `# Validation` or `# SCRATCH`. These are exploratory, not meant to run unattended as part of the pipeline.

## Architecture

### Data pipeline lineage

Scripts are numbered by research goal: `001x` builds the SCF revision dataset from XBRL, `002x` links it to Audit Analytics. Letters run in execution order within a goal. `Code/legacy/` holds superseded approaches, kept for reference and not part of any run.

**Live chain — run in this order:**

| script | does | runtime |
|---|---|---|
| `000-utilities-functions.R` | sourced by all; R options and helper functions | — |
| `001a-extract-scf-lines-and-changes.R` | extracts every SCF line from the raw SEC datasets and detects what changed between as-filed and latest presentation | ~33 min (checkpointed, resumable) |
| `001b-flag-standard-driven-changes.R` | attributes changes to the standards that required them and subtracts them, leaving an unexplained residual per section | ~2 min |
| `001c-build-filing-audit-summary.R` | collapses to one row per filing-period and writes the hand-audit samples | ~1 min |
| `002a-build-audit-analytics-sample.R` | merges Audit Analytics restatement/adjustment data | — |

Only `001a` touches the raw 6.2 GB of SEC files; everything after it reads parquet, so `001b`/`001c` can be re-run freely after any logic change.

**`002a` is not yet wired to this chain.** It still reads `xbrl_all_tsv.parquet` from the superseded `legacy/extract-all-tags-from-XBRL-tsv.R`, not `001b`'s output. Repointing it at `scf_period_adjusted_full.parquet` — and revisiting the join, since the grain and column names differ — is the next substantive task.

**`Code/legacy/`** (not run; numeric prefixes dropped because the old numbering no longer applies):
- `extract-all-tags-from-XBRL-tsv.R` (was `001c`) — the predecessor extractor; still generates the `xbrl_all_tsv.parquet` that `002a` currently consumes.
- `process-GAAP-taxonomy.R` (was `001d`) — selected SCF tags by filtering `GAAP_Taxonomy.xlsx` presentation roles, covering the standard, deposit-based, insurance-based, securities-based, REIT and direct-method SCF variants. Superseded by `001a`, which reads the SEC `pre` file instead; kept as a fallback.

Removed in the reorganisation and recoverable from git history: the JSON/`companyfacts.zip` lineage (`001-extract-CF-tags-from-XBRL-json.R`), a stale copy of the TSV extractor, and `002 Create FS from XBRL data.R` (early exploration of the raw file structure).

### Phase 1 detection design (`001a`)

Detects changes between the SCF **as originally filed** and the SCF **as later re-presented as a prior-period comparative**, at the individual line-item level. Key design decisions, each of which fixes a specific failure mode found while validating against Walmart:

- **SCF membership comes from `pre` (`stmt == "CF"`), not the GAAP taxonomy.** Taxonomy-role filtering (what `legacy/process-GAAP-taxonomy.R` does) silently drops tags a filer borrowed from another statement's taxonomy section. This is the source of the `"missing?"` entries in `Examples.xlsx` — both resolve to standard `us-gaap` tags (`RepaymentsOfLongTermCapitalLeaseObligations`, `OtherNoncashIncomeExpense`) that `legacy/process-GAAP-taxonomy.R` misses.
- **Section assignment** is derived per filing from presentation order: a subtotal closes its own section, so each line takes the first subtotal at or after it. **No canonical operating → investing → financing order is assumed**, and whatever anchors exist are used. An earlier version demanded all three subtotals in strict sequence and set `section = NA` for every line of any filing that failed — discarding 37,465 filings (11.05%) for three unrelated reasons: a subtotal merely absent (25,481 tag operating + financing but no investing), financing legitimately presented *before* investing (2,083), and 867 that satisfied the order yet were rejected because `max(line)` picked up a twice-presented subtotal. What a missing anchor really costs is certainty about the boundary it would have drawn, so those filings are now assigned **and** flagged `section_ambiguous` (34,277 filings, 10.1%) rather than silently dropped. `section = NA` no longer occurs.
- **Anchored on three specific filings**, not aggregated across comparatives:
  - `orig_*` — earliest regular 10-K/10-Q presenting the period as its own current period ("as filed").
  - `comp_*` — the **last** regular filing to re-present it as a prior-period comparative. The **silent** channel: the number changes with no amendment and often no disclosure. Last is used per the project convention that it is the final attestation of correct presentation.
  - `amend_*` — the last 10-K/A or 10-Q/A restating the period as its own current period. The **disclosed** channel. Amendments restate their *own* period, so the restated figures sit in the /A filing's current-period column, not a comparative column.

  A period is kept if it has **either** channel, so amendment-only periods are not lost. Each channel's difference is computed on its own pairwise-normalised basis; channel columns are `NA` where that channel does not exist, which distinguishes "no amendment was filed" from "an amendment changed nothing". `correction_channel` on the period summary classifies each period as Both / Amendment (disclosed) / Comparative revision (silent) / None.
- **Sign convention — resolved per line, not per filing.** Filers churn the stored sign and the `negating` display flag *in opposite directions*, so neither stored nor displayed values work alone:
  - Walmart Q3 FY2019 stores `PaymentsToAcquirePropertyPlantAndEquipment` as 7,014 with `negating = 0` (displaying an outflow as positive); the next year keeps the identical stored 7,014 and corrects the flag to 1. Same fact, different display — comparing *displayed* values invents a 14,028 swing.
  - Akorn stores `NoncashGainOnBargainPurchase` as −849 with `negating = 0`; the later filing stores +849 with `negating = 1`. Both display −849 — comparing *stored* values invents a 1,698 swing.

  Rule: if the stored value is identical the fact did not change, so adopt the later filing's presentation for both sides; otherwise use each filing's own displayed value. Genuine sign corrections still surface (Akorn's Q3 2011 10-Q tags `IncreaseDecreaseInAccruedLiabilities` as −1,024 where its own subtotal requires +1,024 — the section misfoots by exactly 2 × 1,024 — and the later filing corrects it).
- **Restrict each filing to the periods its CF statement actually presents.** `pre` maps a tag to a statement but carries no period, so joining `pre` to `num` on `(adsh, tag)` alone attaches every duration of that tag to the SCF. A Q3 10-Q reports `ProfitLoss` at `qtrs = 1` for the income statement's three-month column while its SCF is nine-month — without this filter that stray row makes the Q3 filing look like it re-presents the prior Q1 cash flow statement. The `(ddate, qtrs)` pairs at which section subtotals appear define the real SCF periods.
- **Income build-up collapse — conditional, decided by the filing's own arithmetic.** The top of the operating section is *sometimes* a nested build-up (net income → discontinued ops → income from continuing operations) where only the last line feeds the subtotal; collapsing it to one `[Income starting point]` line prevents double counting and stops a filer simplifying the build-up from manufacturing a false change (Walmart FY2016). But it is **not always nested**: Akorn presents net income *and* a discontinued-ops adjustment as two genuine parallel components with no "continuing operations" line, and its section foots only when both are counted — collapsing unconditionally discarded net income and blew a 21,461 hole in the reconciliation. So the collapse is applied only where a section does *not* foot with every line counted but *does* foot once collapsed, and is applied to both sides together if either needs it.
- **Zero-value adds/drops are not changes** — `difference` is tested before presence.
- **Tag substitution** (a line relabelled to a different element at the same amount) is flagged, not counted as an economic change.
- The GAAP taxonomy **Calculation linkbase was evaluated and rejected** for identifying aggregates: it describes the standard hierarchy, not the filer's, and mislabels ordinary leaf lines (e.g. `IncreaseDecreaseInAccruedLiabilities`) as aggregates.

**Reconciliation is the coverage test**, per the project's own logic: within a section, component differences must sum to the subtotal difference. It is only a fair test when both filings' own statements foot **and** the section boundaries are unambiguous — `recon_testable = both_sides_foot & !section_ambiguous` is that gate. Note `section_foots()` drops rows whose section is `other`/`NA`, so a side contributing no rows for a section yields no footing row at all; unknown footing must therefore default to **FALSE**. Defaulting it to TRUE (as an earlier version did) declared sections fair tests when footing was never established and inflated the pass denominator — the visible symptom was sections whose subtotal moved while not one component changed (Ryder FY2018 investing: the original supplied components via the comparative's section label but no subtotal of its own, so the subtotal read as a $2.8bn "Added" from zero). Where they foot and reconciliation still fails, the script is missing a line; where they don't, the filing's own XBRL is defective — itself a finding. A confirmed example: Walmart's Q2 FY2019 10-Q omits `ProceedsFromIssuanceOfLongTermDebt` (15,851M) from its tagged SCF entirely — the value exists in `num` but `pre` assigns it to a note, not `stmt == "CF"`, and the statement jumps from short-term debt straight to repayments.

**Validation status (Walmart + Akorn, 86 company-periods, 258 sections):**
- Reproduces every line item and amount in `Examples.xlsx` for Walmart FY2012–FY2016, all 15 section-periods reconciling exactly.
- **Where both filings' own statements foot, reconciliation succeeds 239/239 (100%).** All 8 failures are confined to sections where at least one filing's own SCF does not foot.
- Akorn FY2014 (a known restatement, Audit Analytics `restatement_key` 48887) is detected at tag level: `ExcessTaxBenefitFromShareBasedCompensationOperatingActivities` −38,710 → −29,517 against `...FinancingActivities` +38,710 → +29,517 — i.e. the script names the share-based-comp excess tax benefit line as the driver, which is exactly the discrimination Phase 2 needs and which section subtotals alone cannot provide.

**Amendment-channel checks (the two channels behave as theory predicts):**
- Akorn Q1 2012: the 10-Q/A restated CFO 6,571 → 3,049 and CFI −64,046 → −60,610, and the later comparative carries *exactly* those figures. The disclosed and silent channels agree, which is the expected consistency check.
- Akorn Q2 and Q3 2014: the 10-Q/A restated four and three operating **line items** while leaving CFO **unchanged** (the line changes net to zero) — a disclosed correction that is purely a within-operating reclassification. This is precisely the pattern the project hypothesises: classification treated as immaterial. Note the later comparatives then moved the subtotals *further*, so amendment and comparative disagree for these periods.
- Walmart Q2 FY2014 has a 10-Q/A that did not touch the cash flow statement at all (`n_amend_lines_changed == 0`), so it is correctly classified as a silent comparative revision rather than a disclosed one.

**Outputs** — four grains, each a rollup of the one before. Join keys are `cik` + `start` + `end`, plus `tag` or `section`:

| output | grain | notes |
|---|---|---|
| `scf_lines_parts/` (**directory**) | filing × SCF line × period | every filing's rendition, uncompared. Includes `sic`/`afs` for sample screening. Read with `arrow::open_dataset()`, **not** `read_parquet()` |
| `scf_line_changes.parquet` | company-period × tag | the main deliverable. `orig_val`; `comp_val`/`difference`/`change_type` (silent channel); `amend_val`/`amend_difference`/`amend_change_type` (disclosed channel); plus `is_tag_substitution`, `negating_changed`, `stored_value_same` |
| `scf_section_changes.parquet` | company-period × section | `subtotal_difference` vs `component_difference`, `recon_diff`, `recon_ok`, `both_sides_foot` |
| `scf_period_summary.parquet` | company-period | `cfo/cfi/cff_difference`, `net_difference`, `period_type`, `correction_channel`, `sic`, `afs` — the table to regress on or merge to Audit Analytics |

`difference = orig_val − comp_val` (as-filed minus subsequent, matching `Examples.xlsx`).

Practical path: filter `scf_period_summary` for interesting periods → drop to `scf_line_changes` to see which tags drove it (what Phase 2 needs) → `scf_lines_parts/` to inspect a filing as filed.

### Phase 2 design (`001b-flag-standard-driven-changes.R`)

Attributes changed SCF line items to the standards that required them, sums what each standard accounts for within a section, and subtracts it:

```
unexplained_difference = subtotal_difference - standard_applied
```

Each of the five standards in the `STANDARDS` reference table carries an inline comment block: what it changed, its SCF impact, the nominal effective date, the window observed in our data, and the measured section-level impact. Attribution records **which standard and what kind of impact**, not one boolean, because the standards threaten different outcome variables (see the measured table under Phase 2 in the roadmap). **ASU 2016-15 is deliberately absent** — the detector found no wave in any of its tags, so including it on theory alone would delete genuine revisions.

Three constraints the data forced, each after a failed attempt — do not undo them without re-reading the cited case:

1. **Patterns must match BOTH sides of a rename.** Matching only the new tag leaves the offsetting old-tag change unattributed, so the pair stops netting to zero and subtracting manufactures a residual in a section that never moved. This is why ASU 2016-01 matches `AvailableForSaleSecurities` rather than only the `Debt`/`Equity` variants, and why ASC 606 lists `DeferredRevenue` alongside the contract-liability tags. The cost is breadth, traded against narrow windows.
2. **The adjustment may only explain movement away, never create it.** A plain `subtotal - standard` pushed 12,592 periods from "intra-section reclass" to "net cash flow change" — backwards. The attributed amount is applied only in the direction of the observed movement and capped at its size (`std_applied`). Four sanity checks in the script assert this: zero residuals exceeding the original move, zero sign flips, zero flat sections made to move, and the raw decomposition `standard + other = subtotal` exact on all reconciling sections.
3. **Windows must be tight, and a disclosed correction overrides attribution.** With a window starting FY2014, **Akorn FY2014** (`restatement_key` 48887 — a documented restatement) came back "explained by standard" with a zero residual: a real misstatement erased by a standard not yet issued. Windows are now bounded by the issuance date *and* the three-year comparative reach of a 10-K (an FY2017 adopter can restate FY2015–16, not FY2014), and **any period with a disclosed amendment is never attributed to a standard** — independent evidence of a real error wins. Akorn FY2014 now reads "unexplained (candidate revision)" across all three sections.

**Result on the full population:** of 52,233 testable sections that moved, standards explain all of the movement in ~12% and part of it in ~8%; **42,160 sections remain unexplained candidate revisions.** Financing/investing are ~14.5% explained, operating ~10% — consistent with 606/842 being intra-section renames.

**Known limits.** ASC 842's `"Lease"` pattern is the most over-inclusive entry (3,163 tags) and relies on its narrow window; tighten it first if a residual looks suspiciously small. More generally, tag × window attribution will always produce false positives — Akorn is the proof, and the amendment override only catches cases the filer disclosed. Report headline numbers **both with and without** the ASU exclusion rather than treating the adjusted figure as truth.

**Outputs:** `scf_line_changes_flagged{suffix}.parquet` (line rows + `standard`, `source`, `impact`, `is_standard_driven`, `standard_overridden`, `era`), `scf_section_adjusted{suffix}.parquet` (+ `std_difference`, `std_applied`, `unexplained_difference`, `section_verdict`), `scf_period_adjusted{suffix}.parquet` (+ `cfo/cfi/cff_unexplained`, `period_type_adjusted`, `standards_involved`, `era`). `IN_SUFFIX` at the top selects which `001a` build to read (`"_full"` or `""`).

**Era flags** (on the period output) mark periods that are not clean evidence: 17.6% are `early XBRL phase-in` (FY≤2012), 7.1% `censored right edge` (FY≥2022, where the panel simply lacks later filings to serve as comparatives), leaving 75.3% `clean`. Whether to exclude the early-XBRL era is still an open decision; it is flagged separately from FASB-driven changes on purpose.

### Hand-audit summary (`001c-build-filing-audit-summary.R`)

Collapses the pipeline to **one row per filing-period** (267,282 rows × 61 cols) so a case can be checked against the actual SEC filing by hand. Carries company name, fiscal year/period, both anchor filing dates and accession numbers, **EDGAR index URLs** for the original/comparative/amendment filings, the three section totals as filed and as latest presented, the change, the standard-driven portion, the unexplained residual, per-section changed flags and tag counts, and a readable string of every changed tag with its before/after amounts and standard attribution.

Grain note: one row per filing 99% of the time — 264,477 distinct filings produce 267,282 periods, because 2,769 filings anchor 2–3 periods (typically a 10-K covering both its annual period and a Q4). `n_periods_this_filing` flags those so they are not mistaken for duplicates.

Company name and `fp` do **not** survive `001a` (which carries only `cik`/`sic`/`afs`), so `001c` rebuilds them from the raw `sub` files.

All amounts are RAW DOLLARS as tagged, so they tie to a filing without rescaling; only the `*_changed_tags` strings use $ thousands for readability.

**It also writes two ready-made audit samples** (`audit_sample_changed.csv`, `audit_sample_unchanged.csv`; n = 120 each, fixed seed), drawn only from testable, clean-era periods — a case that fails the pipeline's own quality gates is not a fair test of whether the summary is accurate. Pool: 120,985 periods (15,874 changed / 105,111 unchanged). Population split overall: **83.3% of filing-periods show no section change, 16.7% do**; of the changed ones, 3,337 are fully explained by a standard and 41,174 retain an unexplained residual.

Worked examples from the sample, showing the kind of thing an audit should confirm:
- *Weis Markets FY2016* — `Depreciation` (66,496) and `AmortizationOfIntangibleAssets` (10,366) both go to zero while `DepreciationDepletionAndAmortization` appears at 76,862, i.e. exactly their sum: a 2-into-1 tag consolidation netting to zero. CFO still moves 2,517, correctly traced to a different line (receivables/prepaid).
- *SunCoke Energy Partners Q1 2017* — CFI moves 100, sole changed tag `IncreaseDecreaseInRestrictedCash`, correctly attributed to ASU 2016-18 and leaving no residual.

### Audit Analytics link (`002a-link-audit-analytics.R`)

The XBRL filing panel is the **main table**; Audit Analytics is **left-joined** onto it. That orientation is the point: AA only records a restatement once a filer *discloses* it, so a period where the pipeline finds an unexplained SCF revision and AA has nothing is a **silent revision** — invisible to any study that samples from AA. An earlier AA-anchored version of this script (see git history) could only measure how much of AA we match, and by construction could not see the silent population at all. The AA-anchored view survives as a coverage diagnostic only.

**Join key:** `cik` (10-digit padded) plus the filing's `period_end` falling inside AA's restated/adjustment window. AA windows are firm-level date ranges spanning several reporting periods, so one AA event legitimately matches several filing-periods and a period can match several events; matches are aggregated to one row per filing-period.

**Two independent disclosure channels**, kept separate because a filer can use one without the other: an **amendment** (10-K/A, 10-Q/A) that actually changed the SCF, detected by `001a` from the filings themselves; and an **AA record** covering the period. AA records are further split by `aa_flagged_cash_flow` — AA can log a restatement for, say, revenue recognition that happens to move the SCF too, so the event is disclosed but its cash-flow nature was never identified.

**No right-censoring on the AA side:** AA disclosure dates run to 2025-03 (restatements) and 2026-02 (adjustments) while the panel stops at 2023, so even a 2023 revision had time to be disclosed. The panel's own right edge is a separate problem — see the `era` flag.

**Headline result** (clean sample: testable sections, clean era, n = 120,985 filing-periods; 13,409 carry an unexplained revision):

| | n | share of revisions |
|---|---|---|
| **SILENT — no AA record, no amendment** | **10,727** | **80.0%** |
| In AA at all | 2,569 | 19.2% |
| AA flagged it as a cash-flow failure | 842 | 6.3% |
| Amendment but no AA record | 113 | 0.8% |

So four in five unexplained SCF revisions never surface in either disclosure channel, and only about one in sixteen is characterised by AA as a cash flow problem.

**It is not a small-filer artifact.** The silent rate is essentially flat across filer status — 82.1% for large accelerated filers, 78.2% accelerated, 74.0% non-accelerated, 83.4% smaller reporting. Large accelerated filers are, if anything, *slightly more* likely to have a silent revision, which rules out the obvious objection that this is micro-cap tagging sloppiness.

AA catches cross-section reclassifications marginally more often than net-cash-flow changes (21.0% vs 17.6% in AA), and characterises them as cash-flow issues more often (531 of 6,210 vs 311 of 7,199) — but both rates are low.

**Reverse coverage diagnostic:** of 14,029 AA events whose window overlaps the panel, 9,136 (65.1%) match at least one filing-period. The 35% gap is expected — AA covers filers and periods the XBRL panel does not.

**Outputs:** `scf_filing_with_aa{suffix}.parquet` (the filing panel plus `aa_match`, `aa_flagged_cash_flow`, `aa_n_events`, `aa_first_disclosed`, `aa_days_after_filing`, `aa_keys`, `disclosure_status`, `silent_revision`, `aa_missed_cash_flow_nature`, `in_clean_sample`) and `aa_cleaned.parquet` (17,703 deduplicated AA events: 14,037 restatements, 3,666 adjustments, 1,772 cash-flow flagged, 5,062 formal 8-K Item 4.02).

### Running `001a` (config at the top of the script)

The script has two phases with **separate scopes**, which is the single most important thing to understand before running it:

| config | controls | recommended |
|---|---|---|
| `TARGET_CIKS` | **extraction** — which CIKs get pulled from the raw files into the parts directory | `NULL` (everything) |
| `ANALYSIS_CIKS` | **analysis** — which extracted CIKs get compared | subset freely |
| `PILOT_N_CIKS` | random sample of N filers at extraction (unions with `TARGET_CIKS`) | `NULL` |
| `RESUME` | `TRUE` continues an interrupted extract; `FALSE` wipes the parts and starts clean | `TRUE` |

**Why extraction should always be full.** Filtering by CIK saves almost nothing: extraction is I/O-bound on *reading* each period file, not on processing rows. Measured on `2019_Q1`, two CIKs took 5.3s and all 5,211 filings took 6.2s — 2,600× the filings for 17% more time. A full extract is roughly **10–15 minutes**, so building a partial one only means being unable to reuse it for a different sample later. Extract once; subset at analysis.

**Two phases, with the expensive one checkpointed.** Extraction (Section 5) writes one parquet part per period to `{DATA_PATH}scf_lines_parts/` and appends to a done-log at `scf_lines_parts_done.csv`; analysis (Sections 6–10) reads only the lines belonging to anchor filings. The analysis is cheap (~3s for two companies) and rerunnable, so detection logic can be tuned, and pilots or memory-bounded CIK chunks can be run, without ever re-extracting.

**Changing `TARGET_CIKS` requires `RESUME <- FALSE` once.** The scope guard refuses to resume into parts built for a different extraction scope; that reset rebuilds the full parts directory in the same run.

**Checkpointing is per PERIOD, not per CIK** — the SEC Financial Statement Data Sets are organised by period (one `num`/`pre`/`sub` file holds every filer for a quarter), so resuming per CIK would mean rescanning all 86 period files (6.2 GB) per company. The JSON lineage in `001`/`001b` can checkpoint per CIK only because `companyfacts.zip` stores one file per company.

Three things make resume safe:
- A period is logged as done **even when it yields zero rows** (the target CIKs may not have filed). Otherwise zero-row periods retry forever and the completeness gate never passes.
- A **scope guard** (`scf_lines_parts_scope.rds`) refuses to resume into parts built for a different `TARGET_CIKS`, which would silently mix scopes and corrupt the anchors.
- A **completeness gate** stops before the analysis if any expected period is missing, because picking the *last* filing to re-present a period is only correct on a complete extract — a partial one would silently choose the wrong comparative.

**Memory note:** `scf_lines_raw.parquet` no longer exists as a single file; read the parts with `arrow::open_dataset(lines_dir)`, not `read_parquet()`. Also, arrow memory-maps parquet on read, so having an output file open in RStudio or a VS Code R session will block the next run's write on Windows ("user-mapped section open"); `write_parquet_safely()` retries for ~30s but cannot outlast a held handle.

### Core pattern: original vs. best value

Every pipeline script (`001`, `001b`, `001c`, `legacy/process-GAAP-taxonomy.R`) repeats the same restatement-tracking pattern per tag/account:

1. **Original**: earliest regular (10-K/10-Q) filing where a given (cik, tag, period) was the *current* reporting period (`is_current_period`, computed as `ddate == period` in the TSV lineage or `end == max(end)` per filing in the JSON lineage).
2. **Comparatives**: later regular filings that report the same period as a *prior-year/quarter comparative* — these can silently restate the original number without a formal amendment.
3. **Amendments**: 10-K/A or 10-Q/A filings covering the same period, filed after the original.
4. **`best_val`**: amendment > comparative revision > original, in that priority order. `correction`/`correction_channel`/`correction_route` categorizes which of these happened (naming varies slightly by script vintage).

`legacy/process-GAAP-taxonomy.R` extends this with two extra states other scripts don't have:
- **Sign flip** detection — a same-magnitude, opposite-sign restatement, treated as a reclassification rather than a real correction.
- **Disaggregation** — a tag that appears only in later comparative filings (never in the original filing for that period) is treated as evidence the original line item was later split apart; these get synthetic rows with `correction = "Disaggregated"` and `original_val = 0`.

### Raw data quirks handled throughout

- SEC's Financial Statement Data Sets switched from quarterly to monthly releases mid-2020 (`Q1..Q3` through 2020, then `M10, M11, M12`, then `M01..M12` from 2021 on) — the `search` grid in every TSV-lineage script encodes this.
- `dimh == "0x00000000"` marks the undimensioned "parent" value for a tag; dimensioned (e.g. segment-level) rows are only used as a fallback when no parent value exists for that (adsh, tag, ddate, qtrs).
- `iprx == "0"` and `uom == "USD"` are required quality filters on the raw `num.tsv`.
- CIKs are always zero-padded to 10 digits (`str_pad(..., width = 10, pad = "0")`) to join consistently across sources (XBRL, Audit Analytics).

### Downstream: audit sample construction

`002a-build-audit-analytics-sample.R` is the consumer of the XBRL panel output (`xbrl_all_tsv.parquet` from `001c`). It loads Audit Analytics restatement (`restatement-data-*.xlsx`) and adjustment (`adjustment-data-*.xlsx`) exports, dedupes/combines them (restatements take priority over adjustments on cik/file_date conflicts), then left-joins XBRL periods whose `end` date falls within each restatement's `[misstate_begin, misstate_end]` window (`join_by(cik, between(...))`). YTD figures (CFO/CFI/CFF/NI) are deduplicated to one row per fiscal year before summing across a restatement's window, since summing raw YTD quarters would double-count.

### `Code/000-utilities-functions.R`

A general-purpose R helper library sourced by other scripts, not specific to any one pipeline stage: variable transforms (`winsorize_x`, `truncate_x`, `standardize_x`), Fama-French 12/49 industry classification from SIC codes (`assign_FF12`, `assign_FF49`), logistic regression holdout/performance helpers built for `rsample`/`broom` workflows, LaTeX/`modelsummary` formatting helpers, and an event-study trading-day window function (`trading_day_window`). Also sets global R options (no scientific notation, `America/New_York` timezone for date handling).

## Current status (as of 2026-09-22)

**Full population extracted and analysed, with the section-assignment fixes applied.** Run took 32.95 min end to end (extract rebuild + analysis), 14.4 GB peak on a 127 GB machine — comfortably in memory, no chunking needed.

| | |
|---|---|
| Extract | 85 parts, 13,961 CIKs, 339,054 filings, 21.1M SCF line rows |
| Company-periods with a correction channel | 267,288 (265,107 comparative, 7,740 amendment) |
| Outputs | `scf_line_changes_full.parquet` (170 MB, 8.9M rows), `scf_section_changes_full.parquet` (23 MB, 768,903), `scf_period_summary_full.parquet` (6.8 MB, 267,282) |

**Reconciliation after the fixes** — on testable sections (both sides foot, boundaries unambiguous): **600,144 pass / 3,024 fail = 99.50%**. Before the fixes the comparable figure was 14,174 failures at 2.27%. Most of that "failure" population was the `coalesce(footing, TRUE)` bug manufacturing fair tests that weren't, plus filings whose sections had been discarded wholesale. 51,398 sections (6.7%) are quarantined as `section_ambiguous`; 118,453 have a side that does not foot.

**Walmart regression still exact** — all 15 annual section-periods at `recon_diff = 0`, reproducing `Examples.xlsx`.

**Answered by the broad run:**
- `order_ok == FALSE` affected 11.05% of filings and is now fixed — `section = NA` no longer occurs at all (was 5.38% of lines); ~944k lines recovered into real sections.
- Analysis fits in memory at full scale (14.4 GB), so `ANALYSIS_CIKS` chunking is unnecessary.
- The failure gradient is filer-quality, not a mechanism: pre-fix rates ran 0.46% (large accelerated) to 5.46% (smaller reporting), a 12× spread, with over-represented tags being related-party debt, warrants and derivative liabilities. Discontinued operations was tested and **ruled out** — sections containing any discontinued-ops tag failed at 1.77%, *below* baseline.

**Phase 2 is built** (`001b`, run on the full population). After removing standard-driven amounts, 42,160 testable sections retain an unexplained movement — the candidate revisions.

**All three pieces of the headline claim now exist:** (a) detect SCF revisions (`001a`), (b) strip standard-driven ones (`001b`), (c) show the survivors are absent from Audit Analytics (`002a`). **Result: 80.0% of unexplained SCF revisions in the clean sample are silent** — no AA record and no amendment — and only 6.3% are characterised by AA as a cash-flow failure. The rate is flat across filer size, so it is not a micro-cap artifact.

1. **Repoint the Audit Analytics merge at `001b`'s output.** `002a-build-audit-analytics-sample.R` still consumes `xbrl_all_tsv.parquet` from `001c`; it needs to read `scf_period_adjusted_full.parquet` instead, and the join logic revisited since the grain and column names differ. The deliverable is the proportion of unexplained revisions that Audit Analytics never recorded.
2. **Sample construction**, now decidable because the numbers exist: drop financials (SIC 6000-6999) and probably utilities; decide on the 20.8% non-testable and 6.6% ambiguous sections; truncate the panel to the clean era (roughly FY2013–2021, excluding early-XBRL and the censored right edge).
3. **Lower priority:** the residual 3,024 testable reconciliation failures (0.5%), worth a footnote rather than more engineering.

**Known stale items:** the header comment block at the top of `001a` still describes the earlier two-anchor design (two anchor filings, a single `negating` rule, a `scf_lines_raw.parquet` output) — the body is current. The 451 MB of parts still sit in Dropbox and will sync to coauthors despite being rebuildable in ~31 min.

## Roadmap / open tasks

Work here is sequenced in two dependent phases — do not skip ahead to phase 2 without the user's go-ahead:

1. **Phase 1 (built, validated on two companies): accurate SCF change detection.** Implemented in `001a-identify-scf-line-changes.R` (see "Phase 1 detection design" above). Validated against the Walmart hand-built ground truth and against Akorn, a known restatement case. Open items before declaring Phase 1 done:
   - Validate on more filers. Banks/insurers/REITs are *not* worth hardening against — accounting papers conventionally drop financials (SIC 6000-6999), so those observations get dropped anyway. `sic`/`afs` are carried in the output for that screening.
   - Decide how to treat filings whose own SCF does not foot (`both_sides_foot == FALSE`): exclude, flag, or hand-review. These are a small minority but they are the only place reconciliation fails, and non-footing SCF tagging may itself be evidence for the paper's thesis.
   - Amendments are now a full third anchor (see design above). Two open questions: whether to use the *first* or *last* amendment as the anchor when there are several (currently last, consistent with the latest-attestation convention; `legacy/process-GAAP-taxonomy.R` used first, and `n_amendments` is carried so this can be revisited), and whether comparative columns *inside* an /A filing should also count toward the silent channel (currently they do not — comparatives come only from regular 10-K/10-Q).

   Why tag-level, not section-level: you cannot just take the change in a section's total (e.g. operating cash flow as originally filed vs. as later reported in a comparative-period filing) and call that a misstatement. That net delta conflates true misstatements with legitimate, standard-driven reclassifications — e.g. ASU 2016-09 moved excess tax benefits from share-based comp from financing to operating for essentially every filer in the same adoption window, which would look identical to a misstatement if you only looked at the CFO section total. Only by tracking which *individual tags* changed within a section can phase 2 later distinguish the two.

2. **Phase 2 (BUILT in `001b`; exploratory findings retained below): standard-driven reclassification exclusion list.** Build a reference list of tags/years driven by accounting standard updates and use it to exclude/flag those changes, isolating true misstatements from standard-driven reclassifications.

   **Method that works: empirical wave detection.** A standard-driven reclassification hits many filers in the same window with the same tag, so it shows up as a year where a tag's change rate spikes far above its own baseline, usually paired with the tag being abruptly abandoned or born. Idiosyncratic misstatements are diffuse. This was validated against known answers and then found standards that were *not* on the original list — so it should stay in-code as a detector rather than being frozen into a hand-written list, which also matters for extending past 2023.

   **Waves confirmed** (ranked by excess firms affected in the peak year):

   | standard | signature | peak |
   |---|---|---|
   | **ASC 606 / ASU 2014-09** (revenue) | `IncreaseDecreaseInDeferredRevenue` rate 0.26 → 0.67; `IncreaseDecreaseInContractWithCustomerLiability` born 1 → 1,099 users | FY2017, **569 firms** |
   | **ASU 2016-09** (excess tax benefits) | both `ExcessTaxBenefit...Operating/FinancingActivities` spike FY2015–16 then die (1,298 → 42 users) | FY2016, 254 + 166 |
   | **ASU 2016-01** (financial instruments) | `PaymentsToAcquireAvailableForSaleSecuritiesDebt` / `ProceedsFromSale...Debt` appear (debt/equity split) | FY2017, 206 + 135 |
   | **ASU 2016-18** (restricted cash) | `IncreaseDecreaseInRestrictedCash` rate → 0.78, users −80% | FY2017, 182 |
   | **ASC 842** (leases) | `RepaymentsOfLongTermCapitalLeaseObligations` → `FinanceLeasePrincipalPayments` / `OperatingLeaseRightOfUseAsset...` | FY2018–20, 175 + 155 |

   **ASC 606 is the single largest confound in the data** and was not in the original plan. **ASU 2016-15 shows no wave at all** — its eight targeted issues are either too rare or land in tags that do not move.

   **Two non-ASU classes the detector also surfaces, which must be excluded for different reasons:**
   - *Early-XBRL tagging churn (2010–2012)* — e.g. `OtherNoncashIncomeExpense` spiking in 2010–11 as users grow 137%, `AdjustmentsNoncashItems...` dying in 2012 (473 → 18 users). That is XBRL phase-in by filer size and taxonomy cleanup, not accounting. Open question: flag with the same reason code as ASUs or a separate one — "the taxonomy changed" is a weaker exclusion than "the FASB required this", and a referee may want them reported separately.
   - *Right-edge censoring (2022–23)* — `ProceedsFromIssuanceInitialPublicOffering` (−58%), `ProceedsFromIssuanceOfPrivatePlacement` (−57%), `RepaymentsOfRelatedPartyDebt` (−51%) appear to "die" in 2022. They have not: the extract stops at 2023_M12, so recent periods have fewer later filings available to serve as comparatives. **This argues for truncating the panel before 2022 regardless of Phase 2.**

   **Concept clustering works as a discovery method.** Splitting tag names on CamelCase into unigrams and adjacent bigrams, then looking for term-years where the change rate spikes above that term's own baseline, recovers every standard above and dates it — with no reference list as input. The year-by-year narrative it produces: FY2011 taxes/impairment (early-XBRL phase-in), FY2012 stock options/business combinations (taxonomy churn), **FY2015 excess benefit** (2016-09), **FY2017 securities/restricted/deferred revenue** (2016-01, 2016-18, 606), **FY2018 customer/contract** (606), **FY2020–21 lease/right-of-use** (842). Keep this in-code; it is how the list stays current past 2023.

   **Timing: the ASU signal lands on the periods whose comparatives were restated, NOT the adoption year.** Excess tax benefits spike in FY2015 — a year before ASU 2016-09 was even issued — because a firm adopting in FY2017 retrospectively restates its FY2015–16 comparatives, and `001a` compares "FY2015 as originally filed" against "FY2015 as shown in the latest filing". Exclusion windows must therefore span roughly *adoption year − 2 through adoption year*, keyed off where the rate actually spikes rather than the effective date.

   **Section-level impact, measured (this is what should drive the design):**

   | standard | tags | firm-periods | % section moved | median share of move | intra | cross | net-chg | threatening periods |
   |---|---|---|---|---|---|---|---|---|
   | ASU 2016-09 | 16 | 7,103 | 58.8% | **1.00** | 44.0% | **32.6%** | 23.5% | 3,980 |
   | ASU 2016-18 | 434 | 9,836 | 61.9% | **1.00** | 31.1% | 6.5% | **62.4%** | 6,781 |
   | ASC 606 | 408 | 15,586 | 31.8% | 0.50 | **59.4%** | 5.8% | 34.8% | 6,327 |
   | ASC 842 | 3,163 | 29,560 | 25.2% | 0.73 | **70.8%** | 9.3% | 19.9% | **8,618** |

   Two things this settles:
   - **Per change**, 2016-09 and 2016-18 are the severe ones: the section moves ~60% of the time and the tag is the *sole* driver (median share 1.00). For 606 and 842 the section moves only a quarter to a third of the time and the tag explains half to three-quarters.
   - **By volume the ranking flips.** ASC 842 touches 29,560 firm-periods — 4× ASU 2016-09 — so despite the low per-change rate it yields the most threatening periods (8,618). An earlier triage dismissed 842 and 606 as "within-section, low priority"; that was wrong on volume.

   **Each standard threatens a different outcome variable**, which is why a single blanket flag would be the wrong design:
   - **ASU 2016-09 → cross-section reclassification** (32.6% cross-section, triple any other). The only one genuinely moving cash between sections, so the direct threat to the project's core claim.
   - **ASU 2016-18 → net change in cash** (62.4%). Folding restricted cash into the cash total changes the total, not its classification.
   - **ASC 606 and ASC 842 → predominantly intra-section** (59% / 71%). They contaminate tag-level descriptive counts far more than section-level results — the same exposure as the tags-proxy-for-line-items caveat above.

   Across all four, ~25,700 threatening periods before overlap, against 267,282 total — order of 10% potentially ASU-contaminated.

   **Revised deliverable:** `001b-flag-standard-driven-changes.R` should record **which standard** and **what kind of impact** per affected row, not one `standard_driven` boolean, so exclusions can be applied per outcome variable instead of discarding periods that are harmless to the test at hand. Two layers as before: a curated reference table (tag × year-window, windows set from observed spikes) plus the concept detector retained in-code. Still open: whether early-XBRL taxonomy churn (FY2011–12) gets the same reason code as FASB-driven changes, or a separate, weaker one.

## Working with this repo

- Data files (parquet, `.tsv`, `.xlsx` inputs) are never committed — they live under the paths in `.Renviron`, outside the repo. Don't assume they're readable from a fresh checkout without that env file.
- **`001a-identify-scf-line-changes.R` is the actively maintained script.** Make changes to the original/comparative/amendment detection there. `legacy/process-GAAP-taxonomy.R` is preserved as a fallback and should not be edited without the user's say-so; the JSON lineage (`001`, `001b`) and `002 Create FS from XBRL data.R` are earlier iterations kept for reference.
- `002a-build-audit-analytics-sample.R` still consumes `xbrl_all_tsv.parquet` from `001c`, **not** `001a`'s output. Repointing it at `scf_period_summary.parquet` is future work and will need the join logic revisited, since the grain and column names differ.
- Every non-obvious rule in `001a` was derived from a specific filing that broke a simpler rule, and the counter-example is cited in the comment beside it. Before "simplifying" any of them — the conditional income-block collapse, the per-line sign resolution, the `cf_periods` restriction — check the cited case, because the obvious simplification is usually what was tried first and found wrong.
- **Tooling gotcha:** writing an R script via a Bash heredoc in this environment collapses `\\1` to `\1`, so regex backreferences in a replacement string silently become control characters `\x01`/`\x02` — which corrupts output in ways that look like data problems (words losing their first and last letters) rather than a quoting bug. Either use the Write tool for scripts containing backreferences, or avoid them: zero-width lookarounds with a plain replacement work, e.g. `gsub("(?<=[a-z0-9])(?=[A-Z])", " ", x, perl = TRUE)` to split CamelCase.
