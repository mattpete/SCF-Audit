# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

This is an academic accounting/finance research project (R), studying how SEC filers correct/restate the Statement of Cash Flows (SCF) and related financial statement line items (Net Income, Stockholders' Equity, Assets, Liabilities) across 10-K/10-Q filings, revisions, and amendments (10-K/A, 10-Q/A). It builds firm-period panels from SEC XBRL data and cross-references them against Audit Analytics restatement/adjustment data.

There is no application code, package, or test suite — this is a linear sequence of R data-processing scripts run interactively in RStudio (`SCF-Audit.Rproj`).

### Research question / hypothesis

The core question: do auditors and preparers scrutinize the SCF with the same rigor as the balance sheet and income statement?

The tension motivating the project: classification errors are conventionally treated as immaterial, and that's often defensible for the balance sheet or income statement, where classification doesn't change bottom-line totals. But for the SCF, classification *is* the statement's entire value proposition — moving an item between operating/investing/financing changes nothing about net income or total assets, yet it's the exact distinction users of the SCF rely on. A materiality framework built for the other two statements may therefore be the wrong lens for the SCF.

The hypothesis: there are many undisclosed SCF reclassifications, and where issues are disclosed, they're often characterized as immaterial even when they are numerically material by conventional thresholds.

This is why `001e` tracks the **disclosed** channel (10-K/A, 10-Q/A amendments) separately from the **silent** channel (a figure quietly changing in a later filing's comparative column), and why it works at the individual line-item level: a reclassification recharacterizes *where* a cash flow appears without moving the reported total, so a standard "did net income change" restatement screen misses it entirely. Akorn's Q2/Q3 2014 10-Q/A is the pattern in miniature — a formally disclosed amendment that restated several operating line items while leaving CFO exactly unchanged.

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

The `Code/` scripts are numbered by pipeline stage, but there are two parallel/superseded lineages for extracting XBRL tags — know which one is current before editing:

- **JSON lineage (superseded):** `001-extract-CF-tags-from-XBRL-json.R` → `001b-extract-all-tags-from-XBRL-json.R`. Reads SEC's per-CIK `companyfacts.zip` (JSON), pulling a fixed, hardcoded list of `us-gaap` concept names (e.g. `NetCashProvidedByUsedInOperatingActivities`) directly by tag name, with simple fallback tags (e.g. `ProfitLoss` if `NetIncomeLoss` missing).
- **TSV lineage (current):** `001c-extract-all-tags-from-XBRL-tsv.R` → `001d-process-GAAP-taxonomy.R`. Reads SEC's quarterly/monthly Financial Statement Data Sets (`_num.tsv` + `_sub.tsv`). `001c OLD.R` is a stale prior version kept for reference only.
  - `001d` is the most refined script: instead of a hardcoded tag list, it derives the target SCF tags dynamically from `GAAP_Taxonomy.xlsx` (Presentation sheet), filtered to Statement-of-Cash-Flows-related `definition` values (covers standard, deposit-based, insurance-based, securities-based, REIT, and direct-method SCF variants). This makes tag coverage taxonomy-driven rather than a fixed list, and adds reclassification/disaggregation detection (see below) that the earlier scripts lack.
  - `002 Create FS from XBRL data.R` is a separate, broader/earlier exploration that builds full financial statements (BS/IS/CF/EQ) from the raw `num`/`tag`/`pre`/`sub` files rather than isolating specific SCF tags — largely scratch/validation code exploring the raw file structure.
- **Current script for Phase 1: `001e-identify-scf-line-changes.R`.** Supersedes `001d` for line-item change detection (`001d` is preserved as a fallback). See "Phase 1 detection design" below.

### Phase 1 detection design (`001e`)

Detects changes between the SCF **as originally filed** and the SCF **as later re-presented as a prior-period comparative**, at the individual line-item level. Key design decisions, each of which fixes a specific failure mode found while validating against Walmart:

- **SCF membership comes from `pre` (`stmt == "CF"`), not the GAAP taxonomy.** Taxonomy-role filtering (what `001d` does) silently drops tags a filer borrowed from another statement's taxonomy section. This is the source of the `"missing?"` entries in `Examples.xlsx` — both resolve to standard `us-gaap` tags (`RepaymentsOfLongTermCapitalLeaseObligations`, `OtherNoncashIncomeExpense`) that `001d` misses.
- **Section assignment** is derived per filing from presentation ordering relative to the three section subtotal tags, so every presented line is attributed to operating/investing/financing.
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

**Reconciliation is the coverage test**, per the project's own logic: within a section, component differences must sum to the subtotal difference. It is only a fair test when both filings' own cash flow statements foot, which `both_sides_foot` records. Where they foot and reconciliation still fails, the script is missing a line; where they don't, the filing's own XBRL is defective — itself a finding. A confirmed example: Walmart's Q2 FY2019 10-Q omits `ProceedsFromIssuanceOfLongTermDebt` (15,851M) from its tagged SCF entirely — the value exists in `num` but `pre` assigns it to a note, not `stmt == "CF"`, and the statement jumps from short-term debt straight to repayments.

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

### Running `001e` (config at the top of the script)

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

Every pipeline script (`001`, `001b`, `001c`, `001d`) repeats the same restatement-tracking pattern per tag/account:

1. **Original**: earliest regular (10-K/10-Q) filing where a given (cik, tag, period) was the *current* reporting period (`is_current_period`, computed as `ddate == period` in the TSV lineage or `end == max(end)` per filing in the JSON lineage).
2. **Comparatives**: later regular filings that report the same period as a *prior-year/quarter comparative* — these can silently restate the original number without a formal amendment.
3. **Amendments**: 10-K/A or 10-Q/A filings covering the same period, filed after the original.
4. **`best_val`**: amendment > comparative revision > original, in that priority order. `correction`/`correction_channel`/`correction_route` categorizes which of these happened (naming varies slightly by script vintage).

`001d` extends this with two extra states other scripts don't have:
- **Sign flip** detection — a same-magnitude, opposite-sign restatement, treated as a reclassification rather than a real correction.
- **Disaggregation** — a tag that appears only in later comparative filings (never in the original filing for that period) is treated as evidence the original line item was later split apart; these get synthetic rows with `correction = "Disaggregated"` and `original_val = 0`.

### Raw data quirks handled throughout

- SEC's Financial Statement Data Sets switched from quarterly to monthly releases mid-2020 (`Q1..Q3` through 2020, then `M10, M11, M12`, then `M01..M12` from 2021 on) — the `search` grid in every TSV-lineage script encodes this.
- `dimh == "0x00000000"` marks the undimensioned "parent" value for a tag; dimensioned (e.g. segment-level) rows are only used as a fallback when no parent value exists for that (adsh, tag, ddate, qtrs).
- `iprx == "0"` and `uom == "USD"` are required quality filters on the raw `num.tsv`.
- CIKs are always zero-padded to 10 digits (`str_pad(..., width = 10, pad = "0")`) to join consistently across sources (XBRL, Audit Analytics).

### Downstream: audit sample construction

`002-build-quarterly-audit-sample.R` is the consumer of the XBRL panel output (`xbrl_all_tsv.parquet` from `001c`). It loads Audit Analytics restatement (`restatement-data-*.xlsx`) and adjustment (`adjustment-data-*.xlsx`) exports, dedupes/combines them (restatements take priority over adjustments on cik/file_date conflicts), then left-joins XBRL periods whose `end` date falls within each restatement's `[misstate_begin, misstate_end]` window (`join_by(cik, between(...))`). YTD figures (CFO/CFI/CFF/NI) are deduplicated to one row per fiscal year before summing across a restatement's window, since summing raw YTD quarters would double-count.

### `Code/000-utilities-functions.R`

A general-purpose R helper library sourced by other scripts, not specific to any one pipeline stage: variable transforms (`winsorize_x`, `truncate_x`, `standardize_x`), Fama-French 12/49 industry classification from SIC codes (`assign_FF12`, `assign_FF49`), logistic regression holdout/performance helpers built for `rsample`/`broom` workflows, LaTeX/`modelsummary` formatting helpers, and an event-study trading-day window function (`trading_day_window`). Also sets global R options (no scientific notation, `America/New_York` timezone for date handling).

## Current status (as of 2026-08-14)

**Where we are:** `001e` is built and validated on two companies. The **first full-population extraction** was kicked off from RStudio with `TARGET_CIKS <- NULL` and `RESUME <- FALSE` (the one-time reset needed to change extraction scope). Expected runtime ~10–15 minutes; it writes ~86 parquet parts covering roughly 30M SCF line rows.

**Immediately after the extract finishes**, in order:
1. Confirm the extract completed — `scf_lines_parts_done.csv` should list all 86 expected periods. If the run was interrupted, just re-run with `RESUME <- TRUE`; the completeness gate stops before the analysis if anything is missing.
2. Run the analysis with `ANALYSIS_CIKS <- c("0000104169", "0000003116")` as a **regression check** — Walmart's 15 annual section-periods must all reconcile with `recon_diff == 0`, reproducing `Examples.xlsx`.
3. Run a ~300-CIK random sample (set `ANALYSIS_CIKS` to a sample of the extracted CIKs) for broad reconciliation rates, then the full population.

**Open questions the broad run should answer** — none of these have been seen in two clean industrial filers:
- How often is `order_ok == FALSE` (sections not in the expected operating → investing → financing order)? Those lines currently get `section = NA`.
- How many filings present **no** CF section subtotals at all? Those are silently dropped by the `cf_periods` restriction and should be counted.
- What share of sections have `both_sides_foot == FALSE`? That is the only place reconciliation fails, and it drives the exclude/flag/hand-review decision below.
- Does the analysis phase fit in memory at full scale? If not, chunk it via `ANALYSIS_CIKS` over CIK batches — the extract does not need redoing.

**Known stale item:** the header comment block at the top of `001e` still describes the earlier two-anchor design (it mentions two anchor filings, a single `negating` rule, and a `scf_lines_raw.parquet` output). The body of the script is current; the header needs a refresh.

## Roadmap / open tasks

Work here is sequenced in two dependent phases — do not skip ahead to phase 2 without the user's go-ahead:

1. **Phase 1 (built, validated on two companies): accurate SCF change detection.** Implemented in `001e-identify-scf-line-changes.R` (see "Phase 1 detection design" above). Validated against the Walmart hand-built ground truth and against Akorn, a known restatement case. Open items before declaring Phase 1 done:
   - Validate on more filers. Banks/insurers/REITs are *not* worth hardening against — accounting papers conventionally drop financials (SIC 6000-6999), so those observations get dropped anyway. `sic`/`afs` are carried in the output for that screening.
   - Decide how to treat filings whose own SCF does not foot (`both_sides_foot == FALSE`): exclude, flag, or hand-review. These are a small minority but they are the only place reconciliation fails, and non-footing SCF tagging may itself be evidence for the paper's thesis.
   - Amendments are now a full third anchor (see design above). Two open questions: whether to use the *first* or *last* amendment as the anchor when there are several (currently last, consistent with the latest-attestation convention; `001d` used first, and `n_amendments` is carried so this can be revisited), and whether comparative columns *inside* an /A filing should also count toward the silent channel (currently they do not — comparatives come only from regular 10-K/10-Q).

   Why tag-level, not section-level: you cannot just take the change in a section's total (e.g. operating cash flow as originally filed vs. as later reported in a comparative-period filing) and call that a misstatement. That net delta conflates true misstatements with legitimate, standard-driven reclassifications — e.g. ASU 2016-09 moved excess tax benefits from share-based comp from financing to operating for essentially every filer in the same adoption window, which would look identical to a misstatement if you only looked at the CFO section total. Only by tracking which *individual tags* changed within a section can phase 2 later distinguish the two.

2. **Phase 2 (blocked on phase 1): standard-driven reclassification exclusion list.** Once tag-level change detection in phase 1 is trustworthy, build an exhaustive reference list of specific tags/years driven by known accounting standard updates (e.g. ASU 2016-09 excess tax benefit tags, ASU 2016-15's eight targeted classification issues, ASU 2016-18 restricted cash) and use it to exclude/flag those changes, isolating true misstatements from standard-driven reclassifications. This phase operates on the tag-level output phase 1 produces, so its design may depend on exactly what phase 1's output schema looks like — expect this to spawn further sub-tasks once phase 1 is done.

## Working with this repo

- Data files (parquet, `.tsv`, `.xlsx` inputs) are never committed — they live under the paths in `.Renviron`, outside the repo. Don't assume they're readable from a fresh checkout without that env file.
- **`001e-identify-scf-line-changes.R` is the actively maintained script.** Make changes to the original/comparative/amendment detection there. `001d` is preserved as a fallback and should not be edited without the user's say-so; the JSON lineage (`001`, `001b`) and `002 Create FS from XBRL data.R` are earlier iterations kept for reference.
- `002-build-quarterly-audit-sample.R` still consumes `xbrl_all_tsv.parquet` from `001c`, **not** `001e`'s output. Repointing it at `scf_period_summary.parquet` is future work and will need the join logic revisited, since the grain and column names differ.
- Every non-obvious rule in `001e` was derived from a specific filing that broke a simpler rule, and the counter-example is cited in the comment beside it. Before "simplifying" any of them — the conditional income-block collapse, the per-line sign resolution, the `cf_periods` restriction — check the cited case, because the obvious simplification is usually what was tried first and found wrong.
