# The extraction prompt, and why it is shaped this way

> Loaded by `legacy/extract-statements.py`. Written BEFORE the Nov 2026 prompt
> revision, so it predates the column-object format, the per-row `x` flag and
> the precision-is-not-a-multiplier rule. The live files are
> `prompt/system-prompt.txt` and `prompt/output-schema.json`.

Two files hold the actual prompt. This file explains them.

- `prompt/system-prompt.txt` — the instructions, sent as a cached system block
- `prompt/output-schema.json` — the JSON schema the response is constrained to

Both are loaded by `legacy/extract-statements.py`. Edit the two files, not the Python.

**Status: written, never run.** No API call has been made. Everything below is
a design argument, not a measurement. The pilot (`submit --limit 60`) is what
turns it into a measurement.

---

## The one idea

The model transcribes. It does not interpret.

Everything that requires judgment about *what a number means* — which fiscal
year a column is, whether to keep year-to-date or quarter-to-date, whether a
figure was revised — happens in R and Python afterwards, where it can be read,
tested and corrected. The model's only job is to turn a mangled ASCII table
into the same table in JSON, in printed order.

This matters because the failure modes are different. A transcription error is
visible: the numbers do not foot, the row count is wrong, a cell is missing. An
interpretation error is invisible: the model silently decides a column is 2021
when it is 2020, and nothing in the output looks wrong. We can catch the first
kind for free. We cannot catch the second kind at all.

So the prompt forbids interpretation explicitly, in the hard rules.

---

## The output shape

```json
{
  "found": true,
  "statement": "cash_flow",
  "scale": "thousands",
  "columns": ["Nine Months Ended September 30, 2022",
              "Nine Months Ended September 30, 2021"],
  "rows": [
    {"n": 1, "l": "Cash flows from operating activities:", "v": [null, null]},
    {"n": 2, "l": "Net income", "v": [2533, 1404]},
    {"n": 3, "l": "Depreciation and amortization", "v": [412, 388]},
    {"n": 4, "l": "Net cash provided by operating activities", "v": [3104, 1998]}
  ]
}
```

### Why `l` and `v` instead of `label` and `values`

Output tokens are 79% of the bill. `{"label":...,"values":[...]}` costs roughly
eight more tokens per row than `{"l":...,"v":[...]}`. At ~100 rows per statement
and 2.17M statements that is about 1.7 billion output tokens, or **$435 at the
batch rate** — real money for a purely cosmetic difference. The schema carries
a `description` for each field, so the model is not guessing what they mean.

`n` is the one field bought against that logic, and the next section says why.

### Why `n` — the renamed-line problem

**A renamed line looks exactly like a revision, and this is the failure mode
most likely to inflate the paper's headline number.**

The test compares one period reported twice: as originally filed in year *t*,
and as the prior-year comparative in the filing for *t+1*. Those two statements
are drafted a year apart, by people free to reword any caption. So:

| filing | label | value |
|---|---|---|
| 10-K FY2021, current year column | Depreciation and amortization | 412 |
| 10-K FY2022, prior year column | Depreciation, depletion and amortization | 412 |

Matched on the label, that is a line that vanished and a line that appeared —
and a naive diff scores it as a revision in both directions. Matched on
position, it is one line, unchanged, renamed. The amount never moved.

The reverse error is just as bad: if labels are matched loosely enough to
survive rewording, two genuinely different lines get merged and a real revision
disappears.

Line order fixes this because **statement ordering is far more stable across
filings than captions are**. The same firm's cash flow statement keeps its
sequence — net income, then the non-cash addbacks in the same order, then
working capital, then the subtotal — while the wording drifts. Position plus
the neighbouring labels plus the value identifies the slot. The rule it
enables, stated plainly:

> If the label differs but the position, the neighbours and the prior-period
> value all agree, it is a rename, not a revision.

That rule needs the position to exist in the data, so the model states it.

**Two honest caveats.**

First, `n` is recoverable from the array order, since `rows` is already in
printed order. Having the model emit it costs roughly four output tokens per
row — about **$217 over the full corpus** — for information already implied. It
is bought anyway because the field makes the renaming logic explicit and
survives every downstream filter, join and partial parse, where array position
does not. If the pilot shows ordering is reliable, dropping `n` and numbering
in code is a clean $217 saving with no loss. That decision is cheap either way
and should be made on pilot evidence.

Second, `n` is **not** evidence of completeness. If the model skips a printed
row it will number what remains consecutively, and `n_seq_ok` will still be
true. The field detects a model that loses track mid-table; it cannot detect
one that quietly drops a line. Row-count-against-source and footing checks are
what catch that.

**The companion signal, not implemented:** indent depth. Order tells you *which
slot*; depth tells you *what kind of row* — component, subtotal, or total.
Together they are a much stronger key than either alone, because depth survives
a firm inserting or removing a line, which shifts every position below it. If
position alone proves insufficient for matching, adding a `d` field is the first
thing to try, and it is about one token per row.

### Why `columns` is verbatim text, not parsed periods

Measured on all 498 cash-flow chunks in the sample (`diagnostics/measure-column-order.R`):

| | |
|---|---|
| header states the years explicitly | 488 of 498 |
| states two or more years | 473 |
| **first column is the filing's own fiscal year** | **98.7%** |
| clearly descending (current year first) | 76.7% |
| clearly ascending | 1.5% |
| "mixed" | 21.8% |

The 1.5% ascending is the whole problem. If we told the model "the first column
is the current year" it would be wrong about 7 filings in 500 and we would never
know which. If we assume descending and are wrong, the revision we are trying to
measure is manufactured out of nothing: a difference appears where there is none.

So the model returns the heading text and we parse it. A string we can read is
auditable; a year the model inferred is not.

The 21.8% "mixed" is mostly not ambiguity — it is **repeated** descending
headers, which is how a four-column 10-Q looks: three months ended for two
years, then nine months ended for the same two years. Which brings us to:

### Why repeated columns must be kept

A 10-Q income statement or cash flow statement typically prints four columns:

```
                          Three Months Ended        Nine Months Ended
                             September 30,            September 30,
                          2022        2021         2022        2021
Net income                 980         612        2,533       1,404
```

Both 2022 columns are real and they are different numbers. The prompt tells the
model to return all four, in printed order, without merging or deduplicating the
two blocks that share a date — because the natural instinct is to look at
`2022, 2021, 2022, 2021` and decide something is wrong.

Selecting which pair we actually want is a code decision, made by reading the
`columns` strings. For the revision test we want year-to-date (the nine-month
column), because that is the figure the following year's comparative restates.

### Why `scale` is a word, not a number

"thousands", not 1000. The model is reporting what the page says, which is a
word. Turning it into a multiplier is arithmetic, and arithmetic is our job. The
prompt also forbids applying it: if the page says 2,533 we want 2533, never
2533000. Getting this wrong is a 1,000× error on every number in the filing, and
it is the single most damaging thing the model could silently do.

### Why section headings and totals are rows with null values

Three reasons. They preserve printed order, so a row's position is meaningful.
They make the row count checkable against the source text. And subtotals are in
scope for this study — the user settled that early: full line items *and*
subtotals, because a reclassification often leaves the subtotal untouched while
moving the components, and that contrast is the signal.

### Why `found` and `statement` exist

The selection stage has known false positives it cannot fix structurally:
Schedule I parent-only statements, co-registrant statements, and financial
statements reproduced inside an earnings-release exhibit all pass every
structural test we have. Asking the model what it is actually looking at is the
cheapest available filter, and it costs about three output tokens.

`statement` is also a free check on the selection stage: if the model says
`balance_sheet` for a chunk we labelled `cf`, the chunk was mis-selected.

---

## Request settings, and why

| setting | value | reason |
|---|---|---|
| model | `claude-haiku-5-5` | $0.10/$0.50 per MTok at ≤100K prompts; ours are ~5K |
| Batch API | yes | 50% off, and nothing here is latency-sensitive |
| `output_config.effort` | `low` | **see below** |
| `output_config.format` | `json_schema` | guarantees schema-valid JSON, no parsing of prose |
| `max_tokens` | 8000 | thinking counts toward this on Haiku 5.5 |
| cache | system block | ~1,300 tokens reused by every request |

### effort low is a cost lever, not a quality preference

Claude Haiku 5.5 thinks by default, at effort `medium`, and **thinking tokens
are billed as output**. Output is 79% of our bill. Running at the default could
multiply the output bill for a task that is transcription — there is nothing to
deliberate about. `low` is the setting; raising it is a deliberate experiment to
run against the pilot, not a default.

This is a bigger lever than the model choice itself, and it was not in the
earlier cost estimates. Those estimates are therefore low by an unknown
multiple, which is one more reason to run the 60-chunk pilot before the 1,500.

### Prompt caching

The system prompt is ~1,300 tokens and is identical across every request, so it
is sent with `cache_control: ephemeral`. Haiku 5.5's cache minimum is 512
tokens, so it qualifies. Cache reads cost 0.1× input. Over the full corpus this
saves roughly $127 — immaterial for the pilot, worth having for free.

---

## Known problems with this prompt

Listed because they are real, not because the approach is bad.

1. **Per-row unit exceptions.** A statement "in thousands, except per share
   data" prints EPS in dollars and share counts sometimes in thousands and
   sometimes in units. The prompt says to transcribe as printed, which is
   correct, but it leaves a single `scale` for a statement that has two. This
   lands mainly on the income statement. Current plan: identify EPS and
   share-count rows by label downstream. Not implemented.

2. **Cell-per-line layouts.** About 41% of the cleaned text puts every label and
   every number on its own line, so a row's cells are consecutive lines after
   its label. The prompt explains both layouts and gives an example of each.
   Whether the model handles the cell-per-line case reliably is **the single
   biggest unknown** in the whole pipeline, and it is exactly what the pilot
   tests. If accuracy splits by layout, that is a finding, and it is
   recoverable: chars-per-line separates the two layouts cheaply, so the hard
   half could go to a stronger model.

3. **Two statements in one chunk.** Rule 5 says transcribe the first and ignore
   the rest. That is a guess at the right behaviour. It may be better to return
   both and choose downstream.

4. **No few-shot examples.** The prompt is instructions only. Adding one worked
   example of each layout would probably help accuracy and would cost ~600
   cached input tokens, which is nearly free. Deliberately left out of v1 so the
   pilot measures the instructions alone; if accuracy is marginal, this is the
   first thing to try.

5. **`found: false` is untested.** No chunk in the sample is known to be a
   non-statement, so nothing exercises that path.

---

## How to tell whether it worked

After `fetch`, `extracted.jsonl` carries per-chunk diagnostics that need no
further work: `n_cols`, `n_rows`, `n_bad_rows` (rows whose value count does not
match the column count), `n_seq_ok` (did `n` run 1..N with no gaps),
`stop_reason`, and actual token usage.

A clean pilot looks like: `n_bad_rows` zero almost everywhere, `n_seq_ok` true
everywhere, `stop_reason` `end_turn` not `max_tokens`, `statement` agreeing with
`kind`, and `n_rows` plausible against the chunk's line count. Any of those
failing is diagnostic rather than fatal, and points at a specific fix.

The real accuracy check is footing — cash flow subtotals summing to their
components, balance sheets balancing — and against the five hand-verified chunks
plus Walmart. That is the validation work, and it is deliberately not in these
scripts.
