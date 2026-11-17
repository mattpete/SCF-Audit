#!/usr/bin/env python3
"""
012a  EXTRACT STATEMENTS               step 012 (extract), substep a

Send located statement text to Claude and get structured numbers back.

Reads   extract/chunks.jsonl        (written by 011e-export-chunk-text.R)
Writes  extract/batch-state.json    submission state + custom_id map
        extract/extracted.jsonl     one result per chunk, with token usage

Uses the Message Batches API (50% off) with Claude Haiku 5.5, prompt caching on
the shared system prompt, and structured outputs so the response is schema-valid
JSON rather than JSON-ish prose.

    pip install -r requirements.txt
    set ANTHROPIC_API_KEY=sk-ant-...

    python 012a-extract-statements.py estimate                 # no API key needed
    python 012a-extract-statements.py estimate --limit 60
    python 012a-extract-statements.py submit --limit 60         # the cheap pilot
    python 012a-extract-statements.py status
    python 012a-extract-statements.py fetch

Start with `estimate`, then `submit --limit 60`. Sixty chunks costs well under a
cent and tells you whether the prompt works before you spend on 1,500.

NOTHING here is the validation logic. Footing checks, period assignment and
column selection live downstream and are expected to change as we test. This
script only gets the model's answer onto disk next to the input it came from.
"""

import argparse
import json
import os
import sys
import time
from pathlib import Path

MODEL = "claude-haiku-5-5"

# Batch API, Claude Haiku 5.5, prompts <= 100K tokens: half of $0.10 / $0.50.
# Cache reads are 0.1x the input rate; cache writes 1.25x.
RATE_IN, RATE_OUT = 0.05 / 1e6, 0.25 / 1e6
RATE_CACHE_READ, RATE_CACHE_WRITE = 0.005 / 1e6, 0.0625 / 1e6

# Haiku 5.5's tokenizer runs about 30% denser than the old chars/4 rule of thumb.
CHARS_PER_TOKEN = 3.0

# 724,752 filings in wrdssec.wrds_forms for 10-K/10-Q from 2002 on, x 3 statements.
CORPUS_CHUNKS = 724752 * 3

HERE = Path(__file__).resolve().parent
KINDS = {"cf": "cash flow statement", "is": "income statement", "bs": "balance sheet"}


def renviron_get(name):
    """.Renviron is read by R, not by the shell, so the value is normally absent
    from os.environ. Parse it directly instead of making the caller export it."""
    for d in (HERE, HERE.parent):
        f = d / ".Renviron"
        if not f.exists():
            continue
        for line in f.read_text(encoding="utf-8").splitlines():
            k, sep, v = line.partition("=")
            if sep and k.strip() == name:
                return v.strip().strip('"').strip("'")
    return None


def data_dir():
    """extract/ under SEC_DATA_PATH, the same convention the R scripts use."""
    dp = os.environ.get("SEC_DATA_PATH") or renviron_get("SEC_DATA_PATH")
    if not dp:
        sys.exit("SEC_DATA_PATH is not set. It belongs in .Renviron at the repo "
                 "root; run this from the repo root or export it in the shell.")
    d = Path(dp) / "pilot-2026-11" / "extract"
    d.mkdir(parents=True, exist_ok=True)
    return d


def load_prompt():
    sp = (HERE / "prompt" / "system-prompt.txt").read_text(encoding="utf-8")
    schema = json.loads((HERE / "prompt" / "output-schema.json").read_text(encoding="utf-8"))
    return sp, schema


def load_chunks(limit=None, kinds=None, forms=None):
    path = data_dir() / "chunks.jsonl"
    if not path.exists():
        sys.exit(str(path) + " not found. Run 011e-export-chunk-text.R first.")
    rows = []
    with path.open(encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            r = json.loads(line)
            if kinds and r.get("kind") not in kinds:
                continue
            if forms and r.get("form") not in forms:
                continue
            rows.append(r)
    rows.sort(key=lambda r: (r.get("wrdsfname") or "", r.get("kind") or ""))
    if limit:
        # Spread a small pilot across statements and form types rather than
        # taking the first N, which would all be one statement in one year.
        by = {}
        for r in rows:
            by.setdefault((r.get("kind"), r.get("form")), []).append(r)
        out, i = [], 0
        while len(out) < limit and any(len(v) > i for v in by.values()):
            for v in by.values():
                if i < len(v) and len(out) < limit:
                    out.append(v[i])
            i += 1
        rows = out
    return rows


def user_turn(row):
    kind = KINDS.get(row.get("kind"), "financial statement")
    form = row.get("form") or ""
    return ("The text below was cut out of a " + form + " filing and is expected "
            "to be the " + kind + ". Transcribe it.\n\n"
            "<statement_text>\n" + row["text"] + "\n</statement_text>")


def build_requests(rows, system_prompt, schema, max_tokens, effort):
    from anthropic.types.message_create_params import MessageCreateParamsNonStreaming
    from anthropic.types.messages.batch_create_params import Request

    # One cacheable system block shared by every request in the batch.
    system = [{"type": "text", "text": system_prompt,
               "cache_control": {"type": "ephemeral"}}]

    reqs, idmap = [], {}
    for i, row in enumerate(rows):
        cid = "c%06d" % i
        idmap[cid] = {k: row.get(k) for k in
                      ("wrdsfname", "kind", "form", "rdate", "fdate", "cik",
                       "coname", "start", "end", "chars")}
        reqs.append(Request(
            custom_id=cid,
            params=MessageCreateParamsNonStreaming(
                model=MODEL,
                max_tokens=max_tokens,
                system=system,
                # effort low: Haiku 5.5 thinks by default and thinking is billed
                # as output tokens. Transcription does not need deliberation,
                # and at the default (medium) the output bill multiplies.
                output_config={"effort": effort,
                               "format": {"type": "json_schema", "schema": schema}},
                messages=[{"role": "user", "content": user_turn(row)}],
            ),
        ))
    return reqs, idmap


def cmd_estimate(args):
    system_prompt, _ = load_prompt()
    rows = load_chunks(args.limit, args.kinds, args.forms)
    if not rows:
        sys.exit("no chunks matched")

    sys_tok = len(system_prompt) / CHARS_PER_TOKEN
    in_tok = sum(len(user_turn(r)) / CHARS_PER_TOKEN for r in rows)
    # Output is the same table re-expressed as JSON, so its size tracks the
    # printed text. This is the weakest number here, and the pilot exists
    # partly to replace it with a measurement.
    out_tok = sum(len(r["text"]) / CHARS_PER_TOKEN for r in rows)

    cached = sys_tok * max(len(rows) - 1, 0)
    cost = (in_tok * RATE_IN + sys_tok * RATE_CACHE_WRITE
            + cached * RATE_CACHE_READ + out_tok * RATE_OUT)

    print("chunks                %s" % format(len(rows), ","))
    print("system prompt         %s tokens (cached after the first request)"
          % format(int(sys_tok), ","))
    print("statement text in     %s tokens" % format(int(in_tok), ","))
    print("estimated output      %s tokens  <- the soft number"
          % format(int(out_tok), ","))
    print("estimated cost        $%.4f   (%s, batch, effort %s)"
          % (cost, MODEL, args.effort))
    per = cost / len(rows)
    print("per chunk             $%.6f" % per)
    print("")
    print("scaled to the full corpus (724,752 filings x 3 statements):")
    print("                      $%s" % format(int(per * CORPUS_CHUNKS), ","))
    print("")
    print("No API call was made and no key was needed for this.")


def cmd_submit(args):
    import anthropic
    if not os.environ.get("ANTHROPIC_API_KEY"):
        sys.exit("ANTHROPIC_API_KEY is not set.")
    system_prompt, schema = load_prompt()
    rows = load_chunks(args.limit, args.kinds, args.forms)
    if not rows:
        sys.exit("no chunks matched")

    client = anthropic.Anthropic()
    out = data_dir()
    state = {"model": MODEL, "effort": args.effort, "max_tokens": args.max_tokens,
             "submitted": time.strftime("%Y-%m-%dT%H:%M:%S"), "batches": []}

    for s in range(0, len(rows), args.max_batch):
        part = rows[s:s + args.max_batch]
        reqs, idmap = build_requests(part, system_prompt, schema,
                                     args.max_tokens, args.effort)
        # custom_ids restart at c000000 in each batch, so the map is per batch.
        batch = client.messages.batches.create(requests=reqs)
        state["batches"].append({"id": batch.id, "n": len(part), "idmap": idmap})
        print("submitted %s  %d requests  status %s"
              % (batch.id, len(part), batch.processing_status))

    (out / "batch-state.json").write_text(json.dumps(state, indent=1),
                                          encoding="utf-8")
    print("")
    print("state written to %s" % (out / "batch-state.json"))
    print("Most batches finish within the hour; the cap is 24 hours.")
    print("Next:  python 012a-extract-statements.py status")


def read_state():
    p = data_dir() / "batch-state.json"
    if not p.exists():
        sys.exit(str(p) + " not found. Nothing has been submitted.")
    return json.loads(p.read_text(encoding="utf-8"))


def cmd_status(args):
    import anthropic
    client = anthropic.Anthropic()
    state = read_state()
    done = 0
    for b in state["batches"]:
        batch = client.messages.batches.retrieve(b["id"])
        c = batch.request_counts
        print("%s  %-12s  succeeded %d  errored %d  processing %d  canceled %d  "
              "expired %d" % (b["id"], batch.processing_status, c.succeeded,
                              c.errored, c.processing, c.canceled, c.expired))
        if batch.processing_status == "ended":
            done += 1
    print("")
    print("%d/%d batches ended" % (done, len(state["batches"])))
    if done == len(state["batches"]):
        print("Next:  python 012a-extract-statements.py fetch")


def cmd_fetch(args):
    import anthropic
    client = anthropic.Anthropic()
    state = read_state()
    path = data_dir() / "extracted.jsonl"

    n_ok = n_err = n_parse = 0
    tok_in = tok_out = tok_cr = tok_cw = 0
    with path.open("w", encoding="utf-8") as f:
        for b in state["batches"]:
            idmap = b["idmap"]
            for res in client.messages.batches.results(b["id"]):
                rec = {"custom_id": res.custom_id}
                rec.update(idmap.get(res.custom_id, {}))
                rec["result_type"] = res.result.type
                if res.result.type == "succeeded":
                    msg = res.result.message
                    text = next((blk.text for blk in msg.content
                                 if blk.type == "text"), "")
                    rec["stop_reason"] = msg.stop_reason
                    u = msg.usage
                    usage = {"in": u.input_tokens, "out": u.output_tokens,
                             "cache_read": getattr(u, "cache_read_input_tokens", 0) or 0,
                             "cache_write": getattr(u, "cache_creation_input_tokens", 0) or 0}
                    rec["usage"] = usage
                    tok_in += usage["in"]
                    tok_out += usage["out"]
                    tok_cr += usage["cache_read"]
                    tok_cw += usage["cache_write"]
                    try:
                        data = json.loads(text)
                        rec["data"] = data
                        # Diagnostics only, not the validation logic: how many
                        # rows came back with the wrong number of values.
                        ncol = len(data.get("columns") or [])
                        rws = data.get("rows") or []
                        rec["n_cols"] = ncol
                        rec["n_rows"] = len(rws)
                        rec["n_bad_rows"] = sum(1 for r in rws
                                                if len(r.get("v") or []) != ncol)
                        # Did the model number the rows 1..N with no gaps?
                        seq = [r.get("n") for r in rws]
                        rec["n_seq_ok"] = seq == list(range(1, len(rws) + 1))
                    except json.JSONDecodeError as e:
                        rec["parse_error"] = str(e)
                        rec["raw"] = text
                        n_parse += 1
                    n_ok += 1
                else:
                    err = getattr(res.result, "error", None)
                    rec["error"] = getattr(err, "type", None) or str(err)
                    n_err += 1
                f.write(json.dumps(rec) + "\n")

    cost = (tok_in * RATE_IN + tok_out * RATE_OUT
            + tok_cr * RATE_CACHE_READ + tok_cw * RATE_CACHE_WRITE)
    print("wrote %s" % path)
    print("succeeded %d   failed %d   unparseable JSON %d" % (n_ok, n_err, n_parse))
    print("tokens  in %s  out %s  cache read %s  cache write %s"
          % (format(tok_in, ","), format(tok_out, ","),
             format(tok_cr, ","), format(tok_cw, ",")))
    print("actual cost  $%.4f" % cost)
    if n_ok:
        print("per chunk    $%.6f   -> full corpus $%s"
              % (cost / n_ok, format(int(cost / n_ok * CORPUS_CHUNKS), ",")))


def main():
    p = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd")

    def common(sp):
        sp.add_argument("--limit", type=int, default=None,
                        help="use only N chunks, spread across statement and form type")
        sp.add_argument("--kinds", default=None,
                        help="comma list of cf,is,bs (default all)")
        sp.add_argument("--forms", default=None,
                        help="comma list of form types, e.g. 10-K (default all)")
        sp.add_argument("--effort", default="low",
                        choices=["low", "medium", "high"],
                        help="thinking effort; low is right for transcription")
        sp.add_argument("--max-tokens", type=int, default=8000,
                        help="thinking counts toward this on Haiku 5.5")

    e = sub.add_parser("estimate", help="project cost locally, no API call")
    common(e)
    e.set_defaults(func=cmd_estimate)

    s = sub.add_parser("submit", help="create the batch")
    common(s)
    s.add_argument("--max-batch", type=int, default=10000,
                   help="requests per batch (API cap is 100,000 / 256 MB)")
    s.set_defaults(func=cmd_submit)

    st = sub.add_parser("status", help="poll the batch")
    st.set_defaults(func=cmd_status)

    fe = sub.add_parser("fetch", help="download results to extracted.jsonl")
    fe.set_defaults(func=cmd_fetch)

    a = p.parse_args()
    if not getattr(a, "func", None):
        p.print_help()
        sys.exit(1)
    for attr in ("kinds", "forms"):
        v = getattr(a, attr, None)
        setattr(a, attr, set(v.split(",")) if v else None)
    a.func(a)


if __name__ == "__main__":
    main()
