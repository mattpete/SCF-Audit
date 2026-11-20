## WARNINGS before trusting any number this prints (2026-11-14):
##  1. The "code-parseable" lever (TH = 25 chars/line) is NOT trustworthy. 25 sits
##     mid-mode in the chars/line histogram, not at the trough near 15. The
##     "36% of chunks / saves $245" figure was retracted. Ignore those two lines
##     or re-pick the threshold from the histogram this script prints.
##  2. IN_PER / OUT_PER are guesses (chars/4). Claude Haiku 5.5 uses a newer
##     tokenizer that counts roughly 30% more tokens for the same text, so
##     input is probably nearer 5,000 than 4,000 per filing.
##  3. OUT_PER ignores thinking tokens. Haiku 5.5 thinks by default (effort
##     medium) and thinking is billed as output. Run with output_config
##     {"effort": "low"} or these figures are low by a multiple.
##  4. Rates are Batch API (50% off) at the <=100K-token prompt tier. Above
##     100K tokens Haiku 5.5 jumps to $0.50/$2.50 standard. Our prompts are
##     ~5K, so the cheap tier applies.
## How much of the corpus could be parsed by code instead of by the model?
## Row-per-line layout (label + numbers on one line) is deterministically
## parseable. Cell-per-line is not. Chars-per-line separates them.
SP <- paste0(Sys.getenv("SEC_DATA_PATH"), "/pilot-2026-11/cache/")
pk <- readRDS(paste0(SP, "pick9.rds"))
bz <- readRDS(paste0(SP, "bytes.rds"))
d <- merge(pk[, c("wrdsfname","kind","len","form")],
           bz[, c("wrdsfname","kind","raw")], by = c("wrdsfname","kind"))
d$cpl <- d$raw / pmax(d$len, 1)

cat("=== chars per line, chosen chunks ===\n")
for (ty in c("cf","is","bs")) {
  v <- d$cpl[d$kind == ty]
  cat(sprintf("%-3s  p10 %4.1f  p25 %4.1f  med %4.1f  p75 %4.1f  p90 %4.1f\n",
      ty, quantile(v,.1), quantile(v,.25), median(v), quantile(v,.75), quantile(v,.9)))
}
cat("\n=== is it bimodal? histogram of chars/line (cash flow) ===\n")
v <- d$cpl[d$kind == "cf"]
h <- hist(v, breaks = c(0,10,15,20,25,30,40,50,70,100,1000), plot = FALSE)
for (i in seq_along(h$counts))
  cat(sprintf("%4.0f-%4.0f : %s %d\n", h$breaks[i], h$breaks[i+1],
      strrep("#", round(h$counts[i]/4)), h$counts[i]))

## A statement is code-parseable if its lines carry labels AND numbers together.
## Threshold picked at the trough, not fitted to an outcome.
TH <- 25
cat(sprintf("\n=== share with >= %d chars/line (code-parseable candidate) ===\n", TH))
for (ty in c("cf","is","bs")) {
  x <- d[d$kind == ty, ]
  cat(sprintf("%-3s  %.1f%%", ty, 100*mean(x$cpl >= TH)))
  for (fm in c("10-K","10-Q"))
    cat(sprintf("   %s %.1f%%", fm, 100*mean(x$cpl[x$form == fm] >= TH)))
  cat("\n")
}
cf <- d[d$kind == "cf", ]
cat(sprintf("\ncash flow: %.1f%% of chunks, carrying %.1f%% of the characters\n",
    100*mean(cf$cpl >= TH), 100*sum(cf$raw[cf$cpl >= TH])/sum(cf$raw)))

## ---- cost arithmetic, live batch rates ----
N <- 724752
IN_PER <- 4000; OUT_PER <- 3000
rate <- list(haiku = c(0.05, 0.25), sonnet = c(1, 5), opus = c(2, 10))  # batch $/MTok
cost <- function(frac_filings = 1, frac_text = 1, out_mult = 1, m = "haiku") {
  i <- N * frac_filings * IN_PER * frac_text / 1e6
  o <- N * frac_filings * OUT_PER * frac_text * out_mult / 1e6
  i * rate[[m]][1] + o * rate[[m]][2]
}
## share of characters by statement, to price scope cuts
tot <- tapply(d$raw, d$kind, sum); sh <- tot / sum(tot)
cat("\n=== share of text by statement ===\n"); print(round(100*sh, 1))

cat("\n=== full population, Batch API, Haiku 5.5 ===\n")
cat(sprintf("all three statements            $%6.0f\n", cost()))
cat(sprintf("drop balance sheet              $%6.0f\n", cost(frac_text = 1 - sh[["bs"]])))
cat(sprintf("cash flow only                  $%6.0f\n", cost(frac_text = sh[["cf"]])))
cat(sprintf("10-K only, all three            $%6.0f\n", cost(frac_filings = 192510/N,
      frac_text = 14546/11947)))
cat(sprintf("compact output schema (-40%%out) $%6.0f\n", cost(out_mult = .6)))
cat(sprintf("code-parse the easy %.0f%%         $%6.0f\n", 100*mean(cf$cpl >= TH),
      cost(frac_filings = 1 - mean(cf$cpl >= TH))))
cat("\n=== combined: cash flow only + compact output + code-parse easy ===\n")
cat(sprintf("                                $%6.0f\n",
    cost(frac_filings = 1 - mean(cf$cpl >= TH), frac_text = sh[["cf"]], out_mult = .6)))
cat("\n=== escalating 15%% of filings to Sonnet 5.5 on a footing failure ===\n")
cat(sprintf("adds                            $%6.0f\n", cost(frac_filings = .15, m = "sonnet")))
