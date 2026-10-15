# Consolidated regression tables for the Big R disclosure-decision test.
# No controls (ICMW / SEC / fraud are consequences of the non-reliance
# decision, so conditioning on them conditions on the outcome).
# SEs clustered by CIK throughout.

suppressMessages({
  library(dplyr); library(arrow); library(glue); library(readxl)
  library(stringr); library(sandwich); library(lmtest)
})
dp <- Sys.getenv("DATA_PATH"); options(width = 200, scipen = 999)

# ---------- sample ----------
raw <- read_excel(glue("{dp}restatement-data-031425.xlsx")) |>
  transmute(cik = str_pad(`CIK Code`, width = 10, pad = "0"),
            file_date = as.Date(`Disclosure Accepted`),
            assets_h = as.numeric(`H - Assets ($)`),
            assets_mr = as.numeric(`MR - Assets ($)`),
            mcap_h = as.numeric(`H - Market Cap ($)`),
            mcap_mr = as.numeric(`MR - Market Cap ($)`)) |>
  group_by(cik, file_date) |> slice(1) |> ungroup()

CF_LAB   <- "Cash flow statement (SFAS 95) classification errors"
IS_BROAD <- paste("Revenue recognition issues",
                  "Expense (payroll, SGA, other) recording issues",
                  "Gain or loss recognition issues",
                  "EPS, ratio and classification of income statement issues",
                  sep = "|")
wins <- function(x, p = .01) { q <- quantile(x, c(p, 1-p), na.rm = TRUE); pmin(pmax(x,q[1]),q[2]) }
std  <- function(x) as.numeric(scale(x))

d <- read_parquet(glue("{dp}aa_events_scf_characterised.parquet")) |>
  filter(matched_panel, aa_source == "restatement") |>
  left_join(raw, by = c("cik","file_date")) |>
  mutate(scaler = coalesce(assets_h, assets_mr),
         scaler_mc = coalesce(mcap_h, mcap_mr),
         bigR = as.integer(restate == 1),
         fails = coalesce(acct_failures, ""),
         scf_err = as.integer(str_detect(fails, fixed(CF_LAB))),
         is_err  = as.integer(str_detect(fails, regex(IS_BROAD))),
         scf_only = as.integer(scf_err == 1 & num_failures == 1),
         is_only  = as.integer(is_err  == 1 & num_failures == 1),
         dNI_raw  = abs(as.numeric(chg_ni)) / scaler,
         dCFO_raw = abs(cfo_change) / scaler,
         sNI_raw  = as.numeric(chg_ni) / scaler) |>
  filter(!is.na(scaler), scaler > 0, is.finite(dNI_raw), is.finite(dCFO_raw)) |>
  mutate(dNI = std(wins(dNI_raw)), dCFO = std(wins(dCFO_raw)),
         sNI = std(wins(sNI_raw)))

cat("SAMPLE: AA restatement-file events matched to the XBRL panel\n")
cat("  n =", nrow(d), " Big R =", sum(d$bigR),
    sprintf("(%.1f%%)", 100*mean(d$bigR)), " clusters(cik) =", n_distinct(d$cik), "\n")
cat("  dNI  = |AA cumulative change in net income| / assets, winsorised 1/99, standardised\n")
cat("  dCFO = |our summed CFO change| / assets, winsorised 1/99, standardised\n")
cat("  SCF-labelled =", sum(d$scf_err), " I/S-labelled =", sum(d$is_err),
    " SCF-only =", sum(d$scf_only), " I/S-only =", sum(d$is_only), "\n")

# ---------- formatter: coef with t/z underneath ----------
star <- function(p) if (p < .01) "***" else if (p < .05) "**" else if (p < .1) "*" else ""
fit <- function(fml, data, type = c("logit","lpm")) {
  type <- match.arg(type)
  m <- if (type == "logit") glm(fml, data, family = binomial()) else lm(fml, data)
  V <- vcovCL(m, cluster = data$cik)
  ct <- coeftest(m, vcov = V)
  list(m = m, ct = ct, n = nobs(m), type = type,
       terms = rownames(ct), V = V, b = coef(m))
}
tab <- function(models, labels, keep) {
  rows <- c()
  for (k in keep) {
    l1 <- sprintf("  %-28s", k); l2 <- sprintf("  %-28s", "")
    for (mm in models) {
      if (k %in% mm$terms) {
        b <- mm$ct[k,1]; t <- mm$ct[k,3]; p <- mm$ct[k,4]
        l1 <- paste0(l1, sprintf("%14s", paste0(sprintf("%.4f", b), star(p))))
        l2 <- paste0(l2, sprintf("%14s", sprintf("(%.2f)", t)))
      } else {
        l1 <- paste0(l1, sprintf("%14s", "")); l2 <- paste0(l2, sprintf("%14s",""))
      }
    }
    rows <- c(rows, l1, l2)
  }
  hdr <- sprintf("  %-28s", "") ;
  for (l in labels) hdr <- paste0(hdr, sprintf("%14s", l))
  nline <- sprintf("  %-28s", "N")
  for (mm in models) nline <- paste0(nline, sprintf("%14s", format(mm$n, big.mark=",")))
  cat(hdr, "\n", paste(rep("-", nchar(hdr)), collapse=""), "\n", sep="")
  cat(paste(rows, collapse = "\n"), "\n")
  cat(paste(rep("-", nchar(hdr)), collapse=""), "\n")
  cat(nline, "\n")
}

cat("\n\n==========================================================================\n")
cat("PANEL A. Big R on error magnitude. LOGIT, no controls, SE clustered by cik\n")
cat("         t-statistics in parentheses. *** p<.01 ** p<.05 * p<.1\n")
cat("==========================================================================\n")
a1 <- fit(bigR ~ dNI, d); a2 <- fit(bigR ~ dCFO, d); a3 <- fit(bigR ~ dNI + dCFO, d)
dmc <- d |> filter(!is.na(scaler_mc), scaler_mc > 0) |>
  mutate(dNI = std(wins(abs(as.numeric(chg_ni))/scaler_mc)),
         dCFO = std(wins(abs(cfo_change)/scaler_mc)))
a4 <- fit(bigR ~ dNI + dCFO, dmc)
a5 <- fit(bigR ~ sNI + dCFO, d)
dpos <- d |> filter(dNI_raw > 0, dCFO_raw > 0) |>
  mutate(dNI = std(log(dNI_raw)), dCFO = std(log(dCFO_raw)))
a6 <- fit(bigR ~ dNI + dCFO, dpos)
tab(list(a1,a2,a3,a4,a5,a6),
    c("(1)","(2)","(3)","(4) mcap","(5) signed","(6) logs"),
    c("dNI","dCFO","sNI","(Intercept)"))
cat("\n  (4) scales by market cap not assets. (5) uses SIGNED NI change.\n")
cat("  (6) log magnitudes, restricted to events where both are strictly positive.\n")
wald <- function(mm, k1, k2) {
  b <- mm$b; V <- mm$V
  dif <- b[k1]-b[k2]
  se <- sqrt(V[k1,k1]+V[k2,k2]-2*V[k1,k2])
  cat(sprintf("  Wald b(%s) - b(%s) = 0 : diff %.4f, se %.4f, z %.2f, p %.5f\n",
              k1, k2, dif, se, dif/se, 2*pnorm(-abs(dif/se))))
}
cat("\n"); wald(a3, "dNI", "dCFO"); wald(a6, "dNI", "dCFO")

cat("\n\n==========================================================================\n")
cat("PANEL B. Within each affected group: does OWN-statement magnitude matter?\n")
cat("         LOGIT, no controls, clustered by cik\n")
cat("==========================================================================\n")
b1 <- fit(bigR ~ dCFO, d |> filter(scf_err == 1))
b2 <- fit(bigR ~ dNI,  d |> filter(is_err == 1))
b3 <- fit(bigR ~ dCFO, d |> filter(scf_only == 1))
b4 <- fit(bigR ~ dNI,  d |> filter(is_only == 1))
tab(list(b1,b2,b3,b4),
    c("SCF-any","I/S-any","SCF-only","I/S-only"),
    c("dCFO","dNI","(Intercept)"))
cat("\n  CAUTION: these logit coefficients are NOT comparable across columns.\n")
cat("  Base rates differ hugely (see AMEs below), and logit coefficients are\n")
cat("  normalised by the latent residual variance. Use the AMEs.\n")

cat("\n  Average marginal effect of +1 SD of own-statement magnitude:\n")
ame <- function(data, xraw, lab) {
  dd <- data |> mutate(x = std(wins(xraw)))
  m <- glm(bigR ~ x, dd, family = binomial()); V <- vcovCL(m, cluster = dd$cik)
  p <- plogis(coef(m)[1] + coef(m)[2]*dd$x); dens <- mean(p*(1-p))
  a <- dens*coef(m)[2]; se <- dens*sqrt(V[2,2])
  cat(sprintf("    %-34s n=%5d  base %5.1f%%  AME %6.2f pp  (z %.2f)\n",
              lab, nrow(dd), 100*mean(dd$bigR), 100*a, a/se))
}
ame(d |> filter(scf_err==1), (d |> filter(scf_err==1))$dCFO_raw, "SCF-labelled: effect of dCFO")
ame(d |> filter(is_err==1),  (d |> filter(is_err==1))$dNI_raw,  "I/S-labelled: effect of dNI")

cat("\n\n==========================================================================\n")
cat("PANEL C. High/low magnitude x which statement. LINEAR PROBABILITY MODEL\n")
cat("         Coefficients are PROBABILITY POINTS, comparable across groups.\n")
cat("         bigR ~ is_grp * high ; clustered by cik\n")
cat("==========================================================================\n")
stacked <- bind_rows(
  d |> filter(scf_err == 1, is_err == 0) |> transmute(cik, bigR, mag = dCFO_raw, is_grp = 0L),
  d |> filter(is_err == 1, scf_err == 0) |> transmute(cik, bigR, mag = dNI_raw,  is_grp = 1L))
mods <- list(); labs <- c()
for (t in c(.001, .005, .01, .02)) {
  s <- stacked |> mutate(high = as.integer(mag > t))
  mods <- c(mods, list(fit(bigR ~ is_grp * high, s, "lpm")))
  labs <- c(labs, paste0(">", t*100, "%"))
}
tab(mods, labs, c("(Intercept)","is_grp","high","is_grp:high"))
cat("\n  is_grp      = level gap, I/S vs SCF, within LOW magnitude\n")
cat("  high        = magnitude effect WITHIN the SCF group\n")
cat("  is_grp:high = the DiD, how much MORE magnitude matters for the I/S\n")
cat("\n  implied magnitude effects (probability points):\n")
for (i in seq_along(mods)) {
  b <- mods[[i]]$b
  cat(sprintf("    %-7s  SCF %5.1f pp   I/S %5.1f pp   difference %5.1f pp\n",
      labs[i], 100*b["high"], 100*(b["high"]+b["is_grp:high"]), 100*b["is_grp:high"]))
}

cat("\n\n==========================================================================\n")
cat("PANEL D. Same, SOLE-failure groups only (AA cited nothing else)\n")
cat("==========================================================================\n")
st2 <- bind_rows(
  d |> filter(scf_only == 1, is_err == 0) |> transmute(cik, bigR, mag = dCFO_raw, is_grp = 0L),
  d |> filter(is_only == 1, scf_err == 0) |> transmute(cik, bigR, mag = dNI_raw,  is_grp = 1L))
cat("  n SCF-only =", sum(st2$is_grp==0), " n I/S-only =", sum(st2$is_grp==1), "\n")
m2 <- list(); l2 <- c()
for (t in c(.005, .01)) {
  s <- st2 |> mutate(high = as.integer(mag > t))
  m2 <- c(m2, list(fit(bigR ~ is_grp * high, s, "lpm"))); l2 <- c(l2, paste0(">", t*100, "%"))
}
tab(m2, l2, c("(Intercept)","is_grp","high","is_grp:high"))

cat("\n\n==========================================================================\n")
cat("PANEL E. Raw Big R rates (no model), for reference\n")
cat("==========================================================================\n")
print(d |> mutate(grp = case_when(
    scf_only==1 ~ "SCF error ONLY", scf_err==1 ~ "SCF + other",
    is_only==1  ~ "I/S error ONLY", is_err==1  ~ "I/S + other",
    TRUE ~ "neither SCF nor I/S")) |>
  group_by(grp) |> summarise(n=n(), pct_bigR=round(100*mean(bigR),1), .groups="drop") |>
  arrange(desc(pct_bigR)) |> as.data.frame())
