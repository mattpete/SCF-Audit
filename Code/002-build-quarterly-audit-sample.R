# 1. Setup -----------------------------------------------------------------------
library(dplyr)
library(glue)
library(arrow)
library(DBI)
library(RPostgres)
library(dbplyr)
library(stringr)
library(lubridate)
library(readxl)
library(tidyr)

#define data paths and load functions
raw_data_path <- Sys.getenv('XBRL_DATA_PATH')
data_path     <- Sys.getenv('DATA_PATH')
source("code/000-utilities-functions.R")

#Start with Restatements-------------------------------------------------
#Downloaded this csv dataset from Audit Analytics

misstate <- readxl::read_excel(glue("{data_path}restatement-data-031425.xlsx"))
adjust <- readxl::read_excel(glue("{data_path}adjustment-data-021626.xlsx"))


#Make restatement characteristics------------------------------------------------------

misstate2 <- misstate |> 
  rename(restatement_key = `Restatement Key`,
         cik = `CIK Code`, ticker = Ticker, com_name = Company, filing_time = `Disclosure Accepted`, 
         misstate_begin = `Restated Period Begin`, misstate_end = `Restated Period Ended`,
         acct_failures = `Accounting Rule (GAAP/FASB) Application Failures`, date_8k_402 = `Date of 8-K Item 4.02`) |> 
  mutate(cusip = sub('^\\s*="(.*)"$', '\\1', CUSIP),
         cik = str_pad(cik, width = 10, pad = "0"), 
         big4 = coalesce(if_else(str_detect(`Auditor - During Restated Period`,"Ernst|KPMG|Pricewaterhouse|Deloitte"),1,0),0L),
         salient_ann = if_else(str_detect(Disclosure,"Press Release"),1,0),
         neg_effect = if_else(Effect=="Negative",1,0),
         chg_ni = coalesce(`Cumulative Change in Net Income`,0L),
         materiality = coalesce(chg_ni/coalesce(`H - Assets ($)`,`MR - Assets ($)`),0L),
         material = if_else(abs(materiality)>=0.05,1,0),
         rev_rec = coalesce(if_else(str_detect(acct_failures,"Revenue recognition"),1,0),0L),
         cfr = if_else(str_detect(str_to_lower(acct_failures),"cash flow statement"),1,0),
         icmw = coalesce(if_else(str_detect(`Other Significant Issues`,"Material Weakness"),1,0),0L),
         SEC = coalesce(if_else(`SEC Investigation`=="Y",1,0),0L),
         aud_involve = coalesce(if_else(`Auditor Letter - Discussion`=="Y",1,0),0L),
         board_involve = coalesce(if_else(`Board Involvement`=="Y",1,0),0L),
         fraud = if_else(!is.na(`Financial Fraud, Irregularities and Misrepresentations`),1,0),
         restate = if_else(!is.na(date_8k_402),1,0),
         file_date = date(filing_time),
         misstate_ann_year = year(file_date),
         num_failures = sapply(str_split(replace_na(acct_failures, ""), "\\|"), function(x) sum(x != ""))) |> 
  filter(year(file_date)>=2007, year(file_date)<2025) |> 
  select(restatement_key,cik,ticker,cusip,com_name,file_date,filing_time,misstate_begin,misstate_end,
         big4,salient_ann,neg_effect,chg_ni,cfr,num_failures, materiality,material, rev_rec,icmw,SEC,
         aud_involve,board_involve,fraud,date_8k_402,restate,misstate_ann_year,acct_failures)


#Check dups by restatement - none
misstate2 |> group_by(restatement_key) |> summarise(obs = n()) |> filter(obs > 1)

misstate2 |> group_by(cik,file_date) |> summarise(obs = n(),restate = sum(restate)) |> filter(obs > 1) 

#For duplicates: keep those if restate
misstate3 <- misstate2 |> 
  group_by(cik, file_date) |> 
  #For dups keep ones that are restate and with biggest materiality
  arrange(desc(restate), desc(materiality)) |>  
  #Keep top one - mimics what proc sort nodupkey does in sql
  slice(1) |> 
  ungroup()

#Check dups again by firm-date - none
misstate3 |> group_by(cik,file_date) |> summarise(obs = n(),restater = sum(restate)) |> filter(obs > 1) 


adjust2 <- adjust |> 
  rename(cik = `CIK Code`, ticker = Ticker, com_name = Company, filing_time = `Disclosure Accepted`, 
         misstate_begin = `Adjustment Period Begin`, misstate_end = `Adjustment Period Ended`,
         acct_failures = `Accounting Rule (GAAP/FASB) Application Failures`) |> 
  mutate(cusip = sub('^\\s*="(.*)"$', '\\1', CUSIP),
         cik = str_pad(cik, width = 10, pad = "0"), 
         big4 = coalesce(if_else(str_detect(`Auditor - During Restated Period`,"Ernst|KPMG|Pricewaterhouse|Deloitte"),1,0),0L),
         salient_ann = if_else(str_detect(Disclosure,"Press Release"),1,0),
         neg_effect = if_else(Effect=="Negative",1,0),
         chg_ni = coalesce(`Cumulative Change in Net Income`,0L),
         materiality = coalesce(chg_ni/coalesce(`H - Assets ($)`,`MR - Assets ($)`),0L),
         material = if_else(abs(materiality)>=0.05,1,0),
         rev_rec = coalesce(if_else(str_detect(acct_failures,"Revenue recognition"),1,0),0L),
         cfr = if_else(str_detect(str_to_lower(acct_failures),"cash flow statement"),1,0),
         icmw = coalesce(if_else(str_detect(`Other Significant Issues`,"Material Weakness"),1,0),0L),
         SEC = coalesce(if_else(`SEC Investigation`=="Y",1,0),0L),
         aud_involve = coalesce(if_else(`Auditor Letter - Discussion`=="Y",1,0),0L),
         board_involve = coalesce(if_else(`Board Involvement`=="Y",1,0),0L),
         fraud = if_else(!is.na(`Financial Fraud, Irregularities and Misrepresentations`),1,0),
         file_date = date(filing_time),
         misstate_ann_year = year(file_date),
         num_failures = sapply(str_split(replace_na(acct_failures, ""), "\\|"), function(x) sum(x != ""))) |> 
  filter(year(file_date)>=2007, year(file_date)<2025) |> 
  select(cik,ticker,cusip,com_name,file_date,filing_time,misstate_begin,misstate_end,
         big4,salient_ann,neg_effect,chg_ni,cfr,num_failures, materiality,material, rev_rec,icmw,SEC,
         aud_involve,board_involve,fraud,misstate_ann_year,acct_failures)


#Check dups by firm-date - 28
adjust2 |> group_by(cik,file_date) |> summarise(obs = n()) |> filter(obs > 1) 

#For duplicates: keep those that are more material
adjust3 <- adjust2 |> 
  group_by(cik, file_date) |> 
  #For dups keep ones with biggest materiality
  arrange(desc(materiality)) |>  
  #Keep top one - mimics what proc sort nodupkey does in sql
  slice(1) |> 
  ungroup()

#Check dups again by firm-date - none
adjust3 |> group_by(cik,file_date) |> summarise(obs = n()) |> filter(obs > 1) 

#Combining Restatement and Adjustment data--------------------------------------

adjust4 <- adjust3 |> 
  mutate(restatement_key = as.numeric(00000),date_8k_402 = as.POSIXct(NA), restate=0) 

data_combined <- bind_rows(misstate3, adjust4)

# Remove duplicates by cik and file_date, keeping restatement/revision over adjustment
data <- data_combined |>
  mutate(adjust_flag = if_else(restate == 0, 1, 0)) |>
  arrange(cik, file_date, adjust_flag) |>
  group_by(cik, file_date) |>
  slice(1) |>
  ungroup() |>
  select(-adjust_flag)

data |> group_by(cik, file_date) |> summarise(obs = n()) |> filter(obs > 1)

citi_aa <- data |> filter(cik=="0000831001")
wmt_aa <- data |> filter(cik=="0000104169")
msft_aa <- data |> filter(cik=="0000789019")


# 2. Load and prepare XBRL cash flow data ----------------------------------------
# Filter to 10-Q and 10-K so quarterly period-ends match Compustat datadate
xbrl_scf <- read_parquet(glue("{data_path}xbrl_all_tsv.parquet")) |>
  #filter(filing_form %in% c("10-Q", "10-K")) |>
  mutate(end = as.Date(end))

wmt_xbrl <- xbrl_scf |> filter(cik=="0000104169") |>  select(cik,filing_accn,filing_date,filing_form,ni_original_val,ni_best_val,ni_change)
wmt_xbrl <- xbrl_scf |> filter(cik=="0000104169") |>  select(cik,filing_accn,filing_date,filing_form,end,cfo_original_val,cfo_best_val,cfo_change,
                                                             cfi_original_val,cfi_best_val,cfi_change,cff_original_val,cff_best_val,cff_change)

citi_xbrl <- xbrl_scf |> filter(cik=="0000831001") |> select(cik,filing_accn,filing_date,filing_form,ni_original_val,ni_best_val,ni_change)
citi_xbrl <- xbrl_scf |> filter(cik=="0000831001") |>  select(cik,filing_accn,filing_date,filing_form,end,cfo_original_val,cfo_best_val,cfo_change,
                                                             cfi_original_val,cfi_best_val,cfi_change,cff_original_val,cff_best_val,cff_change)

msft_xbrl <- xbrl_scf |> filter(cik=="0000789019") |>  select(cik,filing_accn,filing_date,filing_form,end,cfo_original_val,cfo_best_val,cfo_change,
                                                              cfi_original_val,cfi_best_val,cfi_change,cff_original_val,cff_best_val,cff_change)


sbux_xbrl <- xbrl_scf |> filter(cik=="0000829224") |>  select(cik,filing_accn,filing_date,filing_form,end,cfo_original_val,cfo_best_val,cfo_change,
                                                              cfi_original_val,cfi_best_val,cfi_change,cff_original_val,cff_best_val,cff_change)

#Test merge---------------------------------
test_rest <- data |> filter(restatement_key=="48887")
test_xbrl <- xbrl_scf |> filter(cik=="0000003116") |>  select(cik,filing_accn,filing_date,filing_form,end,cfo_original_val,cfo_best_val,cfo_change,
                                                              cfi_original_val,cfi_best_val,cfi_change,cff_original_val,cff_best_val,cff_change)

test_aa_xbrl <- test_rest |>
  left_join(xbrl_scf, by = join_by(cik, between(y$end, x$misstate_begin, x$misstate_end))) |>
  mutate(has_xbrl_match = if_else(!is.na(end), 1L, 0L))

test_aa_xbrl_collapsed <- test_aa_xbrl |>
  filter(has_xbrl_match == 1L) |>
  group_by(cik, file_date, fy) |>
  slice_max(end, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(cik, file_date) |>
  summarise(xbrl_ni_change = sum(ni_change, na.rm = TRUE), .groups = "drop")





# 4. AA-anchored misstatement dataset --------------------------------------------

# Start with every Audit Analytics restatement and left join XBRL periods whose period-end date falls within the restated window.
aa_xbrl <- data |>
  left_join(xbrl_scf, by = join_by(cik, between(y$end, x$misstate_begin, x$misstate_end))) |>
  mutate(has_xbrl_match = if_else(!is.na(end), 1L, 0L))

# Check duplicates
aa_xbrl |> group_by(cik,restatement_key) |> summarise(obs=n()) |> filter(obs>1)

# Summary: coverage check
aa_xbrl |>
  summarise(n_aa_restatements  = n_distinct(paste(cik, misstate_begin, misstate_end)),
            n_with_xbrl        = n_distinct(paste(cik, misstate_begin, misstate_end)[has_xbrl_match == 1L]),
            pct_with_xbrl      = n_with_xbrl / n_aa_restatements)

#write_parquet(aa_xbrl, glue("{data_path}misstatements_aa_anchored.parquet"))

akorn <- aa_xbrl |> filter(cik=="0000002034")



# 4.1 Collapse to one row per restatement with cumulative XBRL changes -------------
# CFO/CFI/CFF/NI are YTD within a fiscal year, so first keep only the latest
# period-end per fiscal year (avoids double-counting Q1+Q2+Q3 when a 10-K is available).
# Then sum across fiscal years covered by the restatement window.
# EQ, Assets, Liab are point-in-time so the same dedup + sum gives the net change
# across all restated period-ends.

aa_xbrl_collapsed <- aa_xbrl |>
  filter(has_xbrl_match == 1L) |>
  group_by(cik, file_date, fy) |>
  slice_max(end, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(cik, file_date) |>
  summarise(
    n_xbrl_periods  = n(),
    n_xbrl_fy       = n_distinct(fy),
    xbrl_ni_change  = sum(ni_change,  na.rm = TRUE),
    xbrl_eq_change  = sum(eq_change,  na.rm = TRUE),
    xbrl_cfo_change = sum(cfo_change, na.rm = TRUE),
    xbrl_cfi_change = sum(cfi_change, na.rm = TRUE),
    xbrl_cff_change = sum(cff_change, na.rm = TRUE),
    .groups = "drop")

# Rejoin collapsed XBRL changes back to AA dataset (one row per restatement)
# Limit to restatements whose restated period begins after 2010 (XBRL effective date)
aa_misstatements <- data |>
  filter(year(misstate_begin) > 2010) |>
  left_join(aa_xbrl_collapsed, by = c("cik", "file_date")) |>
  mutate(has_xbrl_match = if_else(!is.na(n_xbrl_fy), 1L, 0L),
         across(starts_with("xbrl_"), ~ replace_na(.x, 0)))

# Validation
cat("Rows (should equal nrow(data)):", nrow(aa_misstatements), "\n")
cat("Duplicates:", nrow(aa_misstatements |> group_by(cik, file_date) |> filter(n() > 1)), "\n")
cat("With XBRL match:", sum(aa_misstatements$has_xbrl_match), "\n")
cat("Without XBRL match:", sum(1L - aa_misstatements$has_xbrl_match), "\n")

# Compare XBRL CFO change to AA cumulative NI change as a sanity check
print(aa_misstatements |>
  filter(has_xbrl_match == 1L, !is.na(chg_ni)) |>
  summarise(n             = n(),
            xbrl_cfo_mean = mean(xbrl_cfo_change, na.rm = TRUE),
            chg_ni_mean   = mean(as.numeric(chg_ni), na.rm = TRUE),
            cor_cfo_ni    = cor(xbrl_cfo_change, as.numeric(chg_ni), use = "complete.obs")))

#write_parquet(aa_misstatements, glue("{data_path}misstatements_aa_collapsed.parquet"))


#Validation----------------------

#55622 Restatement Key (Apache, Inc CIK = 0000006769)
test_rest <- data |> filter(cik=="0000006769")
test_xbrl <- xbrl_scf |> filter(cik=="0000006769")
