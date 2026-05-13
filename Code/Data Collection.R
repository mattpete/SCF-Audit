#Set-up----------------------------------------------------------
library(dplyr)
library(RPostgres)
library(DBI)
library(dbplyr)
library(glue)
library(arrow)
library(haven)
library(stringr)
library(httr)
library(rlang)
library(readr)
library(readxl)
library(writexl)
library(flextable)
library(officer)
library(ggplot2)
library(tidyverse)


#Set data path and load functions
data_path <- "C:/Users/mnp4/Dropbox/Cash Flow Audit Idea/Data/"


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

data |> group_by(cik,file_date) |> summarise(obs = n()) |> filter(obs > 1) 

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


#Look at Cash Flow Restatements ------------------------------------------------
# Split Accounting Rule failures into separate rows and count per error type
misstate_errors <- data |> 
       filter(!is.na(acct_failures) & acct_failures != "") |>
       mutate(acct_failures = str_replace_all(acct_failures, "\\s*\\|\\s*", "|")) |>
       tidyr::separate_rows(acct_failures, sep = "\\|") |>
       mutate(acct_failures = str_trim(acct_failures)) |>
       filter(acct_failures != "") |>
       mutate(misstatement_type = case_when(!is.na(date_8k_402) & restate == 1 ~ "restate",
                                            is.na(date_8k_402) & restate == 0 & restatement_key == 0 ~ "adjust",TRUE ~ "revision"))


# Export unique error names for manual labeling - Manually bucketing similar categories to simplify table in next step
#unique_errors <- misstate_errors |>
#       group_by(acct_failures) |>
#       summarise(obs=n())

#writexl::write_xlsx(unique_errors, path = glue("{data_path}error_labels_to_map.xlsx"))

# Load mapping file with clean_label
error_labels <- readxl::read_excel(glue("{data_path}error_labels_to_map.xlsx"))

# Join clean_label to misstate_errors
misstate_errors <- misstate_errors |> left_join(error_labels |> select(-obs), by = "acct_failures")

# Summarise by clean_label
misstate_error_counts <- misstate_errors |>
       group_by(clean_label) |>
       summarise(misstate_count = n(), unique_rest = n_distinct(restatement_key)) |>
       arrange(desc(misstate_count))

by_error <- misstate_errors |>
       group_by(clean_label) |>
       summarise(total = n(),
              restate = sum(misstatement_type == "restate"),
              revision = sum(misstatement_type == "revision"),
              adjust = sum(misstatement_type == "adjust"),
              .groups = "drop" ) |>
       mutate(restate_percent = restate / total,
              revision_percent = revision / total,
              adjust_percent = adjust / total )

# Test each error type's restatement (BigR) rate against the overall rate
overall_restate_rate <- sum(by_error$restate) / sum(by_error$total)


#Format values
by_error2 <- by_error |>
  mutate(z_stat = round((restate_percent - overall_restate_rate) / sqrt(overall_restate_rate * (1 - overall_restate_rate) / total), 3),
         p_value = round(2 * pnorm(-abs(z_stat)), 3)) |>
  arrange(z_stat) |> 
  mutate(total = formatC(total, format = "d", big.mark = ","),
         restate = formatC(restate, format = "d", big.mark = ","),
         revision = formatC(revision, format = "d", big.mark = ","),
         adjust = formatC(adjust, format = "d", big.mark = ","),
         restate_percent = paste0(formatC(restate_percent * 100, format = "f", digits = 1), "%"),
         revision_percent = paste0(formatC(revision_percent * 100, format = "f", digits = 1), "%"),
         adjust_percent = paste0(formatC(adjust_percent * 100, format = "f", digits = 1), "%"))

# Export by_error table to Word with custom column names
by_error_export <- by_error2 |>
  rename("Error Type" = clean_label,
              "Total" = total,
              "Restate (#)" = restate,
              "Revision (#)" = revision,
              "Adjust (#)" = adjust,
              "Restate (%)" = restate_percent,
              "Revision (%)" = revision_percent,
              "Adjust (%)" = adjust_percent,
              "Test Statistic" = z_stat,
              "P-Value" = p_value) 


ft <- flextable(by_error_export)
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = glue("{data_path}by_error_table.docx"))



#New section-----------------------------------------------------------------------------

# Count cash-flow related misstatements by year (using year misstatement was revealed)
cf_year <- misstate_errors |>
       filter(clean_label == "Cash flow statement") |>
       distinct(restatement_key, misstate_ann_year, misstatement_type) |>
       group_by(misstate_ann_year) |>
       summarise(
              total_misstatements = n(),
              restatements = sum(misstatement_type == "restate"),
              revisions = sum(misstatement_type == "revision"),
              adjustments = sum(misstatement_type == "adjust"),
              .groups = "drop") |>
       arrange(misstate_ann_year)

print(cf_year)

# Percentage of all misstatements that are cash flow errors by year
cf_per_year <- misstate_errors |>
       filter(clean_label == "Cash flow statement") |>
       distinct(restatement_key, misstate_ann_year) |>
       group_by(misstate_ann_year) |>
       summarise(cf_unique = n_distinct(restatement_key), .groups = "drop") |>
       arrange(misstate_ann_year)

total_per_year <- misstate_errors |>
       distinct(restatement_key, misstate_ann_year) |>
       group_by(misstate_ann_year) |>
       summarise(total_unique = n_distinct(restatement_key), .groups = "drop") |>
       arrange(misstate_ann_year)

cf_pct <- total_per_year |>
       left_join(cf_per_year, by = "misstate_ann_year") |>
       mutate(cf_unique = coalesce(cf_unique, 0L),
                             pct_cashflow = cf_unique / total_unique * 100) |>
       arrange(misstate_ann_year)

print(cf_pct)

# Plot percent cash-flow misstatements by year
misstate_cf_pct_plot <- cf_pct |>
       ggplot(aes(x = misstate_ann_year, y = pct_cashflow)) +
       geom_line() +
       geom_point() +
       scale_y_continuous(labels = scales::percent_format(scale = 1)) +
       labs(x = "Year", y = "Percentage of Misstatements with Cash Flow Errors", title = "Percentage of Misstatements with Cash Flow Errors by Year") +
       theme_minimal()

print(misstate_cf_pct_plot)

# Build GAAP error counts per year (used for ranking tables)
gaap_errors_year <- restate_errors |>
  distinct(restatement_key, restate_year, acct_failures) |>
  group_by(restate_year, acct_failures) |>
  summarise(error_count = n_distinct(restatement_key), .groups = "drop") |>
  left_join(total_per_year, by = "restate_year") |>
  mutate(pct_of_year = if_else(total_unique > 0, error_count / total_unique * 100, 0)) |>
  arrange(restate_year, desc(error_count))

# --- BigR-only and non-BigR-only versions of the per-year top-10 table ---
# Totals per year restricted to BigR / non-BigR
total_per_year_bigR <- misstate3 |>
       distinct(restatement_key, restate_year, bigR) |>
       filter(bigR == 1L) |>
       group_by(restate_year) |>
       summarise(total_unique = n_distinct(restatement_key), .groups = "drop") |>
       arrange(restate_year)

total_per_year_nonbigR <- misstate3 |>
       distinct(restatement_key, restate_year, bigR) |>
       filter(bigR == 0L) |>
       group_by(restate_year) |>
       summarise(total_unique = n_distinct(restatement_key), .groups = "drop") |>
       arrange(restate_year)

# Errors counts for BigR only
gaap_errors_year_bigR <- restate_errors |>
       filter(bigR == 1L) |>
       distinct(restatement_key, restate_year, acct_failures) |>
       group_by(restate_year, acct_failures) |>
       summarise(error_count = n_distinct(restatement_key), .groups = "drop") |>
       left_join(total_per_year_bigR, by = "restate_year") |>
       mutate(pct_of_year = if_else(total_unique > 0, error_count / total_unique * 100, 0)) |>
       arrange(restate_year, desc(error_count))

# Errors counts for non-BigR only
gaap_errors_year_nonbigR <- restate_errors |>
       filter(bigR == 0L) |>
       distinct(restatement_key, restate_year, acct_failures) |>
       group_by(restate_year, acct_failures) |>
       summarise(error_count = n_distinct(restatement_key), .groups = "drop") |>
       left_join(total_per_year_nonbigR, by = "restate_year") |>
       mutate(pct_of_year = if_else(total_unique > 0, error_count / total_unique * 100, 0)) |>
       arrange(restate_year, desc(error_count))

# Helper function to build year-columns wide table for a given gaap_errors_year df and totals vector
build_yearcols <- function(gae_df, totals_df){
       yrs <- sort(unique(gae_df$restate_year))
       cols <- lapply(yrs, function(y){
              tmp <- gae_df |>
                     filter(restate_year == y) |>
                     arrange(desc(pct_of_year)) |>
                     slice_head(n = 10) |>
                     mutate(cell = paste0(acct_failures, " (", round(pct_of_year, 1), "% )")) |>
                     pull(cell)
              if(length(tmp) < 10) tmp <- c(tmp, rep(NA_character_, 10 - length(tmp)))
              tmp
       })
       df <- as.data.frame(do.call(cbind, cols), stringsAsFactors = FALSE)
       colnames(df) <- as.character(yrs)
       rownames(df) <- paste0("Rank ", seq_len(nrow(df)))
       df[is.na(df)] <- ""
       # append totals row
       totals_vec <- vapply(as.character(yrs), function(y){
              v <- totals_df |>
                     filter(restate_year == as.integer(y)) |>
                     pull(total_unique)
              if(length(v) == 0) v <- 0L
              as.character(v)
       }, FUN.VALUE = character(1))
       df <- rbind(df, setNames(as.list(totals_vec), names(df)))
       rownames(df)[nrow(df)] <- "Total Restatements"
       df
}

gaap_top10_yearcols_bigR <- build_yearcols(gaap_errors_year_bigR, total_per_year_bigR)
gaap_top10_yearcols_nonbigR <- build_yearcols(gaap_errors_year_nonbigR, total_per_year_nonbigR)

print(gaap_top10_yearcols_bigR)
print(gaap_top10_yearcols_nonbigR)

# Rank tables: rows = overall top-10 errors, columns = years, cells = rank of that error in that year (1 = highest pct)
build_rank_table <- function(gae_df, top_errors){
       yrs <- sort(unique(gae_df$restate_year))
       # compute ranks per year
       ranks_list <- lapply(yrs, function(y){
              dfy <- gae_df |>
                     filter(restate_year == y) |>
                     arrange(desc(pct_of_year)) |>
                     mutate(rank = row_number()) |>
                     select(acct_failures, rank)
              # return named vector mapping error -> rank
              setNames(as.list(dfy$rank), dfy$acct_failures)
       })
       names(ranks_list) <- as.character(yrs)

       # Build output frame with first column Error
       out <- data.frame(Error = top_errors, stringsAsFactors = FALSE)
       for(y in names(ranks_list)){
              ranks_map <- ranks_list[[y]]
              out[[y]] <- vapply(out$Error, function(e){
                     r <- ranks_map[[e]]
                     if(is.null(r)) return("") else return(as.character(r))
              }, FUN.VALUE = "")
       }
       out
}

# Ensure we have the overall top-10 errors list
if(!exists("top10_errors_overall")){
       top10_errors_overall <- gaap_errors_year |>
              group_by(acct_failures) |>
              summarise(total = sum(error_count), .groups = "drop") |>
              arrange(desc(total)) |>
              slice_head(n = 10) |>
              pull(acct_failures)
}

gaap_top10_ranktable <- build_rank_table(gaap_errors_year, top10_errors_overall)
gaap_top10_ranktable_bigR <- build_rank_table(gaap_errors_year_bigR, top10_errors_overall)
gaap_top10_ranktable_nonbigR <- build_rank_table(gaap_errors_year_nonbigR, top10_errors_overall)

print(gaap_top10_ranktable)
print(gaap_top10_ranktable_bigR)
print(gaap_top10_ranktable_nonbigR)


#OLS Regression---------------------------

library(fixest)
summary(lm(bigR ~ cfr*(big4 + material + rev_rec + fraud  + icmw  + neg_effect + SEC + num_failures) , data))

summary(feols(bigR ~ cfr*(big4 + material + rev_rec + fraud  + icmw  + neg_effect + SEC + num_failures) |cik+restate_year, data))

