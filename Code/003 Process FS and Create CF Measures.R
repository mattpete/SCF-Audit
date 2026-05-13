# Setup - Startup -----------------------------------------------------------------

library(edgar)
library(dplyr)
library(stringr)
library(httr)
library(XML)
library(rvest)
library(stringr)
library(stringdist)
library(arrow)
library(tidyverse)


#Define file paths 
raw_data_path <- 'D:/XBRL/XBRL FS Data/'
output_path <- 'D:/XBRL/XBRL SCF Summary Files/'


#Load functions ----------------------------------------------

#Creating a function that checks whether any combination of positive and negative values equals the subtotal
#Necessary because XBRL has A LOT of data entry error (wrong sign)
#So this function allows me to test whether I have all the section lines without having to fix all data entry errors
check_footing <- function(cf_section_lines, section_total) {
  
  #First check if current lines and signs tabulate correctly
  if(sum(cf_section_lines)== section_total){
    return(TRUE)
  }
  #If not try combinations of all signs and sums
  else{
    #Check if positive/negative combo matrix will be too big
    if(length(cf_section_lines) > 20){
      return("TOO BIG")
    }
    #If not too big, then do this in memory
    else{
      #Create positive/negative combo matrix
      binary_rep_matrix <- as.matrix(expand.grid(rep(list(c(-1, 1)), length(cf_section_lines))))
      
      #Multiply section line values by positive/negative combo matrix
      combinations <- t(t(binary_rep_matrix) * cf_section_lines)
      
      #Now test if the sum of ANY combination equals the section total
      output <- any(rowSums(combinations) == section_total)
      return(output)
    }
  }
  #Clean up
  rm(binary_rep_matrix)
  rm(combinations)
  gc()
}


#Looping through year-period (Quarter or Month) XBRL files to create disaggregation measures-------------------

#Create listing of XBRL files to search. Quarterly from 2009 Q1 - 2020 Q3. Monthly afterwards.
#Naming of files correspond to the years and periods in 'search' dataframe.

search <- tribble(~Year,~Period,
                  #"2009","Q1","2009","Q2","2009","Q3","2009","Q4",
                  #"2010","Q1","2010","Q2","2010","Q3","2010","Q4",
                  "2011","Q1","2011","Q2","2011","Q3","2011","Q4",
                  "2012","Q1","2012","Q2","2012","Q3","2012","Q4",
                  "2013","Q1","2013","Q2","2013","Q3","2013","Q4",
                  "2014","Q1","2014","Q2","2014","Q3","2014","Q4",
                  "2015","Q1","2015","Q2","2015","Q3","2015","Q4",
                  "2016","Q1","2016","Q2","2016","Q3","2016","Q4",
                  "2017","Q1","2017","Q2","2017","Q3","2017","Q4",
                  "2018","Q1","2018","Q2","2018","Q3","2018","Q4",
                  "2019","Q1","2019","Q2","2019","Q3","2019","Q4",
                  "2020","Q1","2020","Q2","2020","Q3","2020","M10","2020","M11","2020","M12",
                  "2021","M01","2021","M02","2021","M03","2021","M04","2021","M05","2021","M06","2021","M07","2021","M08","2021","M09","2021","M10","2021","M11","2021","M12",
                  "2022","M01","2022","M02","2022","M03","2022","M04","2022","M05","2022","M06","2022","M07","2022","M08","2022","M09","2022","M10","2022","M11","2022","M12",
                  "2023","M01","2023","M02","2023","M03","2023","M04","2023","M05","2023","M06","2023","M07","2023","M08","2023","M09","2023","M10","2023","M11","2023","M12")



#Test search 
search <- tribble(~Year,~Period,
                  "2014","Q1","2014","Q2")



#For loop iterates through periodic XBRL FS files, creates disaggregation measures, and saves new periodic tidy data set

#Creating overall summary dataset to hold overall accuracy checks
summary <- data.frame(period = character(),
                      year = integer(),
                      ocf_acc = numeric(),
                      icf_acc = numeric(),
                      fcf_acc = numeric())

#Loop
for(i in 1:nrow(search)){
  
  #Look through search table for each period which corresponds to naming of zip files downloaded
  year <- search[[i,1]]
  period <- search[[i,2]]
  
  #Read in FS XBRL files
  fs <- read_parquet(paste0(raw_data_path,year,"_",period,"_FS.parquet"))
  
  #Create statement of cash flow dataset which will be used to create summary measures
  cf_data1 <- fs %>%
    filter(CF==1) %>% 
    #Pre file doesn't have period length of tag so if different period lengths for same tag it causes dups. Requiring highest qtr within tag gets correct period and no dups.
    group_by(adsh,cik,filed,ddate,tag) %>% 
    filter(qtrs == max(qtrs))%>%
    ungroup()%>%
    #Now group by firm, filing, and date to make variables of interest within each observation
    group_by(adsh,cik,form,filed,ddate) %>% 
    mutate(
      #Identify Income Line (sometimes adjustments to cont ops so taking minimum line - will drop the subtotals)
      inc_line = coalesce(min(line[tag %in% c("NetIncomeLoss",
                                              "ProfitLoss",
                                              "IncomeLossFromContinuingOperationsIncludingPortionAttributableToNoncontrollingInterest",
                                              "NetIncomeLossAvailableToCommonStockholdersBasic", "IncomeLossFromContinuingOperations")],na.rm = T), 0L),
      #Now identify extra income lines to drop to make footing work
      inc_line_drop = if_else(tag %in% c("NetIncomeLoss",
                                         "ProfitLoss",
                                         "IncomeLossFromContinuingOperationsIncludingPortionAttributableToNoncontrollingInterest",
                                         "NetIncomeLossAvailableToCommonStockholdersBasic",
                                         "IncomeLossFromContinuingOperations") & line != inc_line, "DROP","KEEP"),
      #Identify each section line number 
      ocf_line = if_else(any(tag=="NetCashProvidedByUsedInOperatingActivities"), first(line[tag=="NetCashProvidedByUsedInOperatingActivities"]), NA),
      icf_line = if_else(any(tag=="NetCashProvidedByUsedInInvestingActivities"), first(line[tag=="NetCashProvidedByUsedInInvestingActivities"]), NA),
      fcf_line = if_else(any(tag=="NetCashProvidedByUsedInFinancingActivities"), first(line[tag=="NetCashProvidedByUsedInFinancingActivities"]), NA),
      #Sometimes ICF and FCF are in different order - section classification depends on order
      order = if_else((ocf_line < icf_line & icf_line < fcf_line),"OIF",
                      if_else((ocf_line< fcf_line & fcf_line < icf_line),"OFI",
                              if_else((ocf_line< fcf_line & fcf_line < icf_line),"OFI","Other"))),
      Section = if_else(order == "OIF",if_else(line>inc_line & line<ocf_line,"Operating",
                                               if_else(line>ocf_line&line<icf_line,"Investing",
                                                       if_else(line>icf_line&line<fcf_line,"Financing","None"))),
                        if_else(line>inc_line & line<ocf_line,"Operating",
                                if_else(line>ocf_line&line<fcf_line,"Financing",
                                        if_else(line>fcf_line&line<icf_line,"Investing","None"))),"None"),
      value = if_else(!is.na(crdr) & Section != "None",if_else(negating == 1,-value, value),value),
      custom = if_else(str_detect(version,"us-gaap"),0,1)) %>%
    #Filter out tags that cause footing issues: missing values (duplicates), subtotals (cont ops vs discont)
    filter(!is.na(value),
           inc_line_drop == "KEEP",
           tag != "NetCashProvidedByUsedInOperatingActivitiesContinuingOperations",
           tag != "AdjustmentsToReconcileNetIncomeLossToCashProvidedByUsedInOperatingActivities",
           tag != "NetCashProvidedByUsedInInvestingActivitiesContinuingOperations",
           tag != "NetCashProvidedByUsedInFinancingActivitiesContinuingOperations")%>%
    ungroup()
  
  
  #Create Cash Flow summary measures
  cf_data2 <- cf_data1  %>%
    group_by(adsh,cik,form,filed,ddate) %>% 
      #Now create summary measures for each statement of cash flow
      summarise(
        ocf_tag = if_else(any(tag=="NetCashProvidedByUsedInOperatingActivities"), first(value[tag=="NetCashProvidedByUsedInOperatingActivities"]), NA),
        icf_tag = if_else(any(tag=="NetCashProvidedByUsedInInvestingActivities"), first(value[tag=="NetCashProvidedByUsedInInvestingActivities"]), NA),
        fcf_tag = if_else(any(tag=="NetCashProvidedByUsedInFinancingActivities"), first(value[tag=="NetCashProvidedByUsedInFinancingActivities"]), NA),
        #Create counts of lines (tags) and custom tags
        ocf_lines = n_distinct(if_else(Section=="Operating",tag,NA)),
        icf_lines = n_distinct(if_else(Section=="Investing",tag,NA)),
        fcf_lines = n_distinct(if_else(Section=="Financing",tag,NA)),
        total_lines = ocf_lines + icf_lines + fcf_lines,
        custom_count = sum(custom),
        #Other measures
        ocf_chgwc_lines = n_distinct(if_else(grepl("IncreaseDecrease",tag),tag,NA)),
        ocf_noncash_lines = ocf_lines - ocf_chgwc_lines,
        other_noncash = abs(sum(case_when(tag %in% c("OtherNoncashIncomeExpense","OtherNoncashExpense") ~ value, TRUE ~ 0)))/abs(ocf_tag),
        other_chg_wc = abs(sum(case_when(tag %in% c("IncreaseDecreaseInOtherOperatingAssets",
                                                    "IncreaseDecreaseInOtherOperatingLiabilities",
                                                    "IncreaseDecreaseInAccruedLiabilitiesAndOtherOperatingLiabilities",
                                                    "IncreaseDecreaseInOtherOperatingCapitalNet",
                                                    "OtherOperatingActivitiesCashFlowStatement") ~ value, TRUE ~ 0)))/abs(ocf_tag),
        other_icf = abs(sum(case_when(tag %in% c("PaymentsForProceedsFromOtherInvestingActivities","PaymentsToAcquireOtherInvestments") ~ value, TRUE ~ 0)))/abs(ocf_tag),
        other_fcf = abs(sum(case_when(tag %in% c("ProceedsFromPaymentsForOtherFinancingActivities") ~ value, TRUE ~ 0)))/abs(ocf_tag),
        order = unique(order),
        int_paid = if_else(any(tag=="InterestPaid")|any(tag=="InterestPaidNet"),1,0),
        tax_paid = if_else(any(tag=="IncomeTaxesPaid")|any(tag=="IncomeTaxesPaidNet"),1,0),
        .groups = "keep") %>%
      #Filtering out observations where missing each section total tag (mostly due to cash accounts having balances for other periods = error)
      filter(!is.na(ocf_tag) & !is.na(icf_tag) & !is.na(fcf_tag))
  
  gc()
  
  #Display 
  cat(" Created Cash Flow summary dataset for:",year,period,"|")
  

  #Now loop through each filing and check whether each section foots
  #Create empty dataset to hold results
  accuracy <- data.frame(adsh = character(),
                         cik = numeric(),
                         form = character(),
                         filed = character(),
                         ddate = character(),
                         ocf_check = numeric(),
                         icf_check = numeric(),
                         fcf_check = numeric())
  
  #Using custom function called check_footing because there is a lot of data error in XBRL tags (wrong signs)
  for (k in 1:nrow(cf_data2)){
    
    #Select one firm
    ind_firm <- cf_data1 %>% filter(adsh == cf_data2$adsh[k] & cik == cf_data2$cik[k] & filed == cf_data2$filed[k] & ddate == cf_data2$ddate[k])
    
    #Apply footing function
    
    #Operating section check
    ocf_check <- check_footing(ind_firm %>% filter(Section == "Operating" | line == inc_line) %>% pull(value),
                                ind_firm %>% mutate(cf_tag = if_else(any(tag == "NetCashProvidedByUsedInOperatingActivities"), first(value[tag == "NetCashProvidedByUsedInOperatingActivities"]), NA)) %>% pull(cf_tag) %>% unique())
    #Investing section check
    icf_check <- check_footing(ind_firm %>% filter(Section == "Investing") %>% pull(value),
                                ind_firm %>% mutate(cf_tag = if_else(any(tag=="NetCashProvidedByUsedInInvestingActivities"), first(value[tag=="NetCashProvidedByUsedInInvestingActivities"]), NA)) %>% pull(cf_tag) %>% unique())
    
    #Financing section check
    fcf_check <- check_footing(ind_firm %>% filter(Section == "Financing") %>% pull(value),
                                ind_firm %>% mutate(cf_tag = if_else(any(tag=="NetCashProvidedByUsedInFinancingActivities"), first(value[tag=="NetCashProvidedByUsedInFinancingActivities"]), NA)) %>% pull(cf_tag) %>% unique())
    
    #Save output
    accuracy[nrow(accuracy) + 1, ] <- c(cf_data2$adsh[k], cf_data2$cik[k], cf_data2$form[k], cf_data2$filed[k], cf_data2$ddate[k], ocf_check, icf_check, fcf_check)
    
    #Display status
    if(k %% 1000 == 0) {cat(" Processing:",year,period,"checked footing thru",k, "out of", nrow(cf_data2),"|")}
  }
  
  #Summarise overall accuracy for each period
  summary <- rbind(summary, data.frame(period = period, year = year, ocf_acc = mean(accuracy$ocf_check == TRUE), icf_acc = mean(accuracy$icf_check == TRUE), fcf_acc = mean(accuracy$fcf_check == TRUE)))
  
  #Merge individual accuracy stats back into CF statement summary measures
  cf_data3 <- cf_data2%>%
    left_join(accuracy%>%mutate(cik = as.integer(cik),filed = as.integer(filed),ddate = as.integer(ddate)), by = c("adsh","cik","form","filed","ddate"))
              
  #Write to parquet file
  write_parquet(cf_data3,paste0(output_path,year,"_",period,"_CF_Summary.parquet"))
  
  #Display status
  cat(" Processed:",year,period,"|")
  
  #Clean up
  gc()
}



#Combine all individual datasets ----------------------------------------

#Create listing of XBRL files to search. Quarterly from 2009 Q1 - 2020 Q3. Monthly afterwards.
#Naming of files correspond to the years and periods in 'search' dataframe.

search <- tribble(~Year,~Period,
                  #"2009","Q1","2009","Q2","2009","Q3","2009","Q4",
                  #"2010","Q1","2010","Q2","2010","Q3","2010","Q4",
                  "2011","Q1","2011","Q2","2011","Q3","2011","Q4",
                  "2012","Q1","2012","Q2","2012","Q3","2012","Q4",
                  "2013","Q1","2013","Q2","2013","Q3","2013","Q4",
                  "2014","Q1","2014","Q2","2014","Q3","2014","Q4",
                  "2015","Q1","2015","Q2","2015","Q3","2015","Q4",
                  "2016","Q1","2016","Q2","2016","Q3","2016","Q4",
                  "2017","Q1","2017","Q2","2017","Q3","2017","Q4",
                  "2018","Q1","2018","Q2","2018","Q3","2018","Q4",
                  "2019","Q1","2019","Q2","2019","Q3","2019","Q4",
                  "2020","Q1","2020","Q2","2020","Q3","2020","M10","2020","M11","2020","M12",
                  "2021","M01","2021","M02","2021","M03","2021","M04","2021","M05","2021","M06","2021","M07","2021","M08","2021","M09","2021","M10","2021","M11","2021","M12",
                  "2022","M01","2022","M02","2022","M03","2022","M04","2022","M05","2022","M06","2022","M07","2022","M08","2022","M09","2022","M10","2022","M11","2022","M12",
                  "2023","M01","2023","M02","2023","M03","2023","M04","2023","M05","2023","M06","2023","M07","2023","M08","2023","M09","2023","M10","2023","M11","2023","M12")



#Test search 
search <- tribble(~Year,~Period,
                  "2014","Q1","2014","Q2","2014","Q3","2014","Q4")



#Creating summary dataset to hold overall accuracy checks
scf <- data.frame(adsh = character(),
                  cik = numeric(),
                  form = character(),
                  filed = numeric(),
                  ddate = numeric(),
                  ocf_tag = numeric(),
                  icf_tag = numeric(),
                  fcf_tag = numeric(),
                  ocf_lines = integer(),
                  icf_lines = integer(),
                  fcf_lines = integer(),
                  total_lines = integer(),
                  ocf_chgwc_lines = integer(),
                  ocf_noncash_lines = integer(),
                  custom_count = numeric(),
                  other_noncash = numeric(),
                  other_chg_wc = numeric(),
                  other_icf = numeric(),
                  other_fcf = numeric(),
                  order = character(),
                  int_paid = numeric(),
                  tax_paid = numeric(),
                  ocf_check = character(),
                  icf_check = character(),
                  fcf_check = character())
 
#Creating overall summary dataset to hold overall accuracy checks
summary <- data.frame(period = character(),
                      year = integer(),
                      ocf_acc = numeric(),
                      icf_acc = numeric(),
                      fcf_acc = numeric())

#Loop
for(i in 1:nrow(search)){
  
  #Look through search table for each period which corresponds to naming of zip files downloaded
  year <- search[[i,1]]
  period <- search[[i,2]]
  
  #Read in FS XBRL files
  cf_ind <- read_parquet(paste0(output_path,year,"_",period,"_CF_Summary.parquet"))
  
  #Summarise overall accuracy for each period
  summary <- rbind(summary, data.frame(period = period, year = year, ocf_acc = mean(cf_ind$ocf_check == TRUE), icf_acc = mean(cf_ind$icf_check == TRUE), fcf_acc = mean(cf_ind$fcf_check == TRUE)))
  
  #Combine
  scf <- bind_rows(scf,cf_ind)
}

#Check dups
check_dups <- scf |> group_by(adsh,cik,ddate) |> summarise(obs = n())

#Save down
write_parquet(scf,paste0(output_path,"SCF_Summary.parquet"))



scf %>% group_by(form) %>% summarise(int_perc = mean(int_paid),tax_perc = mean(tax_paid))
#Scratch---------------------------------------

cf_test <- read_parquet("D:/XBRL/XBRL SCF Summary Files/2016_Q1_CF_Summary.parquet")
fs_test <- read_parquet("D:/XBRL/XBRL FS Data/2016_Q1_FS.parquet")


#Accuracy
mean(cf_test$ocf_check == TRUE)  
mean(cf_test$icf_check == TRUE)  
mean(cf_test$fcf_check == TRUE)  

#Read in raw XBRL files
num <- read_delim('D:/XBRL/XBRL Extracted Data/2016_Q1_num.tsv')
tag <- read_delim('D:/XBRL/XBRL Extracted Data/2016_Q1_tag.tsv')
pre <- read_delim('D:/XBRL/XBRL Extracted Data/2016_Q1_pre.tsv')
sub <- read_delim('D:/XBRL/XBRL Extracted Data/2016_Q1_sub.tsv')


dm <- num |> filter(tag == "NetCashProvidedByUsedInOperatingActivities") |> select(adsh,tag,ddate,value) |> group_by(adsh,tag,ddate,value) |> summarise(obs = n())
dm <- pre |> filter(tag == "NetCashProvidedByUsedInOperatingActivities",stmt=="CF") |> select(adsh,tag) |> group_by(adsh,tag) |> summarise(obs = n())
test <- pre |> filter(adsh == "0000860546-16-000064",stmt=="CF")
comp <- sub |> filter(adsh == "0000860546-16-000064")
  
  
  
cf_data1 <- fs_test %>%
  filter(CF==1) %>% 
  #Pre file doesn't have period length of tag so if different period lengths for same tag it causes dups. Requiring highest qtr within tag gets correct period and no dups.
  group_by(adsh,cik,filed,ddate,tag) %>% 
  filter(qtrs == max(qtrs))%>%
  ungroup()%>%
  #Now group by firm, filing, and date to make variables of interest within each observation
  group_by(adsh,cik,form,filed,ddate) %>% 
  mutate(
    #Identify Income Line (sometimes adjustments to cont ops so taking minimum line - will drop the subtotals)
    inc_line = coalesce(min(line[tag %in% c("NetIncomeLoss",
                                            "ProfitLoss",
                                            "IncomeLossFromContinuingOperationsIncludingPortionAttributableToNoncontrollingInterest",
                                            "NetIncomeLossAvailableToCommonStockholdersBasic", "IncomeLossFromContinuingOperations")],na.rm = T), 0L),
    #Now identify extra income lines to drop to make footing work
    inc_line_drop = if_else(tag %in% c("NetIncomeLoss",
                                        "ProfitLoss",
                                        "IncomeLossFromContinuingOperationsIncludingPortionAttributableToNoncontrollingInterest",
                                        "NetIncomeLossAvailableToCommonStockholdersBasic",
                                        "IncomeLossFromContinuingOperations") & line != inc_line, "DROP","KEEP"),
    #Identify each section line number 
    ocf_line = if_else(any(tag=="NetCashProvidedByUsedInOperatingActivities"), first(line[tag=="NetCashProvidedByUsedInOperatingActivities"]), NA),
    icf_line = if_else(any(tag=="NetCashProvidedByUsedInInvestingActivities"), first(line[tag=="NetCashProvidedByUsedInInvestingActivities"]), NA),
    fcf_line = if_else(any(tag=="NetCashProvidedByUsedInFinancingActivities"), first(line[tag=="NetCashProvidedByUsedInFinancingActivities"]), NA),
    #Sometimes ICF and FCF are in different order - section classification depends on order
    order = if_else((ocf_line < icf_line & icf_line < fcf_line),"OIF",
                    if_else((ocf_line< fcf_line & fcf_line < icf_line),"OFI",
                            if_else((ocf_line< fcf_line & fcf_line < icf_line),"OFI","Other"))),
    Section = if_else(order == "OIF",if_else(line>inc_line & line<ocf_line,"Operating",
                                             if_else(line>ocf_line&line<icf_line,"Investing",
                                                     if_else(line>icf_line&line<fcf_line,"Financing","None"))),
                      if_else(line>inc_line & line<ocf_line,"Operating",
                              if_else(line>ocf_line&line<fcf_line,"Financing",
                                      if_else(line>fcf_line&line<icf_line,"Investing","None"))),"None"),
    value = if_else(!is.na(crdr) & Section != "None",if_else(negating == 1,-value, value),value),
    custom = if_else(str_detect(version,"us-gaap"),0,1)) %>%
  #Filter out tags that cause footing issues: missing values (duplicates), subtotals (cont ops vs discont)
  filter(!is.na(value),
         inc_line_drop == "KEEP",
         tag != "NetCashProvidedByUsedInOperatingActivitiesContinuingOperations",
         tag != "AdjustmentsToReconcileNetIncomeLossToCashProvidedByUsedInOperatingActivities",
         tag != "NetCashProvidedByUsedInInvestingActivitiesContinuingOperations",
         tag != "NetCashProvidedByUsedInFinancingActivitiesContinuingOperations")%>%
  ungroup()



#Create Cash Flow summary measures
cf_data2 <- cf_data1  %>%
  group_by(adsh,cik,form,filed,ddate) %>% 
  #Now create summary measures for each statement of cash flow
  summarise(
    ocf_tag = if_else(any(tag=="NetCashProvidedByUsedInOperatingActivities"), first(value[tag=="NetCashProvidedByUsedInOperatingActivities"]), NA),
    icf_tag = if_else(any(tag=="NetCashProvidedByUsedInInvestingActivities"), first(value[tag=="NetCashProvidedByUsedInInvestingActivities"]), NA),
    fcf_tag = if_else(any(tag=="NetCashProvidedByUsedInFinancingActivities"), first(value[tag=="NetCashProvidedByUsedInFinancingActivities"]), NA),
    #Create counts of lines (tags) and custom tags
    ocf_lines = n_distinct(if_else(Section=="Operating",tag,NA)),
    icf_lines = n_distinct(if_else(Section=="Investing",tag,NA)),
    fcf_lines = n_distinct(if_else(Section=="Financing",tag,NA)),
    total_lines = ocf_lines + icf_lines + fcf_lines,
    custom_count = sum(custom),
    #Other measures
    other_noncash = abs(sum(case_when(tag %in% c("OtherNoncashIncomeExpense","OtherNoncashExpense") ~ value, TRUE ~ 0)))/abs(ocf_tag),
    other_chg_wc = abs(sum(case_when(tag %in% c("IncreaseDecreaseInOtherOperatingAssets",
                                                "IncreaseDecreaseInOtherOperatingLiabilities",
                                                "IncreaseDecreaseInAccruedLiabilitiesAndOtherOperatingLiabilities",
                                                "IncreaseDecreaseInPrepaidDeferredExpenseAndOtherAssets",
                                                "IncreaseDecreaseInOtherOperatingCapitalNet",
                                                "OtherOperatingActivitiesCashFlowStatement") ~ value, TRUE ~ 0)))/abs(ocf_tag),
    other_icf = abs(sum(case_when(tag %in% c("PaymentsForProceedsFromOtherInvestingActivities","PaymentsToAcquireOtherInvestments") ~ value, TRUE ~ 0)))/abs(ocf_tag),
    other_fcf = abs(sum(case_when(tag %in% c("ProceedsFromPaymentsForOtherFinancingActivities") ~ value, TRUE ~ 0)))/abs(ocf_tag),
    order = unique(order),
    int_paid = if_else(any(tag=="InterestPaid")|any(tag=="InterestPaidNet"),1,0),
    tax_paid = if_else(any(tag=="IncomeTaxesPaid")|any(tag=="IncomeTaxesPaidNet"),1,0),
    .groups = "keep") %>%
  #Filtering out observations where missing each section total tag (mostly due to cash accounts having balances for other periods = error)
  filter(!is.na(ocf_tag) & !is.na(icf_tag) & !is.na(fcf_tag))




test <- fs_test |> filter(adsh == "0000006885-16-000411",CF==1,ddate == 20160131)
test2 <- cf_data1 |> filter(adsh == "0000006885-16-000411",ddate==20160131)

pre_test <- pre |> filter(adsh == "0000006885-16-000411",stmt=="CF")
num_test <- num |> filter(adsh == "0000006885-16-000411",ddate==20160131)




scf2 <- scf %>% mutate(ddate = as.Date(as.character(ddate),format ="%Y%m%d"),year = year(ddate))


scf2 %>% group_by(year) %>% summarise(custom = mean(custom_count))
