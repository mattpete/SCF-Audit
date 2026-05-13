# Setup - load packages and functions -----------------------------------------------------------------

library(dplyr)
library(dbplyr)
library(glue)
library(arrow)
library(haven)
library(stringr)
library(httr)
library(fixest)
library(modelsummary)
library(gt)
library(rcompanion)
library(sjlabelled)
library(officer)
library(flextable)
library(tidyverse)

#Home
data_path <- "C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Data/"
source("C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Code/Functions.R")


#Work
data_path <- "C:/Users/m221p165/Dropbox/Dissertation/Statement of Cash Flows/Data/"
source("C:/Users/m221p165/Dropbox/Dissertation/Statement of Cash Flows/Code/Functions.R")


# Load Financial Data------------------------------------------------------------------------------
findata <- read_parquet(paste0(data_path,"financial_data_12272024.parquet")) |> 
  filter(!is.na(CFO_GAAP_A),!is.na(FCF_BS_A)) |> 
  #mutate(FF49 = assign_FF49(sic)) |> 
  mutate_at(vars(starts_with("CF"),FCF_BS_A,FCF_SIM_A,FCF_EQ_A,FCF_GAAP_A,OE_A),winsorize_x) 


# Summary Statistics------------------------------------------

descrip <- bondretdata |> select(CFO_GAAP_F,CFO_FIN_F,CFO_STAX_F,CFO_EBITDA_F,
                          CFBA_F,SFCF_F,PYFCF_F,FCFE_F,OE_F,CFI_GAAP_F,CFI_SFCF_F,ln_mve,cum_credret)


sum_stats <- datasummary( All(descrip) ~ N + Min  + P25 + Median + Mean + P75 + Max  + SD, 
             fmt = 3,
             data = descrip,
             output = "latex")

sum_stats

#Export
officer::read_docx() |>  
  flextable::body_add_flextable(value=sum_stats) |>  
  print(target="C:/Users/m221p165/Dropbox/output.docx")

#Correlation matrix
corr_matrix <- round(cor(descrip, use = "complete.obs",method = "pearson"),3) 
corr_matrix[upper.tri(corr_matrix)] <- NA
corr_matrix <- corr_matrix[nrow(corr_matrix):1, ncol(corr_matrix):1]

corr_matrix

#Define labels for output
display_labels <- c(
  cum_credret = "Bond Ret",
  CFO_GAAP_F = bquote(CFO^GAAP),
  CFO_FIN_F = bquote(CFO^FIN),
  CFO_TAX_F = bquote(CFO^TAX),
  CFO_EBITDA_F = bquote(CFO^EBITDA),
  CFBA_F =  bquote(FCF^GAAP),
  SFCF_F =  bquote(FCF^SIM),
  PYFCF_F =  bquote(FCF^BS),
  FCFE_F =  bquote(FCF^EQ),
  OE_F = "OE",
  CFI_GAAP_F = bquote(CFI^GAAP),
  CFI_SFCF_F = "CapEx",
  ln_mve = "Ln(MVE)")

#Correlation Plot
ggcorrplot::ggcorrplot(corr_matrix, 
                       lab = TRUE, 
                       lab_size = 4, 
                       method = "square", 
                       colors = c("blue", "white", "red"), 
                       outline.color = "black", 
                       show.legend = FALSE,
                       hc.order = FALSE) +
  theme_minimal() +  
  theme(
    text = element_text(family = "Times New Roman"), 
    axis.text.y = element_text(hjust = 0.5),  
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(),  
    axis.ticks.x = element_blank(), 
    axis.ticks.y = element_blank(),  
    panel.grid = element_blank(),  
    axis.text.x.top = element_text(angle = 45, vjust = 0.5),  
    plot.margin = margin(10, 10, 10, 10)) +
  scale_x_discrete(labels = display_labels, position = "top") +  
  scale_y_discrete(labels = display_labels)   



#-----------------Model 1: Decision Usefulness ----------------------
# Model 3: By Firm (Just One Measure)----------------------------------
#Create subset of firms to iterate over
firms <- findata |> group_by(gvkey) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cf_vars <- c("CFO_GAAP_A","CFO_FIN_A","CFO_STAX_A","CFO_EBITDA_A","CFO_RD_A","FCF_GAAP_A","FCF_SIM_A","FCF_BS_A","FCF_EQ_A","OE_A")

#Create empty dataframe to hold output
output <- data.frame()

#Run loop over CF measures
for (j in 1:length(cf_vars)) {
  
  #Create empty dataframe to hold results
  results <- data.frame()
  r2 <- c()  # Initialize R2 vector
  
  #Define CF
  cf_var <- cf_vars[j]

  #Run loop over individual firms for that redefined cash flow measures
  for (i in 1:nrow(firms)) {
    
    #Specify firm gvkey
    ind_firm <- firms$gvkey[i]
    
    #Run models
    formula <- as.formula(paste("P1_CFO_GAAP_A ~ ", cf_var))
    model <- summary(lm(formula, data = findata |> filter(gvkey == ind_firm)))
    
    #Extract coefficients
    #coef_intercept <- model$coefficients[[1]]  # Intercept
    coef_cashflow <- model$coefficients[[2]]  #  Cash Flow Measure

    model_r2 <- model$r.squared
    
    #Combine
    ind_result <- data.frame(coef_cashflow)
    
    #Add to results
    results <- rbind(results, ind_result)
    r2 <- c(r2, model_r2)
  }
  
  #Create list of CFO benchmark
  if (cf_var == "CFO_GAAP_A") {benchmark_r2 <- r2} else {benchmark_r2 <- benchmark_r2}
  
  #Winsorize results for outliers
  results <- results |> mutate_all(winsorize_x2)
  
  #Difference in R2; T-test; and Wilcoxon Signed Rank test for R2
  diff_r2 <- as.data.frame(cbind(benchmark_r2,r2)) |> mutate(diff = r2 - benchmark_r2)
  ttest <- t.test(r2, benchmark_r2, paired = TRUE)$statistic
  wilcox <- wilcoxonZ(r2,benchmark_r2, paired=TRUE,correct=FALSE,digits = 4)[[1]]
  
  #Now take average of yearly coefficients and calculate t-statistics
  ind_output <- rbind(
    data.frame(Variable = cf_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(Variable = "", Output = "Test Statistic", as.data.frame(lapply(results, tstat)), Avg_R2 = "", Avg_Diff_R2 = round(ttest, 3), Med_R2 = "",Med_Diff_R2=round(wilcox, 3)))
  
  # Add to output
  output <- rbind(output, ind_output)
}

# Export
table <- flextable(output) |> colformat_double(j = c("coef_cashflow","Avg_R2","Med_R2"), digits = 3)
table


table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")


# Model 2: By SIC4 with Firm FE (Just One Measure)----------------------------------

bondretdata2 <- bondretdata |> group_by(SIC,CUSIP) |> mutate(within_SIC_obs = n())|>filter(within_SIC_obs>2) |>ungroup()

#Create subset of firms to iterate over
industries <- bondretdata2 |> group_by(SIC) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cf_vars <- c("CFO_GAAP_F", "CFO_FIN_F", "CFO_STAX_P", "CFO_EBITDA_F","SFCF_F", "PYFCF_F", "CFBA_F", "FCFE_F")

#Create empty dataframe to hold output
output <- data.frame()

#Run loop over CF measures
for (j in 1:length(cf_vars)) {
  
  #Create empty dataframe to hold results
  results <- data.frame()
  r2 <- c()  # Initialize R2 vector
  
  #Define CFO or CFI
  cf_var <- cf_vars[j]

  #Run loop over individual firms for that redefined cash flow measures
  for (i in 1:nrow(industries)) {
    
    #Specify firm gvkey
    industry <- industries$SIC[i]
    
    #Run models
    regdata <- bondretdata2 |> filter(SIC == industry)
    formula <- as.formula(paste("cum_credret ~ OE_F +", cf_var,"|CUSIP"))
    model <- summary(feols(formula, data = regdata))
    
    #Extract coefficients
    coef_earnings <- model$coefficients[[1]]  # Earnings
    coef_cashflow <- model$coefficients[[2]]  # Cash Flow
    
    model_r2 <- r2(model,type = "wr2")
    
    #Combine
    ind_result <- data.frame(coef_earnings, coef_cashflow)
    
    #Add to results
    results <- rbind(results, ind_result)
    r2 <- c(r2, model_r2)
  }
  
  #Create list of CFO benchmark
  if (cf_var == "CFO_GAAP_F") {benchmark_r2 <- r2} else {benchmark_r2 <- benchmark_r2}
  
  #Winsorize results for outliers
  results <- results |> mutate_all(winsorize_x2)
  
  #Difference in R2; T-test; and Wilcoxon Signed Rank test for R2
  diff_r2 <- as.data.frame(cbind(benchmark_r2,r2)) |> mutate(diff = r2 - benchmark_r2)
  ttest <- t.test(r2, benchmark_r2, paired = TRUE)$statistic
  wilcox <- wilcoxonZ(r2,benchmark_r2, paired=TRUE,correct=FALSE,digits = 4)[[1]]
  
  #Now take average of yearly coefficients and calculate t-statistics
  ind_output <- rbind(
    data.frame(Variable = cf_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(Variable = "", Output = "Test Statistic", as.data.frame(lapply(results, tstat)), Avg_R2 = "", Avg_Diff_R2 = round(ttest, 3), Med_R2 = "",Med_Diff_R2=round(wilcox, 3)))
  
  # Add to output
  output <- rbind(output, ind_output)
}

# Export
table <- flextable(output) |> colformat_double(j = c("coef_earnings", "coef_cashflow", "Avg_R2","Med_R2"), digits = 3)
table

table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")


# Model 2: By Firm (CFO and CFI)----------------------------------
#Create subset of firms to iterate over
firms <- bondretdata |> group_by(gvkey) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cfo_vars <- c("CFO_GAAP_P", "CFO_FIN_P", "CFO_STAX_P", "CFO_EBITDA_P", "CFO_SFCF_P", "CFO_PYFCF_P", "CFO_FCFE_P")
cfi_vars <- c("CFI_GAAP_P", "CFI_FIN_P", "CFI_STAX_P", "CFI_EBITDA_P", "CFI_SFCF_P", "CFI_PYFCF_P", "CFI_SFCF_P")

#Create empty dataframe to hold output
output <- data.frame()

#Run loop over CF measures
for (j in 1:length(cfo_vars)) {
  
  #Create empty dataframe to hold results
  results <- data.frame()
  r2 <- c()  # Initialize R2 vector
  
  #Define CFO or CFI
  cfo_var <- cfo_vars[j]
  cfi_var <- cfi_vars[j]
  
  #Run loop over individual firms for that redefined cash flow measures
  for (i in 1:nrow(firms)) {
    
    #Specify firm gvkey
    ind_firm <- firms$gvkey[i]
    
    #Run models
    formula <- as.formula(paste("CHG_PRC ~ EARN_P + D_P +", cfo_var, "+", cfi_var))
    model <- summary(lm(formula, data = bondretdata |> filter(gvkey == ind_firm)))
    
    #Extract coefficients
    #model_coef1 <- model$coefficients[[1]]  # Intercept
    coef_earnings <- model$coefficients[[2]]  # Earnings
    coef_dividends <- model$coefficients[[3]]  # Dividends
    coef_cfo <- model$coefficients[[4]]  # CFO
    coef_cfi <- model$coefficients[[5]]  # CFI

    model_r2 <- model$r.squared
    
    #Combine
    ind_result <- data.frame(coef_earnings, coef_dividends, coef_cfo, coef_cfi)
    
    #Add to results
    results <- rbind(results, ind_result)
    r2 <- c(r2, model_r2)
  }
  
  #Create list of CFO benchmark
  if (cfo_var == "CFO_GAAP_P") {benchmark_r2 <- r2} else {benchmark_r2 <- benchmark_r2}
  
  #Winsorize results for outliers
  results <- results |> mutate_all(winsorize_x)
  
  #Difference in R2; T-test; and Wilcoxon Signed Rank test for R2
  diff_r2 <- as.data.frame(cbind(benchmark_r2,r2)) |> mutate(diff = r2 - benchmark_r2)
  ttest <- t.test(r2, benchmark_r2, paired = TRUE)$statistic
  wilcox <- wilcoxonZ(r2,benchmark_r2, paired=TRUE,correct=FALSE,digits = 4)[[1]]
  
  #Now take average of yearly coefficients and calculate t-statistics
  ind_output <- rbind(
    data.frame(Variable = cfo_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(Variable = "", Output = "Test Statistic", as.data.frame(lapply(results, tstat)), Avg_R2 = "", Avg_Diff_R2 = round(ttest, 3), Med_R2 = "",Med_Diff_R2=round(wilcox, 3)))
  
  # Add to output
  output <- rbind(output, ind_output)
}

# Export
table <- flextable(output) |> colformat_double(j = c("coef_earnings", "coef_dividends", "coef_cfo", "coef_cfi","Avg_R2","Med_R2"), digits = 3)
table


table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")


# Model 2: By SIC4 with Firm FE (CFO and CFI)----------------------------------

bondretdata2 <- bondretdata|> group_by(SIC,gvkey) |> mutate(within_SIC_obs = n())|>filter(within_SIC_obs>1)

#Create subset of firms to iterate over
industries <- bondretdata2 |> group_by(SIC) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cfo_vars <- c("CFO_GAAP_P", "CFO_FIN_P", "CFO_STAX_P", "CFO_EBITDA_P", "CFO_SFCF_P", "CFO_PYFCF_P", "CFO_FCFE_P")
cfi_vars <- c("CFI_GAAP_P", "CFI_FIN_P", "CFI_STAX_P", "CFI_EBITDA_P", "CFI_SFCF_P", "CFI_PYFCF_P", "CFI_SFCF_P")

#Create empty dataframe to hold output
output <- data.frame()

#Run loop over CF measures
for (j in 1:length(cfo_vars)) {
  
  #Create empty dataframe to hold results
  results <- data.frame()
  r2 <- c()  # Initialize R2 vector
  
  #Define CFO or CFI
  cfo_var <- cfo_vars[j]
  cfi_var <- cfi_vars[j]
  
  #Run loop over individual firms for that redefined cash flow measures
  for (i in 1:nrow(industries)) {
    
    #Specify firm gvkey
    industry <- industries$SIC[i]
    
    #Run models
    regdata <- bondretdata2 |> filter(SIC == industry)
    formula <- as.formula(paste("CHG_PRC ~ EARN_P + D_P +", cfo_var, "+", cfi_var,"|gvkey"))
    model <- summary(feols(formula, data = regdata))
    
    #Extract coefficients
    coef_earnings <- model$coefficients[[1]]  # Earnings
    coef_dividends <- model$coefficients[[2]]  # Div
    coef_cfo <- model$coefficients[[3]]  # CFO
    coef_cfi <- model$coefficients[[4]]  # CFI

    model_r2 <- r2(model,type = "war2")
    
    #Combine
    ind_result <- data.frame(coef_earnings, coef_dividends, coef_cfo, coef_cfi)
    
    #Add to results
    results <- rbind(results, ind_result)
    r2 <- c(r2, model_r2)
  }
  
  #Create list of CFO benchmark
  if (cfo_var == "CFO_GAAP_P") {benchmark_r2 <- r2} else {benchmark_r2 <- benchmark_r2}
  
  #Winsorize results for outliers
  results <- results |> mutate_all(winsorize_x)
  
  #Difference in R2; T-test; and Wilcoxon Signed Rank test for R2
  diff_r2 <- as.data.frame(cbind(benchmark_r2,r2)) |> mutate(diff = r2 - benchmark_r2)
  ttest <- t.test(r2, benchmark_r2, paired = TRUE)$statistic
  wilcox <- wilcoxonZ(r2,benchmark_r2, paired=TRUE,correct=FALSE,digits = 4)[[1]]
  
  #Now take average of yearly coefficients and calculate t-statistics
  ind_output <- rbind(
    data.frame(Variable = cfo_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(Variable = "", Output = "Test Statistic", as.data.frame(lapply(results, tstat)), Avg_R2 = "", Avg_Diff_R2 = round(ttest, 3), Med_R2 = "",Med_Diff_R2=round(wilcox, 3)))
  
  # Add to output
  output <- rbind(output, ind_output)
}

# Export
table <- flextable(output) |> colformat_double(j = c("coef_earnings", "coef_dividends", "coef_cfo", "coef_cfi","Avg_R2","Med_R2"), digits = 3)
table

table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")

#-------------------Extra Specifications ------------------------------
# Model 1: Pooled ----------------------------------------------------------------------------------
models <- list(
  "(1)"  = lm(mktadj_ret_dd ~ CFO_GAAP_P , data = bondretdata),
  "(2)"  = lm(mktadj_ret_dd ~ CFO_FIN_P , data = bondretdata),
  "(3)"  = lm(mktadj_ret_dd ~ CFO_TAX_P , data = bondretdata),
  "(4)"  = lm(mktadj_ret_dd ~ CFO_EBITDA_P , data = bondretdata),
  "(5)"  = lm(mktadj_ret_dd ~ SFCF_P , data = bondretdata),
  "(6)"  = lm(mktadj_ret_dd ~ PYFCF_P , data = bondretdata),
  "(7)"  = lm(mktadj_ret_dd ~ FCFE_P , data = bondretdata),
  "(8)"  = lm(mktadj_ret_dd ~ FCFF_P , data = bondretdata),
  "(9)" = lm(mktadj_ret_dd ~ OE_P , data = bondretdata))

#Rename coefs to be generic measure
coef <- c("CFO_GAAP_P" = "Measure",
          "CFO_FIN_P" = "Measure",
          "CFO_TAX_P" = "Measure",
          "CFO_EBITDA_P" = "Measure",
          "SFCF_P" = "Measure",
          "PYFCF_P" = "Measure",
          "FCFE_P" = "Measure",
          "FCFF_P" = "Measure",
          "OE_P" = "Measure")

#Label measures across the top
FE_Row <- tribble(~term,~"(1)",~"(2)",~"(3)", ~"(4)",~"(5)",~"(6)",~"(7)",~"(8)", ~"(9)",
                  "","CFO","CFO_F","CFO_Adj", "CEARN","SFCF","FCF","FCFE","FCFF","OE")
attr(FE_Row,"position") <- c(0)

panel <- modelsummary(models, 
                      #vcov =  ~ PERMNO + month_year,
                      statistic = "statistic",
                      #stars = c('*' = .1, '**' = .05, '\\sym{***}' = .01) ,
                      coef_map = coef,
                      gof_map = c("nobs","r.squared"),
                      #output = "flextable", 
                      escape = F,
                      booktabs = T,
                      add_rows = FE_Row) 
panel

read_docx() |>  
  body_add_flextable(value=panel) |>  
  print(target="C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/output.docx")

summary(lm(m12_mktadj_ret ~ CFO_P + ICF_P , data = bondretdata))

# Model 1: Pooled ----------------------------------------------------------------------------------
models <- list(
  "(1)"  = felm(mktadj_ret_dd ~ CFO_GAAP_P |gvkey , data = bondretdata),
  "(2)"  = felm(mktadj_ret_dd ~ CFO_FIN_P |gvkey, data = bondretdata),
  "(3)"  = felm(mktadj_ret_dd ~ CFO_TAX_P |gvkey, data = bondretdata),
  "(4)"  = felm(mktadj_ret_dd ~ CFO_EBITDA_P |gvkey, data = bondretdata),
  "(5)"  = felm(mktadj_ret_dd ~ SFCF_P |gvkey, data = bondretdata),
  "(6)"  = felm(mktadj_ret_dd ~ PYFCF_P |gvkey, data = bondretdata),
  "(7)"  = felm(mktadj_ret_dd ~ FCFE_P |gvkey, data = bondretdata),
  "(8)"  = felm(mktadj_ret_dd ~ FCFF_P |gvkey, data = bondretdata),
  "(9)"  = felm(mktadj_ret_dd ~ OE_P |gvkey, data = bondretdata))

#Rename coefs to be generic measure
coef <- c("CFO_GAAP_P" = "Measure",
          "CFO_FIN_P" = "Measure",
          "CFO_TAX_P" = "Measure",
          "CFO_EBITDA_P" = "Measure",
          "SFCF_P" = "Measure",
          "PYFCF_P" = "Measure",
          "FCFE_P" = "Measure",
          "FCFF_P" = "Measure",
          "OE_P" = "Measure")

#Label measures across the top
FE_Row <- tribble(~term,~"(1)",~"(2)",~"(3)", ~"(4)",~"(5)",~"(6)",~"(7)",~"(8)", ~"(9)",
                  "","CFO_GAAP","CFO_FIN","CFO_TAX", "CFO_EBITDA","SFCF","PYFCF","FCFE","FCFF","OE")
attr(FE_Row,"position") <- c(0)

panel <- modelsummary(models, 
                      #vcov =  ~ PERMNO + month_year,
                      statistic = "statistic",
                      #stars = c('*' = .1, '**' = .05, '\\sym{***}' = .01) ,
                      coef_map = coef,
                      gof_map = c("nobs","adj.r.squared"),
                      #output = "flextable", 
                      escape = F,
                      booktabs = T,
                      add_rows = FE_Row) 
panel

read_docx() |>  
  body_add_flextable(value=panel) |>  
  print(target="C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/output.docx")

summary(lm(m12_mktadj_ret ~ CFO_P + ICF_P , data = bondretdata))

# Model 1: By SIC2-Year ----------------------------------------------------------------------------------

#Create subset of industry-years to iterate over
ind_year <- bondretdata |> filter(!is.na(m12_mktadj_ret),fyear>1987) |> 
  group_by(gvkey) |> mutate(firm_obs = n()) |> filter(firm_obs > 14) |> ungroup() |> 
  group_by(SIC2, fyear) |> summarise(obs = n()) |> filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop over industries
for (i in 1:nrow(ind_year)) {
  
  #Specify industry
  industry <- ind_year$SIC2[i]
  
  #Specify year
  year <- ind_year$fyear[i]
  
  #Subset data
  regdata_subset <- regdata %>% filter(SIC2 == industry, fyear==year)
  
  #Run models
  model_1 <- summary(lm(m12_mktadj_ret ~ CFO_P , data = regdata_subset))
  model_2 <- summary(lm(m12_mktadj_ret ~ CFO_F_P, data = regdata_subset))
  model_3 <- summary(lm(m12_mktadj_ret ~ CFO_AdjTax_P, data = regdata_subset))
  model_4 <- summary(lm(m12_mktadj_ret ~ CEARN_P, data = regdata_subset))
  model_5 <- summary(lm(m12_mktadj_ret ~ CFBA_P, data = regdata_subset))
  model_6 <- summary(lm(m12_mktadj_ret ~ SFCF_P, data = regdata_subset))
  model_7 <- summary(lm(m12_mktadj_ret ~ FCF_P, data = regdata_subset))
  model_8 <- summary(lm(m12_mktadj_ret ~ FCFE_P, data = regdata_subset))
  model_9 <- summary(lm(m12_mktadj_ret ~ FCFF_P, data = regdata_subset))
  model_10 <- summary(lm(m12_mktadj_ret ~ OE_P, data = regdata_subset))
  
  #Extract r-squared
  model_1_rsq <- model_1$r.squared
  model_2_rsq <- model_2$r.squared
  model_3_rsq <- model_3$r.squared
  model_4_rsq <- model_4$r.squared
  model_5_rsq <- model_5$r.squared
  model_6_rsq <- model_6$r.squared
  model_7_rsq <- model_7$r.squared
  model_8_rsq <- model_8$r.squared
  model_9_rsq <- model_9$r.squared
  model_10_rsq <- model_10$r.squared
  
  
  #Combine
  ind_result <- data.frame(model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,model_6_rsq,model_7_rsq,model_8_rsq,model_9_rsq,model_10_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}


#Output results and test for difference in R2
#First calculate the average R-squared for each model
avg_r2 <- sapply(results, mean)
med_r2 <- sapply(results, median)

#Make variable names match measure used
results <- results |> 
  var_labels(model_1_rsq = "CFO",
             model_2_rsq = "CFO_F",
             model_3_rsq = "CFO_Adj",
             model_4_rsq = "CEARN",
             model_5_rsq = "CFBA",
             model_6_rsq = "SFCF",
             model_7_rsq = "FCF",
             model_8_rsq = "FCFE",
             model_9_rsq = "FCFF",
             model_10_rsq = "OE") |> 
  label_to_colnames()

#Perform Wilcoxon signed-rank test and t-test of comparing each column to the first column (i.e. CFO) 
test_results <- lapply(2:ncol(results), function(i) {
  
  #Wilcoxon signed rank test  
  wilcoxon_result <- wilcox.test(results[, 1], results[, i], paired = TRUE)
  p_value <- wilcoxon_result$p.value
  
  #T-test
  t_result <- t.test(results[, i], results[, 1], paired = TRUE)
  t_stat <- t_result$statistic
  
  list(statistic = t_stat, p.value = p_value)
})

#Extract statistic and p-value
t_stats <- sapply(test_results, function(x) x["statistic"])
wilcox_pvalues <- sapply(test_results, function(x) x["p.value"])

#Dataframe for model performance results
results_df <- data.frame(Model = colnames(results),
                         n = count(results),
                         Average_R2 = avg_r2,
                         Median_R2 = med_r2,
                         T_Stat = c(NA, unlist(t_stats)),
                         Wilcox_p_value = c(NA, unlist(wilcox_pvalues))) 

#Make a table
results_table <- gt(results_df) |> 
  tab_header(title = "Regression Model Comparison",
             subtitle = "Average R-squared and Two-sided Wilcoxon Test Statistics and p-values") |> 
  fmt_number(columns = vars(Median_R2,Average_R2, T_Stat, Wilcox_p_value,),decimals = 3) |> 
  cols_label(Model = "Measures",
             n = "Obs",
             Median_R2 = "Median R-squared",
             Average_R2 = "Mean R-squared",
             T_Stat = "T-Statistic",
             Wilcox_p_value = "Wilcoxon p-value")

print(results_table)




# Model 1: By FF12-Year---------------------------------------------------------------------

#Create subset of firms to iterate over
regdata <- bondretdata |> group_by(FF12, fyear) |> mutate(obs = n()) |> filter(obs > 14) |> ungroup()

#Create list of cash flow measures to try
vars <- c("CFO_P", "CFO_F_P", "CFO_AdjTax_P", "CEARN_P", "CFBA_P", "SFCF_P", "FCF_P", "FCFE_P", "FCFF_P","OE_P")

#Create list of industries to loop over
industries <- regdata |> select(FF12) |> distinct()

#Create list of years to loop over
years <- unique(regdata$fyear)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop over CF measures
for (var in vars) {
  
  #Run loop over industries
  for (ind in 1:nrow(industries)) {
    
    #Specify industry
    industry <- industries$FF12[ind]
    
    #Run loop over years
    for (yr in 1:length(years)) {
      
      #Specify year
      year <- years[yr]
      
      #Filter data for the specific industry and year
      regdata_subset <- regdata |> filter(FF12 == industry, fyear == year)
      
      #Check if there are enough observations for the regression
      if (nrow(regdata_subset) > 0) {
        
        #Run models
        formula <- as.formula(paste("m12_mktadj_ret ~ ", var))
        model <- summary(lm(formula, data = regdata_subset))
        
        #Create data frame for the current results
        ind_result <- data.frame(industry = industry, year = year,measure = var, R2 = model$r.squared)
        
        #Add to running results dataframe
        results <- rbind(results, ind_result)
      }
    }
  }
}


#Analyzing time trend 
results2 <- results |> 
  mutate(trend = year - 1988,
         trend2 = if_else(year <= 1999,0,year - 1999))

summary1 <- data.frame()

for (var in vars) {
  
  #Estimate models with time trend by each measure
  m1 <- summary(lm(R2 ~ trend, results2 |> filter(measure == var)))
  
  #Extract coefficients and t-stat
  int_est <- data.frame(Measure=var,Statistic="Intercept",Estimate1=m1$coefficients[1,1])
  int_tstats <- data.frame(Measure=var,Statistic="T-Statistic",Estimate1=m1$coefficients[1,3])
  trend_est <- data.frame(Measure=var,Statistic="Trend",Estimate1=m1$coefficients[2,1])
  trend_tstats <- data.frame(Measure=var,Statistic="T-Statistic",Estimate1=m1$coefficients[2,3])
  
  #Combine
  summary1 <- rbind(summary1,rbind(int_est,int_tstats,trend_est,trend_tstats))
}


# Model 1: By FF49-Year---------------------------------------------------------------------

#Create subset of firms to iterate over
regdata <- bondretdata |> group_by(FF49, fyear) |> mutate(obs = n()) |> filter(obs > 14) |> ungroup()

#Create list of cash flow measures to try
vars <- c("CFO_P", "CFO_F_P", "CFO_AdjTax_P", "CEARN_P", "CFBA_P", "SFCF_P", "FCF_P", "FCFE_P", "FCFF_P","OE_P")

#Create list of industries to loop over
industries <- regdata |> select(FF49) |> distinct()

#Create list of years to loop over
years <- unique(regdata$fyear)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop over CF measures
for (var in vars) {
  
  #Run loop over industries
  for (ind in 1:nrow(industries)) {
    
    #Specify industry
    industry <- industries$FF49[ind]
    
    #Run loop over years
    for (yr in 1:length(years)) {
      
      #Specify year
      year <- years[yr]
      
      #Filter data for the specific industry and year
      regdata_subset <- regdata |> filter(FF49 == industry, fyear == year)
      
      #Check if there are enough observations for the regression
      if (nrow(regdata_subset) > 0) {
        
        #Run models
        formula <- as.formula(paste("m12_mktadj_ret ~ ", var))
        model <- summary(lm(formula, data = regdata_subset))
        
        #Create data frame for the current results
        ind_result <- data.frame(industry = industry, year = year,measure = var, R2 = model$r.squared)
        
        #Add to running results dataframe
        results <- rbind(results, ind_result)
      }
    }
  }
}


#Analyzing time trend 
results2 <- results |> 
  mutate(trend = year - 1988,
         trend2 = if_else(year <= 1999,0,year - 1999))

summary2 <- data.frame()

for (var in vars) {
  
  #Estimate models with time trend by each measure
  m1 <- summary(lm(R2 ~ trend, results2 |> filter(measure == var)))
  
  #Extract coefficients and t-stat
  int_est <- data.frame(Measure=var,Statistic="Intercept",Estimate2=m1$coefficients[1,1])
  int_tstats <- data.frame(Measure=var,Statistic="T-Statistic",Estimate2=m1$coefficients[1,3])
  trend_est <- data.frame(Measure=var,Statistic="Trend",Estimate2=m1$coefficients[2,1])
  trend_tstats <- data.frame(Measure=var,Statistic="T-Statistic",Estimate2=m1$coefficients[2,3])
  
  #Combine
  summary2 <- rbind(summary2,rbind(int_est,int_tstats,trend_est,trend_tstats))
}


# Model 1: By SIC2-Year---------------------------------------------------------------------

#Create subset of firms to iterate over
regdata <- bondretdata |> group_by(SIC,fyear) |> mutate(obs = n()) |> filter(obs > 25) |> ungroup()

#Create list of cash flow measures to try
vars <- c("CFO_GAAP_P", "CFO_FIN_P", "CFO_TAX_P", "CFO_EBITDA_P","SFCF_P", "PYFCF_P", "FCFF_P", "FCFE_P","OE_P")

#Create list of industries to loop over
industries <- regdata |> select(SIC) |> distinct() |>pull()

#Create list of years to loop over
years <- regdata |> select(fyear) |> unique() |> arrange(fyear) |> pull()

#Create empty dataframe to hold results
results <- data.frame()

#Run loop over CF measures
for (var in vars) {
  
  #Run loop over industries
  for (ind in 1:length(industries)) {
    
    #Specify industry
    industry <- industries[ind]
    
    #Run loop over years
    for (yr in 1:length(years)) {
      
      #Specify year
      year <- years[yr]
      
      #Filter data for the specific industry and year
      regdata_subset <- regdata |> filter(SIC == industry, fyear == year)
      
      #Check if there are enough observations for the regression
      if (nrow(regdata_subset) > 0) {
        
        #Run models
        formula <- as.formula(paste("mktadj_ret_ddp3 ~ ", var))
        model <- summary(lm(formula, data = regdata_subset))
        
        #Create data frame for the current results
        ind_result <- data.frame(industry = industry, year = year,measure = var, R2 = model$r.squared)
        
        #Add to running results dataframe
        results <- rbind(results, ind_result)
      }
    }
  }
}


plot_data <- results |> 
  arrange(industry,measure, year) |>
  group_by(measure) |>
  mutate(rolling_mean_R2 = zoo::rollapply(R2, width = 5, FUN = mean, fill = NA, align = "right"),
         rolling_se_R2 = zoo::rollapply(R2, width = 5, FUN = function(x) sd(x) / sqrt(5), fill = NA, align = "right")) |>
  ungroup() |>
  filter(year>1988,year<2024) |> 
  group_by(measure,year) |> 
  summarise(rolling_mean_R2 = median(rolling_mean_R2,na.rm=T),
    rolling_se_R2 = mean(rolling_se_R2,na.rm=T)) |> 
  filter(measure%in%c("CFO_GAAP_P","OE_P","CFO_EBITDA_P"))

# Create the plot
ggplot(plot_data, aes(x = year, y = rolling_mean_R2, color = measure, group = measure)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  #geom_ribbon(aes(ymin = rolling_mean_R2 - rolling_se_R2, ymax = rolling_mean_R2 + rolling_se_R2, fill = measure), 
  #            alpha = 0.2, color = NA) +
  labs(
    title = "10-Year Rolling Average of R2 by Year for All Measures Compared to CFO_GAAP_P",
    x = "Year",
    y = "10-Year Rolling Average R2"
  ) +
  theme_minimal() +
  theme(legend.position = "top")




#Analyzing time trend 
results2 <- results |> 
  mutate(trend = year - 1988,
         trend2 = if_else(year <= 1999,0,year - 1999))

summary3 <- data.frame()

for (var in vars) {
  
  #Estimate models with time trend by each measure
  m1 <- summary(lm(R2 ~ trend, results2 |> filter(measure == var)))
  
  #Extract coefficients and t-stat
  int_est <- data.frame(Measure=var,Statistic="Intercept",Estimate3=m1$coefficients[1,1])
  int_tstats <- data.frame(Measure=var,Statistic="T-Statistic",Estimate3=m1$coefficients[1,3])
  trend_est <- data.frame(Measure=var,Statistic="Trend",Estimate3=m1$coefficients[2,1])
  trend_tstats <- data.frame(Measure=var,Statistic="T-Statistic",Estimate3=m1$coefficients[2,3])
  
  #Combine
  summary3 <- rbind(summary3,rbind(int_est,int_tstats,trend_est,trend_tstats))
}







# Trend in Model 1: By SIC-5Year---------------------------------------------------------------------

#Create subset of firms to iterate over
regdata <- bondretdata |> group_by(SIC,fyear) |> mutate(obs = n()) |> filter(obs > 10) |> ungroup()

#Create list of cash flow measures to try
vars <- c("CFO_GAAP_P", "CFO_FIN_P", "CFO_TAX_P", "CFO_EBITDA_P","SFCF_P", "PYFCF_P", "FCFF_P", "FCFE_P","OE_P")
vars <- c("CFO_GAAP_P","CFO_EBITDA_P","SFCF_P","OE_P")

#Create list of industries to loop over
industries <- regdata |> select(SIC) |> distinct() |>pull()

#Create list of years to loop over
years <- regdata |> select(fyear) |> unique() |> arrange(fyear) |> pull()

#Create empty dataframe to hold results
results <- data.frame()

#Run loop over CF measures
for (var in vars) {
  
  #Run loop over industries
  for (ind in 1:length(industries)) {
    
    #Specify industry
    industry <- industries[ind]
    
    #Run loop over years
    for (yr in 1:length(years)) {
      
      #Specify year
      year <- years[yr]
      
      #Filter data for the specific industry and year
      regdata_subset <- regdata |> filter(SIC == industry, fyear <= year, fyear > (year-5))
      
      #Check if there are enough observations for the regression
      if (nrow(regdata_subset) > 0) {
        
        #Run models
        formula <- as.formula(paste("mktadj_ret_ddp3 ~ ", var))
        model <- summary(lm(formula, data = regdata_subset))
        
        #Create data frame for the current results
        ind_result <- data.frame(industry = industry, year = year,measure = var, R2 = model$adj.r.squared)
        
        #Add to running results dataframe
        results <- rbind(results, ind_result)
      }
    }
  }
}


plot_data <- results |> 
  filter(year>1995,year<2024) |> 
  group_by(measure,year) |> 
  summarise(mean_R2 = mean(R2,na.rm=T),
            se_R2 = mean(R2,na.rm=T)) %>% 
  filter(measure%in%c("CFO_GAAP_P","CFO_EBITDA_P","SFCF_P"))

# Create the plot
ggplot(plot_data, aes(x = year, y = mean_R2, color = measure, group = measure)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  #geom_ribbon(aes(ymin = mean_R2 - se_R2, ymax = mean_R2 + se_R2, fill = measure), 
  #            alpha = 0.2, color = NA) +
  labs(
    title = "10-Year Rolling Average of R2 by Year for All Measures Compared to CFO_GAAP_P",
    x = "Year",
    y = "5-Year Rolling Average R2"
  ) +
  theme_minimal() +
  theme(legend.position = "top")




#Analyzing time trend 
results2 <- results |> 
  mutate(trend = year - 1988,
         trend2 = if_else(year <= 1999,0,year - 1999))

summary3 <- data.frame()

for (var in vars) {
  
  #Estimate models with time trend by each measure
  m1 <- summary(lm(R2 ~ trend, results2 |> filter(measure == var)))
  
  #Extract coefficients and t-stat
  int_est <- data.frame(Measure=var,Statistic="Intercept",Estimate3=m1$coefficients[1,1])
  int_tstats <- data.frame(Measure=var,Statistic="T-Statistic",Estimate3=m1$coefficients[1,3])
  trend_est <- data.frame(Measure=var,Statistic="Trend",Estimate3=m1$coefficients[2,1])
  trend_tstats <- data.frame(Measure=var,Statistic="T-Statistic",Estimate3=m1$coefficients[2,3])
  
  #Combine
  summary3 <- rbind(summary3,rbind(int_est,int_tstats,trend_est,trend_tstats))
}







# Model 1: By SIC4 with Firm FE (CFO and CFI) ----------------------------------------------------------------------------------

#Subset those observations with sufficient CRSP data for return calc
regdata <- bondretdata |> filter(!is.na(mktadj_ret_ddp3))

#Create subset of industries to iterate over
industries <- regdata |> group_by(SIC) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(industries)){
  
  #Specify industry
  industry <- industries[[i,1]]
  
  #Run models
  model_1 <- summary(feols(mktadj_ret_ddp3 ~ CFI_EBITDA_P|gvkey, data = regdata |>  filter(SIC == industry)))
  
  #Extract r-squared
  model_1_rsq <- r2(model_1,type = "wr2")
  
  #Combine
  ind_result <- data.frame(model_1_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}


#Output results and test for difference in R2
#First calculate the average R-squared for each model
avg_r2 <- sapply(results, mean)
med_r2 <- sapply(results, median)

#Make variable names match measure used
results <- results |> 
  var_labels(model_1_rsq = "CFO_GAAP",
             model_2_rsq = "CFO_FIN",
             model_3_rsq = "CFO_TAX",
             model_4_rsq = "CFO_EBITDA",
             model_5_rsq = "SFCF",
             model_6_rsq = "PYFCF",
             model_7_rsq = "FCFE") |> 
  label_to_colnames()


#Calculate difference between first column (i.e. CFO) and each alternative measure  
#Calculate Wilcoxon signed-rank test and t-test of comparing each column to the first column (i.e. CFO) 
test_results <- lapply(2:ncol(results), function(i) {
  
  #Difference
  diff_rsq <- results[,i]-results[,1]
  mean_diff_rsqs <- mean(diff_rsq)
  med_diff_rsqs <- median(diff_rsq)
  
  #Wilcoxon signed rank test  
  wilcoxon_result <- wilcox.test(results[, 1], results[, i], paired = TRUE)
  p_value <- wilcoxon_result$p.value
  wilcoxon_z <- wilcoxonZ(results[,i],results[,1],paired=TRUE,correct=FALSE,digits = 5)
  
  #T-test
  t_result <- t.test(results[, i], results[, 1], paired = TRUE)
  t_stat <- t_result$statistic
  
  list(mean_diff_rsq = mean_diff_rsqs, statistic = t_stat, med_diff_rsq = med_diff_rsqs,p_value = p_value,z_stat = wilcoxon_z)
})

#Extract mean difference, t-test statistic, median difference, and p-value of Wilcoxon
mean_diff <- sapply(test_results, function(x) x["mean_diff_rsq"])
t_stats <- sapply(test_results, function(x) x["statistic"])
med_diff <- sapply(test_results, function(x) x["med_diff_rsq"])
z_stats <- sapply(test_results, function(x) x["z_stat"])

#Dataframe for model performance results
results_df <- data.frame(Model = colnames(results),
                         n = count(results),
                         Average_R2 = avg_r2,
                         Average_Diff_R2 = c(NA, unlist(mean_diff)),
                         T_Stat = c(NA, unlist(t_stats)),
                         Median_R2 = med_r2,
                         Median_Diff_R2 = c(NA, unlist(med_diff)),
                         Z_Stat = c(NA, unlist(z_stats))) 

#Make a table
results_table <- gt(results_df) |> 
  tab_header(title = "Regression Model Comparison",
             subtitle = "Average R-squared and Two-sided Wilcoxon Test Statistics and p-values") |> 
  fmt_number(columns = vars(Median_R2,Average_R2, T_Stat, Z_Stat,Average_Diff_R2,Median_Diff_R2),decimals = 3) |> 
  cols_label(Model = "Measures",
             n = "Obs",
             Average_R2 = "Mean R-squared",
             Average_Diff_R2 = "Mean Diff R-squared",
             T_Stat = "T-Statistic",
             Median_R2 = "Median R-squared",
             Median_Diff_R2 = "Median Diff R-squared",
             Z_Stat = "Z-Statistic")

print(results_table)

#Coverage from main sample = 99.9%
count(regdata |> group_by(SIC) |>  mutate(obs = n()) |> ungroup()|>  filter(obs > 14))/count(regdata)




# Model 1: By SIC4 with Firm FE (CFO and CFI and CFF) ----------------------------------------------------------------------------------

#Subset those observations with sufficient CRSP data for return calc
regdata <- bondretdata |> filter(!is.na(mktadj_ret_ddp3))

#Create subset of industries to iterate over
industries <- regdata |> group_by(SIC) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(industries)){
  
  #Specify industry
  industry <- industries[[i,1]]
  
  #Run models
  model_1 <- summary(feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFI_GAAP_P + CFF_GAAP_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_2 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FIN_P + CFI_FIN_P + CFF_FIN_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_3 <- summary(feols(mktadj_ret_ddp3 ~ CFO_TAX_P + CFI_TAX_P + CFF_TAX_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_4 <- summary(feols(mktadj_ret_ddp3 ~ CFO_EBITDA_P + CFI_EBITDA_P + CFF_EBITDA_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_5 <- summary(feols(mktadj_ret_ddp3 ~ CFO_SFCF_P + CFI_SFCF_P + CFF_SFCF_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_6 <- summary(feols(mktadj_ret_ddp3 ~ CFO_PYFCF_P + CFI_PYFCF_P + CFF_PYFCF_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_7 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFE_P + CFI_FCFE_P + CFF_FCFE_P|gvkey, data = regdata |>  filter(SIC == industry)))
  
  #Extract r-squared
  model_1_rsq <- r2(model_1,type = "war2")
  model_2_rsq <- r2(model_2,type = "war2")
  model_3_rsq <- r2(model_3,type = "war2")
  model_4_rsq <- r2(model_4,type = "war2")
  model_5_rsq <- r2(model_5,type = "war2")
  model_6_rsq <- r2(model_6,type = "war2")
  model_7_rsq <- r2(model_7,type = "war2")
  
  
  #Combine
  ind_result <- data.frame(model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,model_6_rsq,model_7_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}




#Output results and test for difference in R2
#First calculate the average R-squared for each model
avg_r2 <- sapply(results, mean)
med_r2 <- sapply(results, median)

#Make variable names match measure used
results <- results |> 
  var_labels(model_1_rsq = "CFO_GAAP",
             model_2_rsq = "CFO_FIN",
             model_3_rsq = "CFO_TAX",
             model_4_rsq = "CFO_EBITDA",
             model_5_rsq = "SFCF",
             model_6_rsq = "PYFCF",
             model_7_rsq = "FCFE") |> 
  label_to_colnames()


#Calculate difference between first column (i.e. CFO) and each alternative measure  
#Calculate Wilcoxon signed-rank test and t-test of comparing each column to the first column (i.e. CFO) 
test_results <- lapply(2:ncol(results), function(i) {
  
  #Difference
  diff_rsq <- results[,i]-results[,1]
  mean_diff_rsqs <- mean(diff_rsq)
  med_diff_rsqs <- median(diff_rsq)
  
  #Wilcoxon signed rank test  
  wilcoxon_result <- wilcox.test(results[, 1], results[, i], paired = TRUE)
  p_value <- wilcoxon_result$p.value
  wilcoxon_z <- wilcoxonZ(results[,i],results[,1],paired=TRUE,correct=FALSE,digits = 5)
  
  #T-test
  t_result <- t.test(results[, i], results[, 1], paired = TRUE)
  t_stat <- t_result$statistic
  
  list(mean_diff_rsq = mean_diff_rsqs, statistic = t_stat, med_diff_rsq = med_diff_rsqs,p_value = p_value,z_stat = wilcoxon_z)
})

#Extract mean difference, t-test statistic, median difference, and p-value of Wilcoxon
mean_diff <- sapply(test_results, function(x) x["mean_diff_rsq"])
t_stats <- sapply(test_results, function(x) x["statistic"])
med_diff <- sapply(test_results, function(x) x["med_diff_rsq"])
z_stats <- sapply(test_results, function(x) x["z_stat"])

#Dataframe for model performance results
results_df <- data.frame(Model = colnames(results),
                         n = count(results),
                         Average_R2 = avg_r2,
                         Average_Diff_R2 = c(NA, unlist(mean_diff)),
                         T_Stat = c(NA, unlist(t_stats)),
                         Median_R2 = med_r2,
                         Median_Diff_R2 = c(NA, unlist(med_diff)),
                         Z_Stat = c(NA, unlist(z_stats))) 

#Make a table
results_table <- gt(results_df) |> 
  tab_header(title = "Regression Model Comparison",
             subtitle = "Average R-squared and Two-sided Wilcoxon Test Statistics and p-values") |> 
  fmt_number(columns = vars(Median_R2,Average_R2, T_Stat, Z_Stat,Average_Diff_R2,Median_Diff_R2),decimals = 3) |> 
  cols_label(Model = "Measures",
             n = "Obs",
             Average_R2 = "Mean R-squared",
             Average_Diff_R2 = "Mean Diff R-squared",
             T_Stat = "T-Statistic",
             Median_R2 = "Median R-squared",
             Median_Diff_R2 = "Median Diff R-squared",
             Z_Stat = "Z-Statistic")

print(results_table)

#Coverage from main sample = 99.9%
count(regdata |> group_by(SIC) |>  mutate(obs = n()) |> ungroup()|>  filter(obs > 14))/count(regdata)

summary(lm(mkt))
bondretdata$cff

# Using Model 2 CFO, CFI, CFF----------------

bondretdata2 <- bondretdata|> group_by(SIC,gvkey) |> mutate(within_SIC_obs = n())|>filter(within_SIC_obs>1)

#Create subset of firms to iterate over
industries <- bondretdata2 |> group_by(SIC) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cfo_vars <- c("CFO_GAAP_P", "CFO_FIN_P", "CFO_TAX_P", "CFO_EBITDA_P", "CFO_SFCF_P", "CFO_PYFCF_P", "CFO_FCFE_P")
cfi_vars <- c("CFI_GAAP_P", "CFI_FIN_P", "CFI_TAX_P", "CFI_EBITDA_P", "CFI_SFCF_P", "CFI_PYFCF_P", "CFI_FCFE_P")
cff_vars <- c("CFF_GAAP_P", "CFF_FIN_P", "CFF_TAX_P", "CFF_EBITDA_P", "CFF_SFCF_P", "CFF_PYFCF_P", "CFF_FCFE_P")

#Create empty dataframe to hold output
output <- data.frame()

#Run loop over CF measures
for (j in 1:length(cfo_vars)) {
  
  #Create empty dataframe to hold results
  results <- data.frame()
  r2 <- c()  # Initialize R2 vector
  
  #Define CFO or CFI
  cfo_var <- cfo_vars[j]
  cfi_var <- cfi_vars[j]
  cff_var <- cff_vars[j]
  
  #Run loop over individual firms for that redefined cash flow measures
  for (i in 1:nrow(industries)) {
    
    #Specify firm gvkey
    industry <- industries$SIC[i]
    
    #Run models
    regdata <- bondretdata2 |> filter(SIC == industry)
    formula <- as.formula(paste("CHG_PRC ~ ", cfo_var, "+", cfi_var,"+", cff_var,"|gvkey"))
    model <- summary(feols(formula, data = regdata))
    
    #Extract coefficients
    coef_cfo <- model$coefficients[[1]]  # CFO
    coef_cfi <- model$coefficients[[2]]  # CFI
    coef_cff <- model$coefficients[[3]]  # CFF
    
    model_r2 <- r2(model,type = "war2")
    
    #Combine
    ind_result <- data.frame(coef_cfo, coef_cfi, coef_cff)
    
    #Add to results
    results <- rbind(results, ind_result)
    r2 <- c(r2, model_r2)
  }
  
  #Create list of CFO benchmark
  if (cfo_var == "CFO_GAAP_P") {benchmark_r2 <- r2} else {benchmark_r2 <- benchmark_r2}
  
  #Winsorize results for outliers
  results <- results |> mutate_all(winsorize_x)
  
  #Difference in R2; T-test; and Wilcoxon Signed Rank test for R2
  diff_r2 <- as.data.frame(cbind(benchmark_r2,r2)) |> mutate(diff = r2 - benchmark_r2)
  ttest <- t.test(r2, benchmark_r2, paired = TRUE)$statistic
  wilcox <- wilcoxonZ(r2,benchmark_r2, paired=TRUE,correct=FALSE,digits = 4)[[1]]
  
  #Now take average of yearly coefficients and calculate t-statistics
  ind_output <- rbind(
    data.frame(Variable = cfo_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(Variable = "", Output = "Test Statistic", as.data.frame(lapply(results, tstat)), Avg_R2 = "", Avg_Diff_R2 = round(ttest, 3), Med_R2 = "",Med_Diff_R2=round(wilcox, 3)))
  
  # Add to output
  output <- rbind(output, ind_output)
}

# Export
table <- flextable(output) |> colformat_double(j = c("coef_cfo", "coef_cfi","coef_cff","Avg_R2","Med_R2"), digits = 3)
table





summary(lm(cum_credret ~ IB_F + factor(SIC), bondretdata))
summary(lm(mktadj_ret_ddp3 ~ IB_P, bondretdata))

summary(lm(cum_credret ~ IB_F*LOSS + CFO_GAAP_F*LOSS, bondretdata %>% mutate(LOSS=if_else(IB_F<0,1,0))))
summary(lm(mktadj_ret_ddp3 ~ IB_P + CFO_GAAP_P, bondretdata))
