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
library(kableExtra)
library(tidyverse)

#Home
data_path <- "C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Data/"
source("C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Code/Functions.R")

#Work
data_path <- "C:/Users/m221p165/Dropbox/Dissertation/Statement of Cash Flows/Data/"
source("C:/Users/m221p165/Dropbox/Dissertation/Statement of Cash Flows/Code/Functions.R")


# Load Data -------------------------------------------------------------------------------------
wrds <- read_parquet(paste0(data_path,"stockret_data_12272024.parquet")) |> 
  filter(!is.na(mktadj_ret_ddp3),!is.na(FCF_BS_P),!is.na(CFI_GAAP_P),!is.na(IBC_P),!is.na(mve)) |> 
  mutate(FF49 = assign_FF49(SIC),
         FF12 = assign_FF12(SIC),
         ln_mve = log(1+mve),
         RD_P = xrd/l_mve) |> 
  mutate_at(vars(starts_with("CF"),FCF_BS_P,FCF_SIM_P,D_P,FCF_EQ_P,EARN_P,OE_P,IBC_P,CHG_PRC,ln_mve,mktadj_ret_ddp3),winsorize_x)

# Sample Selection---------------------------------------------
#See "001 Value Relevance Data Collection" script for steps

#FYEAR b/w 1988 - 2023 and not missing OANCF and prior MVE not less than 0
step1_obs <- 229977

#Removing Financial Institutions (1 digit SIC not equal to 6)
step2_obs <- 190862
step2_diff <- (step2_obs-step1_obs)

#Removing Share Price less than 1, sales and assets greater than 10 M
step3_obs <- 133019
step3_diff <- (step3_obs-step2_obs)

#Merge to CRSP and missing data
step4_obs <- wrds |> count() |> pull()
step4_diff <- (step4_obs-step3_obs)


# Summary Statistics------------------------------------------

descrip <- wrds |> 
  select(CFO_GAAP_P,CFO_FIN_P,CFO_STAX_P,CFO_EBITDA_P,CFO_RD_P,FCF_GAAP_P,FCF_SIM_P,
         FCF_BS_P,FCF_EQ_P,OE_P,CFI_GAAP_P,CFI_FCFSIM_P,ln_mve,mktadj_ret_ddp3) |>
  var_labels(CFO_GAAP_P = "$CFO^{GAAP}$",
            CFO_FIN_P = "$CFO^{FIN}$",
            CFO_STAX_P = "$CFO^{STAX}$",
            CFO_EBITDA_P = "$CFO^{EBITDA}$",
            CFO_RD_P = "$CFO^{RD}$",
            FCF_GAAP_P = "$FCF^{GAAP}$",
            FCF_SIM_P = "$FCF^{SIM}$",
            FCF_BS_P = "$FCF^{BS}$",
            FCF_EQ_P = "$FCF^{EQ}$",
            CFI_GAAP_P = "$CFI^{GAAP}$",
            CFI_FCFSIM_P = "$CapEx$",
            OE_P = "$OE$",
            ln_mve = "$Ln(MVE)$",
            mktadj_ret_ddp3 = "$Return$") |> 
  label_to_colnames()
          

P10 <- function(x)quantile(x,0.1,na.rm=T)
P90 <- function(x)quantile(x,0.9,na.rm=T)

sum_stats <- datasummary( All(descrip) ~ N + P10  + P25 + Median + Mean + P75 + P90  + SD, 
             fmt = 3,
             data = descrip,
             escape = F,
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
  `$Return$` = "Return",
  `$CFO^{GAAP}$` = bquote(CFO^GAAP),
  `$CFO^{FIN}$` = bquote(CFO^FIN),
  `$CFO^{STAX}$` = bquote(CFO^STAX),
  `$CFO^{RD}$` = bquote(CFO^RD),
  `$CFO^{EBITDA}$` = bquote(CFO^EBITDA),
  `$FCF^{GAAP}$` =  bquote(FCF^GAAP),
  `$FCF^{SIM}$` =  bquote(FCF^SIM),
  `$FCF^{BS}$` =  bquote(FCF^BS),
  `$FCF^{EQ}$` =  bquote(FCF^EQ),
  `$OE$` = "OE",
  `$CFI^{GAAP}$` = bquote(CFI^GAAP),
  `$CapEx$` = "CapEx",
  `$Ln(MVE)$` = "Ln(MVE)")

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




#-----------------Model 1: Correlation with Returns----------------------
# Model 1: By Firm (Just One Measure) ----------------------------------------------------------------------------------

#Subset those firms with sufficient CRSP data for return calc
regdata <- wrds |> filter(!is.na(mktadj_ret_ddp3))

#Create subset of firms to iterate over
firms <- regdata |> group_by(gvkey) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(firms)){
  
  #Specify firm
  ind_firm <- firms[[i,1]]
  
  #Run models
  model_1 <- summary(lm(mktadj_ret_ddp3 ~ CFO_GAAP_P , data = regdata |>  filter(gvkey == ind_firm)))
  model_2 <- summary(lm(mktadj_ret_ddp3 ~ CFO_FIN_P, data = regdata |>  filter(gvkey == ind_firm)))
  model_3 <- summary(lm(mktadj_ret_ddp3 ~ CFO_STAX_P , data = regdata |>  filter(gvkey == ind_firm)))
  model_4 <- summary(lm(mktadj_ret_ddp3 ~ CFO_EBITDA_P , data = regdata |>  filter(gvkey == ind_firm)))
  model_5 <- summary(lm(mktadj_ret_ddp3 ~ CFO_RD_P , data = regdata |>  filter(gvkey == ind_firm)))
  model_6 <- summary(lm(mktadj_ret_ddp3 ~ FCF_GAAP_P, data = regdata |>  filter(gvkey == ind_firm)))
  model_7 <- summary(lm(mktadj_ret_ddp3 ~ FCF_SIM_P, data = regdata |>  filter(gvkey == ind_firm)))
  model_8 <- summary(lm(mktadj_ret_ddp3 ~ FCF_BS_P, data = regdata |>  filter(gvkey == ind_firm)))
  model_9 <- summary(lm(mktadj_ret_ddp3 ~ FCF_EQ_P, data = regdata |>  filter(gvkey == ind_firm)))
  model_10 <- summary(lm(mktadj_ret_ddp3 ~ OE_P, data = regdata |>  filter(gvkey == ind_firm)))
  
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
  ind_result <- data.frame(model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,
                           model_6_rsq,model_7_rsq,model_8_rsq,model_9_rsq,model_10_rsq)
  
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
             model_3_rsq = "CFO_STAX",
             model_4_rsq = "CFO_EBITDA",
             model_5_rsq = "CFO_RD",
             model_6_rsq = "FCF_GAAP",
             model_7_rsq = "FCF_SIM",
             model_8_rsq = "FCF_BS",
             model_9_rsq = "FCF_EQ",
             model_10_rsq = "OE") |> 
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
  wilcoxon_z <- wilcoxonZ(results[,i],results[,1],paired=TRUE,correct=FALSE,digits = 4)
  
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

#Export to Latex
results_df |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(n = format(n,big.mark=","),
         across(c(Average_R2,Average_Diff_R2,T_Stat,Median_R2,Median_Diff_R2,Z_Stat),~formatC(.,format = "f",digits=3)),
         Model = recode(
           Model,
           "CFO_GAAP" = "$CFO^{GAAP}$",
           "CFO_FIN" = "$CFO^{FIN}$",
           "CFO_STAX" = "$CFO^{STAX}$",
           "CFO_EBITDA" = "$CFO^{EBITDA}$",
           "CFO_RD" = "$CFO^{RD}$",
           "FCF_GAAP" = "$FCF^{GAAP}$",
           "FCF_SIM" = "$FCF^{SIM}$",
           "FCF_BS" = "$FCF^{BS}$",
           "FCF_EQ" = "$FCF^{EQ}$",
           "OE" = "$OE$")) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))

#Export to Word
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

table <- as_word(results_table)

officer::read_docx() |>  
  flextable::body_add_flextable(value=table) |>  
  print(target="C:/Users/m221p165/Dropbox/output.docx")


#Coverage from main sample = 56.3%
count(regdata |> group_by(gvkey) |>  mutate(obs = n()) |> ungroup()|>  filter(obs > 14))/count(regdata)

# Model 1: By SIC4 with Firm FE (Just One Measure) ----------------------------------------------------------------------------------

#Subset those observations with sufficient CRSP data for return calc
regdata <- wrds |> filter(!is.na(mktadj_ret_ddp3))

#Create subset of industries to iterate over
industries <- regdata |> group_by(SIC) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(industries)){
  
  #Specify industry
  industry <- industries[[i,1]]
  
  #Run models
  model_1 <- summary(feols(mktadj_ret_ddp3 ~ CFO_GAAP_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_2 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FIN_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_3 <- summary(feols(mktadj_ret_ddp3 ~ CFO_STAX_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_4 <- summary(feols(mktadj_ret_ddp3 ~ CFO_EBITDA_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_5 <- summary(feols(mktadj_ret_ddp3 ~ CFO_RD_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_6 <- summary(feols(mktadj_ret_ddp3 ~ FCF_GAAP_P  |gvkey, data = regdata |>  filter(SIC == industry)))
  model_7 <- summary(feols(mktadj_ret_ddp3 ~ FCF_SIM_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_8 <- summary(feols(mktadj_ret_ddp3 ~ FCF_BS_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_9 <- summary(feols(mktadj_ret_ddp3 ~ FCF_EQ_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_10 <- summary(feols(mktadj_ret_ddp3 ~ OE_P |gvkey, data = regdata |>  filter(SIC == industry)))
  
  #Extract r-squared
  model_1_rsq <- r2(model_1,type = "wr2")
  model_2_rsq <- r2(model_2,type = "wr2")
  model_3_rsq <- r2(model_3,type = "wr2")
  model_4_rsq <- r2(model_4,type = "wr2")
  model_5_rsq <- r2(model_5,type = "wr2")
  model_6_rsq <- r2(model_6,type = "wr2")
  model_7_rsq <- r2(model_7,type = "wr2")
  model_8_rsq <- r2(model_8,type = "wr2")
  model_9_rsq <- r2(model_9,type = "wr2")
  model_10_rsq <- r2(model_10,type = "wr2")
  
  
  #Combine
  ind_result <- data.frame(model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,
                           model_6_rsq,model_7_rsq,model_8_rsq,model_9_rsq,model_10_rsq)
  
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
             model_3_rsq = "CFO_STAX",
             model_4_rsq = "CFO_EBITDA",
             model_5_rsq = "CFO_RD",
             model_6_rsq = "FCF_GAAP",
             model_7_rsq = "FCF_SIM",
             model_8_rsq = "FCF_BS",
             model_9_rsq = "FCF_EQ",
             model_10_rsq = "OE") |> 
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

#Export to Latex
results_df |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(n = format(n,big.mark=","),
         across(c(Average_R2,Average_Diff_R2,T_Stat,Median_R2,Median_Diff_R2,Z_Stat),~formatC(.,format = "f",digits=3)),
         Model = recode(
           Model,
           "CFO_GAAP" = "$CFO^{GAAP}$",
           "CFO_FIN" = "$CFO^{FIN}$",
           "CFO_STAX" = "$CFO^{STAX}$",
           "CFO_EBITDA" = "$CFO^{EBITDA}$",
           "CFO_RD" = "$CFO^{RD}$",
           "FCF_GAAP" = "$FCF^{GAAP}$",
           "FCF_SIM" = "$FCF^{SIM}$",
           "FCF_BS" = "$FCF^{BS}$",
           "FCF_EQ" = "$FCF^{EQ}$",
           "OE" = "$OE$")) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))


#Export to Word
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

#Coverage from main sample = 97.9%
count(regdata |> group_by(SIC,gvkey) |>  mutate(obs = n()) |> ungroup()|>  filter(obs > 1))/count(regdata)
write_csv(results,file = "C:/Users/m221p165/Dropbox/Dissertation/Statement of Cash Flows/output.csv")



# Plot Model 1: By SIC with Firm FE (Just One Measure) ----------------------------------------------------------------------------------

#Subset those observations with sufficient CRSP data for return calc
regdata <- wrds |> filter(!is.na(mktadj_ret_ddp3))

#Create subset of industries to iterate over
industries <- regdata |> group_by(SIC) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(industries)){
  
  #Specify industry
  industry <- industries[[i,1]]
  
  #Run models
  model_1 <- summary(feols(mktadj_ret_ddp3 ~ CFO_GAAP_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_2 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FIN_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_3 <- summary(feols(mktadj_ret_ddp3 ~ CFO_STAX_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_4 <- summary(feols(mktadj_ret_ddp3 ~ CFO_EBITDA_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_5 <- summary(feols(mktadj_ret_ddp3 ~ CFO_RD_P  |gvkey, data = regdata |>  filter(SIC == industry)))
  model_6 <- summary(feols(mktadj_ret_ddp3 ~ FCF_GAAP_P  |gvkey, data = regdata |>  filter(SIC == industry)))
  model_7 <- summary(feols(mktadj_ret_ddp3 ~ FCF_SIM_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_8 <- summary(feols(mktadj_ret_ddp3 ~ FCF_BS_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_9 <- summary(feols(mktadj_ret_ddp3 ~ FCF_EQ_P |gvkey, data = regdata |>  filter(SIC == industry)))

  #Extract r-squared
  model_1_rsq <- r2(model_1,type = "wr2")
  model_2_rsq <- r2(model_2,type = "wr2")
  model_3_rsq <- r2(model_3,type = "wr2")
  model_4_rsq <- r2(model_4,type = "wr2")
  model_5_rsq <- r2(model_5,type = "wr2")
  model_6_rsq <- r2(model_6,type = "wr2")
  model_7_rsq <- r2(model_7,type = "wr2")
  model_8_rsq <- r2(model_8,type = "wr2")
  model_9_rsq <- r2(model_9,type = "wr2")

  
  #Combine
  ind_result <- data.frame(industry, model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,
                           model_6_rsq,model_7_rsq,model_8_rsq,model_9_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}


#Make variable names match measure used
results <- results |> 
  var_labels(model_1_rsq = "CFO_GAAP",
             model_2_rsq = "CFO_FIN",
             model_3_rsq = "CFO_STAX",
             model_4_rsq = "CFO_EBITDA",
             model_5_rsq = "CFO_RD",
             model_6_rsq = "FCF_GAAP",
             model_7_rsq = "FCF_SIM",
             model_8_rsq = "FCF_BS",
             model_9_rsq = "FCF_EQ") |> 
  label_to_colnames()

#Plot (Use Black for GAAP; Blue for Classification; Orange for FCF)
results |>
  mutate(FF12 = as.factor(assign_FF12(floor(industry)))) |>
  select(FF12,starts_with("CFO"),SFCF,FCFE,CFBA)|>
  pivot_longer(cols = c(starts_with("CFO"),SFCF,FCFE,CFBA),
               names_to = "Model",
               values_to = "R2") |>
  group_by(FF12,Model) |>
  summarise(Obs = n(), Avg_R2 = median(R2, na.rm = TRUE)) |>
  ggplot(aes(x = FF12, y = Avg_R2, fill=Model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Median R² Values by Industry",
       x = "Industry (FF12)",
       y = "Median R²") +
  theme_bw()+
  scale_fill_manual(values = c(
    "CFO_GAAP" = "black",  
    "CFO_FIN" = "cadetblue1",   
    "CFO_TAX" = "cornflowerblue",   
    "CFO_EBITDA" = "deepskyblue4", 
    "FCF_SIM" = "coral1",      
    "FCF_EQ" = "darkorange3",      
    "FCF_GAAP" = "chocolate1"       
  )) 


#Create Industry Order for plot
industry_order <- results |>
  mutate(FF12 = as.factor(assign_FF12(floor(industry)))) |>
  select(FF12, starts_with("CFO"), FCF_GAAP, FCF_SIM, FCF_BS, FCF_EQ) |>
  pivot_longer(
    cols = c(starts_with("CFO"), FCF_GAAP, FCF_SIM, FCF_BS, FCF_EQ),
    names_to = "Model",
    values_to = "R2") |>
  filter(Model == "CFO_GAAP") |>
  group_by(FF12) |>
  summarise(Median_R2_GAAP = median(R2, na.rm = TRUE)) |>
  arrange(Median_R2_GAAP) |>
  pull(FF12)

#Plotting CFO Measures
results |>
  mutate(FF12 = factor(assign_FF12(floor(industry)), levels = industry_order)) |>
  select(FF12, CFO_GAAP, CFO_FIN, CFO_STAX, CFO_EBITDA, CFO_RD) |>
  pivot_longer(
    cols = c(CFO_GAAP, CFO_FIN, CFO_STAX, CFO_EBITDA, CFO_RD),
    names_to = "Model",
    values_to = "R2") |>
  group_by(FF12, Model) |>
  summarise(Obs = n(), Med_R2 = median(R2, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = FF12, y = Med_R2, fill = Model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Median R² Values by Industry (Fama-French 12)",
    x = "Industry (FF12)",
    y = "Median R²" ) +
  theme_bw() +
  scale_fill_manual(values = c(
                    "CFO_GAAP" = "black",
                    "CFO_FIN" = "cadetblue1",
                    "CFO_STAX" = "cornflowerblue",
                    "CFO_EBITDA" = "deepskyblue4",
                    "CFO_RD" = "deepskyblue2",
                    
                    "SFCF" = "coral1",
                    "FCFE" = "darkorange3",
                    "CFBA" = "chocolate1",
                    "PYFCF" = "orange"),
                    labels = c(bquote(CFO^EBITDA),bquote(CFO^FIN),bquote(CFO^GAAP),bquote(CFO^RD),bquote(CFO^STAX)))


#Plotting FCF Measures
results |>
  mutate(FF12 = factor(assign_FF12(floor(industry)), levels = industry_order)) |>
  select(FF12, CFO_GAAP, SFCF, FCFE, PYFCF, CFBA) |>
  pivot_longer(
    cols = c(CFO_GAAP, SFCF, FCFE, PYFCF, CFBA),
    names_to = "Model",
    values_to = "R2") |>
  group_by(FF12, Model) |>
  summarise(Obs = n(), Med_R2 = median(R2, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = FF12, y = Med_R2, fill = Model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Median R² Values by Industry",
       x = "Industry (FF12)",
       y = "Median R²" ) +
  theme_bw() +
  scale_fill_manual(values = c(
    "CFO_GAAP" = "black",
    "CFO_FIN" = "cadetblue1",
    "CFO_TAX" = "cornflowerblue",
    "CFO_EBITDA" = "deepskyblue4",
    "SFCF" = "coral1",
    "FCFE" = "darkorange3",
    "CFBA" = "chocolate1",
    "PYFCF" = "orange"),
    labels = c("CFBA",bquote(CFO^GAAP),"FCFE","PYFCF","SFCF"))




# Model 1: By Firm (CFO and CFI) ----------------------------------------------------------------------------------

#Subset those firms with sufficient CRSP data for return calc
regdata <- wrds |> filter(!is.na(mktadj_ret_ddp3))

#Create subset of firms to iterate over
firms <- regdata |> group_by(gvkey) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(firms)){
  
  #Specify firm
  ind_firm <- firms[[i,1]]
  
  #Run models
  model_1 <- summary(lm(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFI_GAAP_P, data = regdata |>  filter(gvkey == ind_firm)))
  model_2 <- summary(lm(mktadj_ret_ddp3 ~ CFO_FIN_P + CFI_FIN_P , data = regdata |>  filter(gvkey == ind_firm)))
  model_3 <- summary(lm(mktadj_ret_ddp3 ~ CFO_STAX_P + CFI_STAX_P , data = regdata |>  filter(gvkey == ind_firm)))
  model_4 <- summary(lm(mktadj_ret_ddp3 ~ CFO_EBITDA_P + CFI_EBITDA_P , data = regdata |>  filter(gvkey == ind_firm)))
  model_5 <- summary(lm(mktadj_ret_ddp3 ~ CFO_RD_P + CFI_RD_P , data = regdata |>  filter(gvkey == ind_firm)))
  model_6 <- summary(lm(mktadj_ret_ddp3 ~ CFO_FCFSIM_P + CFI_FCFSIM_P , data = regdata |>  filter(gvkey == ind_firm)))
  model_7 <- summary(lm(mktadj_ret_ddp3 ~ CFO_FCFBS_P + CFI_FCFBS_P, data = regdata |>  filter(gvkey == ind_firm)))
  model_8 <- summary(lm(mktadj_ret_ddp3 ~ CFO_FCFEQ_P + CFI_FCFEQ_P , data = regdata |>  filter(gvkey == ind_firm)))

  #Extract r-squared
  model_1_rsq <- model_1$r.squared
  model_2_rsq <- model_2$r.squared
  model_3_rsq <- model_3$r.squared
  model_4_rsq <- model_4$r.squared
  model_5_rsq <- model_5$r.squared
  model_6_rsq <- model_6$r.squared
  model_7_rsq <- model_7$r.squared
  model_8_rsq <- model_8$r.squared
  
  #Combine
  ind_result <- data.frame(model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,model_6_rsq,model_7_rsq,model_8_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}


#Output results and test for difference in R2
#First calculate the average R-squared for each model
avg_r2 <- sapply(results, mean)
med_r2 <- sapply(results, median)



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
results_df <- data.frame(CFO = c("$CFO^{GAAP}$","$CFO^{FIN}$","$CFO^{STAX}$","$CFO^{EBITDA}$",
                                 "$CFO^{RD}$","$CFO^{SIM}$","$CFO^{BS}$","$CFO^{EQ}$"),
                         CFI = c("$CFI^{GAAP}$","$CFI^{FIN}$","$CFI^{STAX}$","$CFI^{EBITDA}$",
                                 "$CFI^{RD}$","$CFI^{SIM}$","$CFI^{BS}$","$CFI^{EQ}$"),
                         n = count(results),
                         Average_R2 = avg_r2,
                         Average_Diff_R2 = c(NA, unlist(mean_diff)),
                         T_Stat = c(NA, unlist(t_stats)),
                         Median_R2 = med_r2,
                         Median_Diff_R2 = c(NA, unlist(med_diff)),
                         Z_Stat = c(NA, unlist(z_stats))) 

#Export to Latex
results_df |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(n = format(n,big.mark=","),
         across(c(Average_R2,Average_Diff_R2,T_Stat,Median_R2,Median_Diff_R2,Z_Stat),~formatC(.,format = "f",digits=3))) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))



#Make a table
results_table <- gt(results_df) |> 
  tab_header(title = "Regression Model Comparison",
             subtitle = "Average R-squared and Two-sided Wilcoxon Test Statistics and p-values") |> 
  fmt_number(columns = vars(Median_R2,Average_R2, T_Stat, Z_Stat,Average_Diff_R2,Median_Diff_R2),decimals = 3) |> 
  cols_label(CFO = "CFO",
             CFI = "CFI",
             n = "Obs",
             Average_R2 = "Mean R-squared",
             Average_Diff_R2 = "Mean Diff R-squared",
             T_Stat = "T-Statistic",
             Median_R2 = "Median R-squared",
             Median_Diff_R2 = "Median Diff R-squared",
             Z_Stat = "Z-Statistic")

print(results_table)



# Model 1: By SIC4 with Firm FE (CFO and CFI) ----------------------------------------------------------------------------------

#Subset those observations with sufficient CRSP data for return calc
regdata <- wrds |> filter(!is.na(mktadj_ret_ddp3))

#Create subset of industries to iterate over
industries <- regdata |> group_by(SIC) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(industries)){
  
  #Specify industry
  industry <- industries[[i,1]]
  
  #Run models
  model_1 <- summary(feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFI_GAAP_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_2 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FIN_P + CFI_FIN_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_3 <- summary(feols(mktadj_ret_ddp3 ~ CFO_STAX_P + CFI_STAX_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_4 <- summary(feols(mktadj_ret_ddp3 ~ CFO_EBITDA_P + CFI_EBITDA_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_5 <- summary(feols(mktadj_ret_ddp3 ~ CFO_RD_P + CFI_RD_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_6 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFSIM_P + CFI_FCFSIM_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_7 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFBS_P + CFI_FCFBS_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_8 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFEQ_P + CFI_FCFEQ_P|gvkey, data = regdata |>  filter(SIC == industry)))

  #Extract r-squared
  model_1_rsq <- r2(model_1,type = "wr2")
  model_2_rsq <- r2(model_2,type = "wr2")
  model_3_rsq <- r2(model_3,type = "wr2")
  model_4_rsq <- r2(model_4,type = "wr2")
  model_5_rsq <- r2(model_5,type = "wr2")
  model_6_rsq <- r2(model_6,type = "wr2")
  model_7_rsq <- r2(model_7,type = "wr2")
  model_8_rsq <- r2(model_8,type = "wr2")
  
  
  #Combine
  ind_result <- data.frame(model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,model_6_rsq,model_7_rsq,model_8_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}


#Output results and test for difference in R2
#First calculate the average R-squared for each model
avg_r2 <- sapply(results, mean)
med_r2 <- sapply(results, median)

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
results_df <- data.frame(CFO = c("$CFO^{GAAP}$","$CFO^{FIN}$","$CFO^{STAX}$","$CFO^{EBITDA}$",
                                 "$CFO^{RD}$","$CFO^{SIM}$","$CFO^{BS}$","$CFO^{EQ}$"),
                         CFI = c("$CFI^{GAAP}$","$CFI^{FIN}$","$CFI^{STAX}$","$CFI^{EBITDA}$",
                                 "$CFI^{RD}$","$CFI^{SIM}$","$CFI^{BS}$","$CFI^{EQ}$"),
                         n = count(results),
                         Average_R2 = avg_r2,
                         Average_Diff_R2 = c(NA, unlist(mean_diff)),
                         T_Stat = c(NA, unlist(t_stats)),
                         Median_R2 = med_r2,
                         Median_Diff_R2 = c(NA, unlist(med_diff)),
                         Z_Stat = c(NA, unlist(z_stats))) 

#Export to Latex
results_df |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(n = format(n,big.mark=","),
         across(c(Average_R2,Average_Diff_R2,T_Stat,Median_R2,Median_Diff_R2,Z_Stat),~formatC(.,format = "f",digits=3))) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))


#Make a table in viewer
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

#Coverage from main sample = 97.8%
count(regdata |> group_by(SIC,gvkey) |>  mutate(obs = n()) |> ungroup()|>  filter(obs > 1))/count(regdata)



# Plot Model 2: By SIC with Firm FE (CFO and CFI) ----------------------------------------------------------------------------------


#Subset those observations with sufficient CRSP data for return calc
regdata <- wrds |> filter(!is.na(mktadj_ret_ddp3))

#Create subset of industries to iterate over
industries <- regdata |> group_by(SIC) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(industries)){
  
  #Specify industry
  industry <- industries[[i,1]]
  
  #Run models
  model_1 <- summary(feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFI_GAAP_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_2 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FIN_P + CFI_FIN_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_3 <- summary(feols(mktadj_ret_ddp3 ~ CFO_STAX_P + CFI_STAX_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_4 <- summary(feols(mktadj_ret_ddp3 ~ CFO_EBITDA_P + CFI_EBITDA_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_5 <- summary(feols(mktadj_ret_ddp3 ~ CFO_RD_P + CFI_RD_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_6 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFSIM_P + CFI_FCFSIM_P |gvkey, data = regdata |>  filter(SIC == industry)))
  model_7 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFBS_P + CFI_FCFBS_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_8 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFEQ_P + CFI_FCFEQ_P|gvkey, data = regdata |>  filter(SIC == industry)))
  
  #Extract r-squared
  model_1_rsq <- r2(model_1,type = "wr2")
  model_2_rsq <- r2(model_2,type = "wr2")
  model_3_rsq <- r2(model_3,type = "wr2")
  model_4_rsq <- r2(model_4,type = "wr2")
  model_5_rsq <- r2(model_5,type = "wr2")
  model_6_rsq <- r2(model_6,type = "wr2")
  model_7_rsq <- r2(model_7,type = "wr2")
  model_8_rsq <- r2(model_8,type = "wr2")
  
  
  #Combine
  ind_result <- data.frame(industry,model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,
                           model_6_rsq,model_7_rsq,model_8_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}

#Make variable names match measure used
results <- results |> 
  var_labels(model_1_rsq = "GAAP",
             model_2_rsq = "FIN",
             model_3_rsq = "TAX",
             model_4_rsq = "EBITDA",
             model_5_rsq = "RD",
             model_6_rsq = "FCF_SIM",
             model_7_rsq = "FCF_BS",
             model_8_rsq = "FCF_EQ") |> 
  label_to_colnames()


#Create Industry Order for plot
industry_order <- results |>
  mutate(FF12 = as.factor(assign_FF12(industry))) |>
  pivot_longer(
    cols = c(GAAP, FCF_SIM, FCF_BS, FCF_EQ),
    names_to = "Model",
    values_to = "R2") |>
  filter(Model == "GAAP") |>
  group_by(FF12) |>
  summarise(Median_R2_GAAP = median(R2, na.rm = TRUE)) |>
  arrange(Median_R2_GAAP) |>
  pull(FF12)


#Plotting FCF Measures
results |>
  mutate(FF12 = factor(assign_FF12(industry), levels = industry_order)) |>
  select(FF12, GAAP, FCF_SIM, FCF_BS, FCF_EQ) |>
  pivot_longer(
    cols = c(GAAP, FCF_SIM, FCF_BS, FCF_EQ),
    names_to = "Model",
    values_to = "R2") |>
  group_by(FF12, Model) |>
  summarise(Obs = n(), Med_R2 = median(R2, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = FF12, y = Med_R2, fill = Model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Median R² Values by Industry (Fama-French 12)",
       x = "Industry (FF12)",
       y = "Median R²" ) +
  theme_bw() +
  scale_fill_manual(values = c(
    "GAAP" = "black",
    "FIN" = "cadetblue1",
    "TAX" = "cornflowerblue",
    "EBITDA" = "deepskyblue4",
    "FCF_SIM" = "coral1",
    "FCF_EQ" = "darkorange3",
    "CFBA" = "chocolate1",
    "FCF_BS" = "orange"),
    labels = c(bquote(FCF^BS),bquote(FCF^EQ),bquote(FCF^SIM),"GAAP"))



#-----------------Model 2: Incremental to Earnings----------------------
# Model 2: By Firm (Just One Measure)----------------------------------
#Create subset of firms to iterate over
firms <- wrds |> group_by(gvkey) |> summarise(obs = n()) |> filter(obs >= 14)

#Create list of cash flow measures to try
cf_vars <- c("CFO_GAAP_P","CFO_FIN_P","CFO_STAX_P","CFO_EBITDA_P","CFO_RD_P","FCF_GAAP_P","FCF_SIM_P","FCF_BS_P","FCF_EQ_P")

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
    formula <- as.formula(paste("CHG_PRC ~ EARN_P + D_P +", cf_var))
    model <- summary(lm(formula, data = wrds |> filter(gvkey == ind_firm)))
    
    #Extract coefficients
    #coef_intercept <- model$coefficients[[1]]  # Intercept
    coef_earnings <- model$coefficients[[2]]  # Earnings
    coef_dividends <- model$coefficients[[3]]  # Dividends
    coef_cashflow <- model$coefficients[[4]]  #  Cash Flow Measure


    model_r2 <- model$r.squared
    
    #Combine
    ind_result <- data.frame(coef_earnings, coef_dividends, coef_cashflow)
    
    #Add to results
    results <- rbind(results, ind_result)
    r2 <- c(r2, model_r2)
  }
  
  #Create list of CFO benchmark
  if (cf_var == "CFO_GAAP_P") {benchmark_r2 <- r2} else {benchmark_r2 <- benchmark_r2}
  
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
    data.frame(Variable = "", Output = "Test Statistic", as.data.frame(lapply(results, tstat)), 
               Avg_R2 = "", Avg_Diff_R2 = paste0("(",sprintf("%.3f", ttest),")"), Med_R2 = "", Med_Diff_R2=paste0("(",sprintf("%.3f", wilcox),")")))
  
  # Add to output
  output <- rbind(output, ind_output)
}


#Export to Latex
output |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(across(c(coef_earnings,coef_dividends,coef_cashflow, Avg_R2, Med_R2, Avg_Diff_R2, Med_Diff_R2),
                ~ ifelse(. != "" & !grepl("\\(", .), sprintf("%.3f", as.numeric(.)), .)),#Make all have 3 decimal
         Variable = recode(
           Variable,
           "CFO_GAAP_P" = "$CFO^{GAAP}$",
           "CFO_FIN_P" = "$CFO^{FIN}$",
           "CFO_STAX_P" = "$CFO^{STAX}$",
           "CFO_EBITDA_P" = "$CFO^{EBITDA}$",
           "CFO_RD_P" = "$CFO^{RD}$",
           "FCF_GAAP_P" = "$FCF^{GAAP}$",
           "FCF_SIM_P" = "$FCF^{SIM}$",
           "FCF_BS_P" = "$FCF^{BS}$",
           "FCF_EQ_P" = "$FCF^{EQ}$")) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))


#Export to Word
table <- flextable(output) |> colformat_double(j = c("coef_earnings", "coef_dividends", "coef_cashflow","Avg_R2","Med_R2"), digits = 3)
table


table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")


# Model 2: By Firm (Not PY Variables) ----------------------------------
#Create subset of firms to iterate over
firms <- wrds |> group_by(gvkey) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures
cf_vars <- c("CFO_GAAP_P","CFO_FIN_P","CFO_STAX_P","CFO_EBITDA_P","CFO_RD_P","FCF_GAAP_P","FCF_SIM_P","FCF_BS_P","FCF_EQ_P")

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
    formula <- as.formula(paste("mktadj_ret_ddp3 ~ IBC_P +", cf_var))
    model <- summary(lm(formula, data = wrds |> filter(gvkey == ind_firm)))
    
    #Extract coefficients
    #coef_intercept <- model$coefficients[[1]]  # Intercept
    coef_earnings <- model$coefficients[[2]]  # Earnings
    coef_cashflow <- model$coefficients[[3]]  #  Cash Flow Measure

    
    model_r2 <- model$r.squared
    
    #Combine
    ind_result <- data.frame(coef_earnings, coef_cashflow)
    
    #Add to results
    results <- rbind(results, ind_result)
    r2 <- c(r2, model_r2)
  }
  
  #Create list of CFO benchmark
  if (cf_var == "CFO_GAAP_P") {benchmark_r2 <- r2} else {benchmark_r2 <- benchmark_r2}
  
  #Winsorize results for outliers
  results <- results |> mutate_all(winsorize_x2)
  
  #Difference in R2; T-test; and Wilcoxon Signed Rank test for R2
  diff_r2 <- as.data.frame(cbind(benchmark_r2,r2)) |> mutate(diff = r2 - benchmark_r2)
  ttest <- t.test(r2, benchmark_r2, paired = TRUE)$statistic
  wilcox <- wilcoxonZ(r2,benchmark_r2, paired=TRUE,correct=FALSE,digits = 4)[[1]]
  
  #Now take average of firm coefficients and calculate t-statistics
  ind_output <- rbind(
    data.frame(Variable = cf_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(Variable = "", Output = "Test Statistic", as.data.frame(lapply(results, tstat)), 
               Avg_R2 = "", Avg_Diff_R2 = paste0("(",sprintf("%.3f", ttest),")"), Med_R2 = "", Med_Diff_R2=paste0("(",sprintf("%.3f", wilcox),")")))
  
  # Add to output
  output <- rbind(output, ind_output)
}


#Export to Latex
output |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(across(c(coef_earnings,coef_cashflow, Avg_R2, Med_R2, Avg_Diff_R2, Med_Diff_R2),
                ~ ifelse(. != "" & !grepl("\\(", .), sprintf("%.3f", as.numeric(.)), .)),#Make all have 3 decimal
         Variable = recode(
           Variable,
           "CFO_GAAP_P" = "$CFO^{GAAP}$",
           "CFO_FIN_P" = "$CFO^{FIN}$",
           "CFO_STAX_P" = "$CFO^{STAX}$",
           "CFO_EBITDA_P" = "$CFO^{EBITDA}$",
           "CFO_RD_P" = "$CFO^{RD}$",
           "FCF_GAAP_P" = "$FCF^{GAAP}$",
           "FCF_SIM_P" = "$FCF^{SIM}$",
           "FCF_BS_P" = "$FCF^{BS}$",
           "FCF_EQ_P" = "$FCF^{EQ}$")) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))

#Export to Word
table <- flextable(output) |> colformat_double(j = c("coef_earnings","coef_cashflow","Avg_R2","Med_R2"), digits = 3)
table


table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")


# Model 2: By SIC4 with Firm FE (Just One Measure)----------------------------------

wrds2 <- wrds |> group_by(SIC,gvkey) |> mutate(within_SIC_obs = n())|>filter(within_SIC_obs>1)

#Create subset of firms to iterate over
industries <- wrds2 |> group_by(SIC) |> summarise(obs = n()) |> filter(obs >= 14)

#Create list of cash flow measures to try
cf_vars <- c("CFO_GAAP_P","CFO_FIN_P","CFO_STAX_P","CFO_EBITDA_P","CFO_RD_P","FCF_GAAP_P","FCF_SIM_P","FCF_BS_P","FCF_EQ_P")

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
    regdata <- wrds2 |> filter(SIC == industry)
    formula <- as.formula(paste("CHG_PRC ~ EARN_P + D_P +", cf_var,"|gvkey"))
    model <- summary(feols(formula, data = regdata))
    
    #Extract coefficients
    coef_earnings <- model$coefficients[[1]]  # Earnings
    coef_dividends <- model$coefficients[[2]]  # Div
    coef_cashflow <- model$coefficients[[3]]  # Cash Flow

    
    model_r2 <- r2(model,type = "wr2")
    
    #Combine
    ind_result <- data.frame(coef_earnings, coef_dividends, coef_cashflow)
    
    #Add to results
    results <- rbind(results, ind_result)
    r2 <- c(r2, model_r2)
  }
  
  #Create list of CFO benchmark
  if (cf_var == "CFO_GAAP_P") {benchmark_r2 <- r2} else {benchmark_r2 <- benchmark_r2}
  
  #Winsorize results for outliers
  results <- results |> mutate_all(winsorize_x2)
  
  #Difference in R2; T-test; and Wilcoxon Signed Rank test for R2
  diff_r2 <- as.data.frame(cbind(benchmark_r2,r2)) |> mutate(diff = r2 - benchmark_r2)
  ttest <- t.test(r2, benchmark_r2, paired = TRUE)$statistic
  wilcox <- wilcoxonZ(r2,benchmark_r2, paired=TRUE,correct=FALSE,digits = 4)[[1]]
  
  #Now take average of industry coefficients and calculate t-statistics
  ind_output <- rbind(
    data.frame(Variable = cf_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(Variable = "", Output = "Test Statistic", as.data.frame(lapply(results, tstat)), 
               Avg_R2 = "", Avg_Diff_R2 = paste0("(",sprintf("%.3f", ttest),")"), Med_R2 = "", Med_Diff_R2=paste0("(",sprintf("%.3f", wilcox),")")))
  
  # Add to output
  output <- rbind(output, ind_output)
}


#Export to Latex
output |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(across(c(coef_earnings,coef_dividends,coef_cashflow, Avg_R2, Med_R2, Avg_Diff_R2, Med_Diff_R2),
                ~ ifelse(. != "" & !grepl("\\(", .), sprintf("%.3f", as.numeric(.)), .)),#Make all have 3 decimal
         Variable = recode(
           Variable,
           "CFO_GAAP_P" = "$CFO^{GAAP}$",
           "CFO_FIN_P" = "$CFO^{FIN}$",
           "CFO_STAX_P" = "$CFO^{STAX}$",
           "CFO_EBITDA_P" = "$CFO^{EBITDA}$",
           "CFO_RD_P" = "$CFO^{RD}$",
           "FCF_GAAP_P" = "$FCF^{GAAP}$",
           "FCF_SIM_P" = "$FCF^{SIM}$",
           "FCF_BS_P" = "$FCF^{BS}$",
           "FCF_EQ_P" = "$FCF^{EQ}$")) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))

#Export to Word
table <- flextable(output) |> colformat_double(j = c("coef_earnings", "coef_dividends", "coef_cashflow", "Avg_R2","Med_R2"), digits = 3)
table

table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")


# Model 2: By SIC4 with Firm FE (Not PY Variables)----------------------------------

wrds2 <- wrds |> group_by(SIC,gvkey) |> mutate(within_SIC_obs = n())|>filter(within_SIC_obs>1)

#Create subset of firms to iterate over
industries <- wrds2 |> group_by(SIC) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cf_vars <- c("CFO_GAAP_P","CFO_FIN_P","CFO_STAX_P","CFO_EBITDA_P","CFO_RD_P","FCF_GAAP_P","FCF_SIM_P","FCF_BS_P","FCF_EQ_P")

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
    regdata <- wrds2 |> filter(SIC == industry)
    formula <- as.formula(paste("mktadj_ret_ddp3 ~ IBC_P +", cf_var,"|gvkey"))
    model <- summary(feols(formula, data = regdata))
    
    #Extract coefficients
    coef_earnings <- model$coefficients[[1]]  # Earnings
    coef_cashflow <- model$coefficients[[2]]  # Cash Flow
    
    model_r2 <- r2(model,type = "wr2")
    
    #Combine
    ind_result <- data.frame(coef_earnings,coef_cashflow)
    
    #Add to results
    results <- rbind(results, ind_result)
    r2 <- c(r2, model_r2)
  }
  
  #Create list of CFO benchmark
  if (cf_var == "CFO_GAAP_P") {benchmark_r2 <- r2} else {benchmark_r2 <- benchmark_r2}
  
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
    data.frame(Variable = "", Output = "Test Statistic", as.data.frame(lapply(results, tstat)), 
               Avg_R2 = "", Avg_Diff_R2 = paste0("(",round(ttest, 3),")"), Med_R2 = "",Med_Diff_R2=paste0("(",round(wilcox, 3),")")))
  
  # Add to output
  output <- rbind(output, ind_output)
}


#Export to Latex
output |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(across(c(coef_earnings,coef_cashflow, Avg_R2, Med_R2, Avg_Diff_R2, Med_Diff_R2),
                ~ ifelse(. != "" & !grepl("\\(", .), sprintf("%.3f", as.numeric(.)), .)),#Make all have 3 decimal
         Variable = recode(
           Variable,
           "CFO_GAAP_P" = "$CFO^{GAAP}$",
           "CFO_FIN_P" = "$CFO^{FIN}$",
           "CFO_STAX_P" = "$CFO^{STAX}$",
           "CFO_EBITDA_P" = "$CFO^{EBITDA}$",
           "CFO_RD_P" = "$CFO^{RD}$",
           "FCF_GAAP_P" = "$FCF^{GAAP}$",
           "FCF_SIM_P" = "$FCF^{SIM}$",
           "FCF_BS_P" = "$FCF^{BS}$",
           "FCF_EQ_P" = "$FCF^{EQ}$")) |> 
  kbl(format = "latex", escape = FALSE, booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))


table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")



# Model 2: By Firm (CFO and CFI)----------------------------------
#Create subset of firms to iterate over
firms <- wrds |> group_by(gvkey) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cfo_vars <- c("CFO_GAAP_P","CFO_FIN_P","CFO_STAX_P","CFO_EBITDA_P","CFO_RD_P","CFO_FCFSIM_P","CFO_FCFBS_P","CFO_FCFEQ_P")
cfi_vars <- c("CFI_GAAP_P","CFI_FIN_P","CFI_STAX_P","CFI_EBITDA_P","CFI_RD_P","CFI_FCFSIM_P","CFI_FCFBS_P","CFI_FCFEQ_P")

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
    model <- summary(lm(formula, data = wrds |> filter(gvkey == ind_firm)))
    
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
    data.frame(CFO = cfo_var,CFI = cfi_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(CFO="", CFI="", Output = "Test Statistic", as.data.frame(lapply(results, tstat)),
               Avg_R2 = "", Avg_Diff_R2 = paste0("(",sprintf("%.3f", ttest),")"), Med_R2 = "", Med_Diff_R2=paste0("(",sprintf("%.3f", wilcox),")")))
  
  # Add to output
  output <- rbind(output, ind_output)
}


#Export to Latex
output |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(across(c(coef_earnings,coef_dividends,coef_cfo, coef_cfi,Avg_R2, Med_R2, Avg_Diff_R2, Med_Diff_R2),
                ~ ifelse(. != "" & !grepl("\\(", .), sprintf("%.3f", as.numeric(.)), .)),#Make all have 3 decimal
         CFO = recode(CFO,
                     "CFO_GAAP_P" = "$CFO^{GAAP}$",
                     "CFO_FIN_P" = "$CFO^{FIN}$",
                     "CFO_STAX_P" = "$CFO^{STAX}$",
                     "CFO_EBITDA_P" = "$CFO^{EBITDA}$",
                     "CFO_RD_P" = "$CFO^{RD}$",
                     "CFO_GAAP_P" = "$CFO^{GAAP}$",
                     "CFO_FCFSIM_P" = "$CFO^{SIM}$",
                     "CFO_FCFBS_P" = "$CFO^{BS}$",
                     "CFO_FCFEQ_P" = "$CFO^{EQ}$"),
         CFI = recode(CFI,
                      "CFI_GAAP_P" = "$CFI^{GAAP}$",
                      "CFI_FIN_P" = "$CFI^{FIN}$",
                      "CFI_STAX_P" = "$CFI^{STAX}$",
                      "CFI_EBITDA_P" = "$CFO^{EBITDA}$",
                      "CFI_RD_P" = "$CFI^{RD}$",
                      "CFI_GAAP_P" = "$CFI^{GAAP}$",
                      "CFI_FCFSIM_P" = "$CFI^{SIM}$",
                      "CFI_FCFBS_P" = "$CFI^{BS}$",
                      "CFI_FCFEQ_P" = "$CFI^{EQ}$")) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))


#Export to Word
table <- flextable(output) |> colformat_double(j = c("coef_earnings", "coef_dividends", "coef_cfo", "coef_cfi","Avg_R2","Med_R2"), digits = 3)
table


table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")


# Model 2: By SIC4 with Firm FE (CFO and CFI)----------------------------------

wrds2 <- wrds|> group_by(SIC,gvkey) |> mutate(within_SIC_obs = n())|>filter(within_SIC_obs>1)

#Create subset of firms to iterate over
industries <- wrds2 |> group_by(SIC) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cfo_vars <- c("CFO_GAAP_P","CFO_FIN_P","CFO_STAX_P","CFO_EBITDA_P","CFO_RD_P","CFO_FCFSIM_P","CFO_FCFBS_P","CFO_FCFEQ_P")
cfi_vars <- c("CFI_GAAP_P","CFI_FIN_P","CFI_STAX_P","CFI_EBITDA_P","CFI_RD_P","CFI_FCFSIM_P","CFI_FCFBS_P","CFI_FCFEQ_P")

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
    regdata <- wrds2 |> filter(SIC == industry,intpn!=0,txpd!=0)
    formula <- as.formula(paste("CHG_PRC ~ EARN_P + D_P +", cfo_var, "+", cfi_var,"|gvkey"))
    model <- summary(feols(formula, data = regdata))
    
    #Extract coefficients
    coef_earnings <- model$coefficients[[1]]  # Earnings
    coef_dividends <- model$coefficients[[2]]  # Div
    coef_cfo <- model$coefficients[[3]]  # CFO
    coef_cfi <- model$coefficients[[4]]  # CFI

    model_r2 <- r2(model,type = "wr2")
    
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
    data.frame(CFO = cfo_var,CFI = cfi_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(CFO="", CFI="", Output = "Test Statistic", as.data.frame(lapply(results, tstat)),
               Avg_R2 = "", Avg_Diff_R2 = paste0("(",sprintf("%.3f", ttest),")"), Med_R2 = "", Med_Diff_R2=paste0("(",sprintf("%.3f", wilcox),")")))
  
  # Add to output
  output <- rbind(output, ind_output)
}



#Export to Latex
output |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(across(c(coef_earnings,coef_dividends,coef_cfo, coef_cfi,Avg_R2, Med_R2, Avg_Diff_R2, Med_Diff_R2),
                ~ ifelse(. != "" & !grepl("\\(", .), sprintf("%.3f", as.numeric(.)), .)),#Make all have 3 decimal
         CFO = recode(CFO,
                      "CFO_GAAP_P" = "$CFO^{GAAP}$",
                      "CFO_FIN_P" = "$CFO^{FIN}$",
                      "CFO_STAX_P" = "$CFO^{STAX}$",
                      "CFO_EBITDA_P" = "$CFO^{EBITDA}$",
                      "CFO_RD_P" = "$CFO^{RD}$",
                      "CFO_GAAP_P" = "$CFO^{GAAP}$",
                      "CFO_FCFSIM_P" = "$CFO^{SIM}$",
                      "CFO_FCFBS_P" = "$CFO^{BS}$",
                      "CFO_FCFEQ_P" = "$CFO^{EQ}$"),
         CFI = recode(CFI,
                      "CFI_GAAP_P" = "$CFI^{GAAP}$",
                      "CFI_FIN_P" = "$CFI^{FIN}$",
                      "CFI_STAX_P" = "$CFI^{STAX}$",
                      "CFI_EBITDA_P" = "$CFO^{EBITDA}$",
                      "CFI_RD_P" = "$CFI^{RD}$",
                      "CFI_GAAP_P" = "$CFI^{GAAP}$",
                      "CFI_FCFSIM_P" = "$CFI^{SIM}$",
                      "CFI_FCFBS_P" = "$CFI^{BS}$",
                      "CFI_FCFEQ_P" = "$CFI^{EQ}$")) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))


# Export
table <- flextable(output) |> colformat_double(j = c("coef_earnings", "coef_dividends", "coef_cfo", "coef_cfi","Avg_R2","Med_R2"), digits = 3)
table

table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")

# Model 2: By SIC4 with Firm FE (CFO and CFI) NOT PY ----------------------------------

wrds2 <- wrds|> group_by(SIC,gvkey) |> mutate(within_SIC_obs = n())|>filter(within_SIC_obs>1)

#Create subset of firms to iterate over
industries <- wrds2 |> group_by(SIC) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cfo_vars <- c("CFO_GAAP_P","CFO_FIN_P","CFO_STAX_P","CFO_EBITDA_P","CFO_RD_P","CFO_FCFSIM_P","CFO_FCFBS_P","CFO_FCFEQ_P")
cfi_vars <- c("CFI_GAAP_P","CFI_FIN_P","CFI_STAX_P","CFI_EBITDA_P","CFI_RD_P","CFI_FCFSIM_P","CFI_FCFBS_P","CFI_FCFEQ_P")

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
    regdata <- wrds2 |> filter(SIC == industry)
    formula <- as.formula(paste("mktadj_ret_ddp3 ~ IBC_P +", cfo_var, "+", cfi_var,"|gvkey"))
    model <- summary(feols(formula, data = regdata))
    
    #Extract coefficients
    coef_earnings <- model$coefficients[[1]]  # Earnings
    coef_cfo <- model$coefficients[[2]]  # CFO
    coef_cfi <- model$coefficients[[3]]  # CFI

    model_r2 <- r2(model,type = "wr2")
    
    #Combine
    ind_result <- data.frame(coef_earnings, coef_cfo, coef_cfi)
    
    #Add to results
    results <- rbind(results, ind_result)
    r2 <- c(r2, model_r2)
  }
  
  #Create list of CFO benchmark
  if (cfo_var == "CFO_GAAP_P") {benchmark_r2 <- r2} else {benchmark_r2 <- benchmark_r2}
  
  #Winsorize results for outliers
  #results <- results |> mutate_all(winsorize_x)
  
  #Difference in R2; T-test; and Wilcoxon Signed Rank test for R2
  diff_r2 <- as.data.frame(cbind(benchmark_r2,r2)) |> mutate(diff = r2 - benchmark_r2)
  ttest <- t.test(r2, benchmark_r2, paired = TRUE)$statistic
  wilcox <- wilcoxonZ(r2,benchmark_r2, paired=TRUE,correct=FALSE,digits = 4)[[1]]
  
  #Now take average of yearly coefficients and calculate t-statistics
  ind_output <- rbind(
    data.frame(CFO = cfo_var,CFI = cfi_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(CFO="", CFI="", Output = "Test Statistic", as.data.frame(lapply(results, tstat)),
               Avg_R2 = "", Avg_Diff_R2 = paste0("(",sprintf("%.3f", ttest),")"), Med_R2 = "", Med_Diff_R2=paste0("(",sprintf("%.3f", wilcox),")")))
  
  # Add to output
  output <- rbind(output, ind_output)
}


#Export to Latex
output |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(across(c(coef_earnings,coef_cfo, coef_cfi,Avg_R2, Med_R2, Avg_Diff_R2, Med_Diff_R2),
                ~ ifelse(. != "" & !grepl("\\(", .), sprintf("%.3f", as.numeric(.)), .)),#Make all have 3 decimal
         CFO = recode(CFO,
                      "CFO_GAAP_P" = "$CFO^{GAAP}$",
                      "CFO_FIN_P" = "$CFO^{FIN}$",
                      "CFO_STAX_P" = "$CFO^{STAX}$",
                      "CFO_EBITDA_P" = "$CFO^{EBITDA}$",
                      "CFO_RD_P" = "$CFO^{RD}$",
                      "CFO_GAAP_P" = "$CFO^{GAAP}$",
                      "CFO_FCFSIM_P" = "$CFO^{SIM}$",
                      "CFO_FCFBS_P" = "$CFO^{BS}$",
                      "CFO_FCFEQ_P" = "$CFO^{EQ}$"),
         CFI = recode(CFI,
                      "CFI_GAAP_P" = "$CFI^{GAAP}$",
                      "CFI_FIN_P" = "$CFI^{FIN}$",
                      "CFI_STAX_P" = "$CFI^{STAX}$",
                      "CFI_EBITDA_P" = "$CFO^{EBITDA}$",
                      "CFI_RD_P" = "$CFI^{RD}$",
                      "CFI_GAAP_P" = "$CFI^{GAAP}$",
                      "CFI_FCFSIM_P" = "$CFI^{SIM}$",
                      "CFI_FCFBS_P" = "$CFI^{BS}$",
                      "CFI_FCFEQ_P" = "$CFI^{EQ}$")) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))



# Export
table <- flextable(output) |> colformat_double(j = c("coef_earnings", "coef_cfo", "coef_cfi","Avg_R2","Med_R2"), digits = 3)
table

table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")

#-------------------Extra Specifications ------------------------------
# Model 1: Pooled ----------------------------------------------------------------------------------
models <- list(
  "(1)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P |gvkey, data = wrds),
  "(2)"  = feols(mktadj_ret_ddp3 ~ CFO_FIN_P |gvkey, data = wrds),
  "(3)"  = feols(mktadj_ret_ddp3 ~ CFO_STAX_P |gvkey, data = wrds),
  "(4)"  = feols(mktadj_ret_ddp3 ~ CFO_EBITDA_P |gvkey, data = wrds),
  "(5)"  = feols(mktadj_ret_ddp3 ~ CFBA_P |gvkey, data = wrds),
  "(6)"  = feols(mktadj_ret_ddp3 ~ SFCF_P |gvkey, data = wrds),
  "(7)"  = feols(mktadj_ret_ddp3 ~ PYFCF_P |gvkey, data = wrds),
  "(8)"  = feols(mktadj_ret_ddp3 ~ FCFE_P |gvkey, data = wrds),
  "(9)"  = feols(mktadj_ret_ddp3 ~ OE_P |gvkey, data = wrds))

#Rename coefs to be generic measure
coef <- c("CFO_GAAP_P" = "Measure",
          "CFO_FIN_P" = "Measure",
          "CFO_STAX_P" = "Measure",
          "CFO_EBITDA_P" = "Measure",
          "CFBA_P" = "Measure",
          "SFCF_P" = "Measure",
          "PYFCF_P" = "Measure",
          "FCFE_P" = "Measure",
          "FCFF_P" = "Measure",
          "OE_P" = "Measure")

#Label measures across the top
FE_Row <- tribble(~term,~"(1)",~"(2)",~"(3)", ~"(4)",~"(5)",~"(6)",~"(7)",~"(8)", ~"(9)",
                  "","GAAP","CFO_FIN","CFO_STAX", "CFO_EBITDA","FCF_GAAP","FCF_SIM","FCF_BS","FCF_EQ","OE")
attr(FE_Row,"position") <- c(0)

panel <- modelsummary(models, 
                      #vcov =  ~ PERMNO + month_year,
                      statistic = "statistic",
                      stars = c('*' = .1, '**' = .05, '***' = .01) ,
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


# Model 1: Pooled Incremental ----------------------------------------------------------------------------------
models <- list(
  "(1)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P |gvkey, data = wrds),
  "(2)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFO_FIN_P |gvkey, data = wrds),
  "(3)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFO_STAX_P |gvkey, data = wrds),
  "(4)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFO_EBITDA_P |gvkey, data = wrds),
  "(5)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFBA_P |gvkey, data = wrds),
  "(6)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + SFCF_P |gvkey, data = wrds),
  "(7)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + PYFCF_P |gvkey, data = wrds),
  "(8)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + FCFE_P |gvkey, data = wrds),
  "(9)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + OE_P |gvkey, data = wrds))

#Rename coefs to be generic measure
coef <- c("CFO_GAAP_P" = "CFO_GAAP",
          "CFO_FIN_P" = "Measure",
          "CFO_STAX_P" = "Measure",
          "CFO_EBITDA_P" = "Measure",
          "CFBA_P" = "Measure",
          "SFCF_P" = "Measure",
          "PYFCF_P" = "Measure",
          "FCFE_P" = "Measure",
          "FCFF_P" = "Measure",
          "OE_P" = "Measure")

#Label measures across the top
FE_Row <- tribble(~term,~"(1)",~"(2)",~"(3)", ~"(4)",~"(5)",~"(6)",~"(7)",~"(8)", ~"(9)",
                  "","GAAP","CFO_FIN","CFO_STAX", "CFO_EBITDA","FCF_GAAP","FCF_SIM","FCF_BS","FCF_EQ","OE")
attr(FE_Row,"position") <- c(0)

panel <- modelsummary(models, 
                      #vcov =  ~ PERMNO + month_year,
                      statistic = "statistic",
                      stars = c('*' = .1, '**' = .05, '***' = .01) ,
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


# Model 1: By SIC4 with Firm FE (CFO, CFI, AND CFF) ----------------------------------------------------------------------------------

#Subset those observations with sufficient CRSP data for return calc
regdata <- wrds |> filter(!is.na(mktadj_ret_ddp3))

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
  model_3 <- summary(feols(mktadj_ret_ddp3 ~ CFO_STAX_P + CFI_STAX_P + CFF_FIN_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_4 <- summary(feols(mktadj_ret_ddp3 ~ CFO_EBITDA_P + CFI_EBITDA_P + CFF_EBITDA_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_5 <- summary(feols(mktadj_ret_ddp3 ~ CFO_RD_P + CFI_RD_P + CFF_RD_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_6 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFSIM_P + CFI_FCFSIM_P + CFF_FCFSIM_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_7 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFBS_P + CFI_FCFBS_P + CFF_FCFBS_P|gvkey, data = regdata |>  filter(SIC == industry)))
  model_8 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFEQ_P + CFI_FCFEQ_P + CFF_FCFBS_P|gvkey, data = regdata |>  filter(SIC == industry)))
  
  #Extract r-squared
  model_1_rsq <- r2(model_1,type = "wr2")
  model_2_rsq <- r2(model_2,type = "wr2")
  model_3_rsq <- r2(model_3,type = "wr2")
  model_4_rsq <- r2(model_4,type = "wr2")
  model_5_rsq <- r2(model_5,type = "wr2")
  model_6_rsq <- r2(model_6,type = "wr2")
  model_7_rsq <- r2(model_7,type = "wr2")
  model_8_rsq <- r2(model_8,type = "wr2")
  
  
  #Combine
  ind_result <- data.frame(model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,model_6_rsq,model_7_rsq,model_8_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}


#Output results and test for difference in R2
#First calculate the average R-squared for each model
avg_r2 <- sapply(results, mean)
med_r2 <- sapply(results, median)

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
results_df <- data.frame(Model = c("$CFO^{GAAP}$","$CFO^{FIN}$","$CFO^{STAX}$","$CFO^{EBITDA}$",
                                 "$CFO^{RD}$","$CFO^{SIM}$","$CFO^{BS}$","$CFO^{EQ}$"),
                         n = count(results),
                         Average_R2 = avg_r2,
                         Average_Diff_R2 = c(NA, unlist(mean_diff)),
                         T_Stat = c(NA, unlist(t_stats)),
                         Median_R2 = med_r2,
                         Median_Diff_R2 = c(NA, unlist(med_diff)),
                         Z_Stat = c(NA, unlist(z_stats))) 

#Export to Latex
results_df |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(n = format(n,big.mark=","),
         across(c(Average_R2,Average_Diff_R2,T_Stat,Median_R2,Median_Diff_R2,Z_Stat),~formatC(.,format = "f",digits=3))) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))


#Make a table in viewer
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

#Coverage from main sample = 97.8%
count(regdata |> group_by(SIC,gvkey) |>  mutate(obs = n()) |> ungroup()|>  filter(obs > 1))/count(regdata)




# Model 1: SUR ----------------------------------------------------------------------------------

sur <- wrds |> select(gvkey,fyear,SIC,buyhold_ret_ddp3,mktadj_ret_dd,mktadj_ret_ddp3,CFO_GAAP_P,CFO_FIN_P,CFO_STAX_P,CFO_EBITDA_P,
                      CFBA_P,SFCF_P,PYFCF_P,FCFE_P,OE_P)|>
  pivot_longer(cols = CFO_GAAP_P:OE_P, names_to = "Measure",values_to = "Value")|>
  mutate(AltCF = if_else(Measure == "CFO_GAAP_P","A_CFO_GAAP_P",Measure),
         FirmxAltCFFE = paste0(gvkey,"x",AltCF),
         SICxAltCFFE = paste0(SIC,"x",AltCF),
         YearxAltFE = paste0(fyear,"x",AltCF))

summary(feols(mktadj_ret_ddp3 ~ Value*factor(AltCF)|gvkey+ fyear+ FirmxAltCFFE +YearxAltFE,sur))

models <- list(
  "(1)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P |gvkey, data = wrds),
  "(2)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFO_FIN_P |gvkey, data = wrds),
  "(3)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFO_STAX_P |gvkey, data = wrds),
  "(4)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFO_EBITDA_P |gvkey, data = wrds),
  "(5)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + CFBA_P |gvkey, data = wrds),
  "(6)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + SFCF_P |gvkey, data = wrds),
  "(7)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + PYFCF_P |gvkey, data = wrds),
  "(8)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + FCFE_P |gvkey, data = wrds),
  "(9)"  = feols(mktadj_ret_ddp3 ~ CFO_GAAP_P + OE_P |gvkey, data = wrds))

#Rename coefs to be generic measure
coef <- c("CFO_GAAP_P" = "CFO_GAAP",
          "CFO_FIN_P" = "Measure",
          "CFO_STAX_P" = "Measure",
          "CFO_EBITDA_P" = "Measure",
          "CFBA_P" = "Measure",
          "SFCF_P" = "Measure",
          "PYFCF_P" = "Measure",
          "FCFE_P" = "Measure",
          "FCFF_P" = "Measure",
          "OE_P" = "Measure")

#Label measures across the top
FE_Row <- tribble(~term,~"(1)",~"(2)",~"(3)", ~"(4)",~"(5)",~"(6)",~"(7)",~"(8)", ~"(9)",
                  "","GAAP","CFO_FIN","CFO_STAX", "CFO_EBITDA","FCF_GAAP","FCF_SIM","FCF_BS","FCF_EQ","OE")
attr(FE_Row,"position") <- c(0)

panel <- modelsummary(models, 
                      #vcov =  ~ PERMNO + month_year,
                      statistic = "statistic",
                      stars = c('*' = .1, '**' = .05, '***' = .01) ,
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


# Model 1: By SIC2-Year ----------------------------------------------------------------------------------

#Create subset of industry-years to iterate over
ind_year <- wrds |> filter(!is.na(m12_mktadj_ret),fyear>1987) |> 
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
regdata <- wrds |> group_by(FF12, fyear) |> mutate(obs = n()) |> filter(obs > 14) |> ungroup()

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
regdata <- wrds |> group_by(FF49, fyear) |> mutate(obs = n()) |> filter(obs > 14) |> ungroup()

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


# Trend in Model 1: By SIC-5Year---------------------------------------------------------------------

#Create subset of firms to iterate over
regdata <- wrds |> group_by(SIC,fyear) |> mutate(obs = n()) |> filter(obs > 10) |> ungroup()

#Create list of cash flow measures to try
cf_vars <- c("CFO_GAAP_P","CFO_FIN_P","CFO_STAX_P","CFO_EBITDA_P","CFO_RD_P","FCF_GAAP_P","FCF_SIM_P","FCF_BS_P","FCF_EQ_P")

#Create list of industries to loop over
industries <- regdata |> select(SIC) |> distinct() |>pull()

#Create list of years to loop over
years <- regdata |> select(fyear) |> unique() |> arrange(fyear) |> pull()

#Create empty dataframe to hold results
results <- data.frame()

#Run loop over CF measures
for (var in cf_vars) {
  
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
        ind_result <- data.frame(industry = industry, year = year,measure = var, R2 = model$r.squared)
        
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
            se_R2 = mean(R2,na.rm=T)) |> 
  



# Create the plot
ggplot(plot_data, aes(x = year, y = mean_R2, color = measure, group = measure)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_R2 - se_R2, ymax = mean_R2 + se_R2, fill = measure), alpha = 0.2, color = NA) +
  labs(
    title = "5-Year Rolling Average of R2 by Year for All Measures Compared to CFO_GAAP_P",
    x = "Year",
    y = "5-Year Rolling Average R2"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

plot_data_facet <- plot_data %>%
  filter(measure == "CFO_GAAP_P") %>%
  rename(benchmark = measure) %>%
  bind_rows(
    plot_data %>%
      filter(measure != "CFO_GAAP_P") %>%
      mutate(facet_measure = measure)
  )

# Create the plot
ggplot(plot_data_facet, aes(x = year, y = mean_R2, color = measure, group = measure)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  #geom_ribbon(aes(ymin = mean_R2 - se_R2, ymax = mean_R2 + se_R2, fill = measure), alpha = 0.2, color = NA) +
  labs(
    title = "5-Year Rolling Average of R2 by Year for All Measures Compared to CFO_GAAP_P",
    x = "Year",
    y = "5-Year Rolling Average R2"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~benchmark, scales = "free_y")



#Analyzing time trend 
results2 <- results |> 
  mutate(trend = year - 1988,
         trend2 = if_else(year <= 1999,0,year - 1999))

summary3 <- data.frame()

for (var in cf_vars) {
  
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








# Model 2: By SIC4 with Firm FE (CFO, CFI, and CFF)----------------------------------

wrds2 <- wrds|> group_by(SIC,gvkey) |> mutate(within_SIC_obs = n())|>filter(within_SIC_obs>1)

#Create subset of firms to iterate over
industries <- wrds2 |> group_by(SIC) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cfo_vars <- c("CFO_GAAP_P","CFO_FIN_P","CFO_STAX_P","CFO_EBITDA_P","CFO_RD_P","CFO_FCFSIM_P","CFO_FCFBS_P","CFO_FCFEQ_P")
cfi_vars <- c("CFI_GAAP_P","CFI_FIN_P","CFI_STAX_P","CFI_EBITDA_P","CFI_RD_P","CFI_FCFSIM_P","CFI_FCFBS_P","CFI_FCFEQ_P")
cff_vars <- c("CFF_GAAP_P","CFF_FIN_P","CFF_STAX_P","CFF_EBITDA_P","CFF_RD_P","CFF_FCFSIM_P","CFF_FCFBS_P","CFF_FCFEQ_P")

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
    regdata <- wrds2 |> filter(SIC == industry)
    formula <- as.formula(paste("CHG_PRC ~ ", cfo_var, "+", cfi_var,"+", cff_var,"|gvkey"))
    model <- summary(feols(formula, data = regdata))
    
    #Extract coefficients
    coef_cfo <- model$coefficients[[1]]  # CFO
    coef_cfi <- model$coefficients[[2]]  # CFI
    coef_cff <- model$coefficients[[3]]  # CFF

    model_r2 <- r2(model,type = "wr2")
    
    #Combine
    ind_result <- data.frame(coef_cfo, coef_cfi,coef_cff)
    
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
    data.frame(CFO = cfo_var, CFI = cfi_var, CFF = cff_var, Output = "Avg. Coef. Est.", as.data.frame(lapply(results, mean)), 
               Avg_R2 = round(mean(r2), 3), Avg_Diff_R2 = round(mean(diff_r2$diff),3), Med_R2 = round(median(r2), 3), Med_Diff_R2 = round(median(diff_r2$diff),3)),
    data.frame(CFO="", CFI="", CFF = "", Output = "Test Statistic", as.data.frame(lapply(results, tstat)),
               Avg_R2 = "", Avg_Diff_R2 = paste0("(",sprintf("%.3f", ttest),")"), Med_R2 = "", Med_Diff_R2=paste0("(",sprintf("%.3f", wilcox),")")))
  
  # Add to output
  output <- rbind(output, ind_output)
}



#Export to Latex
output |> 
  rownames_to_column(var = "RowName") |> 
  select(-RowName) |> 
  mutate(across(c(coef_cfo, coef_cfi, coef_cff, Avg_R2, Med_R2, Avg_Diff_R2, Med_Diff_R2),
                ~ ifelse(. != "" & !grepl("\\(", .), sprintf("%.3f", as.numeric(.)), .)),#Make all have 3 decimal
         CFO = recode(CFO,
                      "CFO_GAAP_P" = "$CFO^{GAAP}$",
                      "CFO_FIN_P" = "$CFO^{FIN}$",
                      "CFO_STAX_P" = "$CFO^{STAX}$",
                      "CFO_EBITDA_P" = "$CFO^{EBITDA}$",
                      "CFO_RD_P" = "$CFO^{RD}$",
                      "CFO_GAAP_P" = "$CFO^{GAAP}$",
                      "CFO_FCFSIM_P" = "$CFO^{SIM}$",
                      "CFO_FCFBS_P" = "$CFO^{BS}$",
                      "CFO_FCFEQ_P" = "$CFO^{EQ}$"),
         CFI = recode(CFI,
                      "CFI_GAAP_P" = "$CFI^{GAAP}$",
                      "CFI_FIN_P" = "$CFI^{FIN}$",
                      "CFI_STAX_P" = "$CFI^{STAX}$",
                      "CFI_EBITDA_P" = "$CFO^{EBITDA}$",
                      "CFI_RD_P" = "$CFI^{RD}$",
                      "CFI_GAAP_P" = "$CFI^{GAAP}$",
                      "CFI_FCFSIM_P" = "$CFI^{SIM}$",
                      "CFI_FCFBS_P" = "$CFI^{BS}$",
                      "CFI_FCFEQ_P" = "$CFI^{EQ}$")) |> 
  kbl(format = "latex", escape = FALSE,booktabs = TRUE, caption = "Panel A: R2 from Firm Level Regressions") |> 
  kable_styling(latex_options = c("hold_position"))


# Export  
table <- flextable(output) |> colformat_double(j = c("coef_cfo","coef_cfi","coef_cff","Avg_R2","Med_R2"), digits = 3)
table

table |> save_as_docx(path = "C:/Users/m221p165/Dropbox/output.docx")

