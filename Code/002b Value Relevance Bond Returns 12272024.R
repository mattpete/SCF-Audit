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


# Load Bond Return Data------------------------------------------------------------------------------
bondretdata <- read_parquet(paste0(data_path,"bondret_data_12272024.parquet")) |> 
  filter(!is.na(CFO_GAAP_F),!is.na(FCF_BS_F), !is.na(CFO_EBITDA_F), !is.na(cum_credret),obs>8) |> 
  #mutate(FF49 = assign_FF49(sic)) |> 
  mutate_at(vars(starts_with("CF"),FCF_BS_F,FCF_SIM_F,FCF_GAAP_F,FCF_EQ_F,OE_F,IBC_F,NI_F,cum_credret),winsorize_x) |> 
  mutate(ln_mve = log(1+mve),
         neg_OE = if_else(OE_F<0,1,0),
         neg_CF = if_else(CFO_GAAP_F<0,1,0),
         Acc = IBC_F - CFO_GAAP_F,
         year = year(datadate_p3)) |> 
  #Creating change variables to compare with Easton et al. (2009)
  arrange(cusip, datadate_p3) |>  
  group_by(cusip) |>
  mutate(IBC_change = IBC_F - lag(IBC_F),
         OE_change = OE_F-lag(OE_F),
         CFO_change = CFO_GAAP_F - lag(CFO_GAAP_F),
         Acc_change = Acc - lag(Acc),
         Neg = if_else(IBC_change<0,1,0)) |>
  ungroup()

summary(feols(cum_credret ~ NI_F|0,bondretdata))
summary(feols(cum_credret ~ Acc_change + CFO_change ,bondretdata))
summary(feols(cum_credret ~ Acc_change*Neg + CFO_change*Neg ,bondretdata))

# Summary Statistics------------------------------------------

descrip <- bondretdata |> select(CFO_GAAP_F,CFO_FIN_F,CFO_STAX_F,CFO_EBITDA_F,CFO_RD_F,
                          FCF_GAAP_F,FCF_SIM_F,FCF_BS_F,FCF_EQ_F,IBC_F,
                          CFI_GAAP_F,CFI_FCFSIM_F,cum_credret,ln_mve) 

P10 <- function(x)quantile(x,0.1,na.rm=T)
P90 <- function(x)quantile(x,0.9,na.rm=T)

sum_stats <- datasummary( All(descrip) ~ N + P10  + P25 + Median + Mean + P75 + P90  + SD, 
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
  cum_credret = "Return",
  CFO_GAAP_P = bquote(CFO^GAAP),
  CFO_FIN_P = bquote(CFO^FIN),
  CFO_STAX_P = bquote(CFO^STAX),
  CFO_RD_P = bquote(CFO^RD),
  CFO_EBITDA_P = bquote(CFO^EBITDA),
  FCF_GAAP_P =  bquote(FCF^GAAP),
  FCF_SIM_P =  bquote(FCF^SIM),
  FCF_BS_P =  bquote(FCF^BS),
  FCF_EQ_P =  bquote(FCF^EQ),
  OE_P = "OE",
  CFI_GAAP_P = bquote(CFI^GAAP),
  CFI_FCFSIM_P = "CapEx")

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



#-----------------Model 1: Value Relevance using Bond Returns----------------------
# Model 1: By Bond (Just One Measure) ----------------------------------------------------------------------------------

#Subset those bonds with sufficient bond return data 
regdata <- bondretdata |> filter(!is.na(cum_credret))

#Create subset of bonds to iterate over
bonds <- regdata |> group_by(cusip) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(bonds)){
  
  #Specify bond
  ind_bond <- bonds[[i,1]]
  
  #Run models
  model_1 <- summary(lm(cum_credret ~ CFO_GAAP_F , data = regdata |>  filter(cusip == ind_bond)))
  model_2 <- summary(lm(cum_credret ~ CFO_FIN_F, data = regdata |>  filter(cusip == ind_bond)))
  model_3 <- summary(lm(cum_credret ~ CFO_STAX_F , data = regdata |>  filter(cusip == ind_bond)))
  model_4 <- summary(lm(cum_credret ~ CFO_EBITDA_F , data = regdata |>  filter(cusip == ind_bond)))
  model_5 <- summary(lm(cum_credret ~ CFO_RD_F, data = regdata |>  filter(cusip == ind_bond)))
  model_6 <- summary(lm(cum_credret ~ FCF_GAAP_F, data = regdata |>  filter(cusip == ind_bond)))
  model_7 <- summary(lm(cum_credret ~ FCF_SIM_F, data = regdata |>  filter(cusip == ind_bond)))
  model_8 <- summary(lm(cum_credret ~ FCF_BS_F, data = regdata |>  filter(cusip == ind_bond)))
  model_9 <- summary(lm(cum_credret ~ FCF_EQ_F, data = regdata |>  filter(cusip == ind_bond)))
  model_10 <- summary(lm(cum_credret ~ OE_F, data = regdata |>  filter(cusip == ind_bond)))
  
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

#Export to Word
table <- as_word(results_table)

officer::read_docx() |>  
  flextable::body_add_flextable(value=table) |>  
  print(target="C:/Users/m221p165/Dropbox/output.docx")

#Coverage from main sample = 8%
count(regdata |> group_by(cusip) |>  mutate(obs = n()) |> ungroup()|>  filter(obs > 14))/count(regdata)

# Model 1: By SIC4 with Bond FE (Just One Measure) ----------------------------------------------------------------------------------

#Subset those observations with sufficient CRSP data for return calc
regdata <- bondretdata |> filter(!is.na(cum_credret))

#Create subset of industries to iterate over
industries <- regdata |> group_by(sic) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(industries)){
  
  #Specify industry
  industry <- industries[[i,1]]
  
  #Run models
  model_1 <- summary(feols(cum_totbondret ~ CFO_GAAP_F |cusip, data = regdata |>  filter(sic == industry)))
  model_2 <- summary(feols(cum_totbondret ~ CFO_FIN_F |cusip, data = regdata |>  filter(sic == industry)))
  model_3 <- summary(feols(cum_totbondret ~ CFO_STAX_F |cusip, data = regdata |>  filter(sic == industry)))
  model_4 <- summary(feols(cum_totbondret ~ CFO_EBITDA_F |cusip, data = regdata |>  filter(sic == industry)))
  model_5 <- summary(feols(cum_totbondret ~ CFO_RD_F  |cusip, data = regdata |>  filter(sic == industry)))
  model_6 <- summary(feols(cum_totbondret ~ FCF_GAAP_F |cusip, data = regdata |>  filter(sic == industry)))
  model_7 <- summary(feols(cum_totbondret ~ FCF_SIM_F |cusip, data = regdata |>  filter(sic == industry)))
  model_8 <- summary(feols(cum_totbondret ~ FCF_BS_F |cusip, data = regdata |>  filter(sic == industry)))
  model_9 <- summary(feols(cum_totbondret ~ FCF_EQ_F |cusip, data = regdata |>  filter(sic == industry)))
  model_10 <- summary(feols(cum_totbondret ~ OE_F |cusip, data = regdata |>  filter(sic == industry)))
  
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
count(regdata |> group_by(SIC,CUSIP) |>  mutate(obs = n()) |> ungroup()|>  filter(obs > 1))/count(regdata)
write_csv(results,file = "C:/Users/m221p165/Dropbox/Dissertation/Statement of Cash Flows/output.csv")



# Plot Model 1: By SIC with Firm FE (Just One Measure) ----------------------------------------------------------------------------------

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
  model_1 <- summary(feols(mktadj_ret_ddp3 ~ CFO_GAAP_F |gvkey, data = regdata |>  filter(SIC == industry)))
  model_2 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FIN_F |gvkey, data = regdata |>  filter(SIC == industry)))
  model_3 <- summary(feols(mktadj_ret_ddp3 ~ CFO_TAX_F |gvkey, data = regdata |>  filter(SIC == industry)))
  model_4 <- summary(feols(mktadj_ret_ddp3 ~ CFO_EBITDA_F |gvkey, data = regdata |>  filter(SIC == industry)))
  model_5 <- summary(feols(mktadj_ret_ddp3 ~ CFBA_F  |gvkey, data = regdata |>  filter(SIC == industry)))
  model_6 <- summary(feols(mktadj_ret_ddp3 ~ SFCF_F |gvkey, data = regdata |>  filter(SIC == industry)))
  model_7 <- summary(feols(mktadj_ret_ddp3 ~ PYFCF_F |gvkey, data = regdata |>  filter(SIC == industry)))
  model_8 <- summary(feols(mktadj_ret_ddp3 ~ FCFE2_F |gvkey, data = regdata |>  filter(SIC == industry)))
  model_9 <- summary(feols(mktadj_ret_ddp3 ~ FCFF_F |gvkey, data = regdata |>  filter(SIC == industry)))
  model_10 <- summary(feols(mktadj_ret_ddp3 ~ OE_F |gvkey, data = regdata |>  filter(SIC == industry)))
  
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
  ind_result <- data.frame(industry, model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,model_6_rsq,model_7_rsq,model_8_rsq,model_9_rsq,model_10_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}


#Make variable names match measure used
results <- results |> 
  var_labels(model_1_rsq = "CFO_GAAP",
             model_2_rsq = "CFO_FIN",
             model_3_rsq = "CFO_TAX",
             model_4_rsq = "CFO_EBITDA",
             model_5_rsq = "CFBA",
             model_6_rsq = "SFCF",
             model_7_rsq = "PYFCF",
             model_8_rsq = "FCFE",
             model_9_rsq = "FCFF",
             model_10_rsq = "OE") |> 
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
    "SFCF" = "coral1",      
    "FCFE" = "darkorange3",      
    "CFBA" = "chocolate1"       
  )) 


#Create Industry Order for plot
industry_order <- results |>
  mutate(FF12 = as.factor(assign_FF12(floor(industry)))) |>
  select(FF12, starts_with("CFO"), SFCF, FCFE, FCFF,CFBA) |>
  pivot_longer(
    cols = c(starts_with("CFO"), SFCF, FCFE, FCFF,CFBA),
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
  select(FF12, CFO_GAAP, CFO_FIN, CFO_TAX, CFO_EBITDA) |>
  pivot_longer(
    cols = c(CFO_GAAP, CFO_FIN, CFO_TAX, CFO_EBITDA),
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
                    "CFO_TAX" = "cornflowerblue",
                    "CFO_EBITDA" = "deepskyblue4",
                    "SFCF" = "coral1",
                    "FCFE" = "darkorange3",
                    "CFBA" = "chocolate1",
                    "PYFCF" = "orange"),
                    labels = c(bquote(CFO^EBITDA),bquote(CFO^FIN),bquote(CFO^GAAP),bquote(CFO^TAX)))


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
regdata <- bondretdata |> filter(!is.na(mktadj_ret_ddp3))

#Create subset of firms to iterate over
firms <- regdata |> group_by(gvkey) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(firms)){
  
  #Specify firm
  ind_firm <- firms[[i,1]]
  
  #Run models
  model_1 <- summary(lm(mktadj_ret_ddp3 ~ CFO_GAAP_F + CFI_GAAP_F, data = regdata |>  filter(gvkey == ind_firm)))
  model_2 <- summary(lm(mktadj_ret_ddp3 ~ CFO_FIN_F + CFI_FIN_F , data = regdata |>  filter(gvkey == ind_firm)))
  model_3 <- summary(lm(mktadj_ret_ddp3 ~ CFO_STAX_F + CFI_STAX_F , data = regdata |>  filter(gvkey == ind_firm)))
  model_4 <- summary(lm(mktadj_ret_ddp3 ~ CFO_EBITDA_F + CFI_EBITDA_F , data = regdata |>  filter(gvkey == ind_firm)))
  model_5 <- summary(lm(mktadj_ret_ddp3 ~ CFO_SFCF_F + CFI_SFCF_F , data = regdata |>  filter(gvkey == ind_firm)))
  model_6 <- summary(lm(mktadj_ret_ddp3 ~ CFO_PYFCF_F + CFI_PYFCF_F, data = regdata |>  filter(gvkey == ind_firm)))
  model_7 <- summary(lm(mktadj_ret_ddp3 ~ CFO_FCFE_F + CFI_SFCF_F , data = regdata |>  filter(gvkey == ind_firm)))

  #Extract r-squared
  model_1_rsq <- model_1$r.squared
  model_2_rsq <- model_2$r.squared
  model_3_rsq <- model_3$r.squared
  model_4_rsq <- model_4$r.squared
  model_5_rsq <- model_5$r.squared
  model_6_rsq <- model_6$r.squared
  model_7_rsq <- model_7$r.squared

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
             model_4_rsq = "CEBITDA",
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



# Model 1: By SIC4 with Firm FE (CFO and CFI) ----------------------------------------------------------------------------------

#Subset those observations with sufficient CRSP data for return calc
regdata <- bondretdata |> filter(!is.na(cum_credret))

#Create subset of industries to iterate over
industries <- regdata |> group_by(SIC) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(industries)){
  
  #Specify industry
  industry <- industries[[i,1]]
  
  #Run models
  model_1 <- summary(feols(cum_credret ~ CFO_GAAP_F + CFI_GAAP_F|CUSIP, data = regdata |>  filter(SIC == industry)))
  model_2 <- summary(feols(cum_credret ~ CFO_FIN_F + CFI_FIN_F|CUSIP, data = regdata |>  filter(SIC == industry)))
  model_3 <- summary(feols(cum_credret ~ CFO_STAX_F + CFI_STAX_F|CUSIP, data = regdata |>  filter(SIC == industry)))
  model_4 <- summary(feols(cum_credret ~ CFO_EBITDA_F + CFI_EBITDA_F |CUSIP, data = regdata |>  filter(SIC == industry)))
  model_5 <- summary(feols(cum_credret ~ CFO_SFCF_F + CFI_SFCF_F |CUSIP, data = regdata |>  filter(SIC == industry)))
  model_6 <- summary(feols(cum_credret ~ CFO_PYFCF_F + CFI_PYFCF_F|CUSIP, data = regdata |>  filter(SIC == industry)))
  model_7 <- summary(feols(cum_credret ~ CFO_FCFE_F + CFI_SFCF_F|CUSIP, data = regdata |>  filter(SIC == industry)))

  #Extract r-squared
  model_1_rsq <- r2(model_1,type = "wr2")
  model_2_rsq <- r2(model_2,type = "wr2")
  model_3_rsq <- r2(model_3,type = "wr2")
  model_4_rsq <- r2(model_4,type = "wr2")
  model_5_rsq <- r2(model_5,type = "wr2")
  model_6_rsq <- r2(model_6,type = "wr2")
  model_7_rsq <- r2(model_7,type = "wr2")

  
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
             model_3_rsq = "CFO_STAX",
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

#Coverage from main sample = 97.8%
count(regdata |> group_by(SIC,gvkey) |>  mutate(obs = n()) |> ungroup()|>  filter(obs > 1))/count(regdata)



# Plot Model 2: By SIC with Firm FE (CFO and CFI) ----------------------------------------------------------------------------------


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
  model_1 <- summary(feols(mktadj_ret_ddp3 ~ CFO_GAAP_F + CFI_GAAP_F|gvkey, data = regdata |>  filter(SIC == industry)))
  model_2 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FIN_F + CFI_FIN_F|gvkey, data = regdata |>  filter(SIC == industry)))
  model_3 <- summary(feols(mktadj_ret_ddp3 ~ CFO_STAX_F + CFI_STAX_F|gvkey, data = regdata |>  filter(SIC == industry)))
  model_4 <- summary(feols(mktadj_ret_ddp3 ~ CFO_EBITDA_F + CFI_EBITDA_F |gvkey, data = regdata |>  filter(SIC == industry)))
  model_5 <- summary(feols(mktadj_ret_ddp3 ~ CFO_SFCF_F + CFI_SFCF_F |gvkey, data = regdata |>  filter(SIC == industry)))
  model_6 <- summary(feols(mktadj_ret_ddp3 ~ CFO_PYFCF_F + CFI_PYFCF_F|gvkey, data = regdata |>  filter(SIC == industry)))
  model_7 <- summary(feols(mktadj_ret_ddp3 ~ CFO_FCFE_F + CFI_SFCF_F|gvkey, data = regdata |>  filter(SIC == industry)))
  
  #Extract r-squared
  model_1_rsq <- r2(model_1,type = "wr2")
  model_2_rsq <- r2(model_2,type = "wr2")
  model_3_rsq <- r2(model_3,type = "wr2")
  model_4_rsq <- r2(model_4,type = "wr2")
  model_5_rsq <- r2(model_5,type = "wr2")
  model_6_rsq <- r2(model_6,type = "wr2")
  model_7_rsq <- r2(model_7,type = "wr2")
  
  
  #Combine
  ind_result <- data.frame(industry,model_1_rsq,model_2_rsq,model_3_rsq,model_4_rsq,model_5_rsq,model_6_rsq,model_7_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}

#Make variable names match measure used
results <- results |> 
  var_labels(model_1_rsq = "GAAP",
             model_2_rsq = "FIN",
             model_3_rsq = "TAX",
             model_4_rsq = "EBITDA",
             model_5_rsq = "SFCF",
             model_6_rsq = "PYFCF",
             model_7_rsq = "FCFE") |> 
  label_to_colnames()


#Create Industry Order for plot
industry_order <- results |>
  mutate(FF12 = as.factor(assign_FF12(industry))) |>
  pivot_longer(
    cols = c(GAAP, SFCF, FCFE, PYFCF),
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
  select(FF12, GAAP, SFCF, FCFE, PYFCF) |>
  pivot_longer(
    cols = c(GAAP, SFCF, FCFE, PYFCF),
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
    "GAAP" = "black",
    "FIN" = "cadetblue1",
    "TAX" = "cornflowerblue",
    "EBITDA" = "deepskyblue4",
    "SFCF" = "coral1",
    "FCFE" = "darkorange3",
    "CFBA" = "chocolate1",
    "PYFCF" = "orange"),
    labels = c("FCFE","GAAP","PYFCF","SFCF"))

test <- results |>
  mutate(FF12 = factor(assign_FF12(industry)))



# Model 1: By Year (CFO and CFI) ----------------------------------------------------------------------------------

#Subset those observations with sufficient CRSP data for return calc
regdata <- bondretdata |> filter(!is.na(mktadj_ret_ddp3))

#Create subset of industries to iterate over
years <- regdata |> group_by(fyear) |>  summarise(obs = n()) |>  filter(obs > 14)

#Create empty dataframe to hold results
results <- data.frame()

#Run loop
for (i in 1:nrow(years)){
  
  #Specify industry
  year <- years[[i,1]]
  
  #Run models
  model_1 <- summary(feols(mktadj_ret_ddp3 ~ CFO_GAAP_F |0, data = regdata |>  filter(fyear == year)))
  model_2 <- summary(feols(mktadj_ret_ddp3 ~ CFO_GAAP_F + CFI_GAAP_F|0, data = regdata |>  filter(fyear == year)))
  model_3 <- summary(feols(mktadj_ret_ddp3 ~ CFO_SFCF_F + CFI_SFCF_F|0, data = regdata |>  filter(fyear == year)))

  #Extract r-squared
  model_1_rsq <- r2(model_1,type = "r2")
  model_2_rsq <- r2(model_2,type = "r2")
  model_3_rsq <- r2(model_3,type = "r2")

  
  #Combine
  ind_result <- data.frame(model_1_rsq,model_2_rsq,model_3_rsq)
  
  #Add to results
  results <- rbind(results,ind_result)
}


#Output results and test for difference in R2
#First calculate the average R-squared for each model
avg_r2 <- sapply(results, mean)
med_r2 <- sapply(results, median)

#Make variable names match measure used
results <- results |> 
  var_labels(model_1_rsq = "OCF",
             model_2_rsq = "OCF + ICF",
             model_3_rsq = "OCF + CAPX") |> 
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

#Coverage from main sample = 97.8%
count(regdata |> group_by(SIC,gvkey) |>  mutate(obs = n()) |> ungroup()|>  filter(obs > 1))/count(regdata)




#-----------------Model 2: Incremental to Earnings----------------------
# Model 2: By SIC4 with Firm FE (Just One Measure)----------------------------------

bondretdata2 <- bondretdata |> group_by(sic,cusip) |> mutate(within_SIC_obs = n())|>filter(within_SIC_obs>2) |>ungroup()

#Create subset of firms to iterate over
industries <- bondretdata2 |> group_by(sic) |> summarise(obs = n()) |> filter(obs > 14)

#Create list of cash flow measures to try
cf_vars <- c("CFO_GAAP_F","CFO_FIN_F","CFO_STAX_F","CFO_EBITDA_F","CFO_RD_F","FCF_GAAP_F","FCF_SIM_F","FCF_BS_F","FCF_EQ_F")

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
    industry <- industries$sic[i]
    
    #Run models
    regdata <- bondretdata2 |> filter(sic == industry)
    formula <- as.formula(paste("cum_credret ~ OE_F +", cf_var,"|cusip"))
    #formula <- as.formula(paste("cum_credret ~ Acc_change + CFO_change |cusip"))
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


