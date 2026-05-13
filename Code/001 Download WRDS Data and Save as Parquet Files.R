# Set up--------------------------------------------
library(dplyr, warn.conflicts = FALSE)
library(RPostgres)
library(DBI)
library(ggplot2)
library(lubridate)
library(tidyr)
library(arrow)
library(haven)
library(farr)
library(dbplyr)
library(glue)
library(tidyverse)

#Home
data_path <- "C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Data/WRDS Parquet Files/"

#Work
data_path <- "C:/Users/m221p165/Dropbox/Dissertation/Statement of Cash Flows/Data/"

#Sign on to WRDS
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user=rstudioapi::askForSecret("WRDS username"),
                  password=rstudioapi::askForSecret("WRDS password"),
                  sslmode='require',
                  dbname='wrds')
wrds 


#Collect Data from WRDS and Save as Parquet ----------------------------------------------
#Set tables
comp.fundq <- tbl(wrds,in_schema("comp", "fundq"))
ccmxpf.link <- tbl(wrds,in_schema("crsp", "ccmxpf_lnkhist"))
crsp.dsi <- tbl(wrds,in_schema("crsp", "dsi"))
crsp.dsf <- tbl(wrds,in_schema("crsp", "dsf"))
crsp.stocknames <- tbl(wrds,in_schema("crsp", "stocknames"))

#Collect
fundq <- comp.fundq|> 
  filter(indfmt == "INDL", datafmt == "STD",
         consol == "C", popsrc == "D", year(datadate)>2010) |> 
  select(gvkey,datadate,rdq,fqtr) |> 
  collect()

ccmlink <- ccmxpf.link |> collect()

dsi <- crsp.dsi |> collect()

dsf <- crsp.dsf |> filter(year(date)>2010) |> collect()

stocknames <- crsp.stocknames |> collect()

#Save as Parquet Files
write_parquet(fundq,glue("{data_path}/comp_fundq.parquet"))
write_parquet(ccmlink,glue("{data_path}/ccmlink.parquet"))
write_parquet(dsi,glue("{data_path}/crsp_dsi.parquet"))
write_parquet(dsf,glue("{data_path}/crsp_dsf.parquet"))
write_parquet(stocknames,glue("{data_path}/stocknames.parquet"))
