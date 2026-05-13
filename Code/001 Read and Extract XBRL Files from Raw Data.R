# Setup -----------------------------------------------------------------

library(dplyr)
library(stringr)
library(httr)
library(XML)
library(rvest)
library(stringr)
library(zip)
library(purrr)
library(tidyverse)

#Define file paths 
raw_data_path <- 'D:/XBRL/XBRL Raw Data/'
output_path <- 'D:/XBRL/XBRL Extracted Data/'
temp_path <- 'C:/Users/m221p165/temp/'

#Read and Extract TSV files from Raw XBRL Zip Files----------------------------------------------------------

#Manually downloaded XBRL zip folders from SEC at: https://www.sec.gov/dera/data/financial-statement-and-notes-data-set.html

#Create listing of XBRL zip files to search. Quarterly from 2009 Q1 - 2020 Q3. Monthly afterwards.
search <- tribble(~Year,~Period,
                  "2009","Q1","2009","Q2","2009","Q3","2009","Q4",
                  "2010","Q1","2010","Q2","2010","Q3","2010","Q4",
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



#For loop iterates through XBRL Zip files, extracts necessary files, and saves under new naming convention in different folder
for(i in 1:nrow(search)){
  
  #Look through search table for each period which corresponds to naming of zip files downloaded
  year <- search[[i,1]]
  period <- search[[i,2]]
  
  #Define XBRL path to raw file
  xbrl_raw <- paste0(raw_data_path,year,"_",period,"_notes.zip")
  
  #Extract necessary files into temporary folder
  unzip(xbrl_raw, files = c("num.tsv","tag.tsv","pre.tsv","sub.tsv"),exdir = temp_path)
  
  #Copy files to new directory and rename to match period
  file.copy(from = paste0(temp_path,"num.tsv"),to = paste0(output_path,year,"_",period,"_","num.tsv"))
  file.copy(from = paste0(temp_path,"tag.tsv"),to = paste0(output_path,year,"_",period,"_","tag.tsv"))
  file.copy(from = paste0(temp_path,"pre.tsv"),to = paste0(output_path,year,"_",period,"_","pre.tsv"))
  file.copy(from = paste0(temp_path,"sub.tsv"),to = paste0(output_path,year,"_",period,"_","sub.tsv"))
  
  #Remove all files in temp folder
  unlink(paste0(temp_path,"*"))
}





#Scratch --------------------------------------------------------------------------------
xbrl_raw <- paste0(raw_data_path,"2020_M12_notes.zip")

#Extract necessary files
unzip(xbrl_raw, files = c("num.tsv","tag.tsv","pre.tsv","sub.tsv"),exdir = temp_path)

#Save extracted datasets in XBRL Extracted Data folder
file.copy(from = paste0(temp_path,"num.tsv"),to = paste0(output_path,"num_",year,"_",period,".tsv"))


#Remove all files in temp folder
unlink(paste0(temp_path,"*"))

