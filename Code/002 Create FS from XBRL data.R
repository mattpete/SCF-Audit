# Setup - Startup -----------------------------------------------------------------

library(edgar)
library(dplyr)
library(stringr)
library(httr)
library(XML)
library(rvest)
library(stringr)
library(arrow)
library(tidyverse)


#Define file paths 
raw_data_path <- 'D:/XBRL/XBRL Extracted Data/'
output_path <- 'D:/XBRL/XBRL FS Data/'
temp_path <- 'C:/Users/m221p165/temp/'


#Looping through year-period (Quarter or Month) XBRL files to create disaggregation measures-------

#Create listing of XBRL files to search. Quarterly from 2009 Q1 - 2020 Q3. Monthly afterwards.
#Naming of files correspond to the years and periods in 'search' dataframe.

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



#Test search 
search <- tribble(~Year,~Period,
                  "2020","Q1")




#For loop iterates through periodic XBRL files, creates financial statements, and saves new periodic tidy data set as parquet

for(i in 1:nrow(search)){
  
  #Look through search table for each period which corresponds to naming of zip files downloaded
  year <- search[[i,1]]
  period <- search[[i,2]]
  
  #Read in raw XBRL files
  num <- read_delim(paste0(raw_data_path,year,"_",period,"_num.tsv"))
  tag <- read_delim(paste0(raw_data_path,year,"_",period,"_tag.tsv"))
  pre <- read_delim(paste0(raw_data_path,year,"_",period,"_pre.tsv"))
  sub <- read_delim(paste0(raw_data_path,year,"_",period,"_sub.tsv"))
  

  #Start with presentation dataset and fill in numbers
  pre <- pre %>%
    #Duplicates introduced for in-line parenthetical tags dropping because only want individual line items for this project
    filter(inpth == 0)%>%
    #Keeping Filing Accension Number (adsh), XBRL tag (tag), report number in filing (report), line number (line) and name of financial statement it appears in (stmt)
    select(adsh,tag,report,line,negating,stmt)%>% 
    mutate(face = if_else(stmt %in% c("BS","IS","CF","EQ","CI"),1,0),
           notes = if_else(face == 0,1,0),
           BS = coalesce(if_else(stmt == "BS",1,0),0L),
           IS = coalesce(if_else(stmt == "IS" | stmt == "CI",1,0),0L),
           CF = coalesce(if_else(stmt == "CF",1,0),0L),
           EQ = coalesce(if_else(stmt == "EQ",1,0),0L))%>%
    filter(face == 1) %>%
    distinct()
  
  gc()
  
  
  #Merge number file with tag file 
  #Number file is unique by columns but the tag can be used multiple times in one filing (checked Farmer Bros (cik = 34563) for 1Q 2024)
  #Tag file has tag description information and which yearly-taxonomy it comes from
  num <- num %>%
    #Restricting to just tag information I want now:
    #FASB taxonomy (version), type of data (i.e. dollar or shares) (datatype), tlabel (taxonomy description or firm provided), documentation (standard or firm provided)
    left_join(tag %>% select(tag,version,datatype,crdr,tlabel,doc),by=c("tag","version"))%>%
    #Occasional there are blank tags so dropping those
    filter(value!="")%>% 
    #Signals different facts for same primary key - filter out values greater than or equal to one (see manual)
    filter(iprx == 0) %>%
    #Filter to only USD or amounts in shares
    filter(uom == "USD" | uom == "share")
  
  gc()
  
  
  #Create FS dataset
  fs <- pre %>%
    #Most numbers will have dimension "0x00000000" which is parent 
    #Segment is most popular dimension field which introduces a lot of duplicates for the same tag
    left_join(num%>%filter(dimh=="0x00000000")%>%select(adsh,tag,version,ddate,qtrs,crdr,value),by=c("adsh","tag"))%>%
    rename(parent_value = value,
           parent_version = version,
           parent_ddate = ddate,
           parent_qtrs = qtrs,
           parent_crdr = crdr)%>%
    #Now account for instance where parent dimension is missing but tag is in FS with another dimension (e.g. payments to particular credit facility)
    left_join(num%>%filter(dimh!="0x00000000")%>%select(adsh,tag,version,ddate,qtrs,crdr,value),by=c("adsh","tag"))%>%
    rename(dim_value = value,
           dim_version = version,
           dim_ddate = ddate,
           dim_qtrs = qtrs,
           dim_crdr = crdr)%>%
    #Now coalesce the two which will account for instance where parent dimension is missing but tag is in FS with another dimension 
    mutate(value = coalesce(parent_value,dim_value),
           version = coalesce(parent_version,dim_version),
           ddate = coalesce(parent_ddate,dim_ddate),
           qtrs = coalesce(parent_qtrs,dim_qtrs),
           crdr = coalesce(parent_crdr,dim_crdr))%>%
    filter(!is.na(value),!is.na(ddate))%>%
    select(-starts_with("parent_"),-starts_with("dim_"))%>%
    distinct()%>%
    left_join(sub %>% select(adsh,cik,sic,afs,form,period,filed,accepted,nciks), by=c("adsh"))%>%
    filter(form %in% c("10-K","10-K/A","10-Q","10-Q/A"))
  
  gc()
  
   
  #Write to parquet file
  #write_parquet(fs,paste0(output_path,year,"_",period,"_FS.parquet"))
}




#Scratch-----------------
#Create FS Statements-------------------------------------------------

#Read in raw XBRL files
num <- read_delim('D:/XBRL/XBRL Extracted Data/2020_Q1_num.tsv')
tag <- read_delim('D:/XBRL/XBRL Extracted Data/2020_Q1_tag.tsv')
pre <- read_delim('D:/XBRL/XBRL Extracted Data/2020_Q1_pre.tsv')
sub <- read_delim('D:/XBRL/XBRL Extracted Data/2020_Q1_sub.tsv')



#Start with presentation dataset and fill in numbers
pre <- pre %>%
  #Duplicates introduced for in-line parenthetical tags dropping because only want individual line items for this project
  filter(inpth == 0)%>%
  #Keeping Filing Accension Number (adsh), XBRL tag (tag), report number in filing (report), line number (line) and name of financial statement it appears in (stmt)
  select(adsh,tag,report,line,negating,stmt)%>% 
  mutate(face = if_else(stmt %in% c("BS","IS","CF","EQ","CI"),1,0),
         notes = if_else(face == 0,1,0),
         BS = coalesce(if_else(stmt == "BS",1,0),0L),
         IS = coalesce(if_else(stmt == "IS" | stmt == "CI",1,0),0L),
         CF = coalesce(if_else(stmt == "CF",1,0),0L),
         EQ = coalesce(if_else(stmt == "EQ",1,0),0L))%>%
  filter(face == 1) %>%
  distinct()

gc()


#Merge number file with tag file 
#Number file is unique by columns but the tag can be used multiple times in one filing (checked Farmer Bros (cik = 34563) for 1Q 2024)
#Tag file has tag description information and which yearly-taxonomy it comes from
num <- num %>%
  #Restricting to just tag information I want now:
  #FASB taxonomy (version), type of data (i.e. dollar or shares) (datatype), tlabel (taxonomy description or firm provided), documentation (standard or firm provided)
  left_join(tag %>% select(tag,version,datatype,crdr,tlabel,doc),by=c("tag","version"))%>%
  #Occasional there are blank tags so dropping those
  filter(value!="")%>% 
  #Signals different facts for same primary key - filter out values greater than or equal to one (see manual)
  filter(iprx == 0) %>%
  #Filter to only USD or amounts in shares
  filter(uom == "USD" | uom == "share")



#Create FS dataset
fs1 <- pre %>%
  #Most numbers will have dimension "0x00000000" which is parent 
  #Segment is most popular dimension field which introduces a lot of duplicates for the same tag
  left_join(num%>%filter(dimh=="0x00000000")%>%select(adsh,tag,version,ddate,qtrs,crdr,value),by=c("adsh","tag"))%>%
  rename(parent_value = value,
         parent_version = version,
         parent_ddate = ddate,
         parent_qtrs = qtrs,
         parent_crdr = crdr)%>%
  #Now account for instance where parent dimension is missing but tag is in FS with another dimension (e.g. payments to particular credit facility)
  left_join(num%>%filter(dimh!="0x00000000")%>%select(adsh,tag,version,ddate,qtrs,crdr,value),by=c("adsh","tag"))%>%
  rename(dim_value = value,
         dim_version = version,
         dim_ddate = ddate,
         dim_qtrs = qtrs,
         dim_crdr = crdr)%>%
  #Now coalesce the two which will account for instance where parent dimension is missing but tag is in FS with another dimension 
  mutate(value = coalesce(parent_value,dim_value),
         version = coalesce(parent_version,dim_version),
         ddate = coalesce(parent_ddate,dim_ddate),
         qtrs = coalesce(parent_qtrs,dim_qtrs),
         crdr = coalesce(parent_crdr,dim_crdr))%>%
  filter(!is.na(value),!is.na(ddate))%>%
  select(-starts_with("parent_"),-starts_with("dim_"))%>%
  distinct()%>%
  left_join(sub %>% select(adsh,cik,sic,afs,form,period,filed,accepted,nciks), by=c("adsh"))%>%
  filter(form %in% c("10-K","10-K/A","10-Q","10-Q/A"))


gc()

test23 <- num |> filter(cik == "0000034067-24-000025",stmt == "IS",ddate ==20231231)

fs |> filter(cik==1552800) |> select(adsh) |> distinct()
test2 <-num |> filter(adsh=="0001144204-13-015837")


test4 <- fs1 |> filter(adsh=="0000010048-19-000022")


#Validation Files----------------------------------------------------------------------
#Process: Pick several and recreate 3 FS manually to ensure correct tags have been retained

#Validation #1: CIK = 2969, Period=2024_02, Filing=10-Q
validation1 <- fs |> filter(cik ==2969)

openxlsx::write.xlsx(validation1,'C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Code/Code Validation Files/002 Validation - FirmxTag Matches FS/validation.xlsx')


#Validation #2: CIK = 1289490, Period=2024_02, Filing = 10-K 
validation2 <- data |> filter(cik ==1289490)

openxlsx::write.xlsx(validation2,'C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Code/Code Validation Files/002 Validation - FirmxTag Matches FS/002-script-validation2.xlsx')



#Validation #2: CIK = 929008, Period=2024_02, Filing = 10-K
validation3 <- data |> filter(cik ==1323468) 

openxlsx::write.xlsx(validation3,'C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Code/Code Validation Files/002 Validation - FirmxTag Matches FS/002-script-validation3.xlsx')



sample(data$cik,1)


#Step 1: DONE - Manually download all year-month XBRL files and save in folder in Data folder on Dropbox
#Step 2: DONE - Create loop that reads/unzips the necessary files (see example above) and saves them in a separate folder on Dropbox with month-year naming convention added on.
#Step 3: Iterature through year-month XBRL files and save down data in parquet file of each FS tag with classifications
#Step 4: Iterate through the parquet files and apply XBRL SCF processing. 
        #Output will be tidy data frame like => cik - date - filing info - CF measures. Will save each year-month data frame in separate folder on Dropbox.
        #Some do not tag zeros. So output is saved for each column (period reported). Take the unique number of line items for each stmt to not miss zero amounts that are un-tagged.
        #SEC regulation is saved out in Dropbox. Requirement for disaggregation of line item is if > 10% of avg OCF from prior three years. Other could be sum of many immaterial items.
#Step 5: Combine all year-month tidy data frames.
#Step 6: Analyze data. Descriptives.







#Scratch --------------------------------------------------------

#Read in raw XBRL files
num <- read_delim('C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Data/XBRL/XBRL Raw Data/num_2024_02.tsv')
tag <- read_delim('C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Data/XBRL/XBRL Raw Data/tag_2024_02.tsv')
pre <- read_delim('C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Data/XBRL/XBRL Raw Data/pre_2024_02.tsv')
sub <- read_delim('C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Data/XBRL/XBRL Raw Data/sub_2024_02.tsv')
dim <- read_delim('C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Data/XBRL/XBRL Raw Data/dim_2024_02.tsv')

#Merge number file with tag file 
#Number file is unique by columns but the tag can be used multiple times in one filing (checked Farmer Bros (cik = 34563) for 1Q 2024)
#Tag file has tag description information and which yearly-taxonomy it comes from
num <- num %>%
  #Restricting to just tag information I want now:
  #FASB taxonomy (version), type of data (i.e. dollar or shares) (datatype), tlabel (taxonomy description or firm provided), documentation (standard or firm provided)
  left_join(tag %>% select(tag,version,datatype,crdr,tlabel,doc),by=c("tag","version"))%>%
  #Occasional there are blank tags so dropping those
  filter(value!="")%>% 
  #Signals different facts for same primary key - filter out values greater than or equal to one (see manual)
  filter(iprx == 0) %>%
  #Filter to only USD or amounts in shares
  filter(uom == "USD" | uom == "share")%>%
  #Merge in dimension description to exclude those referring to segments
  #left_join(dim,by=c("dimh"="dimhash"))%>%
  #Restrict to XBRL tag of the parent level company.
  #mutate(dim_desc_head = str_to_lower(sub("=.*","",segments)),
  #  seg = if_else(grepl("segment",dim_desc_head)|grepl("consolidationitems",dim_desc_head)|grepl("equitycomponents",dim_desc_head),1,0))%>%
  #filter(seg == 0)
  filter(dimh=="0x00000000")



#Check dups (unique observation should be filing number, xbrl tag, period end date, number of quarters associated with tag, and numeric value)
check_dups <- num |> 
  group_by(adsh,tag,qtrs,ddate,value) |> 
  summarise(obs = n()) |> 
  filter(obs > 1)
#Good


#Sometimes duplicates introduced in the next step because the pre dataset has the same tag twice in same financial statement (EX: BOP and EOP cash balance)
#However, I only 
pre <- pre %>%
  #Duplicates introduced for in-line parenthetical tags dropping because only want individual line items for this project
  filter(inpth == 0)%>%
  #Keeping Filing Accension Number (adsh), XBRL tag (tag), report number in filing (report), line number (line) and name of financial statement it appears in (stmt)
  select(adsh,tag,report,line,negating,stmt)%>%
  distinct()

gc()


#Merge number/tag dataset with presentation dataset (gets FS location) and company information (sub dataset)
data <- num %>% 
  #Add financial statemetn location from the PRE file (will result in many-to-many matches for tags that appear in multiple locations)
  left_join(pre,by=c("adsh","tag"))%>%
  #Merge in company information
  left_join(sub %>% select(adsh,cik,sic,afs,form,period,filed,accepted,nciks), by=c("adsh"))%>%
  #Next use data in PRE file to classify tags based on financial statements
  mutate(face = if_else(stmt %in% c("BS","IS","CF","EQ","CI"),1,0),
         notes = if_else(face == 0,1,0),
         BS = coalesce(if_else(stmt == "BS",1,0),0L),
         IS = coalesce(if_else(stmt == "IS" | stmt == "CI",1,0),0L),
         CF = coalesce(if_else(stmt == "CF",1,0),0L),
         EQ = coalesce(if_else(stmt == "EQ",1,0),0L),
         UN = coalesce(if_else(stmt == "UN",1,0),0L))%>%
  filter(form %in% c("10-K","10-K/A","10-Q","10-Q/A"))#restrict to Ks and Qs

gc()


#Check dups - should be unique at firm (cik) - XBRL Tag (tag) - reporting period (ddate) - financial statement (stmt) - value
#There are some duplicates by the above due to amended filings. Keeping those for now because amended filings could also occur in later months. Will address those dups in next script.
#For this project only considering duplicates in financial statements because number of unique line items will be variable of interest so filtering out tags in notes or other statements ("UN")
check_dups <- data %>%
  filter(face==1)%>%
  group_by(cik,tag,form,filed,qtrs,ddate,value,stmt)%>%
  summarise(obs = n())%>%
  filter(obs > 1)

num$
  #Save data as parquet files and end script here.. Then make new script to do CF statement specific
  gc()


