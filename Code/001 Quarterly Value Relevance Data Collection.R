# Setup - load packages and functions -----------------------------------------------------------------

library(dplyr)
library(RPostgres)
library(DBI)
library(dbplyr)
library(glue)
library(arrow)
library(haven)
library(stringr)
library(httr)
library(tidyverse)

#Home
data_path <- "C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Data/"
source("C:/Users/mnp4/Dropbox/Dissertation/Statement of Cash Flows/Code/Functions.R")

#Work
data_path <- "C:/Users/m221p165/Dropbox/Dissertation/Statement of Cash Flows/Data/"
source("C:/Users/m221p165/Dropbox/Dissertation/Statement of Cash Flows/Code/Functions.R")


# Download Data from WRDS -------------------------------------------------------------------------------------

#First log on to WRDS postgres
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user="mattpeterson",
                  password="TSU&DUBulldog10s!!",
                  sslmode='require',
                  dbname='wrds')
wrds 


#Set tables
comp.funda <- tbl(wrds,in_schema("comp", "funda"))
comp.fundq <- tbl(wrds,in_schema("comp", "fundq"))
comp.company <- tbl(wrds,in_schema("comp","company"))
ccmxpf.link <- tbl(wrds,in_schema("crsp", "ccmxpf_lnkhist"))
crsp.msi <- tbl(wrds,in_schema("crsp", "msi"))
crsp.msf <- tbl(wrds,in_schema("crsp", "msf"))
crsp.stocknames <- tbl(wrds,in_schema("crsp", "stocknames"))
trace.bondret <- tbl(wrds,in_schema("wrdsapps", "bondret"))
crsp.cti <- tbl(wrds,in_schema("crsp", "mcti"))
crsp.bondlink <- tbl(wrds,in_schema("wrdsapps","bondcrsp_link"))

#Collect
funda <- comp.funda|> 
  filter(indfmt == "INDL", datafmt == "STD",
         consol == "C", popsrc == "D", year(datadate)>1985) |> 
  collect()

fundq <- comp.fundq|> 
  filter(indfmt == "INDL", datafmt == "STD", consol == "C", popsrc == "D", 
         year(datadate)>1985,!is.na(rdq), fqtr == 4) |> 
  select(gvkey,datadate,rdq) |> 
  collect()

company <- comp.company |> collect()

ccmlink <- ccmxpf.link |> collect()

msi <- crsp.msi |> collect()

msf <- crsp.msf |> filter(year(date)>1985) |> collect()

stocknames <- crsp.stocknames |> collect()

bondrets <- trace.bondret |> collect()

cti <- crsp.cti |> collect()

bondlink <- crsp.bondlink |> collect()

#Save as Parquet Files
write_parquet(funda,glue("{data_path}/comp_funda.parquet"))
write_parquet(fundq,glue("{data_path}/comp_fundq_4Qrdq.parquet"))
write_parquet(company,glue("{data_path}/comp_company.parquet"))
write_parquet(ccmlink,glue("{data_path}/crsp_ccmlink.parquet"))
write_parquet(msi,glue("{data_path}/crsp_msi.parquet"))
write_parquet(msf,glue("{data_path}/crsp_msf.parquet"))
write_parquet(stocknames,glue("{data_path}/crsp_stocknames.parquet"))
write_parquet(bondrets,glue("{data_path}/trace_bondrets.parquet"))
write_parquet(cti,glue("{data_path}/crsp_cti.parquet"))
write_parquet(bondlink,glue("{data_path}/crsp_bondlink.parquet"))

# Create Financial Statement Variables --------------------------------------------
#Load in Compustat Annual file
funda_raw <- read_parquet(glue("{data_path}/comp_funda.parquet"))|>
  select(gvkey,datadate,cusip,cik,tic,sich,exchg,fyear,ceq,ibc,at,sale,xrd,re,xidoc,txt,txdi,tlcf,
         txfed,txfo,txdfed,txdfo,pi,pidom,dvc,dv,spi,ni,act,che,ivao,lct,dlc,dp,sstk,prstkc,
         dltis,dltt,cogs,invt,rect,ap,capx,ivch,aqc,fuseo,sppe,siv,ivstch,ivaco,wcapc,chech,
         dlcch,recch,invch,apalch,txach,aoloch,fiao,ibc,dpc,txdc,esubc,sppiv,fopo,fsrco,exre,
         nopi,glp,gla,scf,csho,prcc_f,seq,oibdp,lt,xint,pstk,tstkp,dvp,dvpa,mib,idit,msa,recta,
         mii,esub,nopio,xido,txditc,lo,intpn,intc,wdp,wda,rcp,rca,capxv,ppent,ppegt,xad,drc,
         drlt,oancf,xidoc,ivncf,fincf,dvintf,txpd,dltr,prstkpc,pdvc,cdvc,dtep,dtea)

#Load in Compustat Company names
compname <- read_parquet(glue("{data_path}/comp_company.parquet"))|>
  select(gvkey,conm,sic)

#Load Compustat Quarterly for EA date
fundq_raw <- read_parquet(glue("{data_path}/comp_fundq_4Qrdqparquet"))

#Merge in company name, industry, and ea date (295,711 obs)
data1 <- funda_raw |> 
  inner_join(compname,by=c("gvkey")) |> 
  inner_join(fundq_raw,by=c("gvkey","datadate"))
  

#Check duplicates
data1 |> group_by(gvkey,datadate) |> summarise(obs=n()) |> filter(obs >1) 

#Set some variables with missing values equal to zero
data2 <- data1 |> 
  mutate(across(c(xrd,spi,pstk,dvpa,tstkp,dlc,dltt,ivao,sstk,dltis,nopi,dp,fuseo,sppe,siv,
                  wcapc,dlcch,apalch,aoloch,recch,txach,invch,aqc,dtep,dtea,
                  esubc,sppiv,fopo,fsrco,exre,glp,gla,mib,idit,msa,recta,mii,esub,intc,
                  xint,wdp,wda,capx,rcp,rca,xad,ppent,dvintf,drc,dltis,dltr,dlcch,dvc,
                  prstkc,xidoc,intpn,txpd), ~ replace_na(.x,0)))

#Create variables for merging and filtering
data3 <- data2 |> 
  filter(!is.na(fyear)) |> #helps with dups on subsequent merge
  mutate(mve = csho*prcc_f,
         btm = seq/mve,
         SIC = coalesce(as.numeric(sic),sich),
         SIC2 = floor(SIC/100),
         datadate_p3 = datadate %m+% months(3),
         #Creating lag (l) and future (f) fyear for merging
         l_fyear = fyear-1,
         f_fyear = fyear+1)

#Merge in lagged data and future data
data4 <- data3 |> 
  #Lagged data
  left_join(data3 |> 
              select(gvkey,fyear,datadate,datadate_p3,prcc_f,csho,mve,che,ivao,at,lt,dlc,dltt,pstk,
                     tstkp,dvpa,ceq, mib,xint,dvp,idit,msa,recta,ni,mii,oancf,intpn,ivncf,
                     intc,re,ppent,drc,drlt) |> 
              rename(l_datadate=datadate,l_datadate_p3=datadate_p3,l_prcc_f=prcc_f,l_csho=csho,l_che=che,
                     l_ivao=ivao,l_at=at,l_lt=lt,l_dlc=dlc,l_dltt=dltt,l_pstk=pstk,l_tstkp=tstkp,
                     l_mve = mve,l_dvpa=dvpa,l_ceq=ceq,l_mib=mib,l_xint=xint,l_dvp=dvp,l_idit=idit,
                     l_msa=msa,l_recta=recta,l_ni=ni,l_mii=mii,l_oancf=oancf,l_intpn=intpn,
                     l_ivncf=ivncf,l_intc=intc,l_re=re,l_ppent=ppent,l_drc=drc,l_drlt=drlt)
                      ,by=c("gvkey","l_fyear" = "fyear")) |> 
  #Future data
  left_join(data3 |> 
              select(gvkey,fyear,oancf) |> rename(f_oancf=oancf),by=c("gvkey","f_fyear" = "fyear")) |> 
  #Drop the duplicates (primarily gvkey == '066552')
  group_by(gvkey,fyear) |> 
  mutate(obs = n()) |> 
  filter(obs ==1) |> 
  ungroup() 


#Check duplicates
data4 |> group_by(gvkey,fyear) |> summarise(obs=n()) |> filter(obs >1) 

#Financial Variables
data5 <- data4 |> 
  #Sample Selection Steps
  filter(fyear >=1987,fyear<2024) |> 
  filter(!is.na(oancf),l_mve >= 0) |> #229,977 obs
  filter(floor(SIC/1000) != 6) |> #190,862 obs
  filter(l_prcc_f>=1,prcc_f>1,at>=10,sale>10) |> #133,019 obs
  mutate(#Create Nissim and Penman (2001) variables
         CALYEAR = year(datadate),
         FA = che + ivao,
         L_FA = l_che + l_ivao,
         OA = at - FA,
         L_OA = l_at - L_FA,
         FO = dlc + dltt + pstk + tstkp + dvpa,
         L_FO = l_dlc + l_dltt + l_pstk + l_tstkp + l_dvpa,
         NFO = FO - FA,
         L_NFO = L_FO - L_FA,
         CSE = ceq + tstkp - dvpa,
         L_CSE = l_ceq + l_tstkp - l_dvpa,
         D_CSE = CSE - L_CSE,
         D_RE = re - l_re,
         NOA = NFO + CSE + mib,
         L_NOA = L_NFO + L_CSE + l_mib,
         OL = OA - NOA,
         L_OL = L_OA - L_NOA,
         MTR = if_else(CALYEAR>=2018,0.23,if_else(CALYEAR<2018 & CALYEAR >=1993,0.37,0.36)),
         NFE = xint*(1-MTR) + dvp - idit*(1-MTR),
         UFE = l_msa - msa,
         CNFE = NFE + UFE,
         CSA = msa - l_msa + recta - l_recta,
         CNI = ni - dvp - CSA,
         OI = CNFE + CNI + mii,
         C = oancf + intpn,
         I = ivncf - intc,
         MVF = mve + NFO,
         L_MVF = l_mve + L_NFO,
         D_NOA = NOA - L_NOA,
         
         #Make Cash Flow Variables
         #GAAP
         CFO_GAAP = oancf,
         CFI_GAAP = ivncf,
         CFF_GAAP = fincf,
         FCF_GAAP = oancf + ivncf,
         #CORRECTING R&D
         CFO_RD = oancf + xrd,
         CFI_RD = ivncf - xrd,
         CFF_RD = fincf,
         #EBITDA
         CFO_EBITDA = oancf + intpn + txpd,
         CFI_EBITDA = ivncf,
         CFF_EBITDA = fincf,
         #FINANCING ACTIVITIES
         CFO_FIN = oancf + intpn,
         CFI_FIN = ivncf,
         CFF_FIN = fincf - intpn,
         #STRANDED TAX
         CFO_STAX = oancf + (dtep - dtea) - sppiv*MTR,
         CFI_STAX = ivncf + sppiv*MTR,
         CFF_STAX = fincf - (dtep - dtea),
         #SIMPLE FREE CASH FLOW
         FCF_SIM = oancf - capx,
         CFO_FCFSIM = oancf,
         CFI_FCFSIM = -capx,
         CFF_FCFSIM = fincf,
         #FREE CASH FLOW BALANCE SHEET
         FCF_BS = OI - D_NOA,
         CFO_FCFBS = OI - D_NOA + ivncf,
         CFI_FCFBS = ivncf,
         CFF_FCFBS = fincf,
         #FREE CASH FLOW TO EQUITY
         FCF_EQ = ni + dp - (recch + invch + apalch + txach + aoloch) - capx + dlcch + dltis - dltr,
         NWC = (recch + invch + apalch + txach + aoloch),
         NET_DEBT = dlcch + dltis - dltr,
         CFO_FCFEQ =  ni + dp - (recch + invch + apalch + txach + aoloch),
         CFI_FCFEQ = - capx,
         CFF_FCFEQ = dlcch + dltis - dltr,
         
         #Scale variables by prior MVE for stock return regressions
         EARN_P = (OI + NFE)/l_mve,  #Penman and Yehuda 2009
         OE_P = (oancf-(recch+invch+apalch+txach+aoloch))/l_mve, #Dechow and Dichev 2002
         IBC_P = ibc/l_mve,  #Dechow 1994/Ball and Nikolaeve 2022
         NI_P = ni/l_mve,
         CHG_PRC = (prcc_f - l_prcc_f)/l_prcc_f,
         L_BP = L_CSE/l_mve,
         D_P = (dvc+prstkc)/l_mve,
         DIV_P = dvc/l_mve,
         CFO_GAAP_P = CFO_GAAP/l_mve,
         CFI_GAAP_P = CFI_GAAP/l_mve,
         CFF_GAAP_P = CFF_GAAP/l_mve,
         FCF_GAAP_P = FCF_GAAP/l_mve,
         CFO_RD_P = CFO_RD/l_mve,
         CFI_RD_P = CFI_RD/l_mve,
         CFF_RD_P = CFF_RD/l_mve,
         CFO_EBITDA_P = CFO_EBITDA/l_mve,
         CFI_EBITDA_P = CFI_EBITDA/l_mve,
         CFF_EBITDA_P = CFF_EBITDA/l_mve,
         CFO_FIN_P = CFO_FIN/l_mve,
         CFI_FIN_P = CFI_FIN/l_mve,
         CFF_FIN_P = CFF_FIN/l_mve,
         CFO_STAX_P = CFO_STAX/l_mve,
         CFI_STAX_P = CFI_STAX/l_mve,
         CFF_STAX_P = CFF_STAX/l_mve,
         FCF_SIM_P = FCF_SIM/l_mve,
         CFO_FCFSIM_P = CFO_FCFSIM/l_mve,
         CFI_FCFSIM_P = CFI_FCFSIM/l_mve,
         CFF_FCFSIM_P = CFF_FCFSIM/l_mve,
         FCF_BS_P = FCF_BS/l_mve,
         CFO_FCFBS_P = CFO_FCFBS/l_mve,
         CFI_FCFBS_P = CFI_FCFBS/l_mve,
         CFF_FCFBS_P = CFF_FCFBS/l_mve,
         FCF_EQ_P =  FCF_EQ/l_mve,
         CFO_FCFEQ_P =  CFO_FCFEQ/l_mve,
         CFI_FCFEQ_P = CFI_FCFEQ/l_mve,
         CFF_FCFEQ_P = CFF_FCFEQ/l_mve,   
         
         #Scale by prior MVF for bond return regressions
         EARN_F = (OI + NFE)/L_MVF,  #Penman and Yehuda 2009
         OE_F = (oancf-(recch+invch+apalch+txach+aoloch))/L_MVF, #Dechow and Dichev 2002
         IBC_F = ibc/L_MVF,  #Dechow 1994/Ball and Nikolaeve 2022
         NI_F = ni/L_MVF,
         D_F = (dvc+prstkc)/L_MVF,
         DIV_F = dvc/L_MVF,
         CFO_GAAP_F = CFO_GAAP/L_MVF,
         CFI_GAAP_F = CFI_GAAP/L_MVF,
         CFF_GAAP_F = CFF_GAAP/L_MVF,
         FCF_GAAP_F = FCF_GAAP/L_MVF,
         CFO_RD_F = CFO_RD/L_MVF,
         CFI_RD_F = CFI_RD/L_MVF,
         CFF_RD_F = CFF_RD/L_MVF,
         CFO_EBITDA_F = CFO_EBITDA/L_MVF,
         CFI_EBITDA_F = CFI_EBITDA/L_MVF,
         CFF_EBITDA_F = CFF_EBITDA/L_MVF,
         CFO_FIN_F = CFO_FIN/L_MVF,
         CFI_FIN_F = CFI_FIN/L_MVF,
         CFF_FIN_F = CFF_FIN/L_MVF,
         CFO_STAX_F = CFO_STAX/L_MVF,
         CFI_STAX_F = CFI_STAX/L_MVF,
         CFF_STAX_F = CFF_STAX/L_MVF,
         FCF_SIM_F = FCF_SIM/L_MVF,
         CFO_FCFSIM_F = CFO_FCFSIM/L_MVF,
         CFI_FCFSIM_F = CFI_FCFSIM/L_MVF,
         CFF_FCFSIM_F = CFF_FCFSIM/L_MVF,
         FCF_BS_F = FCF_BS/L_MVF,
         CFO_FCFBS_F = CFO_FCFBS/L_MVF,
         CFI_FCFBS_F = CFI_FCFBS/L_MVF,
         CFF_FCFBS_F = CFF_FCFBS/L_MVF,
         FCF_EQ_F =  FCF_EQ/L_MVF,
         CFO_FCFEQ_F =  CFO_FCFEQ/L_MVF,
         CFI_FCFEQ_F = CFI_FCFEQ/L_MVF,
         CFF_FCFEQ_F = CFF_FCFEQ/L_MVF,
         
         #Scale by lagged total assets for predicting future OCF
         P1_CFO_GAAP_A = f_oancf/at,
         OE_A = (oancf-(recch+invch+apalch+txach+aoloch))/l_at, #Dechow and Dichev 2002
         IBC_A = ibc/l_at,  #Dechow 1994/Ball and Nikolaeve 2022
         NI_A = ni/l_at,
         CFO_GAAP_A = CFO_GAAP/l_at,
         CFI_GAAP_A = CFI_GAAP/l_at,
         CFF_GAAP_A = CFF_GAAP/l_at,
         FCF_GAAP_A = FCF_GAAP/l_at,
         CFO_RD_A = CFO_RD/l_at,
         CFI_RD_A = CFI_RD/l_at,
         CFF_RD_A = CFF_RD/l_at,
         CFO_EBITDA_A = CFO_EBITDA/l_at,
         CFI_EBITDA_A = CFI_EBITDA/l_at,
         CFF_EBITDA_A = CFF_EBITDA/l_at,
         CFO_FIN_A = CFO_FIN/l_at,
         CFI_FIN_A = CFI_FIN/l_at,
         CFF_FIN_A = CFF_FIN/l_at,
         CFO_STAX_A = CFO_STAX/l_at,
         CFI_STAX_A = CFI_STAX/l_at,
         CFF_STAX_A = CFF_STAX/l_at,
         FCF_SIM_A = FCF_SIM/l_at,
         CFO_FCFSIM_A = CFO_FCFSIM/l_at,
         CFI_FCFSIM_A = CFI_FCFSIM/l_at,
         CFF_FCFSIM_A = CFF_FCFSIM/l_at,
         FCF_BS_A = FCF_BS/l_at,
         CFO_FCFBS_A = CFO_FCFBS/l_at,
         CFI_FCFBS_A = CFI_FCFBS/l_at,
         CFF_FCFBS_A = CFF_FCFBS/l_at,
         FCF_EQ_A =  FCF_EQ/l_at,
         CFO_FCFEQ_A =  CFO_FCFEQ/l_at,
         CFI_FCFEQ_A = CFI_FCFEQ/l_at,
         CFF_FCFEQ_A = CFF_FCFEQ/l_at)


#Save down the dataset
write_parquet(data5,glue("{data_path}/financial_data_12272024.parquet"))



# Validation ----------------------------------------

test <- data5 |> filter(cik == "0000104169",CALYEAR%in%c(2020,2021,2022)) |> 
  select(cik,conm,datadate,CFO_GAAP,CFI_GAAP,CFF_GAAP,CFO_FIN,recch, invch , apalch , txach , aoloch,CFO_STAX,CFO_EBITDA,CFO_RD,FCF_GAAP,FCF_SIM,FCF_BS,FCF_EQ)


test <- data5 |> filter(cik == "0000104169",CALYEAR%in%c(2020,2021,2022)) |> 
  select(cik,conm,datadate,CFO_GAAP,FCF_EQ,ni,dp,capx,recch,invch,apalch,txach,aoloch,dlcch,dltis,dltr)



# Calculate Concurrent Equity Returns -------------------------------------

#Bring in linking file
ccm_link <- read_parquet(glue("{data_path}/crsp_ccmlink.parquet")) |> 
  filter(linkprim %in% c('P','C'),linktype %in% c('LU','LC')) |> 
  rename(permno = lpermno) |> 
  select(gvkey,permno,linkdt,linkenddt) |> 
  mutate(linkenddt = coalesce(linkenddt,max(linkenddt, na.rm = TRUE)))

#Merge in PERMNO
data6 <- data5 |> 
  inner_join(ccm_link,by=join_by(gvkey,between(datadate,linkdt,linkenddt)))

#Load monthly returns
msf <- read_parquet(glue("{data_path}/crsp_msf.parquet"))
msi <- read_parquet(glue("{data_path}/crsp_msi.parquet"))

#Merge in monthly returns between datadates
rets <- msf |> 
  left_join(data6 |> select(gvkey,permno,datadate,l_datadate),
            by=join_by(permno,between(date,l_datadate,datadate))) |> 
  filter(!is.na(datadate)) |> #drop unmatched returns
  left_join(msi |> select(date,vwretd),by=c("date")) #merge in index returns
 
#Calculate BHAR
bhar <- rets |> 
  group_by(gvkey,permno,datadate) |> 
  summarise(obs = n(),
            buyhold_ret_dd = exp(sum(log(1 + ret)))-1,
            buyhold_mkt_dd = exp(sum(log(1 + vwretd)))-1,
            mktadj_ret_dd = buyhold_ret_dd - buyhold_mkt_dd)

#Check return distributions for reasonableness
summary(bhar |> pull(buyhold_ret_dd))
summary(bhar |> pull(buyhold_mkt_dd))

#Merge in BHAR
data7 <- data6 |> 
  left_join(bhar,by=c("gvkey","permno","datadate"))


#Merge in monthly returns between datadates_p3
rets <- msf |> 
  left_join(data6 |> select(gvkey,permno,datadate_p3,l_datadate_p3),
            by=join_by(permno,between(date,l_datadate_p3,datadate_p3))) |> 
  filter(!is.na(datadate_p3)) |> #drop unmatched returns
  left_join(msi |> select(date,vwretd),by=c("date")) #merge in index returns

#Calculate BHAR
bhar <- rets |> 
  group_by(gvkey,permno,datadate_p3) |> 
  summarise(obs = n(),
            buyhold_ret_ddp3 = exp(sum(log(1 + ret)))-1,
            buyhold_mkt_ddp3 = exp(sum(log(1 + vwretd)))-1,
            mktadj_ret_ddp3 = buyhold_ret_ddp3 - buyhold_mkt_ddp3)

#Check return distributions for reasonableness
summary(bhar |> pull(buyhold_ret_ddp3))
summary(bhar |> pull(buyhold_mkt_ddp3))

#Merge in BHAR
data8 <- data7 |> 
  left_join(bhar,by=c("gvkey","permno","datadate_p3"))


#Save down the dataset
write_parquet(data8,glue("{data_path}/stockret_data_12272024.parquet"))


# Calculate Concurrent Bond Returns -------------------------------------

#Load monthly bond returns and clean data
bondrets <- read_parquet(glue("{data_path}/trace_bondrets.parquet")) |> 
  #Cleaning procedures from Adreani et al. (2023) RAST
  filter(!(rating_class == "0.IG" & date <= as.Date("2004-11-30") & amount_outstanding < 150000)) |> 
  filter(!(rating_class == "0.IG" & date > as.Date("2004-11-30") & amount_outstanding < 250000)) |> 
  filter(!(rating_class == "1.HY" & date <= as.Date("2016-09-30") & amount_outstanding < 100000)) |> 
  filter(!(rating_class == "1.HY" & date > as.Date("2016-09-30") & amount_outstanding < 250000)) |> 
  filter(!(bond_type %in% c("CMTZ", "CONV"))) |> 
  filter(defaulted != "Y", tmt >= 1) |> 
  #Create variables
  mutate(DATE = as.Date(date, format = "%Y%m%d"),
         month = month(DATE),
         year = year(DATE),
         L_DATE = ceiling_date(DATE - months(1), "month") - days(1),
         MOD_DUR = duration / (1 + (yield / ncoups)))#Modified duration is Macaulay Duration

#Load return to treasuries
mcti <- read_parquet(glue("{data_path}/crsp_cti.parquet")) |> 
  mutate(month = month(caldt),
         year = year(caldt))

#Merge
bondrets2 <- bondrets |> left_join(mcti,by=c("month","year"))

#Calculate excess credit returns by backing out return on duration matched treasury

#Define durations and corresponding returns as a named vector for lookup
durations <- c(1, 2, 5, 7, 10, 20, 30)
return_vars <- c("b1ret", "b2ret", "b5ret", "b7ret", "b10ret", "b20ret", "b30ret")
returns_map <- setNames(return_vars, durations)

#Vectorized approach for bond return calculation
bondrets3 <- bondrets2 %>%
  mutate(
    # Ensure MOD_DUR is bounded within the range of durations
    MOD_DUR = pmin(pmax(MOD_DUR, min(durations)), max(durations)),
    
    # Find D1 and D2 based on MOD_DUR
    D1 = durations[findInterval(MOD_DUR, durations, all.inside = TRUE)],
    D2 = durations[pmin(findInterval(MOD_DUR, durations, all.inside = TRUE) + 1, length(durations))],
    
    # Assign D1_RET and D2_RET using the returns_map
    D1_RET = case_when(
      D1 == 1 ~ b1ret,
      D1 == 2 ~ b2ret,
      D1 == 5 ~ b5ret,
      D1 == 7 ~ b7ret,
      D1 == 10 ~ b10ret,
      D1 == 20 ~ b20ret,
      D1 == 30 ~ b30ret,
      TRUE ~ NA_real_
    ),
    D2_RET = case_when(
      D2 == 1 ~ b1ret,
      D2 == 2 ~ b2ret,
      D2 == 5 ~ b5ret,
      D2 == 7 ~ b7ret,
      D2 == 10 ~ b10ret,
      D2 == 20 ~ b20ret,
      D2 == 30 ~ b30ret,
      TRUE ~ NA_real_
    ),
    
    # Calculate return to interest (ret_int)
    ret_int = if_else(
      D1 == D2,
      D1_RET,
      D1_RET * ((D2 - MOD_DUR) / (D2 - D1)) + D2_RET * ((MOD_DUR - D1) / (D2 - D1))
    )
  )

#Load Bond/CRSP link
bondlink <- read_parquet(glue("{data_path}/crsp_bondlink.parquet"))

#Link in PERMNO
bondrets4 <- bondrets3 |> 
  left_join(bondlink |> select(cusip,permno,link_startdt,link_enddt),
          by=join_by(cusip,between(date,link_startdt,link_enddt))) |> 
  filter(!is.na(permno))

#Now do monthly bond returns between balance sheet dates
rets <- bondrets4 |> 
  inner_join(data6 |> select(gvkey,permno,datadate_p3,l_datadate_p3),
             by=join_by(permno,between(date,l_datadate_p3,datadate_p3))) |> 
  mutate(ret_total = coalesce(ret_eom,0L))
  
#Calculate Bond Buy-Hold Return
bondbhars <- rets |> 
  group_by(gvkey,cusip,permno,datadate_p3) |> 
  summarise(obs = n(),
            rating = case_when((sum(rating_class=="0.IG")-sum(rating_class=="1.HY"))>0 ~ "0.IG", TRUE ~ "1.HY"),
            cum_totbondret = exp(sum(log(1 + ret_eom)))-1,
            cum_intbondret = exp(sum(log(1 + ret_int)))-1,
            cum_credret = cum_totbondret - cum_intbondret)

#Check reasonableness of returns
summary(bondbhars$cum_totbondret)
summary(bondbhars$cum_credret)

#Merge in financial statement data
data9 <- bondbhars |> 
  left_join(data6 |> select(gvkey,permno,datadate_p3,CFO_GAAP_F,CFO_FIN_F,CFO_STAX_F,CFO_EBITDA_F,CFO_RD_F,
                            FCF_GAAP_F,FCF_SIM_F,FCF_BS_F,FCF_EQ_F,CFI_GAAP_F,CFI_FIN_F,CFI_STAX_F,CFI_EBITDA_F,
                            CFI_RD_F,CFI_FCFSIM_F,CFI_FCFBS_F,CFI_FCFEQ_F,OE_F,NI_F,IBC_F,sic,mve),
            by=c("gvkey","datadate_p3"))


#Save down the dataset
write_parquet(data9,glue("{data_path}/bondret_data_12272024.parquet"))

