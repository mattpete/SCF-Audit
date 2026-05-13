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
library(broom)
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
                  user=rstudioapi::askForPassword("WRDS_user"),
                  password=rstudioapi::askForPassword("WRDS_pw"),
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
crsp.delist <- tbl(wrds,in_schema("crsp", "msedelist"))


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

delist <- crsp.delist |> collect()

#Save as Parquet Files
write_parquet(funda,glue("{data_path}/comp_funda.parquet"))
write_parquet(fundq,glue("{data_path}/comp_fundq_4Qrdq.parquet"))
write_parquet(company,glue("{data_path}/comp_company.parquet"))
write_parquet(ccmlink,glue("{data_path}/crsp_ccmlink.parquet"))
write_parquet(msi,glue("{data_path}/crsp_msi.parquet"))
write_parquet(msf,glue("{data_path}/crsp_msf.parquet"))
write_parquet(stocknames,glue("{data_path}/crsp_stocknames.parquet"))
write_parquet(delist,glue("{data_path}/crsp_delist.parquet"))

# Create Financial Statement Variables --------------------------------------------
#Load Compustat Annual File
funda_raw <- read_parquet(glue("{data_path}/comp_funda.parquet"))|>
  select(gvkey,datadate,cusip,cik,tic,sich,exchg,fyear,ceq,ibc,at,sale,xrd,re,xidoc,txt,txdi,tlcf,
         txfed,txfo,txdfed,txdfo,pi,pidom,dvc,dv,spi,ni,act,che,ivao,lct,dlc,dp,sstk,prstkc,
         dltis,dltt,cogs,invt,rect,ap,capx,ivch,aqc,fuseo,sppe,siv,ivstch,ivaco,wcapc,chech,
         dlcch,recch,invch,apalch,txach,aoloch,fiao,ibc,dpc,txdc,esubc,sppiv,fopo,fsrco,exre,
         nopi,glp,gla,scf,csho,prcc_f,seq,oibdp,lt,xint,pstk,tstkp,dvp,dvpa,mib,idit,msa,recta,
         mii,esub,nopio,xido,txditc,lo,intpn,intc,wdp,wda,rcp,rca,capxv,ppent,ppegt,xad,drc,
         drlt,oancf,xidoc,ivncf,fincf,dvintf,txpd,dltr,prstkpc,pdvc,cdvc,dtep,dtea,gp)

#Load in Compustat Company names
compname <- read_parquet(glue("{data_path}/comp_company.parquet"))|>
  select(gvkey,conm,sic)

#Load Compustat Quarterly for EA date
fundq_raw <- read_parquet(glue("{data_path}/comp_fundq_4Qrdq.parquet"))

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
         datadate_p6 = datadate %m+% months(6),
         #Creating lag (l) and future (f) fyear for merging
         l_fyear = fyear-1,
         f_fyear = fyear+1)

#Merge in lagged data and future data
data4 <- data3 |> 
  #Lagged data
  left_join(data3 |> 
              select(gvkey,fyear,datadate,datadate_p6,prcc_f,csho,mve,che,ivao,at,lt,dlc,dltt,pstk,
                     tstkp,dvpa,ceq, mib,xint,dvp,idit,msa,recta,ni,mii,oancf,intpn,ivncf,
                     intc,re,ppent,drc,drlt) |> 
              rename(l_datadate=datadate,l_datadate_p6=datadate_p6,l_prcc_f=prcc_f,l_csho=csho,l_che=che,
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
  #filter(l_prcc_f>=1,prcc_f>1,at>=10,sale>10) |> #133,019 obs
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
    CFF_FCFEQ = dlcch + dltis - dltr)


# Monthly Returns -------------------------------------

#Bring in linking file
ccm_link <- read_parquet(glue("{data_path}/crsp_ccmlink.parquet")) |> 
  filter(linkprim %in% c('P','C'),linktype %in% c('LU','LC')) |> 
  rename(permno = lpermno) |> 
  select(gvkey,permno,linkdt,linkenddt) |> 
  mutate(linkenddt = coalesce(linkenddt,max(linkenddt, na.rm = TRUE)))

#Merge in PERMNO
data6 <- data5 |> 
  inner_join(ccm_link,by=join_by(gvkey,between(datadate,linkdt,linkenddt)))

#Load CRSP data
msf <- read_parquet(glue("{data_path}/crsp_msf.parquet"))
msi <- read_parquet(glue("{data_path}/crsp_msi.parquet"))
stocknames <- read_parquet(glue("{data_path}/crsp_stocknames.parquet"))
delist <- read_parquet(glue("{data_path}/crsp_delist.parquet"))

#Delisting returns
delist2 <- delist |> 
  #Performance related make -30%
  mutate(retd = if_else(dlstcd == 500 | (dlstcd >= 520 & dlstcd <= 584), -0.30, dlret),
         date = ceiling_date(dlstdt, "month")-1)

#Merge in financial statement data (lag by six months)
data7 <- msf |>
  #Join in stocknames file to restrict to common shares on NYSE, AMEX, or NASDAQ
  left_join(stocknames |> select(permno,namedt,nameenddt,shrcd,exchcd),by=join_by(permno,between(date,namedt,nameenddt))) |> 
  filter(shrcd %in% c(10,11),exchcd %in% c(1,2,3,4)) |> 
  #Merge in delisting returns
  left_join(delist2 |> select(permno,date,retd),by=c("permno","date")) |> 
  #Merge in lagged financial statement data
  left_join(data6, by = join_by("permno",closest(date >= datadate_p6))) |> 
  #Drop returns with no prior financial statement data
  filter(!is.na(datadate)) |> 
  #Merge in index return
  left_join(msi |> select(date,vwretd),by=c("date")) |> 
  #Calculate returns (adjust for delisting return if there)
  mutate(ret_adj = if_else(is.na(retd),ret,(1+ret)*(1+retd)-1),
         mve = shrout*abs(prc)*1000,
         excess_ret = (ret_adj - vwretd)*100 )|> 
  #Arrange by firm and get prior MVE (for scaling) and next month return
  arrange(permno,gvkey,date) |> 
  mutate(lag_mve = lag(mve,1)) |> 
  ungroup() |> 
  filter(!is.na(lag_mve),!is.na(seq),!is.na(gp)) |> 
  mutate(CFO_GAAP_P = CFO_GAAP*1000000/lag_mve,
         GP = gp*1000000/lag_mve,
         CFO_EBITDA_P = CFO_EBITDA/lag_mve,
         BTM = seq*1000000/lag_mve,
         year = year(date),
         month = month(date)) |> 
  mutate_at(vars(lag_mve,CFO_GAAP_P:BTM),winsorize_x2)

summary(data7$GP)




#Bring in Fama-French 5 Factors
ff5 <- read_csv(glue("{data_path}/ff_5factor.csv")) |> 
  slice(-(1:2)) |> 
  janitor::row_to_names(1)
  

gc()

model<-summary(lm(excess_ret ~ CFO_GAAP_P + log(BTM) + l_mve,data7 |> filter(year(date)==2002)))
summary(lm(excess_ret ~ CFO_EBITDA_P + log(BTM) + l_mve,data7 |> filter(year(date)==2000)))
model

ocf <- 36074*1000000
shrout <- 2.8*1000000000 
mve <- shrout*48.23


ocf/mve


#Cross-sectional regressions----------------------------------

results<-data.frame()
years<-1989:2023
months<-1:12 
for (i in years){
  for (j in months){
    model <- summary(lm(excess_ret ~ GP + log(1+BTM) + l_mve, data7 |> filter(year==i,month==j)))
    
    coef1 <- model$coefficients[1]
    coef2 <- model$coefficients[2]
    coef3 <- model$coefficients[3]
    coef4 <- model$coefficients[4]
    
    ind_result <- data.frame(year=i,month=j,int=coef1,cf=coef2,btm=coef3,size=coef4)
    results <- rbind(results,ind_result)
  }
}

mean(results$cf)
mean(results$cf)/(sd(results$cf)/sqrt(length(years)*length(months)))
mean(results$btm)
mean(results$btm)/(sd(results$btm)/sqrt(length(years)*length(months)))
mean(results$size)
mean(results$size)/(sd(results$size)/sqrt(length(years)*length(months)))


amazon <- data7 |> filter(permno == 84788) |> select(date,gp, GP, shrout, mve)


