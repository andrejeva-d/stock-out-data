
rm(list = setdiff(ls(), "x"))

library(stringr)
library(xml2)
library(rvest)
library(tidyverse)

######################################################################################
# Get and prepare XML file
######################################################################################

# get the xml file and convert to xml nodeset

xml_data<-read_xml("https://docetp.mpa.se/LMF/Reports/restnoteringar-2-0.xml")
xml_name(xml_data)
xml_nodeset <- xml_find_all(xml_data, "//d1:SupplyShortagePublic", xml_ns(xml_data))

# Extract data and convert into a data frames
# Fill in missing data where multiple packaging options are out of stock or descriptions or alternatives are provided or whatever

fun_extr<-function(x){
  fieldnames <- xml_name(xml_find_all(x, ".//*"))
  fields <- xml_text(xml_find_all(x, ".//*"))
  df <- tibble(fieldnames, fields) %>% 
    group_by(fieldnames) %>% 
    mutate(index = 1,
           index = cumsum(index)) %>% 
    spread(key = "fieldnames", value = "fields")
  df<-df %>% fill(FirstPublicationDate, LastPublicationDate, NplId, ReferenceNumber)
  return(df)
}

length(xml_nodeset)
df<-lapply(xml_nodeset[1:length(xml_nodeset)], fun_extr)
head(df[[4]])

# Combine into one data.frames
df_comb<-bind_rows(df, .id = "column_label")


######################################################################################
# Salect data and get nordick id 
######################################################################################

rm(list = setdiff(ls(), "df_comb"))

dat<-unique(df_comb[,c("FirstPublicationDate","ActualEndDate", "ForecastEndDate","NplId", "NplPackId", "AtcCode")])
head(dat)
packid <- read_delim("~/R_projects/Sandoz_DA/Data/Send to candidates/Sweden_PackId_to_Nordic_number.txt",
                     ";", escape_double = FALSE, col_types = cols(`NPL pack ID` = col_character()),  trim_ws = TRUE)

dat_merged<-merge(packid, dat, by.x="NPL pack ID", "NplPackId", all.y=TRUE)

# fill missing end date with forecasted end date

dat_merged$ActualEndDate[is.na(dat_merged$ActualEndDate)] <- as.character(dat_merged$ForecastEndDate[is.na(dat_merged$ActualEndDate)])
dat<-na.omit(unique(dat_merged[,c("Nordic", "FirstPublicationDate", "ActualEndDate")]))
dat$FirstPublicationDate<-as.Date(dat$FirstPublicationDate, tryFormats = c("%Y-%m-%d"))
dat$ActualEndDate<-as.Date(dat$ActualEndDate, tryFormats = c("%Y-%m-%d"))
colnames(dat)<-c("Nordic_number", "Stockout_starting", "Stockout_ending")

######################################################################################
# Get and prepare XML file
######################################################################################

rm(list = setdiff(ls(), "dat"))
library(dplyr)

# get rid of time period where dates are reversed

sel<-(dat$Stockout_ending-dat$Stockout_starting)<0
table(sel)
nr<-1:length(dat$Stockout_starting)

# First combine stockout times and remove duplicates, then add additional info.
# Get individual stockout days, calculate summary

table = data.frame()
length(dat$Stockout_starting)
for(i in nr[!sel]){
  a1<-seq(from=dat$Stockout_starting[i], to=dat$Stockout_ending[i], by="days")
  a2<-cbind(dat$Nordic_number[i], as.character(a1))
  colnames(a2)<-c("Nordic_number", "dates")
  table<-rbind(table, a2)
  print(i)
}

# assigne dates to groups
# remove duplicates
table<-unique(table)

table$year <- str_sub(table$dates, 1,4)
table$dates_mon <- as.character(as.yearmon(as.Date(table$dates)))
table$dates_week <- as.character(weekdays(as.Date(table$dates))) 

SE_data_all<-table %>% 
  count(Nordic_number) 
SE_data_year<-table %>% 
  count(Nordic_number, year) 
SE_data_month<-table %>% 
  count(Nordic_number, dates_mon) 
SE_data_week<-table %>% 
  count(Nordic_number, dates_week) 

rm(list = setdiff(ls(), c ("SE_data_all", "SE_data_year", "SE_data_month", "SE_data_week")))
gc()

######################################################################################
# stockout table over time
######################################################################################

load("~/R_projects/Sandoz_DA/Data/Sales data.RData")

# separately for countries for many reasons
# combine just to get company and substance info
ALL_SE<-subset(All, Country =="se")
ALL_SE<-unique(ALL_SE[,c("Company", "Substance", "Nordic.ArticleNo")])

TABLE_year<-merge(SE_data_year, ALL_SE, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)
TABLE_month<-merge(SE_data_month, ALL_SE, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)
TABLE_all<-merge(SE_data_all, ALL_SE, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)
TABLE_week<-merge(SE_data_week, ALL_SE, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)

######################################################################################
# Sales table 2020
######################################################################################

# combine 2020 stockout info with the sales table
dat<-subset(All, Country =="se")
dat<-unique(dat[,c("Country","Company", "Substance", "Nordic.ArticleNo",  "Sales.USD")])

ALL_SE<-subset(SE_data_year, year=="2020")
head(ALL_SE)
TABLE_SALES<-merge(ALL_SE, dat, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)

# duplications 
# split_check<-split(TABLE_SALES, f=TABLE_SALES$Nordic_number)
# split_check_dupli<-split_check[sapply(split_check, nrow)>1]



######################################################################################
# SAVE
######################################################################################

save(TABLE_SALES, TABLE_week, TABLE_month,TABLE_all,TABLE_week, file="Data/Sweeden data3.RData")




