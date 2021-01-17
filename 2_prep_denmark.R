
rm(list = setdiff(ls(), "x"))
library(dplyr)
library(stringr)
library(zoo)

dat <- read.csv("~/R_projects/Sandoz_DA/Data/Send to candidates/Denmark_stockouts.csv")
dat$Stockout_start<-as.Date(dat$Stockout_start, tryFormats = c("%Y-%m-%d"))
dat$Stockout_end<-as.Date(dat$Stockout_end, tryFormats = c("%Y-%m-%d"))
colnames(dat)<-c("Nordic_number", "Stockout_starting", "Stockout_ending")

# First combine stockout times and remove duplicates, then add additional info.
# Get individual stockout days, calculate summary

table = data.frame()
length(dat$Stockout_starting)
# runs a bit long 
for(i in 1:length(dat$Stockout_starting)){
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

# format id numbers
while (nchar(table$Nordic_number)<6){
  table$Nordic_number<-str_pad(table$Nordic_number, 6, pad = "0")
}



DK_data_all<-table %>% 
  count(Nordic_number) 
DK_data_year<-table %>% 
  count(Nordic_number, year) 
DK_data_month<-table %>% 
  count(Nordic_number, dates_mon) 
DK_data_week<-table %>% 
  count(Nordic_number, dates_week) 

rm(list = setdiff(ls(), c ("DK_data_all", "DK_data_year", "DK_data_month", "DK_data_week")))
gc()


######################################################################################
# stockout table over time
######################################################################################

load("~/R_projects/Sandoz_DA/Data/Sales data.RData")

# separately for countries for many reasons
# combine just to get company and substance info
ALL_DK<-subset(All, Country =="dk")
ALL_DK<-unique(ALL_DK[,c("Company", "Substance", "Nordic.ArticleNo")])

TABLE_year<-merge(DK_data_year, ALL_DK, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)
TABLE_month<-merge(DK_data_month, ALL_DK, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)
TABLE_all<-merge(DK_data_all, ALL_DK, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)
TABLE_week<-merge(DK_data_week, ALL_DK, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)


######################################################################################
# Sales table 2020
######################################################################################

# combine 2020 stockout info with the sales table
dat<-subset(All, Country =="dk")
dat<-unique(dat[,c("Country","Company", "Substance", "Nordic.ArticleNo",  "Sales.USD")])

ALL_DK<-subset(DK_data_year, year=="2020")
head(ALL_DK)
TABLE_SALES<-merge(ALL_DK, dat, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)

# duplications 
# split_check<-split(TABLE_SALES, f=TABLE_SALES$Nordic_number)
# split_check_dupli<-split_check[sapply(split_check, nrow)>1]

######################################################################################
# SAVE
######################################################################################

save(TABLE_SALES, TABLE_week, TABLE_month,TABLE_all,TABLE_week, file="Data/Denmark data3.RData")
































