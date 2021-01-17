
rm(list = setdiff(ls(), "x"))
library(stringr)

dat <- read_excel("~/R_projects/Sandoz_DA/Data/Send to candidates/FI_SO.xlsx")
dat<-data.frame(apply(dat,2,tolower))
dat$Stockout_starting<-as.Date(dat$Stockout_starting, tryFormats = c("%Y-%m-%d"))
dat$Stockout_ending<-as.Date(dat$Stockout_ending, tryFormats = c("%Y-%m-%d"))

# format id numbers
dat$Nordic_number<-str_replace_all(dat$Nordic_number," ","")
while (nchar(dat$Nordic_number)<6){
  dat$Nordic_number<-str_pad(dat$Nordic_number, 6, pad = "0")
}

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

FI_data_all<-table %>% 
  count(Nordic_number) 
FI_data_year<-table %>% 
  count(Nordic_number, year) 
FI_data_month<-table %>% 
  count(Nordic_number, dates_mon) 
FI_data_week<-table %>% 
  count(Nordic_number, dates_week) 

# Combine back with originnal data

dat<-na.omit(unique(dat[,c("Nordic_number", "active_ingredients", "atcclass", "drugname")]))

FI_data_year<-merge(FI_data_year, dat, by.x="Nordic_number", by.y="Nordic_number", all=TRUE)
FI_data_month<-merge(FI_data_month, dat, by.x="Nordic_number", by.y="Nordic_number", all=TRUE)
FI_data_all<-merge(FI_data_all, dat, by.x="Nordic_number", by.y="Nordic_number", all=TRUE)
FI_data_week<-merge(FI_data_week, dat, by.x="Nordic_number", by.y="Nordic_number", all=TRUE)

rm(list = setdiff(ls(), c ("FI_data_all", "FI_data_year", "FI_data_month", "FI_data_week")))
gc()


######################################################################################
# stockout table over time
######################################################################################

load("~/R_projects/Sandoz_DA/Data/Sales data.RData")

# separately for countries for many reasons
# combine just to get company and substance info
ALL_FI<-subset(All, Country =="fi")
ALL_FI<-unique(ALL_FI[,c("Company", "Substance", "Nordic.ArticleNo")])

TABLE_year<-merge(FI_data_year, ALL_FI, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)
TABLE_month<-merge(FI_data_month, ALL_FI, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)
TABLE_all<-merge(FI_data_all, ALL_FI, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)
TABLE_week<-merge(FI_data_week, ALL_FI, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)

######################################################################################
# Sales table 2020
######################################################################################

# combine 2020 stockout info with the sales table
dat<-subset(All, Country =="fi")
dat<-unique(dat[,c("Country","Company", "Substance", "Nordic.ArticleNo",  "Sales.USD")])

ALL_FI<-subset(FI_data_year, year=="2020")
head(ALL_FI)
TABLE_SALES<-merge(ALL_FI, dat, by.x="Nordic_number", by.y="Nordic.ArticleNo", all=TRUE)

# duplications 
# split_check<-split(TABLE_SALES, f=TABLE_SALES$Nordic_number)
# split_check_dupli<-split_check[sapply(split_check, nrow)>1]


######################################################################################
# SAVE
######################################################################################

save(TABLE_SALES, TABLE_week, TABLE_month,TABLE_all,TABLE_week, file="Data/Finland data3.RData")































