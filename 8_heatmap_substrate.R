
library(reshape2)
library(dplyr)
library(zoo)
library(stringr)
library("NbClust")
library(data.table)
library(dendextend)
library(heatmaply)

rm(list = setdiff(ls(), "x"))
gc()

load("~/R_projects/Sandoz_DA/Data/Denmark data3.RData")
TABLE_SALES_DK<-TABLE_SALES
TABLE_month_DK<-TABLE_month
country<-"DK"

load("~/R_projects/Sandoz_DA/Data/Finland data3.RData")
TABLE_SALES_FI<-TABLE_SALES
TABLE_month_FI<-TABLE_month
country<-"FI"

load("~/R_projects/Sandoz_DA/Data/Sweeden data3.RData")
TABLE_SALES_SE<-TABLE_SALES
TABLE_month_SE<-TABLE_month
country<-"SE"

TABLE_SALES<-dplyr::bind_rows(TABLE_SALES_SE, TABLE_SALES_FI, TABLE_SALES_DK)
TABLE_month<-dplyr::bind_rows(TABLE_month_SE, TABLE_month_FI, TABLE_month_DK)
country<-"ALL"

rm(list = setdiff(ls(), c("TABLE_SALES", "TABLE_month", "country")))
gc()

######################################################################################
# Organize
######################################################################################

# Organize data

TABLE_month$n<-as.numeric(TABLE_month$n)
TABLE_month$n[is.na(TABLE_month$n)]<-0
dat<-data.frame(TABLE_month$n, TABLE_month$Substance, TABLE_month$dates_mon)
colnames(dat)<-c("n", "Substance", "date")
dat<-(dat) %>% 
  group_by(Substance, date) %>% 
  summarise(Frequency = sum(n))
dat<-dcast(dat, Substance ~ date)
dat[is.na(dat)]<-0

# Normalize 
# keep Companies that have >10 products
# get the count from company table (2020, assume they sell about the same in 2018,2019)

count_uniq<-unique(TABLE_SALES[,c("Nordic_number", "Substance")]) %>% 
  count(Substance) 
keep<-count_uniq$n>5

#comp<-count_uniq$Substance[keep]

# merge and divide by days
# delete unnesesery column and na row
count_uniq$n<-count_uniq$n*30
dat_norm<-merge(dat, count_uniq, by.x="Substance", by.y="Substance")
dat_norm2<-dat_norm[,-1]/ dat_norm[,"n"]
row.names(dat_norm2)<-dat_norm[,1]

dat_norm2<-dat_norm2[,-length(colnames(dat_norm2))]# delete 
dat_norm2<-dat_norm2[,-length(colnames(dat_norm2))]
dat_norm2<-dat_norm2*100 # % a month / over 100% has more the 30 stockout
dat_norm2<-dat_norm2[keep,]
dat_norm2<-dat_norm2[-length(row.names(dat_norm2)),]
# OPTIONAL delete companies with very low stockout rate
# there are companies in the 2020 table that are not in any of the stockout tables 


keep<-(rowMeans(dat_norm2, na.rm=TRUE)>5)
table(keep)

if (country == "DK") {
  keep<-(rowMeans(dat_norm2, na.rm=TRUE)>10)
  dat_norm2<-dat_norm2[keep,]
  }
if (country == "FI") {
  keep<-(rowMeans(dat_norm2, na.rm=TRUE)>1)
  dat_norm2<-dat_norm2[keep,]
}
if (country == "SE") {
  keep<-(rowMeans(dat_norm2, na.rm=TRUE)>0)
  dat_norm2<-dat_norm2[keep,]
}
if (country == "ALL") {
  keep<-(rowMeans(dat_norm2, na.rm=TRUE)>5)
  dat_norm2<-dat_norm2[keep,]
}


######################################################################################
# Scale, order
######################################################################################

sel_scale <-t(scale(t(dat_norm2), scale=TRUE, center=TRUE))

order_list<-as.yearmon(seq(as.Date('2018-01-01'),as.Date('2021-01-01'),by = "months"))
length(order_list)
if (country == "SE") {order_list<-as.yearmon(seq(as.Date('2018-02-01'),as.Date('2021-01-01'),by = "months"))}
length(order_list)
if (country == "FI") {order_list<-as.yearmon(seq(as.Date('2018-04-01'),as.Date('2021-01-01'),by = "months"))}
length(order_list)
if (country == "ALL") {order_list<-as.yearmon(seq(as.Date('2018-04-01'),as.Date('2021-01-01'),by = "months"))}
length(order_list)

sel<-colnames(sel_scale) %in% as.character(order_list)
dat_scaled <- sel_scale[, sel]
dat_scaled <- dat_scaled[, as.character(order_list)]
dat_scaled<-apply(dat_scaled, 2, round, 2)

# Order % data for hover text
notes<-dat_norm2[, sel] 
notes<-notes[, as.character(order_list)]
notes<-apply(notes, 2, round, 2)

# Clustering, custom dendrograme
nb<-NbClust(dat_scaled,  diss = NULL,  distance = "euclidean",
            min.nc = 1, max.nc = 5, 
            method = "complete", index = "hartigan")
clu<-as.numeric(as.character(nb$Best.nc[1]))

x  <- as.matrix(dat_scaled)
row_dend  <- x %>% 
  dist %>% 
  hclust(method = "ward.D") %>% 
  as.dendrogram %>%
  set("branches_k_color", k = clu)

# Color, ranges, text length

m1<-max(dat_scaled, na.rm=TRUE)
m2<-min(dat_scaled, na.rm=TRUE)
mm<-c(abs(m1), abs(m2))
custom.col <- rev(c("#700404","#c93636", "#ffffff", "#213fa3",   "#12024d"))
#custom.col <- (c("#292927","#23598c","#6ca1d4",  "#ffffff","#fff200", "#dbc251", "#c4b102" ))
row.names(dat_scaled)<-substr(row.names(dat_scaled), 0, 15)

# Hover text
text_dat<-notes
text_dat<- lapply(row.names(text_dat), function(colname) {
  paste0(text_dat[colname,], "%, ", colname)
})
text_dat <- data.frame(matrix(unlist(text_dat), nrow=nrow(notes), byrow=T))
colnames(text_dat)<-colnames(notes)
row.names(text_dat)<-row.names(notes)
text_dat <- lapply(colnames(text_dat), function(colname) {
  paste0(text_dat[ ,colname], ", ", colname)
})
text_dat <- data.frame(matrix(unlist(text_dat), ncol=ncol(notes), byrow=F))
colnames(text_dat)<-colnames(notes)
row.names(text_dat)<-row.names(notes)

# Heatmap
plot_heatmap<-heatmaply(dat_scaled,
                        plot_method= "plotly",
                        colors = custom.col,
                        limits =c(-max(mm),max(mm)),
                        margins = c(10, 10,50,100),
                        Rowv = row_dend,
                        cexRow=0.5,
                        cexCol = 0.8,
                        colorbar_ypos = 0,
                        colorbar_xpos = 0.05,
                        row_dend_left= TRUE,
                        Colv = FALSE,
                        #main=country,
                        main="Substance All",
                        custom_hovertext=text_dat) 
plot_heatmap

# save RData file
save(plot_heatmap, custom.col,mm,row_dend,country,text_dat,  file="Plot_data/Plot_Substance_All data.RData")








