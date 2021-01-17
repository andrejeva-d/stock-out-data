
library(dplyr)

######################################################################################
# Get Data
######################################################################################

rm(list = setdiff(ls(), "x"))
gc()

load("~/R_projects/Sandoz_DA/Data/Denmark data3.RData")
TABLE_SALES_DK<-TABLE_SALES
TABLE_month_DK<-TABLE_month
TABLE_SALES_DK$Country[is.na(TABLE_SALES_DK$Country)]<-"dk"
country<-"DK"

load("~/R_projects/Sandoz_DA/Data/Finland data3.RData")
TABLE_SALES_FI<-TABLE_SALES
TABLE_month_FI<-TABLE_month
TABLE_SALES_FI$Country[is.na(TABLE_SALES_FI$Country)]<-"fi"
country<-"FI"

load("~/R_projects/Sandoz_DA/Data/Sweeden data3.RData")
TABLE_SALES_SE<-TABLE_SALES
TABLE_month_SE<-TABLE_month
TABLE_SALES_SE$Country[is.na(TABLE_SALES_SE$Country)]<-"se"
country<-"SE"

TABLE_SALES<-dplyr::bind_rows(TABLE_SALES_SE, TABLE_SALES_FI, TABLE_SALES_DK)
TABLE_month<-dplyr::bind_rows(TABLE_month_SE, TABLE_month_FI, TABLE_month_DK)
country<-"ALL"

rm(list = setdiff(ls(), c("TABLE_SALES", "TABLE_month", "country")))
gc()

###############################################################################################################
###############################################################################################################

TABLE_SALES$n[is.na(TABLE_SALES$n)]<-0
TABLE_SALES$share<-1-(TABLE_SALES$n/366)
TABLE_SALES$Sales.USD<-as.numeric(as.character(TABLE_SALES$Sales.USD))
TABLE_SALES$USD_proj<-TABLE_SALES$Sales.USD/TABLE_SALES$share

#Sells nore the 5 prodocts total
count_total<- TABLE_SALES %>% group_by(Company) %>% tally()
sel_all<-subset(count_total, n>5)
A_GALA3<- filter(TABLE_SALES, Company %in% sel_all$Company)

count_total<- A_GALA3 %>% group_by(Substance) %>% tally()
sel_all<-subset(count_total, n>50)
A_GALA4<- filter(A_GALA3, Substance %in% sel_all$Substance)

# aa<-subset(TABLE_SALES, Company=="takeda")
# aa$lost<-aa$USD_proj-aa$Sales.USD
# takeda sold 777895 usd in 6 day, imagin what would happen if they were stocked other 360 days
# I exclude outlyers as it is reportanting issue most lykly
dat_sales<-subset(A_GALA4, A_GALA4$Sales.USD>0)
dat_sales2<-subset(dat_sales, dat_sales$n>330&dat_sales$Sales.USD>2000)
dat_sales3<-subset(dat_sales, !Nordic_number %in% dat_sales2$Nordic_number)
dat_sales<-dat_sales3

rm(list = setdiff(ls(), c("dat_sales")))
gc()

######################################################################################
# Plot barplot sales
######################################################################################

custom.col <- rev(c("#e3d800","#369dc9","#12024d"))

dat_1<-unique(dat_sales[,c("USD_proj", "Company", "Sales.USD", "Country")]) %>% 
  group_by(Company, Country) %>% 
  summarise(sales_real = sum(Sales.USD, na.rm=TRUE),
            sales_poten = sum(USD_proj, na.rm=TRUE))
dat_1$lost<-dat_1$sales_poten-dat_1$sales_real

# organize
tab<-dcast(dat_1[c("Company", "Country", "lost")], Company ~ Country)
tab[is.na(tab)]<-0
row.names(tab)<-tab[,1]
tab<-tab[,-1]
tab<-tab[rowSums(tab)>0,]
tab <- tab[!is.infinite(rowSums(tab)),]

# order
x  <- scale(tab, center=TRUE)
row_dend  <- tab %>% 
  dist %>% 
  hclust(method = "average") 
plot(row_dend)
company_order<-row.names(tab)[row_dend$order]
dat_1$Company <- factor(dat_1$Company, levels = company_order)
dat_1<-na.omit(dat_1)

fig <- plot_ly(droplevels(dat_1), 
               x = ~Company, 
               y = ~lost, 
               type = 'bar', 
               name =~Country,
               color = ~Country, 
               colors=custom.col)
fig <- fig %>% layout(yaxis = list(title = "Lost sales", tickfont = list(size = 12)), 
                      xaxis = list(title = NA, tickfont = list(size = 12)),
                      barmode = 'stack')
fig <- fig %>% layout(bargap = 0.05,
                      legend = list(x = 1, y = 0.99, orientation = "v", font = list(size = 14)),
                      margin=list(l=10, t=10, r=10, b=10),
                      plot_bgcolor='#00000000', paper_bgcolor='#00000000')
fig%>% config(scrollZoom = TRUE)

save(fig, file="Plot_data/Plot_sales_1.RData")

######################################################################################
# Plot barplot sales 2
######################################################################################

custom.col <- rev(c("#e3d800","#369dc9","#12024d"))

dat_1<-unique(dat_sales[,c("USD_proj", "Company", "Sales.USD","Substance", "Country")]) %>% 
  group_by(Substance, Country) %>% 
  summarise(sales_real = sum(Sales.USD, na.rm=TRUE),
            sales_poten = sum(USD_proj, na.rm=TRUE))
dat_1$lost<-dat_1$sales_poten-dat_1$sales_real

# organize
tab<-dcast(dat_1[c("Substance", "Country", "lost")], Substance ~ Country)
tab[is.na(tab)]<-0
row.names(tab)<-tab[,1]
tab<-tab[,-1]
tab<-tab[rowSums(tab)>0,]
tab <- tab[!is.infinite(rowSums(tab)),]

# order
x  <- scale(tab, center=TRUE)
row_dend  <- tab %>% 
  dist %>% 
  hclust(method = "average") 
plot(row_dend)
company_order<-row.names(tab)[row_dend$order]
dat_1$Substance <- factor(dat_1$Substance, levels = company_order)
dat_1<-na.omit(dat_1)

fig <- plot_ly(droplevels(dat_1), 
               x = ~Substance, 
               y = ~lost, 
               type = 'bar', 
               name =~Country,
               color = ~Country, 
               colors=custom.col)
fig <- fig %>% layout(yaxis = list(title = "Lost sales", tickfont = list(size = 12)), 
                      xaxis = list(title = NA, tickfont = list(size = 12)),
                      barmode = 'stack')
fig <- fig %>% layout(bargap = 0.05,
                      legend = list(x = 1, y = 0.99, orientation = "v", font = list(size = 14)),
                      margin=list(l=10, t=10, r=10, b=10),
                      plot_bgcolor='#00000000', paper_bgcolor='#00000000')
fig%>% config(scrollZoom = TRUE)

save(fig, file="Plot_data/Plot_sales_2.RData")


















