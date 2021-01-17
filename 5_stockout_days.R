

library(dplyr)
library(plotly)

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

######################################################################################
# Organize
######################################################################################

Data_tab<-TABLE_SALES
Data_tab$n<-as.numeric(Data_tab$n)
dat<-unique(Data_tab[,c("Nordic_number", "Company", "n")])
dat$n[is.na(dat$n)]<-0

# Filter Companies that sells more then 5 products total
# calculate stuff

dat_st<-subset(dat, n>0)
count_total_uniq<- dat_st %>% group_by(Company) %>% tally()
count_total<- dat %>% group_by(Company) %>% tally()
count_total_ski<-merge(count_total,count_total_uniq, by="Company")
sel_all<-subset(count_total, n>10)
Data_tab<- filter(Data_tab, Company %in% sel_all$Company)

count_stocout_days<-Data_tab %>% 
  group_by(Company) %>% 
  summarize(sum_days=sum(n, na.rm=TRUE))

count_country<-merge(count_stocout_days,count_total_ski)
colnames(count_country)<-c("Company","Stockout_days", "Nr_of_products", "uni_sk")
count_country$Nr_of_products<-as.numeric(count_country$Nr_of_products)
count_country$uni_sk<-as.numeric(count_country$uni_sk)

count_country$days_to_fill<-count_country$Nr_of_products*356
count_country$stockout_ratio<-(count_country$Stockout_days/count_country$days_to_fill)*100
count_country$stockout_ratio<-round(count_country$stockout_ratio,2)
count_country$uni_sk_ratio<-(count_country$uni_sk/count_country$Nr_of_products)*100
count_country<-count_country[!is.na(count_country$Company),]

######################################################################################
# Scatter plots
######################################################################################

rm(list = setdiff(ls(), "count_country"))

p0<-plot_ly( x = count_country$Stockout_days , 
             y = count_country$stockout_ratio,
             type = 'scatter',
             mode = 'markers',
             color = count_country$stockout_ratio, 
             hoverinfo="text",
             text = ~paste(count_country$Company, 
                           '<br>Nr.of.prod.:', count_country$Stockout_days,
                           '<br>St.days.:', count_country$Stockout_days,
                           '<br>Stockout %.:', count_country$stockout_ratio),
             size = count_country$Stockout_days,
             sizes = c(10, 50),
             marker = list(opacity = 0.5, sizemode = 'diameter'))
p0<-p0 %>% layout(yaxis = list(title = "Stockout days", tickfont = list(size = 12)), 
                  xaxis = list(title = "Number of prooducts", tickfont = list(size = 10)),
                  margin=list(l=10, t=30, r=10, b=10, pad=20),
                  title="Stockout rate %") %>% layout(showlegend = FALSE)%>% hide_colorbar()%>% config(scrollZoom = TRUE)
save(p0, file="Plot_data/Plot_basic_0.RData")

######################################################################################

p1<-plot_ly( x = count_country$Nr_of_products , 
             y = count_country$stockout_ratio,
             type = 'scatter',
             mode = 'markers',
             color = count_country$stockout_ratio, 
             hoverinfo="text",
             text = ~paste(count_country$Company, 
                           '<br>Nr.of.prod.:', count_country$Nr_of_products,
                           '<br>St.days.:', count_country$Stockout_days,
                           '<br>Stockout %.:', count_country$stockout_ratio),
             size = count_country$Nr_of_products,
             sizes = c(10, 50),
             marker = list(opacity = 0.5, sizemode = 'diameter'))
p1<-p1 %>% layout(yaxis = list(title = "Stockout rate %", tickfont = list(size = 12)), 
                  xaxis = list(title = "Number of prooducts", tickfont = list(size = 10)),
                  margin=list(l=10, t=30, r=10, b=10, pad=20),
                  title="Stockout rate %") %>% layout(showlegend = FALSE)%>% hide_colorbar()%>% config(scrollZoom = TRUE)
save(p1, file="Plot_data/Plot_basic_1.RData")

######################################################################################

p2<-plot_ly( x = count_country$uni_sk_ratio , 
             y = count_country$stockout_ratio,
             type = 'scatter',
             mode = 'markers',
             color = count_country$stockout_ratio, 
             hoverinfo="text",
             text = ~paste(count_country$Company, 
                           '<br>Nr.of.prod.:', count_country$uni_sk_ratio,
                           '<br>St.days.:', count_country$Stockout_days,
                           '<br>Stockout %.:', count_country$stockout_ratio),
             size = count_country$uni_sk_ratio,
             sizes = c(10, 50),
             marker = list(opacity = 0.5, sizemode = 'diameter'))
p2<-p2 %>% layout(yaxis = list(title = "Stockout rate %", tickfont = list(size = 12)), 
                  xaxis = list(title = "% unique SKU", tickfont = list(size = 10)),
                  margin=list(l=10, t=30, r=10, b=10, pad=20),
                  title="% SKU") %>% layout(showlegend = FALSE)%>% hide_colorbar()%>% config(scrollZoom = TRUE)
save(p2, file="Plot_data/Plot_basic_2.RData")

######################################################################################

p3<-plot_ly( x = count_country$uni_sk , 
             y = count_country$stockout_ratio,
             type = 'scatter',
             mode = 'markers',
             color = count_country$stockout_ratio, 
             hoverinfo="text",
             text = ~paste(count_country$Company, 
                           '<br>Nr.of.prod.:', count_country$uni_sk_ratio,
                           '<br>St.days.:', count_country$Stockout_days,
                           '<br>Stockout %.:', count_country$stockout_ratio),
             size = count_country$uni_sk_ratio,
             sizes = c(10, 50),
             marker = list(opacity = 0.5, sizemode = 'diameter'))
p3<-p3 %>% layout(yaxis = list(title = "Stockout rate %", tickfont = list(size = 12)), 
                  xaxis = list(title = "Number of unique SKU", tickfont = list(size = 10)),
                  margin=list(l=10, t=30, r=10, b=10, pad=20),
                  title="unique SKU") %>% layout(showlegend = FALSE)%>% hide_colorbar()%>% config(scrollZoom = TRUE)
save(p3, file="Plot_data/Plot_basic_3.RData")

######################################################################################

