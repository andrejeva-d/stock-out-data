
library(reshape2)
library(plotly)
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

######################################################################################
# Organize
######################################################################################

# Find Companys that have sold products more then one country
# Exclude negative earnings
TABLE_SALES$Sales.USD<-as.numeric(TABLE_SALES$Sales.USD)
Data_tab<-subset(TABLE_SALES,Sales.USD>0)

Data_tab$n<-as.numeric(Data_tab$n)
dat<-unique(Data_tab[,c("Nordic_number", "Company", "n", "Country")])
dat$n[is.na(dat$n)]<-0


#Sells nore the 5 prodocts total
count_total<- dat %>% group_by(Company) %>% tally()
sel_all<-subset(count_total, n>5)
Data_tab<- filter(Data_tab, Company %in% sel_all$Company)

#sells in 2 countrys at least
count_total<- dat %>% group_by(Company,Country) %>% tally()
sel<-data.frame(table(count_total$Company)>1)
sel<-row.names(subset(sel,sel[,1]==TRUE))
Data_tab_sel<- filter(Data_tab, Company %in% sel)
head(Data_tab)

length(unique(Data_tab_sel$Company))
length(unique(Data_tab$Company))

count_stocout_days<-Data_tab %>% 
  group_by(Company,Country) %>% 
  summarize(sum_days=sum(n, na.rm=TRUE))

count_country<-merge(count_stocout_days,count_total)
count_country$n<-as.numeric(count_country$n)
colnames(count_country)<-c("Company", "Country", "Stockout_days", "Nr_of_products")
count_country$days_to_fill<-count_country$Nr_of_products*356
count_country$stockout_ratio<-(count_country$Stockout_days/count_country$days_to_fill)*100
count_country$stockout_ratio<-round(count_country$stockout_ratio,2)

#Nompanys operatin in all 3 countries
count_country_all<-split(count_country, f=paste(count_country$Company))
count_country_all<-count_country_all[sapply(count_country_all, nrow)>2]
count_country_all = do.call(rbind, count_country_all)

# stockout in 1,2 or all 3 countries in 20202
count_sel<- count_country_all %>% group_by(Company) %>% filter (Stockout_days>0) %>% tally
sel<-subset(count_sel, n==1)
count_country_all_one<- filter(count_country_all, Company %in% sel$Company)
count_country_all_one$stockout_ratio[is.na(count_country_all_one$Stockout_days)]<-NA

sel<-subset(count_sel, n==2)
count_country_all_two<- filter(count_country_all, Company %in% sel$Company)
count_country_all_two$stockout_ratio[is.na(count_country_all_two$Stockout_days)]<-NA

sel<-subset(count_sel, n==3)
count_country_all_three<- filter(count_country_all, Company %in% sel$Company)
count_country_all_three$stockout_ratio[is.na(count_country_all_three$Stockout_days)]<-NA

######################################################################################
# Plot scaterplot
######################################################################################

custom.col <- rev(c("#e3d800","#369dc9","#12024d"))
plot_1<-function(tab){
  plot_ly( x = tab$Nr_of_products , y = tab$stockout_ratio,
           type = 'scatter',
           mode = 'markers',
           color = tab$Country, 
           hoverinfo="text",
           colors = custom.col,
           text = ~paste(tab$Company, 
                         '<br>Country :', tab$Country, 
                         '<br>Nr.of.prod.:', tab$Nr_of_products,
                         '<br>St.days.:', tab$Stockout_days,
                         '<br>Stockout %.:', tab$stockout_ratio
           ),
           size = tab$Nr_of_products,
           sizes = c(10, 50),
           xref = "x",yref = "y",
           showarrow = TRUE,
           font = list(family = "13px-apple-system, Courier New, monospace", size = 4, color = "black"),
           marker = list(opacity = 0.5, sizemode = 'diameter'))%>% 
    layout(yaxis = list(title = "Stockout rate %", tickfont = list(size = 12)), 
           xaxis = list(title = "Number of SKU", tickfont = list(size = 12)))
}

p1<-plot_1(count_country_all)
p2<-plot_1(count_country_all_one)
p3<-plot_1(count_country_all_two)
p4<-plot_1(count_country_all_three)

s2 <- subplot(p2,p3,p4, nrows = 3, shareX = TRUE, shareY = TRUE)
s1 <- subplot(p1, shareY = TRUE)
fig <- subplot(s1, s2, nrows = 1, margin = 0.04, widths =c(0.7,0.3),shareX = TRUE, shareY = TRUE)
fig %>% layout(showlegend = FALSE, title="% SKU by countries and companies")%>% config(scrollZoom = TRUE)

save(fig, file="Plot_data/Plot_country_1.RData")

######################################################################################
# Plot barplot company
######################################################################################

plot_3<-function(tab){
  tab2<-dcast(tab, Company ~ Country)
  x  <- scale(as.matrix(tab2[,-1]), center=TRUE)
  row_dend  <- x %>% 
    dist %>% 
    hclust(method = "ward.D2") 
  company_order<-tab2$Company[row_dend$order]
  tab$Company <- factor(tab$Company, levels = company_order)
  
  fig <- plot_ly(tab, x = ~Company, y = ~stockout_ratio, 
                 type = 'bar', 
                 name =~Country,
                 color = ~Country, 
                 colors=custom.col)
  fig <- fig %>% layout(yaxis = list(title = "%", tickfont = list(size = 10)), 
                        xaxis = list(title = NA, tickfont = list(size = 10)),
                        barmode = 'stack')
  fig <- fig %>% layout(bargap = 0.05,
                        legend = list(x = 1, y = 0.99, orientation = "v", font = list(size = 14)),
                        margin=list(l=10, t=10, r=10, b=10),
                        plot_bgcolor='#00000000', paper_bgcolor='#00000000')
}

p1<-plot_3(count_country_all)%>% layout(showlegend = TRUE, 
                                        margin=list(l=10, t=30, r=10, b=10, pad=20),
                                        title="% SKU by countries and companies")%>% config(scrollZoom = TRUE)
save(p1, file="Plot_data/Plot_country_2.RData")




