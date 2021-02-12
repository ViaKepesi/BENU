#################
#INSTALL PACKAGES
#################

#install required packages
install.packages("e1071")
install.packages("class")
install.packages("randomForest")
install.packages("kernlab")
install.packages("ggplot2")
install.packages("psych")
install.packages("dplyr")
install.packages("forecast")
install.packages("stringr") 
install.packages("sos")
install.packages("enc")
install.packages("utf8")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("zoo")
install.packages("tseries")

#call the requiered libraries
library("e1071") # fuzzy clustering, naive-bayes
library("class") # kNN clustering
library("randomForest") # randomforest
library("kernlab") # SVM
library("ggplot2") # visualization
library("psych") # diagnostics
library("dplyr") # lek√©rdez√©sek
library("forecast") # timeseries forcast
library("stringr") #string manipulation
library("sos") #checking library which package is part of
library("enc") #fixing encoding
library("utf8") #fixing encoding
library("lubridate") #fixing dates
library("tidyverse") #for data cleaning
library("reshape2")
library("zoo") #plotting ts
library(tseries, quietly = T) #run Dickey-Fuller test to indentify stationary data

############
#DATA IMPORT
############
#import datasets
benu_daily_data<-read.csv("C:/Users/malejin/Documents/Rworkinglibrary/_beadando_tobbvaltozos/benu_daily_raw_data.csv", sep=",", dec=".")
benu_daily_data

covid_daily_data<-read.csv("C:/Users/malejin/Documents/Rworkinglibrary/_beadando_tobbvaltozos/covid_daily_raw_data.csv", sep=",", dec=".")
covid_daily_data

benu_generic_data<-read.csv("C:/Users/malejin/Documents/Rworkinglibrary/_beadando_tobbvaltozos/benu_raw_data.csv", sep=",", dec=".")
benu_generic_data

#checking data of the datasets
str(benu_daily_data)
str(covid_daily_data)
str(benu_generic_data)

###########################
#DATA PREPROCESSING PART I.
###########################

#Remove 1st column as it comes from the previous export of the dataframes
#Also remove ogyi_product_name as in this case its a duplicate of product_name
benu_daily_data=select(benu_daily_data, -c(1,2))
benu_daily_data

covid_daily_data=select(covid_daily_data, -1)
covid_daily_data

benu_generic_data<-select(benu_generic_data, c(3,11))
benu_generic_data

#Handling wrong encoding
#findFn("is_utf8")
is_utf8(benu_daily_data$ogyi_product_name)

print(colnames(benu_generic_data[,1:7]))
for (col in colnames(benu_daily_data[,1:7])){
  Encoding(benu_daily_data[[col]]) <- "UTF-8"}
benu_daily_data

for (col in colnames(benu_generic_data)){
  Encoding(benu_generic_data[[col]]) <- "UTF-8"}
benu_generic_data

#NA data is shown as "√ºres", fixing NA
benu_daily_data[benu_daily_data=="√ºres" ]<- NA
benu_daily_data[benu_daily_data=="¸res" ]<- NA
benu_daily_data

#Fixing dates(instead of timestamp we need the actual date)
benu_daily_data$date<-as.Date(benu_daily_data$date)
benu_daily_data

covid_daily_data$date<-as.Date(covid_daily_data$date)
covid_daily_data

#Removing promo and original price as we are working only with price
benu_daily_data_f1=select(benu_daily_data, -c(3,4,5))
benu_daily_data_f1

#Fixing price as numeric value
sapply(benu_daily_data_f1, class)  
benu_daily_data_f1$price <- parse_number(as.character(benu_daily_data_f1$price), locale = locale("hu", decimal_mark = ",", grouping_mark = " "), trim_ws = TRUE)
benu_daily_data_f1
sapply(benu_daily_data_f1, class) 

#check again missing data
length(which(is.na(benu_daily_data_f1==TRUE))) 
na_rows=is.na(benu_daily_data_f1==TRUE)

#if availability is NA that means the product is most possible not available
benu_daily_data_f1$availability[is.na(benu_daily_data_f1$availability)] <- "Nincs kÈszleten"
benu_daily_data_f1
#this is a categorical variable, this can be replaced with 0-1 dummy variables
benu_daily_data_f1$availability = factor(benu_daily_data_f1$availability,
                           levels = c('Nincs kÈszleten', 'KÈszleten'),
                           labels = c(0, 1))
benu_daily_data_f1$availability

#drop duplicated rows where product_name&date are identical
benu_daily_data_f2<-(benu_daily_data_f1 %>% distinct(product_name, date, .keep_all = TRUE))
nrow(benu_daily_data_f2)

benu_generic_data<-(benu_generic_data %>% distinct(product_name, product_category, .keep_all = TRUE))
nrow(benu_generic_data)

covid_daily_data<-(covid_daily_data %>% distinct(date, .keep_all = TRUE))
nrow(covid_daily_data)

#Sort dataframes by date
benu_daily_data_f2<- benu_daily_data_f2[order(benu_daily_data_f2$date),]
covid_daily_data<-covid_daily_data[order(covid_daily_data$date),]
benu_daily_data_f2

#Remove those products that have no price during the examined period
product_list<-sort(unique(benu_daily_data_f2$product_name))
product_list
length(product_list)
str(product_list[1])

list_of_prices_per_prod<-list()
list_of_product_name<-list()
for (i in (1:length(product_list))){
  temp_product_name_list<-list(benu_daily_data_f2[benu_daily_data_f2$product_name==product_list[i],]$product_name)
  temp_price_list<-list(benu_daily_data_f2[benu_daily_data_f2$product_name==product_list[i],]$price)
  print(all(is.na(unlist(temp_price_list))))
  if (all(is.na(unlist(temp_price_list)))){
    list_of_product_name<-c(list_of_product_name,temp_product_name_list )
  }
}
length(list_of_product_name)
product_with_no_price<-unique(unlist(list_of_product_name))

benu_daily_data_f2<-benu_daily_data_f2[!(benu_daily_data_f2$product_name %in% product_with_no_price),]
nrow(benu_daily_data_f2)

#Create regression modell for inputing NA values
benu_daily_data_test_NA<-benu_daily_data_f2[c("price","date")]
covid_daily_data_test_NA<-covid_daily_data[c("new_case","death_stat","date")]
benu_daily_data_test_NA_merged<-merge(benu_daily_data_test_NA, covid_daily_data_test_NA, by.x=c("date"), by.y=c("date"))
benu_daily_data_test_NA_merged

benu_daily_data_test_NA_merged<-benu_daily_data_test_NA_merged %>% mutate_if(is.numeric, round, digits=2)
benu_daily_data_test_NA_merged

symnum(cor(benu_daily_data_test_NA_merged[c("price","new_case")], use = "complete.obs"))
symnum(cor(benu_daily_data_test_NA_merged[c("price","death_stat")], use = "complete.obs"))

missDummy <- function(t)
{
  x <- dim(length(t)) 
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}

benu_daily_data_test_NA_merged$dummy <- missDummy(benu_daily_data_test_NA_merged$price)
benu_daily_data_test_NA_merged

lm(price ~ new_case, benu_daily_data_test_NA_merged)
lm(price ~ death_stat, benu_daily_data_test_NA_merged)

benu_daily_data_test_NA_merged[benu_daily_data_test_NA_merged$dummy==0,]
for(i in (1:nrow(benu_daily_data_test_NA_merged))){
  #print(benu_daily_data_test_NA_merged$price[i])
  if(benu_daily_data_test_NA_merged$dummy[i] == 0)  {
    benu_daily_data_test_NA_merged$price[i] = (2150.8171 + -0.1847*benu_daily_data_test_NA_merged$death_stat[i])
  }
}
benu_daily_data_test_NA_merged[benu_daily_data_test_NA_merged$dummy==0,]

benu_daily_data_f2$price<-benu_daily_data_test_NA_merged$price
benu_daily_data_f2$price

#inputing NAs with KNN

#Check which are the remaining rows with NA price
benu_daily_data_f2_NAs<-benu_daily_data_f2 %>% filter_all(any_vars(is.na(.)))
nrow(benu_daily_data_f2_NAs)

#These product had no price added during the examination period. Most probably these products are available in the webshop just to show the availability of the product on stock and it can be purchased only in the phisical shop. Therefore the related rows can be removed.
benu_daily_data_f3<-benu_daily_data_f2 %>% drop_na()
nrow(benu_daily_data_f3)

#Double check idf surely there isnt NAs
length(which(is.na(benu_daily_data_f3==TRUE))) 
benu_daily_data_f3

#DATA PREPOCESSING PART II.
#Taking price array per product for the examined period
product_list<-sort(unique(benu_daily_data_f3$product_name))
product_list
length(product_list)
str(product_list[1])

list_of_prices_per_prod<-list()
list_of_product_name<-list()
list_of_mean_price<-list()
for (i in (1:length(product_list))){
  temp_product_name_list<-list(benu_daily_data_f3[benu_daily_data_f3$product_name==product_list[i],]$product_name)
  temp_price_list<-list(benu_daily_data_f3[benu_daily_data_f3$product_name==product_list[i],]$price)
  temp_mean_price<-mean(unlist(benu_daily_data_f3[benu_daily_data_f3$product_name==product_list[i],]))
  #print(temp_mean_price)
  list_of_prices_per_prod<-c(list_of_prices_per_prod, temp_price_list)
  list_of_product_name<-c(list_of_product_name,temp_product_name_list )
  list_of_mean_price<-c(list_of_mean_price,temp_mean_price)
}
length(list_of_prices_per_prod)
length(list_of_product_name)
length(list_of_mean_price)


#################################
#DESCRIPTIVE STATS & DATA INSIGHT
#################################
benu_daily_data_f3
summary(benu_daily_data_f3)

#Calculate mean, max, min price and price change per product
mean_price_per_product_v2<-aggregate(price ~ product_name, benu_daily_data_f3, mean)
nrow(mean_price_per_product_v2)
mean_price_per_product_v2$list_of_prices<-list_of_prices_per_prod
mean_price_per_product_v2

descriptive_stats<-mean_price_per_product_v2
nrow(descriptive_stats)
names(descriptive_stats)[names(descriptive_stats) == "price"] <- "mean_price"  #price is actually mean price, so column should be renamed correctly 
descriptive_stats

nr_of_price_change_list<-list()
max_price_list<-list()
min_price_list<-list()
price_change_list<-list()
for (i in (1:nrow(descriptive_stats))){
  #print(length(unique(unlist(descriptive_stats$list_of_prices[i]))))
  nr_of_price_change_list<-c(nr_of_price_change_list,length(unique(unlist(descriptive_stats$list_of_prices[i]))))
  #print(max(sapply(descriptive_stats$list_of_prices[i], max)))
  max_price_list<-c(max_price_list,max(sapply(descriptive_stats$list_of_prices[i], max)))
  #print(min(sapply(descriptive_stats$list_of_prices[i], min)))
  min_price_list<-c(min_price_list,min(sapply(descriptive_stats$list_of_prices[i], min)))
  #print(max(sapply(descriptive_stats$list_of_prices[i], max))-min(sapply(descriptive_stats$list_of_prices[i], min)))
  price_change_list<-c(price_change_list,max(sapply(descriptive_stats$list_of_prices[i], max))-min(sapply(descriptive_stats$list_of_prices[i], min)))
}

descriptive_stats$nr_of_price_change<-nr_of_price_change_list
descriptive_stats$max_price<-max_price_list
descriptive_stats$min_price<-min_price_list
descriptive_stats$price_change<-price_change_list
nrow(descriptive_stats)

#Find those prodct where there was price change
descriptive_stats_prc<-descriptive_stats[descriptive_stats$nr_of_price_change>1,]
descriptive_stats_prc
nrow(descriptive_stats_prc) #35 products had price change in the examined period

descriptive_stats_prc$price_change<-parse_number(as.character(descriptive_stats_prc$price_change), locale = locale("hu", decimal_mark = ",", grouping_mark = " "), trim_ws = TRUE)
descriptive_stats_prc

benu_daily_data_f4<-filter(benu_daily_data_f3,product_name %in% descriptive_stats_prc$product_name)
benu_daily_data_f4
length(unique(benu_daily_data_f4$product_name))

#Find those products where the most time the price was changed
max_nr_price_change<-max(sapply(descriptive_stats$nr_of_price_change, max))
descriptive_stats_max_nr_prc<-descriptive_stats[descriptive_stats$nr_of_price_change==max_nr_price_change,]
nrow(descriptive_stats_max_nr_prc) #14 product had 2times price change in the examined period

benu_daily_data_f5<-filter(benu_daily_data_f3,product_name %in% descriptive_stats_max_nr_prc$product_name)
benu_daily_data_f5
length(unique(benu_daily_data_f5$product_name))

#Find those products from the above ones that had the highest volume of price change
max_price_change<-max(sapply(descriptive_stats$price_change, max))
descriptive_stats_max_nr_prc_max_prc<-descriptive_stats_max_nr_prc[descriptive_stats_max_nr_prc$price_change==max_price_change,]
descriptive_stats_max_nr_prc_max_prc
nrow(descriptive_stats_max_nr_prc_max_prc) #2products had the highest price change with 700Ft in the examined period

benu_daily_data_f6<-filter(benu_daily_data_f3,product_name %in% descriptive_stats_max_nr_prc_max_prc$product_name)
benu_daily_data_f6
length(unique(benu_daily_data_f6$product_name))

#Visualize proportion of products that had no price change at all or had  2 or 3 times price change during the examined period
count_nr_prc<-rep(unlist(descriptive_stats$nr_of_price_change))
result_counts<-data.frame(unclass(rle(sort(count_nr_prc))))
result_counts
result_counts$values
result_counts$values[c(1, 2, 3)] <- c("No price change", "1 price chnage", "2 price change")
result_counts

result_counts <- result_counts %>%
  arrange(desc(values)) %>%
  mutate(lab.ypos = cumsum(lengths) - 0.5*lengths)
result_counts

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF")

ggplot(result_counts, aes(x = "", y = lengths, fill = values)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = lengths), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  labs(title = "Proportion of number of price changes")+
  theme(plot.title = element_text(hjust = 0.5))

#Visualize mean price of products that had price change during the examined period
ggplot(data=descriptive_stats_prc, aes(x=product_name, y=mean_price)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=round(mean_price, digits = 2)), vjust=0.3, hjust=-0.3, nudge_y=-900, color="white", size=2.5, angle=90)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=7))+
  labs(title = "Mean price of product with price change", x="product name", y="mean price in HUF")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

#Visualize volume of price change per product
descriptive_stats_prc_volume<-descriptive_stats_prc[c("product_name","price_change")]
descriptive_stats_prc_volume

ggplot(data=descriptive_stats_prc_volume, aes(x=reorder(product_name, -price_change), y=price_change)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=price_change), vjust=0.3, hjust=-0.3, color="black", size=2.5, angle=90)+
  ylim(0, 800)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=7))+
  labs(title = "Volume of price change per product", x="product name", y="mean price in HUF")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

#Visualize volume of price change and mean price per product
descriptive_stats_prc_mean_and_volume<-descriptive_stats_prc[c("product_name","mean_price","price_change")]
descriptive_stats_prc_mean_and_volume$price_change<-unlist(descriptive_stats_prc_mean_and_volume$price_change)
nrow(descriptive_stats_prc_mean_and_volume)

descriptive_stats_prc_mean_and_volume %>% 
  melt %>% 
  ggplot(aes(product_name, value, fill=variable)) +  geom_col(position = 'dodge')+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=7))+
  labs(title = "Proportion of mean price and price change", x="product name", y="mean price vs price change")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

#Visualize mean price per product category
descriptive_stats_prc_merged<- merge(descriptive_stats_prc, benu_generic_data, by.x=c("product_name"), by.y=c("product_name"))
descriptive_stats_prc_merged
descriptive_stats_prc_merged[descriptive_stats_prc_merged$product_name=="Mucofree 15 mg pasztilla 20x",]$product_category<-descriptive_stats_prc_merged[descriptive_stats_prc_merged$product_name=="Mucofree 30 mg/5 ml szirup 100ml",]$product_category #"kezdolap" is wrongly stated product category, as we know the product category of the Mucofree product family, we can safely replace it!
descriptive_stats_prc_merged

descriptive_stats_prc_grouped_pc_mean<-descriptive_stats_prc_merged %>%
  group_by(product_category) %>%
  summarize(mean_size = mean(mean_price, na.rm = TRUE))
descriptive_stats_prc_grouped_pc_mean
descriptive_stats_prc_grouped_pc_mean<-descriptive_stats_prc_grouped_pc_mean %>% mutate_if(is.numeric, round, digits=2)

descriptive_stats_prc_grouped_pc_mean$ymax <- cumsum(descriptive_stats_prc_grouped_pc_mean$mean_size)
descriptive_stats_prc_grouped_pc_mean$ymin <- c(0, head(descriptive_stats_prc_grouped_pc_mean$ymax, n=-1))
descriptive_stats_prc_grouped_pc_mean$labelPosition <- ((descriptive_stats_prc_grouped_pc_mean$ymax + descriptive_stats_prc_grouped_pc_mean$ymin) / 2)
descriptive_stats_prc_grouped_pc_mean$label <- paste0(descriptive_stats_prc_grouped_pc_mean$mean_size," ","HUF")

ggplot(descriptive_stats_prc_grouped_pc_mean, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=product_category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=product_category), size=3) + 
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  labs(title = "Mean price per product catgeory")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right")


#Visualize price change per product category
descriptive_stats_prc_grouped_pc_prc<-descriptive_stats_prc_merged %>%
  group_by(product_category) %>%
  summarize(mean_size = mean(price_change, na.rm = TRUE))
descriptive_stats_prc_grouped_pc_prc
descriptive_stats_prc_grouped_pc_prc<-descriptive_stats_prc_grouped_pc_prc %>% mutate_if(is.numeric, round, digits=2)

descriptive_stats_prc_grouped_pc_prc$ymax <- cumsum(descriptive_stats_prc_grouped_pc_prc$mean_size)
descriptive_stats_prc_grouped_pc_prc$ymin <- c(0, head(descriptive_stats_prc_grouped_pc_prc$ymax, n=-1))
descriptive_stats_prc_grouped_pc_prc$labelPosition <- ((descriptive_stats_prc_grouped_pc_prc$ymax + descriptive_stats_prc_grouped_pc_prc$ymin) / 2)
descriptive_stats_prc_grouped_pc_prc$label <- paste0(descriptive_stats_prc_grouped_pc_prc$mean_size," ","HUF")

ggplot(descriptive_stats_prc_grouped_pc_prc, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=product_category)) +
  geom_rect() +
  geom_text( x=2.25, aes(y=labelPosition, label=label, color=product_category), size=3) + 
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  labs(title = "Price change per product catgeory")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right")


################################
#MODELLING 
################################

#TIMESERIES ANALYSIS - AUTO ARIMA
#plot Dassergo timeseries
plot(zoo(benu_daily_data_f6[benu_daily_data_f6$product_name=="Dassergo 5 mg filmtabletta 30x",]$price, seq(from=as.Date("2020-07-25"),to=as.Date("2020-09-10"), by=1)))
#Plot Innaler timeseries
benu_daily_data_f6[benu_daily_data_f6$product_name=="Inaller 5 mg filmtabletta 30x",]
plot(zoo(benu_daily_data_f6[benu_daily_data_f6$product_name=="Inaller 5 mg filmtabletta 30x",]$price, seq(from=as.Date("2020-07-25"), to=as.Date("2020-09-10"), by=1)))
#Preparation
ts(benu_daily_data_f6, start = 2020-07-25)
ts(covid_daily_data, start = 2020-07-25)

dassergo<-ts(benu_daily_data_f6[benu_daily_data_f6$product_name=="Dassergo 5 mg filmtabletta 30x",]$price)
innaler<-ts(benu_daily_data_f6[benu_daily_data_f6$product_name=="Inaller 5 mg filmtabletta 30x",]$price)
covid_new_case<-ts(covid_daily_data$new_case)
covid_death_stat<-ts(covid_daily_data$death_stat)

#Dickey-Fuller&KPSS tests to see if the dataset is stationary or non-stationary
#ADF test
adf.test(dassergo) #->p>0,05 Ho is rejected, alternative hypothesis: stationary
adf.test(innaler) #->p>0,05 Ho is rejected, alternative hypothesis: stationary
#KPSS test
kpss.test(dassergo) #->p<0,05 so we accept, H0 is accepted: stationary
kpss.test(innaler) #->p>0,05 Ho is rejected, alternative hypothesis: non-stationary
#Based on ADF and KPSS Dassergo shows strict-stationarity
#Based on ADF and KPSS Innaler shows trend-stationarity

#ACF of price
acf(dassergo, lag.max = 10)
acf(innaler, lag.max = 10)

#CCF of price
?ccf
ccf(dassergo, covid_new_case, lag.max = 10)
ccf(dassergo, covid_death_stat, lag.max = 10)

ccf(innaler, covid_new_case, lag.max = 10)
ccf(innaler, covid_death_stat, lag.max = 10)


#Decompose
?stl
stl(covid_new_case)
stl(covid_death_stat)
stl(dassergo)
stl(innaler)
#Error in stl(covid_new_case): series is not periodic or has less than two periods

?decompose
plot(decompose(ts(benu_daily_data_f6[benu_daily_data_f6$product_name=="Dassergo 5 mg filmtabletta 30x",]$price, start = 2020, frequency =7)))

plot(decompose(ts(benu_daily_data_f6[benu_daily_data_f6$product_name=="Inaller 5 mg filmtabletta 30x",]$price, start = 2020, frequency =7)))


#Auto_arima
# auto.arima(dassergo)
# auto.arima(innaler)

auto.arima(dassergo, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
auto.arima(innaler, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)


fit_1<-auto.arima(dassergo)
forecast(fit_1, h = 10) # h is the period that you want to forecast. 
checkresiduals(fit_1)
accuracy(fit_1)
if (accuracy(fit_1)[2]<(0.1*descriptive_stats_max_nr_prc_max_prc[descriptive_stats_max_nr_prc_max_prc$product_name=="Dassergo 5 mg filmtabletta 30x",]$mean_price)){
  print(paste0("Accuracy is acceptable, error % is: ",(accuracy(fit_1)[2]/(descriptive_stats_max_nr_prc_max_prc[descriptive_stats_max_nr_prc_max_prc$product_name=="Dassergo 5 mg filmtabletta 30x",]$mean_price)*100)))
} else{
  print("Accuracy is acceptable")
}

fit_2<-auto.arima(innaler)
forecast(fit_2, h = 10) # h is the period that you want to forecast. 
checkresiduals(fit_2)
accuracy(fit_2)
if (accuracy(fit_2)[2]<(0.1*descriptive_stats_max_nr_prc_max_prc[descriptive_stats_max_nr_prc_max_prc$product_name=="Inaller 5 mg filmtabletta 30x",]$mean_price)){
  print(paste0("Accuracy is acceptable, error % is: ",(accuracy(fit_2)[2]/(descriptive_stats_max_nr_prc_max_prc[descriptive_stats_max_nr_prc_max_prc$product_name=="Inaller 5 mg filmtabletta 30x",]$mean_price)*100)))
} else{
  print("Accuracy is acceptable")
}

plot(forecast(fit_1))
plot(forecast(fit_2))


#innaler timeserie is trend stationary, lets try to differentiate it(Typically applied differentiation by trend stationary data)
infy_ret_innaler <- 100 * diff(log(innaler))
infy_ret_innaler

adf.test(infy_ret_innaler)
kpss.test(infy_ret_innaler)
acf(infy_ret_innaler)

fit_2b<-auto.arima(infy_ret_innaler)
fit_2b
forecast(fit_2b, h = 10) # h is the period that you want to forecast. 
checkresiduals(fit_2b)
accuracy(fit_2b)
if (accuracy(fit_2b)[2]<(0.1*descriptive_stats_max_nr_prc_max_prc[descriptive_stats_max_nr_prc_max_prc$product_name=="Inaller 5 mg filmtabletta 30x",]$mean_price)){
  print(paste0("Accuracy is acceptable, error % is: ",(accuracy(fit_2b)[2]/(descriptive_stats_max_nr_prc_max_prc[descriptive_stats_max_nr_prc_max_prc$product_name=="Inaller 5 mg filmtabletta 30x",]$mean_price)*100)))
} else{
  print("Accuracy is acceptable")
}
#Accuracy changed from 7.87400813080965 to 0.453460943012661-> result is much better

plot(forecast(fit_2b))


#ARIMA(0,0,0) is practically white noise(however ACF shows 2 lags, 1 significant)
#Try to add noise to see if ARIMA(0,0,0) remains
infy_ret_innaler <- 100 * diff(log(innaler))
infy_ret_innaler
noise <- rnorm(length(infy_ret_innaler), mean = 0, sd = 1)
infy_ret_innaler_with_noise <- ts(infy_ret_innaler + noise)
fit_2c<-auto.arima(infy_ret_innaler_with_noise)
fit_2c
forecast(fit_2c, h = 10) # h is the period that you want to forecast. 
checkresiduals(fit_2c)
accuracy(fit_2c)
if (accuracy(fit_2c)[2]<(0.1*descriptive_stats_max_nr_prc_max_prc[descriptive_stats_max_nr_prc_max_prc$product_name=="Inaller 5 mg filmtabletta 30x",]$mean_price)){
  print(paste0("Accuracy is acceptable, error % is: ",(accuracy(fit_2c)[2]/(descriptive_stats_max_nr_prc_max_prc[descriptive_stats_max_nr_prc_max_prc$product_name=="Inaller 5 mg filmtabletta 30x",]$mean_price)*100)))
} else{
  print("Accuracy is acceptable")
}
#it remained white noise
#plot(forecast(fit_2c))
