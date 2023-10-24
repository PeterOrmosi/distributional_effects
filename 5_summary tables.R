
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(simsalapar)
library(Hmisc)
library(broom)
library(ggpubr)
library(gtools)
library(ggmap)
library(readr)
library(compareDF)
library(ggmap)
library(tidyr)
library(reshape2)
library(geosphere)
library(data.table)
library(lmtest)
library(xtable)
library(xlsx)


setwd("c:/Users/peter/OneDrive - University of East Anglia/UEA/RESEARCH/Distributional effects/Australia/Data/") 



My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 12),
  legend.text=element_text(size=14),
  plot.title = element_text(size = 18))
theme_set(theme_minimal(base_size = 20)) 

weekly$Median.commuting.distance..kms. = weekly$Median.commuting.distance..kms./1.6
weekly$Average.commuting.distance..kms. = weekly$Average.commuting.distance..kms./1.6

fnrollmean <- function (x) {
  rollmean(x,3,align="center",na.pad=TRUE)
}

ischange<-function(x){
  c(FALSE, diff(x) != 0)
}

issmaller<-function(x){
  c(FALSE, diff(x) < 0)
}

islarger<-function(x){
  c(FALSE, diff(x) > 0)
}


number_ticks <- function(n) {function(limits) pretty(limits, n)}


load(file="weekly_mins_traffic_rnr.Rda")




## plot over time (whole length) - FIGURE 1


agg<-aggregate(weekly$ulp, by=list(weekly$time), FUN=mean, na.rm=TRUE)
agg2<-aggregate(weekly$petrol_ws, by=list(weekly$time), FUN=mean, na.rm=TRUE)
agg<-cbind(agg,agg2[,2])
colnames(agg)<-c("time",'price','wholesale')
agg$time=as.Date(agg$time)
agg<-agg[!is.na(agg$wholesale),]

ulp<-ggplot() + geom_line(mapping = aes(x = agg$time, y = agg$wholesale,  colour="#CCCCCC"),size = 1.5) + 
  geom_line(mapping = aes(x = agg$time, y = agg$price, colour="#333333"), size = 1.5) + 
  labs(y="Price (AUS cents)", x = "Time (weeks)") +  ggtitle('ULP') +theme(plot.title = element_text(hjust = 0.48)) +
  scale_colour_manual(values=c("#CCCCCC","#333333"),labels = c('wholesale' , 'retail')) + theme(legend.title=element_blank()) +
  My_Theme

ulp


## plot over time - deseasoned


agg<-aggregate(weekly$ulp_ds, by=list(weekly$time), FUN=mean, na.rm=TRUE)
agg2<-aggregate(weekly$petrol_ws_ds, by=list(weekly$time), FUN=mean, na.rm=TRUE)
agg<-cbind(agg,agg2[,2])
colnames(agg)<-c("time",'price','wholesale')
agg$time=as.Date(agg$time)
agg<-agg[!is.na(agg$wholesale),]

ulp2<-ggplot() + geom_line(mapping = aes(x = agg$time, y = agg$wholesale,  colour="#CCCCCC"),size = 1.5) + 
  geom_line(mapping = aes(x = agg$time, y = agg$price, colour="#333333"), size = 1.5) + 
  labs(y="Price (AUS cents)", x = "Time (weeks)") +  ggtitle('ULP') +theme(plot.title = element_text(hjust = 0.48)) +
  scale_colour_manual(values=c("#CCCCCC","#333333"),labels = c('wholesale' , 'retail')) + theme(legend.title=element_blank()) +
  My_Theme

ulp2

fig<-ggarrange(ulp,ulp2,nrow=2, common.legend = T, legend = "bottom") + My_Theme
fig

ggsave('retail_price_over_time_ds_rnr.pdf')


# time series seasoned v deseasoned - FIGURE 1


agg<-aggregate(weekly$ulp, by=list(weekly$time), FUN=mean, na.rm=TRUE)
agg2<-aggregate(weekly$ulp_ds, by=list(weekly$time), FUN=mean, na.rm=TRUE)
agg<-cbind(agg,agg2[,2])
colnames(agg)<-c("time",'price','wholesale')
agg$time=as.Date(agg$time)
agg<-agg[!is.na(agg$wholesale),]

ulp2<-ggplot() + geom_line(mapping = aes(x = agg$time, y = agg$wholesale,  colour="#CCCCCC"),size = 1) + 
  geom_line(mapping = aes(x = agg$time, y = agg$price, colour="#333333"), size = 1) + 
  labs(y="Price (AUS cents)", x = "Time (weeks)") +  ggtitle('ULP') +theme(plot.title = element_text(hjust = 0.48)) +
  scale_colour_manual(values=c("#CCCCCC","#333333"),labels = c('ULP price' , 'de-seasoned price')) + theme(legend.title=element_blank()) +
  My_Theme

ulp2

fig<-ggarrange(ulp2,nrow=1, common.legend = T, legend = "bottom") + My_Theme

ggsave('retail_price_over_time_ds_v_s_rnr.pdf')









# map showing distance for example - FIGURE 2

list_57<-c('753',"79", "228", "387", "409", "422", "442", "450", "497", "674", "756", "795", "798", "851", "856", "903", "943", "1014", "1041", "1068", "1089", "1198")


list_57_5<-c("79",  "387", "442", "450", "674", "798",  "851",  "903",  "1014", "1041", "1068", "1089")
list_57_2<-c("409", "422", "856", "1198")
list_57_1<-c("228", "497", "795", "943")

register_google(key = "AIzaSyB8y5la_pBtXvFVbrJ3XHcxAlbDz25ab14")  ## change this to your API key


only_stations2<-only_stations[only_stations$panel2 %in% list_57,]
only_stations2$color<-ifelse(only_stations2$panel2 %in% list_57_5,3,ifelse(only_stations2$panel2 %in% list_57_2,2,ifelse(only_stations2$panel2 %in% list_57_1,1,0)))


map_closeup <- get_map(location = c(lon =  116.0193, lat = -32.15274), zoom = 13)

ggmap(map_closeup) +
  geom_point(data = only_stations2, aes(x=lon, y = lat, colour = factor(color),shape=factor(color),size=factor(color)))  + 
  scale_shape_manual(values = c(16,18,17,19),
                     labels = c("68-69 SW Highway","within 1mi", "within 2mi", "within 5mi" ) ) +
  scale_colour_manual(values = c('black',"darkgreen","red","blue"),
                      labels = c("68-69 SW Highway","within 1mi", "within 2mi", "within 5mi" ) ) +
  scale_size_manual(values = c(5,4.5,4,3.5),
                    labels = c("68-69 SW Highway","within 1mi", "within 2mi", "within 5mi" ) ) +
  theme(legend.position="right",legend.title = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text=element_text(size=14)) +
  labs(caption = stat_full) + theme(plot.caption = element_text(hjust = 0, size = 11))
ggsave('indy_station_map.pdf')




## descriptive tables/figures --- Figure 3  Example petrol station and surrounding competition

only_stations<-read.csv('only_stations3.csv')

load('weekly_mins.Rda')

weekly2 = weekly2[weekly2$year>=2004,]
sub<-weekly2[weekly2$panel==753,]
stat_name<-as.character(unique(sub$BRAND_DESCRIPTION))
stat_address<-as.character(unique(sub$full_address))
stat_full<-paste0(stat_name,' station at ',stat_address)
time=ggplot(sub,aes(x=time,y=comp1km)) + geom_line(size=1.5) +  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Number of competitors within 1 mile") + xlab("Time") + geom_vline(xintercept=c(0), linetype="dotted") + 
  labs(caption = stat_full) + theme(plot.caption = element_text(hjust = 0, size = 11))
  #theme(legend.title = element_blank())
time
ggsave('time_competition_example_rnr.pdf')





# station level analysis - entry/exit - FIGURE 5

load(file='exit_data_nearest5_treat_traffic_rnr.Rda') # in the paper we use the full data because it's presented before the data is adjusted for traffic 

db2 = db[db$year>=2004,]

length(unique(db2$id))
db2=db2[,c('id','year','quarter','week')]

db2 = db2 %>% group_by(id) %>% slice(25)

agg_exit<-aggregate(id ~ year,db2, function(x) c(count = length(x)))
names(agg_exit)=c('year','exits')

exits<-ggplot(data=agg_exit, aes(x=year, y=exits)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Number of exits",limits=c(0,100)) + xlab("Year") 
exits

quarts <- aggregate(id ~ quarter,db2, function(x) c(count = length(x)))
names(quarts)=c('quarter',' exits ')
quarts_p <- ggtexttable(quarts, rows = NULL, theme = ttheme("mBlue")) 
exits<- exits + annotation_custom(ggplotGrob(quarts_p), xmin = 2015, ymin = 20)
exits

#exits by week
aggw = aggregate(id ~ week, db2, function(x) c(count = length(x)))
aggw$freq = fnrollmean(aggw$id)


load(file='entry_data_nearest5_treat_traffic_rnr.Rda')

db2 = db_entry[db_entry$year>=2004,]

length(unique(db2$id))
db2=db2[,c('id','year','quarter','week')]

db2 = db2 %>% group_by(id) %>% slice(25)

agg_entry<-aggregate(id ~ year,db2, function(x) c(count = length(x)))
names(agg_entry)=c('year','entries')

entries<-ggplot(data=agg_entry, aes(x=year, y=entries)) +
  geom_bar(stat="identity", fill="red")+
  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Number of entries",limits=c(0,100)) + xlab("Year") 
entries

quarts <- aggregate(id ~ quarter,db2, function(x) c(count = length(x)))
names(quarts)=c('quarter',' entries ')
quarts_p <- ggtexttable(quarts, rows = NULL, theme = ttheme("mRed")) 

entries <- entries + annotation_custom(ggplotGrob(quarts_p), xmin = 2005, ymin = 25)

figure<-ggarrange(exits,entries, ncol = 1, nrow = 2,common.legend = TRUE,legend = "bottom") 
figure
ggsave('entry_exit_over_time.pdf')

# plotting exit/entry by week

aggw2 = aggregate(id ~ week, db2, function(x) c(count = length(x)))
aggw2$freq = fnrollmean(aggw2$id)
aggweek = left_join(aggw[,1:2],aggw2[,1:2],by="week")
names(aggweek) = c('id','exit','entry')
long_week = gather(aggweek, type, count, exit:entry, factor_key=TRUE)

weeks<-ggplot(data=long_week, aes(x=id, y=count,group=type,col=type)) +
  geom_line(size=1.5) + scale_color_grey(start = 0, end = .7) +
  theme_minimal() +  My_Theme +  theme(legend.position="bottom",legend.title=element_blank()) +
   xlab("week") + ylab('number of exits/entries') 
weeks
ggsave('exits_entry_byweek.pdf')


## WEEKLY EXIT - showing number of exits and entries over the year


fnrollmean <- function (x) {
  rollmean(x,3,align="center",na.pad=TRUE)
}


load(file='exit_data_only_treat.Rda')

db2<-db[db$seq==0,]

agg<-aggregate(db2$panel, by=list(db2$week), FUN=length)
names(agg)=c('week','count')
agg$count<-fnrollmean(agg$count)
exit<-ggplot() + geom_line(mapping = aes(x = agg$week, y = agg$count), size = 2, color = "black") + xlab('weeks') + ylab('number of exits')
exit


load(file='entry_data_onlytreat.Rda')

db2<-db_entry[db_entry$seq==0,]

agg2<-aggregate(db2$panel, by=list(db2$week), FUN=length)
names(agg2)=c('week','count')
agg2$count<-fnrollmean(agg2$count)
entry<-ggplot() + geom_line(mapping = aes(x = agg2$week, y = agg2$count), size = 2, color = "black") + xlab('weeks') + ylab('number of entries')
entry

both=ggarrange(exit, entry, ncol=2)

both
ggsave('entry_exit_by_weeks.pdf')




#### TABLE 4



load(file='weekly_mins_traffic_rnr.Rda')



xs=quantile(weekly$median_income,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(income_cat=cut(median_income, breaks=xs, labels=c("low income","high income"))) # "0-15","15 or more"

xs=quantile(weekly$comp2km,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(comp_cat=cut(comp2km, breaks=xs, labels=c("low competition","high competition"))) # "0-15","15 or more"

selected = weekly[,c('margin_u_ds','income_cat','comp_cat')]


agg = aggregate(margin_u_ds ~ comp_cat + income_cat , data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg = do.call("data.frame", agg) 
names(agg)[3:4]= c('mean','sd')
agg[,3:4] = round(agg[,3:4],3)
agg$sd <- paste0("(", agg$sd, ")")

library(data.table)   
setDT(agg)   
agg2 <- dcast(agg, comp_cat ~ income_cat   , 
              value.var = c("mean", "sd"))

tab1 = agg2[,c(1:3)] # separating means
names(tab1)[2:3] = c('low income','high income')
tab1 <- mutate(tab1, id = rownames(tab1))
tab2 = agg2[,c(1,4:5)] # separating sd
names(tab2)[2:3] = c('low income','high income')
tab2 <- mutate(tab2, id = rownames(tab2))

table = rbindlist(list(tab1, tab2))[order(id)]
table = table[,-4]
table

write.xlsx(table,'income_comp_margin_traffic.xlsx')





## examine margin distribution for main variables - Table 4

load(file="weekly_mins_traffic_rnr.Rda")

weekly$home_internet = weekly$Internet.accessed.from.dwelling.num/weekly$population

weekly2 = weekly
weekly2$commute = ifelse(weekly2$Median.commuting.distance..kms.>median(weekly2$Median.commuting.distance..kms.,na.rm=T),1,0)
weekly2$comp1mi = ifelse(weekly2$comp1km>median(weekly2$comp1km,na.rm=T),1,0)
weekly2$comp2mi = ifelse(weekly2$comp2km>median(weekly2$comp2km,na.rm=T),1,0)
weekly2$comp5mi = ifelse(weekly2$comp5km>median(weekly2$comp5km,na.rm=T),1,0)
weekly2$income_cat = ifelse(weekly2$median_income>median(weekly2$median_income,na.rm=T),1,0)
weekly2$education = ifelse(weekly2$IEO>median(weekly2$IEO,na.rm=T),1,0)
weekly2$pensioners = ifelse(weekly2$A65PLUS>median(weekly2$A65PLUS,na.rm=T),1,0)
weekly2$internet = ifelse(weekly2$home_internet>median(weekly2$home_internet,na.rm=T),1,0)

agg1 = ddply(weekly2, .(commute), summarize, ulp=mean(margin_u_ds,na.rm=T), ulp_sd=sd(margin_u_ds,na.rm=T),
             diesel=mean(margin_d_ds,na.rm=T), diesel_sd=sd(margin_d_ds,na.rm=T))
agg1 = cbind(t(agg1[1:2,2:3]),t(agg1[1:2,4:5]))

agg2 = ddply(weekly2, .(comp1mi), summarize, ulp=mean(margin_u_ds,na.rm=T), ulp_sd=sd(margin_u_ds,na.rm=T),
             diesel=mean(margin_d_ds,na.rm=T), diesel_sd=sd(margin_d_ds,na.rm=T))
agg2 = cbind(t(agg2[1:2,2:3]),t(agg2[1:2,4:5]))

agg3 = ddply(weekly2, .(comp2mi), summarize, ulp=mean(margin_u_ds,na.rm=T), ulp_sd=sd(margin_u_ds,na.rm=T),
             diesel=mean(margin_d_ds,na.rm=T), diesel_sd=sd(margin_d_ds,na.rm=T))
agg3 = cbind(t(agg3[1:2,2:3]),t(agg3[1:2,4:5]))

agg4 = ddply(weekly2, .(comp5mi), summarize, ulp=mean(margin_u_ds,na.rm=T), ulp_sd=sd(margin_u_ds,na.rm=T),
             diesel=mean(margin_d_ds,na.rm=T), diesel_sd=sd(margin_d_ds,na.rm=T))
agg4 = cbind(t(agg4[1:2,2:3]),t(agg4[1:2,4:5]))

agg5 = ddply(weekly2, .(income_cat), summarize, ulp=mean(margin_u_ds,na.rm=T), ulp_sd=sd(margin_u_ds,na.rm=T),
             diesel=mean(margin_d_ds,na.rm=T), diesel_sd=sd(margin_d_ds,na.rm=T))
agg5 = cbind(t(agg5[1:2,2:3]),t(agg5[1:2,4:5]))

agg6 = ddply(weekly2, .(education), summarize, ulp=mean(margin_u_ds,na.rm=T), ulp_sd=sd(margin_u_ds,na.rm=T),
             diesel=mean(margin_d_ds,na.rm=T), diesel_sd=sd(margin_d_ds,na.rm=T))
agg6 = cbind(t(agg6[1:2,2:3]),t(agg6[1:2,4:5]))

agg7 = ddply(weekly2, .(pensioners), summarize, ulp=mean(margin_u_ds,na.rm=T), ulp_sd=sd(margin_u_ds,na.rm=T),
             diesel=mean(margin_d_ds,na.rm=T), diesel_sd=sd(margin_d_ds,na.rm=T))
agg7 = cbind(t(agg7[1:2,2:3]),t(agg7[1:2,4:5]))

agg8 = ddply(weekly2, .(internet), summarize, ulp=mean(margin_u_ds,na.rm=T), ulp_sd=sd(margin_u_ds,na.rm=T),
             diesel=mean(margin_d_ds,na.rm=T), diesel_sd=sd(margin_d_ds,na.rm=T))
agg8 = cbind(t(agg8[1:2,2:3]),t(agg8[1:2,4:5]))

agg = as.data.frame(rbind(agg1,agg2,agg3,agg4,agg5,agg6,agg7,agg8))


names(agg)=c('low','high','low','high')
rownames(agg) = c('commuting distance','1','competition 1mi','2','competition 2mi','3',
                        'competition 5mi','4','income','5','education','6','% of people +65 age','7',
                        '% people internet home','8')

agg = round(agg,3)

bracket<-function(x){
  x<-paste0('(',x,')')
}

agg[c(2,4,6,8,10,12,14,16),]=apply(agg[c(2,4,6,8,10,12,14,16),],2,bracket)

agg_ulp = agg[,1:2]
write.xlsx(agg_ulp,'margins_by_factors_rnr.xlsx')





## N by exit/entry - TABLE 5

load(file='entry_data_nearest5_treat_traffic_rnr.Rda')
load(file='exit_data_nearest5_treat_traffic_rnr.Rda')


ids_entry = unique(db_entry2$id2[db_entry2$treat==1]) # get this from the CF_ensemble script (need to run cf_ensemble first)

dbe=db_entry2[db_entry2$treat==1,]
dbe = dbe[dbe$id2 %in% ids_entry,]

dbe <- dbe %>%  group_by(id2) %>% 
  mutate(comp1mi = min(comp1km))
dbe <- dbe %>%  group_by(id) %>% 
  mutate(comp2mi = min(comp2km))
dbe <- dbe %>%  group_by(id) %>% 
  mutate(comp5mi = min(comp5km))

dbe = dbe[!duplicated(dbe$id2),]

entry1=round(mean(dbe$comp1mi,na.rm=T),3)
entry1s=paste0("(",round(sd(dbe$comp1mi,na.rm=T),3),")")
entry2=round(mean(dbe$comp2mi,na.rm=T),3)
entry2s=paste0("(",round(sd(dbe$comp2mi,na.rm=T),3),")")
entry5=round(mean(dbe$comp5mi,na.rm=T),3)
entry5s=paste0("(",round(sd(dbe$comp5mi,na.rm=T),3),")")


#### exit

ids_exit = unique(db2$id2[db2$treat==1]) # get this from the CF_ensemble script (need to run cf_ensemble first)

db=db2[db2$treat==1,]
db = db[db$id2 %in% ids_exit,]

db <- db %>%  group_by(id) %>% 
  mutate(comp1mi = min(comp1km))
db <- db %>%  group_by(id) %>% 
  mutate(comp2mi = min(comp2km))
db <- db %>%  group_by(id) %>% 
  mutate(comp5mi = min(comp5km))

db = db[!duplicated(db$id2),]

exit1=round(mean(db$comp1mi,na.rm=T),3)
exit1s=paste0("(",round(sd(db$comp1mi,na.rm=T),3),")")
exit2=round(mean(db$comp2mi,na.rm=T),3)
exit2s=paste0("(",round(sd(db$comp2mi,na.rm=T),3),")")
exit5=round(mean(db$comp5mi,na.rm=T),3)
exit5s=paste0("(",round(sd(db$comp5mi,na.rm=T),3),")")

tab1 = as.data.frame(rbind(entry1,entry1s,exit2,exit2s,exit5,exit5s))
tab2 = as.data.frame(rbind(exit1,exit1s,entry2,entry2s,entry5,entry5s))
table = cbind(tab1,tab2)
names(table)=c('exit','entry')
write.xlsx(table,'exit_entry_number_firms_rnr.xlsx')






# exogenous estimates - this script compares means of most important variables for a sample of exits/entries around the end of tax year 
# with the total sample - TABLE 16.


load(file='entry_data_nearest5_treat_traffic_rnr.Rda')
load(file='exit_data_nearest5_treat_traffic_rnr.Rda')



db2$home_internet = db2$Internet.accessed.from.dwelling.num/db2$population

db_s = db2[db2$seq==0,]
db = db_s[(db_s$week<30)&(db_s$week>16),]

id_list = unique(db$id)
length(id_list)/6
db2$exog = ifelse(db2$id %in% id_list,1,0)


agg = aggregate(cbind(median_income,home_internet,Median.commuting.distance..kms.,comp1km,comp2km,comp5km) ~ exog, db2,function(x) c(mean = mean(x), sd = sd(x)))
agg = do.call("data.frame", agg) 

agg = round(agg,3)
agg = agg[,-1]
sds = agg[, seq_len(ncol(agg)) %% 2 == 0]
sds=matrix(paste0("(",format(unlist(sds)),")"),nrow=2)
sds=as.data.frame(apply(sds,2,function(x)gsub('\\s+', '',x)))
sds <- mutate(sds, id = rownames(sds))
names(sds) = c('income','internet','commuting','comp1mi','comp2mi','comp5mi','id')
mean = agg[, seq_len(ncol(agg)) %% 2 == 1]
mean <- mutate(mean, id = rownames(mean))
names(mean) = c('income','internet','commuting','comp1mi','comp2mi','comp5mi','id')
table = rbindlist(list(mean, sds))[order(id)]






db_entry2$home_internet = db_entry2$Internet.accessed.from.dwelling.num/db_entry2$population

db_s = db_entry2[db_entry2$seq==0,]
db = db_s[(db_s$week<40)&(db_s$week>26),]

id_list = unique(db$id)
length(id_list)/6
db2$exog = ifelse(db2$id %in% id_list,1,0)


agg = aggregate(cbind(median_income,home_internet,Median.commuting.distance..kms.,comp1km,comp2km,comp5km) ~ exog, db2,function(x) c(mean = mean(x), sd = sd(x)))
agg = do.call("data.frame", agg) 

agg = round(agg,3)
agg = agg[,-1]
sds = agg[, seq_len(ncol(agg)) %% 2 == 0]
sds=matrix(paste0("(",format(unlist(sds)),")"),nrow=2)
sds=as.data.frame(apply(sds,2,function(x)gsub('\\s+', '',x)))
sds <- mutate(sds, id = rownames(sds))
names(sds) = c('income','internet','commuting','comp1mi','comp2mi','comp5mi','id')
mean = agg[, seq_len(ncol(agg)) %% 2 == 1]
mean <- mutate(mean, id = rownames(mean))
names(mean) = c('income','internet','commuting','comp1mi','comp2mi','comp5mi','id')
table2 = rbindlist(list(mean, sds))[order(id)]

table = rbind(table,table2)
table
row.names(table) = c('rest of the sample','sd1','exogenous','sd2','rest of the sample2','sd3','exogenous2','sd4')

write.xlsx(table,'exog_compare_rnr.xlsx')










######### APPENDIX #################




# summary table - Table B.2

mean(weekly$margin_u_ds,na.rm=T)
names(weekly2)
vars <- c('ulp_ds','petrol_ws_ds','cushing_ds','margin_u_ds',
          "Usual.Resident.Population","People.aged.0_14.years", "People.aged.15_64.years",
          "People.aged.65.years.and.over", 'Median.age','Sex.ratio','comp1km',"comp2km" ,"comp5km",
          'median_income','mean_income','total_income','earners_age','number_of_earners',
          "Advanced.Diploma.and.Diploma.Level","Bachelor.Degree.Level","Postgraduate.Degree.Level",
          "Average.commuting.distance..kms.",'Median.commuting.distance..kms.',
          'Car.as.driver',"one.motor.vehicle",
          "Index.of.Relative.Socio.economic.Disadvantage","Index.of.Relative.Socio.economic.Advantage.and.Disadvantage",
          "Index.of.Economic.Resources","Index.of.Education.and.Occupation",'home_internet'
)

setdiff(vars,names(weekly))
summs = weekly[,vars]
f <- function(x){
  c(mean = mean(x,na.rm=T),sd=sd(x,na.rm=T),quantile(x, 0.1,na.rm=T),quantile(x, 0.25,na.rm=T),quantile(x, 0.5,na.rm=T),quantile(x, 0.75,na.rm=T), quantile(x, 0.9,na.rm=T) )
}

sum_tab=as.data.frame(t(sapply(weekly[,vars], f)))

write.xlsx2(sum_tab,'sum_tab.xlsx')






## margin by competition - Table B.3

load(file="weekly_mins_traffic_rnr.Rda")

weekly2 = weekly


xs=quantile(weekly2$comp1km,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly2 <- weekly2 %>% mutate(comp1km_cat=cut(comp1km, breaks=xs, labels=c("low","high"))) # "0-2","3 or more"

xs=quantile(weekly2$comp2km,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly2 <- weekly2 %>% mutate(comp2km_cat=cut(comp2km, breaks=xs, labels=c("low","high"))) # "0-5","6 or more"

xs=quantile(weekly2$comp5km,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly2 <- weekly2 %>% mutate(comp5km_cat=cut(comp5km, breaks=xs, labels=c("low","high"))) # "0-15","15 or more"


agg = aggregate(cbind(margin_u_ds,margin_d_ds) ~ comp1km_cat + comp2km_cat + comp5km_cat, data = weekly2, FUN = mean)
write.xlsx(agg,'margin_competition_rnr.xlsx')






## mean margin by search and competition - Table B.4

load(file="weekly_mins_traffic_rnr.Rda")

weekly$home_internet = weekly$Internet.accessed.from.dwelling.num/weekly$population

xs=quantile(weekly$median_income,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(income_cat=cut(median_income, breaks=xs, labels=c("low income","high income"))) # "0-15","15 or more"

xs=quantile(weekly$home_internet,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(internet_cat=cut(home_internet, breaks=xs, labels=c("low internet","high internet"))) # "0-15","15 or more"

xs=quantile(weekly$Median.commuting.distance..kms.,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(commute_cat=cut(Median.commuting.distance..kms., breaks=xs, labels=c("low commute","high commute"))) # "0-15","15 or more"

xs=quantile(weekly$comp5km,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(comp_cat=cut(comp5km, breaks=xs, labels=c("low competition","high competition"))) # "0-15","15 or more"

selected = weekly[,c('margin_u_ds','margin_d_ds','income_cat','comp_cat','commute_cat','internet_cat')]


agg = aggregate(margin_u_ds ~ comp_cat + income_cat + internet_cat + commute_cat , data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg = do.call("data.frame", agg) 
names(agg)[5:6]= c('mean','sd')
agg[,5:6] = round(agg[,5:6],3)
agg$sd <- paste0("(", agg$sd, ")")

library(data.table)   
setDT(agg)   
agg2 <- dcast(agg, comp_cat + income_cat  ~ internet_cat +commute_cat , 
              value.var = c("mean", "sd"))

tab1 = agg2[,c(1:6)]
names(tab1)[3:6] = c('low internet - low commute','low internet - high commute','high internet - low commute','high internet - high commute')
tab1 <- mutate(tab1, id = rownames(tab1))
tab2 = agg2[,c(1,2,7:10)]
names(tab2)[3:6] = c('low internet - low commute','low internet - high commute','high internet - low commute','high internet - high commute')
tab2 <- mutate(tab2, id = rownames(tab2))

table = rbindlist(list(tab1, tab2))[order(id)]
table = table[,-7]


write.xlsx(table,'income_search_comp_rnr.xlsx')





# number of exits and market exits - Table B.5

load(file='exit_data_nearest5_treat_traffic_rnr.Rda')

db3<-db2[db2$seq==0,]
db3 = db3[!duplicated(db3$id2),]


db3 = db3[!duplicated(c(db3$id2,db3$time)),]
brand_exitters=as.data.frame(table(db3$exitter))
names(brand_exitters)=c('brand','exitter')

# entry


load(file='entry_data_nearest5_treat_traffic_rnr.Rda')

db_entry3<-db_entry2[db_entry2$seq==0,]
db_entry3 = db_entry3[!duplicated(db_entry3$id2),]

db_entry3 = db_entry3[!duplicated(c(db_entry3$id2,db_entry3$time)),]
brand_entrants=as.data.frame(table(db_entry3$entrant))
names(brand_entrants)=c('brand','entrant')


brands<-merge(brand_exitters,brand_entrants,by="brand",all=T)

stations<-read.csv('only_stations3.csv')

brandlist2<-as.data.frame(table(stations$BRAND_DESCRIPTION))
names(brandlist2)<-c('brand','total_frequency')
brands<-merge(brands,brandlist2,by='brand',all=T)
brands


xtable(brands, type = "latex", file = "brands.tex",include.rownames=FALSE)
write.csv(brands,'brands_rnr.csv')




## brand margin by income_cat - Table B.6


xs=quantile(weekly$median_income,c(0,1/3,2/3,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(income_cat=cut(median_income, breaks=xs, labels=c("low","middle","high")))

agg<-aggregate(margin_u_ds ~ BRAND_DESCRIPTION + income_cat, data=weekly, mean,na.rm=T) # get margin by income and brand
agg1<-aggregate(panel ~ BRAND_DESCRIPTION + income_cat, data=weekly, function(x) length(unique(x))) # get number of stations by income/brand
agg2<-aggregate(comp1km ~ BRAND_DESCRIPTION + income_cat, data=weekly, mean,na.rm=T) # get competition (1mi) by income/brand
agg3<-aggregate(comp5km ~ BRAND_DESCRIPTION + income_cat, data=weekly, mean,na.rm=T) # get competition (5mi) by income/brand


agg=as.data.frame(with( agg, tapply(margin_u_ds, list(BRAND_DESCRIPTION,income_cat), mean)))
agg1=as.data.frame(with( agg1, tapply(panel, list(BRAND_DESCRIPTION,income_cat), mean)))
agg2=as.data.frame(with( agg2, tapply(comp1km, list(BRAND_DESCRIPTION,income_cat), mean)))
agg3=as.data.frame(with( agg3, tapply(comp5km, list(BRAND_DESCRIPTION,income_cat), mean)))


agg<-cbind(agg,agg1,agg2,agg3)
agg<-agg[c(1,4,7,10,2,5,8,11,3,6,9,12)]
agg1<-agg[c(1:4)]
agg2<-agg[c(5:8)]
agg3<-agg[c(9:12)]


agg1_l <- pivot_longer(agg1, cols=1:4, names_to = "names", values_to = "low")
agg2_l <- pivot_longer(agg2, cols=1:4, names_to = "names", values_to = "med")
agg3_l <- pivot_longer(agg3, cols=1:4, names_to = "names", values_to = "high")

agg_l=cbind(agg1_l,agg2_l,agg3_l)
agg_l$station<-rownames(agg_l)

agg_l=agg_l[,c(7,2,4,6)]

agg_l$station<-gsub("(.*)....*", "\\1", agg_l$station)
agg_l[2:4]=round(agg_l[2:4],3)

write.csv(agg_l,'brand_margin.csv')






## COMPARE Control V Treatment - Table B.7


load(file='exit_data_nearest5_treat_traffic_rnr.Rda')
names(db2)
db_compare = db2[db2$seq<=0,]
vars = c('treat','id','ulp_ds','margin_u_ds','median_income','mean_ind_income','average_no_cars',
         'Number.of.employed.people.living.in.region..no..',
         "A0_15" ,"A65PLUS",
         'Internet.accessed.from.dwelling','Car.as.driver',"one.motor.vehicle",
         "population","IEO",'comp1km',
         "comp2km" ,"comp5km",'brand_size')
db_compare = db_compare[vars]



agg <- as.data.frame(t(aggregate(db_compare, by=list(treat = db_compare$treat), mean,na.rm=T)))
names(agg) = c('control_mean','treatment_mean')
agg2 <- as.data.frame(t(aggregate(db_compare, by=list(treat = db_compare$treat), sd,na.rm=T)))
names(agg2) = c('control_sd','treatment_sd')

agg= cbind(agg,agg2)
agg=agg[,c(1,3,2,4)]
agg=agg[c(-1,-2),]

agg1 = agg[,c(1,2)]
agg1 = round(agg1,3)
agg1$control_sd = paste0("(",agg1$control_sd,")")
agg2 = agg[,c(3,4)]
agg2 = round(agg2,3)
agg2$treatment_sd = paste0("(",agg2$treatment_sd,")")



agg1 = as.data.frame(do.call(rbind, lapply(1:nrow(agg1), function(x) t(agg1[x,]))))
agg2 = as.data.frame(do.call(rbind, lapply(1:nrow(agg2), function(x) t(agg2[x,]))))

agg_exit = cbind(agg1,agg2)  

rownames(agg_exit)=c('id','idsd','margin_u_ds','0','median_income','2','median_age','3','number_of_earners','4',
                     "People.aged.0_14.years",'5a',"People.aged.15_64.years",'5b' ,"People.aged.65.years.and.over",'5c',
                     'Median.commuting.distance..kms.','6',"gini_coefficient",'7','Car..as.driver','8',"One.motor.vehicle",'9',
                     "Usual.Resident.Population",'10',"Index.of.Education.and.Occupation",'11','comp1km','12',
                     "comp2km",'13',"comp5km",'14','brand_size','15')
names(agg_exit)=c('control','treatment')


# entry

load(file='entry_data_nearest5_treat_traffic_rnr.Rda')
names(db2)
db_compare = db2[db2$seq<=0,]
vars = c('treat','id','ulp_ds','margin_u_ds','median_income','mean_ind_income','average_no_cars',
         'Number.of.employed.people.living.in.region..no..',
         "A0_15" ,"A65PLUS",
         'Internet.accessed.from.dwelling','Car.as.driver',"one.motor.vehicle",
         "population","IEO",'comp1km',
         "comp2km" ,"comp5km",'brand_size')
db_compare = db_compare[vars]

agg <- as.data.frame(t(aggregate(db_compare, by=list(treat = db_compare$treat), mean,na.rm=T)))
names(agg) = c('control_mean','treatment_mean')
agg2 <- as.data.frame(t(aggregate(db_compare, by=list(treat = db_compare$treat), sd,na.rm=T)))
names(agg2) = c('control_sd','treatment_sd')

agg= cbind(agg,agg2)
agg=agg[,c(1,3,2,4)]
agg=agg[c(-1,-2),]

agg1 = agg[,c(1,2)]
agg1 = round(agg1,3)
agg1$control_sd = paste0("(",agg1$control_sd,")")
agg2 = agg[,c(3,4)]
agg2 = round(agg2,3)
agg2$treatment_sd = paste0("(",agg2$treatment_sd,")")


agg1 = as.data.frame(do.call(rbind, lapply(1:nrow(agg1), function(x) t(agg1[x,]))))
agg2 = as.data.frame(do.call(rbind, lapply(1:nrow(agg2), function(x) t(agg2[x,]))))

agg_entry = cbind(agg1,agg2)  

rownames(agg_entry)=c('id','idsd','margin_u_ds','0','median_income','2','median_age','3','number_of_earners','4',
                      "People.aged.0_14.years",'5a',"People.aged.15_64.years",'5b' ,"People.aged.65.years.and.over",'5c',
                      'Median.commuting.distance..kms.','6',"gini_coefficient",'7','Car..as.driver','8',"One.motor.vehicle",'9',
                      "Usual.Resident.Population",'10',"Index.of.Education.and.Occupation",'11','comp1km','12',
                      "comp2km",'13',"comp5km",'14','brand_size','15')
names(agg_entry)=c('control','treatment')

agg_entry


agg_total = cbind(agg_exit,agg_entry)

write.xlsx(agg_total,'treatment_control_means_rnr.xlsx')






####### NOT USED IN PAPER


#### map - not used in final version of paper


load(file="weekly_wide_data_geo.Rda")

load(file='exit_data_only_treat.Rda')
load(file='entry_data_onlytreat.Rda')
only_stations<-read.csv('only_stations3.csv')

# first get the list of panel numbers that had exit/entry
exit_pan=unique(db$panel)
entry_pan=unique(db_entry$panel)

exit_pan=append(exit_pan,entry_pan)
exit_pan=unique(exit_pan)

all_pan=unique(weekly$panel)


only_stations$change<-ifelse(only_stations$panel2 %in% exit_pan,0,1)


register_google(key = "AIzaSyB8y5la_pBtXvFVbrJ3XHcxAlbDz25ab14")  
map <- get_map(location = "Western Australia", zoom = 5)


wa_map = ggmap(map) +
  geom_point(data = only_stations, aes(x=lon, y = lat, colour = factor(change),shape=factor(change),size=factor(change)))  + 
  scale_colour_manual(values = c("blue", "red"),labels = c("No change", "At least one change") ) +
  scale_shape_manual(labels = c("No change", "At least one change"),values=c(16,17)) +
  scale_size_manual(labels = c("No change", "At least one change"),values=c(2.5,1.5)) +
  theme(legend.position="bottom",legend.title = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text=element_text(size=14)) 
wa_map
ggsave('aus_map.pdf')



library(shades)

panel_margin<-aggregate(weekly$margin_u_ds, by=list(weekly$panel), FUN=mean, na.rm=T)
colnames(panel_margin)<-c('panel2','panel_margin')
only_stations<-left_join(only_stations,panel_margin)
only_stations$panel_margin2<-only_stations$panel_margin**2

only_stations<-only_stations[!is.na(only_stations$panel_margin),]

fair_cols <- c("#38170B","#BF1B0B", "#FFC465", "#66ADE5", "#252A52")
names(fair_cols) <- letters[1:5]
fair_ramp <- scales::colour_ramp(fair_cols)

margin_wa = ggmap(map) +
  geom_point(data = only_stations, aes(x=lon, y = lat, colour = panel_margin), size=3)  + 
  scale_colour_gradientn(colours = fair_cols) + 
  theme(legend.position="right",axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text=element_text(size=14)) +
  labs(color='Retail margin') 

margin_wa

ggsave('margin_WA.pdf')




map_closeup <- get_map(location = c(lon = 116.012252, lat = -32.089163), zoom = 13)
map1<-ggmap(map_closeup) +   geom_point(data = only_stations, aes(x=lon, y = lat, colour = panel_margin), size=4)  + 
  scale_colour_gradientn(colours = fair_cols)  + ggtitle('Gosnells, Perth') + theme(plot.title = element_text(hjust=0.5)) +
  theme(legend.position="right",axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text=element_text(size=14)) +
  labs(color='Retail margin') 

map_closeup2 <- get_map(location = c(lon = 114.131983, lat = -21.942643), zoom = 14)
map2<-ggmap(map_closeup2) +   geom_point(data = only_stations, aes(x=lon, y = lat, colour = panel_margin), size=4)  + 
  scale_colour_gradientn(colours = fair_cols) + ggtitle('Exmouth') + theme(plot.title = element_text(hjust=0.5)) +
  theme(legend.position="right",axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text=element_text(size=14)) +
  labs(color='Retail margin')
example_wa = ggarrange(map1, map2,nrow=1, ncol=2,common.legend = T, legend='right')
example_wa

ggsave('margin_small_maps.pdf')






# margin by income/population/competition

load(file="weekly_mins_traffic_rnr.Rda")

xs=quantile(weekly$median_income,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(income_cat=cut(median_income, breaks=xs, labels=c("low income","high income"))) # "0-15","15 or more"

xs=quantile(weekly$population,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(population_cat=cut(population, breaks=xs, labels=c("low population","high population"))) # "0-15","15 or more"

xs=quantile(weekly$comp1km,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(comp_cat=cut(comp1km, breaks=xs, labels=c("low competition","high competition"))) # "0-15","15 or more"


selected = weekly[,c('margin_u_ds','income_cat','comp_cat','population_cat')]


agg = aggregate(margin_u_ds ~ comp_cat * income_cat + population_cat , data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg = do.call("data.frame", agg) 
names(agg)[4:5]= c('mean','sd')
agg[,4:5] = round(agg[,4:5],3)
agg$sd <- paste0("(", agg$sd, ")")

library(data.table)   
setDT(agg)   
agg2 <- dcast(agg,  comp_cat ~ income_cat  + population_cat  , value.var = c("mean", "sd"))

tab1 = agg2[,c(1:5)]
names(tab1)[2:5] = c('low income - low population','low income - high population','high income - low population','high income - high population')
tab1 <- mutate(tab1, id = rownames(tab1))
tab2 = agg2[,c(1,6:9)]
names(tab2)[2:5] = c('low income - low population','low income - high population','high income - low population','high income - high population')
tab2 <- mutate(tab2, id = rownames(tab2))

table = rbindlist(list(tab1, tab2))[order(id)]

write.xlsx(table,'income_comp_population_rnr.xlsx')




# income and per capita competition

load(file="weekly_mins_traffic_rnr.Rda")

weekly$home_internet = weekly$Internet.accessed.from.dwelling.num/weekly$population

weekly2 = weekly %>% filter(!is.na(median_income))

xs=quantile(weekly2$median_income,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly2 <- weekly2 %>% mutate(income_cat=cut(median_income, breaks=xs, labels=c("low income","high income"))) # "0-15","15 or more"


weekly2$comp_norm<-(weekly2$comp1km+1)/(weekly2$population/10000)
summary(weekly2$comp_norm)
quantile(weekly2$comp_norm, c(.97, .98, .99), na.rm=T)
weekly2 = weekly2[weekly2$comp_norm<quantile(weekly2$comp_norm, .98, na.rm=T),]


weekly2$comp_norm5<-(weekly2$comp5km+1)/(weekly2$population/10000)
summary(weekly2$comp_norm5)
quantile(weekly2$comp_norm5, c(.97, .98, .99), na.rm=T)
weekly2 = weekly2[weekly2$comp_norm5<quantile(weekly2$comp_norm5, .98, na.rm=T),]



selected = weekly2[,c('margin_u_ds','comp1km','comp5km','comp_norm','comp_norm5','income_cat','home_internet','Median.commuting.distance..kms.')]

selected$comp1km= selected$comp1km+1
selected$comp5km= selected$comp5km+1

agg = aggregate(margin_u_ds ~ income_cat, data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg = do.call("data.frame", agg) 
names(agg)[2:3]= c('mean','sd')
agg[,2:3] = round(agg[,2:3],3)
agg$sd <- paste0("(", agg$sd, ")")

agg <- as.data.frame(do.call(rbind, lapply(1:nrow(agg), function(x) t(agg[x,]))))


agg2 = aggregate(comp_norm ~ income_cat, data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg2 = do.call("data.frame", agg2) 
names(agg2)[2:3]= c('mean','sd')
agg2[,2:3] = round(agg2[,2:3],3)
agg2$sd <- paste0("(", agg2$sd, ")")

agg2 <- as.data.frame(do.call(rbind, lapply(1:nrow(agg2), function(x) t(agg2[x,]))))

agg2a = aggregate(comp_norm5 ~ income_cat, data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg2a = do.call("data.frame", agg2a) 
names(agg2a)[2:3]= c('mean','sd')
agg2a[,2:3] = round(agg2a[,2:3],3)
agg2a$sd <- paste0("(", agg2a$sd, ")")

agg2a <- as.data.frame(do.call(rbind, lapply(1:nrow(agg2a), function(x) t(agg2a[x,]))))

agg3 = aggregate(comp1km ~ income_cat, data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg3 = do.call("data.frame", agg3) 
names(agg3)[2:3]= c('mean','sd')
agg3[,2:3] = round(agg3[,2:3],3)
agg3$sd <- paste0("(", agg3$sd, ")")

agg3 <- as.data.frame(do.call(rbind, lapply(1:nrow(agg3), function(x) t(agg3[x,]))))

agg4 = aggregate(comp5km ~ income_cat, data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg4 = do.call("data.frame", agg4) 
names(agg4)[2:3]= c('mean','sd')
agg4[,2:3] = round(agg4[,2:3],3)
agg4$sd <- paste0("(", agg4$sd, ")")

agg4 <- as.data.frame(do.call(rbind, lapply(1:nrow(agg4), function(x) t(agg4[x,]))))


agg5 = aggregate(home_internet ~ income_cat, data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg5 = do.call("data.frame", agg5) 
names(agg5)[2:3]= c('mean','sd')
agg5[,2:3] = round(agg5[,2:3],3)
agg5$sd <- paste0("(", agg5$sd, ")")

agg5 <- as.data.frame(do.call(rbind, lapply(1:nrow(agg5), function(x) t(agg5[x,]))))

agg6 = aggregate(Median.commuting.distance..kms. ~ income_cat, data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg6 = do.call("data.frame", agg6) 
names(agg6)[2:3]= c('mean','sd')
agg6[,2:3] = round(agg6[,2:3],3)
agg6$sd <- paste0("(", agg6$sd, ")")

agg6 <- as.data.frame(do.call(rbind, lapply(1:nrow(agg6), function(x) t(agg6[x,]))))


aggs = cbind(agg,agg2,agg2a,agg3,agg4,agg5,agg6)

names(aggs) = c('margin','N by 10000 people','N (5mi) by 10000 people','N within 1mi','N within 5mi','internet','commute')

write.xlsx(aggs,'main_factors_by_income.xlsx')




# SEARCH ONLY

## search and margins


load(file="weekly_mins_traffic_rnr.Rda")


weekly$home_internet = weekly$Internet.accessed.from.dwelling.num/weekly$population

xs=quantile(weekly$median_income,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(income_cat=cut(median_income, breaks=xs, labels=c("low income","high income"))) # "0-15","15 or more"

xs=quantile(weekly$home_internet,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(internet_cat=cut(home_internet, breaks=xs, labels=c("low internet","high internet"))) # "0-15","15 or more"

xs=quantile(weekly$Median.commuting.distance..kms.,c(0,1/2,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
weekly <- weekly %>% mutate(commute_cat=cut(Median.commuting.distance..kms., breaks=xs, labels=c("low commute","high commute"))) # "0-15","15 or more"


selected = weekly[,c('margin_u_ds','margin_d_ds','income_cat','commute_cat','internet_cat')]


agg = aggregate(margin_u_ds ~   internet_cat + commute_cat , data = selected, function(x) c(mean = mean(x), sd = sd(x)))
agg = do.call("data.frame", agg) 
names(agg)[3:4]= c('mean','sd')
agg[,3:4] = round(agg[,3:4],3)
agg$sd <- paste0("(", agg$sd, ")")

library(data.table)   
setDT(agg)   
agg2 <- dcast(agg,  internet_cat  ~  commute_cat , 
              value.var = c("mean", "sd"))

tab1 = agg2[,c(1:3)]
names(tab1)[2:3] = c('low commute','high commute')
tab1 <- mutate(tab1, id = rownames(tab1))
tab2 = agg2[,c(1,4:5)]
names(tab2)[2:3] = c('low commute','high commute')
tab2 <- mutate(tab2, id = rownames(tab2))

table = rbindlist(list(tab1, tab2))[order(id)]
table = table[,-4]

write.xlsx(table,'income_search_rnr.xlsx')


