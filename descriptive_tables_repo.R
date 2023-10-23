
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



weekly<-weekly[!is.na(weekly$petrol_ws),]

weekly$home_internet = weekly$Internet.accessed.from.dwelling.num/weekly$population


# summary table

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




## plot over time (whole length)




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


# time series seasoned v deseasoned



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





## competition and margins

load(file='exit_data_nearest5_treat_traffic_rnr.Rda')

agg<-aggregate(weekly$margin_u_ds, by=list((weekly$comp1km+1)), FUN=mean, na.rm=TRUE)
colnames(agg)<-c("competitors",'margin')
agg = as.data.frame(t(round(agg,3)))

write.xlsx(agg,'competition_margin_rnr.xlsx')




##  brand analysis - summary stats of entry and exit

only_stations<-read.csv('only_stations3.csv')
count<- only_stations %>% group_by(BRAND_DESCRIPTION) %>% dplyr::mutate(count = n())
count<-count[,c(4,13)]
count<-count[!duplicated(count$BRAND_DESCRIPTION),]
names(count)<-c('brand','frequency')
agg_margin<-aggregate(list(weekly$ulp_ds,weekly$margin_u_ds), by=list(weekly$BRAND_DESCRIPTION), FUN=mean,na.rm=T)
names(agg_margin)=c('brand','petrol','petrol_margin')
agg_margin<-left_join(count,agg_margin)
write.csv(agg_margin,'margins_brand_rnr.csv')





# number of exits and market exits

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



