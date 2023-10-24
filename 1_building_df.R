library(zoo)
library(tidyverse)
library(interplot)
library(lubridate)
library(fastDummies)
library(dummies)
library(varhandle)
library(dplyr)
library(ggplot2)
library(jtools)
library(sjPlot)
library(DiagrammeR)
library(DiagrammeRsvg)
library(Hmisc)
library(classInt)
library(broom)
library(ggpubr)
library(gtools)
library(ggmap)
library(readr)
library(tidyr)
library(reshape2)
library(geosphere)


# READING DATA

setwd("[add_your_path_of_data]") 

coding<-read.csv("Coding Index.csv")
income<-read.csv("income_full3.csv")
distribution<-read.csv("income_distribution.csv")

soc_econ_index<-read.csv("soc_econ_index.csv")
commuting_dist<-read.csv("commuting_dist.csv")

vehicles<-read.csv("cars_full.csv",sep=",", header=TRUE)
income2 = read.csv('income_full.csv')
seifa = read.csv('seifa_full.csv')
rent = read.csv('rent_full.csv')
age_pop = read.csv('age_population_full.csv')
internet = read.csv('internet_full.csv')
education = read.csv('education_full.csv')
commuting_means = read.csv('commute_mean_full.csv')




df_temp<- read.csv(unz("petrol_data_wide_geo.zip", "petrol_data_wide_geo.csv"), header = TRUE,sep = ",")

names(df_temp)[names(df_temp)=='lat']<-'long'
names(df_temp)[names(df_temp)=='lon']<-'lat'

stations<-read.csv('only_stations3.csv')



# GET NUMBER OF COMPETITORS

stations<-stations[order(stations$panel),]

mat<-as.data.frame(distm(stations[,c('lon','lat')],fun = distHaversine ))
panels<-stations$panel
colnames(mat)=panels
diag(mat)<-NA

# these are named as km throught all codes but this is miles

within1km=apply(mat, 1, function(x) as.character(colnames(mat)[which(x<1600)]))

for (i in 1:nrow(stations)){
  stations$within1km_list[i]<-within1km[i]
}
stations$within1km_list[stations$within1km_list=="character(0)"]<-0


within2km=apply(mat, 1, function(x) as.character(colnames(mat)[which(x<3200)]))

for (i in 1:nrow(stations)){
  stations$within2km_list[i]<-within2km[i]
}
stations$within2km_list[stations$within2km_list=="character(0)"]<-0


within5km=apply(mat, 1, function(x) as.character(colnames(mat)[which(x<8000)]))

for (i in 1:nrow(stations)){
  stations$within5km_list[i]<-within5km[i]
}
stations$within5km_list[stations$within5km_list=="character(0)"]<-0


within10km=apply(mat, 1, function(x) as.character(colnames(mat)[which(x<16000)]))

for (i in 1:nrow(stations)){
  stations$within10km_list[i]<-within10km[i]
}
stations$within10km_list[stations$within10km_list=="character(0)"]<-0




# now we multiply stations by 19 to create one for each year

stations = do.call("rbind", replicate(19, stations, simplify = FALSE))
year<-rep(2001:2019,each=1299)
stations$year = year
stations$year2 = ifelse(stations$year<2009,2006,ifelse(((stations$year>=2009) &(stations$year<2014)),2011,2016))
stations$year3 = ifelse(stations$year<2011,2011,ifelse(((stations$year>2016) ),2016,stations$year))

coding$SA2_NAME=as.character(coding$SA2_NAME)
names(vehicles)[1]='year'


# joining by SA2

names(distribution)[1]='SA2'
names(soc_econ_index)[1]='SA2'
names(commuting_dist)[1]='SA2'

income$SA2=as.character(income$SA2)

coding$SA2=as.character(coding$SA2)
distribution$SA2=as.character(distribution$SA2)
soc_econ_index$SA2=as.character(soc_econ_index$SA2)
commuting_dist$SA2=as.character(commuting_dist$SA2)
seifa$SA2=as.character(seifa$SA2)


demog <- list(distribution,commuting_dist,soc_econ_index) %>% reduce(left_join, by = "SA2")

income<-income[,-1]
demog[demog=="np"]<-NA
demog[3:23] <- sapply(demog[3:23],function(x) as.numeric(gsub(",", "",x)))
income[income=="np"]<-NA
income[3:8] <- sapply(income[3:8],function(x) as.numeric(gsub(",", "",x)))

coding<-coding[!duplicated(coding$POSTCODE),]
stations<-left_join(stations,coding,by=c('POSTCODE'))
stations<-left_join(stations,demog,by=c('SA2'))


stations<-left_join(stations,income,by=c('SA2','year'))
names(income2)[3]='POSTCODE'
income2 = income2[-1]
stations<-left_join(stations,income2,by=c('POSTCODE','year'))
names(internet)[3]='year2'
internet = internet[-1]
stations<-left_join(stations,internet,by=c('SA2','year2'))
age_pop = age_pop[-1]
stations<-left_join(stations,age_pop,by=c('SA2','year'))
names(education)[3]='year2'
education = education[-1]
stations<-left_join(stations,education,by=c('SA2','year2'))
names(seifa)[3]='year2'
seifa = seifa[-1]
stations<-left_join(stations,seifa,by=c('SA2','year2'))
names(rent)[3]='year2'
rent = rent[-1]
stations<-left_join(stations,rent,by=c('SA2','year2'))
names(commuting_means)[1]='year2'
names(commuting_means)[2]='POSTCODE'
stations<-left_join(stations,commuting_means,by=c('POSTCODE','year2'))
names(vehicles)[1]='year2'
names(vehicles)[2]='POSTCODE'
stations<-left_join(stations,vehicles,by=c('POSTCODE','year2'))

#names(stations)

stations$IEO[is.na(stations$IEO)] = stations$Index.of.Education.and.Occupation[is.na(stations$IEO)]
stations$IER[is.na(stations$IER)] = stations$Index.of.Economic.Resources[is.na(stations$IER)]
stations$IER[is.na(stations$IRSD)] = stations$Index.of.Relative.Socio.economic.Disadvantage[is.na(stations$IRSD)]
stations$IER[is.na(stations$IRSAD)] = stations$Index.of.Relative.Socio.economic.Advantage.and.Disadvantage[is.na(stations$IRSAD)]

stations[26:ncol(stations)] <- sapply(stations[26:ncol(stations)],function(x) as.numeric(gsub(",", "",x)))





# merging station data with price data

panels=unique(stations$panel)
df_temp2=df_temp[df_temp$panel %in% panels,]
df_temp2$year = as.integer(substring(df_temp2$PUBLISH_DATE,7,10))
stations2<-stations[!duplicated(stations[c(1,17)]),] # keep unique panel and year
df<-left_join(df_temp2,stations2[,c(1,10:18,25:162)],by=c('panel','year'))




crude<-read.csv('crude_oil.csv')
colnames(crude)[1]<-'date'
crude$date<-as.Date(crude$date, format = "%d/%m/%Y")
df$PUBLISH_DATE<-as.Date(df$PUBLISH_DATE, format = "%d/%m/%Y")
colnames(df)[4]<-'date'
df<-left_join(df,crude,by='date')

wholesale<-read.csv('wholesale.csv')
colnames(wholesale)<-c('date','petrol_ws','diesel_ws')
wholesale$date<-as.Date(wholesale$date, format = "%d/%m/%Y")

df<-left_join(df,wholesale,by='date')

save(df,file='daily_wide_data_geo.Rda')





############### GENERATE WEEKLY DATA



df$year=format(as.Date(df$date), "%Y")
df$month=format(as.Date(df$date), "%Y-%m")


library(lubridate) # for the wday() and ymd() functions
df$date2 <- ymd(df$date)
saturdays <- df[wday(df$date2) == 2, ]
startDate <- min(saturdays$date2) # select first Saturday
df$week <- floor(as.numeric(difftime(df$date2, startDate, units = "weeks")))
df$week<-df$week+2

weeks<-as.data.frame(seq(as.Date("2001-01-01"),as.Date("2019-12-31"), by = "week"))
weeks$week<-seq(1:992)
names(weeks)[1]<-"weekly"
df<-left_join(df,weeks,by='week')


weekly<-aggregate(list(ulp=df$ULP, diesel=df$Diesel, lpg=df$LPG,pulp=df$PULP),by=list(time=df$weekly,panel2=df$panel2),mean,na.rm=TRUE)
weekly$year = year(weekly$time)
stations2<-stations[!duplicated(stations[c(12,17)]),] # keep unique panel and year

weekly<-left_join(weekly,stations2,by=c('panel2','year'))
weekly$panel<-weekly$panel2
weekly<-weekly[,-2]

weekly2<- weekly %>% distinct(time, panel, .keep_all = T)

sample = weekly2[0:1000,]



# adding wholesale weekly averages

wholesale<-read.csv('wholesale.csv')
colnames(wholesale)<-c('date','petrol_ws','diesel_ws')
wholesale$date<-as.Date(wholesale$date, format = "%d/%m/%Y")

wholesale$date2 <- ymd(wholesale$date)
saturdays <- wholesale[wday(wholesale$date2) == 2,]
startDate <- min(saturdays$date2) # select first Saturday
wholesale$week <- floor(as.numeric(difftime(wholesale$date2, startDate, units = "weeks")))
wholesale$week<-wholesale$week+1

weeks<-as.data.frame(seq(as.Date("2004-01-05"),as.Date("2020-07-03"), by = "week"))
weeks$week<-seq(1:861)
names(weeks)[1]<-"weekly"
wholesale<-left_join(wholesale,weeks,by='week')

wholesale_agg<-aggregate(list(petrol_ws=wholesale$petrol_ws, diesel_ws=wholesale$diesel_ws),by=list(time=wholesale$weekly),mean,na.rm=TRUE)

weekly<-left_join(weekly,wholesale_agg, by="time")

# adding weekly crude

crude<-read.csv('crude_oil.csv')
colnames(crude)[1]<-'date'
crude$date<-as.Date(crude$date, format = "%d/%m/%Y")
crude = crude[crude$date >=as.Date("2001-01-01"),]
crude$date2 <- ymd(crude$date)
saturdays <- crude[wday(crude$date2) == 2, ]
startDate <- min(saturdays$date2) # select first Saturday
crude$week <- floor(as.numeric(difftime(crude$date2, startDate, units = "weeks")))
crude$week<-crude$week+2

weeks<-as.data.frame(seq(as.Date("2001-01-01"),as.Date("2020-07-03"), by = "week"))
weeks$week<-seq(1:1018)
names(weeks)[1]<-"weekly"
crude<-left_join(crude,weeks,by='week')

crude_agg<-aggregate(list(cushing=crude$cushing, brent=crude$brent),by=list(time=crude$weekly),mean,na.rm=TRUE)

weekly<-left_join(weekly,crude_agg, by="time")



#### counting competition in every week


dates<-as.character(unique(sort(weekly$time)))

weekly$brands<-as.numeric(weekly$BRAND_DESCRIPTION)
brand_list<- weekly[,c("BRAND_DESCRIPTION","brands")]
brand_list<-brand_list[!duplicated(brand_list$BRAND_DESCRIPTION),]


templist=list()

for (d in 1:length(dates)){
  print(d)
  subset<-weekly[weekly$time==dates[d],]
  temp <- data.frame(matrix( nrow=nrow(subset)))
  for (i in 1:nrow(subset)){
    temp$comps[i]<-as.numeric(length(intersect(as.character(unique(subset$panel)),unlist(subset$within1km_list[i]))))
    temp$comps2[i]<-list(intersect(as.character(unique(subset$panel)),unlist(subset$within1km_list[i])))
    temp$panel[i]=subset$panel[i]
    temp$date[i]=dates[d]
  }
  temp<-temp[,-1]
  templist[[d]]=temp
}

trying<-do.call(rbind, lapply(templist, as.data.frame))

names(trying)<-c('comp1km','competitors1km','panel','time')
trying$time<-as.Date(trying$time, format='%Y-%m-%d')
trying<-trying[with(trying, order(panel, time)),]
weekly<-weekly[with(weekly, order(panel, time)),]

weekly<-cbind(weekly,trying[1:2])


# 2km

templist=list()

for (d in 1:length(dates)){
  print(d)
  subset<-weekly[weekly$time==dates[d],]
  temp <- data.frame(matrix( nrow=nrow(subset)))
  for (i in 1:nrow(subset)){
    temp$comps[i]<-as.numeric(length(intersect(as.character(unique(subset$panel)),unlist(subset$within2km_list[i]))))
    temp$comps2[i]<-list(intersect(as.character(unique(subset$panel)),unlist(subset$within2km_list[i])))
    temp$panel[i]=subset$panel[i]
    temp$date[i]=dates[d]
  }
  temp<-temp[,-1]
  templist[[d]]=temp
}

trying<-do.call(rbind, lapply(templist, as.data.frame))

names(trying)<-c('comp2km','competitors2km','panel','time')
trying$time<-as.Date(trying$time, format='%Y-%m-%d')
trying<-trying[with(trying, order(panel, time)),]
weekly<-weekly[with(weekly, order(panel, time)),]

weekly<-cbind(weekly,trying[1:2])


# 5km

templist=list()

for (d in 1:length(dates)){
  print(d)
  subset<-weekly[weekly$time==dates[d],]
  temp <- data.frame(matrix( nrow=nrow(subset)))
  for (i in 1:nrow(subset)){
    temp$comps[i]<-as.numeric(length(intersect(as.character(unique(subset$panel)),unlist(subset$within5km_list[i]))))
    temp$comps2[i]<-list(intersect(as.character(unique(subset$panel)),unlist(subset$within5km_list[i])))
    temp$panel[i]=subset$panel[i]
    temp$date[i]=dates[d]
  }
  temp<-temp[,-1]
  templist[[d]]=temp
}

trying<-do.call(rbind, lapply(templist, as.data.frame))


names(trying)<-c('comp5km','competitors5km','panel','time')
trying$time<-as.Date(trying$time, format='%Y-%m-%d')
trying<-trying[with(trying, order(panel, time)),]
weekly<-weekly[with(weekly, order(panel, time)),]

weekly<-cbind(weekly,trying[1:2])



weekly$Median.commuting.distance..kms. = weekly$Median.commuting.distance..kms./1.6 # the commuting distance was in kms - convert to miles
weekly$Average.commuting.distance..kms. = weekly$Average.commuting.distance..kms./1.6


save(weekly,file='weekly_wide_data_geo.Rda')


# DETRENDING


weekly$year<-as.numeric(format(as.Date(weekly$time), "%Y"))
weekly$quarter<-as.numeric(format(as.yearqtr(weekly$time), "%q"))
weekly$week<-as.numeric(week(weekly$time))

sample=weekly[1:1000,]

dummies<-to.dummy(weekly$year,'year')
detrend<-cbind(weekly$ulp,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('ulp')

xnam <- paste("year.", 2001:2018, sep="")
fmla <- as.formula(paste("ulp ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend)

coeffs<-summary(model)$coefficients[2:19,1]
coeffs[19]<-0

for (i in 1:19)
  weekly$ulp_ds[weekly$year==(i+2000)]<-weekly$ulp[weekly$year==(i+2000)]-as.numeric(coeffs[i])



dummies<-to.dummy(weekly$week,'week')
detrend<-cbind(weekly$ulp_ds,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('ulp_ds')

xnam <- paste("week.", 2:53, sep="")
fmla <- as.formula(paste("ulp_ds ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend,na.action=na.exclude)

coeffs<-summary(model)$coefficients[1:53,1]
coeffs[1]<-0
for (i in 1:53)
  weekly$ulp_ds[weekly$week==(i)]<-weekly$ulp_ds[weekly$week==(i)]-as.numeric(coeffs[i])



# diesel detrend - YEAR
dummies<-to.dummy(weekly$year,'year')
detrend<-cbind(weekly$diesel,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('diesel')

xnam <- paste("year.", 2001:2018, sep="")
fmla <- as.formula(paste("diesel ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend)

coeffs<-summary(model)$coefficients[2:19,1]
coeffs[19]<-0

for (i in 1:19)
  weekly$diesel_ds[weekly$year==(i+2000)]<-weekly$diesel[weekly$year==(i+2000)]-as.numeric(coeffs[i])



# diesel detrend - WEEK
dummies<-to.dummy(weekly$week,'week')
detrend<-cbind(weekly$diesel_ds,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('diesel_ds')

xnam <- paste("week.", 2:53, sep="")
fmla <- as.formula(paste("diesel_ds ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend,na.action=na.exclude)

coeffs<-summary(model)$coefficients[1:53,1]
coeffs[1]<-0
for (i in 1:53)
  weekly$diesel_ds[weekly$week==i]<-weekly$diesel_ds[weekly$week==i]-as.numeric(coeffs[i])




# petrol_ws detrend - YEAR
dummies<-to.dummy(weekly$year,'year')
detrend<-cbind(weekly$petrol_ws,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('petrol_ws')

xnam <- paste("year.", 2004:2018, sep="")
fmla <- as.formula(paste("petrol_ws ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend)

coeffs<-summary(model)$coefficients[2:16,1]
coeffs[16]<-0

for (i in 1:16)
  weekly$petrol_ws_ds[weekly$year==(i+2003)]<-weekly$petrol_ws[weekly$year==(i+2003)]-as.numeric(coeffs[i])



# petrol_ws detrend - WEEK
dummies<-to.dummy(weekly$week,'week')
detrend<-cbind(weekly$petrol_ws_ds,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('petrol_ws_ds')

xnam <- paste("week.", 2:53, sep="")
fmla <- as.formula(paste("petrol_ws_ds ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend,na.action=na.exclude)

coeffs<-summary(model)$coefficients[1:53,1]
coeffs[1]<-0
for (i in 1:53)
  weekly$petrol_ws_ds[weekly$week==(i)]<-weekly$petrol_ws_ds[weekly$week==(i)]-as.numeric(coeffs[i])



# diesel_ws detrend - YEAR
dummies<-to.dummy(weekly$year,'year')
detrend<-cbind(weekly$diesel_ws,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('diesel_ws')

xnam <- paste("year.", 2004:2018, sep="")
fmla <- as.formula(paste("diesel_ws ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend)

coeffs<-summary(model)$coefficients[2:16,1]
coeffs[16]<-0

for (i in 1:16)
  weekly$diesel_ws_ds[weekly$year==(i+2003)]<-weekly$diesel_ws[weekly$year==(i+2003)]-as.numeric(coeffs[i])



# diesel_ws detrend - WEEK
dummies<-to.dummy(weekly$week,'week')
detrend<-cbind(weekly$diesel_ws_ds,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('diesel_ws_ds')

xnam <- paste("week.", 2:53, sep="")
fmla <- as.formula(paste("diesel_ws_ds ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend,na.action=na.exclude)

coeffs<-summary(model)$coefficients[1:53,1]
coeffs[1]<-0
for (i in 1:53)
  weekly$diesel_ws_ds[weekly$week==(i)]<-weekly$diesel_ws_ds[weekly$week==(i)]-as.numeric(coeffs[i])




# cushing detrend - YEAR
dummies<-to.dummy(weekly$year,'year')
detrend<-cbind(weekly$cushing,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('cushing')

xnam <- paste("year.", 2001:2018, sep="")
fmla <- as.formula(paste("cushing ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend,na.action=na.exclude)

coeffs<-summary(model)$coefficients[2:19,1]
coeffs[19]<-0

for (i in 1:19)
  weekly$cushing_ds[weekly$year==(i+2000)]<-weekly$cushing[weekly$year==(i+2000)]-as.numeric(coeffs[i])


# cushing detrend - WEEK
dummies<-to.dummy(weekly$week,'week')
detrend<-cbind(weekly$cushing_ds,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('cushing')

xnam <- paste("week.", 2:53, sep="")
fmla <- as.formula(paste("cushing ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend,na.action=na.exclude)

coeffs<-summary(model)$coefficients[1:53,1]
coeffs[1]<-0
for (i in 1:53)
  weekly$cushing_ds[weekly$week==i]<-weekly$cushing_ds[weekly$week==i]-as.numeric(coeffs[i])




# brent detrend - YEAR
dummies<-to.dummy(weekly$year,'year')
detrend<-cbind(weekly$brent,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('brent')

xnam <- paste("year.", 2001:2018, sep="")
fmla <- as.formula(paste("brent ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend,na.action=na.exclude)

coeffs<-summary(model)$coefficients[2:19,1]
coeffs[19]<-0

for (i in 1:19)
  weekly$brent_ds[weekly$year==(i+2000)]<-weekly$brent[weekly$year==(i+2000)]-as.numeric(coeffs[i])


# brent detrend - WEEK
dummies<-to.dummy(weekly$week,'week')
detrend<-cbind(weekly$brent_ds,dummies)
detrend<-as.data.frame(detrend)
colnames(detrend)[1]<-c('brent')

xnam <- paste("week.", 2:53, sep="")
fmla <- as.formula(paste("brent ~ ", paste(xnam, collapse= "+")))

model<-lm(fmla, data=detrend,na.action=na.exclude)

coeffs<-summary(model)$coefficients[1:53,1]
coeffs[1]<-0
for (i in 1:53)
  weekly$brent_ds[weekly$week==i]<-weekly$brent_ds[weekly$week==i]-as.numeric(coeffs[i])





weekly$margin_u<-weekly$ulp/((weekly$petrol_ws))
weekly$margin_d<-weekly$diesel/((weekly$diesel_ws))

weekly$margin_u_ds<-weekly$ulp_ds/((weekly$petrol_ws_ds))
weekly$margin_d_ds<-weekly$diesel_ds/((weekly$diesel_ws_ds))

weekly$margin_ws_u<-weekly$petrol_ws_ds/((weekly$cushing/159)*144)
weekly$margin_ws_d<-weekly$diesel_ws_ds/((weekly$cushing/159)*144)

weekly$margin_ws_u_brent<-weekly$petrol_ws_ds/((weekly$brent/159)*144)
weekly$margin_ws_d_brent<-weekly$diesel_ws_ds/((weekly$brent/159)*144)




# adding brand size 

only_stations<-read.csv('only_stations3.csv')
count<- only_stations %>% group_by(BRAND_DESCRIPTION) %>% mutate(count = n())
count<-count[,c(4,13)]
count<-count[!duplicated(count$BRAND_DESCRIPTION),]
names(count)<-c('BRAND_DESCRIPTION','brand_size')
count$brand_size[count$BRAND_DESCRIPTION == 'Independent'] = 1
weekly = left_join(weekly,count,by='BRAND_DESCRIPTION')

# adding mean income


save(weekly,file='weekly_wide_data_geo.Rda')


sample = sample_n(weekly,1000)


# plot weekly 

agg<-aggregate(diesel_ds ~ week, data=weekly, function(x) c(mn = mean(x)))
ggplot() + geom_line(mapping = aes(x = agg$week, y = agg$diesel_ds), size = 2, color = "blue") 


# plot yearly

agg<-aggregate(diesel_ds ~ year, data=weekly, function(x) c(mn = mean(x)))
ggplot() + geom_line(mapping = aes(x = agg$year, y = agg$diesel_ds), size = 2, color = "blue") 

# plot time series


agg_w<-aggregate(weekly$cushing_ds, by=list(weekly$time), FUN=mean, na.rm=TRUE)
colnames(agg_w)<-c('time','ulp')
ggplot(agg_w,aes(x=time,y=ulp)) + geom_line(size=1) +  theme_minimal() +
  scale_x_date(breaks=number_ticks(15))

mean(weekly$margin_u, na.rm=T)

