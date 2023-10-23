#install.packages('dotwhisker')
#install.packages('ggstatsplot')
# install.packages('TMB')
#install.packages('ivreg')
#install.packages('sjPlot')
#install.packages("multiwayvcov")

library(DiagrammeRsvg)
library(ggeffects)
library(ivreg)
library(ggstatsplot)
library(ggplot2)
library(dotwhisker)
library(broom)
library(dplyr)
library(grf)
library(tidyverse)
library(interplot)
library(lubridate)
library(fastDummies)
library(dummies)
library(varhandle)
library(MatchIt)
library(dplyr)
library(ggplot2)
library(simsalapar)
library(jtools)
# this file gets the min prices and sd of prices within 1km and within 2km, then outputs weekly_mins

library(sjPlot)
library(dyn)
library(DiagrammeR)
library(DiagrammeRsvg)
library(Hmisc)
library(classInt)
library(broom)
library(personalized)
library(ggpubr)
library(gtools)
library(ggmap)
library(readr)
library(compareDF)
library(ggmap)
library(tidyr)
library(tidygeocoder)
library(reshape2)
library(geosphere)
library(stargazer)
library(MatchIt)
library(MatchIt)
library(stargazer)
library(data.table)
library(lmtest)
library(plyr)
library(dummies)
library(interplot)
library(pbapply)

ischange<-function(x){
  c(FALSE, diff(x) != 0)
}

issmaller<-function(x){
  c(FALSE, diff(x) < 0)
}

islarger<-function(x){
  c(FALSE, diff(x) > 0)
}


fnrollmean <- function (x) {
  rollmean(x,3,align="center",na.pad=TRUE)
}




setwd("c:/Users/User/OneDrive - University of East Anglia/UEA/RESEARCH/Distributional effects/Australia/Data/") 



load(file="weekly_wide_data_geo.Rda")





sample=weekly[1:100,]

get_minimums = function(x){
  l1 = c(x$panel, x$competitors1km)
  t1 = x$time
  s1 = weekly[(weekly$panel %in% l1)&(weekly$time==t1),]
  min_ulp1 = min(s1$ulp_ds,na.rm=T)
  min_ulp_marg1 = min(s1$margin_u_ds,na.rm=T)
  min_die1 = min(s1$diesel_ds,na.rm=T)
  min_die_marg1 = min(s1$margin_d_ds,na.rm=T)
  sd_ulp1 = sd(s1$ulp_ds,na.rm=T)
  sd_ulp_marg1 = sd(s1$margin_u_ds,na.rm=T)
  sd_die1 = sd(s1$diesel_ds,na.rm=T)
  sd_die_marg1 = sd(s1$margin_d_ds,na.rm=T)
  
  lowest_ulp1 = ifelse(s1[s1$panel==x$panel,]$ulp_ds==min_ulp1,1,0)
  highest_ulp1 = ifelse(s1[s1$panel==x$panel,]$ulp_ds==max(s1$ulp_ds,na.rm=T),1,0)
  
  l2 = c(x$competitors2km, x$panel)
  t2 = x$time
  s2 = weekly[(weekly$panel %in% l2)&(weekly$time==t2),]
  min_ulp2 = min(s2$ulp_ds,na.rm=T)
  min_ulp_marg2 = min(s2$margin_u_ds,na.rm=T)
  min_die2 = min(s2$diesel_ds,na.rm=T)
  min_die_marg2 = min(s2$margin_d_ds,na.rm=T)
  
  sd_ulp2 = sd(s2$ulp_ds,na.rm=T)
  sd_ulp_marg2 = sd(s2$margin_u_ds,na.rm=T)
  sd_die2 = sd(s2$diesel_ds,na.rm=T)
  sd_die_marg2 = sd(s2$margin_d_ds,na.rm=T)
  
  lowest_ulp2 = ifelse(s2[s2$panel==x$panel,]$ulp_ds==min_ulp2,1,0)
  highest_ulp2 = ifelse(s2[s2$panel==x$panel,]$ulp_ds==max(s2$ulp_ds,na.rm=T),1,0)
  
  list1 = list(min_ulp1=min_ulp1,min_ulp_marg1=min_ulp_marg1,min_die1=min_die1,min_die_marg1=min_die_marg1,
               sd_ulp1=sd_ulp1,sd_ulp_marg1=sd_ulp_marg1,sd_die1=sd_die1,sd_die_marg1=sd_die_marg1,lowest_ulp1=lowest_ulp1,highest_ulp1=highest_ulp1,
               min_ulp2=min_ulp2,min_ulp_marg2=min_ulp_marg2,min_die2=min_die2,min_die_marg2=min_die_marg2,
               sd_ulp2=sd_ulp2,sd_ulp_marg2=sd_ulp_marg2,sd_die2=sd_die2,sd_die_marg2=sd_die_marg2,lowest_ulp2=lowest_ulp2,highest_ulp2=highest_ulp2) 
  return(list1)
}

weekly1 = weekly[weekly$year>=2004,]
mins = pbapply(weekly[1:300000,],1,get_minimums)

mins1 = as.data.frame(do.call(rbind, lapply(mins, as.vector)))
mins1 = cbind(my.var=rownames(mins1), mins1)
  
save(mins1,file="mins1.Rda")

mins = pbapply(weekly[300001:nrow(weekly),],1,get_minimums)

mins2 = as.data.frame(do.call(rbind, lapply(mins, as.vector)))
mins2 = cbind(my.var=rownames(mins2), mins2)

save(mins2,file="mins2.Rda")

mins_full = rbind(mins1,mins2)
weekly2 = cbind(weekly,mins_full)

save(weekly2,file='weekly_mins.Rda')
# add mins to weekly (missing code)





# adding same brand info

load(file="weekly_mins.Rda")

weekly = weekly2

brandlist<-weekly[,c('panel','BRAND_DESCRIPTION')]
brandlist<-brandlist[!duplicated(brandlist$panel),]
names(brandlist)<-c('brand_exit','exitter')

db2<-left_join(db2,brandlist,by="brand_exit")



### the following counts the number of same brand stations within 1,2,5 miles.

same_brand = data.frame()
for (i in 1:nrow(weekly2)){
  print(i)
  brand = weekly2$BRAND_DESCRIPTION[i]

  brands1 = brandlist$exitter[brandlist$brand_exit %in% sub$competitors1km[[1]]]
  brands2 = brandlist$exitter[brandlist$brand_exit %in% sub$competitors2km[[1]]]
  brands5 = brandlist$exitter[brandlist$brand_exit %in% sub$competitors5km[[1]]]
  
  sb1mi = sum(str_count(brands1,as.character(brand)))
  sb2mi = sum(str_count(brands2,as.character(brand)))
  sb5mi = sum(str_count(brands5,as.character(brand)))
  temp = cbind(i,sb1mi,sb2mi,sb5mi)
  same_brand = rbind(same_brand,temp)
}

same_brand = same_brand[,-1]

weekly = cbind(weekly2,same_brand)

save(weekly,file='weekly_mins_sb.Rda')



# adding traffic data and filtering to remove highways and busiest areas

weekly$Median.commuting.distance..kms. = weekly$Median.commuting.distance..kms./1.6
weekly$Average.commuting.distance..kms. = weekly$Average.commuting.distance..kms./1.6

traffic = read.csv('c:/Users/peter/OneDrive - University of East Anglia/UEA/RESEARCH/Distributional effects/Australia/Data/traffic/ave_sum_traffic_within_radius_21092023.csv')

traffic$highway = ifelse(grepl('Hwy',traffic$full_address),1,0) # create highway dummy

relevant = c("panel2","ave_traffic_mon_sun_1_miles", "ave_traffic_mon_fri_1_miles",    
             "ave_traffic_sat_sun_1_miles", "ave_traffic_heavy_mon_sun_1_miles", "ave_traffic_heavy_mon_fri_1_miles",
             "ave_traffic_heavy_sat_sun_1_miles", "sum_traffic_mon_sun_1_miles", "sum_traffic_mon_fri_1_miles",
             "sum_traffic_sat_sun_1_miles", "sum_traffic_heavy_mon_sun_1_miles", "sum_traffic_heavy_mon_fri_1_miles",
             "sum_traffic_heavy_sat_sun_1_miles", "ave_traffic_mon_sun_2_miles", "ave_traffic_mon_fri_2_miles",
             "ave_traffic_sat_sun_2_miles", "ave_traffic_heavy_mon_sun_2_miles", "ave_traffic_heavy_mon_fri_2_miles",
             "ave_traffic_heavy_sat_sun_2_miles", "sum_traffic_mon_sun_2_miles", "sum_traffic_mon_fri_2_miles",
             "sum_traffic_sat_sun_2_miles", "sum_traffic_heavy_mon_sun_2_miles", "sum_traffic_heavy_mon_fri_2_miles",
             "sum_traffic_heavy_sat_sun_2_miles", "ave_traffic_mon_sun_5_miles", "ave_traffic_mon_fri_5_miles",
             "ave_traffic_sat_sun_5_miles", "ave_traffic_heavy_mon_sun_5_miles", "ave_traffic_heavy_mon_fri_5_miles",
             "ave_traffic_heavy_sat_sun_5_miles", "sum_traffic_mon_sun_5_miles", "sum_traffic_mon_fri_5_miles",
             "sum_traffic_sat_sun_5_miles", "sum_traffic_heavy_mon_sun_5_miles", "sum_traffic_heavy_mon_fri_5_miles",
             "sum_traffic_heavy_sat_sun_5_miles",'highway'   )


names(traffic)[1] = 'panel'

traffic = traffic[traffic$highway==0,] # remove highways

# defining low traffic as all stations with adjacent traffic less than the 75th percentile of traffic

low_traffic = traffic[traffic$sum_traffic_mon_fri_2_mile<quantile(traffic$sum_traffic_mon_fri_2_mile,0.75,na.rm=T),]
low_traffic_id = unique(low_traffic$panel)
traffic$low_traffic = ifelse(traffic$panel %in% low_traffic_id,1,0)


traffic = traffic[traffic$low_traffic==1,]

ids = unique(traffic$panel)

weekly = weekly[weekly$panel %in% ids,]


save(weekly,file="weekly_mins_traffic_rnr.Rda")


