
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

#library(plm)

ischange<-function(x){
  c(FALSE, diff(x) != 0)
}

issmaller<-function(x){
  c(FALSE, diff(x) < 0)
}

islarger<-function(x){
  c(FALSE, diff(x) > 0)
}


setwd("[]add_your_path_of_data]") 



load(file="weekly_mins_traffic_rnr.Rda")

# check if exit/entry within 1km

inx<-aggregate(weekly$comp1km, by=list(weekly$panel), FUN=issmaller)
inx<-as.data.frame(unlist(inx$x))
names(inx)<-'exit1km'
sum(inx$exit1km)

weekly<-cbind(weekly,inx)
weekly$exit1km<-as.numeric(weekly$exit1km)

inx<-aggregate(weekly$comp1km, by=list(weekly$panel), FUN=islarger)
inx<-as.data.frame(unlist(inx$x))
names(inx)<-'entry1km'
sum(inx$entry1km)

weekly<-cbind(weekly,inx)
weekly$entry1km<-as.numeric(weekly$entry1km)


# check if exit/entry within 2km

inx<-aggregate(weekly$comp2km, by=list(weekly$panel), FUN=issmaller)
inx<-as.data.frame(unlist(inx$x))
names(inx)<-'exit2km'
sum(inx$exit2km)

weekly<-cbind(weekly,inx)
weekly$exit2km<-as.numeric(weekly$exit2km)

inx<-aggregate(weekly$comp2km, by=list(weekly$panel), FUN=islarger)
inx<-as.data.frame(unlist(inx$x))
names(inx)<-'entry2km'
sum(inx$entry2km)

weekly<-cbind(weekly,inx)
weekly$entry2km<-as.numeric(weekly$entry2km)


# check if exit/entry within 5km

inx<-aggregate(weekly$comp5km, by=list(weekly$panel), FUN=issmaller)
inx<-as.data.frame(unlist(inx$x))
names(inx)<-'exit5km'
sum(inx$exit5km)

weekly<-cbind(weekly,inx)
weekly$exit5km<-as.numeric(weekly$exit5km)

inx<-aggregate(weekly$comp5km, by=list(weekly$panel), FUN=islarger)
inx<-as.data.frame(unlist(inx$x))
names(inx)<-'entry5km'
sum(inx$entry5km)

weekly<-cbind(weekly,inx)
weekly$entry5km<-as.numeric(weekly$entry5km)


panel<-unique(weekly$panel)

 
db<-data.frame()
control1<-data.frame()
count=0


for (p in panel) {
  print(p)
  sub<-weekly[weekly$panel==p,]
  date_exit<-sub$time[sub$exit1km==1]
  date_entry<-sub$time[sub$entry1km==1]
  count=count+1
  if (length(date_exit)==0){next}
  for (i in 1:length(date_exit)){
    count2=count2+1
    lower<-date_exit[i]-175 #112
    upper<-date_exit[i]+175 #140
    db1 <- sub[(sub$time>=lower & sub$time<=upper),]
    if (sum(db1$exit1km)!=1){next}
    if (sum(db1$entry1km)!=0){next}
    if (nrow(db1)<51){next}
    db1$exit1km<-ifelse(db1$time>=date_exit[i],1,0)
    db1$panel2<-as.numeric(count)
    competitors<-db1$comp1km[db1$time==date_exit[i]]
    db1$competitors<-as.numeric(competitors)
    db1$treat=1
    if (length(setdiff(db1$competitors1km[1][[1]],tail(db1$competitors1km,1)[[1]]))!=1){next}
    db1$brand_exit<-setdiff(db1$competitors1km[1][[1]],tail(db1$competitors1km,1)[[1]])
    db1$brand_exit<-as.numeric(db1$brand_exit)
    for (t in 1:length(db1$time)){
      db1$seq[t]<-as.numeric(difftime(db1$time[t],date_exit[i],units="weeks"))
    }
    avoid<-db1$within10km_list[[1]]
    brand_avoid<-db1$BRAND_DESCRIPTION[[1]]
    competition<-db1$competitors[[1]]
    control1<-weekly[(weekly$time>=lower) & (weekly$time<=upper),] 
    control_agg<-aggregate(list(exit=control1$exit1km,entry=control1$entry1km), by=list(control1$panel),mean,na.rm=TRUE)
    control_agg<-control_agg[(control_agg$exit==0)&(control_agg$entry==0),]
    control_list<-unique(control_agg$Group.1)
    control1<-control1[control1$panel %in% control_list,]
    counts <- data.frame(table(control1$panel))
    nonmissing<-counts[counts$Freq>=51,]
    control1<-control1[control1$panel %in% nonmissing$Var1, ]
    control1<-control1[!control1$panel %in% avoid, ]
    control1<-control1[control1$BRAND_DESCRIPTION!=brand_avoid,]
    #control1<-control1[control1$comp1km==competition,]
    if (nrow(control1)==0){next}
    control1$panel2<-as.numeric(count)
    control1$competitors<-control1$comp1km
    control1$treat<-0
    for (t in 1:length(control1$time)){
      control1$seq[t]<-as.numeric(difftime(control1$time[t],date_exit[i],units="weeks"))
    }
    control1$brand_exit<-mean(db1$brand_exit)
    db1<-rbind(db1,control1)
    
    #x = db1[db1$seq<0,]
    x<-db1[c('treat','panel','mean_ind_income','comp1km','population','brand_size','sb2mi')]
    x2<-aggregate(x,by=list(x$panel),mean,na.rm=T)
    x2 = x2[!is.na(x2$mean_ind_income),]
    #x2 = x2[!is.na(x2$ulp),]
    m_out=try(matchit(treat ~ mean_ind_income + comp1km + population + brand_size + sb2mi , 
                      method = "nearest",ratio = 5,data = x2, replace=FALSE,na.action=na.exclude),TRUE)
    if(isTRUE(class(m_out)=="try-error")) { next } else { 
      m_dat <- match.data(m_out)
      panel_match = m_dat$panel
      db_temp = db1[db1$panel %in% panel_match,]
      db_temp$id2 = paste0(count2,count) 
      db=rbind(db,db_temp)
      count=count+1
    }
    #db<-db[!duplicated(db[c('time','panel2','treat')]),]
  }}



names(weekly)
nrow(db)/51

db2 <- db %>% arrange(id2,panel,treat,seq)
id<-rep(1:(nrow(db2)/51),each=51)
db2$id<-id

# adding brand information for the exitter

brandlist<-weekly[,c('panel','BRAND_DESCRIPTION')]
brandlist<-brandlist[!duplicated(brandlist$panel),]
names(brandlist)<-c('brand_exit','exitter')

db2<-left_join(db2,brandlist,by="brand_exit")


save(db2,file='exit_data_nearest5_treat_traffic_rnr.Rda')













## ENTRY


db1<-data.frame()
db_entry<-data.frame()

#db$panel2<-0
count=2000
count2=1

for (p in panel) {
  print(p)
  sub<-weekly[weekly$panel==p,]
  date_exit<-(sub$time[sub$exit1km==1]-7)
  date_entry<-(sub$time[sub$entry1km==1]-7)
  if (length(date_entry)==0){next}
  for (i in 1:length(date_entry)){
    count2=count2+1
    lower<-date_entry[i]-175
    upper<-date_entry[i]+175
    db1 <- sub[(sub$time>=lower & sub$time<=upper),]
    if (sum(db1$exit1km)!=0){next}
    if (sum(db1$entry1km)!=1){next}
    if (nrow(db1)<51){next}
    db1$entry1km<-ifelse(db1$time>=date_entry[i],1,0)
    db1$panel2<-as.numeric(count)
    competitors<-db1$comp1km[db1$time==date_entry[i]]
    db1$competitors<-as.numeric(competitors)
    db1$treat=1
    if (length(setdiff(tail(db1$competitors1km,1)[[1]],db1$competitors1km[1][[1]]))!=1){next}
    db1$brand_entry<-setdiff(tail(db1$competitors1km,1)[[1]],db1$competitors1km[1][[1]])
    db1$brand_entry<-as.numeric(db1$brand_entry)
    for (t in 1:length(db1$time)){
      db1$seq[t]<-as.numeric(difftime(db1$time[t],date_entry[i],units="weeks"))
    }
    avoid<-db1$within5km_list[[1]]
    brand_avoid<-db1$BRAND_DESCRIPTION[[1]]
    competition<-db1$competitors[[1]]
    control1<-weekly[(weekly$time>=lower) & (weekly$time<=upper),] 
    control_agg<-aggregate(list(exit=control1$exit1km,entry=control1$entry1km), by=list(control1$panel),mean,na.rm=TRUE)
    control_agg<-control_agg[(control_agg$exit==0)&(control_agg$entry==0),]
    control_list<-unique(control_agg$Group.1)
    control1<-control1[control1$panel %in% control_list,]
    counts <- data.frame(table(control1$panel))
    nonmissing<-counts[counts$Freq>=51,]
    control1<-control1[control1$panel %in% nonmissing$Var1, ]
    control1<-control1[!control1$panel %in% avoid, ]
    control1<-control1[control1$BRAND_DESCRIPTION!=brand_avoid,]
    #control1<-control1[control1$comp1km==competitors,]
    if (nrow(control1)==0){next}
    control1$panel2<-as.numeric(count)
    control1$competitors<-competitors
    control1$treat<-0
    control1$brand_entry<-mean(db1$brand_entry)
    
    for (t in 1:length(control1$time)){
      control1$seq[t]<-as.numeric(difftime(control1$time[t],date_entry[i],units="weeks"))
    }
    db1<-rbind(db1,control1)
    
    x<-db1[c('treat','panel','mean_ind_income','comp1km','population','brand_size','sb2mi')]
    x2<-aggregate(x,by=list(x$panel),mean,na.rm=T)
    x2 = x2[!is.na(x2$mean_ind_income),]
    m_out=try(matchit(treat ~ mean_ind_income + comp1km + population + brand_size + sb2mi , method = "nearest",ratio = 5,data = x2, replace=FALSE,na.action=na.exclude),TRUE)
    if(isTRUE(class(m_out)=="try-error")) { next } else { 
      m_dat <- match.data(m_out)
      panel_match = m_dat$panel
      db_temp = db1[db1$panel %in% panel_match,]
      db_temp$id2 = paste0(count2,count) 
      db_entry=rbind(db_entry,db_temp)
      count=count+1
    }}}



nrow(db_entry)/51

db_entry2 <- db_entry %>% arrange(id2,panel,treat,seq)
id<-rep(1:(nrow(db_entry2)/51),each=51)
db_entry2$id<-id


brandlist<-weekly[,c('panel','BRAND_DESCRIPTION')]
brandlist<-brandlist[!duplicated(brandlist$panel),]
names(brandlist)<-c('brand_entry','entrant')

db_entry2<-left_join(db_entry2,brandlist,by="brand_entry")


save(db_entry2,file='entry_data_nearest5_treat_traffic_rnr.Rda')


