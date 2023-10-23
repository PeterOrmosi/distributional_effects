
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


load(file='entry_data_nearest5_treat_traffic_rnr.Rda')
load(file='exit_data_nearest5_treat_traffic_rnr.Rda')

## N by exit/entry

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

