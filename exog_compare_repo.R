
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


# exogenous estimates - this script compares means of most important variables for a sample of exits/entries around the end of tax year 
# with the total sample


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
