# doing estimations for simple 2sls models

library(ggstatsplot)
library(ggplot2)
library(broom)
library(dplyr)
library(tidyverse)
library(lubridate)
library(fastDummies)
library(dummies)
library(dplyr)
library(jtools)
library(classInt)
library(ggpubr)
library(gtools)
library(tidyr)
library(reshape2)
library(data.table)
library(plm)
library(xlsx)


setwd("[add_your_path_of_data]") 

load(file='exit_data_nearest5_treat_traffic_rnr.Rda')


# preparing for regressions

top_brand<-c('Coles Express','Caltex','Caltex Woolworths','7-Eleven') ## adding a dummy for top brand exitter
db2$top_brand<-ifelse((db2$exitter %in% top_brand),1,0)

db_s<-db2

db_s <- db_s %>%  ungroup()

db_s<-db_s[abs(db_s$seq)<16,]


db_s$pm<-ifelse(db_s$seq>0,1,0)
db_s$pmtreat <- db_s$pm * db_s$treat


# setting up for panel data

# selecting main variables of interest 

db_panel=db_s[,c('id','id2','seq','pm','treat','pmtreat','panel','year','quarter','week','BRAND_DESCRIPTION'
                 ,'margin_u_ds','margin_d_ds',
                  'ulp_ds', 'margin_u','ulp','median_income',
                      'comp1km','comp2km','comp5km',
                      'Index.of.Economic.Resources','IER','Index.of.Relative.Socio.economic.Disadvantage','IRSAD',
                      'Index.of.Education.and.Occupation','IEO',
                      'age_earners','earners','A65PLUS',
                      'Car.as.driver','one.motor.vehicle', 
                      'Median.commuting.distance..kms.',
                      'population','sb2mi')]#,"ave_traffic_mon_sun_1_mile")] #(db_panel$id %in% high_income_id),]



db_panel[,12:ncol(db_panel)]=as.data.frame(scale(db_panel[,12:ncol(db_panel)]))

db_panel=pdata.frame(db_panel, index=c("id","seq"), drop.index=FALSE, row.names=TRUE)



###### plms by income and competition

lhs = 'margin_u_ds ~'
rhs = c('pm','treat','pmtreat','comp2km')
  
formula <- as.formula(paste(lhs, paste(rhs, collapse="+")))

plm1 <- plm(formula, index=c('id','seq')
            ,data = db_panel[(db_panel$median_income<quantile(db_panel$median_income,0.33,na.rm=T))&
                               (db_panel$comp2km<median(db_panel$comp2km,na.rm=T)),],model = "within", effect='individual')
treat1 = round(summary(plm1)$coefficients[2],3)
se1 = paste0("(" , round(summary(plm1)$coefficients[2,2],3), ")")


plm2 <- plm(formula, index=c('id','seq')
            ,data = db_panel[(db_panel$median_income>quantile(db_panel$median_income,0.66,na.rm=T))&
                               (db_panel$comp2km<median(db_panel$comp2km,na.rm=T)),],model = "within", effect='individual')
treat2 = round(summary(plm2)$coefficients[2],3)
se2 = paste0("(" , round(summary(plm2)$coefficients[2,2],3), ")")

plm3 <- plm(formula, index=c('id','seq')
            ,data = db_panel[(db_panel$median_income<quantile(db_panel$median_income,0.33,na.rm=T))&
                               (db_panel$comp2km>median(db_panel$comp2km,na.rm=T)),],model = "within", effect='individual')
treat3 = round(summary(plm3)$coefficients[2],3)
se3 = paste0("(" , round(summary(plm3)$coefficients[2,2],3), ")")

plm4 <- plm(formula, index=c('id','seq')
            ,data = db_panel[(db_panel$median_income>quantile(db_panel$median_income,0.66,na.rm=T))&
                               (db_panel$comp2km>median(db_panel$comp2km,na.rm=T)),],model = "within", effect='individual')
treat4 = round(summary(plm4)$coefficients[2],3)
se4 = paste0("(" , round(summary(plm4)$coefficients[2,2],3), ")")

col1 = rbind(treat1,se1,treat3,se3)
col2 = rbind(treat2,se2,treat4,se4)

table = cbind(col1,col2)

rownames(table) = c("low competition",'low competition (se)','high competition','high competition (se)')
colnames(table) = c('low income','high income')
table


write.xlsx(table,file="2wls_income_comp_rnr.xlsx")



# Rregression - ENTRY

load(file='entry_data_nearest5_treat_traffic_rnr.Rda')

# preparing for regressions

db_s<-db_entry2

db_s <- db_s %>%  ungroup()

db_s<-db_s[abs(db_s$seq)<16,]


db_s$pm<-ifelse(db_s$seq>0,1,0)
db_s$pmtreat <- db_s$pm * db_s$treat


# setting up for panel data

db_panel=db_s[,c('id','id2','seq','pm','treat','pmtreat','panel','year','quarter','week','BRAND_DESCRIPTION'
                 ,'margin_u_ds','margin_d_ds',
                 'ulp_ds', 'margin_u','ulp','median_income',
                 'comp1km','comp2km','comp5km',
                 'Index.of.Economic.Resources','IER','Index.of.Relative.Socio.economic.Disadvantage','IRSAD',
                 'Index.of.Education.and.Occupation','IEO',
                 'age_earners','earners','A65PLUS',
                 'Car.as.driver','one.motor.vehicle', 
                 'Median.commuting.distance..kms.',
                 'population','sb2mi')]#,"ave_traffic_mon_sun_1_mile")] #(db_panel$id %in% high_income_id),]



db_panel[,12:ncol(db_panel)]=as.data.frame(scale(db_panel[,12:ncol(db_panel)]))

db_panel=pdata.frame(db_panel, index=c("id","seq"), drop.index=FALSE, row.names=TRUE)


###### plms by income and competition

lhs = 'margin_u_ds ~'
rhs = c('pm','treat','pmtreat','comp1km')

formula <- as.formula(paste(lhs, paste(rhs, collapse="+")))

plm1 <- plm(formula, index=c('id','seq')
            ,data = db_panel[(db_panel$median_income<quantile(db_panel$median_income,0.33,na.rm=T))&
                               (db_panel$comp2km<median(db_panel$comp1km,na.rm=T)),],model = "within", effect='individual')
treat1 = round(summary(plm1)$coefficients[2],3)
se1 = paste0("(" , round(summary(plm1)$coefficients[2,2],3), ")")


plm2 <- plm(formula, index=c('id','seq')
            ,data = db_panel[(db_panel$median_income>quantile(db_panel$median_income,0.66,na.rm=T))&
                               (db_panel$comp2km<median(db_panel$comp1km,na.rm=T)),],model = "within", effect='individual')
treat2 = round(summary(plm2)$coefficients[2],3)
se2 = paste0("(" , round(summary(plm2)$coefficients[2,2],3), ")")

plm3 <- plm(formula, index=c('id','seq')
            ,data = db_panel[(db_panel$median_income<quantile(db_panel$median_income,0.33,na.rm=T))&
                               (db_panel$comp2km>median(db_panel$comp1km,na.rm=T)),],model = "within", effect='individual')
treat3 = round(summary(plm3)$coefficients[2],3)
se3 = paste0("(" , round(summary(plm3)$coefficients[2,2],3), ")")

plm4 <- plm(formula, index=c('id','seq')
            ,data = db_panel[(db_panel$median_income>quantile(db_panel$median_income,0.66,na.rm=T))&
                               (db_panel$comp2km>median(db_panel$comp1km,na.rm=T)),],model = "within", effect='individual')
treat4 = round(summary(plm4)$coefficients[2],3)
se4 = paste0("(" , round(summary(plm4)$coefficients[2,2],3), ")")

col1 = rbind(treat1,se1,treat3,se3)
col2 = rbind(treat2,se2,treat4,se4)

table = cbind(col1,col2)

rownames(table) = c("low competition",'low competition (se)','high competition','high competition (se)')
colnames(table) = c('low income','high income')
table


write.xlsx(table,file="2wls_entry_income_comp_rnr.xlsx")


