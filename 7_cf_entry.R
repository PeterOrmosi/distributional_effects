
library(matrixStats) 
library(ggstatsplot)
library(ggplot2)
library(ggpubr)
library(gtools)
library(dplyr)
library(grf)
library(tidyverse)
library(lubridate)
library(dplyr)
library(jtools)
library(sjPlot)
library(DiagrammeR)
library(DiagrammeRsvg)
library(broom)
library(tidyr)
library(reshape2)
library(lmtest)
library(interplot)
library(doParallel)
library(xlsx)

setwd("[add_your_path_of_data]") 


load(file='entry_data_nearest5_treat_traffic_rnr.Rda')

top_brand<-c('Coles Express','Caltex','Caltex Woolworths','7-Eleven') ## adding a dummy for top brand exitter
db_entry2$top_brand<-ifelse((db_entry2$entrant %in% top_brand),1,0)


gc()
db_entry2$home_internet = db_entry2$Internet.accessed.from.dwelling.num/db_entry2$population

to_drop = c('Number.of.employed.people.living.in.region..no..', 'Usual.Resident.Population', 'SA2.NAME', 'individuals', 'income', 'mean_ind_income', 'total.business.income.n', 'total.business.income', 'total.business.expense.n', 'total.business.expense', 'net.business.income.n', 'net.business.income', 'estimated.business.net.tax.n', 'estimated.business.net.tax', 'gross.rent.n', 'gross.rent', 'net.rent.n', 'net.rent', 'Not.stated.x', 'Internet.accessed.from.dwelling.num', 'Internet.not.accessed.from.dwelling.num', 'Not.stated.num', 'Not.stated.y', 'Not.applicable', 'Total')
db_temp = db_entry2[ , !names(db_entry2) %in% to_drop] # dropping not used vars

db_s<-db_temp[db_temp$year<2018,]
db_s<-db_s[(abs(db_s$seq)<16),] # limiting to =/- 15 weeks
db_s$lnincome=log(db_s$median_income) 
db_s$lnmarginu=log(db_s$margin_u_ds)

db_s$pm<-ifelse(db_s$seq>=0,1,0)

names(db_s)

vars1 = c("treat", "id", "pm", "margin_u_ds")  
vars2 = c('year', 'gini_coefficient', 'top_1p', 'top_5p', 'top_10p', 'lowest_quart', 'second_quart', 'third_quart', 'highes_quart', 'Average.commuting.distance..kms.', 'Interquartile.range..kms.', 'Standard.deviation..kms.', 'earners', 'age_earners', 'sum_income', 'mean_income', 'mean_total_business_income', 'mean_total_business_expense', 'mean_net_business_income', 'mean_estimated_business_tax', 'mean_gross_rent', 'mean_net_rent', 'Internet.accessed.from.dwelling', 'Internet.not.accessed.from.dwelling', 'A8599', 'A04', 'A10', 'A15', 'A20', 'A25', 'A30', 'A35', 'A40', 'A45', 'A50', 'A55', 'A59', 'A60', 'A65', 'A70', 'A75', 'A80', 'A65PLUS', 'A35_65', 'A15_35', 'A0_15', 'population', 'Advanced.Diploma.and.Diploma.Level', 'Bachelor.Degree.Level', 'Certificate.I...II.Level', 'Certificate.III...IV.Level', 'Certificate.Level', 'Certificate.Level..nfd', 'Graduate.Diploma.and.Graduate.Certificate.Level', 'Postgraduate.Degree.Level', 'IEO', 'IER', 'IRSAD', 'IRSD', 'average_rent', 'X1_74', 'X100_124', 'X125_149', 'X150_174', 'X175_199', 'X200_224', 'X225_249', 'X250_274', 'X275_299', 'X300_324', 'X325_349', 'X350_374', 'X375_399', 'X400_424', 'X425_449', 'X450_549', 'X550_649', 'X650_749', 'X75_99', 'X750_849', 'X850_949', 'X950.and.over', 'Nil.payments', 'Train', 'Bus', 'Ferry', 'Tram', 'Taxi', 'Car.as.driver', 'Car.as.passenger', 'Truck', 'Motorbike.scooter', 'Bicycle', 'Walked.only', 'Worked.at.home', 'Did.not.go.to.work', 'None', 'one.motor.vehicle', 'two.motor.vehicles', 'three.motor.vehicles', 'four.or.more', 'average_no_cars',  'quarter', 'week', 'brand_size', 'competitors', 'top_brand', 'sb1mi', 'sb2mi', 'sb5mi','is_min_2km','is_max_2km','top_brand','lnincome', 'Index.of.Relative.Socio.economic.Disadvantage', 'Index.of.Relative.Socio.economic.Advantage.and.Disadvantage', 'Index.of.Economic.Resources', 'Index.of.Education.and.Occupation')

db_s = db_s[(db_s$median_income > quantile(db_s$median_income, 0.05, na.rm=T))&(db_s$median_income < quantile(db_s$median_income, 0.95, na.rm=T)), ]

db<-db_s[ , names(db_s) %in% vars1] # main variables
db_var = db_s[ , names(db_s) %in% vars2] # all covariates
db_var_std = as.data.frame(scale(db_var)) # standardising covariates
db_pc = db_s %>% dplyr::select(id,POSTCODE) # selectingpostcode 
db_main = db_s %>% dplyr::select(id,median_income,comp5km) #,home_internet,Median.commuting.distance..kms.) # selecting the income, competition and two search variables
db_main_std = as.data.frame(scale(db_main[,2:ncol(db_main)])) # standardising heterogeneity variables


agg = aggregate(db, by=list(treat = db$treat,id = db$id,pm = db$pm), FUN='mean', na.rm=T)

agg = agg[,c('id','pm','treat','margin_u_ds')]

difference=function(x){
  x[2]-x[1]} # we calculate the before-after difference, and run the CF on this difference as the outcome variable

agg2 =  aggregate(agg[4], by=list(id = agg$id), FUN= difference)
agg2 = left_join(agg2,agg[,1:3],by='id')
agg2 = agg2[!duplicated(agg2$id),]


preds = data.frame(matrix("", ncol = 0, nrow = nrow(agg2)))
preds$id = agg2$id
preds_var = data.frame(matrix("", ncol = 0, nrow = nrow(agg2)))
preds_var$id = agg2$id
importance = data.frame(matrix("", ncol = 0, nrow = length(vars2)))
importance$variable = vars2
pred_tab = data.frame(matrix("", ncol = 0, nrow = 4))
pred_tab_var = data.frame(matrix("", ncol = 0, nrow = 4))
pred_tab2 = data.frame(matrix("", ncol = 0, nrow = 2))
pred_tab_var2 = data.frame(matrix("", ncol = 0, nrow = 2))
ATE = list()
ATT = list()
ATEs = list()
ATTs = list()
sampling = 15
# the following loop randomly samples 'sampling' number of variables, and runs a causal forest on this subset of variables only


for (i in 1:400){
  gc()
  print(i)
  x1 <- as.integer(runif(15, 1, ncol(db_var)))
  db_var2 = db_var[,x1]
  db_var2 = cbind(db_var2,db_s$id) # adding id
  names(db_var2)[ncol(db_var2)]='id'
  db_var2 = db_var2[!duplicated(db_var2$id),]
  db_main2 = db_main[!duplicated(db_main$id),]
  
  aggs = left_join(agg2,db_var2,by='id')
  aggs = left_join(aggs,db_main2,by='id')

  aggs<-na.omit(aggs)
  if (nrow(aggs[aggs$treat==1,])==0){next}
  X = scale(aggs[,c(5:ncol(aggs))])
  Y = scale(aggs$margin_u_ds)
  W = aggs$treat
  # first stage 
  Y.forest = regression_forest(X, Y)
  Y.hat = predict(Y.forest)$predictions
  W.forest = regression_forest(X, W)
  W.hat = predict(W.forest)$predictions
  cf = causal_forest(X, Y, W,Y.hat = Y.hat , W.hat = W.hat)
  tau.hat = as.data.frame(predict(cf)$predictions) # getting predicted TE
  tau.hat$id = aggs$id
  preds = left_join(preds,tau.hat,by='id')
  tau.hat = as.data.frame(predict(cf,estimate.variance=T)$variance.estimates) # getting predicted TE
  tau.hat$id = aggs$id
  preds_var = left_join(preds_var,tau.hat,by='id')
  cf1<-cf %>% # creates a df of variable importances
    variable_importance() %>% 
    as.data.frame() %>% 
    mutate(variable = colnames(cf$X.orig)) %>% 
    arrange(desc(V1)) 
  # getting income-competition combination first
  source("c:/Users/peter/OneDrive - University of East Anglia/UEA/RESEARCH/Distributional effects/Australia/Data/r/newx_income_comp.R") # this newx file creates a df of the median of the four covariates of interest (income competition, commuting, internet)
  
  tau2 = predict(cf,newx,estimate.variance=T) # predicted tau using the estimated CF and median values of four main coefficients
  
  pred_tab = cbind(pred_tab,tau2$predictions)
  pred_tab_var = cbind(pred_tab_var,tau2$variance.estimates)

  importance = left_join(importance,cf1,by='variable')
  ATE[[i]] = average_treatment_effect(cf,target.sample="overlap")[1]
  ATT[[i]] = average_treatment_effect(cf,target.sample="treated")[1]
  ATEs[[i]] = average_treatment_effect(cf,target.sample="overlap")[2]
  ATTs[[i]] = average_treatment_effect(cf,target.sample="treated")[2]
  }

# after the loop, arrange the estimated ATE and att into a single df.

ATE2 = as.data.frame(do.call(rbind, ATE))
ATT2 = as.data.frame(do.call(rbind, ATT))
AT = cbind(ATE2, ATT2)
ATE2 = as.data.frame(do.call(rbind, ATEs))
ATT2 = as.data.frame(do.call(rbind, ATTs))
ATs = cbind(ATE2, ATT2)
AT = cbind(AT,ATs)
names(AT) = c('ate','att','ates','atts')
save(AT,file='ATs_petrol_entry_rnr.Rda')

# create a table of the averaged ATEs and ATTs (using a fixed effect meta study formula)

AT$ATE_numerator = AT$ate * (1/AT$ates)
AT$ATE_denominator = (1/AT$ates)
ATE_es = round(sum(AT$ATE_numerator)/sum(AT$ATE_denominator),3)
ATE_se = round(sqrt(1/sum(AT$ATE_denominator)),3)

AT$ATT_numerator = AT$att * (1/AT$atts)
AT$ATT_denominator = (1/AT$atts)
ATT_es = round(sum(AT$ATT_numerator)/sum(AT$ATT_denominator),3)
ATT_se = round(sqrt(1/sum(AT$ATT_denominator)),3)

ATS_petrol_entry = as.data.frame(matrix(c(ATE_es,ATE_se,ATT_es,ATT_se),ncol=2))
names(ATS_petrol_entry)=c('ATE','ATT')
ATS_petrol_entry=round(ATS_petrol_entry,3)
ATS_petrol_entry[2,]=paste0("(",ATS_petrol_entry[2,],")")

write.xlsx(ATS_petrol_entry,'ats_petrol_enrty_rnr.xlsx')           
save(ATS_petrol_entry,file='ats_petrol_entry_rnr.Rda')

# PREDICTION ROWMEAN

# creating a df of predictions

preds$rowmean = rowMeans(preds[,-1],na.rm=T) # row mean
predictions = left_join(preds[c(1,ncol(preds))],agg2,by='id')
db_var_std = cbind(db_var_std,db_s$id) # here we standardise the covariates before merging with the TEs
db_main_std = cbind(db_main_std,db_s$id) # here we standardise the covariates before merging with the TEs

names(db_var_std)[ncol(db_var_std)]='id'
names(db_main_std)[ncol(db_main_std)]='id'

db_var_std = db_var_std[!duplicated(db_var_std$id),]
db_main_std = db_main_std[!duplicated(db_main_std$id),]

predictions = left_join(predictions,db_var_std,by='id')
predictions = left_join(predictions,db_main_std,by='id')

save(predictions,file='predictions_petrol_rnr.Rda')

# IMPORTANCE 

# creating a df of importance

importance$rowmean = rowMeans(importance[2:ncol(importance)],na.rm=T)
importance2 = importance[,c(1,ncol(importance))]
save(importance2,file='importance_petrol_rnr.Rda')



# PREDICTION Comp-income

temp = as.data.frame(t(pred_tab))
temp2 = as.data.frame(t(pred_tab_var))

lilc = as.data.frame(t(rbind(temp[,1],temp2[,1])))
lilc$numerator = lilc$V1 * (1/lilc$V2)
lilc$denominator = (1/lilc$V2)
lilc_es = round(sum(lilc$numerator)/sum(lilc$denominator),3)
lilc_se = round(sqrt(1/sum(lilc$denominator)),3)

hilc = as.data.frame(t(rbind(temp[,2],temp2[,2])))
hilc$numerator = hilc$V1 * (1/hilc$V2)
hilc$denominator = (1/hilc$V2)
hilc_es = round(sum(hilc$numerator)/sum(hilc$denominator),3)
hilc_se = round(sqrt(1/sum(hilc$denominator)),3)

lihc = as.data.frame(t(rbind(temp[,3],temp2[,3])))
lihc$numerator = lihc$V1 * (1/lihc$V2)
lihc$denominator = (1/lihc$V2)
lihc_es = round(sum(lihc$numerator)/sum(lihc$denominator),3)
lihc_se = round(sqrt(1/sum(lihc$denominator)),3)

hihc = as.data.frame(t(rbind(temp[,4],temp2[,4])))
hihc$numerator = hihc$V1 * (1/hihc$V2)
hihc$denominator = (1/hihc$V2)
hihc_es = round(sum(hihc$numerator)/sum(hihc$denominator),3)
hihc_se = round(sqrt(1/sum(hihc$denominator)),3)

z_score_low_comp = round((lilc_es-hilc_es)/sqrt(lilc_se^2+hilc_se^2),3)
z_score_high_comp = round((lihc_es-hihc_es)/sqrt(lihc_se^2+hihc_se^2),3)

lilc_se = paste0("(", lilc_se, ")")
lilc = rbind (lilc_es,lilc_se)
hilc_se = paste0("(", hilc_se, ")")
hilc = rbind (hilc_es,hilc_se)
lihc_se = paste0("(", lihc_se, ")")
lihc = rbind (lihc_es,lihc_se)
hihc_se = paste0("(", hihc_se, ")")
hihc = rbind (hihc_es,hihc_se)

effects = as.data.frame(cbind(rbind(lilc,z_score_low_comp,lihc,z_score_high_comp),rbind(hilc,z_score_low_comp,hihc,z_score_high_comp)))

names(effects) = c('low income','high income')
rownames(effects) = c('low competition','low competition se','z-score','high competition','high competition se','z-score2')

effects

write.xlsx(effects,'TE_income_comp_entry_rnr.xlsx')



