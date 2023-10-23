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


My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 12),
  legend.text=element_text(size=14),
  plot.title = element_text(size = 18),
  legend.title = element_text(size=16))
theme_set(theme_minimal(base_size = 20)) 




setwd("c:/Users/peter/OneDrive - University of East Anglia/UEA/RESEARCH/Distributional effects/Australia/Data/") 

#load(file='exit_data_matchit.Rda')

load(file='exit_data_nearest5_treat_traffic_rnr.Rda')
load(file='entry_data_nearest5_treat_traffic_rnr.Rda')




# Treat v Cont by income


db_short=db2 #[!db2$panel2 %in% reenter,]
db_short$margin_u_ds[sapply(db_short$margin_u_ds, is.infinite)] <- NA
db_short$margin_u_ds = as.numeric(db_short$margin_u_ds)
db_short=db_short[db_short$median_income<quantile(db_short$median_income,0.33,na.rm=T),]

agg<-aggregate(margin_u_ds ~ seq + treat, data=db_short, function(x) c(mn = mean(x)))
colnames(agg)<-c("time",'treat','margin')
agg$margin<-fnrollmean(agg$margin)

exit2=ggplot(agg,aes(x=time,y=margin,color=interaction(treat))) + geom_line(size=1.5) +  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Retail margin") + xlab("Time relative to exit (weeks)") + geom_vline(xintercept=c(0), linetype="dotted") + ggtitle("Lowest income tercile") +
  theme(plot.title = element_text(hjust = 0.48)) + theme(legend.position="bottom", legend.title = element_blank()) +
  scale_colour_grey(start = 0.8, end = 0,labels = c( "control","treatment"),name='') + ylab("retail margin") +
  xlim(-15,25)
exit2

db_short=db2

db_short=db_short[db_short$median_income>quantile(db_short$median_income,0.66,na.rm=T),]

agg<-aggregate(margin_u_ds ~ seq + treat, data=db_short, function(x) c(mn = mean(x)))
colnames(agg)<-c("time",'treat','margin')
agg$margin<-fnrollmean(agg$margin)

exit2a=ggplot(agg,aes(x=time,y=margin,color=interaction(treat))) + geom_line(size=1.5) +  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Retail margin") + xlab("Time relative to exit (weeks)") + geom_vline(xintercept=c(0), linetype="dotted") + ggtitle("Highest income tercile") +
  theme(plot.title = element_text(hjust = 0.48)) + theme(legend.position="bottom", legend.title = element_blank()) +
  scale_colour_grey(start = 0.8, end = 0,labels = c( "control","treatment"),name='') + ylab("retail margin") +
  xlim(-15,25)

exit2a

treatm<-ggarrange(exit2,exit2a,nrow=1, common.legend = T, legend='bottom')
treatm
ggsave('exit_treat_low_v_high_income_rnr.pdf')





# Treat v Cont by income - ENTRY


db_short=db_entry2 #[!db2$panel2 %in% reenter,]
db_short$margin_u_ds[sapply(db_short$margin_u_ds, is.infinite)] <- NA
db_short$margin_u_ds = as.numeric(db_short$margin_u_ds)
db_short=db_short[db_short$median_income<quantile(db_short$median_income,0.33,na.rm=T),]

agg<-aggregate(margin_u_ds ~ seq + treat, data=db_short, function(x) c(mn = mean(x)))
colnames(agg)<-c("time",'treat','margin')
agg$margin<-fnrollmean(agg$margin)

exit2=ggplot(agg,aes(x=time,y=margin,color=interaction(treat))) + geom_line(size=1.5) +  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Retail margin") + xlab("Time relative to entry (weeks)") + geom_vline(xintercept=c(0), linetype="dotted") + ggtitle("Lowest income tercile") +
  theme(plot.title = element_text(hjust = 0.48)) + theme(legend.position="bottom", legend.title = element_blank()) +
  scale_colour_grey(start = 0.8, end = 0,labels = c( "control","treatment"),name='') + ylab("retail margin") +
  xlim(-15,25)
exit2

db_short=db_entry2

db_short=db_short[db_short$median_income>quantile(db_short$median_income,0.33,na.rm=T),]

agg<-aggregate(margin_u_ds ~ seq + treat, data=db_short, function(x) c(mn = mean(x)))
colnames(agg)<-c("time",'treat','margin')
agg$margin<-fnrollmean(agg$margin)

exit2a=ggplot(agg,aes(x=time,y=margin,color=interaction(treat))) + geom_line(size=1.5) +  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Retail margin") + xlab("Time relative to entry (weeks)") + geom_vline(xintercept=c(0), linetype="dotted") + ggtitle("Highest income tercile") +
  theme(plot.title = element_text(hjust = 0.48)) + theme(legend.position="bottom", legend.title = element_blank()) +
  scale_colour_grey(start = 0.8, end = 0,labels = c( "control","treatment"),name='') + ylab("retail margin") +
  xlim(-15,25)

exit2a

treatm<-ggarrange(exit2,exit2a,nrow=1, common.legend = T, legend='bottom')
treatm
ggsave('entry_treat_low_v_high_income_rnr.pdf')









# BRAND


top_brand<-c('Coles Express','Caltex','Caltex Woolworths','7-Eleven') ## adding a dummy for top brand exitter
db2$top_brand<-ifelse((db2$exitter %in% top_brand),1,0)

db_short<-db2 #[db2$top_brand==1,]
db_short=db_short[db_short$top_brand==1,]

agg<-aggregate(margin_u_ds ~ seq + treat, data=db_short, function(x) c(mn = mean(x)))
colnames(agg)<-c("time",'disadvantage','margin')
agg$margin<-fnrollmean(agg$margin)

exit4a=ggplot(agg,aes(x=time,y=margin,color=interaction(disadvantage))) + geom_line(size=2.5) +  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Retail margin") + xlab("Time relative to exit (weeks)") + geom_vline(xintercept=c(0), linetype="dotted") + 
  ggtitle("ULP + Top brands only") +
  theme(plot.title = element_text(hjust = 0.48)) + theme(legend.position="bottom", legend.title = element_blank()) +
  scale_colour_grey(start = 0, end = .8,labels = c("Treatment", "Control"),name='') + ylab("retail margin") +
  xlim(-15,25)
exit4a


db_short<-db2 #[db2$top_brand==1,]
db_short=db_short[db_short$top_brand==0,]

agg<-aggregate(margin_u_ds ~ seq + treat, data=db_short, function(x) c(mn = mean(x)))
colnames(agg)<-c("time",'disadvantage','margin')
agg$margin<-fnrollmean(agg$margin)

exit4b=ggplot(agg,aes(x=time,y=margin,color=interaction(disadvantage))) + geom_line(size=2.5) +  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Retail margin") + xlab("Time relative to exit (weeks)") + geom_vline(xintercept=c(0), linetype="dotted") + 
  ggtitle("ULP + Not top brands") +
  theme(plot.title = element_text(hjust = 0.48)) + theme(legend.position="bottom", legend.title = element_blank()) +
  scale_colour_grey(start = 0, end = .8,labels = c("Treatment", "Control"),name='') + ylab("retail margin") +
  xlim(-15,25)
exit4b


brand<-ggarrange(exit4a,exit4b,nrow=1, common.legend = T, legend='bottom')
brand

ggsave('exit_brand.pdf')



# INCOME


db_short<-db2
db_short=db_short[db_short$treat==1,]
#db_short=db_short[db_short$competitors==0,]

xs=quantile(db_short$median_income,c(0,1/3,2/3,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
db_short <- db_short %>% mutate(incomecat=cut(median_income, breaks=xs, labels=c("low","middle","high")))
tab<-table(db_short$incomecat)/51

agg<-aggregate(margin_u_ds ~ seq + incomecat, data=db_short, function(x) c(mn = mean(x)))
colnames(agg)<-c("time",'income','margin')
agg$margin<-fnrollmean(agg$margin)

exit3a=ggplot(agg,aes(x=time,y=margin,color=interaction(income))) + geom_line(size=3) +  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Retail margin") + xlab("Time relative to exit (weeks)") + geom_vline(xintercept=c(0), linetype="dotted") + ggtitle("ULP") +
  theme(plot.title = element_text(hjust = 0.48)) + theme(legend.position="bottom") +
  #scale_color_discrete(labels = c(paste0("Low (",tab[1],' markets)'), paste0("Medium (",tab[2],' markets)'),paste0("High (",tab[3],' markets)')),name='Median income') + 
  scale_colour_grey(start = 0, end = .8,labels = c(paste0("Low (",tab[1],' markets)'), paste0("Medium (",tab[2],' markets)'),paste0("High (",tab[3],' markets)')),name='Income') + 
  guides(color = guide_legend(title.position="top", title.hjust = 0.5)) + xlim(-15,15)
exit3a

agg<-aggregate(margin_d_ds ~ seq + incomecat, data=db_short, function(x) c(mn = mean(x)))
colnames(agg)<-c("time",'income','margin')
agg$margin<-fnrollmean(agg$margin)

exit3b=ggplot(agg,aes(x=time,y=margin,color=interaction(income))) + geom_line(size=3) +  theme_minimal() +  My_Theme +
  scale_y_continuous(name = "Retail margin") + xlab("Time relative to exit (weeks)") + geom_vline(xintercept=c(0), linetype="dotted") + ggtitle("Diesel") +
  theme(plot.title = element_text(hjust = 0.48)) + theme(legend.position="bottom") +
  #scale_color_discrete(labels = c(paste0("Low (",tab[1],' markets)'), paste0("Medium (",tab[2],' markets)'),paste0("High (",tab[3],' markets)')),name='Median income') + 
  scale_colour_grey(start = 0, end = .8,labels = c(paste0("Low (",tab[1],' markets)'), paste0("Medium (",tab[2],' markets)'),paste0("High (",tab[3],' markets)')),name='Income') + 
  guides(color = guide_legend(title.position="top", title.hjust = 0.5)) + xlim(-15,15)
exit3b

inc<-ggarrange(exit3a,exit3b,nrow=1, common.legend = T, legend='bottom')
inc
ggsave('exit_income.pdf')






# POPULATION


db_short<-db2
db_short=db_short[db_short$treat==1,]
#db_short=db_short[db_short$competitors==0,]

xs=quantile(db_short$Usual.Resident.Population,c(0,1/3,2/3,1),na.rm=TRUE)
xs[1]=xs[1]-.00005
db_short <- db_short %>% mutate(pop_cat=cut(Usual.Resident.Population, breaks=xs, labels=c("low","med","high")))
tab<-table(db_short$pop_cat)/37
db_short<-db_short[db_short$pop_cat!='med',]

agg<-aggregate(margin_u_ds ~ seq + pop_cat, data=db_short, function(x) c(mn = mean(x)))
colnames(agg)<-c("time",'income','margin')
agg1<-agg[agg$income=='low',]
agg2<-agg[agg$income=='high',]
agg1$margin<-fnrollmean(agg1$margin)
agg2$margin<-fnrollmean(agg2$margin)

exit3a<-ggplot() + geom_line(mapping = aes(x = agg1$time, y = agg1$margin), size = 2, color = "#000000") + 
  geom_line(mapping = aes(x = agg2$time, y = agg2$margin*1.03), size = 2, color = "#C0C0C0") + 
  My_Theme + ggtitle("ULP") +
  scale_y_continuous(name = "retail margin (low population)", sec.axis = sec_axis(~./1.03, name = "retail margin (high Population)") ) +
  theme(axis.title.y = element_text(color = "#000000"),axis.title.y.right = element_text(color = "#C0C0C0")) +
  xlab("Time relative to exit (weeks)") + geom_vline(xintercept=c(0), linetype="dotted") +  theme(plot.title = element_text(hjust = 0.48)) +
  theme(axis.line.y.right = element_line(color = "#C0C0C0"), 
        axis.ticks.y.right = element_line(color = "#C0C0C0"),
        axis.text.y.right = element_text(color = "#C0C0C0"), 
        axis.title.y.right = element_text(color = "#C0C0C0"),
        axis.line.y.left = element_line(color = "#000000"), 
        axis.ticks.y.left = element_line(color = "#000000"),
        axis.text.y.left = element_text(color = "#000000"), 
        axis.title.y.left = element_text(color = "#000000")
  ) + xlim(-15,15)
exit3a


aggb<-aggregate(margin_d_ds ~ seq + pop_cat, data=db_short, function(x) c(mn = mean(x)))
colnames(aggb)<-c("time",'income','margin')
agg1b<-aggb[aggb$income=='low',]
agg2b<-aggb[aggb$income=='high',]
agg1b$margin<-fnrollmean(agg1b$margin)
agg2b$margin<-fnrollmean(agg2b$margin)

exit3b<-ggplot() + geom_line(mapping = aes(x = agg1b$time, y = agg1b$margin), size = 2, color = "#000000") + 
  geom_line(mapping = aes(x = agg2b$time, y = agg2b$margin*1.01), size = 2, color = "#C0C0C0") + 
  My_Theme + ggtitle("ULP") +
  scale_y_continuous(name = "retail margin (low population)", sec.axis = sec_axis(~./1.01, name = "retail margin (high Population)") ) +
  theme(axis.title.y = element_text(color = "#000000"),axis.title.y.right = element_text(color = "#C0C0C0")) +
  xlab("Time relative to exit (weeks)") + geom_vline(xintercept=c(0), linetype="dotted") +  theme(plot.title = element_text(hjust = 0.48)) +
  theme(axis.line.y.right = element_line(color = "#C0C0C0"), 
        axis.ticks.y.right = element_line(color = "#C0C0C0"),
        axis.text.y.right = element_text(color = "#C0C0C0"), 
        axis.title.y.right = element_text(color = "#C0C0C0"),
        axis.line.y.left = element_line(color = "#000000"), 
        axis.ticks.y.left = element_line(color = "#000000"),
        axis.text.y.left = element_text(color = "#000000"), 
        axis.title.y.left = element_text(color = "#000000")
  ) + xlim(-15,15)
exit3b

population<-ggarrange(exit3a,exit3b,nrow=1, common.legend = T, legend='bottom')
population
ggsave('exit_population.pdf')













## PLM
library(plm)



load(file='exit_data_average_treat.Rda')

load(file="weekly_wide_data_geo.Rda")

# library(data.table)
# setDT(kleenheat)
# setkeyv(kleenheat, c("time", "panel"))







# filtering those where there's only 1 exit

db2$comp_norm<-db2$competitors/(db2$Usual.Resident.Population/10000)


db2$pm<-ifelse(db2$seq<0,0,1)

db2$comp_norm<-ifelse(db2$competitors==0,0,db2$competitors/db2$Usual.Resident.Population * 10000)

summary(db2$comp_norm)
quantile(db2$comp_norm, c(.97, .98, .99), na.rm=T) 
db2$comp_norm<-as.integer((db2$comp_norm))




brandlist<-weekly[,c('panel','BRAND_DESCRIPTION')]
brandlist<-brandlist[!duplicated(brandlist$panel),]
names(brandlist)<-c('brand_exit','exitter')

db2<-left_join(db2,brandlist,by="brand_exit")

top_brand<-c('BP','Shell','Caltex')
db2$top_brand<-ifelse((db2$exitter %in% top_brand),1,0)



# corvar<-c('Index.of.Economic.Resources','Index.of.Education.and.Occupation','median_income')
# cormat<-db2[corvar]
# 
# rcorr(as.matrix(cormat))

db_s<-db2

#db_s<-db_s[db_s$seq<16,]
  
db_s <- db_s %>%  ungroup()

#db_s<-db_s[db_s$competitors<3,]
db_s<-db_s[abs(db_s$seq)<16,]

#db_s$seq2<-db_s$seq+17

# 
# 
# time_dummies<-c('seq_1','seq_2','seq_3','seq_4','seq_5',
#                 'seq_6','seq_7','seq_8','seq_9','seq_10','seq_11','seq_12','seq_13','seq_14','seq_15','seq_16','seq_17','seq_18',
#                 'seq_19','seq_20','seq_21', 'seq_22','seq_23','seq_24', 'seq_25','seq_26','seq_27', 'seq_28','seq_29','seq_30',
#                 'seq_31','seq_32','seq_33','seq_34','seq_35','seq_36','seq_37','seq_38')

#db_s$brand<-as.numeric(db_s$BRAND_DESCRIPTION)
db_s$pm<-ifelse(db_s$seq>0,1,0)
db_s$pmtreat <- db_s$pm * db_s$treat
# 
# variables <- c('pm','pmtreat','ulp','median_income','competitors','Index.of.Economic.Resources','Index.of.Relative.Socio.economic.Disadvantage',
#                'earners_age','number_of_earners','gini_coefficient','Car..as.driver','One.motor.vehicle', 
#                'Usual.Resident.Population')

#db_s <- db_s %>% mutate_at(c("margin",'pmtreat', "median_income",'competitors','ulp','margin_ws','petrol_ws'), ~(scale(.) %>% as.vector))


db_panel<-pdata.frame(db_s, index=c("id","seq"), drop.index=FALSE, row.names=TRUE)
 db_panel<-db_panel[,c('pm','treat','pmtreat','panel','margin_u_ds','margin_d_ds','ulp_ds', 'margin_u','ulp','median_income','competitors','Index.of.Economic.Resources','Index.of.Relative.Socio.economic.Disadvantage',
                       'earners_age','number_of_earners','gini_coefficient','Car..as.driver','One.motor.vehicle', 
                       'Index.of.Relative.Socio.economic.Disadvantage','Index.of.Economic.Resources',
                       'Index.of.Education.and.Occupation','Median.commuting.distance..kms.','comp_norm','year','quarter','top_brand',
                       'Usual.Resident.Population','brand_exit','margin_ws_u','petrol_ws','cushing_ds','comp2km')]



db_panel[,5:30]<-as.data.frame(scale(db_panel[,5:30]))
#db_panel<-db_panel[db_panel$median_income<2.5,]

#db_panel$median_income<-db_panel$median_income/10000

# db_panel<-db_panel[,c('pm','panel','panel2','margin','ulp','petrol_ws','median_income','competitors','Index.of.Economic.Resources','Index.of.Relative.Socio.economic.Disadvantage',
#                       'earners_age','number_of_earners','gini_coefficient','Car..as.driver','One.motor.vehicle', 
#                       'Index.of.Relative.Socio.economic.Disadvantage',
#                       'Usual.Resident.Population','cushing','margin_ws','Median.commuting.distance..kms.','year','quarter',
#                       'comp2km','comp5km','brand','top_brand' )]

#db_panel$margin<-db_panel$ulp/db_panel$petrol_ws
#f <- as.formula(paste("margin",paste(variables,collapse="+"),sep="~"))
model1 <- plm(margin_u_ds ~     pmtreat
             ,data = db_panel,model = "pooling", effect='twoway')

model2 <- plm(margin_u_ds ~   pm + treat +  pmtreat * median_income
             ,data = db_panel,model = "pooling", effect='twoway')


model3 <- plm(margin_u_ds ~   pm + treat + pmtreat + pmtreat * median_income * competitors
             ,data = db_panel,model = "pooling", effect='individual')

model4 <- plm(margin_u_ds ~   pm + treat + pmtreat + pmtreat * median_income * competitors * comp2km
             ,data = db_panel,model = "pooling", effect='individual')


model <- plm(margin_u_ds ~    pmtreat + 
               pmtreat * Index.of.Economic.Resources * competitors * Index.of.Education.and.Occupation  + 
               pmtreat * comp2km + pmtreat*Median.commuting.distance..kms. +
               as.factor(year)*pmtreat + as.factor(quarter)*pmtreat, data = db_panel,model = "pooling", effect='individual')


#model <- plm(margin_u_ds ~     pmtreat : as.factor(seq2),data = db_panel,model = "within", effect='twoway')

model <- lm(margin_u_ds ~   pm + treat +  pmtreat + 
              pmtreat * Index.of.Economic.Resources * competitors * Index.of.Education.and.Occupation * median_income + 
              pmtreat * comp2km + pmtreat*Median.commuting.distance..kms. +
               as.factor(year)*pmtreat + as.factor(quarter)*pmtreat , data = db_panel)



model1 <- lm(margin_u_ds ~   pm + treat +  pmtreat , data = db_panel)
model2 <- lm(margin_u_ds ~   pm + treat +  pmtreat * median_income, data = db_panel)
model3 <- lm(margin_u_ds ~   pm + treat +  pmtreat * median_income * competitors, data = db_panel)
model4 <- lm(margin_u_ds ~   pm + treat +  pmtreat * median_income * competitors * as.factor(quarter), data = db_panel)



model <- lm(margin_u_ds ~    pmtreat + pmtreat * median_income  + pmtreat*as.factor(year) +
              pmtreat * competitors  + pmtreat* as.factor(quarter) + pmtreat * competitors * median_income  * Median.commuting.distance..kms. 
             ,data = db_panel)


model <- ivreg(margin_u_ds ~    pmtreat + 
                 pmtreat * Index.of.Economic.Resources * competitors * Index.of.Education.and.Occupation * median_income + 
                 pmtreat * comp2km + pmtreat*Median.commuting.distance..kms. 
                  |  as.factor(year)*pmtreat + as.factor(quarter)*pmtreat , data = db_panel)

model <- plm(margin_u_ds ~   pmtreat + pmtreat * median_income +
               pmtreat * competitors + pmtreat * median_income *competitors + pmtreat *  Usual.Resident.Population +
               pmtreat*number_of_earners + pmtreat* earners_age  , #+ pmtreat * top_brand,
             data = db_panel,model = "within", effect='individual')
 
model <- plm(ulp_ds ~   pmtreat + pmtreat * median_income
               ,
             data = db_panel,model = "within", effect='individual')

#cor(db_panel$Usual.Resident.Population,db_panel$number_of_earners,  use = "complete.obs")

# model <- plm(margin ~ pmtreat +  median_income + median_income * pmtreat + I(median_income^2)*pmtreat  + competitors + competitors * pmtreat  , data = db_panel,model = "within", effect='twoway')

coeftest(model) #,vcov = vcovHC(model,type="HC0"))
summary(model4)

nobs(model)



library(sjPlot)
library(sjmisc)
library(sjlabelled)

theme_set(theme_sjplot())

set_theme(
  base = theme_blank(),
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 3
)


plot_model(model1, type = "pred", terms = c("median_income","pmtreat")) 
plot_model(model2, type = "pred", terms = c("median_income","pmtreat")) 
plot_model(model3, type = "pred", terms = c("median_income","pmtreat", 'competitors[0,2,4]'),legend.title = NULL) + 
  scale_color_discrete(labels = c("control", "treatment"))
plot_model(model4, type = "pred", terms = c("median_income","pmtreat",'competitors[0,4,8]','quarter')) 

plot_model(model4,show.values = TRUE, value.offset = .3) 

ggpred <- ggpredict(model3, terms = c("median_income",'pmtreat',"competitors"))
ggplot(ggpred, aes(x, predicted)) + 
  geom_line(aes(linetype=group, color=group)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.15) +
  scale_linetype_manual(values = c("dashed", "solid"),labels = c('control','treatment'),name='') + facet_wrap(~facet) +
  scale_colour_manual(values=c('#808080','#000000'),labels = c('control','treatment'),name='') + 
  scale_fill_manual(values=c('#808080','#000000'),labels = c('control','treatment'),name='')



d<-density(db_panel$median_income, na.rm=T)
plot(d)


median(db_panel$median_income, na.rm=T)

