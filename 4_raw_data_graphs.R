
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




setwd("[add_your_path_of_data]") 


load(file='exit_data_nearest5_treat_traffic_rnr.Rda')
load(file='entry_data_nearest5_treat_traffic_rnr.Rda')



# Treat v Cont by income - exit


db_short=db2 
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





