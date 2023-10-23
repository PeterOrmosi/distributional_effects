# this file generates all combinations of 0.1 and 0.9 percentile values for the variables of interest. This results in n^2 combinations.
# for 2 relevant variables (competition and income) we have 4 predictions to run

newx = as.data.frame(t(colMeans(X,na.rm=T)))
newx = newx[rep(seq_len(nrow(newx)), each = 4), ]


newx$median_income[1]=quantile(db_main_std$median_income,0.1, na.rm=T)
newx$comp5km[1]=quantile(db_main_std$comp5km,0.1, na.rm=T)

newx$median_income[2]=quantile(db_main_std$median_income,0.9, na.rm=T)
newx$comp5km[2]=quantile(db_main_std$comp5km,0.1, na.rm=T)

newx$median_income[3]=quantile(db_main_std$median_income,0.1, na.rm=T)
newx$comp5km[3]=quantile(db_main_std$comp5km,0.9, na.rm=T)

newx$median_income[4]=quantile(db_main_std$median_income,0.9, na.rm=T)
newx$comp5km[4]=quantile(db_main_std$comp5km,0.9, na.rm=T)
