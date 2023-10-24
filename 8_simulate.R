
library(matrixcalc)
library(corpcor)
library(lfe, quietly = TRUE)
library(lme4)
require(snowfall)
library(MASS)
library(grf)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(grid)
library(scales)
library(gridExtra)
library(dplyr)

#  p is no. of variables, n is no. of firms.

loops = data.frame()
noloops = data.frame()

for (j in 2:12){
  
  set.seed(11)
  
  p=10 * j

  print(paste('main',as.character(j)))
  
  n=2000
  # generate p random variables

  firm=seq(1,n)
  data <- expand.grid(firm=firm)
  
  # W is the treatment assignment
  data$W <- rbinom(n, 1, 0.5)
  data$train <- 1 #rbinom(n, 1, 0.5)
  
  # generate two correlated variables
  covarMat = diag(p)
  covarMat[,] =  0    #insert highly correlated variables
  covarMat[,] = covarMat[,] - sample(-50:-1., p^2, replace=TRUE)  #make data a more realistic
  
  # making sure the matrix is symmetric
  covarMat_u <- covarMat
  covarMat_u[upper.tri(covarMat, F)] = 0
  covarMat_l <- t(covarMat_u)
  covarMat = covarMat_l + covarMat_u
  covarMat = covarMat/100
  diag(covarMat)=1
  
  covarMat = make.positive.definite(covarMat, tol=1e-3) # changing standard errors to make it positive definite

  data.2  = as.data.frame(mvrnorm(n , mu=rep(0,p), Sigma=covarMat ))
  
  data<- bind_cols(data,data.2) %>%
   tibble::as_tibble()
  
  y<-c()
  unit<-c()
  X<-c()
 
  # this loops adds 20% of all variables to affect the treatment effect

    for(i in 1:n){
      unit[i]<-i
      covar=0
      for (param in 1:(round(p/5,0)-1)){
        current = 0
        current = data[i,(4+param)] +  data[i,(4+param)]*data$W[i]
        covar= covar + current
        }
      y[i]<-1+ data$W[i] + data$V1[i] +  data$V1[i]*data$W[i]  + rnorm(1,mean=0,sd=1) + covar
    }
    
    
  data$unit=unit
  data$y=as.numeric(y)

  data.train <- data %>%
    filter(train==1)
  data.test <- data %>%
    filter(train==0)
  

  # a is a data set of firm dummies
  #a <- as.data.frame(model.matrix(y~as.factor(firm),data.train))
  X <- as.matrix(bind_cols(data.train[,c(4:(ncol(data.train)-3))]))
  Y <- data.train$y
  W <- data.train$W
  # causal forest is run on y against V1, to V10 and firm dummies
  Y.forest = regression_forest(X, Y)
  Y.hat = predict(Y.forest)$predictions
  W.forest = regression_forest(X, W)
  W.hat = predict(W.forest)$predictions
  tau.forest = causal_forest(X, Y, W,Y.hat = Y.hat , W.hat = W.hat)
  #tau.hat = as.data.frame(predict(cf)$predictions)
  tau.forest = causal_forest(X, Y, W)
  
  average_treatment_effect(tau.forest, target.sample = "all")
  average_treatment_effect(tau.forest, target.sample = "treated")
  
  tau.hat = predict(tau.forest, X, estimate.variance = TRUE)
  data.train$pred <- tau.hat$predictions
  ggplot(data.train, aes(x=V1, y=pred)) + geom_point() + geom_abline(intercept = 1, slope = 2)
  
  
  varimp = variable_importance(tau.forest)
  selected.idx = which(varimp  > mean(varimp)) #quantile(varimp, 0.90 ))
  selected.idx=sort(unique(c(selected.idx,1,2,3)),decreasing=F)
  
  X_sel = as.data.frame(X[,selected.idx])
  #X_sel=as.matrix(bind_cols(data.train2[,c(4:(ncol(data.train2)-3))]))
  
  Y.forest = regression_forest(X_sel, Y)
  Y.hat = predict(Y.forest)$predictions
  W.forest = regression_forest(X_sel, W)
  W.hat = predict(W.forest)$predictions
  cf = causal_forest(X_sel, Y, W,Y.hat = Y.hat , W.hat = W.hat)
  tau.hat2 = predict(cf)$predictions
  X_sel$pred=tau.hat2
  X_sel$W = data.train$W
  ggplot(X_sel, aes(x=V1, y=pred)) + geom_point() + geom_abline(intercept = 1, slope = 2)
  
  cf1<-cf %>%
    variable_importance() %>%
    as.data.frame() %>%
    mutate(variable = colnames(cf$X.orig)) %>%
    arrange(desc(V1))
  cf1
  
  mod = lm(tau.hat2 ~ . , data=as.data.frame(X_sel[,1:(ncol(X_sel)-2)]))
  
  summary(mod)
  
  coeffs = coeftest(mod)
  est11 = cbind(p,coeffs[2,1],coeffs[2,2])
  est12 = cbind(p,coeffs[3,1],coeffs[3,2])
  est13 = cbind(p,coeffs[4,1],coeffs[4,2])
  vars = rbind(est11,est12,est13)



  ### ESNEMBLE VERSION
  
  covars = data.train[,c(4:(ncol(data.train)-3))]
  preds = data.frame(matrix("", ncol = 0, nrow = nrow(data.train)))
  preds$firm = data.train$firm
  ATE = list()
  ATT = list()
  ATEs = list()
  ATTs = list()
  sampling = max(8,(round(p/10,0)))
  
  for (i in 1:50){
    set.seed(i)
    
    print(i)
    x1 <- as.integer(sample(1:(ncol(covars)), sampling, replace=F))
    covars2 = covars[,x1]
    covars2$V1 = data.train$V1
    covars2$V2 = data.train$V2
    covars2$V3 = data.train$V3
    
    data.train2 = cbind(covars2,data.train$firm,data.train$W,data.train$y) # adding id
    names(data.train2)[(ncol(data.train2)-2):(ncol(data.train2))] = c('firm','W','y')
    X = as.matrix(covars2)
    Y = data.train2$y
    W = data.train2$W
    
    Y.forest = regression_forest(X, Y)
    Y.hat = predict(Y.forest)$predictions
    W.forest = regression_forest(X, W)
    W.hat = predict(W.forest)$predictions
    cf = causal_forest(X, Y, W,Y.hat = Y.hat , W.hat = W.hat)
  
    tau.hat = as.data.frame(predict(cf)$predictions)
    tau.hat$firm = data.train$firm
    preds = left_join(preds,tau.hat,by='firm')
  
    ATE[[i]] = average_treatment_effect(cf,target.sample="overlap")[1]
    ATEs[[i]] = average_treatment_effect(cf,target.sample="overlap")[2]
  }
  
  ATE2 = as.data.frame(do.call(rbind, ATE))
  
  ATE2s = as.data.frame(do.call(rbind, ATEs))
  ATs = cbind(ATE2, ATE2s)
  names(ATs) = c('ate','ates')
  
  preds$rowmean = rowMeans(preds[2:ncol(preds)],na.rm=T)
  predictions = left_join(preds[c(1,ncol(preds))],data.train,by='firm')
  
  mod = lm(rowmean ~   . , data=predictions[,c(2,5:(5+(round(p/5,0)-1)))])
  
  coeffs = coeftest(mod)
  est21 = cbind(p,coeffs[2,1],coeffs[2,2])
  est22 = cbind(p,coeffs[3,1],coeffs[3,2])
  est23 = cbind(p,coeffs[4,1],coeffs[4,2])
  vars2 = rbind(est21,est22,est23)

  noloops = rbind(noloops,vars)
  loops = rbind(loops,vars2)

}

noloops$type = 0
loops$type = 1
result = rbind(noloops,loops)

names(result) = c('params','es','se','type')
write.csv(result,'c:/Users/User/OneDrive - University of East Anglia/UEA/RESEARCH/Distributional effects/Australia/Data/simulations/simulations_single_auto4.csv')

result1=result
result1$type=as.factor(result1$type)

pod = position_dodge2(width=10)

My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 12),
  legend.text=element_text(size=14),
  plot.title = element_text(size = 18))
theme_set(theme_minimal(base_size = 20)) 

f<- ggplot(result1, aes(x=params, y=es,color=type)) + 
  geom_point(position = position_dodge2(width=4))+
  geom_errorbar(aes(ymin=es-se, ymax=es+se), position=position_dodge2(30),width=3,size=1) + theme_minimal() +
  scale_colour_grey(labels=c("Single causal forest","Ensemble causal forest"),name="",start = 0.6, end = 0) + 
  theme(legend.position="bottom") +
  xlab('Number of parameters') + ylab('Estimate') + 
  scale_x_continuous(breaks = round(seq(min(result$params), max(result$params), by = 10),1)) + 
  geom_hline(yintercept=1, linetype="dashed", color = "red")
f



