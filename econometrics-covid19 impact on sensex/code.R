library("foreign")
library("dplyr")
library("plm")
library("sandwich")
library("skedastic")
library("lmtest")
library(caret)
install.packages("fastDummies")
library("fastDummies")


cdata=read.csv("Cleaned.csv")
head(cdata)

################################
#Data preparation
################################
colnames(cdata)[7:8] <- c("complete_lock", "partial_lock")
cdata <- dummy_cols(cdata, select_columns = "partial_lock")[-c(8)]




################################
#Running OLS model
################################

summary(ols<-lm(charity~age+income+price+deps+ms, data=cdata))

################################
#Running Fixed effect model
################################

summary(fe<-plm(charity~age+income+price+deps+ms, data=cdata, model="within", index=c("subject", "time")))

## with Robust SE
coeftest(fe, vcov = vcovHC(fe, method = "arellano"))
coeftest(fe, vcov = vcovHC(fe, type = "HC3"))  
################################

################################
#Running Random effect model
################################

summary(re<-plm(charity~age+income+price+deps+ms, data=cdata, model="random", index=c("subject", "time")))

## with Robust SE
coeftest(re, vcov = vcovHC(re, method = "white1"))
coeftest(re, vcov = vcovHC(re, type = "HC1"))  
################################

################################
#Running Hausman test
################################

phtest(fe, re)

# Decision: use fixed effect model
################################