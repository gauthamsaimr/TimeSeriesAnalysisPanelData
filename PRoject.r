library(haven)
library(data.table)
library(ggplot2)
library(broom)
library(forecast)
library(plm)
library(margins)
library(sandwich)
library(lmtest) 
library(tseries)
library(DBI)
library(RSQLite)
library(tidyverse)
library(broom)
library(car)
library(TSA)
library(deeplr)
library(plm)
library(margins)
library(dplyr)
library(partykit)

mydata<- read_dta("Guns.dta")
summary(mydata)
qplot(mydata$incarc_rate, geom = 'histogram', binwidth = 2) + xlab('Incarc rate')
qplot(mydata$density, geom = 'histogram', binwidth = 1) + xlab('density')
## this tells us that incarc_rate and density are highly skewed and we need to make Log transformations on it
corr<-cor(mydata)
#we can see that there is high correaltion between pb1064 and pw1064
library(ggcorrplot)
ggcorrplot(corr)

summary(lm(log(vio)~shall,data = mydata))
coeftest(lm(log(vio)~shall,data=mydata),vcovHC)

# rsquare is just 0.08 which is very low 
#Regression without control variables|^
# we can see that there is 44% reduce in violent crime rates after issuing shall laws 
# now lets see Regression with Control Variables|>

summary(lm(log(vio)~shall+log(incarc_rate)+pop+avginc+log(density)+pb1064+pw1064+pm1029, data=mydata))
coeftest(lm(log(vio)~shall+log(incarc_rate)+pop+avginc+log(density)+pb1064+pw1064+pm1029, data=mydata),vcovHC)

# rsquare is 0.6713
# We can see that still there is a large effect with a small drop in the coefficient, which is 28.26% reduce in violent crime rate after issuing shall laws.

#correlation
cor(mydata$pb1064,mydata$pw1064)
#These are highly correlated (-0.98)

#Pooled regression data frame
gunsvio<-pdata.frame(mydata, index = c("stateid","year"))
#violence as Dependent 
#pooled without Cluster Robust SE
vio_polled<-plm(log(vio)~shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density)+pw1064,data=gunsvio, model="pooling")
summary(vio_polled)
# we see that pb1064 is highly correlated with pw1064 as we saw before so we are removing pw1064
# pooled with cluster Robust and no pb1064
# reason being the OLS estimates of the model are still unbiased and linear but no longer the best and the standard errors are incorrect which makes the confidence intervals and hypothesis tests misleading.  
vio_polled1<-plm(log(vio)~shall+log(incarc_rate)+pm1029+pop+avginc+log(density)+pb1064,data=gunsvio, model="pooling")
summary(vio_polled1)
coeftest(vio_polled1,vcovHC)
##We notice that pw1064 is highly insignificant. 
##we corrected the OLS standard errors, but these estimates are still not the best as the model is inefficient. 
##This could be because of the omitted variable bias. So, we next wanted to implement a fixed effects model which is immune to omitted variable bias from variables that are constant over time and vary between states and not within states. For example, the attitude of the people committing crime or quality of police cannot be quantified using a pooled OLS model where as it won't introduce any bias in a fixed effects model.

#Fixed effects without robust SE
vio_fixed1<-plm(log(vio)~shall+log(incarc_rate)+pop+avginc+log(density)+pw1064+pb1064+pm1029 ,data=gunsvio, model="within")
summary(vio_fixed1)
#we see that avginc is insignificant and we check on Ftest
Hnull <- c("avginc=0")
linearHypothesis(vio_fixed1,Hnull)
#this shows that avginc is insignificant and we can drop 
vio_fixed2<-plm(log(vio)~shall+log(incarc_rate)+pop+log(density)+pw1064++pb1064+pm1029 ,data=gunsvio, model="within")
summary(vio_fixed2)
# do the interpretations 

#We are not planning to use Cluster Robust Standard errors for Entity Fixed effects because fixed effects controls for omitted variable bias because of variables that are constant over time and change with states.
#Still there can be omitted variables which can possibly vary over time but are constant across states. We then implemented used entity fixed and time fixed effects model to address the bias from such omitted variables.

#fixed effects with time
vio_fixed3 <- plm(log(vio)~shall+log(incarc_rate)+pop+avginc+log(density)+pb1064+pm1029+pw1064+factor(year)-1, data=gunsvio, model="within")
summary(vio_fixed3)

Hnull <- c("pb1064=0","pop=0","avginc=0","pw1064=0")
linearHypothesis(vio_fixed3,Hnull)

vio_fixed4 <- plm(log(vio)~log(incarc_rate)+pm1029+log(density)+shall+factor(year)-1, data=gunsvio, model="within")
summary(vio_fixed4)

#{r murder}

mydata<- read_dta("Guns.dta")
summary(mydata)

summary(lm(log(mur)~shall,data = mydata))
coeftest(lm(log(mur)~shall,data=mydata),vcovHC)
# rsquare is just 0.08 which is very low with 47% decrease in violent crime and is highly significant 
summary(lm(log(mur)~shall+log(incarc_rate)+pop+avginc+log(density)+pb1064+pw1064+pm1029, data=mydata))
coeftest(lm(log(mur)~shall+log(incarc_rate)+pop+avginc+log(density)+pb1064+pw1064+pm1029, data=mydata),vcovHC)
# r square is 0.64 and the coefficient has reduced to 21% and still significant 

## pooled ols without cluster Robust SE
gunsmur<-pdata.frame(mydata, index = c("stateid","year"))
mur_pooled<-plm(log(mur)~shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density)+pw1064,data=gunsmur, model="pooling")
summary(mur_pooled)
##We have observed that "pw1064" is insignificant and it might be because of high correlation with "pb1064", which has been observed earlier. Therefore, removing "pw1064" and executing pooled model.

##pooled ols without pw1064
mur_pooled<-plm(log(mur)~shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density),data=gunsmur, model="pooling")
summary(mur_pooled)
# there is a decrease n murder rate

##lets check for CLuster Robust
coeftest(mur_pooled,vcovHC)
## now we see that pb1064 is insignificant 
##Using the robust standard errors, we corrected the OLS standard errors but these estimates are still not the best as the model is inefficient. This could be because of the omitted variable bias. So, we next wanted to implement a fixed effects model which is immune to omitted variable bias from variables that are constant over time and vary between states and not within states

##state fixed entity effect
mur_fixed<-plm(log(mur)~shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density),data=gunsmur, model="within")
summary(mur_fixed)
##Pm1029 and pop are insignificant. 
Hnull <- c("pm1029=0","pop=0")
linearHypothesis(mur_fixed,Hnull)

#drop these 2
mur_fixed1<-plm(log(mur)~shall+log(incarc_rate)+pb1064+avginc+log(density),data=gunsmur, model="within")
summary(mur_fixed1)

#interpret 

## Fixed Time effect 
mur_fixed2<-plm(log(mur)~shall+log(incarc_rate)+pb1064+pm1029+avginc+log(density)+factor(year)-1,data=gunsmur, model="within")
summary(mur_fixed2)
#shall is insignificant 
##We further wanted to address any bias from unobserved omitted variables. So, we decided to try and implement Random effects model. But we saw that the data is not collected using random sampling. So, we should not implement Random effects model. \

#---------------robbery---------

mydata<- read_dta("Guns.dta")
summary(mydata)

summary(lm(log(rob)~shall,data = mydata))
coeftest(lm(log(rob)~shall,data=mydata),vcovHC)
# rsquare is just 0.12 which is very low with 77% decrease in robbery crime and is highly significant 
summary(lm(log(rob)~shall+log(incarc_rate)+pop+avginc+log(density)+pb1064+pw1064+pm1029, data=mydata))
coeftest(lm(log(rob)~shall+log(incarc_rate)+pop+avginc+log(density)+pb1064+pw1064+pm1029, data=mydata),vcovHC)
# rsquare is 0.6899 with a decrease of 41% in robbery crime and is highly significant 

##pooled ols without Robust SE
gunsrob<-pdata.frame(mydata, index = c("stateid","year"))
rob_pooled<-plm(log(rob)~shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density)+pw1064,data=gunsrob, model="pooling")
summary(rob_pooled)

#We have observed that "pw1064" is insignificant and it might be because of high correlation with "pb1064", which has been observed earlier. Therefore, removing "pw1064" and executing pooled model.
rob_pooled1<-plm(log(rob)~shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density),data=gunsrob, model="pooling")
summary(rob_pooled1)

##heterosckedasticity
coeftest(rob_pooled1,vcovHC)
##Only "pb1064" becomes insignificant at p value of 0.1
#Using the robust standard errors, we corrected the OLS standard errors but these estimates are still not the best as the model is inefficient. This could be because of the omitted variable bias. So, we next wanted to implement a fixed effects model which is immune to omitted variable bias from variables that are constant over time and vary between states and not within states. 

#state Fixed entity effect
rob_fixed<-plm(log(rob)~shall+log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density),data=gunsrob, model="within")
summary(rob_fixed)

#after testing for all insignificant variables on Ftest we remove them
rob_fixed1<-plm(log(rob)~shall+log(incarc_rate)+pb1064,data=gunsrob, model="within")
summary(rob_fixed1)
##After controlling for entity fixed effects, the direction of impact of log (incarceration_rate) has changed in comparison to the pooled model. Controlling for omitted variables bias has led to this change. 

##fixed time and entity
rob_fixed2<-plm(log(rob)~shall+log(incarc_rate)+pb1064+pm1029+avginc+log(density)+factor(year)-1,data=gunsrob, model="within")
summary(rob_fixed2)

#shall variable is insignificant



#analysis on murder rate
