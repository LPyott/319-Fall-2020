#Module 4/Chapter 8 code
#Residual analysis
#Detecting a misspecified model
#FInding outliers and influential values

setwd("c:/data/CSV")
electric <-read.csv("electric.csv", header=TRUE)
olympic <-read.csv("olympic.csv", header=TRUE)


options(scipen=5)
#misspecified model
modelelec <-lm(USAGE~SIZE, data=electric)
summary(modelelec)
resid<-residuals.lm(modelelec)
predicteds <-predict(modelelec)
data <-cbind(predicteds, resid)
data2 <-cbind(electric, data)
plot(data2$SIZE, data2$resid, main="Size (x) versus Predicted Values")
abline(h=0)


modelelecsq <-lm(USAGE~SIZE+SIZESQ, data=electric)
summary(modelelecsq)
resid2<-residuals.lm(modelelecsq)
predicteds2 <-predict(modelelecsq)
data3 <-cbind(predicteds, resid)
data4 <-cbind(electric, data3)
plot(data4$SIZE, data4$resid, main="Size (x) versus Predicted Values")
abline(h=0)

#Detecting unequal variances
salary <-read.csv("salary.csv", header=TRUE)
plot(salary$EXP, salary$SALARY)
EXP2<-salary$EXP*salary$EXP
model3 <-lm(SALARY~EXP+EXP2, data=salary)
summary(model3)

logsalary <-log(salary$SALARY)
newdata <-cbind(logsalary, salary)
newdata <-cbind(EXP2, newdata)
model4 <-lm(logsalary~EXP+EXP2, data=newdata)
summary(model4)

model5 <-lm(logsalary~EXP, data=newdata)
summary(model5)

#detecting outliers
install.packages("MASS")
library("MASS")
clocks2 <-read.csv("gfclocks2.csv", header=TRUE)
model6 <- lm(PRICE~AGE+NUMBIDS+AGE*NUMBIDS, data=clocks2)
summary(model6)
clockresid <-residuals(model6)
clockresid
zresid <-stdres(model6)
newdata <-cbind(clocks2, clockresid)
newdata <-cbind(newdata, zresid)
newdata

plot(model6)
plot(fit, pch=18, col='red', which=c(4))

#leverage and cooks
fastfood <-read.csv("fastfood.csv", header=TRUE)
CITYDUMMY <-as.character(fastfood$CITY)
fastfood <-cbind(CITYDUMMY, fastfood)
model7 <-lm(SALES~CITYDUMMY+TRAFFIC, data=fastfood)
summary(model7)
resanalysis<-influence.measures(model7, infl=influence(model7))
resanalysis
rstandard(model7)
cooks.distance(model7)
hatvalues(model7)