#Chapter 4 code
#these examples are in the Chapter 4 powerpoint

setwd("C:/data/CSV")

library(ggplot2)

#**************************************************#

#Fuel consumption
#y=consumption, x1=hourly.temp, x2=chill.index

fuel <-read.csv("fuel consumption.csv", header=TRUE)

plot(fuel$hourly.temp, fuel$consumption, col="blue")
plot(fuel$chill.index, fuel$consumption, col="red")

model1 <-lm(consumption~hourly.temp, data=fuel)
model2 <-lm(consumption~chill.index, data=fuel)

summary(model1)
summary(model2)

plot(fuel$hourly.temp, fuel$chill.index, col="red")

model3 <-lm(consumption~chill.index+hourly.temp, data=fuel)
summary(model3)

newdata <-data.frame(chill.index=10, hourly.temp=40)
predict(model3, newdata=newdata)

#Predicted 95% Confidence intervals for mean and individual values#
fitpredicted<-predict(model3,newdata, level=.95,interval="prediction")
fitpredicted
fitmean<-predict(model3,newdata, level=.95, interval="confidence")
fitmean

#**********************************************************#
#Grandfather clocks
#y=price of clock received at auction
#x1=age of clokc(years)
#x2=number of bidders

clocks <-read.csv("clocks.csv", header=TRUE)
plot(clocks$AGE, clocks$PRICE)
plot(clocks$NUMBIDS, clocks$PRICE)

clockmodel <-lm(PRICE~AGE+NUMBIDS, data=clocks)
summary(clockmodel)


#Predicted 95% Confidence intervals for mean and individual values
newdata <-data.frame(AGE=150,NUMBIDS=10)

fitpredicted<-predict(clockmodel,newdata, level=.95,interval="prediction")
fitpredicted
fitmean<-predict(clockmodel,newdata, level=.95, interval="confidence")
fitmean

#Clock model with interaction term
clockmodelint <-lm(PRICE~AGE+NUMBIDS+AGE*NUMBIDS, data=clocks)
summary(clockmodelint)

#********************************************************#
#Aerobic data set
#y=IGG (measure of immunity)
#x=MAXOXY(maximum oxygen uptake
#model with quadratic term

aerobic <-read.csv("aerobic.csv", header=TRUE)

plot(IGG~MAXOXY, data=aerobic)

#create two models
model_linear <- lm(IGG~MAXOXY, data=aerobic)
summary(model_linear)

MAXOXY2 <-aerobic$MAXOXY*aerobic$MAXOXY
model_quad <- lm(IGG~MAXOXY+MAXOXY2, data=aerobic)
summary(model_quad)

#creates a plot with curved fit
library(ggplot2)
ggplot(aerobic, aes(y=IGG, x=MAXOXY)) +
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))


#***********************************************************#
#Cargo data set
#Qualitative variable with three levels
cargo <- read.csv("cargo.csv", header=T)
model_cargo <- lm(COST~CARGO, data=cargo)
summary(model_cargo)
          