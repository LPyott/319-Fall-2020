#RLAB 2
  #Grandfather clocks
  #y=price of clock received at auction
  #x1=age of clokc(years)
  #x2=number of bidders

#SETUP
#Set working directory, read in data set, 
#and look at the first six observations
setwd("c:/data/csv")
clocks <-read.csv("clocks.csv", header=TRUE)
head(clocks)

#Needed to calculate the CV
y_bar<-mean(clocks$PRICE)
y_bar

#Model 1
#Use age to predict price

model1 <-lm(PRICE~AGE, data=clocks)
summary(model1)
confint(model1, "AGE", level=.95)

#Model 2
#YOU CAN DO THIS!!!
#I BELIEVE IN YOU!!!
#Copy and paste above code, and change variable names 
#where necessary
model2 <-lm(PRICE~NUMBIDS, data=clocks)
summary(model2)
confint(model2, "NUMBIDS", level=.95)


#Predicted 95% Confidence intervals for mean and individual values
newdata <-data.frame(AGE=160,NUMBIDS=11)

#STOP!!!!

#Don't run the code below until you choose your model
#Make sure you enter your chosen model in place of ???
fitpredicted<-predict(model1,newdata, level=.95,interval="prediction")
fitpredicted
fitmean<-predict(model1,newdata, level=.95, interval="confidence")
fitmean

#Scatteplots for each predictor vs. price
#the graphs will include the regression line
plot(clocks$AGE, clocks$PRICE, xlab="Age", ylab="Price")
abline(lm(PRICE~AGE, data=clocks), col="red")
plot(clocks$NUMBIDS, clocks$PRICE, xlab="Number of Bidders", ylab="Price")
abline(lm(PRICE~NUMBIDS, data=clocks), col="green")


#REsidual analysis for model 1
#Are the errors normally distributed with mean=0?
#Do they have constant variance?

errors1<-residuals(model1)
errors1

mean(errors1)

hist(errors1)

qqnorm(errors1)
qqline(errors1)

yhat <- fitted(model1)
yhat

newerdata <- cbind(clocks, errors1)

newestdata <- cbind(newerdata, yhat)
#plot()
plot1 <-plot(newestdata$yhat, newestdata$errors)
plot1
abline(h=0)

#Model 2
errors2<-residuals(model2)

mean(errors2)

hist(errors2)
qqnorm(errors2)
qqline(errors2)

yhat <- fitted(model2)

newerdata <- cbind(clocks, errors2)

newestdata <- cbind(newerdata, yhat)

plot2 <-plot(newestdata$yhat, newestdata$errors)
plot2

#Model 3
#Multiple linear regression 

model3 <-lm(PRICE~AGE+NUMBIDS, data=clocks)
summary(model3)

errors3<-residuals(model3)
#errors

mean(errors3)

hist(errors3)

qqnorm(errors3)
qqline(errors3)

yhat <- fitted(model3)

newerdata <- cbind(clocks, errors3)

newestdata <- cbind(newerdata, yhat)

plot(newestdata$yhat, newestdata$errors)
abline(h=0, col="blue")
