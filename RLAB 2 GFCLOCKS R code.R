#RLAB 3
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

model2 <-



#Predicted 95% Confidence intervals for mean and individual values
newdata <-data.frame(AGE=160,NUMBIDS=11)

#STOP!!!!

#Don't run the code below until you choose your model
#Make sure you enter your chosen model in place of ???
fitpredicted<-predict(model???,newdata, level=.95,interval="prediction")
fitpredicted
fitmean<-predict(model???,newdata, level=.95, interval="confidence")
fitmean
 
#Scatteplots for each predictor vs. price
#the graphs will include the regression line
plot(clocks$AGE, clocks$PRICE, xlab="Age", ylab="Price")
abline(lm(PRICE~AGE, data=clocks), col="red")
plot(clocks$NUMBIDS, clocks$PRICE, xlab="Number of Bidders", ylab="Price")
abline(lm(PRICE~NUMBIDS, data=clocks), col="green")