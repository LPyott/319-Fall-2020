#The first section of your code should be reading in the libraries
#that will be required to run your code

library(lmtest)
library(car)

#The next section should be reading in the data set, 
#And understanding what the variables look like

#boilers.csv is a data set with one quantitative response vaiable (y=manhours)
#and two quantitative predictors, pressure and capacity
#plus two dummy variables boiler and drum


boiler<-read.table(file.choose(),header=T,sep=',')
attach(boiler)

names(boiler)

#Descriptive statistics
summary(boiler)
V <- var(boiler)
cov2cor(V)
cor(boiler)
cor.test(boiler$ManHours, boiler$Capacity)



#Scatterplots
plot(Capacity, ManHours)
plot(Pressure, ManHours)

#first order model (model1) includes only pressure and capacity
model1<-lm(ManHours~Pressure+Capacity, data=boiler)
summary(model1)

#interaction model (model2) x1, x2, plus interaction
model2<-lm(ManHours~Pressure*Capacity)
summary(model2)

#Residual Analysis
resid1<-residuals(model1)
resid2<-residuals(model2)

hist(resid1,freq=FALSE,xlab="residual",ylab='Frequency',col="blue")
outlierTest(model1)

hist(resid2,freq=FALSE,xlab="residual",ylab='Frequency',col="blue")
outlierTest(model2)
par(mfrow=c(2,2))

#Residual plots 
#1=verus fits, 2=QQ plot, 3=fitted values vs. standardized resid 
# 4=Cooks distances, 5=resid vs leverage, 6=cooks dist vs. leverage 

plot(model1, which = c(4))
plot(model2, which = c(4))
dwtest(model1)
dwtest(model2)
cooks.distance(model1)
cooks.distance(model2)


#Prediction and Estimation
#Create a new data frame for new observation
newdata<-data.frame(Capacity=441000,Pressure=410)

#Predicted 95% Confidence intervals for mean and individual values#
fitpredicted1<-predict(model1,newdata, level=.95,interval="prediction")
fitpredicted1
fitmean1<-predict(model1,newdata, level=.95, interval="confidence")
fitmean1





