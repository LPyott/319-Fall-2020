#RLAB #1 
#Data=poverty.csv
#install.packages(ggplot2)
library(ggplot2)

setwd("C:/data/CSV")

poverty<-read.csv("poverty.csv", header=T)

names(poverty)

#Creating a Scatter Plot 
plot(TeenBrth~PovPct, data=poverty, main="Scatter Plot",col='blue')

#Calculating the linear Correlation Coefficient (r)
r<-cor(poverty$PovPct,poverty$TeenBrth)
r
#Mean of dependent variable for CV calculation
y_bar <- mean(poverty$TeenBrth)
y_bar

# Calculating the least Squares Model
fit<-lm(TeenBrth~PovPct, data=poverty)
fit
summary(fit)

#Confidence interval for slope
confint(fit, 'PovPct', level=.95)

# Estimating the teen birth rate when poverty pct is 12%
# Calculating Prediction and Confience Intervals
newdata<-data.frame(PovPct=12)
y_fit_predict_12<-predict(fit, newdata, interval="predict")
y_fit_confidence<-predict(fit, newdata, interval="confidence")
y_fit_predict_12
y_fit_confidence


#attach prediction interval values for all observations to our data set
y_fit_predict<-predict(fit, poverty, interval="predict")
my_data<-cbind(poverty,y_fit_predict)

#Scatterplot with fitted line and two sets of confidence bands
ggplot(my_data,aes(PovPct,TeenBrth))+
  geom_point()+
  geom_smooth(method=lm)+
  geom_line(aes(y=lwr), color="red", linetype="dashed")+
  geom_line(aes(y=upr), color="red", linetype="dashed")






