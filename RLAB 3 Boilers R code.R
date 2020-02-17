#RLAB #3
#Data=Boilers



boilers <- read.csv("BOILERS.CSV")

names(boilers)

#create scatterplots
options(scipen=5)
plot(boilers$Capacity,boilers$ManHours,xlab='Capacity', ylab='Hours', main="Hours vs. Capacity")
plot(boilers$Pressure,boilers$ManHours,xlab='Pressure', ylab='Hours', main="Hours vs. Pressure")

#linear regression analysis
model <- lm(ManHours~Capacity+Pressure, data=boilers)
modelsummary <- summary(model)
modelsummary
confint(model, "Capacity", level=.95)
confint(model, "Pressure", level=.95)
#Model utility
sigma <- modelsummary$sigma
y_bar <- mean(boilers$ManHours)
cv <- (sigma/y_bar)*100
sigma
y_bar
cv

#prediction and estimation
newdata <-data.frame(Capacity=441000, Pressure=410)
predict(model, newdata, interval="prediction")
predict(model, newdata, interval="confidence")

#residual analysis
errors<-residuals(model)
#errors

mean(errors)
hist(errors)
qqnorm(errors)
qqline(errors)
yhat <- fitted(model)
newerdata <- cbind(boilers, errors)
newestdata <- cbind(newerdata, yhat)
plot(newestdata$yhat, newestdata$errors)
abline(h=0, col="blue")