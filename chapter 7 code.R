#Chapter 7 Pitfalls

setwd("c:/data/csv")

#Library "car"---Companion to Applied Regression
#Contains the Variation Inflation Factor function
library(car)

#Data FTC 
#y=carbon monoxide content
#x1=tar
#x2=nicotine
#x3=weight

ftc <-read.csv("ftc.csv", header =T)

head(ftc)

#Multicollinearity
#scatterplots of three independent variables
#with y all show positive relationship

plot(ftc$TAR, ftc$CO)
plot(ftc$NICOTINE, ftc$CO)
plot(ftc$WEIGHT, ftc$CO)

#first-order multiple linear regression model with all 3 variables
model <- lm(CO~TAR+NICOTINE+WEIGHT, data=ftc)
summary(model)

#correlation matrix
cor(ftc)

#Variation Inflation Factors
vif(model)
