#Chapter 5 code

setwd("c:/data/csv")

#Using linear regression for one qualitative variable
#this is the same as a one-way ANOVA
#Data=maintenance
#three states, comparing cost of maintenance

maintenance <-read.csv("maintenance.csv", header = T)
maintenance

boxplot(maintenance$cost~maintenance$state)
model <-lm(cost~state, data = maintenance)
summary(model)

#Higher order models
#Curvature is evident in scatterplot
#To operate efficiently, power companies must be able 
#to predict the peak power load at their various stations.
#y=daily peak power load during summer months
#x=daily high temperature



powerload <- read.csv("powerload.csv", header=T)
plot(powerload$temp, powerload$load)

#create quadratic and cubic terms for model
temp2<- powerload$temp^2
temp3 <- powerload$temp^3

#model 2 includes x^2 and x^3 terms
model2 <-lm(load~temp+temp2+temp3, data=powerload)
summary(model2)

#cubic term not significant
#remove and rerun with just quadratic term
model3 <-lm(load~temp+temp2, data=powerload)
summary(model2)
