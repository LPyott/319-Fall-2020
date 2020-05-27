#Module 2/Chapter 6 Variable Screening Processes

mtcars

#10 independent variables to estimate MPG
#mtcars data comes from the 1974 Motor Trend magazine. 
#The data includes fuel consumption data, 
#and ten aspects of car design for then-current car models.
#This data set lives inside R


#Fit complete first-order model, including all variables 
fitall <-lm(mpg~., data=mtcars)
summary(fitall)

#Backward stepwise regression
#calls complete, first-order model
step(fitall, direction = "backward")


#Forward stepwise regression
fitstart <- lm(mpg~1, data=mtcars)
step(fitstart, direction = "forward", scope=formula(fitall))


