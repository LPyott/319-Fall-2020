#This code is for simple graphs of one quantitative variable
#Most graphs are made with base R
#The dotplot is made with ggplot

#Set the working directory to where your data set resides
#setwd("C:/data/CSV/Examples")

#Call the data into R with read.csv
epagas <- read.csv("epagas.csv", header=TRUE)
epagas

#par(mfrow=c(1,1))

#Histogram
hist(epagas$MPG, freq=FALSE, col="red", main="Histogram on MPG", 
     xlab="Miles per gallon", ylab="Relative Frequency")

#Boxplot
boxplot(epagas$MPG, border="black", col="blue", ylab="Miles Per Gallon",
        horizontal=TRUE)

#Stem and leaf plot
#This plot shows up in the console window
stem(epagas$MPG)

#ggplot2 is required to create the dotplot we want
library(ggplot2)

ggplot(data=epagas)+
  geom_dotplot(aes(x=epagas$MPG), color="red", fill="red")+
  theme(plot.title = element_text(hjust = 0.5),
        #panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  ggtitle("Miles per gallon of New Hybrid Vehicle")

#benzene hypothesis test examplesetwd("C:/data/CSV")
setwd("C:/data/CSV")
benzene <- read.csv("benzene.csv", header=TRUE)
benzene
mean <-mean(benzene$benzene)
t.test(benzene$benzene, alternative = "two.sided", mu=1, paired=FALSE, conf.level = .95)
mean

#correlation test with tirewear
tirewear <- read.csv("tirewear.csv", header =TRUE)
cor.test(tirewear$X, tirewear$Y)

#skin cancer mortality SLR
skin <-read.csv("skincancer.csv", header=T)
plot(skin$Lat,skin$Mort)
cor(skin$Lat,skin$Mort)

model <- lm(Mort~Lat, data=skin)
confint(model, 'Lat', level=.95)

summary(model)
summ<-summary(model)
summ
names(summ)
y_mean <-mean(skin$Mort)
rsq <-summary(model)$r.squared
fstat <-summary(model)$fstatistic
sigma <-summary(model)$sigma
cv <- (sigma/y_mean)*100
y_mean
fstat
sigma
cv
names(model)
coef(model)
intslope<-coef(model)

tTest <- t.test(Mort,Lat)

tStat <- summ$coefficients["Lat","t value"] 
pValue <- summ$coefficients["Lat","Pr(>|t|)"]


#need mean of resonse variable to calculate CV
mean(skin$Mort)

#Prediction and Estimation
#Create a new data frame for new observation
newdata<-data.frame(Lat=28)

#Predicted 95% Confidence intervals for mean and individual values#
fitpredicted<-predict(model,newdata, level=.95,interval="prediction")
fitpredicted
fitmean<-predict(model,newdata, level=.95, interval="confidence")
fitmean
