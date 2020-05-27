#This code is for simple graphs of one quantitative variable
#The examples from Chapter 1 powerpoints are in this file

#Most graphs are made with base R
#The dotplot is made with ggplot

#Set the working directory to where your data set resides

setwd("C:/data/CSV")

library(ggplot2)

#Read the data into R with read.csv
epagas <- read.csv("EPAGAS.csv", header=TRUE)
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
ggplot(data=epagas)+
  geom_dotplot(aes(x=MPG), color="red", fill="red")+
  theme(plot.title = element_text(hjust = 0.5),
        #panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  ggtitle("Miles per gallon of New Hybrid Vehicle")

#data=benzene 
#hypothesis test example
#one sample t-test 
#Test the null hypothesis that the mean=1
#PLus 95% confidence interval


benzene <- read.csv("benzene.csv", header=TRUE)
benzene
mean <-mean(benzene$benzene)
t.test(benzene$benzene, alternative = "two.sided", mu=1, paired=FALSE, conf.level = .95)
mean


