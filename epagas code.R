#This code is for simple graphs of one quantitative variable
#Most graphs are made with base R
#The dotplot is made with ggplot

#Set the working directory to where your data set resides
setwd("C:/data/CSV/Examples")

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

