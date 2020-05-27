
#WPower50 R Code
#This code will produce the output required to complete RLAB #1
#You will need to copy and paste from R to a Word document
#You can also choose to use R Markdown

#Set the working directory. This is wherever your data set lives.
#You can also do this using the Session menu above.
#setwd("C:/data/CSV")

#Read in the data set and give it a name with the L arrow
wpower <- read.csv("WPOWER50.csv", header = TRUE)

#What does this data set look like? Get the first 6 obervations with the head function
head(wpower)

#What are the variable names? How are they spelled? Are they capitalized? It matters!!
names(wpower)


#Creating Histogram
#I want a reltive frequency histogram, so set FREQ to FALSE
hist(wpower$AGE,freq=FALSE,xlab='age',ylab='Relative Frequency',col="red", main="Age Distribution")

#Code for calculating all descriptive statistics
#Calculating Sample Mean
y_bar<-mean(wpower$AGE)
y_bar

#Sample Meadian
y_mid<-median(wpower$AGE)
y_mid

#Variance
y_var<-var(wpower$AGE)
y_var

#Sample S.D #
y_sd<-sd(wpower$AGE)
y_sd

#Min,Max value and Range #
y_min<-min(wpower$AGE)
y_min

y_max<-max(wpower$AGE)
y_max

rang<-y_max-y_min
rang

#Sample size
n<-length(wpower$AGE)
n

#Five number summary
summary(wpower$AGE)

#Stem and leaf plot
stem(wpower$AGE)