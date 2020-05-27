
#WPower50 today is my birthday
setwd("C:/data/CSV")

wpower <- read.csv("WPOWER50.csv", header = TRUE)


wpower

#names(wpower)


#Creating Histogram

hist(wpower$AGE,freq=FALSE,xlab="age",ylab='Frequency',col="blue")


#Calculating Sample Mean #

y_bar<-mean(wpower$AGE)
y_bar

#Calculating Sample Meadian #

y_mid<-median(wpower$AGE)
y_mid

#Calculating Variance #

y_var<-var(wpower$AGE)
y_var

#Calculating Sample S.D #

y_sd<-sd(wpower$AGE)
y_sd

#Calculating Min,Max value and Range #

y_min<-min(wpower$AGE)
y_min

y_max<-max(wpower$AGE)
y_max

rang<-y_max-y_min
rang

#Calculating the sample size#

n<-length(wpower$AGE)
n


summary(wpower$AGE)
# Getting histogram Components #

#h<-hist(AGE)
#h