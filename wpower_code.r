#************************************#
#           Assignment # I           #
#           Powerful Women           #
#************************************#

setwd("C:/data/CSV")
wpower <- read.csv("WPOWER50.csv", header = TRUE)


#wpower<-read.table(file.choose(),header=T,sep=',')
#attach(wpower)
wpower
names(wpower)
age <-wpower$AGE

#Creating Histogram

hist(age,freq=FALSE,xlab="age",ylab='Frequency',col="blue")
#curve(dnorm(x,mean=mean(AGE),sd=sd(AGE)),add=TRUE,col='red')

#Calculating Sample Mean #

y_bar<-mean(AGE)
y_bar

#Calculating Sample Meadian #

y_mid<-median(AGE)
y_mid

#Calculating Variance #

y_var<-var(AGE)
y_var

#Calculating Sample S.D #

y_sd<-sd(AGE)
y_sd

#Calculating Min,Max value and Range #

y_min<-min(AGE)
y_min

y_max<-max(AGE)
y_max

rang<-y_max-y_min
rang

#Calculating the sample size#

n<-length(AGE)
n

# Getting histogram Components #

#h<-hist(AGE)
#h