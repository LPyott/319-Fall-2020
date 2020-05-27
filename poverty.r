#************************************#
#           Assignment # I           #
#           Powerful Women           #
#************************************#

poverty<-read.table(file.choose(),header=T,sep=',')
attach(poverty)
poverty
names(poverty)

# Creating a Scatter Plot #

plot(TeenBrth~PovPct,main="Scatter Plot",col='blue')
abline(0,0)

# Calculating the linear Correlation Coefficient (r) #

r<-cor(PovPct,TeenBrth)
r

# Calculating the least Square Model #

fit<-lm(TeenBrth~PovPct)
summary(fit)
anova(fit)           #Only to obtain Anova Table

# Estimating the teen birth rate when poverty pct is 12% #
# Calculating Prediction Interval #

newdata<-data.frame(PovPct=12)
y_fit2<-predict(fit, newdata, interval="predict")
y_fit2 

my_data<-cbind(poverty,y_fit2)

#By default R only give you 95% confidence Interval#

##########################################

library("ggplot2")
p<-ggplot(my_data,aes(PovPct,TeenBrth))+geom_point()+
geom_smooth(method=lm)






