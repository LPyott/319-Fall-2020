library(car)

setwd("c:/data/csv")
stb1 <-read.csv("stb1.csv", header=TRUE)
head(stb1)

bbatch <-as.character(stb1$Batch)

model1<-lm(Potency~Month+bbatch+Month*bbatch, data=stb1)
summary(model1)
anovatable1<-anova(model1)
pval1<-anovatable1$`Pr(>F)`[3]
pval1


model2 <-lm(Potency~Month+bbatch, data=stb1)
summary(model2)
anovatable2<-anova(model2)
anovatable2
pval2<-anovatable2$`Pr(>F)`[2]
pval2


model3 <-lm(Potency~Month, data=stb1)
summary(model3)
anovatable3<-anova(model3)
anovatable3
#Graph for final model
library(ggplot2)
y_fit_predict<-predict(model3, stb1, interval="predict")
my_data<-cbind(koleva,y_fit_predict)
ggplot(my_data,aes(Month,Potency))+
  geom_point()+
  geom_smooth(method=lm)+
  geom_smooth(aes(y=lwr), color="red", linetype="dashed", se=FALSE)+
  geom_smooth(aes(y=upr), color="red", linetype="dashed", se=FALSE)+
  geom_hline(yintercept=95, linetype="dashed")+
  theme(legend.position="right",
      plot.title = element_text(hjust = 0.5)) +
  ggtitle("Expiration Date")

#grid.locator(unit="native")

#attach prediction interval values for all observations to our data set

#https://stackoverflow.com/questions/34248347/r-locate-intersection-of-two-curves
#cross <-est[which.min(abs(1-est$fitline)),] 
#Scatterplot with fitted line and two sets of confidence bands









