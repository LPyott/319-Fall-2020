library(car)
options(contrasts = c("contr.sum", "contr.poly"))
setwd("c:/data/csv")
koleva <-read.csv("stb1.csv", header=TRUE)
head(koleva)
bbatch <-as.character(koleva$Batch)
model1<-lm(Potency~Month+bbatch+Month*bbatch, data=koleva)
summary(model1)
names(model1)
anovatable1<-anova(model1)
#anovatableIII<-anova(model1, type="III")
names(anovatable1)
pval1<-anovatable1$`Pr(>F)`[3]
pval1

#Need if/then here
#If pval1<.25 then stop and get expiration date
#if pval>.25 then remove interaction from model
#model 2
model2 <-lm(Potency~Month+bbatch, data=koleva)
summary(model2)
anovatable2<-anova(model2)
anovatable2
pval2<-anovatable2$`Pr(>F)`[2]
pval2

#Need if/then here
#If pval2<.25 then stop and get expiration date
#if pval2>.25 then remove batch from model
#model 2
model3 <-lm(Potency~Month, data=koleva)
summary(model3)
anovatable3<-anova(model3)
anovatable3
#Graph for final model
library(ggplot2)
y_fit_predict<-predict(model3, koleva, interval="predict")
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









