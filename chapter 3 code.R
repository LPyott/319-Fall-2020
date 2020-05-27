#correlation test with tirewear
tirewear <- read.csv("tirewear.csv", header =TRUE)
cor(tirewear$X, tirewear$Y)
plot(tirewear$X, tirewear$Y)

#skin cancer mortality scatterplot and SLR
skin <-read.csv("skincancer.csv", header=T)
plot(skin$Mort, skin$Lat)
cor(skin$Mort, skin$Lat)

model <- lm(Mort~Lat, data=skin)
modelsummary <- summary(model)
modelsummary
names(modelsummary)

#need mean of resonse variable to calculate CV
y_mean <- mean(skin$Mort)
sigma <-modelsummary$sigma
cv <- (sigma/y_mean)*100
#Prediction and Estimation
#Create a new data frame for new observation
newdata<-data.frame(Lat=40, Mort=NA, Ocean=NA, Long=NA, State=NA)
newdata

#95% Confidence intervals for mean and individual values at latitude=40
fitpredicted<-predict(model,newdata, level=.95,interval="prediction")
fitpredicted
fitmean<-predict(model,newdata, level=.95, interval="confidence")
fitmean

#this is for the graph only!
allpredicteds <-predict(model, skin, level=.95, interval="prediction")
mydata <- cbind(skin, allpredicteds)

#scatterplot with fitted lines and confidence bands
ggplot(mydata, aes(Lat,Mort))+
  geom_point()+
  stat_smooth(method=lm)+
  geom_line(aes(y=lwr), color="red", linetype="dashed")+
  geom_line(aes(y=upr), color="red", linetype="dashed")