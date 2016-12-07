list.files()
fly.data <- read.csv("FlySelfMedication.csv")
str(fly.data)

#finding the means, standard deviation, and n
mean<-aggregate(fly.data$proportionEggsEthanol,by=list(fly.data$waspTreatment),FUN=mean)
sd<-aggregate(fly.data$proportionEggsEthanol,by=list(fly.data$waspTreatment),FUN=sd)
n<-aggregate(fly.data$proportionEggsEthanol,by=list(fly.data$waspTreatment),FUN=length)

#putting these new values into a data frame
table<-cbind(mean,sd[2],n[2])

#remaning the collumns of the table to identify what ech value is
colnames(table)<-c("Treatment","mean","sd","n")
str(table)

#transform the data to make it more normal
arcsin.data <- asin(sqrt(fly.data$proportionEggsEthanol))

#refind the mean, standard deviation and n for the this transformed data
mean.t<-aggregate(arcsin.data,by=list(fly.data$waspTreatment),FUN=mean)
sd.t<-aggregate(arcsin.data,by=list(fly.data$waspTreatment),FUN=sd)
n.t<-aggregate(arcsin.data,by=list(fly.data$waspTreatment),FUN=length)

#create a new data frame
trans.table<-cbind(mean.t,sd.t[2],n.t[2])

#again rename the collumns 
colnames(trans.table)<-c("Treatment","mean","sd","n")

#ANOVA TEST

#the right way to do it after a few trials
fly.lm.t <- lm(arcsin.data~ fly.data$waspTreatment) #lm aov both run the test
plot(fly.lm.t)
summary(fly.lm.t) #get more details on results of test
sum(fly.lm.t$residuals) #theoretically this should be 1...

#the same thing with a different function (aov)
fly.aov.t <- aov(arcsin.data~ fly.data$waspTreatment)
plot(fly.aov.t)
summary(fly.aov.t)

#comparing coefitients and transformed means
fly.lm.t$coefficients
fly.aov.t$coefficients
mean.t

#adding residuals 
sum(fly.aov.t$residuals)
sum(fly.lm.t$residuals)


