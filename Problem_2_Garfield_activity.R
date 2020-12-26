rm(list=ls())
#Loading the Libraries

library('rpart')
library('rpart.plot')
library('caTools')
library('ROCR')
library('rattle')
library('RColorBrewer')
library('data.table')
library('ineq')
library(corrplot)
library(caret)

#Input Data
setwd("C:/Users/ahmad/Desktop/Data Science role GBT")
Garfield_Activity <- read.csv("problem_2_garfield_activity.csv",header=TRUE,stringsAsFactors=T)
str(Garfield_Activity)

#Define some dummies
levels(Garfield_Activity$X8AM)
Garfield_Activity$Morning_Coffee<-ifelse(Garfield_Activity$X8AM=="Coffee",1,0)
Garfield_Activity$Morning_Doughnut<-ifelse(Garfield_Activity$X8AM=="Doughnut",1,0)
Garfield_Activity$Morning_Sandwich<-ifelse(Garfield_Activity$X8AM=="Sandwich",1,0)
Garfield_Activity$Morning_9AM_Coffee<-Garfield_Activity$X9AM*Garfield_Activity$Morning_Coffee
Garfield_Activity$Morning_9AM_Doughnut<-Garfield_Activity$X9AM*Garfield_Activity$Morning_Doughnut
Garfield_Activity$Morning_9AM_Sandwich<-Garfield_Activity$X9AM*Garfield_Activity$Morning_Sandwich
Garfield_Activity$Morning_10AM_Coffee<-Garfield_Activity$X10AM*Garfield_Activity$Morning_Coffee
Garfield_Activity$Morning_10AM_Doughnut<-Garfield_Activity$X10AM*Garfield_Activity$Morning_Doughnut
Garfield_Activity$Morning_10AM_Sandwich<-Garfield_Activity$X10AM*Garfield_Activity$Morning_Sandwich
Garfield_Activity$Morning_11AM_Coffee<-Garfield_Activity$X11AM*Garfield_Activity$Morning_Coffee
Garfield_Activity$Morning_11AM_Doughnut<-Garfield_Activity$X11AM*Garfield_Activity$Morning_Doughnut
Garfield_Activity$Morning_11AM_Sandwich<-Garfield_Activity$X11AM*Garfield_Activity$Morning_Sandwich

levels(Garfield_Activity$Noon)
Garfield_Activity$Noon_Lenthils<-ifelse(Garfield_Activity$Noon=="Lenthils",1,0)
Garfield_Activity$Noon_Taco<-ifelse(Garfield_Activity$Noon=="Taco",1,0)
Garfield_Activity$Noon_Sandwich<-ifelse(Garfield_Activity$Noon=="Sandwich",1,0)
Garfield_Activity$Noon_1PM_Lenthils<-Garfield_Activity$X1PM*Garfield_Activity$Noon_Lenthils
Garfield_Activity$Noon_1PM_Taco<-Garfield_Activity$X1PM*Garfield_Activity$Noon_Taco
Garfield_Activity$Noon_1PM_Sandwich<-Garfield_Activity$X1PM*Garfield_Activity$Noon_Sandwich
Garfield_Activity$Noon_2PM_Lenthils<-Garfield_Activity$X2PM*Garfield_Activity$Noon_Lenthils
Garfield_Activity$Noon_2PM_Taco<-Garfield_Activity$X2PM*Garfield_Activity$Noon_Taco
Garfield_Activity$Noon_2PM_Sandwich<-Garfield_Activity$X2PM*Garfield_Activity$Noon_Sandwich
Garfield_Activity$Noon_3PM_Lenthils<-Garfield_Activity$X3PM*Garfield_Activity$Noon_Lenthils
Garfield_Activity$Noon_3PM_Taco<-Garfield_Activity$X3PM*Garfield_Activity$Noon_Taco
Garfield_Activity$Noon_3PM_Sandwich<-Garfield_Activity$X3PM*Garfield_Activity$Noon_Sandwich

levels(Garfield_Activity$X4PM)
Garfield_Activity$Evening_Coffee<-ifelse(Garfield_Activity$X4PM=="Coffee",1,0)
Garfield_Activity$Evening_PingPong<-ifelse(Garfield_Activity$X4PM=="PingPong",1,0)
Garfield_Activity$Evening_Tea<-ifelse(Garfield_Activity$X4PM=="Tea",1,0)
Garfield_Activity$Evening_Workout<-ifelse(Garfield_Activity$X4PM=="Workout",1,0)
Garfield_Activity$Evening_5PM_Coffee<-Garfield_Activity$X5PM*Garfield_Activity$Evening_Coffee
Garfield_Activity$Evening_5PM_PingPong<-Garfield_Activity$X5PM*Garfield_Activity$Evening_PingPong
Garfield_Activity$Evening_5PM_Tea<-Garfield_Activity$X5PM*Garfield_Activity$Evening_Tea
Garfield_Activity$Evening_5PM_Workout<-Garfield_Activity$X5PM*Garfield_Activity$Evening_Workout

levels(Garfield_Activity$Commute)
Garfield_Activity$Long_Commute<-ifelse(Garfield_Activity$Commute=="Long",1,0)

levels(Garfield_Activity$DayOfWeek)
Garfield_Activity$DayOfWeek_Fri<-ifelse(Garfield_Activity$DayOfWeek=="Fri",1,0)
Garfield_Activity$DayOfWeek_Mon<-ifelse(Garfield_Activity$DayOfWeek=="Mon",1,0)
Garfield_Activity$DayOfWeek_Sat<-ifelse(Garfield_Activity$DayOfWeek=="Sat",1,0)
Garfield_Activity$DayOfWeek_Thu<-ifelse(Garfield_Activity$DayOfWeek=="Tue",1,0)
Garfield_Activity$DayOfWeek_Wed<-ifelse(Garfield_Activity$DayOfWeek=="Wed",1,0)

levels(Garfield_Activity$WatchTV)
Garfield_Activity$WatchedTV<-ifelse(Garfield_Activity$WatchTV=="Yes",1,0)

str(Garfield_Activity)



#Check correlation among IV
Garfield_Activity.Corr<-subset(Garfield_Activity[,c(17:25,28:32,42:52)])
str(Garfield_Activity.Corr)
cor(Garfield_Activity.Corr)
correlations<- cor(Garfield_Activity.Corr)
corrplot(correlations, method="circle")

#Dividing Data in Training and testing data set optional as the data set is not big
Garfield_Activity.req<-subset(Garfield_Activity[,c(17:52)])
set.seed(1234)
pd<-sample(2, nrow(Garfield_Activity.req), replace = TRUE, prob = c(0.7,0.3))
train<-Garfield_Activity.req[pd==1,]
val<-Garfield_Activity.req[pd==2,]
str(train)
str(Garfield_Activity)

#Creating Logistic Regression model
Logit.1<-WatchedTV~Morning_9AM_Coffee+Morning_9AM_Doughnut+Morning_9AM_Sandwich+Morning_10AM_Coffee+Morning_10AM_Sandwich+Morning_10AM_Sandwich+Morning_11AM_Sandwich+Morning_11AM_Coffee+Morning_11AM_Doughnut+Noon_2PM_Sandwich+Noon_2PM_Taco+Noon_3PM_Sandwich+Noon_3PM_Taco+Noon_3PM_Lenthils+Noon_1PM_Sandwich+Noon_1PM_Taco+Noon_1PM_Lenthils+Evening_5PM_Workout+Evening_5PM_Tea+Evening_5PM_PingPong+Evening_5PM_Coffee+Long_Commute+DayOfWeek_Wed+DayOfWeek_Thu+DayOfWeek_Sat+DayOfWeek_Mon+DayOfWeek_Fri
Logit.plot<-glm(Logit.1, data = train, family = binomial())
summary(Logit.plot)

#Creating more accurate Logistic Regression model after removing corelated variables
Logit.2<-WatchedTV~Morning_9AM_Coffee+Morning_10AM_Coffee+Morning_10AM_Sandwich+Morning_11AM_Sandwich+Noon_2PM_Sandwich+Noon_2PM_Taco+Noon_3PM_Sandwich+Noon_3PM_Taco+Noon_3PM_Lenthils+Noon_1PM_Sandwich+Noon_1PM_Taco+Noon_1PM_Lenthils+Evening_5PM_Workout+Evening_5PM_Tea+Evening_5PM_PingPong+Evening_5PM_Coffee+Long_Commute
Logit.plot<-glm(Logit.2, data = train, family = binomial())
summary(Logit.plot)

pred.logit.plot<-predict.glm(Logit.plot, newdata = val, type = "response")

pred.logit.plot

#Classification
tab.logit<-table(val$WatchedTV, pred.logit.plot)
tab.logit

