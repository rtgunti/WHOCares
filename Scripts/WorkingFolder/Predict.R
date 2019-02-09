# Title: Multiple linear regression with evaluation and visualisation

##### Activate libraries
library(Hmisc)
library(ggplot2)
library(dplyr)
library(readr)
library(broom)
library(ridge)
library(caret)
#FNDDS <- read.csv("MergedDataSet.csv")
# Read MergedDataSet.csv data set and eliminate missing values
data.all <- FNDDS
data <- data.all %>% filter(Country == 'IND') # This snippet only considers data for India. Change the country here to change the dataset
####### Use the below when using Anamolies############
data.sel <- subset(data, select = c(14,16,18,20,1))  #Anamoly Columns

#######Use the below block when using Absolute values#######
data.sel <- subset(data, select = c(5,8,9,4,1))  #Absolute columns
colnames(data.sel)[1]<-"AN_Temp"
colnames(data.sel)[2]<-"AN_GDP"
colnames(data.sel)[3]<-"AN_CO2"
colnames(data.sel)[4]<-"AN_POP"
#################################

set.seed(2017)
train.size <- 0.8 
train.index <- sample.int(length(data$AN_Temp), round(length(data.sel$AN_Temp) * train.size))
train.sample <- data.sel[train.index,]
valid.sample <- data.sel[-train.index,]

# # M O D E L S # # #

#Linear model using lm()
fit <- lm(AN_Temp ~ ., data=data.sel)
#Find all predicted values for both a training set and a validation set
train.sample$Pred.AN_Temp <- predict(fit, newdata = subset(train.sample, select=c(AN_GDP,AN_CO2,AN_POP,Year)))
valid.sample$Pred.AN_Temp <- predict(fit, newdata = subset(valid.sample, select=c(AN_GDP,AN_CO2,AN_POP,Year)))
# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.AN_Temp, train.sample$AN_Temp), 2)
train.RMSE <- round(sqrt(mean((train.sample$Pred.AN_Temp - train.sample$AN_Temp)^2)),2)
train.MAE <- round(mean(abs(train.sample$Pred.AN_Temp - train.sample$AN_Temp)),2)
vec <- c("R Square", "RMSE", "MAE")
print(vec)
vec <- c(train.corr^2, train.RMSE, train.MAE)
print(vec)
