# Title: Multiple linear regression with evaluation and visualisation

##### Activate libraries
# Read MergedDataSet.csv data set and eliminate missing values
#setwd("Data sets")
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

##### Develop a linear model
#     The model will be built using the training sample of the data
#     The model will be validated using the validation sample of the data

# Split data into training and validation samples
# We will use (train.size)% for training and (100-train.size)% for validation
#head(train.sample)

# # M O D E L S # # #

#Linear model using lm()
fit <- lm(AN_Temp ~ ., data=data.sel)
summary(fit)

# coefficients(fit) # model coefficients
# confint(fit, level=0.95) # CIs for model parameters 
# fitted(fit) # predicted values
# residuals(fit) # residuals
# anova(fit) # anova table 
# vcov(fit) # covariance matrix for model parameters 
# influence(fit) # regression diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)