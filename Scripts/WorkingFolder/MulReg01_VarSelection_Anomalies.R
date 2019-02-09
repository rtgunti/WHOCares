# Title: Multiple linear regression with evaluation and visualisation

##### Activate libraries
library(Hmisc)
library(psych)
library(reshape2)
library(ggplot2)
library(dplyr)
library(readr)
library(broom)
library(ridge)
library(caret)
library(caTools)
library(car)
library(carData)
library(tidyr)

# Read MergedDataSet.csv data set and eliminate missing values
setwd("Data sets")
data.all <- read.csv(file = "MergedDataSet.csv", header=TRUE, na.strings="?")
data <- data.all %>% filter(Country == 'CHN')
# head(data)
# summary(data)
# colnames(data)

####### Use the below when using Anamolies############
data.sel <- subset(data, select = c(14,16,18,20))  #Anamoly Columns

#######Use the below block when using Absolute values#######
data.sel <- subset(data, select = c(5,8,9,4))  #Absolute columns
colnames(data.sel)[1]<-"AN_Temp"
colnames(data.sel)[2]<-"AN_GDP"
colnames(data.sel)[3]<-"AN_CO2"
colnames(data.sel)[4]<-"AN_POP"
#################################
head(data.sel)
summary(data.sel)
colnames(data.sel)
plot(data.sel)

##### Analyse variables for
#     - Normality of distribution
#     - Multiple collinearity
#     - Extreme values 
#     - Homoscedasticity (even distribution of residuals)
##### All such problems should have been fixed here

#Visual inspection of vars
pairs.panels(data.sel, col="red")

#Linear Model on combinations of predictors#######
# fit <- lm(AN_Temp ~ AN_POP, data=data.sel)
# summary(fit)
# 
# fit <- lm(AN_Temp ~ AN_CO2, data=data.sel)
# summary(fit)
# 
# fit <- lm(AN_Temp ~ AN_GDP, data=data.sel)
# summary(fit)
# 
# fit <- lm(AN_Temp ~ AN_POP+AN_CO2, data=train.sample)
# summary(fit)
# 
# fit <- lm(AN_Temp ~ AN_POP+AN_GDP, data=train.sample)
# summary(fit)
# 
# fit <- lm(AN_Temp ~ AN_CO2+AN_GDP, data=train.sample)
# summary(fit)
# 
# fit <- lm(AN_Temp ~ ., data=train.sample)
# summary(fit)

##### Develop a linear model
#     The model will be built using the training sample of the data
#     The model will be validated using the validation sample of the data

# Split data into training and validation samples
# We will use (train.size)% for training and (100-train.size)% for validation
set.seed(2017)
train.size <- 0.8 
train.index <- sample.int(length(data$AN_Temp), round(length(data.sel$AN_Temp) * train.size))
train.sample <- data.sel[train.index,]
valid.sample <- data.sel[-train.index,]
head(train.sample)

# Use a stepwise selection of variables by backwards elimination
# consider all candidate variables and eliminate one at the time

# # M O D E L S # # #

#Linear model using lm()
fit <- lm(AN_Temp ~ ., data=data.sel)
#Multiple R-squared:  0.5067,	Adjusted R-squared:  0.4678 
#F-statistic: 13.01 on 3 and 38 DF,  p-value: 5.392e-06
summary(fit)

# K-fold cross-validation

library(DAAG)
fit <- lm(AN_Temp ~ ., data=data.sel,x = TRUE, y = TRUE)
cv.lm(df=data.sel, fit, m=3) # 3 fold cross-validation

# Assessing R2 shrinkage using 4-Fold Cross-Validation 

fit <- lm(AN_Temp ~ ., data=data.sel,x = TRUE, y = TRUE)

library(bootstrap)
# define functions 
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
# matrix of predictors
X <- as.matrix(data.sel[-c(1)])
# vector of predicted values
y <- as.matrix(data.sel[c("AN_Temp")]) 

results <- crossval(X,y,theta.fit,theta.predict,ngroup=4)
cor(y, fit$fitted.values)**2 # raw R2 
cor(y, results$cv.fit)**2 # cross-validated R2

library(lmvar)
fit <- lm(AN_Temp ~ ., data=data.sel,x = TRUE, y = TRUE)
v_data <- cv.lm(fit, k = 4, ks_test = FALSE, fun = NULL, log = FALSE, seed = NULL, max_cores = NULL)
attributes(v_data)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics
plot(fit)

#RidgeRegressionModel
fit <- linearRidge(AN_Temp ~ ., data = train.sample)
summary(fit)

#caret#########
controlPara = trainControl(method="cv",number = 3,savePredictions = TRUE)
fit = train(AN_Temp~.,data=data.sel,method="lm",trControl = controlPara)
summary(fit)

#Back to linear model
fit <- lm(AN_Temp ~ ., data=data.sel)
plot(fit)


# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
av.Plots(fit)

# Influence Plot 
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#Find all predicted values for both a training set and a validation set
train.sample$Pred.AN_Temp <- predict(fit, newdata = subset(train.sample, select=c(AN_GDP,AN_CO2,AN_POP)))
valid.sample$Pred.AN_Temp <- predict(fit, newdata = subset(valid.sample, select=c(AN_GDP,AN_CO2,AN_POP)))
head(train.sample)
colnames(train.sample)
subset(train.sample, select=c("Pred.AN_Temp","AN_Temp"))
summary(fit)

# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.AN_Temp, train.sample$AN_Temp), 2)
train.RMSE <- round(sqrt(mean((train.sample$Pred.AN_Temp - train.sample$AN_Temp)^2)),2)
train.MAE <- round(mean(abs(train.sample$Pred.AN_Temp - train.sample$AN_Temp)),2)
c(train.corr^2, train.RMSE, train.MAE)
# 0.5041 0.2300 0.1800

# Check how good is the model on the validation set - correlation^2, RME ad MAE
valid.corr <- round(cor(valid.sample$Pred.AN_Temp, valid.sample$AN_Temp), 2)
valid.RMSE <- round(sqrt(mean((valid.sample$Pred.AN_Temp - valid.sample$AN_Temp)^2)),2)
valid.MAE <- round(mean(abs(valid.sample$Pred.AN_Temp - valid.sample$AN_Temp)),2)
c(valid.corr^2, valid.RMSE, valid.MAE)
# 0.3136 0.2500 0.1900

####### Measure Accuracy ########
compare <- cbind (actual=train.sample$AN_Temp, train.sample$Pred.AN_Temp) 
mean(apply(compare, 1, min)/apply(compare, 1, max))

compare <- cbind (actual=valid.sample$AN_Temp, valid.sample$Pred.AN_Temp) 
mean(apply(compare, 1, min)/apply(compare, 1, max))

# Results can improved by eliminating extreme values and normalising vars
# Plot component + residual plots
# First check for non-linearity properly if good go further
crPlots(fit, terms = ~.,layout=NULL)

#Eliminate extreme values
#Some extreme values, which could be removed : 
#train.sample[which(rownames(train.sample) %in% c("50", "12", "24")),]
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
summary(fit)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("50", "51", "53")),]     
print(fit)
### Refit the model (2)
fit <- lm(AN_Temp ~ ., data=train.sample)
summary(fit)
head(data.sel)
#Multiple R-squared:  0.4275,	Adjusted R-squared:  0.3784 
#F-statistic: 8.711 on 3 and 35 DF,  p-value: 0.0001883

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("1", "43", "49")),]     

### Refit the model (3)
fit <- lm(AN_Temp ~ ., data=train.sample)
summary(fit)
#Multiple R-squared:  0.1506,	Adjusted R-squared:  0.1472 
#F-statistic: 44.85 on 3 and 759 DF,  p-value: < 2.2e-16

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("24", "48", "7")),]     

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)                        # We should continue checking Cook!

# Check for multi-collinearity with Variance Inflation Factor
# Correlated: none VIF=1, moderately 1<VIF<5, ** highly 5<VIF<10, ...
vif(fit)

#Broom : To save the summary of model to dataframe (for further use)
tidy(fit)
summary(fit)
glance(fit)
augmented_mod <- augment(fit)
augmented_mod %>% 
  ggplot(aes(AN_POP, AN_Temp)) + 
  geom_point(aes(color = .resid)) +
  geom_abline(intercept = fit$coefficients["(Intercept)"], 
              slope = fit$coefficients["Population"], size = 0.5) +
  scale_color_distiller(palette = "RdBu") +
  labs(x = "AN_POP", y = "AN_TMP")

# Stepwise Regression
# Lower the AIC, better the model 
library(MASS)
fit <- lm(AN_Temp ~ .,data=data.sel)
step <- stepAIC(fit, direction="both")
step$anova # display results

# Small data set - Cross-validation should be used, but vars selection needs to be auto!