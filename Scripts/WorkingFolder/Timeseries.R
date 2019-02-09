library(tseries)
library(forecast)
library(dplyr)
library(readr)
data <- read.csv("ReportScripts/GlobalLandTemperaturesByCountry.csv") # considering the base dataset
Ind = data %>% filter(data$Country == "India") # filtering the dataset for values for India
Ind = Ind %>% filter(format(Ind[,1],format="%Y")>=1960) # Considering data only from 1960 onwards
Ind = Ind %>% filter(format(Ind[,1],format="%Y")<2013) # Consider only data that is used in our final dataset
library(ggplot2)

#Dropping some columns and formatting some columns to be able to use in time series
Ind$AverageTemperatureUncertainty = NULL
Ind$Country = NULL
Ind$dt = as.Date(Ind$dt)
Ind1 = ts(Ind$AverageTemperature, frequency = 12, start = c(1960, 1))
plot(Ind1) # plotting the time series (data)

ggseasonplot(Ind1)
pp = decompose(Ind1)
plot(pp)

# Augmented Dickey-Fuller Test
adf.test(Ind1,alternative = "stationary")

# Augmented Dickey-Fuller Test
# 
# data:  Ind$AverageTemperature
# Dickey-Fuller = -5.9488, Lag order = 11, p-value = 0.01
# alternative hypothesis: stationary
#help("adf.test")
#bline(reg = lm(Ind1~time(Ind1)))

#NDIFFS
ndiffs(Ind1)

# ARIMA FIT
fit = auto.arima(Ind1,seasonal = TRUE)
summary(fit)
fore = forecast(fit,n=36)
autoplot(fore)

