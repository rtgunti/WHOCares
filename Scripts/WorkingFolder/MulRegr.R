library(reshape2)
library(ggplot2)
library(dplyr)
library(readr)

AllData <- read.csv("FinalDataSet.csv")
AllData$MinTemperature <- NULL
AllData$MaxTemperature <- NULL
AllData$Country.Name <- NULL
AllData_IND <- AllData %>% filter(Country == 'IND')
head(AllData)

MulRegMod <- lm(AverageTemperature ~ GDP + Population + Emissions, data=AllData_IND)
#Error_Summary[match((Country1),Country_List)]<-summary(linearMod)
print(summary(MulRegMod))
print(MulRegMod$coefficients)
#GDP - 184000000000
#Population - 696783517
#Emission - 0.450665384
pop_test = 696783517
gdp_test = 184000000000
emission_test = 0.450665384
temp_pred = MulRegMod$coefficients[1]+MulRegMod$coefficients[2]*gdp_test + MulRegMod$coefficients[3]*pop_test +MulRegMod$coefficients[4]*emission_test
#print(temp_pred)