library(reshape2)
library(ggplot2)
library(dplyr)
library(readr)

# Reading the dataset into a frame
PopulationDataset_02_Comments_Removed <- read_csv("PopulationDataset_Raw.csv")

#Deleting irrelevant columns
PopulationDataset_02_Comments_Removed$`Country Name` <- NULL
PopulationDataset_02_Comments_Removed$`Indicator Name` <- NULL
PopulationDataset_02_Comments_Removed$`Indicator Code` <- NULL
PopulationDataset_02_Comments_Removed <- PopulationDataset_02_Comments_Removed[!(PopulationDataset_02_Comments_Removed$`Country Code`=="INX"),]

#Melting the dataset here to the required format
Population_Data <- melt(PopulationDataset_02_Comments_Removed, id=c("Country Code"))
Population_Data$Year <- as.numeric(as.character(Population_Data$Year))

#Rename Columns per this Project Standards
colnames(Population_Data)[1] = "Country"
colnames(Population_Data)[2] = "Year"
colnames(Population_Data)[3] = "Population"
Population_Data <- Population_Data[order(Population_Data$Country),]
#Population_Data[is.na(Population_Data)] <- 0

Country_List <- unique(Population_Data$Country)
#Country_List <- c("ERI")

Population_final = Population_Data[FALSE,]
Countries_Ommitted <- c("INX")
o = 1 # counting the number of countries omitted

# FOR LOOP To loop through all the countries
# Check if any country has more than 25% missing values -> ignore the country
# If missing values < 25%, impute missing values using linear regression model
for (Country1 in Country_List) 
{
  cat("Current Country is ", Country1)
  Population_Country <- Population_Data %>% filter(Country == Country1)
  if(sum(is.na(Population_Country$Year)) > ncol(Population_Country)/4)
  {
    o++
    c(Countries_Ommitted,Country1)
    break
  }
  
  temp_country <- na.omit(Population_Country)
  
  temp_country$Year <- as.numeric(as.character(temp_country$Year))
  Population_Country$Year <- as.numeric(as.character(Population_Country$Year))
  Population_Data$Year <- as.numeric(as.character(Population_Data$Year))
  linearMod <- lm(Population ~ Year, data=temp_country) # built the model here
  
  # print(summary(linearMod))
  # cat("Values in object\n")
  attributes(linearMod)
  #cat("Liner Model Coeffs",linearMod$coefficients,"\n")
  
  for(i in 1:nrow(Population_Country))
  {
     if(is.na(Population_Country$Population[i])) # checking in my data if the value is missing
      {
        # if it is missing, imputing the value
        Population_Country$Population[i] = as.numeric(linearMod$coefficients[1]) + as.numeric(linearMod$coefficients[2])*(Population_Data$Year[i])
      }
    }
    Population_final <- rbind(Population_final,Population_Country)
}
#head(Population_final)
write.csv(Population_final, file = "PreProcessed/PopulationDataset_Preprocessed.csv", row.names = FALSE)