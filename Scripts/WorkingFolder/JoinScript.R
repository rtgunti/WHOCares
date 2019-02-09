library(readr)
library(dplyr)
library(tidyr)

#CO2DS <- read_csv("emissions.csv")
POPDS <- read_csv("PopulationDataset_03_Preprocessed.csv")
TMPDS <- read_csv("TemperatureDatasetFinal.csv")
MRGDS <- merge(POPDS,TMPDS)
GDPDS <- read_csv("GDPDataset.csv")
GDPDS <- select(GDPDS, "Country Name", "Country Code", "Year","GDP")
#Rename the country code to country
colnames(GDPDS)[2]<-"Country"
MRGDS <- merge(MRGDS,GDPDS)

CO2DS <- read_csv("EmissionDataSet.csv")
CO2DS <- select(CO2DS, "Country Code", "Year","Emissions")
#Rename the country code to country
colnames(CO2DS)[1]<-"Country"
MRGDS <- merge(MRGDS,CO2DS)
MRGDS <- MRGDS %>% drop_na()

# Since all the datasets were ending in the year 2012, we considered the year 2012 for selecting something
# Filtered Dataset
FLTDS = MRGDS %>% filter((format(MRGDS$Year,format="%Y") == 2012))
FLTDS = sqldf('select Country from FLTDS WHERE GDP>1000000000000 OR Population > 100000000')
# In the year 2012, select countries with GDP over a trillion and Population over 100 million
# Now, consider data for only these countries from all the data sources

FNLDS <- merge(MRGDS, FLTDS, by = "Country")
View(FNLDS)
