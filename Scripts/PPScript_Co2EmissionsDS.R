library(reshape2)
library(dplyr)
library(readxl)

#reading the initial data set in xlsx format and melting the data in order to make it possible to combine it with the other data sets
carbondataset <- read_excel("Data sets/carbondataset.xlsx")
carbon_data <- melt(carbondataset, id=c("Year"))

#renaming the columns variable and value to country and emissions after melting
colnames(carbon_data)[2] <- "Country"
colnames(carbon_data)[3] <- "Emissions"

#writing the formatted data into csv file to make it available for further processing of the data
write.csv(carbon_data, file = "PreProcessed/Co2EmissionsDataSet.csv", row.names =FALSE )