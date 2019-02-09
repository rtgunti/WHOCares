library(readr)
# Script to find the summary values for Gdp data set.
# This is used to build the table in the report

gdpDataset=read.csv("Data sets/GDPDataSet.csv")
GDPDataSet<-gdpDataset[-c(1)]
colnames(GDPDataSet)[5]<-"Year"
colnames(GDPDataSet)[6]<-"GDP"
print(NROW(GDPDataSet))
print(sum(is.na(GDPDataSet)))
print(mean(GDPDataSet$GDP,na.rm = TRUE))
print(var(GDPDataSet$GDP,na.rm = TRUE))