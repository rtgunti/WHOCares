library(readr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tibble)
library(sqldf)
library(tibble)

dataSet_final=FinaldatasetFinal
df_corCountryList=sqldf('select distinct country from dataSet_final')
print(df_corCountryList)
df_corCalculation <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("Country", "GDPCor1980", "GDPCor1981","EmissionCor1980", "EmissionCor1981","PopulationCor1980", "PopulationCor1981","DisasterCor1980", "DisasterCor1981")
colnames(df_corCalculation) <- x

cor_CountryList=as.data.frame(df_corCountryList)
#creating an array equal to the length of the gdp_omitcountrylist for running it as a for loop for filtering  purpose
cor_ListCount<-array(1:NROW(cor_CountryList))
print(cor_ListCount)
for (rowIndex in cor_ListCount) {
  fDataset=FinaldatasetFinal
  cab<-cor_CountryList[rowIndex,1]
  Finaldataset=fDataset %>% filter(fDataset$Country==cab,fDataset$Year<1981)
  fDataset1=FinaldatasetFinal
  Finaldataset1=fDataset1 %>% filter(fDataset1$Country==cab,fDataset1$Year>1981)
  
  df=cor(Finaldataset$anamoly,Finaldataset$GDP,method="spearman")
  df1=cor(Finaldataset1$anamoly,Finaldataset1$GDP,method="spearman")
  
  df2=cor(Finaldataset$anamoly,Finaldataset$Emissions,method="spearman")
  df3=cor(Finaldataset1$anamoly,Finaldataset1$Emissions,method="spearman")
  
  df4=cor(Finaldataset$anamoly,Finaldataset$Population,method="spearman")
  df5=cor(Finaldataset1$anamoly,Finaldataset1$Population,method="spearman")
  
  df6=cor(Finaldataset$anamoly,Finaldataset$TotalDeaths,method="spearman")
  df7=cor(Finaldataset1$anamoly,Finaldataset1$TotalDeaths,method="spearman")
  newRow <- data.frame(Country=cab,GDPCor1980=df,GDPCor1981=df1,EmissionCor1980=df2,EmissionCor1981=df3,PopulationCor1980=df4,PopulationCor1981=df5,DisasterCor1980=df6,DisasterCor1981=df7)

  
  df_corCalculation <- rbind(df_corCalculation,newRow)

}


