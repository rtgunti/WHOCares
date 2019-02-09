library(readr)
getwd()
library(tidyr)
library(ggplot2)
library(readr)
GlobalLandTemperaturesByCountry <- read_csv("GlobalLandTemperaturesByCountry.csv")

df = data.frame(GlobalLandTemperaturesByCountry)
library(dplyr)


library(tibble)
#"Adding the uncertainities to produce min and max uncertaininty"

df = cbind(df,df[,2]-df[,3],df[,2]+df[,3])
df = cbind(df,format(df[,1],format="%Y"))
# renaming the columns
colnames(df) <- c("dt","AverageTemperature", "AverageTemperatureUncertainty","Country","minUncertain","maxUncertain","Year")
df = df %>% drop_na()
list1 = 1:length(df[[1]])
df1= df[-list1,]
df1[] <- lapply(df1[7], as.double)
# dropping the continents from the table of 500000 rows

df = df %>% 
  filter((format(df[,1],format="%Y") >= 1900))
df = df %>%
  filter(df[,4] != "Åland")
df = df %>%
  filter(df[,4] != "Africa")
df = df %>%
  filter(df[,4] != "Antarctica")
df = df %>%
  filter(df[,4] != "North America")
df = df %>%
  filter(df[,4] != "South America")
df = df %>%
  filter(df[,4] != "Asia")
df = df %>%
  filter(df[,4] != "Africa")
df = df %>%
  filter(df[,4] != "Oceania")
df = df %>%
  filter(df[,4] != "Europe")


#calculating the mean temperature for the year, since the data is at month level.
df1 <- df %>%
  group_by(Country,Year) %>%
  summarise(AverageTemperature = mean(AverageTemperature),MinTemperature = mean(MinTemperature),MaxTemperature = mean(MaxTemperature))
head(df)
write.csv(df1, "PreProcessed/TemperatureDataset.csv", row.names = FALSE)


