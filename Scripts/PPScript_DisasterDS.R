library(magrittr)
library(dplyr)

### Time for reading NDT
Disaster_Type=read.csv("NaturalDisasters.csv")

#sum(is.na(Disaster_Type))
colnames(Disaster_Type)[11] <- "Total.damage" # Renaming the column

# Considering values from 1960 onwards
Disaster_Type = Disaster_Type %>% filter(Disaster_Type$year >= 1960)
#sum(is.na(Disaster_Type))


#Disaster_Type[is.na(Disaster_Type)] <- 0

## Aggregate Deaths grouped by country, year
DisType_ctry_grpd_dt <- Disaster_Type %>% 
  group_by(year,country_name,iso) %>%
  summarize(TotalDeaths = sum(Total.deaths), TotalAffected = sum(Total.affected), TotalDamage=sum(Total.damage))

DisType_ctry_grpd_dt[is.na(DisType_ctry_grpd_dt)] <- 0
#View(DisType_ctry_grpd_dt) 
# Renaming the columns so that the joins can happen easily
colnames(DisType_ctry_grpd_dt)[1]<-"Year"
colnames(DisType_ctry_grpd_dt)[3]<-"Country"

#writing the preprocessed dataset to a file
write.csv(DisType_ctry_grpd_dt, file = "PreProcessed/Deaths_by_year_country.csv",row.names = FALSE)
