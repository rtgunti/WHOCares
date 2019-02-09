library(gridExtra)
library(sqldf)
library(dplyr)
library(ggplot2)
data2 = data.frame()
FNDDS <- read.csv("Data sets/PreProcessed/MergedDataSet.csv")
DeathDs = select(FNDDS, "Year", "CountryName", "AverageTemperature","Population", "GDP", "Emissions", "TotalDeaths")
DeathDs1 = DeathDs %>% filter(DeathDs$TotalDeaths>50000)
n = nrow(DeathDs1)

for(i in 1:n){
  yr = DeathDs1$Year[i]
  cou = DeathDs1$CountryName[i]
  {
    data1 = DeathDs %>% filter(CountryName == cou)%>%  filter(Year>(yr-2))
    data3 = data1 %>% filter(Year<yr+4)
    data2 = rbind(data2,data3)
  }
}

CountryList=sqldf('select distinct CountryName from data2')
CountryListCount<-array(1:NROW(CountryList))

#for(i in CountryListCount){
#  cab<-CountryList[i,1]
  dataFinal=data2 %>% filter(CountryName=="India")
  gg1<-ggplot(data = dataFinal, mapping=aes(x = Year, y = Emissions)) +
    geom_line() + guides(size = FALSE) + 
    theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), plot.margin = margin(2,.8,2,.8, "cm")) 
  gg2<-ggplot(data = dataFinal, mapping=aes(x = Year, y = GDP)) +
    geom_line() + guides(size = FALSE) + 
    theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), plot.margin = margin(2,.8,2,.8, "cm")) 
    #geom_line() + ggtitle(cab) + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
  gg3<-ggplot(data = dataFinal, mapping=aes(x = Year, y = Population)) +
    geom_line() + guides(size = FALSE) + 
    theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), plot.margin = margin(2,.8,2,.8, "cm")) 
  gg4<-ggplot(data = dataFinal, mapping=aes(x = Year, y = AverageTemperature)) +
    geom_line() + guides(size = FALSE) + 
    theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), plot.margin = margin(2,.8,2,.8, "cm")) 
  
  grid.arrange(gg1, gg2, ncol=2)
  grid.arrange(gg3, gg4, ncol=2)
#}