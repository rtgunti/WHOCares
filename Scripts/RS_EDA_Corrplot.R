methodtype <- c("circle", "square", "ellipse", "shade", "number", "circle", "pie", "square", "ellipse")
CountryList=sqldf('select distinct "CountryName","Country" from FNDDS')

#CountryListCount<-array(1:18)#NROW(CountryList))

CountryListCount<-array(1:9)#Better Viewing
par(mfrow=c(3,3))

# For the list of countries selected, plot the graphs
for (rowIndex in CountryListCount) {
  cab <- CountryList[rowIndex,2]
  cab1 <- CountryList[rowIndex,1]
  t1<- mod(rowIndex,9)
  mthd <- methodtype[t1]
  if(t1==0) 
  {
    mthd <- methodtype[9] # To get the correct index of the method type
  }
  INDDS <- FNDDS %>% filter(Country==cab)
  INDDS <- select (INDDS,"AverageTemperature","Emissions","GDP", "Population")
  colnames(INDDS)[1]<-"Temp"
  colnames(INDDS)[2]<-"CO2"
  colnames(INDDS)[4]<-"Popln"
  res <- round(cor(INDDS),2)
  corrplot(res, title =cab1 , method = mthd,  tl.col = "black",mar=c(0,0,1,0))
}
#tl.srt = 45