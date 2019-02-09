library(DAAG)
library(readr)
MergedDataSet <- read_csv("MergedDataSet.csv")
set.seed(123)
library("caret")
cvResults <- suppressWarnings(CVlm(df=MergedDataSet, form.lm=AN_Anamoly ~ AN_GDP+AN_POP+AN_CO2, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
train_control <- trainControl(method="cv", number=10)
finaldatasetFinal = data.frame(MergedDataSet)
library("caTools")
library(dplyr)
dataset = finaldatasetFinal %>% filter(finaldatasetFinal$Country=="USA")
index = createDataPartition(dataset$AN_Temp,p = 3/4,list=FALSE)
train_data = dataset[index,]
test_data = dataset[-index,]
controlPara = trainControl(method="cv",number = 3,savePredictions = TRUE)
model = train(AN_Temp~AN_GDP+AN_CO2+AN_POP,data=dataset,method="lm",trControl = controlPara)
predicted_temp = predict(model,test_data)
x = test_data$AN_Temp
f = data.frame(predicted_temp)
cbind(f,x)
print(model)
