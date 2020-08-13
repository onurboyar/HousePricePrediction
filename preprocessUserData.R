
library(dplyr)
#function for preprocessing the data that is retrieved by the web site by using restAPI

preprocess2 <- function(newData){
  newData <- newData %>% 
    mutate(Street = factor(Street, levels = c("Grvl", "Pave"))) %>% 
    mutate(RoofStyle = factor(RoofStyle, levels = c("Flat","Gable","Gambrel","Hip","Mansard","Shed"))) %>% 
    mutate(BsmtFinType2 = factor(BsmtFinType2, levels = c("ALQ", "BLQ", "GLQ", "LwQ", "Rec", "Unf"))) %>% 
    mutate(Heating = factor(Heating, levels = c("Floor","GasA","GasW","Grav","OthW","Wall")))  %>% 
    mutate(GarageCond = factor(GarageCond, levels = c("Ex", "Fa", "Gd", "Po", "TA", "None"))) %>%
    mutate(OverallQual = factor(OverallQual, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))) %>%
    mutate(OverallCond = factor(OverallCond, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))) 
  newData$LotFrontage <- as.numeric(as.character(newData$LotFrontage))
  newData$LotFrontage <- log(newData$LotFrontage+1)
  mean.lf = 4.202490145
  sd.lf = 0.3206272149
  newData$LotFrontage = (newData$LotFrontage - mean.lf) / sd.lf
  
  newData$YearBuilt <- as.numeric(as.character(newData$YearBuilt))
  mean.yb = 1971.313
  sd.yb = 30.29144
  newData$YearBuilt <- (newData$YearBuilt - mean.yb) / sd.yb
  
  newData$TotRmsAbvGrd <- as.numeric(as.character(newData$TotRmsAbvGrd))
  newData$TotRmsAbvGrd <- log(newData$TotRmsAbvGrd+1)
  mean.tr <- 6.451524
  sd.tr <- 1.569379
  newData$TotRmsAbvGrd <- (newData$TotRmsAbvGrd - mean.tr) / sd.tr
  newData
}

save(preprocess2, file = "data_for_house.RData")
