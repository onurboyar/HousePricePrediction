# !!!---- API CREATION


#library(rjson)
library(jsonlite)
library(randomForest)
library(gbm)
library(dplyr)
load("rf_for_house.RData")
load("data_for_house.RData")
load("gbm_for_house.RData")

#* @post /predict
#* @serializer unboxedJSON
predict.house <- function(Street, RoofStyle, BsmtFinType2, Heating, 
               GarageCond, OverallQual, OverallCond, LotFrontage, YearBuilt, TotRmsAbvGrd){
  data <- list(
   Street=Street, RoofStyle = RoofStyle, BsmtFinType2 = BsmtFinType2, Heating = Heating, 
     GarageCond = GarageCond, OverallQual = OverallQual,
    OverallCond = OverallCond, LotFrontage = LotFrontage, YearBuilt = YearBuilt,
    TotRmsAbvGrd = TotRmsAbvGrd
  )
  data <- as.data.frame(data)
  print(data)
  data <- preprocess2(data)
  
  prediction <- predict(rf.model, data)
  pred.gbm <- predict(mod.gbm,data, n.trees = 1000)
  
  prediction <- exp(prediction)
  pred.gbm <- exp(pred.gbm)
  
  ensembled <- (pred.gbm + prediction) / 2
  
  json <- jsonlite::toJSON(ensembled, auto_unbox = TRUE)
  }

library(plumber)
# !!! launch API
#r <- plumb(file = "rf.model.R")
#r$run(port=8000)
