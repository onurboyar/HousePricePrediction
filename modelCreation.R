library(gbm)
library(randomForest)
library(neuralnet)

# selected parameters for prediction
columns <- c("Street","RoofStyle","BsmtFinType2","Heating","GarageCond","OverallQual","OverallCond","LotFrontage","YearBuilt","TotRmsAbvGrd")
train.v1 <- train[,columns]

#model creation 
rf.model <- randomForest(outcomes~., data = train.v1, mtry = 3, importance = TRUE)
mod.gbm <- gbm(medv ~ . ,data = train.v1,distribution = "gaussian",n.trees = 10000,
               shrinkage = 0.01, interaction.depth = 4)



# matrix creation for neural networks model
matrix.train <- model.matrix(~.,data=train.v1)
matrix.train <- matrix.train[,-1]

#not used in our model
nn <- neuralnet(outcomes~StreetPave+RoofStyleGable+RoofStyleGambrel+RoofStyleHip+RoofStyleMansard+RoofStyleShed+
                  BsmtFinType2BLQ+BsmtFinType2GLQ+BsmtFinType2LwQ+BsmtFinType2Rec+BsmtFinType2Unf+
                  HeatingGasA+HeatingGasW+HeatingGrav+HeatingOthW+HeatingWall+
                  GarageCondFa+GarageCondGd+GarageCondPo+GarageCondTA+GarageCondNone+
                  OverallQual2+OverallQual3+OverallQual4+OverallQual5+OverallQual6+OverallQual7+OverallQual8+OverallQual9+OverallQual10+
                  OverallCond2+OverallCond3+OverallCond4+OverallCond5+OverallCond6+OverallCond7+OverallCond8+OverallCond9+
                  LotFrontage + YearBuilt + TotRmsAbvGrd,data = matrix.train,hidden = 4,linear.output = F, act.fct = "tanh")

save(rf.model, file = "rf_for_house.RData")
save(mod.gbm, file = "gbm_for_house.RData")


