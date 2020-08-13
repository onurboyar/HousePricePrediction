
# Libraries
require(dplyr)
require(ggplot2)
require(readr)
require(e1071)
require(Matrix)

#data can be downloaded from
# "https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data"

train <- read.csv("./data/train.csv") %>% mutate(is.train = 1)
outcomes <- log(train$SalePrice); train$SalePrice <- NULL

test <- read.csv("./data/test.csv") %>% mutate(is.train = 0)
fullData <- rbind(train,test) 

fullData$MSSubClass <- as.factor(fullData$MSSubClass)
fullData$OverallQual <- as.factor(fullData$OverallQual)
fullData$OverallCond <- as.factor(fullData$OverallCond)

colClass <- sapply(fullData, class)

#fill null values in factor variables
fillNaFactor <- function(icol){
  temp <- fullData[complete.cases(fullData[,icol]),] %>%group_by_(.dots=colnames(fullData)[icol]) %>% summarize(n()) %>%
    as.data.frame()
  ind.max <- which.max(temp[,2])
  temp[ind.max,1] # Return value
}

#full null values in integer & numeric values ^burayı incele
fillNaInteger <- function(icol){
  median(fullData[complete.cases(fullData[,icol]),icol]) # Return the median
}

for (icol in 1:ncol(fullData)){
  # Factor variables
  if (colClass[icol] == "factor"){
    l.NA <- is.na(fullData[,icol]) # NA's 
    
    # Replace NA's with most common level
    if (sum(l.NA) < 100){ # Less than 100 observations is NA
      fullData[l.NA,icol] <- fillNaFactor(icol)
    }
    # If there are too many NA's (i.e. 100 in this case) create a new level
    if (sum(l.NA) > 100){
      levels(fullData[,icol]) <- c(levels(fullData[,icol]), "None")
      fullData[l.NA,icol] <- "None"
    }
  }
  # Integers
  if (colClass[icol] == "integer"){
    l.NA <- is.na(fullData[,icol])
    #fullData[l.NA,icol] <- 0 # Replace NAs in integers with 0
    fullData[l.NA,icol] <- fillNaInteger(icol) # Replace with median
  }
}

# log transformation
deskew <- function(x){
  if (abs(skewness(x)) > 0.5){
    x <- log(1+x)
  }
  x
}

# normalization
rescale <- function(x) { (x-mean(x))/sd(x) }

numericCols <- setdiff(names(colClass[colClass == "integer" | colClass == "numeric"]),c("Id","is.train"))

fullData <- fullData %>% mutate_at(.cols=numericCols, funs(deskew)) %>% mutate_at(.cols=numericCols, funs(rescale))

train <- filter(fullData, is.train == 1) %>% select(-is.train)
test <- filter(fullData, is.train == 0) %>% select(-is.train)
Id.test <- test$Id # Id numbers for test set
Id.train <- train$Id
