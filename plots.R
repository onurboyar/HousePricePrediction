# working on the orginal values, not log transformed or scaled
fullData$HouseAge <- 2011 - fullData$YearBuilt 

ggplot(data = fullData2) + geom_point(mapping = aes(x = YearBuilt, y = HouseAge, color = Heating)) + facet_wrap(~Street)

ggplot(train.v1,aes(x = LotFrontage, y = outcomes)) + geom_smooth() + geom_point() 

OverallQual : 30
YearBuilt : 20
TotRmsAbvGrd : 15
LotFrontage : 11
OverallCond : 7
GarageCond : 6
RoofStyle : 5
BsmtFinType2 : 3
Heating : 2
Street : 1

ggplot(test,aes(x = Features, y = ImportanceRatio)) + geom_point(color="red", size = 10) + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))


ggplot(test,aes(x = Features, y = ImportanceRatio)) + geom_point(color="red", size = 10) + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

ggplot(test, aes(x=Features, y=ImportanceRatio))+geom_bar(width = 1, stat = "identity", fill = mycols) + theme(axis.text=element_text(size=10),axis.title=element_text(size=10,face="bold"))
