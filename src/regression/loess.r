################################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 - Outlier detection
################################

library(ISLR)
library(locfit)
source("preProcessing.R")



data <- data_NO_outlier_training

#data$Private <- ifelse(data$Private=="Yes",1,0)
# do not use private directly as it does not work here
# yT <- locfit(data$Apps ~ Accept + Top25perc + P.Undergrad + Room.Board + Books + Personal + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate,alpha=0.4,deg=2,data=data)
# alpha = 0.4 -> 40% neighborhood used
# deg = 2, local polynomial = parabel


# only use columns of linear model here - if all columns are used there is not enough memory & time
yT <- locfit(data$Apps ~ Accept + Expend,kern="tcub",alpha=0.2,deg=0,data=data)

dataTest <- data_test_outlier
dataTest$Private <- ifelse(dataTest$Private=="Yes",1,0)

colNames  = c("Apps","Accept","Expend")

dataTest = dataTest[,(names(dataTest) %in% colNames)]
data = data[,(names(data) %in% colNames)]

prediction <- predict(yT,dataTest)
plot(dataTest$Apps,prediction) # prediction vs correct value


# can only handle 4 columns
# y <- loess(data$Apps ~ ., data=data)
