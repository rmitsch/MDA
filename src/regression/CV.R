################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 Cross Validation
################

#This script performs model selection over all used techniques via CV on the 
# training set. The performance of the best model is validated on a hold-out
# test set that has never been used in the previous process.

source("preProcessing.R")

library(ISLR)
library("rpart")
library(FNN)
library(MASS)
library(caret)
library(ROCR)
library(Metrics)
library(mgcv)
library(gamclass)
library(locfit)

#Normalization function to map to [0,1]
#Source: http://vitalflux.com/data-science-scale-normalize-numeric-data-using-r/ (last access: 25.01.16)
normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

#Convert binary col to numeric, as this knn implementation cannot deal with mixed data-types
dataNumeric <- data_outlier_training
privateData = data_outlier_training$Private
privateData = gsub("Yes", "1", privateData)
privateData = gsub("No", "0", privateData)
dataNumeric$Private = as.numeric(privateData)

dataNumeric_NO_outlier <- data_NO_outlier_training
privateData2 = data_NO_outlier_training$Private
privateData2 = gsub("Yes", "1", privateData2)
privateData2 = gsub("No", "0", privateData2)
dataNumeric_NO_outlier$Private = as.numeric(privateData2)

numberOfModels <- 13

correct <- v[0]
correctAll <- v[0]
results <- vector("list", numberOfModels )
methodNames <- v[0]

for( i in 1:10 ){	# For each fold (fold i is the test set)	
	# Create training and test set
	testSetN <- dataNumeric[ foldsAll[[i]], ]
	#old
	#testSet <- data_NO_outlier_training[ folds[[i]], ]
	#testSetAll <- data_outlier_training[ foldsAll[[i]], ]
	testSet <- data_outlier_training[ foldsAll[[i]], ]
	testSetAll <- testSet #in the current setup both are the same (with outliers)

	firstTrainingFold<-0
	if( (i!=1) ){
		trainingSetN <- dataNumeric_NO_outlier[ folds[[1]], ]
		trainingSet <- data_NO_outlier_training[ folds[[1]], ]
		trainingSetAll <- data_outlier_training[ foldsAll[[1]], ]
		firstTrainingFold<-1
	}else{
		trainingSetN <- dataNumeric_NO_outlier[ folds[[2]], ]
		trainingSet <- data_NO_outlier_training[ folds[[2]], ]
		trainingSetAll <- data_outlier_training[ foldsAll[[2]], ]
		firstTrainingFold<-2
	}
	for(j in 1:10){ #unify training folds
		if(i!=j && j!=firstTrainingFold){
			trainingSetN<- rbind(trainingSetN, dataNumeric_NO_outlier[ folds[[j]], ])
			trainingSet<- rbind(trainingSet, data_NO_outlier_training[ folds[[j]], ])
			trainingSetAll <- rbind(trainingSetAll, data_outlier_training[ foldsAll[[j]], ])
		}
	}

	##########################
	#Build models and predict#
	##########################
	trainingSetNWithoutClass= trainingSetN[,!(names(trainingSetN) %in% c("Apps"))]
	testSetNWithoutClass= testSetN [,!(names(testSetN ) %in% c("Apps"))]
	correct <- c(correct ,testSet$Apps)
	correctAll <- c(correctAll ,testSetAll$Apps)

	#knn (with normalization)
	#trainingSetN_short <- as.data.frame(cbind(trainingSetN$Expend, trainingSetN$Apps))
	#testSetN_short <- as.data.frame(cbind(testSet$Expend, testSet$Apps));	

	trainingSetN_norm <- as.data.frame(lapply( trainingSetN, normalize))
	trainingSetN_norm$Apps <- as.numeric(trainingSetN$Apps)
	testSetN_norm <- as.data.frame(lapply( testSetN, normalize));
	testSetN_norm$Apps <- as.numeric(testSetN$Apps)
	#knn <- knn.reg(trainingSetN_norm, y=trainingSetN$Apps, test=testSetN_norm, k = 4)
	results[[1]] <- c(results[[1]],knn$pred)
	methodNames[1] <- "k-nn"		

	#Cubic Spline
	data.splines = gam(Apps ~ Private + Accept + Top25perc + 
    P.Undergrad + Room.Board + Books + Personal + Terminal + 
    S.F.Ratio + perc.alumni + Expend + Grad.Rate, data=trainingSetN)
	results[[2]] <- c(results[[2]],predict(data.splines, testSetNWithoutClass))
	methodNames[2] <- "Cubic Spline"

	#Linear Regression (The small model)
	regSmall <- lm(trainingSet$Apps ~ Accept + Top25perc + Books + S.F.Ratio + Expend + Grad.Rate, data=trainingSet)
	results[[3]] <- c(results[[3]], predict(regSmall , testSet))
	methodNames[3] <- "Linear Regression"

	#LOESS 
	yT <- locfit(trainingSetN$Apps ~ Accept + Top25perc + Room.Board:Private,alpha=0.4,deg=2, data=trainingSetN)
	results[[4]] <- c(results[[4]], predict(yT,newdata=testSetN))
	methodNames[4] <- "LOESS"

	#Regression Tree
	fit = rpart(trainingSet$Apps~., data=trainingSet, method="anova")
	pfit = prune(fit, fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
	preds.rpart <- predict(pfit , newdata=testSet)
	results[[5]] <- c(results[[5]],preds.rpart)	
	methodNames[5] <- "Regression Tree"

	#Robust Regression

	#Robust M-Estimator with Huber-Weight
	hubert <- rlm(trainingSetAll$Apps~., data=trainingSetAll)
	results[[6]] <- c(results[[6]], predict(hubert,newdata=testSetAll))	
	methodNames[6] <- "Robust M-Estimator with Huber-Weight"

	# Robust M-Estimator with Bisquare-Weight
	bisquare <- rlm(trainingSetAll$Apps~., data=trainingSetAll, method="MM")
	results[[7]] <- c(results[[7]], predict(bisquare,newdata=testSetAll))	
	methodNames[7] <- "Robust M-Estimator with Bisquare-Weight"
	
	# LTS-FIT
	lts <- lqs(trainingSetAll$Apps~., data=trainingSetAll)
	results[[8]] <- c(results[[8]], predict(lts,newdata=testSetAll))	
	methodNames[8] <- "LTS-FIT"

	#Robust M-Estimator with Huber-Weight
	hubert <- rlm(trainingSet$Apps~., data=trainingSet)
	results[[9]] <- c(results[[9]], predict(hubert,newdata=testSet))	
	methodNames[9] <- "Robust M-Estimator with Huber-Weight - Reduced Data"

	# Robust M-Estimator with Bisquare-Weight
	bisquare <- rlm(trainingSet$Apps~., data=trainingSet, method="MM")
	results[[10]] <- c(results[[10]], predict(bisquare,newdata=testSet))	
	methodNames[10] <- "Robust M-Estimator with Bisquare-Weight - Reduced Data"
	
	# LTS-FIT
	lts <- lqs(trainingSet$Apps~., data=trainingSet)
	results[[11]] <- c(results[[11]], predict(lts,newdata=testSet))	
	methodNames[11] <- "LTS-FIT - Reduced Data"

	#Non-linear Regression

	#boxCox
	trainingCopy <- trainingSetN
	testCopy <- testSetN

	boxCox.lin <- lm(trainingCopy$Apps~Private + Accept + Top25perc + 
    P.Undergrad + Room.Board + Books + Personal + Terminal + 
    S.F.Ratio + perc.alumni + Expend + Grad.Rate, data=trainingCopy)
	b <- boxcox(boxCox.lin,plotit=FALSE)
	best <- b$x[which(b$y==max(b$y))]
	trainingCopy$Apps_t <- (trainingCopy$Apps^best-1)/best
	testCopy$Apps_t <- (testCopy$Apps^best-1)/best
	boxCox.reg <- lm(trainingCopy$Apps_t~Private + Accept + Top25perc + 
    P.Undergrad + Room.Board + Books + Personal + Terminal + 
    S.F.Ratio + perc.alumni + Expend + Grad.Rate, data=trainingCopy)
	results[[12]] <- c(results[[12]], predict(boxCox.reg,newdata=testCopy))	
	methodNames[12] <- "Box-Cox transformation"

	#log
	logRegr <- glm(trainingSetN$Apps~., data=trainingSetN,family=gaussian(link="log"))
	results[[13]] <- c(results[[13]], predict(logRegr,newdata=testSetN))	
	methodNames[13] <- "Log transformation"	

}

#Print model comparison
for( i in 1:numberOfModels ){
	print(methodNames[i])
	print(rmse(predicted=results[[i]], actual=correctAll))
}


#FINAL EVALUATION
#For the best model:
#Build on data_outlier_training
#Test on  data_test_outlier

dataTestNumeric <- data_test_outlier_reduced
privateData = data_test_outlier_reduced$Private
privateData = gsub("Yes", "1", privateData)
privateData = gsub("No", "0", privateData)
dataTestNumeric$Private = as.numeric(privateData)

#knn
#dataNumeric_norm<- as.data.frame(lapply(dataNumeric, normalize))
#dataTestNumeric_norm <- as.data.frame(lapply(dataTestNumeric , normalize));
#knnTest <- knn.reg(dataNumeric_norm, y=dataNumeric$Apps, test=dataTestNumeric_norm, k = 4)
#print("knn on test set")
#print(rmse(predicted=knnTest$pred, actual=dataTestNumeric$Apps ))

#loess
#yT <- locfit(dataNumeric$Apps ~ Accept + Top25perc + Room.Board:Private,alpha=0.4,deg=2, data=dataNumeric)
#print("loesson test set")
#print(rmse(predicted=predict(yT,newdata=dataTestNumeric ), actual=dataTestNumeric$Apps ))

hubertT <- rlm(dataNumeric$Apps~., data=dataNumeric)
print("hubert test set")
print(rmse(predicted=predict(hubertT,newdata=dataTestNumeric ), actual=dataTestNumeric$Apps ))
