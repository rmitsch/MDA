################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 Cross Validation
################

#This script performs model selection over all used techniques via CV on the 
# training set. The performance of the best model is validated on a hold-out
# test set that has never been used in the previous process.
#detach("package:FNN", unload=TRUE)
library(ISLR)
library("rpart")
library("class")
library(MASS)
library(covmat)
library("e1071");
library(caret)
library(ROCR)
library("verification")
library("klaR")
library(randomForest)

source("preProcessing_Classification.R")

#Normalization function to map to [0,1]
#Source: http://vitalflux.com/data-science-scale-normalize-numeric-data-using-r/ (last access: 25.01.16)
normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

#Convert binary col to numeric, as this knn implementation cannot deal with mixed data-types
dataNumeric <- data
studentData = data$student
studentData = gsub("Yes", "1", studentData)
studentData = gsub("No", "0", studentData)
dataNumeric$student = as.numeric(studentData)


numberOfModels <- 7

correct <- vector(length=0)
results <- vector("list", numberOfModels)
rocRes <- vector("list", numberOfModels)
# new plot of F-measure
predPos <- vector("list", numberOfModels) # predPositiveValues
rocPred <- vector("list", numberOfModels) # rocPrediction
rocPerf <- vector("list", numberOfModels) # rocPerformance
plotMeasure = "mat"
methodNames <- vector(length=0)

for( i in 1:10 ){	# For each fold (fold i is the test set)	
	# Create training and test set
	testSetN <- dataNumeric[ folds[[i]], ]
	testSet <- data[ folds[[i]], ]
	firstTrainingFold<-0
	if( (i!=1) ){
		trainingSetN <- dataNumeric[ folds[[1]], ]
		trainingSet <- data[ folds[[1]], ]
		firstTrainingFold<-1
	}else{
		trainingSetN <- dataNumeric[ folds[[2]], ]
		trainingSet <- data[ folds[[2]], ]
		firstTrainingFold<-2
	}
	for(j in 1:10){ #unify training folds
		if(i!=j && j!=firstTrainingFold){
			trainingSetN<- rbind(trainingSetN,dataNumeric[ folds[[j]], ])
			trainingSet<- rbind(trainingSet,data[ folds[[j]], ])
		}
	}

	#Build models and predict
	trainingSetNWithoutClass= trainingSetN[,!(names(trainingSetN) %in% c("default"))]
	testSetNWithoutClass= testSetN [,!(names(testSetN ) %in% c("default"))]
	correct <- c(correct ,testSet$default)

#-Decision tree-----------------------------------------------------------
	fit <- rpart(trainingSet$default~ .,method="class",data=trainingSet)
	optimal.CP <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
	pruned <- prune(fit, cp=optimal.CP)
	preds.rpart <- predict(pruned, newdata= testSet, type="class")
	results[[1]] <- c(results[[1]],preds.rpart)	
	probs.rpart <- predict(pruned, newdata= testSet)
	rocRes[[1]] <- c(rocRes[[1]],probs.rpart[,2])	
	methodNames[1] <- "Decision Tree"		
	#plot F measure
	predPos <- probs.rpart[,2]
	rocPred <- prediction(predPos, testSet$default)
	rocPerf[[1]] <- c(rocPerf[[1]], performance(rocPred, measure = plotMeasure))
#---------------------------------------------------------------------------


#-LDA------------------------------------------------------------------------
	data.lda 			= lda(trainingSet$default ~ ., data = trainingSet)
	data.lda.prediction	= predict(data.lda, newdata = testSet)
	results[[2]] <- c(results[[2]],data.lda.prediction$class)
	rocRes[[2]] <- c(rocRes[[2]],data.lda.prediction$posterior[,2])	
	methodNames[2] <- "LDA"
	#plot F measure
	predPos <- data.lda.prediction$posterior[,2]
	rocPred <- prediction(predPos, testSet$default)
	rocPerf[[2]] <- c(rocPerf[[2]],performance(rocPred, measure = plotMeasure))
#---------------------------------------------------------------------------
		
#-Logistic Regression---------------------------------------------------------
	data.logReg 		= glm(trainingSet$default ~ ., data = trainingSet, family = binomial)
	data.logReg.prediction	= predict(data.logReg, newdata = testSet, type = 'response')
	results[[3]] <- c(results[[3]], ifelse(data.logReg.prediction > 0.5,as.integer(2),as.integer(1)))
	rocRes[[3]] <- c(rocRes[[3]],data.logReg.prediction)	
	methodNames[3] <- "Logistic Regression"	
	#plot F measure
	predPos <- ifelse(data.logReg.prediction > 0.5,as.integer(2),as.integer(1))
	rocPred <- prediction(predPos, testSet$default)
	rocPerf[[3]] <- c(rocPerf[[3]],performance(rocPred, measure = plotMeasure))
#---------------------------------------------------------------------------

#-Naive Bayes---------------------------------------------------------------
	naiveBayes <- NaiveBayes(trainingSet$default~., data=trainingSet)
	naiveBayes.prediction <- predict(naiveBayes, newdata=testSet)
	results[[4]] <- c(results[[4]],naiveBayes.prediction$class)
	rocRes[[4]] <- c(rocRes[[4]],naiveBayes.prediction$posterior[,2])
	methodNames[4] <- "Naive Bayes"	
	#plot F measure
	predPos <- naiveBayes.prediction$posterior[,2]
	rocPred <- prediction(predPos, testSet$default)
	rocPerf[[4]] <- c(rocPerf[[4]],performance(rocPred, measure = plotMeasure))
#---------------------------------------------------------------------------
	
#-QDA-----------------------------------------------------------------------
	data.qda 			= qda(trainingSet$default ~ ., data = trainingSet)
	data.qda.prediction	= predict(data.qda, newdata = testSet)
	results[[5]] <- c(results[[5]],data.qda.prediction$class)
	rocRes[[5]] <- c(rocRes[[5]],data.qda.prediction$posterior[,2])
	methodNames[5] <- "QDA"		
	#plot F measure
	predPos <- data.qda.prediction$posterior[,2]
	rocPred <- prediction(predPos, testSet$default)
	rocPerf[[5]] <- c(rocPerf[[5]],performance(rocPred, measure = plotMeasure))
#---------------------------------------------------------------------------



#-KNN-----------------------------------------------------------------------
	#k ist zu 1 gesetzt um balanced accuracy zu optimieren statt der normalen acc.
	
	trainingSetNWithoutClass_norm<- as.data.frame(lapply( trainingSetNWithoutClass, normalize))
	testSetNWithoutClass_norm <- as.data.frame(lapply( testSetNWithoutClass, normalize))
	
	knnResult<-knn(train=trainingSetNWithoutClass_norm, test=testSetNWithoutClass_norm,cl=trainingSet$default, k = 1)
	results[[6]] <- c(results[[6]], knnResult)
	knnResult<-knn(train=trainingSetNWithoutClass_norm, test=testSetNWithoutClass_norm,cl=trainingSet$default, k = 1, prob=TRUE)
	h <- attr(knnResult, "prob")
	rocRes[[6]] <- c(rocRes[[6]],ifelse(knnResult==1, h, 1-h))
	methodNames[6] <- "K-nn"
	#plot F measure
	predPos <- ifelse(knnResult==1, h, 1-h)
	rocPred <- prediction(predPos, testSet$default)
	rocPerf[[6]] <- c(rocPerf[[6]],performance(rocPred, measure = plotMeasure))
#---------------------------------------------------------------------------
	
#-Random forest--------------------------------------------------------------
	# Remark: Test results were determined using n = 2000 subtrees. For performance reason ntree is reduced to 100 here.
	randomForest <- randomForest(trainingSet$default~ .,method="class",data=trainingSet, ntree = 250)
	# Get predicted class labels.
	randomForest.predClass <- predict(randomForest, newdata = testSet)
	# Get predicted probabilties.
	randomForest.predProb <- predict(randomForest, newdata = testSet, type="prob")
	# Add to list of results.
	results[[7]] = c(results[[7]], randomForest.predClass)
	rocRes[[7]] = c(rocRes[[7]], randomForest.predProb[, 2])
	methodNames[7] <- "Random Forest"		
	#plot F measure
	predPos <- randomForest.predProb[, 2]
	rocPred <- prediction(predPos, testSet$default)
	rocPerf[[7]] <- c(rocPerf[[7]],performance(rocPred, measure = plotMeasure))
#---------------------------------------------------------------------------

	
}

#Print confusion matrices and plot ROC-curves
for( i in 1:length(methodNames) ){
	print(methodNames[i])
	print(confusionMatrix(data=as.factor(results[[i]]), reference=as.factor(correct)))

# do not plot each roc individually
#roc.plot(ifelse(correct==2,1,0), rocRes[[i]], legend=T, leg.text=methodNames[i],plot.thres=NULL, main="Test Data")
}

x11()
roc.plot(ifelse(correct==2,1,0), cbind(rocRes[[1]], rocRes[[2]],rocRes[[3]],rocRes[[4]],rocRes[[5]],rocRes[[6]], rocRes[[7]]), legend=T, 
         leg.text=c("decision tree","LDA",
                    "logistic regression","naive bayes", "QDA", "knn", "Random Forest"),  plot.thres=NULL)

# Conclusion: LDA and logistic regression seem to perform best.
# Last step: Apply best-performing algorithms on "real" test set.

# Logistic regression
data.logReg 			= glm(default ~ ., data = data, family = binomial)
data.logReg.prediction	= predict(data.logReg, newdata = dataTest, type = 'response')
results[[3]] 			<- ifelse(data.logReg.prediction > 0.5, 'Yes', 'No')
rocRes[[3]] 			<- data.logReg.prediction	

#LDA
data.lda 				= lda(data$default ~ ., data = data)
data.lda.prediction		= predict(data.lda, newdata = dataTest)
results[[2]] 			<- data.lda.prediction$class
rocRes[[2]] 			<- data.lda.prediction$posterior[,2]	


#roc.plot(ifelse(testSet$default==2,1,0), cbind(finalLDAROCRes), legend=T, 
#         leg.text=c("LDA"),  plot.thres=NULL)
dataTestCopy 			<- dataTest
responseData 			= dataTest$default
responseData 			= gsub("Yes", "1", responseData)
responseData 			= gsub("No", "0", responseData)
dataTestCopy$default 	= as.numeric(responseData)

# Print confusion matrix
print(paste(methodNames[2], "confusion matrix", sep = " - "))
print( confusionMatrix(table(results[[2]], dataTest$default)) )
print(paste(methodNames[3], "confusion matrix", sep = " - "))
print( confusionMatrix(table(results[[3]], dataTest$default)) )

# Random Forest
randomForest <- randomForest(data$default~ ., method="class",data=data, ntree = 1000)
# Get predicted class labels.
randomForest.predClass <- predict(randomForest, newdata = dataTest)
# Get predicted probabilties.
randomForest.predProb <- predict(randomForest, newdata = dataTest, type="prob")
# Add to list of results.
results[[1]] = randomForest.predClass
rocRes[[1]] = randomForest.predProb[, 2]


# Print confusion matrix
print(paste(methodNames[1], "confusion matrix", sep = " - "))
print( confusionMatrix(table(results[[1]], dataTest$default)) )
roc.plot(ifelse(dataTest$default==2,1,0), ifelse(rocRes[[1]]==2,1,0), legend=T, leg.text="RPART",plot.thres=NULL, main="Test Data")

# Plot f-measure.
x11()
par(mfrow = c(1, 3))
rocPrediction 	= prediction(rocRes[[1]], dataTestCopy$default)
rocPerformance	= performance(rocPrediction, measure = "tnr", x.measure = "fnr")
plot(rocPerformance, main = 'Decision Tree: tnr ~ fnr', col = 'red')
rocPerformance	= performance(rocPrediction, measure = "tpr", x.measure = "fpr")
plot(rocPerformance, main = 'Decision Tree: tpr ~ fpr', col = 'blue', add = TRUE)

rocPrediction 	= prediction(rocRes[[2]], dataTestCopy$default)
rocPerformance	= performance(rocPrediction, measure = "tnr", x.measure = "fnr")
plot(rocPerformance, main = 'LDA: tnr ~ fnr', col = 'red')
rocPerformance	= performance(rocPrediction, measure = "tpr", x.measure = "fpr")
plot(rocPerformance, main = 'LDA: tpr ~ fpr', col = 'blue', add = TRUE)

rocPrediction 	= prediction(rocRes[[3]], dataTestCopy$default)
rocPerformance	= performance(rocPrediction, measure = "tnr", x.measure = "fnr")
plot(rocPerformance, main = 'Log. Reg.: tnr ~ fnr', col = 'red')
rocPerformance	= performance(rocPrediction, measure = "tpr", x.measure = "fpr")
plot(rocPerformance, main = 'Log. Reg.: tpr ~ fpr', col = 'blue', add = TRUE)
#x11()

x11()
roc.plot(dataTestCopy$default, cbind(rocRes[[2]], rocRes[[3]]), legend=T, 
         main='Performance in test set as ROC Curve', leg.text=c("LDA", "Logistic Regression"),  plot.thres=NULL)
x11()

plot(fit)
text(fit, cex=0.7)

plot(pruned)
text(pruned, cex=1.0)
