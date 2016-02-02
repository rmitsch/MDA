################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 Random Forest
################

library(randomForest)


randomForest <- randomForest(data$default~ ., data=data, ntree = 1000)
# Get predicted class labels.
randomForest.predClass <- predict(randomForest, newdata= dataTest)
# Get predicted probabilties.
randomForest.predProb <- predict(randomForest, newdata= dataTest, type="prob")
print(randomForest.predProb)

# Confusion matrix for quick test of correctness.
print( confusionMatrix(table(randomForest.predClass, dataTest$default)) )

# ROC curve.
print(nrow(randomForest.predProb))
print(randomForest.predProb)
rocPrediction 	= prediction(randomForest.predProb[, 2], dataTest$default)
rocPerformance	= performance(rocPrediction, measure = "tpr", x.measure = "fpr")
plot(rocPerformance, col = rainbow(10))