################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 - LDA
################

# Linear Discriminant Analysis
# Model assumptions:
#	- For common LDA: Same covariance matrix for all groups.
#	- Normally distributed (? -> http://stats.stackexchange.com/questions/113133/does-fisher-linear-discriminant-analysis-lda-require-normal-distribution-of-th)
#	- Groups: 2 -> Default = yes, Default = no.
#
# Important: Linear discriminant analysis  does not allow for categorical explanatory variables (http://stats.stackexchange.com/questions/131375/explanatory-variables-with-linear-discriminant-analysis).
#			 Therefore, we are forced to convert our logical to numerical variables.	

library("e1071");
library(MASS)
library(covmat)
library(caret)
library(ROCR)

# Preprocess data.
# ---------------------------------------------------------------------------

# Copy data, convert student to numerical type.
dataCopy			= data
dataCopy$student	= gsub("Yes", "1", dataCopy$student)
dataCopy$student 	= gsub("No", "0", dataCopy$student)
dataCopy$student 	= as.numeric(dataCopy$student)
# Seperate in groups.
dataCopyDNo			= dataCopy[which(dataCopy$default == 'No'), ]
dataCopyDYes		= dataCopy[which(dataCopy$default == 'Yes'), ]
# Remove respone column.
dataCopyDNo 		= dataCopyDNo[,!(names(data) %in% c('default'))]
dataCopyDYes 		= dataCopyDYes[,!(names(data) %in% c('default'))]

# ---------------------------------------------------------------------------

# Find priors. 
totalRowNumber 	= nrow(dataCopyDNo) + nrow(dataCopyDYes)
priors 			= c(nrow(dataCopyDYes) / totalRowNumber, nrow(dataCopyDNo) / totalRowNumber)

# Train LDA.
data.lda 			= lda(data$default ~ ., data = data)
# Predict using the trained model.
data.lda.prediction	= predict(data.lda, newdata = dataTest)

print(data.lda$call)

# ---------------------------------------------------------------------------

# Confusion matrix for quick test of correctness.
print( confusionMatrix(table(data.lda.prediction$class, dataTest$default)) )

# ROC curve:
# Convert default to numeric.
rocDataCopy			= dataTest
rocDataCopy$default	= gsub("Yes", "1", rocDataCopy$default)
rocDataCopy$default = gsub("No", "0", rocDataCopy$default)
rocDataCopy$default = as.numeric(rocDataCopy$default)
# Get real and predicted positive values.
predPositiveValues 	= data.lda.prediction$posterior[,2]
realPositiveValues	= rocDataCopy$default
# Determine and plot ROC curve.
rocPrediction 	= prediction(predPositiveValues, realPositiveValues)

par(mfrow = c(4, 2))
rocPerformance	= performance(rocPrediction, measure = "phi", x.measure = "tpr")
plot(rocPerformance, col = rainbow(10))
rocPerformance	= performance(rocPrediction, measure = "phi", x.measure = "fpr")
plot(rocPerformance, col = rainbow(10))
rocPerformance	= performance(rocPrediction, measure = "phi", x.measure = "tnr")
plot(rocPerformance, col = rainbow(10))
rocPerformance	= performance(rocPrediction, measure = "phi", x.measure = "fnr")
plot(rocPerformance, col = rainbow(10))
rocPerformance	= performance(rocPrediction, measure = "f", x.measure = "tpr")
plot(rocPerformance, col = rainbow(10))
rocPerformance	= performance(rocPrediction, measure = "f", x.measure = "fpr")
plot(rocPerformance, col = rainbow(10))
rocPerformance	= performance(rocPrediction, measure = "f", x.measure = "tnr")
plot(rocPerformance, col = rainbow(10))
rocPerformance	= performance(rocPrediction, measure = "f", x.measure = "fnr")
plot(rocPerformance, col = rainbow(10))