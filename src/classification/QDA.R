################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 - QDA
################

# Quadratic Discriminant Analysis
# Model assumptions:
#	- For common LDA: Same covariance matrix for all groups.
#	- Normally distributed (? -> http://stats.stackexchange.com/questions/113133/does-fisher-linear-discriminant-analysis-lda-require-normal-distribution-of-th)
#	- Groups: 2 -> Default = yes, Default = no.
#
# Important: Linear discriminant analysis maths does not allow for categorical explanatory variables (http://stats.stackexchange.com/questions/131375/explanatory-variables-with-linear-discriminant-analysis).
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

# Train QDA.
data.qda 			= qda(data$default ~ ., data = data)
data.qda.prediction	= predict(data.qda, newdata = dataTest)

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
predPositiveValues 	= data.qda.prediction$posterior[,2]
realPositiveValues	= rocDataCopy$default
# Determine and plot ROC curve.
x11()
rocPrediction 	= prediction(predPositiveValues, realPositiveValues)
rocPerformance	= performance(rocPrediction, measure = "tpr", x.measure = "fpr")
plot(rocPerformance, col = rainbow(10))

# Conclusion: No visible difference between LDA and QDA - was to be expected,
# since covariance matrices seem to be equal.