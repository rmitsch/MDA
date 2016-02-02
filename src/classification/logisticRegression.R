################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 - Logistic regression
################

# Linear Discriminant Analysis
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

# Train  logistic regression.
# Question: Which family?
data.logReg 			= glm(data$default ~ ., data = data, family = binomial)
# Predict using the trained model.
data.logReg.prediction	= predict(data.logReg, newdata = dataTest, type = 'response')
data.logReg.prediction.results <- ifelse(data.logReg.prediction > 0.5,'Yes','No')

print(summary(data.logReg))

# ---------------------------------------------------------------------------

# Confusion matrix for quick test of correctness.
print( confusionMatrix(table(data.logReg.prediction.results, dataTest$default)) )

# ROC curve:
# Convert default to numeric.
#rocDataCopy			= dataTest
#rocDataCopy$default	= gsub("Yes", "1", rocDataCopy$default)
#rocDataCopy$default = gsub("No", "0", rocDataCopy$default)
#rocDataCopy$default = as.numeric(rocDataCopy$default)
# Get real and predicted positive values.
#predPositiveValues 	= data.logReg.prediction$posterior[,2]
#realPositiveValues	= rocDataCopy$default
# Determine and plot ROC curve.
#x11()
#rocPrediction 	= prediction(predPositiveValues, realPositiveValues)
#rocPerformance	= performance(rocPrediction, measure = "tpr", x.measure = "fpr")
#plot(rocPerformance, col = rainbow(10))