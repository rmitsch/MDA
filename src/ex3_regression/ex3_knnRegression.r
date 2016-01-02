################################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 - KNN Regression
################################

library(ISLR)
library(FNN)

print(College)
# TODO How to detect outliers?
# Modelling assumptions: None.

responseColNames 	= c("Apps")
CollegeWOResponse	= College[,!(names(College) %in% responseColNames)]


# Apply KNN with leave-one-out cross-validation.
knn.reg(CollegeWOResponse, test = NULL, y = College$Apps, k = 3)