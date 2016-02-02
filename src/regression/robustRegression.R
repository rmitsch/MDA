################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 Robust Regression
################

#Robust regression with different weighting functions
library(MASS)

#First: summaries of models using all data

#Robust M-Estimator with Huber-Weight -> puts less weight on extreme values
hubert <- rlm(data_test_outlier$Apps~., data=data_test_outlier)
summary(hubert)

plot(hubert, las=1)

# Robust M-Estimator with Bisquare-Weight -> same, but levels off after some time
bisquare <- rlm(data_test_outlier$Apps~., data=data_test_outlier, method="MM")
summary(bisquare)

# plot(bisquare, las=1) does not work for this kind of model


# LTS-FIT -> has a high breakdown point (can resist a lot of 'bad' data), disregards outliers
lts <- lqs(data_test_outlier$Apps~., data=data_test_outlier)
summary(lts$coefficients)

#plot(lts, las=1) does not work for this kind of model
