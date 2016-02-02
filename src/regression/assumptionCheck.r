################################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 linear regression
################################

# Check of model assumptions.
library("e1071");
library(MASS)
library(covmat)
library(caret)
library(ROCR)
library(ISLR)
library(lmtest)

# Chosen dataset: Credit card default. 

# Following: Tests about
#	(1) normality of data/errors,
#	(2) heteroscedasticity,
#	(3) differences in covariance matrices,
#	(x) independence of data -> see covariance check in preprocessing.

# ---------------------------------------------------------------------------

#Private
#Apps
#Accept
#Top25perc
#P.Undergrad
#Room.Board
#Books
#Personal
#Terminal
#S.F.Ration
#perc.alumni
#Expend
#Grad.Rate

# Test 1: Is data/are errors normally distributed?
# Execute Shapiro-Wilk test for all non-response, numeric columns.
print(head(data_NO_outlier_allRows, n = 5))
print(shapiro.test(data_NO_outlier_allRows$Apps))
print(shapiro.test(data_NO_outlier_allRows$Accept))
print(shapiro.test(data_NO_outlier_allRows$Top25perc))
print(shapiro.test(data_NO_outlier_allRows$P.Undergrad))
print(shapiro.test(data_NO_outlier_allRows$Room.Board))
print(shapiro.test(data_NO_outlier_allRows$Books))
print(shapiro.test(data_NO_outlier_allRows$Personal))
print(shapiro.test(data_NO_outlier_allRows$Terminal))
print(shapiro.test(data_NO_outlier_allRows$S.F.Ratio))
print(shapiro.test(data_NO_outlier_allRows$perc.alumni))
print(shapiro.test(data_NO_outlier_allRows$Expend))
print(shapiro.test(data_NO_outlier_allRows$Grad.Rate))
x11()
par(mfrow = c(4, 3))
qqnorm(data_NO_outlier_allRows$Apps, main = 'LDA - Normal Q-Q Plot for Apps')
qqnorm(data_NO_outlier_allRows$Accept, main = 'LDA - Normal Q-Q Plot for Accept')
qqnorm(data_NO_outlier_allRows$Top25perc, main = 'LDA - Normal Q-Q Plot for Top25perd')
qqnorm(data_NO_outlier_allRows$P.Undergrad, main = 'LDA - Normal Q-Q Plot for Undergrad')
qqnorm(data_NO_outlier_allRows$Room.Board, main = 'LDA - Normal Q-Q Plot for Board')
qqnorm(data_NO_outlier_allRows$Books, main = 'LDA - Normal Q-Q Plot for Books')
qqnorm(data_NO_outlier_allRows$Personal, main = 'LDA - Normal Q-Q Plot for Personal')
qqnorm(data_NO_outlier_allRows$Terminal, main = 'LDA - Normal Q-Q Plot for Terminal')
qqnorm(data_NO_outlier_allRows$S.F.Ratio, main = 'LDA - Normal Q-Q Plot for S.F.Ratio')
qqnorm(data_NO_outlier_allRows$perc.alumni, main = 'LDA - Normal Q-Q Plot for alumni')
qqnorm(data_NO_outlier_allRows$Expend, main = 'LDA - Normal Q-Q Plot for Expend')
qqnorm(data_NO_outlier_allRows$Grad.Rate, main = 'LDA - Normal Q-Q Plot for Rate')

# Conclusion: Most columns are not normally distributed.

# Test 3: Same covariance matrix for all groups?
# Copy data, convert student to numerical type.
dataCopy			= College
# Seperate in groups.
dataCopyDNo			= dataCopy[which(dataCopy$Private == 'No'), ]
dataCopyDYes		= dataCopy[which(dataCopy$Private == 'Yes'), ]
# Remove respone column.
dataCopyDNo 		= dataCopyDNo[,!(names(data) %in% c('Private'))]
dataCopyDYes 		= dataCopyDYes[,!(names(data) %in% c('Private'))]
# Calculate covariance matrix.
covDefaultNo 		= cov(dataCopyDNo)
covDefaultYes 		= cov(dataCopyDYes)
x11()
compareCov(covDefaultYes, covDefaultNo, labels = c('Private = Yes', 'Private = No'))

# Conclusion: Covariance matrices are strongly similar.

# Test 2: Test if heteroscedasticity occurs.
print(bptest(Apps ~ ., data = data_NO_outlier_allRows, studentize = TRUE))