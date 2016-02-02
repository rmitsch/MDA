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
library(lmtest)
library(het.test)

# Chosen dataset: Credit card default. 

# Following: Tests about
#	(1) normal distribution of numeric non-response variables, 
#	(2) covariance between data,
#	(3) relevance if subject is student or not,
#	(4)	heteroscedasticity.

# ---------------------------------------------------------------------------

# Test 1: Is data normally distributed?
# Execute Shapiro-Wilk test for all non-response, numeric columns.
x11()
par(mfrow = c(2, 1))
print(shapiro.test(head(data$balance, n = 5000)))
print(shapiro.test(tail(data$balance, n = 5000)))
qqnorm(data$balance, main = 'LDA - Normal Q-Q Plot for Balance')
print(shapiro.test(head(data$income, n = 5000)))
print(shapiro.test(tail(data$income, n = 5000)))
qqnorm(data$income, main = 'LDA - Normal Q-Q Plot for Income')

# Conclusion: Balance is (except at the beginning) probably normally distributed,
# deviates from normal distribution. (The Shapiro tests seem to indicate no 
# normal distribution.)
# Although LDA gives no guarantee about the generalization error, we nonetheless use
# it - at least for testing purposes.

# Test 2: Same covariance matrix for all groups?
# Copy data, convert student to numerical type.
dataCopy			= data
dataCopy$student	= gsub("Yes", "1", dataCopy$student)
dataCopy$student 	= gsub("No", "0", dataCopy$student)
dataCopy$student 	= as.numeric(dataCopy$student)
# Seperate in groups.
dataCopyDNo			= dataCopy[which(dataCopy$default == 'No'), ]
dataCopyDYes		= dataCopy[which(dataCopy$default == 'Yes'), ]
# Remove responSe column.
dataCopyDNo 		= dataCopyDNo[,!(names(data) %in% c('default'))]
dataCopyDYes 		= dataCopyDYes[,!(names(data) %in% c('default'))]
# Calculate covariance matrix.
covDefaultNo 		= cov(dataCopyDNo)
covDefaultYes 		= cov(dataCopyDYes)
x11()
compareCov(covDefaultYes, covDefaultNo, labels = c('Default = Yes', 'Default = No'))

# Conclusion: Covariance matrices are strongly similar. QDA shouldn't yield
# results much different from LDA's results.

# --------------------------------------------------------------------------

# Test 3: Does it make a difference if subject is a student or not?
# We apply logistic regression
logReg 			= glm(default ~ ., data = data, family = 'binomial')
studentLogReg 	= glm(default ~ . * student, data = data, family = 'binomial')
print( summary(anova(logReg, studentLogReg)) )
# Perform chi-squared test to compare models with and without grouped student values.
# H0: Slopes/planes of model are not related to column 'student'.
print( anova(logReg, studentLogReg, test="Chisq") )
# Conclusion: H0 cannot be rejected; no indication that being a student is relevant.

# --------------------------------------------------------------------------

# Test 4: Test if heteroscedasticity occurs.
dataCopy$default = ifelse(dataCopy$default == 'Yes', TRUE, FALSE)
print(nrow(which(dataCopy$default == 1)))

print(bptest(default ~ ., data = dataCopy, studentize = FALSE))
