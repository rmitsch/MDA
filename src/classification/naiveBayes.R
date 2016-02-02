################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 Naive Bayes
################

library("e1071");

#Test if predictors are independent, given the class variable
splittedData <- split(x=data, f=data$default)

#Testing the model assumption of independence
#High p values indicate that we cannot reject the null-hypothesis of independence
#Adjusted p-value: 0,05/6 - but even without it the results don't change
for( i in 1:2 ){
	tbl = table(splittedData[[i]]$student, splittedData[[i]]$balance)
	 print(chisq.test(tbl))
	tbl = table(splittedData[[i]]$student, splittedData[[i]]$income) 
	 print(chisq.test(tbl))
	tbl = table(splittedData[[i]]$balance, splittedData[[i]]$income)
	 print(chisq.test(tbl))
}

#This implementation also assumes gaussian distribution of the predictors, given the target class.
#This condition is not met, given the tests in the AssumtionCheck.r script.

#Since all possible categorical values are in the dataset, no laplacian smoothing is needed.
naiveBayes <- naiveBayes(data$default~., data=data)
naiveBayes

# predict(naiveBayes, newdata=dataTest)