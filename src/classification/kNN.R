################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 knn
################

#install.packages("class")
unloadNamespace("e1071")
library("class")

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

#dataNumeric <- as.data.frame(lapply( dataNumeric , normalize))

#k-nn: find best k ba using CV on the training set (model selection)
oldResult <- 0
vK<-c()
vResult<-c()
for( k in 1:25 ){ # Test k from 1 to 25

	correct <- 0
	total <- 0

	vKnnResults <- v[0]
	vCorrectResults <- v[0]

	for( i in 1:10 ){	# For each fold (fold i is the test set)
		
		# Create trining and test set
		testSet <- dataNumeric[ folds[[i]], ]
		firstTrainingFold<-0
		if( (i!=1) ){
			trainingSet <- dataNumeric[ folds[[1]], ]
			firstTrainingFold<-1
		}else{
			trainingSet <- dataNumeric[ folds[[2]], ]
			firstTrainingFold<-2
		}
		for(j in 1:10){ #unify training folds
			if(i!=j && j!=firstTrainingFold){
				trainingSet<- rbind(trainingSet,dataNumeric[ folds[[j]], ])
			}
		}

		trainingSetWithoutClass= trainingSet[,!(names(trainingSet) %in% c("default"))]
		testSetWithoutClass= testSet [,!(names(testSet ) %in% c("default"))]

		knnResult<-knn(train=trainingSetWithoutClass, test=testSetWithoutClass,cl=trainingSet$default, k = k)
		correct <- correct + sum(testSet$default == knnResult)
		total <- total + length(testSet$default);

		vKnnResults <- c(vKnnResults ,knnResult)
		vCorrectResults <- c(vCorrectResults ,testSet$default)

	}
	#print(confusionMatrix(data=vKnnResults , reference=vCorrectResults ))
	result <- confusionMatrix(data=vKnnResults , reference=vCorrectResults )$byClas[[8]]#balanced accuracy
	vResult[k]<-result 
	vK[k]<-k

	oldResult <- vResult[i]
}

plot(vK, vResult, pch=20, lwd=2, xlab="k", ylab="Balanced Accuracy", main = "k-nn", type="o")
abline(v=seq(0,25,by=5),col="gray",lty=3)
