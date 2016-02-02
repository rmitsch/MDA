# Chapter Regression Smoothing
# k-NN regression

#install.packages("FNN")
library(FNN)

#Normalization function to map to [0,1]
#Source: http://vitalflux.com/data-science-scale-normalize-numeric-data-using-r/ (last access: 25.01.16)
normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

#Convert binary col to numeric, as this knn implementation cannot deal with mixed data-types
dataNumeric <- data_test_NO_outlier
privateData = dataNumeric$Private
privateData = gsub("Yes", "1", privateData)
privateData = gsub("No", "0", privateData)
dataNumeric$Private = as.numeric(privateData)

#normalize
dataNumeric<- as.data.frame(lapply(dataNumeric, normalize))

#test<-cbind( dataNumeric$Expend, dataNumeric$Room.Board, dataNumeric$Apps)
test<-dataNumeric

# k-NN regression, from package FNN
# chosing the k that maximizes R^2
bestK <- 0
bestR2 <- 0
oldR2 <- 0
vR2 <- v[0]
vK <- v[0]
for(i in 1:25) {
	#The results of knn.reg are computed via cross validation.
	knn <- knn.reg(test, y=dataNumeric$Apps, k = i)
	r2 <- knn$R2Pred
	if(r2>bestR2){
		bestR2<-r2
		bestK<-i
	}
	vR2[i]<-r2
	vK[i]<-i
	oldR2<-r2
}
plot(vK, vR2, pch=20, lwd=2, xlab="k", ylab="R^2", main = "k-nn", type="o")
abline(v=seq(0,25,by=5),col="gray",lty=3)
bestK
bestR2


