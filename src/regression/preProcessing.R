###
# find unnecessary columns
# remove unnecessary columns
# remove outliers (>= 2000)
# store everything in variable data 
###

library(car)
library(ISLR)

#Normalization function to map to [0,1]
#Source: http://vitalflux.com/data-science-scale-normalize-numeric-data-using-r/ (last access: 25.01.16)
normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}


# plot(College) # scatterplot matrix


# observe visual: correlation of
#	Enroll & Undergraduate
#	Top10 & Top25


# Test for correlation - compute R_j's by linear regression on each column
# VIF= 1/(1-"R-sqared"^2) > 4 - multicoliniarity, error in slides?

College.reg <- lm(College$Apps~., data=College)## . takes all columns not used on the left side
summary(College.reg)

#-----------------------------------------------------------------------------
## remove colinear columns


# remove response variable for tests
data = College[,!(names(College) %in% c("Apps"))]

#Normalizing
#data_helper = data[,!(names(data) %in% c('Private'))]
#data  <- cbind(College$Private, as.data.frame(lapply( data_helper, normalize)))
#names(data)[names(data)=="College$Private"] <- "Private"

data.reg <- lm(College$Apps~., data=data)
vif(data.reg)


# plot vif of all columns
x <- as.factor(1:17)
v <- vif(data.reg)
plot(v, xaxt='n', type='h', xlab='',ylab='vif')
axis(side=1, at=x, labels=names(data),las=2)
abline(a=4,b=0, col="red")



# remove all columns with vif > 4 (recalculated mode between each call)
data = data[,!(names(data) %in% c("Enroll"))]
data.reg <- lm(College$Apps~., data=data)
vif(data.reg)
max(vif(data.reg))
data = data[,!(names(data) %in% c("F.Undergrad"))]
data.reg <- lm(College$Apps~., data=data)
vif(data.reg)
max(vif(data.reg))
data = data[,!(names(data) %in% c("Top10perc"))]
data.reg <- lm(College$Apps~., data=data)
vif(data.reg)
max(vif(data.reg))
data = data[,!(names(data) %in% c("Outstate"))]
data.reg <- lm(College$Apps~., data=data)
vif(data.reg)
max(vif(data.reg))
data = data[,!(names(data) %in% c("PhD"))]
data.reg <- lm(College$Apps~., data=data)
vif(data.reg)
max(vif(data.reg))



# now all vif's are < 4 
# add back response variable
data$Apps <- College$Apps



# - cols, +rows,+outlier
# reduced columns and all rows WITH outliers
data_outlier_allRows <- data

# - cols, +rows,-outlier
# reduced columns and all rows WITHOUT outliers
data_NO_outlier_allRows <- na.omit(data[data$Apps<2000,])

# - cols, -rows,+outlier
# reduced columns and reduced rows WITH outliers
set.seed(0)
removeIndx <- sample(dim(data)[1])[floor(1:dim(data)[1]*0.75)] # use 75% as training set
data_outlier_training <- na.omit(data_outlier_allRows[removeIndx, ])

# - cols, -rows,-outlier
# reduced columns and reduced rows WITHOUT outliers
data_NO_outlier_training <- na.omit(data_NO_outlier_allRows[removeIndx, ])

# + cols, -rows,+outlier
# all columns and reduced rows
data_test_outlier <- na.omit(College[-removeIndx, ])

# - cols, -rows,+outlier
# all columns and reduced rows
data_test_outlier_reduced <- na.omit(data[-removeIndx, ])


# + cols, -rows,-outlier
# all columns and reduced rows
data_test_NO_outlier <- na.omit(data_test_outlier[data$Apps<2000,])




#Splits data into n CV folds
n=10;


######
# folds 	= NO outlier
# foldsAll 	= with outlier 
######


library(caret)
foldsAll <- createFolds(y=data_outlier_training$Apps, k = n, list = TRUE, returnTrain = FALSE)
#data[ folds[[foldNumber]], ] -> returns this fold


outlierFrame <- data.frame(c(data$Apps>2000),seq(1,dim(data)[1],1))
mul <- ifelse(outlierFrame[1]==TRUE,1,0)
removeIndex <- mul*outlierFrame[2]
removeIndex <- removeIndex[removeIndex > 0]
folds <- vector("list", n )


# create clean folds without outliers
for( i in 1:10 ){	# For each fold (fold i is the test set)
	folds[[i]] <- foldsAll[[i]][which( !foldsAll[[i]] %in% removeIndex)]
}
