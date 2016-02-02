###
# find unnecessary columns
# remove unnecessary columns
# remove outliers (>= 2000)
# store everything in variable data 
###

library(car)
library(ISLR)

dataOriginal <- College
College.reg <- lm(College$Apps~., data=College)## . takes all columns not used on the left side
summary(College.reg)

# see "lm_Full.png" -> remove columns that are not significant


colNames  = c("P.Undergrad","Books","Personal","Terminal","S.F.Ratio","perc.alumni")

data = College[,!(names(College) %in% colNames)]

# plot(data) # scatterplot matrix
data.reg <- lm(data$Apps~., data=data)



# observe visual: correlation of
#	Enroll & Undergraduate
#	Top10 & Top25


# Test for correlation - compute R_j's by linear regression on each column
# VIF= 1/(1-"R-sqared"^2) > 4 - multicoliniarity, error in slides?


x <- as.factor(1:12)
v <- vif(data.reg)
plot(v, xaxt='n', type='h', xlab='',ylab='vif')
axis(side=1, at=x, labels=names(data),las=2)
abline(a=4,b=0, col="red")



# remove Enroll (vif=21.052281)
data = data = data[,!(names(data) %in% c("Enroll"))]

data.reg <- lm(data$Apps~., data=data)
vif(data.reg)
# remove Top10perc (vif=6.672607)
data = data = data[,!(names(data) %in% c("Top10perc"))]


#remove F.Undergrad (vif=5.904212)
data = data = data[,!(names(data) %in% c("F.Undergrad"))]

data.reg <- lm(data$Apps~., data=data)
vif(data.reg)

# now all vif's are < 4



# in linear Regression smaller model was enough so remove Private,Apps,Outstate, PhD, Expend, Grad.Rate and Room.board

Private_01 <- ifelse(data$Private=="Yes",1,0)
data$Room.Board_x_Private <- data$Room.Board*Private_01

colNames  = c("Outstate", "PhD", "Expend", "Grad.Rate","Room.Board","Private")
data = data = data[,!(names(data) %in% colNames)]



#######	old version
# dataAll <- data
## remove outliers
# data <- data[data$Apps<2000,]
#######


# EVERYTHING columns and all rows
data_original <- College

# - cols, +rows,+outlier
# reduced columns and all rows WITH outliers
data_outlier_allRows <- data

# - cols, +rows,-outlier
# reduced columns and all rows WITHOUT outliers
data_NO_outlier_allRows <- data[data$Apps<2000,]

# - cols, -rows,+outlier
# reduced columns and reduced rows WITH outliers
set.seed(0)
removeIndx <- sample(dim(data)[1])[floor(1:dim(data)[1]*0.75)] # use 75% as training set
data_outlier_training <- data_outlier_allRows[removeIndx, ]

# - cols, -rows,-outlier
# reduced columns and reduced rows WITHOUT outliers
data_NO_outlier_training <- data_NO_outlier_allRows[removeIndx, ]

# + cols, -rows,+outlier
# all columns and reduced rows
data_test_outlier <- College[-removeIndx, ]


# + cols, -rows,-outlier
# all columns and reduced rows
data_test_NO_outlier <- data_test_outlier[data$Apps<2000,]




#Splits data into n CV folds
n=10;

library(caret)
folds <- createFolds(y=data_NO_outlier_training$Apps, k = n, list = TRUE, returnTrain = FALSE)
foldsAll <- createFolds(y=data_outlier_training$Apps, k = n, list = TRUE, returnTrain = FALSE)

#data[ folds[[foldNumber]], ] -> returns this fold


