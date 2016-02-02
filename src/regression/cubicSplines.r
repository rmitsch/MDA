################################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 - Cubic Splines
################################

library(ISLR)
library(mgcv)
library(gamclass)

# Operate on copy of data.
data2 = data

# Convert attribute 'Private' to numeric.
privateData = data2$Private
privateData = gsub("Yes", "1", privateData)
privateData = gsub("No", "0", privateData)
data2$Private = as.numeric(privateData)

# Get data without response column.
responseColNames  = c("Apps")
dataWOResponse	= data2[,!(names(data2) %in% responseColNames)]
print (dataWOResponse)

# Perform cubic spline interpolation.
# Documentation: 	https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.construct.cr.smooth.spec.html
# SO entry: 	http://stackoverflow.com/questions/6370361/is-there-an-implementation-of-loess-in-r-with-more-than-3-parametric-predictors
data.splines = gam(Apps ~ 	Private+Accept+Top25perc+P.Undergrad+
					Room.Board+Books+Personal+Terminal+
					S.F.Ratio+perc.alumni+Expend+Grad.Rate,
					data=data2)

# Cross-validation.
#data.splines.CV = CVgam(Apps ~ 	Private+Accept+Enroll+Top10perc+
#						Top25perc+F.Undergrad+P.Undergrad+Outstate+
#						Room.Board+Books+Personal+PhD+Terminal+
#						S.F.Ratio+perc.alumni+Expend+Grad.Rate, 
#						data=data2, nfold = 10, printit = TRUE)
#print(predict(data.splines, dataWOResponse))
