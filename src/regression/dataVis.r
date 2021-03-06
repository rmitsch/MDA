################################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 - Outlier detection
################################

library(ISLR)

# Scatterplot matrix of all variables.
X11()
#old
#pairs(~Private+Apps+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+
#	Room.Board+Books+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate,
#	data = data, main="Simple Scatterplot Matrix")

pairs(~Private+Apps+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+
	Room.Board+Books+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate,
	data = College, main="Simple Scatterplot Matrix", pch='.')

	
# Plot various measures to detect outliers.
data.reg <- lm(data$Apps~., data=data)
# Plot result.
plot(data.reg)




dataNumeric <- College#data_outlier_training
privateData = College$Private#data_outlier_training$Private
privateData = gsub("Yes", "1", privateData)
privateData = gsub("No", "0", privateData)
dataNumeric$Private = as.numeric(privateData)

# Mostly taken from http://www.statmethods.net/graphs/scatterplot.html
library(gclus)
library(plotrix)
dta <- dataNumeric # get data
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r, breaks=c(-1,0,.3,.5,.8,1),rainbow(5))#dmat.color(dta.r) # get colors
dta.o <- order.single(dta.r)
cpairs(dta, cex=1.7,dta.o, panel.colors=dta.col, gap=.5, oma=c(3,3,5,13), cex.labels = 0.9, main="Variables Ordered and Colored by Correlation", pch='.' )
color.legend(xl=0.9,yb=0,xr=0.97,yt=0.1, align="rb", gradient="y",cex=0.7,legend=c("-1 to 0","0 to 0.3","0.3 to 0.5","0.5 to 0.8","0.8 to 1"),rect.col=rainbow(5));

#i chose this color breaks and scheme to make the important parts most visible
