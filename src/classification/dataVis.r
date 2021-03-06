################################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 - Visualization
################################

library(ISLR)

# Scatterplot matrix of all variables.
X11()
#pairs(~Private+Apps+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+
#	Room.Board+Books+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate,
#	data = data, main="Simple Scatterplot Matrix")
plot(data)

dev.new()

#Histograms/barcharts of every variable. E.g. to illustrate imbalanced class.
par(mfrow=c(2,2))
plot(data$default, main="default")
hist(data$income, main="income")
hist(data$balance, main="balance")
plot(data$student, main="student")



dataNumeric <- data
studentData = data$student
studentData = gsub("Yes", "1", studentData)
studentData = gsub("No", "0", studentData)
dataNumeric$student = as.numeric(studentData)

defaultData = data$default
defaultData = gsub("Yes", "1", defaultData)
defaultData = gsub("No", "0", defaultData)
dataNumeric$default = as.numeric(defaultData)



# Mostly taken from http://www.statmethods.net/graphs/scatterplot.html
library(gclus)
library(plotrix)
dta <- dataNumeric # get data
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r, breaks=c(-1,0,.3,.5,.8,1),rainbow(5))#dmat.color(dta.r) # get colors
dta.o <- order.single(dta.r)
cpairs(dta, cex=1.7,dta.o, panel.colors=dta.col, gap=.5, oma=c(3,3,5,13), cex.labels = 0.9, main="Variables Ordered and Colored by Correlation", pch='.' )
color.legend(xl=0.85,yb=0.4,xr=0.92,yt=0.6, align="rb", gradient="y",cex=0.7,legend=c("-1 to 0","0 to 0.3","0.3 to 0.5","0.5 to 0.8","0.8 to 1"),rect.col=rainbow(5));

#i chose this color breaks and scheme to make the important parts most visible


