################################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 - Cubic Splines
################################

library(ISLR)

# See http://stats.stackexchange.com/questions/57467/how-to-reconstruct-original-data-using-a-small-number-of-principal-components for details.

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

res = princomp(dataWOResponse, center = TRUE, scale = FALSE)
x11()
plot(res, type = "l")
#print(names(res))
print(res)
x11()

# Cumulative explained variance.
plot(cumsum(res$sdev^2/sum(res$sdev^2)))

#print(res$center)
#print(res$loadings[,1:pc.use])
pc.use = 3
trunc <- res$center[1:pc.use] %*% t(res$loadings[,1:pc.use])
print(trunc)
print(summary(res))
