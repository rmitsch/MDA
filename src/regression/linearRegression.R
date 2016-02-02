################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 linear regression
################

# can use data which contains cleaned version of College

library(ISLR)


data <- data_NO_outlier_training

## . takes all columns not used on the left side
data.all.reg <- lm(data$Apps~., data=data)
summary(data.all.reg)


colNames  = c("Private","P.Undergrad","Room.Board","Personal","Terminal","perc.alumni")

data = data[,!(names(data) %in% colNames)]

# plot(data) # scatterplot matrix
data.red1.reg <- lm(data$Apps~., data=data)
summary(data.red1.reg)


colNames  = c("Top25perc","Books","S.F.Ratio","Grad.Rate")

data = data[,!(names(data) %in% colNames)]

# plot(data) # scatterplot matrix
data.red2.reg <- lm(data$Apps~., data=data)
summary(data.red2.reg)



anova(data.all.reg,data.red2.reg)
# decide to omit variables
# alpha = 0.95 seems to be enough here (p-val = 0.002979)



#--------------------------------------------
# test indikator variables not needed, as private was removed
# this column selection was done on a different subset





#	# additive term is by default on PrivateYes, multiplicative was significant for Accept & Room.Board
#	#data.reg <- lm(data$Apps ~ Private + Accept + Top25perc + Outstate + Room.Board + PhD + Expend + Grad.Rate + Accept:Private + Top25perc:Private + Outstate:Private + Room.Board:Private + PhD:Private + Expend:Private + Grad.Rate:Private, data=data)



# #remove insignificant ones
#	data.regBig <- lm(data$Apps ~ Private + Accept + Top25perc + Outstate + Room.Board + PhD + Expend + Grad.Rate + Accept:Private + Room.Board:Private, data=data)

#	data.regSmall <- lm(data$Apps ~ Accept + Top25perc + Room.Board + Expend + Accept:Private, data=data)


#	data.regSmall <- lm(data$Apps ~ Accept + Top25perc + Room.Board:Private, data=data)


#	#test if smaller model is enough
#	anova(data.regSmall,data.regBig)
#	# decide to omit variables
#	# alpha = 0.95 seems to be enough here
#--------------------------------------------


