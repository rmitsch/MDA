################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 linear regression
################

library(ISLR)

## . takes all columns not used on the left side
College.reg <- lm(College$Apps~., data=College)
summary(College.reg)

# compute model without highly significant columns (<0.001):
# Private Accept Enroll Top10perc Outstate Room.Board Expend
College.reg <- lm(College$Apps~.-Private-Accept-Enroll-Top10perc-Outstate-Room.Board-Expend, data=College) # cannot skip all Rsq was 0.92 now 0.72
summary(College.reg)



#residuals
plot(rstandard(College.reg))
plot(rstudent(College.reg))

# leverage - "unusual in X"
hatvalues(College.reg)
lm.influence(College.reg)$hat

#dffits
dffits(College.reg)


# cook's distance - influental observations
cooks.distance(College.reg)
plot(College.reg, which=4)
