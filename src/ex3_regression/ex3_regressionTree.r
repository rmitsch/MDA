################################
##	MDA WS 15/16
##	Linhardt, Mitsch, Schwarzl
##	Exercise 3 - Regression Tree
################################

library(ISLR)
library(rpart)

# TODO How to detect outliers?
# Modelling assumptions: None.

# Train regression tree.
fit = rpart(College$Apps~., data=College, method="anova")

# Display cp table.
printcp(fit)

# Plot cross validation results.
X11()
plotcp(fit)

# Plot decison tree.
X11()
plot(fit, uniform=TRUE, main="Unpruned Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# Plot approx. r-squared and relative error.
#print(rsq.rpart(fit))

# Post-prune tree to avoid overfitting.
pfit = prune(fit, fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
# plot the pruned tree
X11()
plot(pfit, uniform=TRUE, main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)