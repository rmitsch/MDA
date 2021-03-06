# Author: Marcus Hudec 

library(faraway)
library(car)
library(QuantPsyc)
data(chredlin)
help(chredlin)

head(chredlin)  
attach(chredlin)

# Multiple Regression Model
res <- lm(involact ~ race + fire + theft + age + income)
res

summary(res)

summary(lm(involact ~ income))

anova(res)
Anova(res)

# Standardized Regression Coefficient
coef(res)

c (coef(res)[2]*sd(race)/sd(involact),
   coef(res)[3]*sd(fire)/sd(involact),
   coef(res)[4]*sd(theft)/sd(involact),
   coef(res)[5]*sd(age)/sd(involact),
   coef(res)[6]*sd(income)/sd(involact))

lm.beta(lm(involact ~ race + fire + theft + age + income))

# Semipartial Correlation
cor(involact, income)

income.all_other <- lm(income ~ race + fire + theft + age)$residuals
cor(involact, income.all_other)       # semipartial correlation

summary(lm(involact ~ income.all_other))
cor(involact, income.all_other)^2 



# General Model Test
big   <- lm(involact ~ race + fire + theft + age + income)
small <- lm(involact ~ fire + theft)

summary(big)
summary(small)

anova(small, big)

# Final Model After Removal of Income
res <- lm(involact ~ race + fire + theft + age)
summary(res)

plot(res)                   # Diagnostic Plots
par(mfrow=c(2,2))
plot(res) 
par(mfrow=c(1,1))

# -----------------------------------------------------------------
# MODEL SELECTION
# -----------------------------------------------------------------

# Needs some additional libraries
library(e1071)
library(leaps)
Xp <- chredlin[, c(-5,-7)]          # Matrix of possible Regressors
Y  <- chredlin[, 5]
res.leaps.Cp    <- leaps(Xp, Y, method="Cp", names=dimnames(Xp)[[2]])
res.leaps.Cp

res.leaps.adjr2 <- leaps(Xp, Y, method="adjr2", names=dimnames(Xp)[[2]])
res.leaps.adjr2

res.leaps.r2    <- leaps(Xp, Y, method="r2", names=dimnames(Xp)[[2]])
res.leaps.r2


# ==================================================================
leaps.print<-function(x, plot.leaps = T, ...)
# x ... object generated by leap
{
label.model <- apply(x$which, 1, function(x, lab.var)
               (paste(lab.var[x],collapse=" ")), x$label[-1])
if (!is.null(x$Cp))
   { criterion <- x$Cp
     crit.lab  <- "Cp" }
else
   if (!is.null(x$adjr2))
      { criterion <- x$adjr2
        crit.lab  <- "adjR2" }
   else
      if (!is.null(x$r2))
         { criterion <- x$r2
           crit.lab  <- "R2" }
p <- max(x$size)
ranking <-sort.list(criterion, decreasing=(crit.lab!="Cp"))
print(paste(sprintf("%10.2f", criterion[ranking]), label.model[ranking]))
if (plot.leaps)
{plot(x$size-1, criterion, type="n", ylab=crit.lab,
     xlim=c(0.5,p-0.5), xaxp = c(1, p-1, p-2), xlab="# Regressor-Variables",
     main="Best Subset Regression Models", ...)
text(x$size-1, criterion, label.model, cex=0.7,adj=c(0.5,0))}
}
# ======================================================================

leaps.print(res.leaps.Cp)

# From Library e1071
res.regsub <- regsubsets(Xp, Y, nbest=3)
res.regsub
summary(res.regsub, all.best=F)
summary(res.regsub, all.best=T)
plot(res.regsub)

subsets(res.regsub)
plot(regsubsets(involact~., data=chredlin))

# Stepwise Algorithms
# BACKWARD
res <- lm(involact ~ race + fire + theft + age + income, data=chredlin)
step(res)
# FORWARD
res.null <- lm(involact ~ 1, data=chredlin)
step(res.null, 
     scope=list(lower = ~1, upper = ~ race + fire + theft + age + income), 
     data=chredlin)
     
     
# Influence Measures
res <- lm(involact ~ race + fire + theft + age)
hi  <- lm.influence(res)$hat     
mean(hi)
length(hi)
5/47
plot(hi, type="h", main="Diagonal Values of Hat-Matrix")
abline(h=mean(hi), col="grey")
abline(h=2*mean(hi), col="red", lty=2, lwd=2)
abline(h=3*mean(hi), col="red", lwd=2)

dffits(res)
plot(dffits(res), type="h", main="DFFITS")
abline(h=2*sqrt(length(res$coef)/length(hi)), col="red", lty=2, lwd=2)
abline(h=-2*sqrt(length(res$coef)/length(hi)), col="red", lty=2, lwd=2)

cooks.distance(res)
plot(cooks.distance(res), type="h", main="Cooks Distance")
abline(h=4/length(hi), col="red", lty=2, lwd=2)

vif(res)
