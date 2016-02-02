# Author: Marcus Hudec 
library(MASS)
library(UsingR)
attach(emissions)

inter.lsfit <- function(x, y, lab, intercept=T, ...)

# Least sqares fit with interactive deletion of outliers 
# ======================================================
#
# x.......... vector or matrix of explanatory variables
# y.......... numeric vector: dependent variable of regression
# intercept . if FALSE regression is forced through the origin
#

{
obs <- 1:length(x)
repeat 
  {
   ls.out  <- lsfit(x=x, y=y, intercept=intercept)
   fv      <- y-ls.out$residuals
   res     <- ls.diag(ls.out)$std.res
   plot(fv, res, xlab = "Fitted Values", 
        ylab = "Standardized Residuals", ...)
   abline(h=0, lty=2)
   suspect <-identify(fv, res, labels=lab, col="red")
   if (length(suspect) == 0) break
   obs <- obs[-suspect]
   x   <- x[-suspect]
   y   <- y[-suspect]
  }
list(ls.out=ls.out, remain.obs=obs)
}

# OLS-estimation
res.lsfit<-lsfit(perCapita, CO2)
plot(perCapita, CO2, xlab="GDP per Capita", ylab="CO2 - Emission", pch=19,
main=paste("Y = ", round(res.lsfit$coef[1],3), " + ",
             round(res.lsfit$coef[2],3),"*X"))
abline(res.lsfit)
identify(perCapita, CO2, label=rownames(emissions))


res.lsfit<-lsfit(perCapita, CO2)
ls.print(res.lsfit)

res.inter <- inter.lsfit(perCapita, CO2, lab=rownames(emissions), pch=19)
ls.print(res.inter$ls.out)
res.inter$remain.obs


xCO2 <- log(CO2)
res.logfit<-lsfit(perCapita, xCO2)
ls.print(res.logfit)


# ========================================================

plot(perCapita, CO2, xlab="GDP per Capita", ylab="CO2 - Emission", pch=19)
xi <- seq(from=min(perCapita), to=max(perCapita), length=100)

res.lsfit<-lsfit(perCapita, CO2)             
abline(res.lsfit)

abline(res.inter$ls.out, col=2)


xCO2 <- log(CO2)
res.logfit<-lsfit(perCapita, xCO2)
lines(xi, exp(res.logfit$coef[1]+res.logfit$coef[2]*xi), col=3)




legend(5000, 6500, c(
        paste("OLS-Line       Y = ", round(res.lsfit$coeff[1], 3), "  + ", 
                      round(res.lsfit$coeff[2], 6), " * X"),
        paste("OLS without US Y = ", round(res.inter$ls.out$coeff[1], 3), "  + ", 
                      round(res.inter$ls.out$coeff[2], 6), " * X"),                      
        paste("OLS-Line   ln(Y) = ", round(res.logfit$coeff[1], 3), "  + ", 
                      round(res.logfit$coeff[2], 6), " * X")),          
                      lty = c(1,1,1), col = c(1,2,3), lwd=c(1,2,2))               
   
# ================================================================
plot(perCapita, CO2, xlab="GDP per Capita", ylab="CO2 - Emission", pch=19,
ylim=c(0,3000))
xi <- seq(from=min(perCapita), to=max(perCapita), length=100)

res.lsfit<-lsfit(perCapita, CO2)             
abline(res.lsfit)

res.lts <- lqs(CO2 ~ perCapita)
res.lts
abline(res.lts, col=4)

res.huber <- rlm(CO2 ~ perCapita)
summary(res.huber)
abline(res.huber, col=2)


res.biweight <- rlm(CO2 ~ perCapita, method="MM")
summary(res.biweight)
abline(res.biweight, col=3)

legend(5000, 2800,c(
        paste("OLS-Line    Y = ", round(res.lsfit$coeff[1], 3), "  + ", 
                      round(res.lsfit$coeff[2], 6), " * X"),
        paste("Huber M-estimate Y = ", round(res.huber$coeff[1], 3), "  + ", 
                      round(res.huber$coeff[2], 6), " * X"),
        paste("Bisquare M-estimate Y = ", round(res.biweight$coeff[1], 3), "  + ", 
                      round(res.biweight$coeff[2], 6), " * X"),
        paste("LTS-Line    Y = ", round(res.lts$coeff[1], 3), "  + ", 
                      round(res.lts$coeff[2], 6), " * X")),             
        lty = c(1,1,1,1), col = c(1,2,3,4), lwd=c(2,2,2,2))
        
   