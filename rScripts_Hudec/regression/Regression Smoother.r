library(UsingR)
library(splines)

# Development of Dow Jones Index
attach(dowdata)
head(dowdata)

n   <- length(dowdata$High)
x.time <- 1:n

lag <- c(10, 30, 50)

# Moving Average

plot(lag[3]:n, dowdata$High[50:n], type="l", col="grey", lwd=2,
xlab="Time", ylab="Dow Jones Index", main = "Moving Average")


for (i in 1:3)
lines((lag[i]-lag[i]/2+1):(n-lag[i]/2), simple.lag(dowdata$High, lag[i]),
       type="l", col=i+1, lwd=2)
legend(50,9800,
       c("window-size=10", "window-size=30","window-size=50"),
        lty = c(1,1,1), col = c(2,3,4), lwd=c(2,2,2), cex=1)

# Moving Median

plot(lag[3]:n, dowdata$High[50:n], type="l", col="grey", lwd=2,
xlab="Time", ylab="Dow Jones Index", main = "Moving Median")


for (i in 1:3)
lines((lag[i]-lag[i]/2+1):(n-lag[i]/2), 
       simple.lag(dowdata$High, lag[i], FUN=median),
       type="l", col=i+1, lwd=2)
legend(50,9800,
       c("window-size=10", "window-size=30","window-size=50"),
        lty = c(1,1,1), col = c(2,3,4), lwd=c(2,2,2), cex=1)
  
# Naive Regression
plot(x.time, dowdata$High, pch=20, col="darkgrey", cex=1.2,
xlab="Time", ylab="Dow Jones Index", main = "Naive Regression")

x.breaks = cut(x.time, breaks=20, labels=F)
x.naive <- by(x.time, x.breaks, mean)
x.cut   <- by(x.time, x.breaks, max)
y.naive <- by(dowdata$High, x.breaks, mean)
lines( x.naive, y.naive , pch=19, cex=1.5, type="o", lwd=2, col=2)
abline(v=x.cut, col="grey", lwd=0.8)

# Verpackt als Funktion
naive.reg <- function(x, y, interv=10, type=mean, ...)
{
plot(x, y, pch=20, col="darkgrey", cex=1.2, ...)
x.breaks = cut(x, breaks=interv, labels=F)
x.naive <- by(x, x.breaks, type)
x.cut   <- by(x, x.breaks, max)
y.naive <- by(y, x.breaks, type)
lines( x.naive, y.naive , pch=19, cex=1.5, type="o", lwd=2, col=2)
abline(v=x.cut, col="grey", lwd=0.8)
}

naive.reg(x.time, dowdata$High, xlab="Time", ylab="Dow Jones Index", 
          main = "Naive Regression")
           
naive.reg(x.time, dowdata$High, type=median, interv=15, xlab="Time", 
          ylab="Dow Jones Index", main = "Naive Regression") 

# Piecewise Constant
const.reg <- function(x, y, interv=10, type=mean, ...)
{
plot(x, y, pch=20, col="darkgrey", cex=1.2, ...)
x.breaks = cut(x, breaks=interv, labels=F)
x.naive <- by(x, x.breaks, type)
x.cut   <- c(0, by(x, x.breaks, max))
y.naive <- by(y, x.breaks, type)
for (i in 1:(length(x.breaks)-1))
   lines(c(x.cut[i], x.cut[i+1]), c(y.naive[i],y.naive[i]), col=2)
abline(v=x.cut, col="grey", lwd=0.8)
}

const.reg(x.time, dowdata$High, xlab="Time", ylab="Dow Jones Index", 
          main = "Piecewise Constant Regression")


# Lokale Funktionen
# -------------------------------------------------------------
piecewise <- function(x, y, wo=2, vertical=T, ...)

{
	if(length(wo) == 1)
		wo <- quantile(x, probs = seq(0, 1, 1/wo))
	bruch <- (cut(x, wo,labels=F))
	bruch[is.na(bruch)] <- 1
	res <- vector("list", max(bruch))
  plot(x, y, ...)
  for(i in 1:length(res)) {   
    res[[i]] <- lm(y ~ x, subset = (bruch == i))
		xp <- wo[i:(i + 1)]
		yp <- xp * res[[i]]$coefficients[2] + res[[i]]$coefficients[1]
		lines(xp, yp, col=2)
	}
  if (vertical) abline(v=wo, lty=2)
	res
}


# Broken Stick Regression
piecewise(x.time, dowdata$High, 10, xlab="Time", ylab="Dow Jones Index", 
          main = "Piecewise Linear Regression")


# Local Regression
alpha = c(0.1, 0.35, 0.75)
plot(x.time, dowdata$High, pch=16, col="grey", 
     xlab="Time", ylab="Dow Jones Index", main = "Different Linear Loess Fits") 
for (i in 1:3)     
{
local.reg <- loess(dowdata$High ~ x.time, span=alpha[i], degree=1)
lines(x.time, local.reg$fitted, lwd=2, col=i+1)
}

legend(10,9800,c(
        expression(alpha == 0.10),
        expression(alpha == 0.35),
        expression(alpha == 0.75)),
        lty = c(1,1,1), col = c(2,3,4), lwd=c(2,2,2), cex=1)

# Local Regression
alpha = c(0.1, 0.35, 0.75)
plot(x.time, dowdata$High, pch=16, col="grey", 
     xlab="Time", ylab="Dow Jones Index", main = "Different Quadratic Loess Fits") 
for (i in 1:3)     
{
local.reg <- loess(dowdata$High ~ x.time, span=alpha[i], degree=2)
lines(x.time, local.reg$fitted, lwd=2, col=i+1)
}

legend(10,9800,c(
        expression(alpha == 0.10),
        expression(alpha == 0.35),
        expression(alpha == 0.75)),
        lty = c(1,1,1), col = c(2,3,4), lwd=c(2,2,2), cex=1)

# Cubic Splines  
plot(x.time, dowdata$High, pch=16, col="grey", 
     xlab="Time", ylab="Dow Jones Index", main = "Fitted B-Splines") 
res.bs <- lm(dowdata$High ~ bs(x.time, df=10))
lines(x.time, fitted(res.bs), col=2, lwd=2)
res.bs <- lm(dowdata$High ~ bs(x.time, df=20))
lines(x.time, fitted(res.bs), col=3, lwd=2)
res.bs <- lm(dowdata$High ~ bs(x.time, df=70))
lines(x.time, fitted(res.bs), col=4, lwd=2)
legend(10,9800,c("k=10","k=20", "k=70"),
        lty = c(1,1,1), col = c(2,3,4), lwd=c(2,2,2), cex=1)

# Natural Splines  (auch Restricted Cubic Splines)
plot(x.time, dowdata$High, pch=16, col="grey", 
     xlab="Time", ylab="Dow Jones Index", main = "Fitted N-Splines") 
res.ns <- lm(dowdata$High ~ ns(x.time, df=10))
lines(x.time, fitted(res.ns), col=2, lwd=2)
res.ns <- lm(dowdata$High ~ ns(x.time, df=20))
lines(x.time, fitted(res.ns), col=3, lwd=2)
res.ns <- lm(dowdata$High ~ ns(x.time, df=70))
lines(x.time, fitted(res.ns), col=4, lwd=2)
legend(10,9800,c(
        "k=10","k=20", "k=70"),
        lty = c(1,1,1), col = c(2,3,4), lwd=c(2,2,2), cex=1)

