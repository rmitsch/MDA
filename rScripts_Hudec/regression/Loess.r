# Demonstration of Loess Estimation
library(car)  

UN <- na.omit(UN)

gdp <- UN$gdp
infant <- UN$infant.mortality

ord <- order(gdp)       # sort data by gdp
gdp <- gdp[ord]
infant <- infant[ord]

x0 <- gdp[150]           
dist <- abs(gdp - x0)   # distance from focal x
h <- sort(dist)[95]     # bandwidth for span of .5 (where n = 190)
pick <- dist <= h       # observations within window

# ----------------------------------------------------------
plot(gdp, infant, xlab="GDP per Capita", ylab="Infant-Mortality Rate",
    type="n", main="(a) Observations Within the Window\nspan = 0.5")
points(gdp[pick], infant[pick], col="black")
points(gdp[!pick], infant[!pick], col=gray(0.75))
abline(v=x0)    # focal x
abline(v=c(x0 - h, x0 + h), lty=2)  # window
text(x0, par("usr")[4] + 10, expression(x[(150)]), xpd=TRUE)

# ----------------------------------------------------------
win.graph()
plot(range(gdp), c(0,1), xlab="GDP per Capita",
    ylab="Tricube Kernel Weight",
    type="n", main="(b) Tricube Weights")
abline(v=x0)
abline(v=c(x0 - h, x0 + h), lty=2)

tricube <- function(x, x0, h) {
    z <- abs(x - x0)/h
    ifelse(z < 1, (1 - z^3)^3, 0)
}
tc <- function(x) tricube(x, x0, h) # to use with curve
curve(tc, min(gdp), max(gdp), n=1000, lwd=2, add=TRUE)
points(gdp[pick], tricube(gdp, x0, h)[pick], col="gray20")
abline(h=c(0, 1), col="gray")

# ----------------------------------------------------------
win.graph()
plot(gdp, infant, xlab="GDP per Capita", ylab="Infant-Mortality Rate",
    type="n", main="(c) Weighted Average (Kernal Estimate)")
points(gdp[pick], infant[pick], col="black")
points(gdp[!pick], infant[!pick], col=gray(0.75))
abline(v=x0)
abline(v=c(x0 - h, x0 + h), lty=2)
yhat <- weighted.mean(infant, w=tricube(gdp,  x0, h))  # kernel estimate
lines(c(x0 - h, x0 + h), c(yhat, yhat), lwd=3)

# ----------------------------------------------------------
win.graph()
plot(gdp, infant, xlab="GDP per Capita", ylab="Infant-Mortality Rate",
    main="(d) Complete Loess Estimate")
yhat <- numeric(length(gdp))
for (i in 1:length(gdp)){   # kernel estimate at each x
    x0 <- gdp[i]
    dist <- abs(gdp - x0)
    h <- sort(dist)[95]
    yhat[i] <- weighted.mean(infant, w=tricube(gdp, x0, h))
    }
lines(gdp, yhat, lwd=2)



