# Autor: Marcus Hudec
# -------------------------------------------------------------
# Verwendete Libraries
# -------------------------------------------------------------
library(car)
library(lattice)
library(rpart)


# Pfadangabe
# -------------------------------------------------------------
setwd("C:/Work")
# -------------------------------------------------------------

data <- airquality[complete.cases(airquality),]
attach(data)

head(data)

# All pairwise scattergrams
pairs(data[,1:4],
    panel=function(x, y){
        points(x, y)
        abline(lm(y ~ x), lty="dashed")
        lines(lowess(x, y), col=2)
    },
    diag.panel=function(x){
        par(new=TRUE)
        hist(x, main="", axes=FALSE)
    }
)

# One predictor
plot(Wind, Ozone, xlab="Wind", ylab="Ozone")
abline(lsfit(Wind, Ozone))

res.loess <- loess(Ozone ~ Wind)
h <- seq(3, 20, 0.1)
pv <- predict(res.loess, data.frame(Wind = h), se = TRUE)

lines(h, pv$fit, lwd=1.5, col=2)
lines(h, pv$fit+2*pv$se.fit, col=2, lty=2)
lines(h, pv$fit-2*pv$se.fit, col=2, lty=2)

# Regression Tree
res <- rpart(Ozone ~ Wind)
plot(res)
text(res)
tree.pre <- predict(res, data.frame(Wind=h))

plot(Wind, Ozone, xlab="Wind", ylab="Ozone")
abline(lsfit(Wind, Ozone))
lines(h, pv$fit, lwd=1.5, col=2)
lines(h, pv$fit+2*pv$se.fit, col=2, lty=2)
lines(h, pv$fit-2*pv$se.fit, col=2, lty=2)
lines(h, tree.pre, col=3, lwd=2)

# A more complex tree
res.2 <- rpart(Ozone ~ Wind , cp=0.000001)
tree.pre.2 <- predict(res.2, data.frame(Wind=h))
lines(h, tree.pre.2, col=4, lwd=2, lty=2)

plot(res.2)
text(res.2)

# Choosing the cost complexity parameter
 
xx <- printcp(res.2)
matplot(xx[,2], xx[,3:4], type="l", 
        ylab="Relative Error", xlab="Complexity")

# Pre-defined plots
par(mfrow=c(1,2))
rsq.rpart(res.2)
par(mfrow=c(1,1))

plotcp(res.2, minline = TRUE, lty = 3, col = 1,
       upper = "size")


# Final Model
res <- rpart(Ozone ~ Wind, cp=0.026)
plot(res)
text(res)

h <- seq(from=2, to=25, length=400)
tree.pre <- predict(res, data.frame(Wind=h))
plot(Wind, Ozone, xlab="Wind", ylab="Ozone")
lines(h, tree.pre, lwd=1.5)

# Two Explanatory Variables



h1   <- seq(from=2, to=25, length=40)
h2   <- seq(from=50, to=100, length=40)
grid <- expand.grid(list(Wind=h1, Temp=h2))

scatter3d(Ozone ~ Wind + Temp)
scatter3d(Ozone ~ Wind + Temp, fit="quadratic")

res  <- lm(Ozone ~ poly(Wind,3) + poly(Temp,3))
Ozone.Est <- predict(res, data.frame(grid))
wireframe(Ozone.Est ~ Wind * Temp, data=grid, drape=T, 
          at=quantile(Ozone.Est, 0:24/25),col.regions=heat.colors(25))


res <- rpart(Ozone ~ Wind + Temp, cp=0.0000001)
plot(res)
text(res)
printcp(res)
plotcp(res)

res <- rpart(Ozone ~ Wind + Temp, cp=0.0051424)
plot(res)
text(res)

h1 <- seq(from=2, to=25, length=40)
h2 <- seq(from=50, to=100, length=40)
grid <- expand.grid(list(Wind=h1, Temp=h2))
Ozone.Est <- predict(res, data.frame(grid))
wireframe(Ozone.Est ~ Wind * Temp, data=grid, drape=T, 
          at=quantile(Ozone.Est, 0:24/25),col.regions=heat.colors(25))

# More Regressors
res <- rpart(Ozone ~ Wind + Solar.R + Temp, cp=0.00001)
printcp(res)
plotcp(res)

res <- rpart(Ozone ~ Wind + Solar.R + Temp, cp=0.0026759)
plot(res)
text(res)
