# Autor: Marcus Hudec

library(xlsx)
library(MASS)
# Pfadangabe
# -------------------------------------------------------------
setwd("C:/Work/Data")

Data <- read.xlsx(file="Windmill.xlsx", sheetName="Rawdata")
attach(Data)

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
		lines(xp, yp)
	}
  if (vertical) abline(v=wo, lty=2)
	res
}


# ---------------------------------------------------------------------

plot(X,Y,xlab="Windgeschwindigkeit", pch=20,
ylab="Gleichstromproduktion", xlim=c(0,10), xaxp=c(0,10,5), ylim=c(0,2.5))
abline(lsfit(X,Y), lty=2, )

plot(lm(Y~X))

res.lsfit <- lsfit(X,Y)
ls.print(res.lsfit)


res <- piecewise(X, Y, 2, xlab="Windgeschwindigkeit",
       ylab="Gleichstromproduktion")

# Umsetzung mit Indikatorvariblen
I.L6 <-ifelse(X>6, 1, 0)
# Additives Modell
res <- lm(Y~X+I.L6)
summary(res)
plot(Y~X)
lines(X[X>6], predict(res)[X>6], col=2)
lines(X[X<=6], predict(res)[X<=6])
# Multiplikatives Modell
res <- lm(Y~X*I.L6)     # äquivalent dazu   Y~X+I.L6+X:L.I6
summary(res)
plot(Y~X)
lines(X[X>6], predict(res)[X>6], col=2)
lines(X[X<=6], predict(res)[X<=6])

# Lineare Splines
# ===================================================================
lspline  <- ifelse(X>6, X-6,0)
res.cont <- lm(Y~X+lspline)
summary(res.cont)
model.matrix(~X + lspline)
plot(X, Y)
lines(X, fitted(res.cont))


# Try Logarithmic Transformation
# ==================================================================
Xt <- log(X)
res.lsfit <- lsfit(Xt,Y)
ls.print(res.lsfit)

plot(Xt,Y,xlab="ln(Windgeschwindigkeit)", pch=20,
     ylab="Gleichstromproduktion", xlim=c(0,10), xaxp=c(0,10,5),
     ylim=c(0,2.5))
abline(res.lsfit, lty=2)

xi <- seq(from=min(X), to=max(X), length=100)
yi <- res.lsfit$coef[1] + log(xi)*res.lsfit$coef[2]

plot(X,Y,xlab="Windgeschwindigkeit", pch=20,
ylab="Gleichstromproduktion", xlim=c(0,10), xaxp=c(0,10,5), ylim=c(0,2.5))
lines(xi, yi, lty=2)

ls.residuals(res.lsfit)


# Try Square-Root Transformation
# ==================================================================
Xt <- sqrt(X)
res.lsfit <- lsfit(Xt,Y)
ls.print(res.lsfit)


plot(Xt,Y,xlab="Windgeschwindigkeit^(1/2)", pch=20,
ylab="Gleichstromproduktion", xlim=c(0,10), xaxp=c(0,10,5), ylim=c(0,2.5))
abline(res.lsfit, lty=2)

xi <- seq(from=min(X), to=max(X), length=100)
yi <- res.lsfit$coef[1] + sqrt(xi)*res.lsfit$coef[2]

plot(X,Y,xlab="Windgeschwindigkeit", pch=20,
ylab="Gleichstromproduktion", xlim=c(0,10), xaxp=c(0,10,5), ylim=c(0,2.5))
lines(xi, yi, lty=2)

ls.residuals(res.lsfit)

# Show Box-Cox Family

bc.trans <- function(xi, p)
{

trans <- function(x, p) 
{
if (p==0) log(x)
else
   (x^p-1)/p
}

res <- matrix(0, length(xi), length(p))
for (i in 1:length(p))  res[,i] <- trans(xi, p[i])
res
}

xi <- seq(from=0.1, to=4, length=200)
p  <- c(-1, -1/2, 0, 1, 2, 3)
matplot(xi, bc.trans(xi,p), type="l", col=1:6, lty=rep(1,6), lwd=2,
        ylab="Transfromed Data", main="Various Box-Cox Transformations")
legend(0, 15, as.character(p), col=1:6, lty=rep(1,6), lwd=2)


# Use Box-Cox Family of Transformations
# ==================================================================

boxcox(lm(Y~X), lambda=seq(-3, 3, length=100))

boxcox(lm(Y~X), lambda=seq(2, 3, length=100))

Yt <- (Y^2.4-1)/2.4
res.lsfit <- lsfit(X,Yt)
ls.print(res.lsfit)

plot(X,Yt,xlab="Windgeschwindigkeit", pch=20,
ylab="Gleichstromproduktion nach Box-Cox Transformation", 
xlim=c(0,10), xaxp=c(0,10,5), ylim=c(0,2.5))
abline(res.lsfit, lty=2)

xi <- seq(from=min(X), to=max(X), length=100)
yi <- ((res.lsfit$coef[1] + xi*res.lsfit$coef[2])*2.4+1)^(1/2.4)

plot(X,Y,xlab="Windgeschwindigkeit", pch=20,
ylab="Gleichstromproduktion", xlim=c(0,10), xaxp=c(0,10,5), ylim=c(0,2.5))
lines(xi, yi, lty=2)

ls.residuals(res.lsfit)

# Use Relationship from Physics Energy ~ Amount of Air per Time
# ==================================================================

Xt <- 1/X
res.lsfit <- lsfit(Xt,Y)
ls.print(res.lsfit)


plot(Xt,Y,xlab="Windgeschwindigkeit^-1", pch=20,
ylab="Gleichstromproduktion", xlim=c(0,10), xaxp=c(0,10,5), ylim=c(0,2.5))
abline(res.lsfit, lty=2)

xi <- seq(from=min(X), to=max(X), length=100)
yi <- res.lsfit$coef[1] + 1/xi*res.lsfit$coef[2]

plot(X,Y,xlab="Windgeschwindigkeit", pch=20,
ylab="Gleichstromproduktion", xlim=c(0,10), xaxp=c(0,10,5), ylim=c(0,2.5))
lines(xi, yi, lty=2)


# With Interval
# =========================================================================
res <- lm(Y ~ Xt)
xi <- seq(from=2, to=12, length=100)

plot(X,Y,xlab="Windgeschwindigkeit", pch=20,
ylab="Gleichstromproduktion", xlim=c(0,12), xaxp=c(0,10,5), ylim=c(0,2.5))

pred.frame      = data.frame(Xt = 1/xi)
pred.prediction = predict(res, se.fit=T, int = "p", newdata = pred.frame)
pred.prediction
matlines(xi, pred.prediction$fit[,2:3], lty=c(2,2), lwd=2, col=3)
lines(xi, pred.prediction$fit[,1], lty=1, lwd=2, col=1)
# =========================================================================



# Use Polynomial Model
# ==================================================================

Xz  <- X-mean(X)
Xz2 <- Xz^2
Xz3 <- Xz^3
res <- lm(Y ~ Xz+Xz2+Xz3)                                            
summary(res)
                                            
plot(X,Y,xlab="Windgeschwindigkeit", pch=20,
ylab="Gleichstromproduktion", xlim=c(0,10), xaxp=c(0,10,5), ylim=c(0,2.5))
lines(xi, yi, lty=2)
res <- lm(Y ~ Xz+Xz2+Xz3)
summary(res)
lines(X, fitted(res), lty=1, col=2)



# Take Care when extrapolating Polynomial Models
# ==============================================

Xzn <- -4:16
pred.frame      = data.frame(Xz = Xzn, Xz2 = Xzn^2, Xz3 =Xzn^3)
pred.prediction = predict(res, se.fit=T, int = "p", newdata = pred.frame)
pred.prediction
plot(X,Y,xlab="Windgeschwindigkeit", pch=19,
ylab="Gleichstromproduktion", xlim=c(0,15), xaxp=c(0,15,5), ylim=c(0,5))
matlines(Xzn+mean(X), pred.prediction$fit[,2:3], lty=c(2,2), lwd=2, col=3)
matlines(Xzn+mean(X), pred.prediction$fit[,1], lty=1, lwd=2, col=1)





# Discuss Orthogonality of Polynomials
# ======================================================================

X2 <- X^2
X3 <- X^3
cor(cbind(X, X2, X3))

Xz  <- X-mean(X)
Xz2 <- Xz^2
Xz3 <- Xz^3
cor(cbind(Xz, Xz2, Xz3))

# poly(X, degree=3)
cor(poly(X, degree=3))
res.poly  <- lm(Y ~ X+X2+X3)
res.ortho <- lm(Y ~ poly(X, degree=3))
summary(res.poly)
summary(res.ortho)

fitted(res.poly)
fitted(res.ortho)

