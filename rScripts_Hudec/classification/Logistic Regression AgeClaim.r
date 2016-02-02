library("verification")
# Path of Data File
# -------------------------------------------------------------
setwd("C:/Work/Data/MEDA")
# -------------------------------------------------------------


daten <- read.table(file="AgeClaim.txt", header=T)
head(daten)
attach(daten)

plot(age, claim)
abline(lsfit(age, claim), lwd=2, col="grey")


agegroup <- cut(age, 5)
c.g      <- tapply(claim, agegroup, sum)
n.g      <- tapply(claim, agegroup, length)
mean.g   <- tapply(age, agegroup, mean)
lines(mean.g, c.g/n.g, type="o", pch=19, col=3, lwd=2)

res.logit <- glm(claim ~ age, family=binomial)
summary(res.logit)
xi <- seq(from=20, to=70, length=400)
yi <- 1/(1+exp(-coef(res.logit)[1]-coef(res.logit)[2]*xi))
lines(xi, yi, lwd=2, col=2)

res.probit <- glm(claim ~ age, family=binomial(link="probit"))
summary(res.probit)
xi <- seq(from=20, to=70, length=400)
yi <- pnorm((coef(res.probit)[1]+coef(res.probit)[2]*xi), 0, 1)
lines(xi, yi, lwd=2, col=4)

# Goodness of fit
eta <- predict(res.logit)               # gives the linear predictor  
pred.prob <- exp(eta)/(1+exp(eta))
pred.prob
predict(res.logit, type="response")     # more direct way

plot(age, pred.prob, type="l")


# Prediction
cut <- 0.5                              # change values of cut
pred.class <- ifelse(pred.prob > cut, 1, 0)
h <- table(pred.class, claim)
h
h1 <- cbind(h, apply(h, 1, sum))
rbind(h1, apply(h1, 2, sum))

sensitivity <- h[2,2]/sum(h[, 2])
sensitivity

specifity   <- h[1,1]/sum(h[, 1])
specifity


# ROC-Curve 
preds.logit <- predict(res.logit)

roc.plot(claim, preds.logit, legend=T, leg.text="Logit-Model",
         plot.thres=NULL)
roc.area(claim, preds.logit)



# Demo of Logit
xi <- seq(from=-5, to=5, length=400)
pi <- 1/(1+exp(-xi))
par(mfrow=c(1,2))
plot(xi, xi, xlab="Linear Predictor eta", ylab="Logit", type="l", lwd=2)
plot(xi, pi, xlab="Linear Predictor eta", ylab="Probability", type="l", lwd=2)
par(mfrow=c(1,1))