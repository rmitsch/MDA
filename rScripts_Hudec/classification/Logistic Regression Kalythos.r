# Autor: Marcus Hudec
# -------------------------------------------------------------
# Verwendete Libraries
# -------------------------------------------------------------

# Pfadangabe
# -------------------------------------------------------------
setwd("C:/Work")
# -------------------------------------------------------------

# ================================================================
ld50 <- function(x)

# Returns LD50 for Logit- and Probit-Models
{
retval <- 0

if (!is.null(class(x)) && (class(x)[1] == "glm"))
   if (x$family[1] == "binomial")
      if ((substring(x$family[2]$link, 1, 5) == "logit") ||
          (substring(x$family[2]$link, 1, 6) == "probit"))
          retval <- -coef(x)[1]/coef(x)[2]
      else
         cat("Link-Function of GLM-Object is neither Logit or Probit \n")
   else
      cat("Family of GLM-Object is not Binomial \n")
else
   cat("Class of Object is not GLM \n")
retval
}

# ================================================================

plot.logit <- function(x, y, xlab = "Dose", ylab = "Prob. of Response",
                       resol = 1000, ...)

# Gives a graphical display of univariate Dose-Response Models


 {
 plot(x, y[,1]/(y[,1]+y[,2]), xlab = xlab, ylab = ylab)
 title(main="Logit-Link")
 xi  <- seq(from = range(x)[1], to = range(x)[2], length=resol)
 fit <- glm(y~x, family=binomial())
 daten=as.data.frame(xi)
 dimnames(daten)[[2]] <- "x"
 lines(xi, predict(fit, newdata=daten, type="response"))
 abline(h=c(0.25, 0.5, 0.75), lty = 2)
 invisible(fit)
 }

# ================================================


plot.probit <- function(x, y, xlab = "Dose", ylab = "Prob. of Response",
                       resol = 1000, ...)

# Gives a graphical display of univariate Dose-Response Models


 {
 plot(x, y[,1]/(y[,1]+y[,2]), xlab = xlab, ylab = ylab)
 title(main="Probit-Link")
 xi  <- seq(from = range(x)[1], to = range(x)[2], length=resol)
 fit <- glm(y~x, family = binomial(link=probit))
 daten=as.data.frame(xi)
 dimnames(daten)[[2]] <- "x"
 lines(xi, predict(fit, newdata=daten, type="response"),...)
 abline(h=c(0.25, 0.5, 0.75), lty = 2)
 invisible(fit)
 }

 # =====================================================================

KALYTHOS <- data.frame(Age = c(20,35,45,55,70), N = rep(50,5),
                              Blind = c(6,17,26,37,44))

print(KALYTHOS)
attach(KALYTHOS)

Y <- cbind(Blind, N-Blind)
cbind(Y, Age)

fit.logit <- glm(Y~Age, family = binomial(link=logit))
summary(fit.logit)
ld50(fit.logit)

fit.probit <- glm(Y~Age, family = binomial(link=probit))
summary(fit.probit)
ld50(fit.probit)

par(mfrow=c(1,2))
plot.logit(Age, Y)
plot.probit(Age, Y, col=2)
par(mfrow=c(1,1))
