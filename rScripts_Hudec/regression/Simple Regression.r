# Author: Marcus Hudec 

library(faraway)
data(chredlin)
# help(chredlin)

head(chredlin)  
attach(chredlin)

# First OLS
plot(involact~race)
reg.race <- lm(involact~race)
abline(reg.race)
summary(reg.race)

# OLS with Prediction/Confidence Intervals
plot(involact~race, xlab="Minority Percentage",
                          ylab="FAIR plan activity",
                          xlim=c(0,100), ylim=c(0,2.5)
                          ,pch=19)
abline(reg.race)
pred.frame      = data.frame(race = 0:100)
pred.confidence = predict(reg.race, int = "c", newdata = pred.frame)
pred.prediction = predict(reg.race, int = "p", newdata = pred.frame)
matlines(pred.frame$race, pred.confidence[,2:3], lty=c(2,2), lwd=2, col=3)
matlines(pred.frame$race, pred.prediction[,2:3], lty=c(3,3), lwd=2, col=2)
legend(1,2.45,c(
        paste("Regression Line Y = ", round(reg.race$coeff[1], 3), "  + ", 
                      round(reg.race$coeff[2], 3), " * X"),
        expression(paste("Confidence-Intervall (",alpha==0.95,")")),
        expression(paste("Prediction-Intervall (",alpha==0.95,")"))),
        lty = c(1,2,3), col = c(1,3,2), lwd=c(1,2,2), cex=0.95)

