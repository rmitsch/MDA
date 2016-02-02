# Autor: Marcus Hudec
# Übungen: Logistic Regression
# -------------------------------------------------------------
# Verwendete Libraries
# -------------------------------------------------------------
library("HSAUR")
library(lattice)

# Pfadangabe
# -------------------------------------------------------------
setwd("C:/Work")
# -------------------------------------------------------------

data(plasma)
plasma
data("plasma", package = "HSAUR")
layout(matrix(1:2, ncol = 2))
cdplot(ESR ~ fibrinogen, data = plasma)
cdplot(ESR ~ globulin, data = plasma)
layout(matrix(1:1, ncol = 1))

plasma_glm_1 <- glm(ESR ~ fibrinogen,
                   data = plasma, family = binomial())

summary(plasma_glm_1)
confint(plasma_glm_1, parm="fibrinogen")
exp(coef(plasma_glm_1))
exp(confint(plasma_glm_1, parm="fibrinogen"))

plasma_glm_2 <- glm(ESR ~ fibrinogen + globulin,
                   data = plasma, family = binomial())
summary(plasma_glm_2)
anova(plasma_glm_1, plasma_glm_2, test = "LRT")

# Some Visualisation Experiments

prob <- predict(plasma_glm_1, type="response")
plot(globulin ~ fibrinogen, data = plasma, xlim = c(2,6),
    ylim = c(25, 50), pch = ".", main="Model-1 Fibrinogen")
symbols(plasma$fibrinogen, plasma$globulin, circles = prob, add = TRUE)

prob <- predict(plasma_glm_2, type="response")
plot(globulin ~ fibrinogen, data = plasma, xlim = c(2,6),
    ylim = c(25, 50), pch = ".", main="Model-2 Fibrinogen + Globulin")
symbols(plasma$fibrinogen, plasma$globulin, circles = prob, add = TRUE)


h1   <- seq(from=2,  to=6, length=100)
h2   <- seq(from=25, to=50, length=100)
grid <- expand.grid(list(fibrinogen=h1, globulin=h2))
prob1 <- predict(plasma_glm_1, type="response", newdata=as.data.frame(grid))
levelplot(prob1 ~ fibrinogen * globulin, data=grid,
      xlab = "Fibrinogen",
      ylab = "Globulin", 
      at=quantile(prob1, 0:24/25),col.regions=heat.colors(25), main="Model-1 Fibrinogen")

prob2 <- predict(plasma_glm_2, type="response", newdata=as.data.frame(grid))
levelplot(prob2 ~ fibrinogen * globulin, data=grid,
      xlab = "Fibrinogen",
      ylab = "Globulin", 
      at=quantile(prob2, 0:24/25),col.regions=heat.colors(25),
      main="Model-2 Fibrinogen + Globulin")

wireframe(prob2 ~ fibrinogen * globulin, data=grid, drape=T, 
          at=quantile(prob2, 0:24/25),col.regions=heat.colors(25)
          ,main="Model-2 Fibrinogen + Globulin")

# Just for showing how it works with interactions
plasma_glm_3 <- glm(ESR ~ fibrinogen * globulin,
                   data = plasma, family = binomial())
summary(plasma_glm_3) 
prob3 <- predict(plasma_glm_3, type="response", newdata=as.data.frame(grid))

levelplot(prob3 ~ fibrinogen * globulin, data=grid,
      xlab = "Fibrinogen",
      ylab = "Globulin", 
      at=quantile(prob3, 0:24/25),col.regions=heat.colors(25))  
      
wireframe(prob3 ~ fibrinogen * globulin, data=grid, drape=T, 
          at=quantile(prob3, 0:24/25),col.regions=heat.colors(25)
          ,main="Model-3 Fibrinogen * Globulin")             