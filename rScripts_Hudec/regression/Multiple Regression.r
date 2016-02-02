# Author: Marcus Hudec 

library(faraway)
data(chredlin)
# help(chredlin)

head(chredlin)  
attach(chredlin)

# Multiple Regression Model
res <- lm(involact ~ race + fire + theft + age + income)
summary(res)

# Removal of Income
res <- lm(involact ~ race + fire + theft + age)
summary(res)

# General Linear Hypothesis
res.Big   <- lm(involact ~ race + fire + theft + age + income)
res.Small <- lm(involact ~ race + fire + theft)
anova(res.Small, res.Big)

plot(res)                   # Diagnostic Plots
