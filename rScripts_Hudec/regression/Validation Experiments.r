library(ISLR)
library (ggplot2)
library(boot)
library(FNN)
library(car)



# data splitting  checking for overfit with lm
# --------------------------------------------
set.seed(1)
repl <- 100
n <- nrow(Auto)
attach(Auto)
scatterplot(mpg~horsepower)


mse.train <- numeric(repl)
mse.test  <- numeric(repl)

for (i in 1:repl)
{
train  <- sample(1:n, n/2)
lm.fit <-lm(mpg~horsepower, subset=train)
mse.train[i] <- mean((mpg-predict(lm.fit, Auto))[train]^2)
mse.test[i]  <- mean((mpg-predict(lm.fit, Auto))[-train]^2)
}

mean(mse.train)
mean(mse.test)

df  <- as.data.frame(cbind(mse.train, mse.test))
dfs <-stack(df)
ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind))

# data splitting  checking for overfit with k-NN k=1
# --------------------------------------------------
set.seed(1)

mse.train <- numeric(repl)
mse.test  <- numeric(repl)

for (i in 1:repl)
{
train  <- sample(1:n, n/2)
kNN.fit <-knn.reg(horsepower, y=mpg, k=1)
mse.train[i] <- mean((mpg-kNN.fit$pred)[train]^2)
mse.test[i]  <- mean((mpg-kNN.fit$pred)[-train]^2)
}

mean(mse.train)
mean(mse.test)

df  <- as.data.frame(cbind(mse.train, mse.test))
dfs <-stack(df)
ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind))

# data splitting  checking for overfit with k-NN k=3
# --------------------------------------------------
set.seed(1)

mse.train <- numeric(repl)
mse.test  <- numeric(repl)

for (i in 1:repl)
{
train  <- sample(1:n, n/2)
kNN.fit <-knn.reg(horsepower, y=mpg, k=3)
mse.train[i] <- mean((mpg-kNN.fit$pred)[train]^2)
mse.test[i]  <- mean((mpg-kNN.fit$pred)[-train]^2)
}

mean(mse.train)
mean(mse.test)

df  <- as.data.frame(cbind(mse.train, mse.test))
dfs <-stack(df)
ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind))

median(mse.train)
median(mse.test)


# Experiments with Polynomials
# data splitting  checking for order of polynomial used
# -----------------------------------------------------
mse.1 <- numeric(repl)
mse.2 <- numeric(repl)
mse.3 <- numeric(repl)

for (i in 1:repl)
{
train  <- sample(1:n, n/2)
lm.fit.1 <-lm(mpg~horsepower, subset=train)
mse.1[i] <- mean((mpg-predict(lm.fit.1, Auto))[-train]^2)
lm.fit.2 <-lm(mpg~poly(horsepower,2), subset=train)
mse.2[i] <- mean((mpg-predict(lm.fit.2, Auto))[-train]^2)
lm.fit.3 <-lm(mpg~poly(horsepower,3), subset=train)
mse.3[i] <- mean((mpg-predict(lm.fit.3, Auto))[-train]^2)
}

mean(mse.1)
mean(mse.2)
mean(mse.3)

df  <- as.data.frame(cbind(mse.1, mse.2, mse.3))
dfs <-stack(df)
ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind))


# cross validation checking for order of polynomial used
# ------------------------------------------------------
set.seed(1)
repl <- 50
mse.1.cv <- numeric(repl)
mse.2.cv <- numeric(repl)

for (i in 1:repl)
{
glm.fit.1   <- glm(mpg~horsepower)
mse.1.cv[i] <- cv.glm(Auto, glm.fit.1, K=10)$delta[1]
glm.fit.2   <- glm(mpg~poly(horsepower,2))
mse.2.cv[i] <- cv.glm(Auto, glm.fit.2, K=10)$delta[1]
}

summary(mse.1.cv)
mean(mse.1.cv)
summary(mse.2.cv)
mean(mse.2.cv)
df  <- as.data.frame(cbind(mse.1.cv, mse.2.cv))
dfs <-stack(df)
ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind))


