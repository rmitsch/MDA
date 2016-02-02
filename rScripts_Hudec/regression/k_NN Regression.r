# K-NN Regression

# Marcus Hudec
 
library(FNN)

# First Look at the Data
attach(faithful)
head(faithful)
plot(eruptions~waiting)
abline(lm(eruptions~waiting))

# Transformations of Data
X <- matrix(waiting,ncol=1)
h <- order(waiting)
X <- matrix(waiting[h], ncol=1)
Y <- eruptions[h]

# k-nn Regression
plot(eruptions~waiting)
abline(lm(eruptions~waiting))
res.knn.9 <- knn.reg(X, y=Y, k=9)
lines(X,res.knn.9$pred,col=2)
res.knn.3 <- knn.reg(X, y=Y, k=3)
lines(X,res.knn.3$pred,col=3)
legend(45,5,
       c("OLS-Regression", "k-NN Reg with k=9","k-NN Reg with k=3"),
        lty = c(1,1,1), col = 1:3, lwd=c(2,2,2), cex=1)

res.knn.3
res.knn.9


# Crossvalidation

crossvalind <- function(N, kfold)
{
len.seg <- ceiling(N/kfold)
incomplete <- kfold*len.seg - N
complete <- kfold - incomplete
ind <- matrix(c(sample(1:N), rep(NA, incomplete)), nrow = len.seg, byrow =
TRUE)
cvi <- lapply(as.data.frame(ind), function(x) c(na.omit(x))) # a list
return(cvi)
}


N <- length(Y)
kfold <- 10
cvi <- crossvalind(N, kfold)
mse.ls <- numeric(kfold)
mse.k3 <- numeric(kfold)
mse.k9 <- numeric(kfold)


for (i in 1:length(cvi))
{
xc <- X[unlist(cvi[-i]), ]             # x in training set
yc <- Y[unlist(cvi[-i])]               # y in training set

xt <- X[cvi[[i]], ]                    # x in test set
yt <- Y[cvi[[i]]]                      # y in test set

# Least Square
lm.mod <- lm(yc ~ xc)
yt.pred <- predict(lm.mod, newdata=data.frame(xc =xt))
mse.ls[i] <- sum((yt - yt.pred)^2)/length(yt)
#knn
Xc <- matrix(xc, ncol=1)    #
Yc <- matrix(yc, ncol=1)
Xt <- matrix(xt, ncol=1)
res.knn.3 <- knn.reg(train=Xc, test=Xt, y=Yc, k=3)
mse.k3[i] <- sum((yt - res.knn.3$pred)^2)/length(yt)
res.knn.9 <- knn.reg(train=Xc, test=Xt, y=Yc, k=9)
mse.k9[i] <- sum((yt - res.knn.9$pred)^2)/length(yt)
}

sum(mse.ls)
sum(mse.k3)
sum(mse.k9)

