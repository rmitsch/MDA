# Regression with Indicator Variables
# ===================================

x1 <- c(rnorm(50, 25, 2.5),rnorm(50, 10, 2.5))
x2 <- rnorm(100, 15, 5)
gr <- c(rep("m",50), rep("f", 50))
y1 <- 0.5*x1+rnorm(100,0,1)+ifelse(gr=="m", -5, 5)
y2 <- 0.5*x2+rnorm(100,0,1)+ifelse(gr=="m", -5, 5)

par(mfrow=c(1,2))
plot(y1~x1, xlim=c(0,30), ylim=c(0,20))
abline(lm(y1~x1, subset=gr=="m"), col=2)
abline(lm(y1~x1, subset=gr=="f"), col=3)
abline(lm(y1~x1), lty=2)
plot(y2~x2, xlim=c(0,30),ylim=c(0,20))
abline(lm(y2~x2, subset=gr=="m"), col=2)
abline(lm(y2~x2, subset=gr=="f"), col=3)
abline(lm(y2~x2), lty=2)
par(mfrow=c(1,1))


# All data without grouping Info
res.1  <- lm(y1 ~ x1)
summary(res.1)


# Common Slope Model
i.gr <- ifelse(gr=="m", 1, 0)
res.1a <- lm(y1 ~ x1 + i.gr)
summary(res.1a)


plot(y1~x1, xlim=c(0,30), ylim=c(0,20))
abline(res.1, lty=2)

# Visualisierung Variante 1
xi <- c(0,30)
yi <- coef(res.1a)[1] + xi*coef(res.1a)[2] +  coef(res.1a)[3]
lines(xi, yi, col=2)
yi <- coef(res.1a)[1] + xi*coef(res.1a)[2]
lines(xi, yi, col=3)


# Visualisierung Variante 2
plot(y1~x1, xlim=c(0,30), ylim=c(0,20))
abline(res.1, lty=2)
new.m <- as.data.frame(cbind(x1=sort(x1[1:50]), i.gr=rep(1,50)))
new.f <- as.data.frame(cbind(x1=sort(x1[51:100]), i.gr=rep(0,50)))
lines(sort(x1[1:50]), predict(res.1a, new=new.m), col=2)
lines(sort(x1[51:100]), predict(res.1a, new=new.f), col=3)

# Model with Interactions
x_i.gr <- x*i.gr
res.1m <- lm(y1 ~ x1 + i.gr + x_i.gr)
summary(res.1m)

summary(lm(y1~x1+gr))