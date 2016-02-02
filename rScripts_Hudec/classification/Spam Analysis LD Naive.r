# Autor:    Marcus Hudec
# Übungen:  Classification Algorithms
# -------------------------------------------------------------
# Verwendete Libraries
# -------------------------------------------------------------
library("MASS")
library("rpart")
library("verification")
library("klaR")
library("e1071")

# Pfadangabe
# -------------------------------------------------------------
# setwd("C:/Work/Data")
# -------------------------------------------------------------

SPAM <- read.table(file="../data/SpambaseData.txt", sep=",")
head(SPAM)
dimnames(SPAM)[[2]][c(1, 20, 21, 23, 24, 52, 53,57,58)] <-
    c("Make", "Credit", "Your", "N000", "Money",
      "Exclam", "Dollars", "Capitals", "Class")

spam <- SPAM[ , c(1, 20, 21, 23, 24, 52, 53,57,58)]
attach(spam)

FClass <- as.factor(Class)

# Deskriptive Statistiken & Boxplots
by(spam, Class, summary)

par (mfrow=c(2, 4))
boxplot(Make ~ Class)
boxplot(Credit ~ Class)
boxplot(Your ~ Class)
boxplot(N000 ~ Class)
boxplot(Money ~ Class)
boxplot(Exclam ~ Class)
boxplot(Dollars ~ Class)
boxplot(Capitals ~ Class)
par (mfrow=c(1, 1))

splitting <- ifelse(runif(nrow(spam))> 0.5, 1, 2)

# Linear Discriminant Analysis
spam.lda   <- lda(FClass ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals)
spam.lda
plot(spam.lda)
preds.lda <- predict(spam.lda)
h <- table(FClass, preds.lda$class)
h
h/rowSums(h)

# LDA Priors verändert ==> Unterschiedliche Gewichtung der Fehler
spam.lda.90   <- lda(FClass ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals,
                  prior=c(0.90, 0.10))
preds.lda.90 <- predict(spam.lda.90)
h <- table(FClass, preds.lda.90$class)
h
h/rowSums(h)

# ROC-Curve for LDA
roc.plot(Class, preds.lda$posterior[,2], legend=T, leg.text="LDA",
         plot.thres=NULL)
roc.area(Class, preds.lda$posterior[,2])

# Naive Bayes Parametric
spam.Naive.N <-
    NaiveBayes(FClass ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals,
               data = spam)
plot(spam.Naive.N)
preds.Naive.N <- predict(spam.Naive.N)
table(FClass, preds.Naive.N$class)

# Naive Bayes Priors verändert
spam.Naive.N90 <-
    NaiveBayes(FClass ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals,
               data = spam, prior=c(0.90, 0.10))
preds.Naive.N90 <- predict(spam.Naive.N90)
table(FClass, preds.Naive.N90$class)

# ROC-Curve for Naive Bayes
roc.plot(Class, preds.Naive.N$posterior[,2], legend=T, leg.text="Naive Bayes",
         plot.thres=NULL)

# Modellvergleich
l1 <- paste("LDA ", round(roc.area(Class, preds.lda$posterior[,2])$A, 4),
      sep="")

h <- roc.plot(Class, preds.Naive.N$posterior[,2])
l2 <- paste("Naive Bayes ", round(h$roc.vol["Area"], 4), sep="")

roc.plot(Class, preds.lda$posterior[,2], plot.thres=NULL)
lines(h$plot.data[,3,1], h$plot.data[,2,1], lwd=1.5, col=2)
legend(0.5, 0.4, legend=c(l1, l2), col=1:2, lwd=rep(1.5, 2))

# Naive Bayes Non-Parametric
spam.Naive.K <- NaiveBayes(FClass ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals,
                         data = spam, usekernel = TRUE)
plot(spam.Naive.K)
preds.Naive.K <- predict(spam.Naive.K)
table(FClass, preds.Naive.K$class)

# Modellvergleich  3 Modelle

h2 <- roc.plot(Class, preds.Naive.N$posterior[,2])
l2 <- paste("Naive Bayes Normal ", round(h2$roc.vol["Area"], 4), sep="")

h3 <- roc.plot(Class, preds.Naive.K$posterior[,2])
l3 <- paste("Naive Bayes Kernel ", round(h3$roc.vol["Area"], 4), sep="")


roc.plot(Class, preds.lda$posterior[,2], plot.thres=NULL)
lines(h2$plot.data[,3,1], h2$plot.data[,2,1], lwd=1.5, col=2)
lines(h3$plot.data[,3,1], h3$plot.data[,2,1], lwd=1.5, col=3)
legend(0.5, 0.4, legend=c(l1, l2, l3), col=1:3, lwd=rep(1.5, 3))

# Quadratische Diskriminanzanalyse
spam.qda   <- qda(Class ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals)

# Variable für Data Splitting
splitting <- ifelse(runif(nrow(spam))> 0.5, 1, 2)

# Linear Discriminant Analysis DATA SPLITTING
spam.lda.1   <- lda(FClass ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals,
                    subset =splitting ==1)
preds.lda.1 <- predict(spam.lda.1, newdata= spam[splitting==2, ])
table(FClass, preds.lda$class)
table(FClass[splitting==2], preds.lda.1$class)


DF <- data.frame(OBS=Class[splitting==2], EST=preds.lda.1$posterior[,2])
roc.plot(DF$OBS, DF$EST, plot.thres=NULL)
roc.area(DF$OBS, DF$EST)

spam.Naive.K1 <- NaiveBayes(FClass ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals,
                         data = spam, subset =splitting ==1, usekernel = TRUE)
preds.Naive.K1 <- predict(spam.Naive.K1, newdata= spam[splitting==2, ])
table(FClass[splitting==2], preds.Naive.K1$class)

DF <- data.frame(OBS=Class[splitting==2], EST=preds.Naive.K1$posterior[,2])
roc.plot(DF$OBS, DF$EST)
roc.area(DF$OBS, DF$EST)



