# Autor: Marcus Hudec
#
# -------------------------------------------------------------
# Verwendete Libraries
# -------------------------------------------------------------
library("MASS")
library("rpart")
library("verification")
library(class)


# Pfadangabe
# -------------------------------------------------------------
# setwd("C:/Work/Data")
# -------------------------------------------------------------

confusion <- function(pred.class, true.class)
{     
  h <- table(pred.class, true.class)
  rownames(h) <- c("Pred: 0", "Pred: 1")
  colnames(h) <- c("True: 0", "True: 1")
  h <- cbind(h, apply(h, 1, sum))
  confusion.matrix <- rbind(h, apply(h, 2, sum))
  print(paste("Sensitivity: ", 
            round(confusion.matrix[2,2]/confusion.matrix[3,2]*100, 2)))
  print(paste("Specifity: ", 
            round(confusion.matrix[1,1]/confusion.matrix[3,1]*100, 2)))
confusion.matrix
}
# -----------------------------------------------


SPAM <- read.table(file="../data/SpambaseData.txt", sep=",")
head(SPAM)
# data are also available from package nutshell
dimnames(SPAM)[[2]][c(1, 20, 21, 23, 24, 52, 53,57,58)] <-
    c("Make", "Credit", "Your", "N000", "Money",
      "Exclam", "Dollars", "Capitals", "Class")

spam <- SPAM[ , c(1, 20, 21, 23, 24, 52, 53,57,58)]
attach(spam)

FClass <- as.factor(Class)

# Deskriptive Statistiken & Boxplots
by(spam, Class, summary)

par (mfrow=c(2, 4))
boxplot(Make ~ Class, main="Make")
boxplot(Credit ~ Class, main="Credit")
boxplot(Your ~ Class, main="Your")
boxplot(N000 ~ Class, main="N000")
boxplot(Money ~ Class, main="Money")
boxplot(Exclam ~ Class, main="Exclam")
boxplot(Dollars ~ Class, main="Dollars")
boxplot(Capitals ~ Class, main="Capitals")
par (mfrow=c(1, 1))


# Recursive Partitioning
spam.rpart <- rpart(FClass ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals)
spam.rpart
summary(spam.rpart)
plot(spam.rpart)
text(spam.rpart)
printcp(spam.rpart)


spam.rpart.c <- rpart(FClass ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals,
      cp=0.0001)
plot(spam.rpart.c)
text(spam.rpart.c, cex=0.5)
printcp(spam.rpart.c)
plotcp(spam.rpart.c)

# Händisches Beschneiden des Baums
snipped <- snip.rpart(spam.rpart.c)
plot(snipped)
text(snipped, cex=0.5)

# Automatisches Prunen
pruned <- prune(spam.rpart.c, cp=0.003)
plot(pruned)
text(pruned, cex=0.7)

# Evaluation of Model performance
probs.rpart <- predict(pruned, newdata= spam)
preds.rpart <- predict(pruned, newdata= spam, type="class")
confusion(preds.rpart, Class)

roc.plot(Class, probs.rpart[,2], legend=T, leg.text="RPART",
         plot.thres=NULL)
roc.area(Class, probs.rpart[,2])

# Data Splitting to avoid overfitting
splitting <- ifelse(runif(nrow(spam))> 0.5, 1, 2)

spam1.rpart <- rpart(FClass ~ Make+Credit+Your+N000+Money+Exclam+Dollars+Capitals,
                     subset=splitting==1, parms=list(split="gini"),  cp=0.003)
            

# Within sample test
probs.rpart <- predict(spam1.rpart, newdata=spam[splitting==1, ])
preds.rpart <- predict(spam1.rpart, newdata=spam[splitting==1, ], type="class")
confusion(preds.rpart, Class[splitting==1])

par(mfrow=c(1,2))
roc.plot(Class[splitting==1], probs.rpart[,2], legend=T, leg.text="RPART",
         plot.thres=NULL, main="Training Sample")
         
# Out of sample test                   
probs.rpart <- predict(spam1.rpart, newdata=spam[splitting==2, ])
preds.rpart <- predict(spam1.rpart, newdata=spam[splitting==2, ], type="class")
confusion(preds.rpart, Class[splitting==2])

roc.plot(Class[splitting==2], probs.rpart[,2], legend=T, leg.text="RPART",
         plot.thres=NULL, main="Test Sample")

par(mfrow=c(1,1))


# knn ---------------------------------------------------------------------
set.seed(1234)
praedictor <- cbind(Make,Credit,Your,N000,Money,Exclam,Dollars,Capitals)
splitting  <- ifelse(runif(nrow(spam))> 0.5, 1, 2)
train      <- praedictor[splitting==1,]
test       <- praedictor[splitting==2,]

res.knn3   <- knn(train, test, Class[splitting==1], k = 3, prob=TRUE)
h <- attr(res.knn3, "prob")
posterior3 <- ifelse(res.knn3==1, h, 1-h)     # context free interpretation

res.knn7   <- knn(train, test, Class[splitting==1], k = 7, prob=TRUE)
h <- attr(res.knn7, "prob")
posterior7 <- ifelse(res.knn7==1, h, 1-h)     # context free interpretation

res.knn15   <- knn(train, test, Class[splitting==1], k = 15, prob=TRUE)
h <- attr(res.knn15, "prob")
posterior15 <- ifelse(res.knn15==1, h, 1-h)     # context free interpretation

res.knn25   <- knn(train, test, Class[splitting==1], k = 25, prob=TRUE)
h <- attr(res.knn25, "prob")
posterior25 <- ifelse(res.knn25==1, h, 1-h)     # context free interpretation

roc.plot(Class[splitting==2], cbind(posterior3, posterior7, posterior15, posterior25), legend=T, 
         leg.text=c("k-NN with k=3","k-NN with k=7",
                    "k-NN with k=15","k-NN with k=25"),  plot.thres=NULL)

     
