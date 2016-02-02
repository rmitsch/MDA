library(ISLR)
library("rpart")


source("preProcessing_Classification.R")

#from cross validation
# data # training dataSet
# dataTest # to validate


######
# Author: Marcus Hudec
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
#-----------------------------------------------------------------


attach(data)

# Recursive Partitioning
fit <- rpart(default ~ .,method="class",data=data,cp=0.000001)
fit
summary(fit)
plot(fit)
text(fit, cex=0.7)
printcp(fit)




optimal.CP <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

# Automatisches Prunen
pruned <- prune(fit, cp=optimal.CP)
plot(pruned)
text(pruned, cex=0.7)


# Evaluation of Model performance
# use test data for prediction - result is quite good ;)
probs.rpart <- predict(pruned, newdata= data)
preds.rpart <- predict(pruned, newdata= data, type="class")
confusion(preds.rpart, default)


library("verification")
roc.plot(ifelse(default=="Yes",1,0), probs.rpart[,2], legend=T, leg.text="RPART", plot.thres=NULL)


# Data Splitting to avoid overfitting
splitting <- ifelse(runif(nrow(data))> 0.5, 1, 2)

data1.rpart <- rpart(default ~ .,data=data,subset=splitting==1, parms=list(split="gini"),  cp=0.003)
            

# Within sample test
probs.rpart <- predict(data1.rpart, newdata=data[splitting==1, ])
preds.rpart <- predict(data1.rpart, newdata=data[splitting==1, ], type="class") 
confusion(preds.rpart, default[splitting==1])

par(mfrow=c(1,2))
roc.plot(ifelse(default=="Yes",1,0)[splitting==1], probs.rpart[,2], legend=T, leg.text="RPART", plot.thres=NULL, main="Training Data")


# Out of sample test                   
probs.rpart <- predict(data1.rpart, newdata=data[splitting==2, ])
preds.rpart <- predict(data1.rpart, newdata=data[splitting==2, ], type="class")
confusion(preds.rpart, ifelse(default=="Yes",1,0)[splitting==2])

roc.plot(ifelse(default=="Yes",1,0)[splitting==2], probs.rpart[,2], legend=T, leg.text="RPART",plot.thres=NULL, main="Test Data")

par(mfrow=c(1,1))



