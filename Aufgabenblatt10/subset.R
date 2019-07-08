library(mlbench)
library(caret)
library(class)
library(dplyr)
library(randomForest)
library(Metrics)
library(e1071)

#a
mydata <- get(load("data1.rda"))
#head(mydata)
levels(mydata$CLASS)

print(nrow(mydata))
mydata <- mydata %>% distinct()
print(nrow(mydata))
print(sum(is.na(mydata)))

mydata$CLASS<-as.factor(mydata$CLASS)


###---------aus alter uebung

kNN <- function(x, neighbours=1) {
  knn <- list(train=x[-ncol(x)], cl=x[[ncol(x)]], k=neighbours)
  class(knn) <- "kNN"
  return (knn)
}

predict.kNN <- function(o, newdata=NULL) {
  if (is.null(newdata)) {
    knn.cv(train=o$train, cl=o$cl, k=o$k)
  } else {
    knn(train=o$train, test=newdata, cl =o$cl, k=o$k)
  }
}

heldout <- function(x, newdata=x, method=kNN, ...) {
  if (is.null(newdata)) {
    classes <- x[[length(x)]]
    predicted <- predict(method(x, ...))
  } else {
    classes <- newdata[[length(newdata)]]
    predicted <- predict(method(x, ...), newdata[-length(newdata)])
  }
  confused <- table(classes, predicted)
  result <- sum(classes != predicted) / length(classes)
  attr(result, "confused") <- confused
  return(result)
}

#-------------------caret 
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model.lvq <- train(CLASS ~ ., data=mydata, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model.lvq, scale=FALSE)
measure.lvq <- data.frame(importance$importance)
extractedFeaturesLVQ <- rownames(measure.lvq[order(rowSums(measure.lvq), decreasing = TRUE),][1:10,])
extractedFeaturesLVQ <- c(extractedFeaturesLVQ, "CLASS")

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(mydata[-ncol(mydata)], y = mydata$CLASS,sizes =c(1:10), rfeControl = control)
extractedFeaturesRFE <- predictors(results)[1:10]
extractedFeaturesRFE <- c(extractedFeaturesRFE, "CLASS")


#--------------------random forest

model_rf <- randomForest (CLASS ~ ., data = mydata)
measure.rf <- importance(model_rf)

measure.rf <- measure.rf[order(-measure.rf[,"MeanDecreaseGini"]),,drop=FALSE]
extractedFeaturesRF <- names(measure.rf[1:10,,drop=TRUE])
extractedFeaturesRF <- c(extractedFeaturesRF, "CLASS")


#------get error rates
errorWithoutSelection <- heldout(x = mydata, newdata = NULL)
errorWithoutSelection

df.lvq <- subset(mydata, select=extractedFeaturesLVQ)
errorWithLVQ <- heldout(x = df.lvq, newdata = NULL)
errorWithLVQ
extractedFeaturesLVQ

df.rfe <- subset(mydata, select=extractedFeaturesRFE)
errorWithRFE <- heldout(x = df.lvq, newdata = NULL)
errorWithRFE
extractedFeaturesRFE

df.randomforest <- subset(mydata, select=extractedFeaturesRF)
errorWithRandomForest <- heldout(x = df.randomforest, newdata = NULL)
errorWithRandomForest
extractedFeaturesRF

#-----save character vectors

v1 <- extractedFeaturesLVQ[-length(extractedFeaturesLVQ)]
v2 <- extractedFeaturesRF[-length(extractedFeaturesRF)]
v3 <- extractedFeaturesRFE[-length(extractedFeaturesRFE)]

save(v1, v2, v3, file="subset.rda", version = 3)
