library(class)
# a)
setClass("kNN", slots = c(train = "data.frame", factors = "factor", k = "numeric"))
kNN <- function(x, neighbours=1){
  train <- x[-ncol(x)]
  factors <- x[[ncol(x)]]
  new("kNN", train= train, factors = factors, k = neighbours)
}

# b)
predict.kNN <- function(o, newdata){
  if (is.null(newdata)){
    return(knn.cv(o@train, cl = o@factors, k = o@k))
  } else {
    if(is.factor(newdata[[ncol(newdata)]])) {
      newdata <- newdata[-ncol(newdata)]
    }
    return(knn(o@train, newdata, cl = o@factors, k = o@k))
  }
}

# d)
heldout <- function(x, newdata = x, method, ...) {
  pred <- predict(method(x, ...), newdata)
  if (is.null(newdata)){
    falserate <- mean(pred != x[, ncol(x)])
  }else {
    falserate <- mean(pred != newdata[, ncol(newdata)])
  }
  falserate
}

# e)
run1st <- function(x,y,choice =1+2*0:8){
  row_1 <- sapply(choice, heldout, x = x, newdata = y, method=kNN)
  row_2 <- sapply(choice, heldout, x = y, newdata = x, method=kNN) 
  x_y <- unique(rbind(x, y)) # Vereinigungsmenge der Lerndaten
  row_3 <- sapply(choice, heldout, x = x_y, newdata = NULL, method=kNN)
  row_4 <- sapply(choice, heldout, x = x_y, newdata = NULL, method=kNN)
  row_5 <- sapply(choice, heldout, x = x_y, newdata = NULL, method=kNN)
  falserate_matrix <- as.data.frame(rbind(row_1, row_2, row_3, row_4, row_5))
  colnames(falserate_matrix) = c(choice)
  rownames(falserate_matrix) = c("train/test", "test/train", "L10_1", "L10_2", "L10_3")
  falserate_matrix
  }

# f)
run2nd <- function(x,y,choice =2^(0:13)){
  falserate_vector <- sapply(choice, function(j) heldout(x = x[1:j,], newdata = y, method=kNN))
  names(falserate_vector) = c(choice)
  falserate_vector
}

# g)
run3rd <- function(x,choice=2:ncol(x)-1) {
  falserate_vector <- sapply(choice, function(j) heldout(x = x[,-j], newdata = NULL, method=kNN))
  names(falserate_vector) = c(colnames(x)[2:ncol(x)-1])
  falserate_vector
}

# e)
load("Aufgabenblatt8/vehicle.rda")
load("Aufgabenblatt8/australia.rda")
load("Aufgabenblatt8/letter.rda")

pe.1 <- run1st(vehicle.lern, vehicle.test)
pe.2 <- run2nd(letter.lern, letter.test)
pe.3 <- run3rd(unique(rbind(australia.lern, australia.test)))
save(pe.1,pe.2,pe.3,file="Aufgabenblatt8/kNN.rda")


# f)
layout(matrix(1:2, nr = 2))
barplot(as.matrix(100*pe.1), beside=T, ylab = "error rate [%]", legend=rownames(pe.1))
barplot(t(as.matrix(100*pe.1)), beside=T, ylab = "error rate [%]", legend=colnames(pe.1))
barplot(100*pe.2, xlab = " training set size", ylab = "error rate [%]")
barplot(100*pe.3, xlab = "knock-out feature", ylab = "error rate [%]")

