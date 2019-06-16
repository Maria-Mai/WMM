# a)
naivegauss <- function(x) {
  stopifnot(is.factor(x[[ncol(x)]]))
  
  xk <- split(x[-ncol(x)], x[, ncol(x)])
  fac <- x[[ncol(x)]]
  
  p <- t(as.matrix(table(fac) / length(fac)))
  mu <- sapply (xk, colMeans)
  s <- sapply(xk, cov)
  
  structure (.Data = list(
    p = p,
    mu = mu,
    s = s,
    class = 'naivegauss'
  ))
}

# lapply über tapply um Mittelwertvektoren über alle Merkemale unterschieden durch Faktoren
#lapply(iris[,-5],function (x) tapply(x, iris[[5]], mean))

#b)
predict.naivegauss <- function(o, newdata) {

  u <- matrix(0,nr = nrow(newdata),nc = length(o$p))
  colnames(u) <- colnames(o$p)
  for (i in 1:length(o$p)) {
    u[,i] <- apply(as.matrix(newdata), 1, FUN = function(x,i) { mu <- t(o$mu[, i, drop = F]); S <- matrix(o$s[, i], nr = sqrt(length(o$s[,i]))); o$p[i] * ((1 / sqrt(det(2 * pi * S))) * exp((-1 / 2) * (x - mu) %*% solve(S) %*% t(x - mu)))}, i)
  }
  
  f <- factor(apply(u, 1, which.max),levels = c(1:length(o$p)), labels = colnames(o$p))
  return(f)
}



# c)
heldout <- function(x, newdata = x,  method, ...) {
  testfac <- newdata[[ncol(newdata)]]
  if(is.factor(newdata[[ncol(newdata)]])) {
    newdata <- newdata[-ncol(newdata)]
  }
  pred <- predict.naivegauss(method(x, ...), newdata)
  
  falserate <- mean(testfac!= pred) 

  classtest <- split(cbind(testfac, pred), f = testfac)
  falseratematrix <- table(testfac,pred)
  
  structure (.Data = list(
    falserate = as.numeric(falserate)),
    confused = falseratematrix
  )
}


# d)
heldout(iris,iris,naivegauss)

# e)
load("Aufgabenblatt7/diabetes.rda")

heldout(diabetes.lern, diabetes.lern, naivegauss)
heldout(diabetes.lern, diabetes.test, naivegauss)
heldout(diabetes.test, diabetes.lern, naivegauss)
heldout(diabetes.test, diabetes.test, naivegauss)

# f)
leave1out <- function(x, method, ...) {
  #falserates <- apply(data <- expand.grid(x,x), 1, function (d) heldout(get(d[1]),get(d[2]),naivegauss))
  n = nrow(x)
  falserate = 0
  for (i in 1: length(n)) {
    testdata <- x[-i,]
    traindata <- x[i,]
    falserate = falserate + heldout(x,x,method,...)$falserate
  }
  falserate = falserate / n
  return(falserate)
}
leave1out(iris,naivegauss)
leave1out(rbind(diabetes.test,diabetes.lern), naivegauss)

