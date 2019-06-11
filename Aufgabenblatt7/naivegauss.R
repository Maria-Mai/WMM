# a)
naivegauss <- function(x) {
  stopifnot(is.factor(x[[ncol(x)]]))
  
  xk <- split(x[-ncol(x)], x[, ncol(x)])
  fac <- x[[ncol(x)]]
  c <- names(x[ncol(x)])
  
  p <- t(as.matrix(table(fac) / length(fac)))
  #p <- as.vector(sapply (xk, nrow) / nrow(x))
  
  #mu <- by(x[,-ncol(x)],fac,FUN=colMeans)
  mu <- sapply (xk, colMeans)
  
  #s <- by(x[,-ncol(x)],fac,FUN=cov)
  s <- sapply(xk, cov)
  
  structure (.Data = list(
    p = p,
    mu = mu,
    s = s,
    c = c,
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

  pred <- data.frame(newdata,f )
  colnames(pred)[ncol(pred)] <- o$c
  return(pred)
}

# c)
heldout <- function(x, newdata = x,  method, ...) {
  method(...)
  return(numeric[1])
}

# d)