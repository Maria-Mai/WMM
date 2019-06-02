# a)
naivegauss <- function(x) {
  stopifnot(is.factor(x[[ncol(x)]]))
  xk <- split(x[-ncol(x)], x[, ncol(x)])
  fac <- names(x)[ncol(x)]
  
  #p <- as.list(table(x$fac) / length(x$fac))
  p <- as.vector(sapply (xk, nrow) / nrow(x))
  
  #mu <- by(x[,-ncol(x)],x$fac,FUN=colMeans)
  mu <- sapply (xk, colMeans)
  
  #S <- by(x[,-ncol(x)],x$fac,FUN=cov)
  S <- sapply(xk, cov)
  
  structure (.Data = list(p = p,
                          mu = mu,
                          s = s), class = 'naivegauss')
}


#b)
predict.naivegauss <- function(o, newdata) {
  print(o$mu)
  u <- apply(newdata, 1, FUN = function(x){o$p * dnorm(x,mean = o$mu, sd = sqrt(o$S))})
  return(u)
}

# c)
heldout <- function(x, newdata = x,  method, ...) {
  method(...)
  return(numeric[1])
}

# d)