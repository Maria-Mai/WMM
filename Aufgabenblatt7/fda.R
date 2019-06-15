#a
data(iris)
load("fda.rda")

list.fda <- list(iris,ADIDAS, diabetes,dna,FIAT,mafia,vehicle)

plot.lfd <- function(x, subset=(1:length(x))[-nf], nf=dim(x)[2], ...){
  stopifnot (all(subset>0))
  classes <- x[,nf]
  K <- length(levels(classes))
  plot(x[,subset],col=rainbow(K)[classes], ... )
}

#b, c
class.scatter <- function(X,f){
  data <- X[,-f, drop=FALSE]
  means <- colMeans(data)
  
  #sw
  covOne <- function(x, nf, class) {
    part <- x[x[,nf] == class, -nf]
    cov(part) * (nrow(part)-1)
  }
  sw <- 0
  for(i in classes) {
    sw <- sw + covOne(x,nf,i)
  }
  sw <- sw*(1/nrow(x))
  
  #sb
  covTwo <- function(x,nf,class) {
    mu <- colMeans(x[-nf])
    part <- x[x[,nf] == class, -nf]
    muK <- colMeans(part)
    (nrow(part)/nrow(x))*((muK-mu)%*%t(muK-mu))
  }
  sb <- 0
  for(i in classes) {
    sb <- sb + coveTwo(x,nf,i)
  }
    
  swb <- sb + sw
  
  s <- cov(data)
  s <- s * (1-1/nrow(data))
    
  stopifnot(s == swb) # all.equal
  list(mean = means, total = s, within = sw, between = sb)
}

#d, e
fisher <- function(x, train=x, n=0, method=c("FDA", "PCA", "BSA", "orig")){
  if(missing(n)) {
    n = ncol(x)-1
  }

  if(is.factor(x[[ncol(x)]])) {
    x <- x[,-ncol(x)]
  }
  
  scatter <- class.scatter(x)
  
  kernmatrix <- switch (method,
                        "FDA" = solve(scatter$within, scatter$sb),
                        "PCA" = scatter$total,
                        "BSA" = scatter$between,
                        "orig" = x,
                        x)
  
  mu <- scatter$mean
  eigenstuff <- eigen(kernmatrix)
  eigenval <- eigenstuff$values[1:n]
  eigenvec <- eigenstuff$vectors[,1:n]

  structure (.Data = list(mu = mu,
                          eigenval = eigenval, 
                        eigenvec = eigenvec), class = method)
}

#f



#g