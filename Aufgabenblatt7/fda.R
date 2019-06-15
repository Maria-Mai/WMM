#a
data(iris)
load("fda.rda")
pdf("fda.pdf")

list.fda <- list("iris"=iris, "ADIDAS"=ADIDAS, "diabetes"=diabetes, "dna"=dna, "FIAT"=FIAT, "mafia"=mafia, "vehicle"=vehicle)

plot.ldf <- function(x,subset = (1:ncol(x))[-nf],nf = ncol(x), ...) {
  plot(x[, subset], col = rainbow(nlevels(x[[nf]]))[x[[nf]]], ...)
}

#b, c
class.scatter <- function(X,f=ncol(X)){

  means <- colMeans(as.matrix(X[,-f]))
  classes <- levels(X[[f]])
  
  #sw
  covOne <- function(X, f, class) {
    part <- X[X[,f] == class, -f]
    cov(part) * (nrow(part)-1)
  }
  sw <- 0
  for(i in classes) {
    sw <- sw + covOne(X,f,i)
  }
  sw <- sw*(1/nrow(X))
  
  #sb
  covTwo <- function(X,f,class) {
    mu <- colMeans(X[-f])
    part <- X[X[,f] == class, -f]
    muK <- colMeans(part)
    (nrow(part)/nrow(X))*((muK-mu)%*%t(muK-mu))
  }
  sb <- 0
  for(i in classes) {
    sb <- sb + covTwo(X,f,i)
  }
    
  swb <- sb + sw
  
  s <- cov(X[-f]) * ((nrow(X)-1)/nrow(X))
  
  #print(s)
  #print(swb)
  
  #stopifnot(all.equal(s, swb)) # dimnames sind unterschiedlich
  list(mean = means, total = s, within = sw, between = sb)
}

#d, e
fisher <- function(x, train=x, n=ncol(x)-1, method=c("FDA", "PCA", "BSA", "orig")){
  
  x. <- data.matrix(x[,-ncol(x)])

  scatter <- class.scatter(train)
  kernmatrix <- switch (method,
                        "FDA" = solve(scatter$within, scatter$between),
                        "PCA" = scatter$total,
                        "BSA" = scatter$between,
                        "orig" = return(x))
  
  mu <- scatter$mean
  eigenstuff <- eigen(kernmatrix)
  eigenval <- eigenstuff$values[1:n]
  eigenvec <- Re(eigenstuff$vectors)[,1:n]
  
  result <- 0
  result <- x. %*% eigenvec
  result <- data.frame(result)
  result[,ncol(result)+1]<-x[,ncol(x)]
  result
}

#f
for(aSet in list.fda){
  layout(matrix(c(1,2,3,4),2, byrow = TRUE))
  plot.ldf(fisher(aSet, aSet, 2, "orig"),2, main="orig")
  plot.ldf(fisher(aSet,aSet,2, "FDA"), main="FDA")
  plot.ldf(fisher(aSet,aSet,2, "PCA"), main="PCA")
  plot.ldf(fisher(aSet,aSet,2, "BSA"), main="BSA")
  layout.show(0)
}

#g
#head(mafia)

plot.ldf(fisher(mafia, mafia, 22, "FDA"), labels=names(mafia)[-length(mafia)])

dev.off()


