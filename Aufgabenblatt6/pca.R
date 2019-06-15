# a)
data(iris)
load("Aufgabenblatt6/pca.rda")


# b)
plot.ldf <- function(x,...) {
  stopifnot(is.factor(x[[ncol(x)]]))
  plot(x[,-ncol(x)],col = x[[ncol(x)]],...)
  #legend("bottom",unique(x[[ncol(x)]]),col=1:length(x[[ncol(x)]]),...)
}

#c)

for (j in 1:4) {
  plot.ldf(iris[c(1:j,5)], pch = 21)
}

#d)
PCA <- function(x, n) {
  if(missing(n)) {
    n = ncol(x)-1
  }
  if(is.factor(x[[ncol(x)]])) {
    x <- x[,-ncol(x)]
  }
  
  mu <- colMeans(x)
  covx.eig <- eigen(cov(x))
  eigenval <- covx.eig$values[1:n]
  eigenvec <- covx.eig$vectors[,1:n]
  
  structure (.Data = list(mu = mu,
                          eigenval = eigenval, 
                          eigenvec = eigenvec), class = 'PCA')
}

#e)
predict.PCA <- function(o, newdata) {
  if(is.factor(newdata[[ncol(newdata)]])) {
    fac <- newdata[,ncol(newdata)]
    x <- as.matrix(newdata[,-ncol(newdata)])
  }
  
  d <- diag(o$eigenval^(-1/2), length(o$eigenval))
  
  proj <- t((d %*% t(o$eigenvec)) %*% t(sweep(x,2,o$mu)))
  
  return(cbind(data.frame(proj),fac))
}


# f)
for (j in 1:4) {
  plot.ldf(predict.PCA(PCA(iris,j),iris))
}

#g) 
layout(matrix(1:4, nr = 2))
viris <- iris[iris$Species == "versicolor",]
ldata <- list(iris, iris, viris, viris)
tdata <- list(iris, viris, iris, viris)
for (i in 1:4) {
  plot.ldf(predict.PCA(PCA(ldata[[i]],2),tdata[[i]]))
}


#h)
for (data in list(ADIDAS,diabetes,dna,FIAT,heart,iris, vehicle, watermark)) {
  layout(matrix(1:4, nr = 2))
  plot.ldf(data[c(1,2,ncol(data))])
  plot.ldf(data[c(ncol(data)-2,ncol(data)-1,ncol(data))])
  plot.ldf(predict.PCA(PCA(data,2),data))
  #plot.ldf(predict.PCA(PCA(data,),data))
}
