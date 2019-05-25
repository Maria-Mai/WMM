library (cluster)
library(MASS)

#a
load("zip.rda")

#b
bits <- function(x,compress = TRUE) {
  if (compress){
    x <- memCompress(x,"gzip")
    length(x)*8
  } else{
    sum(nchar(x,"bytes"))*4
  }
}

#c
compression.factor <- sapply(text,function(x){bits(x)/bits(x, FALSE)})
dotchart(sort( compression.factor , decreasing = T), main="Compressionfactors", cex=0.85, col="blue", pch=16 )


#d
cross <- function(xp, xq){
  ( bits( c( xq, xp ) ) - bits( xq ) ) / bits( xp, FALSE )
}

#e
divergence <- function(xp, xq){
  cross( xp, xq ) - cross( xp, xp )
}

distance <- function(X){
  tri <- outer(X, X, Vectorize(divergence))
  qua <- tri + t(tri)
  return(as.dist(qua))
}

#f
plot(agnes(distance(text)), which.plots=2)
plot(diana(distance(text)), which.plots=2)

#g
cmdscale 
sammon

