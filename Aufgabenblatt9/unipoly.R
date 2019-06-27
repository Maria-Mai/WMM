#a
data <- get(load("unipoly.rda"))

#b
polyfun <- function(x,a){
  sapply(x, function(x)(sum(a*x**((0:(length(a)-1))))))
}

#c
polyfit <- function(xy,n){
  lm(xy[[2]] ~ poly(xy[[1]],n, raw=TRUE))
}

#d
polyfits <- functio(xy, deg, plot=FALSE){
  
}

