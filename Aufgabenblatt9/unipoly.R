#a
data <- get(load("unipoly.rda"))

#b
polyfun <- function(x,a){
  sapply(x, function(x)(sum(a*x**((0:(length(a)-1))))))
}

#c
polyfit <- function(xy,n){
  if(n==0) {
    lm(xy[[2]] ~ 1)
  }else{
    lm(xy[[2]] ~ poly(xy[[1]],n, raw=TRUE))
  }
}

#d
polyfits <- function(xy, deg, plot=FALSE){
  lm.list <- sapply(deg, polyfit, xy=xy, simplify = FLASE)
  if(plot==TRUE){
    aci.list <- sapply(lm_list, AIC)
    bci.list <-sapply(lm_list, BIC)
    plot()
  }
  lm.list
}


#e
polyplot <- function(o, xy){

  
}

#polyfit(data, 6)

#f
polyplots <- function(xy, deg=0:11){
  
}