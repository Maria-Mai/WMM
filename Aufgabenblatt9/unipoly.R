#a
mydata <- get(load("unipoly.rda"))

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
  lm.list <- sapply(deg, polyfit, xy=xy, simplify = FALSE)
  if(plot==TRUE){
    aic <- sapply(lm.list, AIC)
    bic <-sapply(lm.list, BIC)
    col <- c("blue", "red")
    plot(rep(deg, 2), c(aic, bic), pch=16, col=col[c(rep(1, length(aic)), rep(2,length(bic)))],
          main=paste(names(xy),collapse=" ~ "), xlab="degree", ylab="goodness of fit" )
  }
  lm.list
}

#e
polyplot <- function(o, xy){
  a.vec <- o$coefficients
  plot(xy, main=mtext(paste("BIC:" , round(BIC(o), 2), "PolyGrad:" ,length(o$coefficients)-1)))
  curve(polyfun(x, a.vec), col=2, add=TRUE)
  out <- paste(round(o$coeff), collapse=', ')
  mtext(out,side=4, cex=0.6)
}

#f
polyplots <- function(xy, deg=0:11){
  poly.list <- polyfits(xy, deg,TRUE)
  layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
  sapply(poly.list, polyplot, xy=xy)
  layout(1)
}

#g
pdf("unipoly.pdf")
polyplots(mydata)
data("cars")
polyplots(cars)

polyplots(data.frame(LifeCycleSavings$pop15, LifeCycleSavings$pop75))
polyplots(data.frame(LifeCycleSavings$pop15, LifeCycleSavings$dpi))
polyplots(data.frame(LifeCycleSavings$pop75, LifeCycleSavings$pop15))
polyplots(data.frame(LifeCycleSavings$pop75, LifeCycleSavings$dpi))
polyplots(data.frame(LifeCycleSavings$dpi, LifeCycleSavings$pop15))
polyplots(data.frame(LifeCycleSavings$dpi, LifeCycleSavings$pop75))

dev.off()