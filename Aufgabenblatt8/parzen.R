#a,g 
parzen <- function(x, sd=NULL){
  if(is.null(sd)){
    logL1o <- function(x,sd) sum(log(predict(parzen(x,sd))))
    sd <- optimize(logL1o, x, interval=c(0, diff(range(x))), maximum=TRUE)$maximum
  }
  structure(.Data=list(support=x, sigma=sd), class="parzen")
}

#b,e
predict.parzen <- function(o, newdata=NULL){
  if(is.null(newdata)){
    sapply(1:length(o$support),function(i) mean(dnorm(o$support[i],o$support[-i],o$sigma)))
  }else{
    sapply(newdata, function(z) mean(dnorm(z, mean = o$support, sd = o$sigma)))
  }
}

#c,f
plot.parzen <- function(o, xlim=c(min(o$support)-4*o$sigma, max(o$support)+4*o$sigma), ...){
  curve(predict(o,x),xlim = xlim,...)
  rug(o$support)
  mtext(side = 3,paste("s=",signif(o$sigma,3)),cex=.8)
  points(o$support, predict(o), pch=".")
}

#d
load("parzen.rda")
layout(matrix(c(1:6), 2, byrow=TRUE))
for(i in (6:-5)){
  plot(parzen(samples, 0.75**i))
}

#h
layout(1)
plot(parzen(samples))
