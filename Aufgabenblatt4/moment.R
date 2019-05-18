load("moment.rda")
pdf("moment.pdf")

images <- list(rocket.dark, rocket.icon, rocket.modes, rocket.plan,
               rocket.pyro, rocket.toy, rocket.trend, rocket.V2)

#a
moment <- function(x,plot=TRUE, ...){
  
  mom <- function(x,p,q,xs=0,ys=0){
    sum(x*((row(x)-xs)^p) * ((col(x)-ys)^q))
  }
  
  center_x <- mom(x,1,0)/mom(x,0,0)
  center_y <- mom(x,0,1)/mom(x,0,0)
  
  inclination <-  1/2 * atan2(2*mom(x,1,1,center_x,center_y), mom(x,2,0, center_x, center_y)-mom(x,0,2, center_x, center_y))
  inclinationDegree <- inclination *180/pi

  if(plot==TRUE){
    #b schwerpunkt plot 
    plot.array(x)
    v = center_y-1/2
    h = nrow(x) - center_x+1/2
    abline(h=h, v=v, col="yellow")
    
    #c neigung plot
    mtext(paste(round(inclinationDegree, digits = 2),"°"), side=4)
    b <- tan(inclination + pi / 2)
    a <- h - v*b

    abline( a, b, col="red")
  }

  result <- c(center=c(center_x,center_y), inclinationDegree=inclinationDegree)
  
}

#d

for(x in images){
  layout(matrix(1:4, 2, 2, byrow=TRUE))
  moment(x)
  moment(1-x)
  moment(binarise(x))
  moment(binarise(1-x))
  layout(1)
}

#e 
#plot.array(rocket.modes)
#plot.array(rocket.trend)

dev.off()

