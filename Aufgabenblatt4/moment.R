image.vec <- c(rocket.dark, rocket.icon, rocket.modes, rocket.plan,
               rocket.pyro, rocket.toy, rocket.trend, rocket.V2)

#a
moment <- function(x,plot=TRUE, ...){
  
  mom <- function(x,p,q,xs=0,ys=0){
    sum(x*((col(x)-xs)^p) * ((row(x)-ys)^q))
  }
  
  center_x <- mom(x,1,0)/mom(x,0,0)
  center_y <- mom(x,0,1)/mom(x,0,0)
  
  inclination <-  1/2 * atan2(2*mom(x,1,1,center_x,center_y), mom(x,2,0, center_x, center_y)-mom(x,0,2, center_x, center_y))
  inclinationDegree <- ifelse(inclination > 0, -(90-(inclination * 180/pi)), (90-(-inclination * 180/pi)))
    
  result <- c(center=c(center_x,center_y), inclination=inclination, inclinationDegree=inclinationDegree)
  print(result)
  
  #b schwerpunkt plot 
  plot.array(x)
  abline(v=center_x, h=center_y)
  
  #c neigung plot
  
}

moment(rocket.toy)

#d

