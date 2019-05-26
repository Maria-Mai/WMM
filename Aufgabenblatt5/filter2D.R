# Loading data
load("Aufgabenblatt5/filter2D.rda") 

#plot.array(coke)

# a)
translate <- function(x,dr,dc) {
  # um dr Zeilen und dc Spalten versetzte zyklische Bildmatrix
  N = dim(x)[[1]]
  M = dim(x)[[2]]

  row <- ((1:N - (dr+1)) %% N) +1
  col <- ((1:M - (dc+1)) %% M) +1
  
  x[row,col]
  }


layout(matrix(c(1:4),ncol = 1))
d = c(11,-11,47,-47)
for (i in c(11,-11)) {
  for (j in c(47,-47)) {
    plot.array(translate(coke,i, j), main = paste("Shift: ", i, j))
  }
}


# b)
filter.mean.3x3 <- function(x, norm = TRUE) {
  x <- (translate(x, -1, -1) + translate(x, -1, 0) + translate(x, -1, 1) + translate(x, 0, -1) + translate(x, 0, 0) + translate(x, 0, 1) + translate(x, 1, -1) + translate(x, 1, 0) + translate(x, 1, 1))
  ifelse((norm == TRUE), return(x/9), return(x))
  
} 


filter.robert <- function(x, norm = TRUE) {
  
  for (i in 1: (dim(x)[1]-1)) {
    for (j in 1: (dim(x)[2]-1)) {
      x[i,j] <- abs(x[i,j]-x[i+1,j+1]) + abs(x[i+1,j] - x[i,j+1])
    }
  }
  
  return (x)
  
  n<-seq(1,dim(x)[[1]])
  m<-seq(1,dim(x)[[2]])
  
  x <- abs(x[n,m]-translate(x,1,1)[n,m]) + abs(translate(x,1,0)[n,m] - translate(x,0,1)[n,m])
  ifelse((norm == TRUE), return(x/2), return(x))
  
}


filter.sobel.v <- function(x, norm = TRUE) {
  x <- (-translate(x, 1, 1) - 2* translate(x, 0, 1) - translate(x, -1, 1) + translate(x, 1, -1) + 2 * translate(x, 0, -1) + translate(x, -1, -1))
  ifelse(norm == TRUE, return((x/8)+0.5), return(x))
}



filter.sobel.h <- function(x, norm = TRUE) {

  x <- (-translate(x, 1, 1) - 2 * translate(x, 1, 0) - translate(x, 1, -1) + translate(x, -1, 1) + 2 * translate(x, -1, 0) + translate(x, -1, -1))
  ifelse(norm == TRUE, return((x/8)+0.5), return(x))
  
  }


filter.grad.mag <- function(x, norm = TRUE) {
  x <- sqrt(filter.sobel.h(x, norm = FALSE)^2 + filter.sobel.v(x, norm = FALSE)^2)
  ifelse(norm == TRUE, ifelse(max(x) > 1, return(x/max(x)), return(x)), return(x))
}

filter.grad.angle <- function(x, norm = TRUE) {
  x <- atan2(filter.sobel.h(x),filter.sobel.v(x))
  ifelse(norm == TRUE, return((x/(2*pi)) +0.5), return(x))
}

layout(matrix(1:6,nr=3))
plot.array(filter.mean.3x3(coke), main = "Filter mean 3x3")
plot.array(filter.robert(coke), main = "FilterRobert")
plot.array(filter.sobel.v(coke), main = "Filter Sobel_v")
plot.array(filter.sobel.h(coke), main = "Filter Sobel_h")
plot.array(filter.grad.mag(coke), main = "Betrag")
plot.array(filter.grad.angle(coke), main = "Winkel")


# e) 
layout(matrix(1:2,nr=2))
hist(filter.grad.angle(coke)*180/pi, breaks = seq(-180,180,by = 1/100), main = "Histogramm Winkel", xlab = "Grauwerte")

g <- filter.grad.mag(coke)
g <- g[apply( g , c(1,2) , function(x) x > median(g))]
hist(g,breaks = seq(0,1,by = 1/100), main = "Histogramm Betrag größer Median", xlab = "Grauwerte" )

#f)
layout(matrix(1:6,nr=2))
for (pic in list(algae,cashmere,muscle,tonga14,zebra,xray)) {
  plot.array(filter.grad.mag(pic), main = pic)
}

#g)
layout(matrix(1:6,nr=2))
for (pic in list(algae,cashmere,muscle,tonga14,zebra,xray)) {
  plot.array(mulaw(filter.grad.mag(translate(pic,0,0))), main = pic)
}

