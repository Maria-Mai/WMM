sinfunc <- function(w, x){
  w**(-1) * sin(2*pi*w*x)
}

for(i in 1:5) {
  curve(sinfunc(i, x), from=0, to=1, col="blue",main="Darstellung Sinuskurven", add=TRUE)
}

