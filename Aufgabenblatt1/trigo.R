sinfunc <- function(w, t, phi=0){1/w * sin(2*pi*w*t + phi)}
color <- c("blue", "green", "red", "cyan", "orange" )

layout(matrix (c(seq(1,6)), nrow=3, byrow=TRUE))

#a)
curve(sinfunc(1, x), from=0, to=1, col=color[1], main="a) Sinuskurven")
for(i in 2:5){
  curve(sinfunc(i, x),  col=color[i], add=TRUE)
}

#b)
sum_sin <- function(x){sinfunc(1,x)+sinfunc(2,x)+sinfunc(3,x)+sinfunc(4,x)+sinfunc(5,x)}
curve(sum_sin(x), from=0, to=1, col="blue", main="b) f1 + ... + f5")

#c)
amp_sin <- function(x){
  z <- 0
  for(i in 1:5){
    z <- z + exp(-i)*sin(2*pi*i*x)
  }
  z
}
curve(amp_sin(x), from=0, to=1, col="blue", main="c) Amplituden")

#d)
phase_sin <- function(x){
  z <- 0
  for(i in 1:5){
    z <- z + (1/i)*sin(2*pi*i*x+(pi/2))
  }
  z
}
curve(phase_sin(x), from=0, to=1, col="blue", main="d) Phasen")

#e)
odd_sin <- function(x){
  z <- 0
  for(i in 1:5){
    if(i %% 2 == 1)(z <- z + sinfunc(i,x))
  }
  z
}
curve(odd_sin(x), main="e) ungerade")

#f)
even_sin <- function(x){
  z <- 0
  for(i in 1:5){
    if(i %% 2 == 0)(z <- z + sinfunc(i,x))
  }
  z
}
curve(even_sin(x), main="f) gerade")

layout(1)
