sinfunc <- function(w, t, phi=0){1/w * sin(2*pi*w*t + phi)}
color <- c("blue", "green", "red", "cyan", "orange" )

layout(matrix (c(seq(1,6)), nrow=3))

#a)
curve(sinfunc(1, x), from=0, to=1, col=color[1], main="a) Sinuskurven")
for(i in 2:5){
  curve(sinfunc(i, x),  col=color[i], add=TRUE)
}

#b)
sum_sin <- function(x){sinfunc(1,x)+sinfunc(2,x)+sinfunc(3,x)+sinfunc(4,x)+sinfunc(5,x)}
curve(sum_sin(x), from=0, to=1, col="blue", main="b) f1 + ... + f5")

#c)
curve(sin(x))

#d)
curve(sin(x))

#e)
curve(sin(x))

#f)
curve(sin(x))

layout(1)
