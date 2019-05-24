load("filter2D.rda")
pdf("legendre.pdf")

#a
walsh <- function(n){
  if(n == 1){
    matrix(1,1,1)
  }
  else if(n==2){
    return(matrix(c(1,1,1,-1), 2, 2) )
  }
  else if(n >2){
    return(walsh(2) %x% walsh(n/2))
  }
}

#b
layout(matrix(1:9,3,3,byrow = TRUE))
for(i in 0:8){
  w <- (walsh(2**i))
  rows = nrow(w)
  cols = ncol(w)
  w <- array(w, dim=c(rows,cols,3))

  eins <- array(c(1,1,1), dim = c(1,1,3))
  blue <- array(c(0,0,1), dim = c(1,1,3))
  yellow <- array(c(1,1,0), dim = c(1,1,3))

  for(i in 1:rows){
    for(j in 1:cols){
      w[i,j,] <- ifelse(w[i,j,,drop=FALSE] == eins, yellow, blue)
    }
  }
  
  plot.array(w, main=i)
}

#c
legendre <- function(x, n) {
  if(n==0){
    return(rep(1, length(x)))
  }
  else if(n==1){
    return(x)
  }
  else{
    return(((2* n - 1) / n) * x * legendre(x, n - 1) - ((n - 1) / n) * legendre(x, n - 2))
  }
}

#d
layout(matrix(c(1:9), 3, 3, byrow = TRUE))
sapply(0:17, function(n) {
  x <- seq(from = -1, to = 1, length = 1000)
  plot(
    y = legendre(x,n), x = x, col = n + 1, ylim = c(-1,1), type = "l",
    main = paste("Legendre Polynom P(x,", n,")"), ylab = "y"
  )
})

#e
erdnegel <- function(x=0,n=0){
  if(n==0){
    return(1)
  }
  else{
    l_old <- 1
    l_new <- x
  
    for(i in 1:(n-1)){ 
      tmp <- l_old
      l_old <- l_new
      l_new <- (2*i+1)/(i+1)*x*l_old - i/(i+1) * tmp
    }
    return(l_new)
  }
}

#legendre(n=100)
erdnegel(n=100)

#f
layout(matrix(c(1:6), 3, 2, byrow = TRUE))  
sapply(5:10, function(n) {
  x <- seq(from = -1/10, to = 1/10, length = 1000)
  plot(
    y = erdnegel(x,2**n), x = x, col = n + 1, type = "l", 
    main = paste("Legendre Polynom P(x,", 2**n,")"), ylab = "y"
  )
})
dev.off()
