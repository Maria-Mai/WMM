# a) Function to equalize picture matrices
equalize <- function(x, RGB) {
  if(missing(RGB)) {
    RGB = FALSE
  }
  if (length(dim(x)) == 3 && RGB == TRUE) {
    for (i in 1:3) {
      x[,,i] = equalize(x[,,i])
    }
    return(x)
  } else if (length(dim(x)) == 3 && RGB == FALSE) {
    g <- array(0, dim = c(dim(x)[1:2]))
    for (i in 1:dim(x)[1]) {
      for (j in 1:dim(x)[2]) {
        g[i,j] <- mean(x[i,j,])
      }
    }
    return(equalize(g))
  }
  
  if(length(dim(x)) == 2 ) {
    x[] <- rank(x)/length(x) 
    x <- (x - min(x)) / (max(x) - min(x))
    return (x) 
  }

}

# b) Loading data
pics = load("Aufgabenblatt4/equalize.rda")

# c) Function to plot histograms
plot.equalize <- function(x, main = "", K = 64) {
  xEqu <- equalize(x)
  
  layout(matrix(c(1:6), ncol = 2, byrow = T))
  plot.array(x,"Original Picture")
  plot.array(xEqu,"Equalized Picture")
  
  xHist <- hist(x, breaks = seq(0,1,l= K+1),main = "absolute Histogram  of Original Picture")
  xEquHist <- hist(xEqu, breaks = seq(0,1,l= K+1), main = "absolute Histogram of Equalized Picture")
  
  plot.ecdf(x, xlab = "Greyvalue", ylab= "Propability", main = "Cumulative Distribution of Original Picture")
  plot.ecdf(xEqu, xlab = "Greyvalue", ylab= "Propability", main = "Cumulative Distribution of Original Picture")
  
  layout(1)
}


#d) Testing all pictures
for ( i in 1:length(pics)) {
  x <- get(pics[i])
  if (is.array(x)) plot.equalize(x)
}

# e) GUESS
# f) RGB == True
# g) RGB == False
# h) Plot Potus
p <- get("potus")
layout(matrix(c(1:4),2))
plot.array(p, main = "Original Potus")
plot.array(equalize(p), main = "Equalized Potus")
plot.array(equalize(p, RGB = T), main = "Equalized Color Potus")
plot.array(equalize(p, RGB = F), main = "Equalized Grey Potus")
