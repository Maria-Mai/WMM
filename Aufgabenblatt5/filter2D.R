# Loading data
load("Aufgabenblatt5/filter2D.rda") 

# a)
translate <- function(x,dr,dc) {
  # um dr Zeilen und dc Spalten versetzte zyklische Bildmatrix
  N = dim(x)[[1]]
  M = dim(x)[[2]]
  print(c(N,M))
  
  if(dr != 0) {
     if  (dr < 0 ) {
      dr = N + dr
    }
  x[] <- rbind(x[(1 + dr):N,], x[1:dr,])
  }
  
  if(dc != 0) {
    if (dr < 0) {
      dc = M + dc
    }
  x[] <- cbind(x[,(1 + dc):M], x[,1:dc])
  }
  
  return (x)
  
  }

plot.array(translate(coke,0, 0), main = "Orginial")

layout(matrix(c(1:4),ncol = 1))
d = c(11,-11,47,-47)
for (i in d) {
  for (j in d) {
    plot.array(translate(coke,i, j), main = paste("Shift: ", i, j))
  }
}



