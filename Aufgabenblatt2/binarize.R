  load("/home/mai/Documents/UniSS19/Werkzeuge/Abgabe/Aufgabenblatt2/binarize.rda")
  pdf(file="/home/mai/Documents/UniSS19/Werkzeuge/Abgabe/Aufgabenblatt2/binarize.pdf", 
      onefile = TRUE)
  
  #a)
  layout(matrix(c(1,2,3,4),2,2,byrow = TRUE))
  plot.array(algae)
  breaksAlgae <- seq(from=min(algae), to=max(algae), l=36)
  hist(algae, breaks=breaksAlgae, col="blue", main = "Hist Algae", xlab="Intensity", ylab = "Frequency")
  
  #b)
  alga = algae[(50:100),(10:60)]
  plot.array(alga)
  breaksAlga <- seq(from=min(alga), to=max(alga), l=36)
  hist(alga, breaks=breaksAlga, col="blue", main = "Hist AlgaeSnippet", xlab="Intensity", ylab = "Frequency")
  layout(1)
  
  #c) d)
  binarize <- function(x, method="fixed", threshold=0.5, plot=FALSE){
    if(method=="median"){
      threshold = median(x)
    }
    else if(method=="mean"){
      threshold = mean(x)
    }
    else if(method=="kmeans"){
      h <- kmeans(c(x),2)
      threshold <- median(h$centers)
    }
    x <- ifelse(x < threshold, 0, 1)
    
    if(plot==TRUE){
      plot.array(x, main=paste("Method: ", method , ", Threshold: " , threshold))
    }
    x
  }
  
  #c)
  layout(matrix(c(1:9),3,3, byrow=TRUE))
  for(i in seq(0.2,0.6,l=9)){
    binarize(alga, threshold=i, plot=TRUE)
  }
  layout(1)
  
  #e)
  graphic_fields <- list(algae, tonga, alga)
  methods <- c("fixed", "mean", "median", "kmeans")
  
  layout(matrix(1:4, 2, 2))
  
  for(g in graphic_fields){
    for(m in methods){
      binarize(g, m, plot=TRUE)
    }
  }
  layout(1)
  dev.off()