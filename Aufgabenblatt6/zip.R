library (cluster)
library(MASS)

#a
load("zip.rda")
pdf("zip.pdf")

#b
bits <- function(x,compress = TRUE) {
  if (compress){
    x <- memCompress(x,"gzip")
    length(x)*8
  } else{
    sum(nchar(x,"bytes"))*8
  }
}

#c
compression.factor <- sapply(text,function(x){bits(x)/bits(x, FALSE)})
dotchart(sort( compression.factor , decreasing = T), main="Compressionfactors", cex=0.85, col="blue", pch=16 )

#d
cross <- function(xp, xq){
  (bits(c(xq,xp)) - bits(xq)) / bits(xp,FALSE)
}

#e
divergence <- function(xp, xq){
  cross(xp, xq) - cross(xp, xp)
}

distance <- function(X){
  S <- sapply(X,function (a) sapply(X,function(b) divergence(a,b)))
  as.dist(S+t(S))
}

#f
plot(agnes(distance(text)), which.plots=2)
plot(diana(distance(text)), which.plots=2)

#g
multiscale <- cmdscale(distance(text),2)
x <- multiscale[,1]
y <- multiscale[,2]
plot(x, y, type = "p", asp = 1, main = "cmdscale", xlab = "", ylab="")
text(x, y, rownames(multiscale), cex = 0.6, pos=1)

multiscale_sammon <- sammon(distance(text), k=2)
sammon_labes <- names(text)
plot(multiscale_sammon$points, type="p", main="Sammon-MDS", xlab = "", ylab="")
text(multiscale_sammon$points, labels=sammon_labes, pos=1,  cex = 0.6)

#---------------------mini beispiel

text2 <- list("Spanish"=text$Spanish, "Italian"=text$Italian, "English"=text$English, "French"=text$French, "German"=text$German, "Luxemburgish"=text$Luxemburgish)
text2 <- rev(text2)

compression.factor2 <- sapply(text2,function(x){bits(x)/bits(x, FALSE)})
dotchart(sort( compression.factor2 , decreasing = T), main="Compressionfactors", cex=0.85, col="blue", pch=16 )

plot(agnes(distance(text2)), which.plots=2)
plot(diana(distance(text2)), which.plots=2)

multiscale2 <- cmdscale(distance(text2),2)
x <- multiscale2[,1]
y <- multiscale2[,2]
plot(x, y, type = "p", asp = 1, main = "cmdscale", xlab = "", ylab="")
text(x, y, rownames(multiscale2), cex = 0.6, pos=1)

multiscale_sammon2 <- sammon(distance(text2), k=2)
sammon_labes2 <- names(text2)
plot(multiscale_sammon2$points, type="p", ylim=c(-0.6,0.6), xlim=c(-0.6,0.6), xlab = "", ylab="")
text(multiscale_sammon2$points, labels=sammon_labes2, pos=1)

dev.off()