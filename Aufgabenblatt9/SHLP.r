# a)
library(nnet)

#b)
SHLP <- function(x,hidden, rescale = FALSE, ...) {
  stopifnot(is.factor(x[[ncol(x)]]))
  fac <- x[[ncol(x)]]
  nnetrun <- nnet(as.formula(paste(colnames(x)[ncol(x)],'~.')), data = x, size = hidden, ...)
  structure (.Data = nnetrun,
             class = 'SHLP')	
}

# c)
predict.SHLP <- function(o, newdata) {
  predict(o, newdata=newdata)
}

#d) aus Aufgabe 7.2c)
heldout <- function(x, newdata = x,  method, ...) {
  testfac <- newdata[[ncol(newdata)]]
  if(is.factor(newdata[[ncol(newdata)]])) {
    newdata <- newdata[-ncol(newdata)]
  }
  pred <- predict(method(x, ...), newdata)
  
  falserate <- mean(testfac!= pred) 
  
  classtest <- split(cbind(testfac, pred), f = testfac)
  falseratematrix <- table(testfac,pred)
  
  structure (
    .Data = as.numeric(falserate),
    confused = k
  )
}

heldout(iris, iris[,1:4], SHLP, hidden = 3, Wts = cos(1:m(4,3,3)*883))

# e) in Textformat
m <- function(D, H, K) {
  (D + 1) * H + (H + 1) * K
}


# f)
load("Aufgabenblatt9/diabetes.rda")
load("Aufgabenblatt9/heart.rda")
load("Aufgabenblatt9/vehicle.rda")
load("Aufgabenblatt9/segment.rda")

H = c(1,2,3,5,8,13,21)

# Testaufrufe mit Ausgaben

#g)
#Veränderung der SHLP -Funktion -> einbau von recale Änderung

#h)
#Testreihen mit recale = True 
