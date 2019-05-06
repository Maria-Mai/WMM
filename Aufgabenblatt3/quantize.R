#a
quantize <- function(x,bits=8){
  numberOfIntervals <- 2^bits
  sizeOfIntervals <- 1/numberOfIntervals
  max <- 1 - sizeOfIntervals/2
  min <- sizeOfIntervals/2
  ifelse(x < 0, min, x)
  ifelse(x > 1, max, x)
  ((floor(x * numberOfIntervals))/numberOfIntervals) + min
}

#b
test_seq = seq(0,1,length=5000)**3
plot(quantize(test_seq))

#c 
decibel <- function(x){
  10 * log10 (var(x))
}

SNR <- function(x,y){
  decibel(x) - decibel(x-y)
}

#d sollte das nicht kleiner werden??? todo
SNR(test_seq, quantize(test_seq, 8))
SNR(test_seq, quantize(test_seq, 12))
SNR(test_seq, quantize(test_seq, 16))

#e
norm_test_seq = runif(5000, 0, 1)
SNR(norm_test_seq, quantize(norm_test_seq, 8))
SNR(norm_test_seq, quantize(norm_test_seq, 12))
SNR(norm_test_seq, quantize(norm_test_seq, 16))

#f
rnorm_test_seq = rnorm(5000, 1/2, 1/8)
SNR(rnorm_test_seq, quantize(rnorm_test_seq, 8))
SNR(rnorm_test_seq, quantize(rnorm_test_seq, 12))
SNR(rnorm_test_seq, quantize(rnorm_test_seq, 16))

#g todo seq
aequi_test_seq <- seq(from=0, to=1, length=5000)
SNR(aequi_test_seq, quantize(aequi_test_seq, 8))
SNR(aequi_test_seq, quantize(aequi_test_seq, 12))
SNR(aequi_test_seq, quantize(aequi_test_seq, 16))

#h
h_test_seq = runif(5000,-1/10,1)
SNR(h_test_seq, quantize(h_test_seq, 8))
SNR(h_test_seq, quantize(h_test_seq, 12))
SNR(h_test_seq, quantize(h_test_seq, 16))

#i
rnorm(5000,1/2,1/2*C)
