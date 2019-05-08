library(gridExtra)
pdf("quantize.pdf")

#a
quantize <- function(x,bits=8){
  numberOfIntervals <- 2^bits
  sizeOfIntervals <- 1/numberOfIntervals
  min <- sizeOfIntervals/2
  max <- 1 - min
  x <- ifelse(x <= 0, min, x)
  x <- ifelse(x >= 1, max, x)
  x <- ((floor(x * numberOfIntervals))/numberOfIntervals) + min
}

#b
#auskommentierte Objekte befinden sich schon in der rda-Datei
#test_seq = seq(0,1,length=5000)**3
plot(quantize(test_seq))

#c 
decibel <- function(x){
  10 * log10 (var(x))
}

SNR <- function(x,y){
  decibel(x) - decibel(x-y)
}

empty_data <- c(NA,NA,NA)
snr_data <- data.frame("Bits" = c(8,12,16), "d" = empty_data, "e" = empty_data,
                       "f" = empty_data, "g" = empty_data, "h" = empty_data)

#d
snr_data[1,"d"] <- SNR(test_seq, quantize(test_seq, 8))
snr_data[2,"d"] <- SNR(test_seq, quantize(test_seq, 12))
snr_data[3,"d"] <- SNR(test_seq, quantize(test_seq, 16))

#e
#norm_test_seq = runif(5000, 0, 1)
snr_data[1,"e"] <- SNR(norm_test_seq, quantize(norm_test_seq, 8))
snr_data[2,"e"] <- SNR(norm_test_seq, quantize(norm_test_seq, 12))
snr_data[3,"e"] <- SNR(norm_test_seq, quantize(norm_test_seq, 16))

#f
#rnorm_test_seq = rnorm(5000, 1/2, 1/8)
snr_data[1,"f"] <- SNR(rnorm_test_seq, quantize(rnorm_test_seq, 8))
snr_data[2,"f"] <- SNR(rnorm_test_seq, quantize(rnorm_test_seq, 12))
snr_data[3,"f"] <- SNR(rnorm_test_seq, quantize(rnorm_test_seq, 16))

#g
#aequi_test_seq <- seq(from=0, to=1, length=5000)
snr_data[1,"g"] <- SNR(aequi_test_seq, quantize(aequi_test_seq, 8))
snr_data[2,"g"] <- SNR(aequi_test_seq, quantize(aequi_test_seq, 12))
snr_data[3,"g"] <- SNR(aequi_test_seq, quantize(aequi_test_seq, 16))

#h
#h_test_seq = runif(5000,-1/10,1)
snr_data[1,"h"] <- SNR(h_test_seq, quantize(h_test_seq, 8))
snr_data[2,"h"] <- SNR(h_test_seq, quantize(h_test_seq, 12))
snr_data[3,"h"] <- SNR(h_test_seq, quantize(h_test_seq, 16))

grid.table(format(snr_data, digits = 5))

#i
C <- seq(1, 8, length.out = 36) 
C[(which(C==4))] <- 4.001

SNR_results <- numeric(0)
for(i in C){
  i_test_seq = rnorm(5000,1/2,1/(2*i))
  SNR_results <- c(SNR_results, SNR(i_test_seq, quantize(i_test_seq)))
}

plot(C, SNR_results)
dev.off()
