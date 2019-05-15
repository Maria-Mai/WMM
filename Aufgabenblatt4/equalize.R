#Loading data
pics = load("Aufgabenblatt4/equalize.rda", verbose = T)

#Function to equalize picture matrices
equalize <- function(x) {
  #TODO calculate relative grey value [q(l)] hist for [fnm]
  # -> number of grexvlues divided by the total muber of picture points
  #compute cumulative grey value hist qsum(gamma) = sum_l=0_gamma(q(gamma))
  #transform all picture points to floor((L-1) * qsum(fnm)) with [0, L-1] and L= 2^b the quantisierungsbedingte Grauindexbereich 
  # B = 8 -> 2^b -> 256 -> grey values from 0 to 255
  
  b = 8
  L= 2^b
  
  greyValues = hist(x, breaks = seq(0,1, l = L), plot=FALSE)[[2]]
  relGreyValues = greyValues/length(x)
  
  qsum = cumsum(relGreyValues)
  
  

}

x <- get(pics[1])
max(greyValues)
max(relGreyValues)
length(x)
hist(x, breaks = seq(0,1, l = L))
sum(relGreyValues)

y = 2:5
for (i in y) {
  print(i)
  
}
