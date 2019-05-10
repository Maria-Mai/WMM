### Aufgabe 2
### Betragsquadratspektrum |G(exp(i*omega))|^2 eines kausalen FIR-Systems

# a) Berechnung der Werte |G(exp(i*omega))|^2 der Frequenzantwort G(z) = G(exp(i*omega))

sms.Z <- function(g,n = length(g)) {
  # g -> Vektor mit vorherigen Impulsantworten
  # n -> Anzahl der äquidistanten Kreisfrequenzen omega im Intervall [0, pi]
  
  w.vec <- seq(0,pi, length.out = n) # Vektor mit äquidistanten Kreisfrequenzen
  z.vec <- exp(w.vec*1i) # Vektor mit komplexen Zahlen der Kreisfrequenzen
  mu <- seq(1,length(g)) # = mu läuft von 0, ..., M ; M ist erste/letzte Element, das in Frequenzantwort miteinfließt
  
  z_mu <- outer(z.vec,-mu,"^") # Operation z^(-mu)
  Gz <- rowSums(sweep(z_mu, 2, g, "*")) # Operation g * z^(-mu)
  abs(Gz)^2 # Operation Betragsquadrat
}



set.seed(1)
g = runif(20, 0.0, 1.0)
sms.Z(g,10)


