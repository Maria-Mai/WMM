## Einlesen der Daten
funktionen = load(file = "Aufgaben/Aufgabenblatt2/claude.rda")
wave = get(funktionen[1])
shannon = get(funktionen[2])

## b) Funktion plotten
curve(expr = wave, from = 1, to = 2, n = 501, col = "blue")

## c) Wave-Plot im Intervall [1,2]
# äquidistante Abtastzeitpunkte t1,...,tn
t.seq = seq(1,2,by = 1/50)
# zugehörige Abtastwerte
f.seq = wave(t.seq)
# Plotten der Abtastzeiten und -werte
plot(x = t.seq, y = f.seq, col = "blue", type = "h")


## d) 
# Originaldaten zuweisen
f.seq.org = shannon(x = t.seq, times = t.seq, samples = f.seq)
# Plotten des Originalsignals
points(x = t.seq, y = f.seq.org, col = "red", pch = 16)

## e) Überlagern der Abtastfunktion und der rekonstruierten Kurve
curve(expr = wave, from = 1, to = 2, n = 501, col = "blue")
curve(expr = shannon(x,t.seq,f.seq), from = 1, to = 2,n = 501, col ="red",add=TRUE)

## f) Plot mit unterschiedlichen Abtastfrequenzen
layout(matrix(c(1:4),ncol=2))
freq= c(8,16,32,64)
for (hertz in freq) {
  thertz = seq(1,2,by = 1/hertz)
  fhertz = wave(thertz)
  curve(expr = wave, from = 1, to = 2, n = 501, col = "blue", ylab = "f(x)", main = paste("Sampling frequency", hertz, "Hz", sep = " "))
  curve(expr = shannon(x,thertz,fhertz), from = 1, to = 2,n = 501, col ="red",add=TRUE)
}

layout(1)

## g) Kurvenverlauf im Intervall [0,3]
curve(expr = wave, from = 0, to = 3, n = 501, col = "blue")
curve(expr = shannon(x,t.seq,f.seq), from = 0, to = 3,n = 501, col ="red",add=TRUE)