## Einlesen der Daten
funktionen = load(file = "Übung 2/claude.rda")
wave = get(funktionen[1])
shannon = get(funktionen[2])

## als PDF zu beantworten -> was genau macht outer und sweep funktion

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

## e) 