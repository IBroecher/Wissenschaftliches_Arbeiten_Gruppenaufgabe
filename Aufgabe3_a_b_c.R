################################################################################
################################### Git Hib Aufgabe ############################
################################################################################
#                                                                              #
#    Tobias Hübner, Paula Ruf, Helen Wallner, Xenia Wehner, Ina Bröcher        #
#                                     14.02.2023                               #
#                                                                              #
################################################################################



test <- c(3, 5, 2, 7, 2, 1, 7, 7, 2, 3)

test2 <- c("w", "m", "m", "m", "w", "d", "w", "w", "m")


A <- function(x, ...){
  cat(paste("Länge des Datensatzes (N):", length(x), "\n",
  "Minimum:", min(x), "\n",
  "Maximum:", max(x), "\n",
  "Mittelwert:", mean(x), "\n",
  "Median:", median(x), "\n",
  "Standardabweichung:", sd(x) ))
}


A(test)


B <- function(x, ...) {
  cat(paste("Faktorstufen:", levels(factor(x)),
  "Häufigkeiten:", table(x) ))
}

B(test2)


c <- function(x, y, ...) {
  cat(paste("Korrelation:", corr(x, y), "\n",
  "lineares Modell:", lm(x, y) ))
}

# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammengang zwischen einer metrischen und einer dichotomen Variablen berechnet und ausgibt

# Die einzige dichotome Variable im Datensatz ist der Mathe LK, und die dazu passenste metrische Variable das Interesse an Mathematik.
# Nur habe ich schwierigkeiten, eine passende Darstellung zu finden, da die Variable des Mathe LKs ja non-numeric ist.
# Weiß da wer weiter?

#(e) Eine Funktion, die eine mindestens ordinal skalierte Variable quantilbasiert kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)

teste <- sample(c(1:5), 10, replace = TRUE)

E <- function(x){
  niedrig <- c()
  mittel <- c()
  hoch <- c()
  
  for(i in 1:length(x)){
  if(x[i] <= quantile(x)[2]){
    niedrig <- append(niedrig, x[i]) 
  }
  if(x[i] > quantile(x)[2] && x[i] <= quantile(x)[4]){
    mittel <- append(mittel, x[i])
  }
  if(x[i] > quantile(x)[4]){
    hoch <- append(hoch, x[i])
  }
  }
  cat("niedrig: ", niedrig, "\n",
      "mittel: ", mittel, "\n",
      "hoch: ", hoch)
}

E(teste)


# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen Variablen erstellt

barplot(table(Datensatz$Studienfach), main="Verteilung der Studienfächer",
        ylab="Abs. Häufigkeiten")

barplot(table(Datensatz$Mathe_LK), main="Mathematik Leistungskurs",
        ylab="Abs. Häufigkeiten")

barplot(table(Datensatz$Mathe_LK, Datensatz$Studienfach), beside = TRUE,
        main="Mathe LK aufgeteilt nach Studienfächern",
        ylab="Abs. Häufigkeiten")

barplot(table(Datensatz$Interesse_Programmieren, Datensatz$Studienfach), beside = TRUE,
        main="Interesse Programmieren aufgeteilt nach Studienfächern",
        ylab="Abs. Häufigkeiten")

barplot(table(Datensatz$Interesse_Programmieren, Datensatz$Studienfach), beside = TRUE,
        main="Interesse Programmieren aufgeteilt nach Studienfächern",
        ylab="Abs. Häufigkeiten")
