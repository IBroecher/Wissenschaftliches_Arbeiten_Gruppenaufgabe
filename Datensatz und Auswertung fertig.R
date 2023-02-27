set.seed(1710)

# Mithilfe einer Funktion das ganze Erstellen?
# Oder als Matrix?


# (1) Alter, simuliert aus einer Normalverteilung mit Erwartungswert 25 und 
#     einer Standardabweichung von 2

Alter <- rnorm(n = 100, mean = 25, sd = sqrt(2))  # ??
# auf ganze Zahlen runden?

# (2) Studienfach, zufällig gezogen für alle Personen aus einer Auswahl von 
#     „Statistik“, „Data Science“, „Mathe“ und „Informatik“, wobei die Fächer 
#     „Statistik“ und „Data Science“ mit gleicher Wahrscheinlichkeit studiert
#     werden sollen, „Informatik“ mit einer etwas geringeren Wahrscheinlichkeit
#     und „Mathe“ mit der geringsten Wahrscheinlichkeit

# Stat: 37%   Data: 37%
# Info: 20%
# Mathe: 6%

Studienfach <- sample(c("Statistik", "Data Science", "Mathe", "Informatik"),
                      100, replace = TRUE, prob=c(0.37, 0.37, 0.2, 0.06))

# (3) Interesse an Mathematik, wobei hier ein von Ihnen frei zu wählender
#     Zusammenhang mit Studienfach bestehen darf (aber nicht muss); hier können 
#     Sie überlegen, welchen Zusammenhang Sie für plausibel halten und 
#     entsprechend simulieren; die Variable soll nur Werte ???{1,2,3,4,5,6,7} 
#     annehmen können (wie Sie dafür sorgen ist Ihnen frei überlassen), wobei
#     1 = sehr geringes Interesse, und 7 = sehr hohes Interesse abbildet

# Stat: 3-6  Data: 4-7
# Info: 4-7
# Mathe: 6-7
Interesse_Mathematik = rep(0,100)
for(i in 1:100){
  if (Studienfach[i] == "Statistik")
  {Interesse_Mathematik[i] <- sample(c(3, 4, 5, 6))}
  else 
    if (Studienfach[i] == "Data Science")
    {Interesse_Mathematik[i] <- sample(c(4, 5, 6, 7))}
  else
    if (Studienfach[i] == "Informatik")
    {Interesse_Mathematik[i] <- sample(c(4, 5, 6, 7))}
  else
    if (Studienfach[i] == "Mathe")
    {Interesse_Mathematik[i] <- sample(c(6, 7))}
}


# (4) Interesse an Programmieren, wobei hier ein von Ihnen frei zu wählender 
#     Zusammenhang mit Studienfach bestehen darf (aber nicht muss); hier können
#     Sie überlegen, welchen Zusammenhang Sie für plausibel halten und 
#     entsprechend simulieren, die Variable soll nur Werte ???{1,2,3,4,5,6,7} 
#     annehmen können (wie Sie dafür sorgen ist Ihnen frei überlassen), wobei 
#     1 = sehr geringes Interesse, und 7 = sehr hohes Interesse abbildet

# Stat: 2-5  Data: 6-7
# Info: 6-7
# Mathe: 1-4
Interesse_Programmieren = rep(0, 100)
for(i in 1:100){
  if (Studienfach[i] == "Statistik")
  {Interesse_Programmieren[i] <- sample(c(2, 3, 4, 5))}
  else 
    if (Studienfach[i] == "Data Science")
    {Interesse_Programmieren[i] <- sample(c(6, 7))}
  else
    if (Studienfach[i] == "Informatik")
    {Interesse_Programmieren[i] <- sample(c(6, 7))}
  else
    if (Studienfach[i] == "Mathe")
    {Interesse_Programmieren[i] <- sample(c(1, 2, 3, 4))}
}


# (5) Mathe-LK (ja/nein), eine dichotome Variable, die kodiert, ob jemand in der
#     Schule Mathe-LK hatte oder nicht; hierbei darf (muss aber nicht) ein
#     Zusammenhang Ihrer Wahl mit Studienfach, Interesse an Mathematik und 
#     Interesse an Programmieren bestehen

# Ich weiß nicht, wie man es von mehreren Faktoren abhängig macht, wen du da 
# mehr weißt, dann kannst du es sehr gerne sagen :)
# Ich nehme jetzt mal das Interesse an Mathematik, da man sich ja auch erst im 
# letzten Schuljahr für ein Studium aussuchen kann. Und dann fand ich
# das Mathe Interesse relevanter als das Programmier Interesse.
# Aber da kannst du sehr gerne deine Meinung zu schreiben :)
Mathe_LK = rep(0,100)
for(i in 1:100){
  if (Interesse_Mathematik[i] == 7)
  {Mathe_LK[i] <- 1}
  else
    if (Interesse_Mathematik[i] == 6)
    {Mathe_LK[i] <- sample(c(1, 0), prob=c(0.9, 0.1))}
  else
    if (Interesse_Mathematik[i] == 5)
    {Mathe_LK[i] <- sample(c(1, 0), prob=c(0.7, 0.3))}
  else
    if (Interesse_Mathematik[i] == 4)
    {Mathe_LK[i] <- sample(c(1, 0), prob=c(0.55, 0.55))}
  else
    if (Interesse_Mathematik[i] == 3)
    {Mathe_LK[i] <- sample(c(1, 0), prob=c(0.45, 0.55))}
  else
    if (Interesse_Mathematik[i] == 2 || Interesse_Mathematik[i] == 1)
    {Mathe_LK[i] <- 0}
}


daten <- data.frame(Age = Alter, Subject = Studienfach, Int_Mat = Interesse_Mathematik,Int_Prog = Interesse_Programmieren,MLK = Mathe_LK)

SF_Factor <- ordered( Studienfach )
data = daten[-2]

################################################################################
################################### Git Hib Aufgabe ############################
################################################################################
#                                                                              #
#                      Tobias Hübner, Paula Ruf, Helen Wallner                 #
#                                     13.01.2023                               #
#                                                                              #
################################################################################






A <- function(x, ...){ #Diese Funktion dient einer Übersicht über die numerischen Daten des Datensatzes
  cat(paste("Länge des Datensatzes (N):", length(x), "\n",
            "Minimum:", min(x), "\n",
            "Maximum:", max(x), "\n",
            "Mittelwert:", mean(x), "\n",
            "Median:", median(x), "\n",
            "Standardabweichung:", sd(x) ))
}

A(daten$Age)
#Minimum des Alters ca. 21, Maximum des Alters ist ca. 28
#Mittleres Alter knapp 25, sowie der Median auch mit einer Standardabweichung 
#ca 1Jahr und 3Monaten
A(daten$Int_Mat) #Interesse Mathematik
#Range der Antwortmoeglichkeiten ist 1(kein Interesse) bis 7(grosses Interesse)
#Minimum bei 3, Maximum bei 7, Mittelwert bei 5.33, Median ist 6,
#Standardabweichung bei 1.28
A(daten$Int_Prog) #Interesse Programmieren
#Range der Antwortmoeglichkeiten ist 1(kein Interesse) bis 7(grosses Interesse)
#Minimum bei 1, Maximum bei 7, Mittelwert bei 4.76, Median bei 5,
#Standardabeichung bei 1.88
A(daten$MLK) #Mathe-Lk gewaehlt(0: nein, 1: Ja)
#Mittelwert bei 0.73 und Standardabweichung bei 0.45


B <- function(x, ...) { #Diese Funktion dient einer Übersicht über die kategoriale Variable "Studienfach"
  cat(paste("Faktorstufen:", levels(factor(x)),
            "Häufigkeiten:", table(x) )) #Häufigkeitstabelle
}

B(daten$Subject)
#Data Science Häufigkeiten: 38 
#Informatik Häufigkeiten: 9 
#Mathe Häufigkeiten: 16 
#Statistik Häufigkeiten: 37


LiMo <- function(x, y, ...) { #Lineares Modell wird auf zwei Variablen angewendet
  cat(paste("Korrelation:", cor.test(x, y), "\n", #Korrelationskoeffizient wird ausgewertet
            "lineares Modell:", plot(lm(x ~ y)) ))
}
LiMo(daten$Int_Mat,daten$MLK)
#Korrelation: 0.58 (hohe Korrelation)

LiMo(daten$Int_Mat, (as.numeric(daten$Subject == "Mathe")))
#Korrelation: 0.38 (etwas niedrigere Korrelation)

LiMo(daten$MLK,(as.numeric(daten$Subject == "Mathe")))
#Korrelation: 0.26 (etwas niedrigere Korrelation)


# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammengang zwischen einer metrischen und einer dichotomen Variablen berechnet und ausgibt

# Nach Mathe LK (ja/nein) aufgeteilt, wie das Interesse an Mathematik/ am Programmieren ist

D <- function(daten){ #Unterschied im Interesse an Mathe und Programmieren je nachdem ob Studierende Person Mathe-LK hatte
  i <- round(mean(daten$Int_Mat[daten$MLK == 1]),3)
  ii <- round(mean(daten$Int_Prog[daten$MLK == 1]),3)
  iii <- round(mean(daten$Int_Mat[daten$MLK == 0]),3)
  iv <- round(mean(daten$Int_Prog[daten$MLK == 0]),3)
  cat("Durchschnittsinteresse an Mathematik:\n",
      "\nMit Mathe Leistungskurs: ", i,
      "\nOhne Mathe Leistungskurs: ", iii,
      "\n\nDurchschnittsinteresse Programmieren:\n",
      "\nMit Mathe Leistungskurs: ", ii,
      "\nOhne Mathe Leistungskurs: ", iv)
}

D(daten)
#Man kann erkennen, dass das Durchschnittsinteresse an Mathematik bei Studierenden, die Mathe-LK hatten hoeher ist
#Das Durchschnittsinteresse an Programmieren wird kaum davon beeinflusst, ob die Studierenden Mathe-LK hatten




#(e) Eine Funktion, die eine mindestens ordinal skalierte Variable quantilbasiert kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)

E <- function(x){
  niedrig <- c()
  mittel <- c()
  hoch <- c()
  
  for(i in 1:length(x)){
    if(x[i] <= quantile(x)[1]){    #Alles unter 0.25 Quantil ist niedrig
      niedrig <- append(niedrig, x[i]) 
    }
    if(x[i] > quantile(x)[1] && x[i] <= quantile(x)[3]){ #Alles zwischen 0.25 Quantil und 0.75 Quantil ist mittel
      mittel <- append(mittel, x[i])
    }
    if(x[i] > quantile(x)[3]){ #Alles über 0.75 Quantil ist hoch
      hoch <- append(hoch, x[i])
    }
  }
  x=matrix(c(length(niedrig),length(mittel),length(hoch)),ncol=3) # Matrix zur Übersicht
  colnames(x) = c("niedrig","mittel","hoch")
  x
  
}


E(daten$Int_Prog)
#4 Studierende im Datensatz haben niedriges Interesse an Programmieren, 49 haben
#mittleres Interesse und 47 Studierende haben hohes Interesse an Programmieren

E(daten$Int_Mat)
#10 Studierende im Datensatz haben niedriges Interesse an Mathematik, 70 haben
#mittleres Interesse und 20 Studierende haben hohes Interesse an Mathematik

# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen Variablen erstellt

f <- function(daten){
  par(mfrow = c(2,2))
  barplot(table(daten$Subject), main="Verteilung der Studienfächer",    
          ylab="Abs. Häufigkeiten")
  
  barplot(table(daten$MLK, daten$Subject), beside = TRUE,
          main="Mathe LK aufgeteilt nach Studienfächern",
          ylab="Abs. Häufigkeiten")
  
  # barplot(table(Datensatz$Mathe_LK, Datensatz$Studienfach), beside = TRUE,
  #       main="Mathe LK aufgeteilt nach Studienfächern",
  #       ylab="Abs. Häufigkeiten", col=c("darkred", "orange"))
  # legend("topright", c("Ja", "Nein"), col=c("darkred", "orange"))
  
  barplot(table(daten$Int_Prog, daten$Subject), beside = TRUE,
          main="Interesse Programmieren aufgeteilt nach Studienfächern",
          ylab="Abs. Häufigkeiten")
  
  barplot(table(daten$Int_Mat, daten$Subject), beside = TRUE,
          main="Interesse Mathematik aufgeteilt nach Studienfächern",
          ylab="Abs. Häufigkeiten")
  
  
}
f(daten)
#Ca 40 Studierende haben jeweils Daten Science und Statistik als Studienfach
#Ca. 10 Studierende haben Informatik und ca. 15 Studierende Mathe

#die meisten der Informatik und Data Science Studierenden hatten auch Mathe-LK und
#alle die Mathe studieren hatten Mathe-LK, waehrend Studierende, die Statistik 
#studieren nur ca. zur haelfte Mathe-Lk hatten

#Das Interesse an Programmieren ist in Informatik und Data Science besonders hoch, 
#bei Mathe Studierenden eher gering bis mittel und bei Statistik Studierenden mittel

#Das Interesse an Mathematik ist bei Data Science und Informatik mittel, bei Statistik
#eher niedriger und Mathe-Studierenden sehr hoch
