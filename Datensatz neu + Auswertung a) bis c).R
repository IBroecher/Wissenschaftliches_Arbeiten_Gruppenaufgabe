# 100 Personen, je 5 Variablen + ID-Spalte

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
                      #     entsprechend simulieren; die Variable soll nur Werte ∈{1,2,3,4,5,6,7} 
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
                      #     entsprechend simulieren, die Variable soll nur Werte ∈{1,2,3,4,5,6,7} 
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
A(daten$Age)
A(daten$Int_Mat)
A(daten$Int_Prog)
A(daten$MLK)
################################################################################
################################### Git Hib Aufgabe ############################
################################################################################
#                                                                              #
#                      Tobias Hübner, Paula Ruf, Helen Wallner                 #
#                                     13.01.2023                               #
#                                                                              #
################################################################################






A <- function(x, ...){
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


B <- function(x, ...) {
  cat(paste("Faktorstufen:", levels(factor(x)),
            "Häufigkeiten:", table(x) ))
}

B(daten$Subject)
# Data Science Häufigkeiten: 38 
#Informatik Häufigkeiten: 9 
#Mathe Häufigkeiten: 16 
#Statistik Häufigkeiten: 37


LiMo <- function(x, y, ...) {
  cat(paste("Korrelation:", cor.test(x, y), "\n",
            "lineares Modell:", plot(lm(x ~ y)) ))
}
LiMo(daten$Int_Mat,daten$MLK)
#Korrelation: 0.58 (hohe Korrelation)
LiMo(daten$Int_Mat, daten$Subject == "Mathe")




                        






