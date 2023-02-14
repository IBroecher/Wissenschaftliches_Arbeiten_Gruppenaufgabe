################################################################################
################################### Git Hib Aufgabe ############################
################################################################################
#                                                                              #
#                      Tobias H�bner, Paula Ruf, Helen Wallner                 #
#                                     13.01.2023                               #
#                                                                              #
################################################################################



test <- c(3, 5, 2, 7, 2, 1, 7, 7, 2, 3)

test2 <- c("w", "m", "m", "m", "w", "d", "w", "w", "m")


A <- function(x, ...){
  cat(paste("L�nge des Datensatzes (N):", length(x), "\n",
  "Minimum:", min(x), "\n",
  "Maximum:", max(x), "\n",
  "Mittelwert:", mean(x), "\n",
  "Median:", median(x), "\n",
  "Standardabweichung:", sd(x) ))
}


A(test)


B <- function(x, ...) {
  cat(paste("Faktorstufen:", levels(factor(x)),
  "H�ufigkeiten:", table(x) ))
}

B(test2)


c <- function(x, y, ...) {
  cat(paste("Korrelation:", corr(x, y), "\n",
  "lineares Modell:", lm(x, y) ))
}