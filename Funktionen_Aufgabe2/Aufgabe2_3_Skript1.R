# source("Skript 2 Aufgabe 2 a iii, iv")
bivariate_stats <- function(data, var1, var2) {
  library(dplyr)
  library(vcd)  # Für Assoziationsmaße wie Phi-Koeffizient
  library(psych) # Für Punktbiseriale Korrelation
  
  # Prüfen, ob var1 dichotom und var2 metrisch ist
  if (length(unique(data[[var1]])) == 2 && is.numeric(data[[var2]])) {
    cat("\nPunktbiseriale Korrelation:\n")
    point_biserial <- biserial.cor(data[[var2]], as.numeric(as.factor(data[[var1]])))
    print(point_biserial)
  }
  
  # Kontingenztabelle
  cat("\nKontingenztabelle:\n")
  tbl <- table(data[[var1]], data[[var2]])
  print(tbl)
  
  # Relative Häufigkeiten
  cat("\nRelative Häufigkeiten (gesamt):\n")
  print(prop.table(tbl))
  
  cat("\nRelative Häufigkeiten (zeilenweise):\n")
  print(prop.table(tbl, margin = 1))
  
  cat("\nRelative Häufigkeiten (spaltenweise):\n")
  print(prop.table(tbl, margin = 2))
  
  # Phi-Koeffizient (nur für dichotome Variablen)
  if (length(unique(data[[var1]])) == 2 && length(unique(data[[var2]])) == 2) {
    cat("\nPhi-Koeffizient:\n")
    phi_value <- assocstats(tbl)$phi
    print(phi_value)
  } else {
    cat("\nPhi-Koeffizient nicht berechnet: Mindestens eine Variable hat mehr als zwei Kategorien.\n")
  }
}

# Beispielaufruf mit Titanic-Daten
# bivariate_stats(titanic_data, "Survived", "Sex")


# bivariate_stats(titanic_Berichtigt,"Survived", "Sex")


bivariate_statsfkt <- function(data, dichotom_var, metric_var) {
  library(dplyr)
  library(psych)  # Für Punktbiseriale Korrelation
  
  
  # Prüfen, ob die Variablen korrekt sind
  if (!(dichotom_var %in% names(data)) || !(metric_var %in% names(data))) {
    stop("Eine oder beide Variablen existieren nicht im Datensatz.")
  }
  
  if (length(unique(data[[dichotom_var]])) != 2) {
    stop("Die erste Variable muss dichotom sein (nur zwei eindeutige Werte).")
  }
  
  if (!is.numeric(data[[metric_var]])) {
    stop("Die zweite Variable muss metrisch sein.")
  }
  
  # Punktbiseriale Korrelation berechnen
  cat("\nPunktbiseriale Korrelation:\n")
  point_biserial <- biserial(data[[metric_var]], as.numeric(as.factor(data[[dichotom_var]])))
  print(point_biserial)
} 


# Beispielaufruf mit Titanic-Daten:
# bivariate_stats(titanic_data, "Survived", "Age")

# bivariate_statsfkt(titanic_Berichtigt,"Survived", "Age")

