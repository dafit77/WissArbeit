source("Aufgabe_2_Skript_2.R")

#2.i
#Eine Funktion, die verschiedene geeignete deskriptive Statistiken für metrische Variablen berechnet und ausgibt

# func_spread_summary gibt für einen numerischen Vektor, mit den Hilfsfunktionen
# Interquartilsabstand, Spannweite, Varianz und Variationskoeffizient
# Rückgabe erfolgt als Liste der einzelnen Werte

func_spread_summary <- function(data){
  if(!is.numeric(data)){
    stop(paste("Eingabe ist nicht numerisch, sondern",typeof(data)))
  } #Kontrolle ob die Eingabe korrekt ist, abbruch und Fehlermeldung wenn nicht
  func_variationscoeff <- function(data) {
    mean <- mean(data)            
    sd <- sd(data)               
    #Variationskoeffizient berechnet sich durch Standardabweichung/ Standardabweichung
    #normiert durch die "Einheit"
    if (mean == 0) {
      return(NA) 
    }
    # Abbruch für mean = 0
    return(sd / mean)  
  }
  
  
  iqr <- func_IQR(data = data)
  range <- func_range(data = data)
  variance <- var(data, na.rm = TRUE)
  var_coef <- func_variationscoeff(data)
  
  
  
  
  return(list("InterquartileRange" = iqr, "Range" = range, "Variance" = variance, "Variationcoefficient" = var_coef))
  
  
}

# func_categorial_summary bestimmt für einen ordinalen Vektor den Modus, die relative Häufigkeit jedes Wertes,
# Sowie die Anzahl Kategorien

#2. ii
#Eine Funktion, die verschiedene geeignete deskriptive Statistiken für kategoriale Variablen berechnet und ausgibt

func_categorial_summary <- function(data){
  if (!is.character(data) && !is.factor(data)) {
    stop(paste("Eingabe muss ein kategorialer Vektor sein (Faktor oder Character). Aber der Vektor ist:",
               typeof(data)))
  }
  # Ebenfalls abfangen von Fehlermeldung bei falschem Datentyp in der Eingabe 
  
  mode <- func_modus_kategorial(data)
  relfreq <- func_relative_hkeit(data)
  Categories <- func_Categories(data)
  
  return(list("Modus" = mode, "RelativeFrequency" = relfreq, "NumberCategories" = Categories))
  
}


# kategorial_data <- c("Colin", "Paul", "David", "Michel", "Max", "Michel", "David", "David", "Michel", "David")
# func_categorial_summary(kategorial_data)

#2. iii
#Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
#Zusammenhang zwischen zwei kategorialen Variablen berechnet
#ausgibt

bivariate_stats <- function(data, var1, var2) {
  library(dplyr)
  library(vcd)  # Für Assoziationsmaße wie Phi-Koeffizient
  library(psych) # Für Punktbiseriale Korrelation
  
  # Prüfen, ob var1 dichotom und var2 metrisch ist
  if (length(unique(data[[var1]])) == 2 && is.numeric(data[[var2]])) {
    cat("\nPunktbiseriale Korrelation:\n")
    point_biserial <- suppressWarnings(biserial.cor(data[[var2]], as.numeric(as.factor(data[[var1]]))))
    print(point_biserial)
  }
  
  # Kontingenztabelle
  cat("\nKontingenztabelle:\n")
  tbl <- suppressWarnings(table(data[[var1]], data[[var2]]))
  print(tbl)
  
  # Relative Häufigkeiten
  cat("\nRelative Häufigkeiten (gesamt):\n")
  print(suppressWarnings(prop.table(tbl)))
  
  cat("\nRelative Häufigkeiten (zeilenweise):\n")
  print(suppressWarnings(prop.table(tbl, margin = 1)))
  
  cat("\nRelative Häufigkeiten (spaltenweise):\n")
  print(suppressWarnings(prop.table(tbl, margin = 2)))
  
  # Phi-Koeffizient (nur für dichotome Variablen)
  if (length(unique(data[[var1]])) == 2 && length(unique(data[[var2]])) == 2) {
    cat("\nPhi-Koeffizient:\n")
    phi_value <- suppressWarnings(assocstats(tbl)$phi)
    print(phi_value)
  } else {
    cat("\nPhi-Koeffizient nicht berechnet: Mindestens eine Variable hat mehr als zwei Kategorien.\n")
  }
}

# Beispielaufruf mit Titanic-Daten
# bivariate_stats(titanic_data, "Survived", "Sex")


# bivariate_stats(titanic_Berichtigt,"Survived", "Sex")

#2. iv
#Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammengang zwischen einer metrischen und einer dichotomen Variablen berechnet und ausgibt

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

#2.v
#Eine Funktion, die eine geeignete Visualisierung von drei oder vierkategorialen Variablen erstellt
Visualisierung <- function(data, ...) {
  var <- check(data, ...)
  
  par(mfrow = c(length(var), 1), mar = c(5, 5, 2, 1))
  #passt grafik an je nach anzahl an variablen
  
  for (i in seq_along(var)) {
    # Berechne relative Häufigkeiten
    rel.Haeufigkeit <- prop.table(table(data[[var[i]]]))
    
    # Erstelle das Balkendiagramm
    heights <- barplot(rel.Haeufigkeit,
                       main = paste("Relative Häufigkeit von", var[i]),
                       xlab = var[i],
                       ylab = "Relative Häufigkeit",
                       col = rainbow(length(rel.Haeufigkeit)), 
                       # Verwende rainbow für unterschiedliche Farben
                       border = "black",
                       ylim = c(0, max(c(rel.Haeufigkeit)) * 1.2)) 
                       #Setze ymax auf einen Wert über dem Maximum
    
    # Füge die relativen Häufigkeiten über den Balken hinzu
    text(x = heights,
         y = rep(min(rel.Haeufigkeit) * 1.05, length(rel.Haeufigkeit)), 
         # Positionierung etwas über dem niedrigsten Punkt aller Balken
         labels = round(rel.Haeufigkeit, digits = 2), 
         # Rel. Häufigkeit in Prozent anzeigen
         pos = 3) 
         # pos=3 platziert den Text oberhalb des Punktes
    
    
  }
  
}
