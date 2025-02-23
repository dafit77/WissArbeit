
#2.v
#Eine Funktion, die eine geeignete Visualisierung von drei oder vierkategorialen Variablen erstellt

source("Aufgabe_2_Skript_2.R")


#' Title
#'
#' @param data 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
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

