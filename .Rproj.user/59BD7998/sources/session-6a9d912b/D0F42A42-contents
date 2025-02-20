# Hilfsfunktion zur Überprüfung der Anzahl der Variablen
check <- function(data, ...) {
  var<-c(...)
  if (length(var) < 3 || length(var) > 4) {
    stop("Bitte geben Sie zwischen 3 und 4 kategoriale Variablen an.")
    #Wenn es weniger als 3 oder mehr als 4 Variablen sind gibt es eine Fehlermeldung an
  }
  return(var)
}