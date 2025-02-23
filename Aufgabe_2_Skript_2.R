# Aufgabe 2b) 

# Helper Funktionen für i und ii


# Quantil für Funktion func_spread_summary 
# eine Funktion die verschiedene Deskriptive Statistiken berechnet (Metrisch)

#' Title
#'
#' @param x 
#' @param perc 
#'
#' @return
#' @export
#'
#' @examples
func_quantil <- function(x, perc) {
  if (!is.numeric(x)) {
    stop(paste("Eingabe muss numerisch sein, ist aber", typeof(x)))
  }
  if (!is.numeric(perc) || length(perc) != 1 || perc < 0 || perc > 1) {
    stop("Funktionsargument muss eine Zahl zwischen 0 (Minimum) und 1 (Maximum) sein.")
  }
  #Kontrolliert ob die Eingabe wirklich ein zulässiger Wert ist
  
  x <- sort(x, na.last = NA)  
  # Sortiere den Vektor
  n <- length(x)
  index <- perc * (n - 1) + 1  
  untere_grenze <- floor(index)
  obere_grenze <- ceiling(index)
  
  if (untere_grenze == obere_grenze) {
    pquantil <- x[untere_grenze]
  } else {
    pquantil <- x[untere_grenze] + (index - untere_grenze) * (x[obere_grenze] - x[untere_grenze])
  }
  
  names(pquantil) <- paste0(perc * 100, "%")
  return(pquantil)
}

# Testen: numerischer Vektor, character Vektor , boolean Vektor

# a <- c("Paul", "Michel", "Colin", "David", "Max")
# func_quantil(a, .5)
# Wie erwartet eine Fehlermeldung
# b <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)
# func_quantil(b, .05)
# func_quantil(b, 10)
# Ebenfalls wie erwartet eine Fehlermeldung
# c <- c(1,2,3,4,5,6,7,8,9,10)
# func_quantil(c,1)
# Liefert Korrekt das Maximum zurück
# func_quantil(c,0)
# Liefert Korrekt das Minimum zurück
# func_quantil(c,5)
# Korrekterweise Fehlermeldung, da >1
# func_quantil(c,c(0.5,0.05))
# Korrekterweise Fehlermeldung, da Vektor statt Zahl übergeben

# Spannweite oder auch Range berechnet die Distanz von Maximum und Minimum

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
func_range <- function(data){
  if (!is.numeric(data)) {
    stop(paste("Eingabe muss ein numerischer Vektor sein stattdessen ist", typeof(data)))
  }
  
  range <- unname(func_quantil(data,1))-unname(func_quantil(data,0))
  return(range)
}

# Der Interquartilsabstand nutzt die Quantil Funktion um den Abstand von q0.75 und q0.25

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
func_IQR <- function(data){
  if(!is.numeric(data)){
    stop(paste("Eingabe muss ein numerischer Vektor sein stattdessen ist", typeof(data)))
  }
  
  IQR <- unname(func_quantil(data, .75))-unname(func_quantil(data, .25))
  return(IQR)
}



# Modus berechnet den häufigsten Wert von Eingabevektor x 

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
func_modus <- function(x) {
  if (length(x) == 0) {
    stop("Der Vektor darf nicht leer sein")
  }
  
  tab <- table(x)
  
  max_freq <- max(tab)
  
  modus <- names(tab[tab == max_freq])
  
  if (is.numeric(x)) {
    return(as.numeric(modus))
  } else {
    return(modus)
  }
}


# func_absolute_hkeit berechnet wie häufig jeder Wert vorgekommen ist (absolute Häufigkeit)

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
func_absolute_hkeit <- function(data) {
  return(table(data))
}


# func_relative_keit berechnet die relative Häufigkeit der unique Werte von data

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
func_relative_hkeit <- function(data) {
  return(prop.table(table(data)))
}


# func_modus_kategorial berechnet ebenfalls den Modus (Diskussion ob überflüssig)

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
func_modus_kategorial <- function(x) {
  haeufigkeiten <- func_absolute_hkeit(x)
  mostfrequent<- max(haeufigkeiten)
  return(names(haeufigkeiten[haeufigkeiten == mostfrequent]))
}

# func_Categories gibt an wieviele unterschiedliche Kategorien es gibt

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
func_Categories <- function(x) {
  return(length(unique(x)))
}

## für iii


#2.v
# Hilfsfunktion zur Überprüfung der Anzahl der Variablen
#' Title
#'
#' @param data 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
check <- function(data, ...) {
  var<-c(...)
  if (length(var) < 3 || length(var) > 4) {
    stop("Bitte geben Sie zwischen 3 und 4 kategoriale Variablen an.")
    #Wenn es weniger als 3 oder mehr als 4 Variablen sind gibt es eine Fehlermeldung an
  }
  return(var)
}
