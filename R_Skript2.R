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
  
  x <- sort(x, na.last = NA)  
  
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
