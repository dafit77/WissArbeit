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
    pquantil <- x[untere_grenzen] + (index - untere_grenze) * (x[obere_grenze] - x[untere_grenze])
  }
  
  names(pquantil) <- paste0(perc * 100, "%")
  return(pquantil)
}

# Testen: numerischer Vektor, character Vektor , boolean Vektor

a <- c("Paul", "Michel", "Colin", "David", "Max")
func_quantil(a, .5)
# Wie erwartet eine Fehlermeldung
b <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)
func_quantil(b, .05)
func_quantil(b, 10)
# Ebenfalls wie erwartet eine Fehlermeldung
c <- c(1,2,3,4,5,6,7,8,9,10)
func_quantil(c,1)
# Liefert Korrekt das Maximum zurück
func_quantil(c,0)
# Liefert Korrekt das Minimum zurück
func_quantil(c,5)
# Korrekterweise Fehlermeldung, da >1
func_quantil(c,c(0.5,0.05))
# Korrekterweise Fehlermeldung, da Vektor statt Zahl übergeben
