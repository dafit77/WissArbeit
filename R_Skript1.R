source("R_Skript2.R")

# func_spread_summary gibt für einen numerischen Vektor, mit den Hilfsfunktionen
# Interquartilsabstand, Spannweite, Varianz und Variationskoeffizient
# Rückgabe erfolgt als Liste der einzelnen Werte
#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
func_spread_summary <- function(data){
  if(!is.numeric(data)){
    stop(paste("Eingabe ist nicht numerisch, sondern",typeof(data)))
  }
  func_variationscoeff <- function(data) {
    abs_freq <- func_absolute_hkeit(data)  
    mean_freq <- mean(abs_freq)            
    sd_freq <- sd(abs_freq)               
    
    if (mean_freq == 0) {
      return(NA) 
    }
    
    return(sd_freq / mean_freq)  
  }
  
  
  iqr <- func_IQR(data = data)
  range <- func_range(data = data)
  variance <- var(data, na.rm = TRUE)
  var_coef <- func_variationscoeff(data)
  
  
  
  
  return(list("InterquartileRange" = iqr, "Range" = range, "Variance" = variance, "Variationcoefficient" = var_coef))
  
  
}

# func_categorial_summary bestimmt für einen ordinalen Vektor den Modus, die relative Häufigkeit jedes Wertes,
# Sowie die Anzahl Kategorien
#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
func_categorial_summary <- function(data){
  if (!is.character(data) && !is.factor(data)) {
    stop(paste("Eingabe muss ein kategorialer Vektor sein (Faktor oder Character). Aber der Vektor ist:",
               typeof(data)))
  }
  
  mode <- func_modus_kategorial(data)
  relfreq <- func_relative_hkeit(data)
  Categories <- func_Kategorien(data)
  
  return(list("Modus" = mode, "RelativeFrequency" = relfreq, "NumberCategories" = Categories))
  
}


# kategorial_data <- c("Colin", "Paul", "David", "Michel", "Max", "Michel", "David", "David", "Michel", "David")
# func_categorial_summary(kategorial_data)
