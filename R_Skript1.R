source("R_Skript2.R")

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
  
  iqr <- func_IQR(data = data)
  range <- func_range(data = data)
  variance <- var(data)
  
  
  return(list("InterquartileRange" = iqr, "Range" = range, "Variance" = variance))
  
  
}


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
