source("R_Skript2.R")

func_range <- function(data){
  if (!is.numeric(data)) {
    stop(paste("Eingabe muss ein numerischer Vektor sein stattdessen ist", typeof(data)))
  }
  
  range <- unname(func_quantil(data,1))-unname(func_quantil(data,0))
  return(range)
}

func_IQR <- function(data){
  if(!is.numeric(data)){
    stop(paste("Eingabe muss ein numerischer Vektor sein stattdessen ist", typeof(data)))
  }
  
  IQR <- unname(func_quantil(data, .75))-unname(func_quantil(data, .25))
  return(IQR)
}
