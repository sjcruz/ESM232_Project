#' Set up harvest MPA vectors for both predator and prey
#'
#' Computes the change in both predator and prey abundance over time in each patch from species abundance and rates of ecological function
#' @param data 
#' 
#' 
#' 
#' @author Seleni Cruz and Juliette Verstaen
#' @return list with focal prey and predator abundance at each time interval specified 

calc_harvest <- function(hx, hp, ncells, MPA_width)  {
  
  harvest_mpa_hx <- vector(length=ncells)
  harvest_mpa_hx[] <- hx   
  
  # stochastic element of model: location of MPA changes with each model run

  harvest_mpa_hx[sample(ncells, MPA_width)] <- 0  
  harvest_mpa_hp <- harvest_mpa_hx
  harvest_mpa_hp[harvest_mpa_hp == hx] <- hp
  
  return(list(harvest_mpa_hx, harvest_mpa_hp))

}

  
  
  
  
  
