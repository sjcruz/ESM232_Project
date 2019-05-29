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
  
  harvest_mpa_hp <- vector(length=ncells)
  harvest_mpa_hp[] <- hp   
  
  if (MPA_width > 0) {   
    MPA_begin <- round((ncells-MPA_width)/2)+1  #start cell of MPAin the middle of ncells 
    MPA_end <- MPA_begin + MPA_width -1         #end cell of MPA in the middle of ncells 
    harvest_mpa_hx[MPA_begin:MPA_end] <- 0
    
  }
  
  if (MPA_width > 0) {   
    MPA_begin <- round((ncells-MPA_width)/2)+1  #start cell of MPAin the middle of ncells 
    MPA_end <- MPA_begin + MPA_width -1         #end cell of MPA in the middle of ncells 
    harvest_mpa_hp[MPA_begin:MPA_end] <- 0
  
  }
  
  return(list(harvest_mpa_hx, harvest_mpa_hp))

}


  
  
  
  
  
