#' MPA Spatial Model
#'
#' Computes the change in both predator and prey abundance over time in each patch from species abundance and rates of ecological function
#' @param data 
#' 
#' 
#' 
#' @author Seleni Cruz and Juliette Verstaen
#' @return list with focal prey and predator abundance at each time interval specified 

calc.harvest.vec <- function(harvest, ncells, MPA.width)  {
  harvest.vec <- vector(length=ncells) #create vector to store harvest rates in each cell hence the the length is ncells 
  harvest.vec[] <- harvest      
  if (MPA.width > 0) {   
    MPA.begin <- round((ncells-MPA.width)/2)+1  #start cell of MPAin the middle of ncells 
    MPA.end <- MPA.begin + MPA.width -1         #end cell of MPA in the middle of ncells 
    harvest.vec[MPA.begin:MPA.end] <- 0        
  }
  return(harvest.vec)

}


