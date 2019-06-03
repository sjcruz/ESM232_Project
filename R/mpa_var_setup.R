#' Set up harvest MPA vectors for both predator and prey RENAME THIS
#'
#' Computes the change in both predator and prey abundance over time in each patch from species abundance and rates of ecological function
#' @param data 
#' 
#' 
#' 
#' @author Seleni Cruz and Juliette Verstaen
#' @return list with focal prey and predator abundance at each time interval specified 

mpa_var_setup <- function(pars, values=values)  {

  ncells <- pars[3]
  MPA_width <- pars[4]
  hx <- pars[5]
  hp <- pars[6]
  
  # set up harvest vectors (ie: where the MPAs go = 0 harvest)
  # stochastic element of model: location of MPA changes with each model run
  harvest_mpa_hx <- vector(length=ncells)
  harvest_mpa_hx[] <- hx   
  
  harvest_mpa_hx[sample(ncells, MPA_width)] <- 0  
  harvest_mpa_hp <- harvest_mpa_hx
  harvest_mpa_hp[harvest_mpa_hp == hx] <- hp
  
  # create empty vectors to fill with population abundance
  popX <- vector(length=ncells) 
  popP <- vector(length=ncells) 
  
  # need to divide by ncells beause the total population is getting spread over space (ie: the vector)
  X <- values[1]/ncells
  P <- values[2]/ncells
  
  # fill vector with population abundance of X and P
  popX[]<-X
  popP[]<-P
  
  # create left and right cells that the fish and move into 
  left.cell<- c(ncells, 1: (ncells-1))
  right.cell<- c(2: ncells, 1)
  
  # all the necessary vectors with rates and fish in there to run the actual spatial mpa model
  return(list(harvest_mpa_hx, harvest_mpa_hp, popX, popP, left.cell, right.cell))

}

  
  
  
  
  
