

MPA_setup <- function(pars, values) ###values and time are only necessayr for the ode not here, so take out
    {
  
#  mrate_X <- pars[1]
 # mrate_P <- pars[2]
  ncells <- pars[3]
  #MPA_width <- pars[4]
  hx <- pars[5]
  hp <- pars[6]
  #rx <- pars[7]
  #Kx <- pars[8]
  #ax <- pars[9]
  #c <- pars[10]
  #ay <- pars[11]
  #dp <- pars[12]
  #Kp <- pars[13]
  #Y <- 500

  ### add in a pseudo sentivity analysis: 3 sizes: 10, 20, 30

  #this is now in the wrapper function to keep it cleaner? can delete at the end if keep that organization  
# note, the calc harvest function returns both x and p numbers in one list, need to extract to two
#  harvest_all <- calc_harvest_stochastic(pars)
#  harvest_mpa_hx <-  harvest_all[1] %>%
#    as_vector()
#  harvest_mpa_hp <-  harvest_all[2] %>%
#    as_vector()
  
# create empty vectors to fill with population abundance
  popX <- vector(length=ncells) 
  popP <- vector(length=ncells) 
  
# extract the last values THIS IS WRONG
  # wont this restart the process every time?
  # do i need to set up the MPA population vectors seperatly?
  #need to divide by ncells beause the total population is getting spread over space (ie: the vector)
  X <- values[1]/ncells
  P <- values[2]/ncells
  
  ### fill vector with population abundance of X and P
  popX[]<-X
  popP[]<-P

#create left and right cells that the fish and move into (will be looped over time in the wrapper function)  
  left.cell<- c(ncells, 1: (ncells-1))
  right.cell<- c(2: ncells, 1)
  
  return(list(c(popX, popP, left.cell, right.cell))) 
  
  }

