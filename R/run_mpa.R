

run_mpa <- function(pars ) ###values and time are only necessayr for the ode not here, so take out
    {
  
  mrate_X <- pars[1]
  mrate_P <- pars[2]
  ncells <- pars[3]
 # MPA_width <- pars[4]
  hx <- pars[5]
  hp <- pars[6]
  rx <- pars[7]
  Kx <- pars[8]
  ax <- pars[9]
  c <- pars[10]
  ay <- pars[11]
  dp <- pars[12]
  Kp <- pars[13]
  Y <- 500

  ### add in a pseudo sentivity analysis: 3 sizes: 10, 20, 30
    
  leavingX<-mrate_X*popX
  leavingP<-mrate_P*popP
  
  #The number of immigrants is 1/2 those leaving cells to the left and 1/2 those leaving cells to the right. 
  #The idea here is that individuals evely migrate to left and right cells
  arrivingX<-0.5*leavingX[left.cell]+ 0.5*leavingX[right.cell]
  arrivingP<-0.5*leavingP[left.cell]+ 0.5*leavingP[right.cell]

  #surplus production from the predator prey function
  
  ##will this math work not as a vector?
  surplus <- pred_prey(pars = pars, values= values) 
  
  ##negative surpluses don't really know why
  surplusX <- surplus[1]
  surplusP <- surplus[2]
  
  
  #catches = harvest rate in each cell times the population size 
  #basically how much are we catching at each time step
  
  catchesX <- harvest_mpa_hx*popX
  catchesP <- harvest_mpa_hp*popP
  
  
  #Now that we caught some fish and some migrated we update the population numbers
  popX <- popX + surplusX - catchesX - leavingX + arrivingX
  popP <- popP + surplusP - catchesP - leavingP + arrivingP
  
  popXsum <- sum(popX)
  popPsum <- sum(popP)

  
  return(list(c(popX, popP))) 
  
  }

