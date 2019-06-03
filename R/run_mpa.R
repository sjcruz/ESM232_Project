

run_mpa <- function(time, values, pars_mpa) 
    {
  
  mrate_X <- pars_mpa[[1]]
  mrate_P <- pars_mpa[[2]]
  harvest_mpa_hx <- pars_mpa[[3]]
  harvest_mpa_hp <- pars_mpa[[4]]
  popX <- pars_mpa[[5]] ## these somehow need to be be the new population sizes from the results of the end of this model
  popP <- pars_mpa[[6]] ##
  left.cell <- pars_mpa[[7]]
  right.cell <- pars_mpa[[8]]
  rx <- pars_mpa[[9]]
  Kx <- pars_mpa[[10]]
  ax <- pars_mpa[[11]]
  c <- pars_mpa[[12]]
  ay <- pars_mpa[[13]]
  dp <- pars_mpa[[14]]
  Kp <- pars_mpa[[15]]
  hx <- pars_mpa[[16]]
  hp <- pars_mpa[[17]]

  ### add in a pseudo sentivity analysis: 3 sizes: 10, 20, 30

  leavingX<-mrate_X*popX
  leavingP<-mrate_P*popP
  
  #The number of immigrants is 1/2 those leaving cells to the left and 1/2 those leaving cells to the right. 
  #The idea here is that individuals evely migrate to left and right cells
  arrivingX<-0.5*leavingX[left.cell]+ 0.5*leavingX[right.cell]
  arrivingP<-0.5*leavingP[left.cell]+ 0.5*leavingP[right.cell]

  #surplus production from the predator prey function
  
  surplus <- pred_prey(pars = pars_mpa) 
  
  ##negative surpluses don't really know why
  surplusX <- surplus[[1]]
  surplusP <- surplus[[2]]
  
  #catches = harvest rate in each cell times the population size 
  #basically how much are we catching at each time step
  
  catchesX <- harvest_mpa_hx*popX
  catchesP <- harvest_mpa_hp*popP
  
  
  #Now that we caught some fish and some migrated we update the population numbers
  popX <- popX + surplusX - catchesX - leavingX + arrivingX
  popP <- popP + surplusP - catchesP - leavingP + arrivingP
  
  popXsum<- sum(popX)
  popPsum <- sum(popP)

  ### can someone explain to me the logic behind these numbers going into the new run for it and not getting rewritten by the parameters we bring in at the start of this function?
  return(list(popXsum, popPsum)) 
  
  }

