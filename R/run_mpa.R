

spatial_P_X <- function(time, mrate_X = 0.5, mrate_P = 0.5, ncells=10, MPA_width, rx = 1.2, Kx = 100, ax = 0.03,  c = 0.05, 
                        ay = 0.03, dp = 0.25, Kp = 25, variables) 
    {
  harvest_mpa_hx <- variables[[1]]
  harvest_mpa_hp <- variables[[2]]
  popX <- variables[[3]]
  popP <- variables[[4]]
  left.cell <- variables[[5]]
  right.cell <- variables[[6]]
  Kx <- Kx/ncells
  Kp <- Kp/ncells
  
  Y <- 500/ncells
  
  results<- data.frame(popXsum=NA, popPsum=NA, Xharsum=NA, Pharsum=NA)

  for(t in c(1:time)) {
  leavingX <- mrate_X*popX
  leavingP <- mrate_P*popP
  
  #The number of immigrants is 1/2 those leaving cells to the left and 1/2 those leaving cells to the right. 
  #The idea here is that individuals evely migrate to left and right cells
  arrivingX <-0.5*leavingX[left.cell]+ 0.5*leavingX[right.cell]
  arrivingP <-0.5*leavingP[left.cell]+ 0.5*leavingP[right.cell]

  growthX <- (rx*popX)*(1-(popX/Kx))-(ax*popP)
  growthP <- popP*((c*((ax*popX)+(ay*Y)))-dp)*(1-(popP/Kp))
  
  #catches = harvest rate in each cell times the population size 
  #basically how much are we catching at each time step
  
  harvestX <- popX*harvest_mpa_hx
  harvestP <- popP*harvest_mpa_hp
  
  #Now that we caught some fish and some migrated we update the population numbers
  popX <- popX + growthX - harvestX - leavingX + arrivingX
  popP <- popP + growthP - harvestP - leavingP + arrivingP
  
  popXsum<- sum(popX)
  popPsum <- sum(popP)
  
  Xharsum <- sum(harvestX)
  Pharsum <- sum(harvestP)
  
  res <- cbind(popXsum, popPsum, Xharsum, Pharsum)
  results <- rbind(results, res)
  }
  return(results) 
  }

