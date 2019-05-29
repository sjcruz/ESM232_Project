

MPA_model <- function(rx = 1, Kx = 100, ax = 0.03, hx = 0.65, c = 0.05, ay = 0.03, dp = 0.25, Kp = 25, hp = 0.32, t0 = 0, t1 = 40, X1, P1, mrate_X = 0.3, mrate_P = 0.5, ncells=10, MPA_width=4)
  {
  
# note, the calc harvest function returns both x and p numbers in one list, need to extract to two
  harvest_all <- calc_harvest(hx=hx, hp=hp, ncells=ncells, MPA_width=MPA_width)
  
  harvest_vec_X <-  harvest_all[1] %>%
    as_vector()
  harvest_vec_P <-  harvest_all[2] %>%
    as_vector()
  
# Population in each cell, empty cells place holder for later calcualtions 
  popX <- vector(length=ncells) 
  popP <- vector(length=ncells) 
  
# set starting population in vector pop in each cell 
# equal to the equilibrium abundance from running the first time
  popX[]<-X1
  popP[]<-P1

#create left and right cells that the fish and move into (will be looped over time in the wrapper function)  
  left.cell<- c(ncells, 1: (ncells-1))
  right.cell<- c(2: ncells, 1)
    
  leavingX<-2*mrate_X*popX
  leavingP<-2*mrate_P*popP
  
  
  #The number of immigrants is 1/2 those leaving cells to the left and 1/2 those leaving cells to the right. 
  #The idea here is that individuals evely migrate to left and right cells
  arrivingX<-0.5*leavingX[left.cell]+ 0.5*leavingX[right.cell]
  arrivingP<-0.5*leavingP[left.cell]+ 0.5*leavingP[right.cell]

  ######################################
  ######################################  
  #surplus production from the predator prey function
somehow here bring in the last numbers from the p and x populations which will loop somewhere, probably the wrapper
  surplus X
  surplus P
  
  #catches = harvest rate in each cell times the population size 
  #basically how much are we catching at each time step
  catches<- u.vec*pop
  
  
  #Now that we caught some fish and some migrated we update the population numbers
  pop<-pop+surplus-catches- leaving+ arriving
  
  
  
  }

