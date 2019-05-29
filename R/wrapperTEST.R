#' Wrapper Function
#'
#' Allows user to set all parameters and computes the number of individuals harvested at each time step
#' @param data starting parameters include initial predator and prey abundance (# indivduals), predator and prey carrying capacity (# indivduals), predator and prey growth rates (yˆ-1), predator and prey harvest rates (yˆ-1), nondynamic prey abundance (#indiviudals), predator mortality rate (yˆ-1), predation rate on focal prey and nondynamic prey (# individualsˆ-1*yˆ-1), and the predator converstion rate of prey (prey/predator)
#' @author Seleni Cruz and Juliette Verstaen
#' @return data frame with focal prey and predator abundance at each time interval specified and number of indivduals harvested 

wrapper_model <- 
  
  function(rx = 1, Kx = 100, ax = 0.03, hx, c = 0.05, ay = 0.03, dp = 0.25, Kp = 25, hp, t0 = 0, t1 = 40, X0, P0, mrateX, mrateP)
    
    {

###########
        ### Call to MPA function to determine population sizes for each cell for prey and predator
    
      
      #loop through the time steps (nsteps or # of years)
      for (i in 1:nsteps) {
        #Vector "leaving" is number of individuals leaving each cell 
        #Number leaving each cell is 2*movement rate*pop size
        #since i used vector operation there is no need to loop 
        leaving<-2*mrate*pop
        
        #The number of immigrants is 1/2 those leaving cells to the left and 1/2 those leaving cells to the right. 
        #The idea here is that individuals evely migrate to left and right cells
        arriving<-0.5*leaving[left.cell]+ 0.5*leaving[right.cell]
        
        #surplus production from the logistic model
        #surplus<- r*pop *(1-pop/K)
        pred prey model
        
        
        #catches = harvest rate in each cell times the population size 
        #basically how much are we catching at each time step
        catches<- u.vec*pop
        
        
        #Now that we caught some fish and some migrated we update the population numbers
        pop<-pop+surplus-catches- leaving+ arriving  
      
##########      
      
    pars <- c(rx, Kx, ax, hx,c, ay, dp,hp, Kp)

    
    ### Define values
    values <- c(X0, P0) 
    ### Define time
    time <- seq(t0, t1, by = 0.1)
    

lsoda(y = values, times = time, func = MPAMODEL??, parms = pars) %>% 
  as.data.frame() %>%
  magrittr::set_colnames(value = c("Time", "X", "P")) %>% 
  gather(Organism, Abundance, X, P) %>%
  mutate(H = case_when(Organism == "X" ~ hx*Abundance, Organism == "P" ~ hp*Abundance))
      
      }