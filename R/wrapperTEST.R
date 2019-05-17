#' Wrapper Function
#'
#' Allows user to set all parameters and computes the number of individuals harvested at each time step
#' @param data starting parameters include initial predator and prey abundance (# indivduals), predator and prey carrying capacity (# indivduals), predator and prey growth rates (yˆ-1), predator and prey harvest rates (yˆ-1), nondynamic prey abundance (#indiviudals), predator mortality rate (yˆ-1), predation rate on focal prey and nondynamic prey (# individualsˆ-1*yˆ-1), and the predator converstion rate of prey (prey/predator)
#' @author Seleni Cruz and Juliette Verstaen
#' @return data frame with focal prey and predator abundance at each time interval specified and number of indivduals harvested 

wrapper_model <- 
  
  function(rx = 1, Kx = 100, ax = 0.03, hx, c = 0.05, ay = 0.03, dp = 0.25, Kp = 25, hp,   t0 = 0, t1 = 40, X0, P0)
    
    { pars <- c(rx, Kx, ax, c, ay, dp, Kp)
    
    ### Call to MPA function to determine harvest rates for prey and predator
    
    
    
    
    ### Define values
    values <- c(X0, P0) 
    ### Define time
    time <- seq(t0, t1, by = 0.1)
    

lsoda(y = values, times = time, func = pred_prey, parms = pars) %>% 
  as.data.frame() %>%
  magrittr::set_colnames(value = c("Time", "X", "P")) %>% 
  gather(Organism, Abundance, X, P) %>%
  mutate(H = case_when(Organism == "X" ~ hx*Abundance, Organism == "P" ~ hp*Abundance))
      
      }