#' Wrapper Function
#'
#' Allows user to set all parameters and computes the number of individuals harvested at each time step
#' @param data starting parameters include initial predator and prey abundance (# indivduals), predator and prey carrying capacity (# indivduals), predator and prey growth rates (yˆ-1), predator and prey harvest rates (yˆ-1), nondynamic prey abundance (#indiviudals), predator mortality rate (yˆ-1), predation rate on focal prey and nondynamic prey (# individualsˆ-1*yˆ-1), and the predator converstion rate of prey (prey/predator)
#' @author Seleni Cruz and Juliette Verstaen
#' @return data frame with focal prey and predator abundance at each time interval specified and number of indivduals harvested 

wrapper_modelTEST <- 
  
  function(rx = 1.2, Kx = 100, ax = 0.03, hx = 0.65, hp = 0.32, c = 0.05, ay = 0.03, dp = 0.25, Kp = 25, t0 = 0, t1 = 40, X0, P0, mrate_X = 0.5, mrate_P = 0.5, ncells=10, MPA_width=4)
    
    {

   pars <- c(rx, 
             Kx,
             ax,
             hx,
             hp,
              c,
             ay,
             dp,
             Kp,
             mrate_X,
             mrate_P,
             ncells,
             MPA_width)
    
    
    ### Define time
    time <- seq(t0, t1, by = 0.1)
    values <- c(X0, P0) 
    
test <- lsoda(y = values, times = time, func = MPA_model, parms = pars) 
  as.data.frame() 
  #%>%
#  magrittr::set_colnames(value = c("Time", "X", "P")) %>% 
#  gather(Organism, Abundance, X, P) %>%
#  mutate(H = case_when(Organism == "X" ~ hx*Abundance, Organism == "P" ~ hp*Abundance))

 
      }