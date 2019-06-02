#' Wrapper Function
#'
#' Allows user to set all parameters and computes the number of individuals harvested at each time step
#' @param data starting parameters include initial predator and prey abundance (# indivduals), predator and prey carrying capacity (# indivduals), predator and prey growth rates (yˆ-1), predator and prey harvest rates (yˆ-1), nondynamic prey abundance (#indiviudals), predator mortality rate (yˆ-1), predation rate on focal prey and nondynamic prey (# individualsˆ-1*yˆ-1), and the predator converstion rate of prey (prey/predator)
#' @author Seleni Cruz and Juliette Verstaen
#' @return data frame with focal prey and predator abundance at each time interval specified and number of indivduals harvested 

wrapper_modelTEST <- 
  
  function(mrate_X = 0.5, mrate_P = 0.5, ncells=10, MPA_width=0, hx = 0.65, hp = 0.32, rx = 1.2, Kx = 100, ax = 0.03,  c = 0.05, ay = 0.03, dp = 0.25, Kp = 25, t0 = 0, t1 = 40, X0, P0) {
  
  pars <- c(mrate_X, mrate_P, ncells, MPA_width, hx, hp, rx, Kx, ax, c, ay, dp, Kp)
   
  ### Define values  
  values <- c(X0, P0) 
    
  ### Define time
  time <- seq(t0, t1, by = 0.1)

  ### Run the MPA set up model and define initial variables
  #######RENAME THIS MODEL AND GET RID OF MODEL SET UP
  mpa_var_setup <- mpa_var_setup(pars=pars)
  harvest_mpa_hx <- mpa_var_setup[1]
  harvest_mpa_hp <- mpa_var_setup[2]
  popX <- mpa_var_setup[3]
  popP <- mpa_var_setup[4]
  left.cell <- mpa_var_setup[5]
  right.cell <- mpa_var_setup[6]
  
  # Run ode function      
  test <- lsoda(y = values, times = time, func = run_mpa, parms = pars) 
  as.data.frame() 
  
  #%>%
#  magrittr::set_colnames(value = c("Time", "X", "P")) %>% 
#  gather(Organism, Abundance, X, P) %>%
#  mutate(H = case_when(Organism == "X" ~ hx*Abundance, Organism == "P" ~ hp*Abundance))

 
      }