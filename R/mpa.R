#' Marine protectes area simulation
#' Computes the change in both predator and prey abundance
#' @param t time used for the ODE function
#' @param values initial population of predator and prey
#' @param pars starting parameters include initial predator and prey abundance (# indivduals), predator and prey carrying capacity (# indivduals), predator and prey growth rates (yˆ-1), predator and prey harvest rates (yˆ-1), nondynamic prey abundance (#indiviudals), predator mortality rate (yˆ-1), predation rate on focal prey and nondynamic prey (# individualsˆ-1*yˆ-1), and the predator converstion rate of prey (prey/predator)
#' @author Seleni Cruz and Juliette Verstaen
#' @return Prey and predator abundance at each time interval 


MPA_sim <- function (hp, hx, X3, P3, t0){
  
  MPA<- foreach(mpa = seq(0,1, 0.1), .combine = rbind)%do%{
    hp_default <- 0.325 
    hx_default <- 0.65
    hp.mpa <- hp_default * (1-mpa)
    hx.mpa <- hp_default * (1-mpa)
    
    res <- run_model(hp=hp.mpa, hx=hx.mpa, t0=90, t1=200, X0=X3, P0=P3)
    
    results <- res%>% mutate(MPA=mpa)
  }
  return (MPA)
}