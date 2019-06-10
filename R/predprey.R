#' Predator Prey Model 
#' Computes the change in both predator and prey abundance over time from species abundance and rates of ecological function
#' @param t time used for the ODE function
#' @param values initial population of predator and prey
#' @param pars starting parameters include initial predator and prey abundance (# indivduals), predator and prey carrying capacity (# indivduals), predator and prey growth rates (yˆ-1), predator and prey harvest rates (yˆ-1), nondynamic prey abundance (#indiviudals), predator mortality rate (yˆ-1), predation rate on focal prey and nondynamic prey (# individualsˆ-1*yˆ-1), and the predator converstion rate of prey (prey/predator)
#' @author Seleni Cruz and Juliette Verstaen
#' @return Prey and predator abundance at each time interval 

pred_prey <- function(t, values, pars){
  # Extract parameters
  rx <- pars[1]
  Kx <- pars[2]
  ax <- pars[3]
  hx <- pars[4]
  c <- pars[5]
  ay <- pars[6]
  dp <- pars[7]
  Kp <- pars[8]
  hp <- pars[9]
  Y <- 500
  # Extract state variables
  X <- values[1]
  P <- values[2]
  # Update equations
  dXdt <- (rx*X)*(1-(X/Kx))-(ax*P*X)-(hx*X)
  dPdt <- P*((c*((ax*X)+(ay*Y)))-dp)*(1-(P/Kp))-(hp*P)
  return(list(c(dXdt, dPdt)))
}