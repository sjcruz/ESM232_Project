#' Predator Prey Lotka Voltera Interaction
#'
#' Computes the change in both predator and prey abundance over time from species abundance and rates of ecological function
#' @param data starting parameters include initial predator and prey abundance (# indivduals), predator and prey carrying capacity (# indivduals), predator and prey growth rates (yˆ-1), predator and prey harvest rates (yˆ-1), nondynamic prey abundance (#indiviudals), predator mortality rate (yˆ-1), predation rate on focal prey and nondynamic prey (# individualsˆ-1*yˆ-1), and the predator converstion rate of prey (prey/predator)
#' @author Seleni Cruz and Juliette Verstaen
#' @return list with focal prey and predator abundance at each time interval specified 

pred_prey <- function(values, pars){
  
  #  mrate_X <- pars[1]
  # mrate_P <- pars[2]
 # ncells <- pars[3]
  #MPA_width <- pars[4]
  hx <- pars[5]
  hp <- pars[6]
  rx <- pars[7]
  Kx <- pars[8]/ncells
  ax <- pars[9]
  c <- pars[10]
  ay <- pars[11]
  dp <- pars[12]
  Kp <- pars[13]/ncells
  Y <- 500/ncells
  
  ### Equations
  dXdt_vec <- (rx*popX)*(1-popX/Kx)-(ax*popP)*(popX)-(hx*popX)
  dPdt_vec <- P*((c*((ax*popX)+(ay*Y)))-dp)*(1-(popP/Kp))-(hp*popP)
  
  return(list(c(dXdt_vec, dPdt_vec))) }




