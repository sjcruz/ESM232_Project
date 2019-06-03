#' Predator Prey Lotka Voltera Interaction
#'
#' Computes the change in both predator and prey abundance over time from species abundance and rates of ecological function
#' @param data starting parameters include initial predator and prey abundance (# indivduals), predator and prey carrying capacity (# indivduals), predator and prey growth rates (yˆ-1), predator and prey harvest rates (yˆ-1), nondynamic prey abundance (#indiviudals), predator mortality rate (yˆ-1), predation rate on focal prey and nondynamic prey (# individualsˆ-1*yˆ-1), and the predator converstion rate of prey (prey/predator)
#' @author Seleni Cruz and Juliette Verstaen
#' @return list with focal prey and predator abundance at each time interval specified 

pred_prey <- function(time, values,pars_mpa){
  
  ncells <- pars_mpa[[18]]
  harvest_mpa_hx <- pars_mpa[[3]]
  harvest_mpa_hp <- pars_mpa[[4]]
  popX <- pars_mpa[[5]]
  popP <- pars_mpa[[6]]
  rx <- pars_mpa[[9]]
  Kx <- pars_mpa[[10]]/ncells
  ax <- pars_mpa[[11]]
  c <- pars_mpa[[12]]
  ay <- pars_mpa[[13]]
  dp <- pars_mpa[[14]]
  Kp <- pars_mpa[[15]]/ncells
  hx <- pars_mpa[[16]]
  hp <- pars_mpa[[17]]
  Y <- 500/ncells
  
  ### Equations
  dXdt_vec <- (rx*popX)*(1-popX/Kx)-(ax*popP)*(popX)-(hx*popX)
  dPdt_vec <- popP*((c*((ax*popX)+(ay*Y)))-dp)*(1-(popP/Kp))-(hp*popP)
  
  return(list(dXdt_vec, dPdt_vec)) }




