#' Predator Prey Lotka Voltera Interaction
#'
#' Computes the change in both predator and prey abundance over time from species abundance and rates of ecological function
#' @param data starting parameters include initial predator and prey abundance (# indivduals), predator and prey carrying capacity (# indivduals), predator and prey growth rates (yˆ-1), predator and prey harvest rates (yˆ-1), nondynamic prey abundance (#indiviudals), predator mortality rate (yˆ-1), predation rate on focal prey and nondynamic prey (# individualsˆ-1*yˆ-1), and the predator converstion rate of prey (prey/predator)
#' @author Seleni Cruz and Juliette Verstaen
#' @return list with focal prey and predator abundance at each time interval specified 

pred_prey <- function(time, values,pars_mpa){
  
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
  
  X <- values[1]
  P <- values[2]
  
  ### Equations
  dXdt_vec <- (rx*popX)*(1-popX/Kx)-(ax*popP)*(popX)-(hx*popX)
  dPdt_vec <- popP*((c*((ax*popX)+(ay*Y)))-dp)*(1-(popP/Kp))-(hp*popP)
  
  return(list(dXdt_vec, dPdt_vec))}


run_model <- function(rx = 1, Kx = 100, ax = 0.03, hx = 0.65,
                      c = 0.05, ay = 0.03, dp = 0.25, Kp = 25,
                      hp = 0.325, t0 = 0, t1 = 40, X0, P0){
  # Define parameters
  pars <- c(rx, Kx, ax, hx, c, ay, dp, Kp, hp)
  # Define values
  values <- c(X0, P0)
  # Define time
  time <- seq(t0, t1, by = 0.1)
  lsoda(y = values, times = time, func = pred_prey, parms = pars) %>%
    as.data.frame() %>%
    magrittr::set_colnames(value = c("Time", "X", "P")) %>%
    mutate(H_X = hx*X, H_P = hp*P)
}