#' Wrapper Function- uses predator prey model and ODE function 
#' Allows user to set all parameters and computes the number of individuals harvested at each time step
#' @param rx prey growth rate (yearly rate)
#' @param Kx prey carrying capacity (# of individuals)
#' @param ax predation rate on focal prey (# individualsˆ-1*yˆ-1)   
#' @param hx focal prey harvest rate (yearly rate) 
#' @param c predator conversion rate of prey (prey/predator)
#' @param ay predation rate on prey (Y, # individualsˆ-1*yˆ-1) 
#' @param dp predator mortality rate (yearly rate)
#' @param Kp Predator carrying capcity (# of individuals) 
#' @param hp predator harvest rate (yearly rate) 
#' @param t0 start time 
#' @param t1 end time 
#' @param X0 initial prey population (# of individuals) 
#' @param P0 initial predator population (# of individuals) 
#' @author Seleni Cruz and Juliette Verstaen
#' @references Samhouri, Jameal F., Adrian C. Stier, Shannon M. Hennessey, Mark Novak, Benjamin S. Halpern, and Phillip S. Levin. 2017. “Rapid and Direct Recoveries of Predators and Prey Through Synchronized Ecosystem Management.” Nature Ecology & Evolution 1 (4): 0068. doi:10.1038/s41559-016-0068.
#' @return data frame with focal prey and predator abundance at each time interval specified and number of indivduals harvested 

run_model <- function(rx = 1, Kx = 100, ax = 0.03, hx = 0.65,
                      c = 0.05, ay = 0.03, dp = 0.25, Kp = 25,
                      hp = 0.325, t0 = 0, t1 = 40, X0, P0){
  # Define parameters
  pars <- c(rx, Kx, ax, hx, c, ay, dp, Kp, hp)
  # Define values
  values <- c(X0, P0)
  # Define time
  time <- seq(t0, t1, by = 0.1)
  lsoda(y = values, times = time, func = pred_prey, parms = pars)%>%
    as.data.frame()%>%
    magrittr::set_colnames(value = c("Time", "X", "P"))%>%
    mutate(H_X= X*hx, H_P= P*hp)
}



