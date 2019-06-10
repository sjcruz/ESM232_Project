#' Function runs sensitivity analysis for scenrio 1: Predator recovery first and then prey 
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


scen1_sensitivity <- function (hx, hp, c, rx, dp, X0=30, P0=50, ax, ay, Kx=100, Kp=25, Y=500){
  #Start simulation and runt o equlibrium 
  start <- run_model(hx = 0, hp = 0, t1 = 30, X0 = X0, P0 = P0)
  
  X1 <- tail(start$X, 1)
  P1 <- tail(start$P, 1)
  #Harvest predator: trophic downgrading 
  harvest_P <- run_model(hx = 0, t0 = 30, t1 = 60, X0 = X1, P0 = P1)
  
  X2 <- tail(harvest_P$X, 1)
  P2 <- tail(harvest_P$P, 1)
  #Harvest prey: trophic downgrading 
  harvest_X <- run_model(t0 = 60, t1 = 90, X0 = X2, P0 = P2)
  
  X3 <- tail(harvest_X$X, 1)
  P3 <- tail(harvest_X$P, 1)
  #Stop harvesting predator first 
  stop_harvest_P <- run_model(hp = 0, t0 = 90, t1 = 120, X0 = X3, P0 = P3)
  
  X4 <- tail(stop_harvest_P$X, 1)
  P4 <- tail(stop_harvest_P$P, 1)
  #Stop harvesting prey after 
  stop_harvest_X <- run_model(hx = 0, hp = 0, t0 = 120, t1 = 200, X0 = X4, P0 = P4)
  
  X5 <- tail(stop_harvest_X$X, 1)
  P5 <- tail(stop_harvest_X$P, 1)
  
  sens <- rbind(start, harvest_P, harvest_X, stop_harvest_P, stop_harvest_X)%>%
    select(X, P)%>%
    summarise_all(funs(sum))
}
