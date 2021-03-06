#' Predator Prey Lotka Voltera Interaction
#'
#' Computes the change in both predator and prey abundance over time from species abundance and rates of ecological function
#' @param data starting parameters include initial predator and prey abundance (# indivduals), predator and prey carrying capacity (# indivduals), predator and prey growth rates (yˆ-1), predator and prey harvest rates (yˆ-1), nondynamic prey abundance (#indiviudals), predator mortality rate (yˆ-1), predation rate on focal prey and nondynamic prey (# individualsˆ-1*yˆ-1), and the predator converstion rate of prey (prey/predator)
#' @author Seleni Cruz and Juliette Verstaen
#' @return list with focal prey and predator abundance at each time interval specified 

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


scen1_sensitivity <- function (hx, hp, c, rx, dp, X0, P0, ax, ay, Kx, Kp, Y){
  start <- run_model(hx = 0, hp = 0, t1 = 30, X0 = X0, P0 = P0)
  
  X1 <- tail(start$X, 1)
  P1 <- tail(start$P, 1)
  harvest_P <- run_model(hx = 0, t0 = 30, t1 = 70, X0 = X1, P0 = P1)
  
  X2 <- tail(harvest_P$X, 1)
  P2 <- tail(harvest_P$P, 1)
  harvest_X <- run_model(t0 = 70, t1 = 120, X0 = X2, P0 = P2)
  
  X3 <- tail(harvest_X$X, 1)
  P3 <- tail(harvest_X$P, 1)
  
  stop_harvest_P <- run_model(hp = 0, t0 = 120, t1 = 140, X0 = X3, P0 = P3)
  
  X4 <- tail(stop_harvest_P$X, 1)
  P4 <- tail(stop_harvest_P$P, 1)
  
  stop_harvest_X <- run_model(hx = 0, hp = 0, t0 = 140, t1 = 200, X0 = X4, P0 = P4)
  
  X5 <- tail(stop_harvest_X$X, 1)
  P5 <- tail(stop_harvest_X$P, 1)
  
  sens <- rbind(start, harvest_P, harvest_X, stop_harvest_P, stop_harvest_X)%>%
    select(X, P)%>%
    summarise_all(funs(sum))
}

##########################################
#NOT USING 



harvest <- function(hx=hx, ncells=ncells, MPA_width=MPA_width, hp=hp)  {
  harvest_hx <- c(rep(hx, length = ncells))
  harvest_hx[sample(ncells, MPA_width, replace=FALSE)] <- 0  
  harvest_hp <- harvest_hx
  harvest_hp[harvest_hp == hx] <- hp
  
  return(list(hx=harvest_hx, hp=harvest_hp))
}

MPA_model <- function(rx = 1, Kx = 100, ax = 0.03, hx=0.65,
                      c = 0.05, ay = 0.03, dp = 0.25, Kp = 25,
                      hp = 0.325, t0= 0, t1 = 40, X, P, ncells=10, MPA_width){
  
  #setting up modeling space (vectors)
  left.cell<- c(ncells, 1: (ncells-1))
  right.cell<- c(2: ncells, 1)
  Kx <- Kx/ncells
  Kp <- Kp/ncells
  X <- c(rep(X/ncells, length = ncells))
  P <- c(rep(P/ncells, length = ncells))
  Y <- 500/ncells
  ax <- ax/ncells
  ay <- ay/ncells
  
  #putting MPAs in place 
  out <- harvest(hx=.65, ncells=10, MPA_width=0, hp=.32)
  hp <- out$hp
  hx <- out$hx
  mrate <- 0.5
  results <- data.frame (Time=NA, X =NA, P =NA, H_X=NA, H_P=NA)
  
  time <- seq(t0, t1, by = 0.1)
  for (i in time) {

    leavingX <- mrate*X
    leavingP <- mrate*P
    
    arrivingX <- 0.5*leavingX[left.cell]+ 0.5*leavingX[right.cell]
    arrivingP <- 0.5*leavingP[left.cell]+ 0.5*leavingP[right.cell]

    X <- X + (rx*X)*(1-(X/Kx))-(ax*P*X)-(hx*X) - leavingX + arrivingX
    P <- P + P*(c*(ax*X+ay*Y)-dp)*(1-(P/Kp))-(hp*P)- leavingP + arrivingP

    harX <- hx*X
    harP <- hp*P
      
    X_pop <- sum(X)
    P_pop <- sum(P) 
    
    out <- cbind(Time= i, X=X_pop, P=P_pop, H_X= sum(harX), H_P= sum(harP))%>%as.data.frame()
    
    results <- rbind(results, out)
    }
  
  return(results[-1,])
  
}