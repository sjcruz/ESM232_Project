#' Function creates modelling space in the form of 10 vectors 
#' @param values initial predator (P) and prey (X) abundances  
#' @param pars predator harvest rate (hp), 
#' 
#' 
#' @author Seleni Cruz and Juliette Verstaen
#' @return list with focal prey and predator abundance at each time interval specified 

harvest <- function(hx=hx, ncells=ncells, MPA_width=MPA_width, hp=hp)  {
  harvest_hx <- c(rep(hx, length = ncells))
  harvest_hx[sample(ncells, MPA_width)] <- 0  
  harvest_hp <- harvest_hx
  harvest_hp[harvest_hp == hx] <- hp
  
  # create left and right cells that the fish and move into 
  left.cell<- c(ncells, 1: (ncells-1))
  right.cell<- c(2: ncells, 1)

  return(list(hx=harvest_hx, hp=harvest_hp,l.cell=left.cell, r.cell=right.cell))
  }
  

pred_prey <- function(X, P, pars, i, t){
  # Extract parameters
  rx <- pars$rx
  Kx <- pars$Kx/pars$ncells
  ax <- pars$ax
  hx <- pars$hx
  c <- pars$c
  ay <- pars$ay
  dp <- pars$dp
  Kp <- pars$Kp/pars$ncells
  hp <- pars$hp
  Y <- 500/pars$ncells
  mrate <- 0.5
  # Update equations
 
  leavingX <- 2*mrate*X
  leavingP <- 2*mrate*P
  arrivingX <- 0.5*leavingX[pars$l.cell]+ 0.5*leavingX[pars$r.cell]
  arrivingP <- 0.5*leavingP[pars$l.cell]+ 0.5*leavingP[pars$r.cell]
  
  
  Xpop <- X + ((rx*X)*(1-(X/Kx))-(ax*P*X)-(hx*X))+ arrivingX - leavingX
  Ppop <- P + (P*((c*((ax*X)+(ay*Y)))-dp)*(1-(P/Kp))-(hp*P)) + arrivingP -leavingP

  har_X <- sum(hx*X)
  har_P <- sum(hp*P)
  
  summary <- cbind(X= sum(Xpop), P=sum(Ppop), H_X = har_X, H_P=har_P)
  
  if (i == t){
    return(list(summary=summary, Xpop=Xpop, Ppop=Ppop))
  }else{
  return(list(summary=summary))
}}


run_model <- function(rx = 1, Kx = 100, ax = 0.03, hx = 0.65,
                      c = 0.05, ay = 0.03, dp = 0.25, Kp = 25,
                      hp = 0.325, t0= 0, t = 40, X, P, ncells=10, MPA_width){
  
  out <- harvest(hx=hx, ncells=ncells, MPA_width=MPA_width, hp=hp)
 
  pars <- list(rx=rx, Kx=Kx, ax=ax, hx=out$hx, c=c, ay=ay, dp=dp, Kp=Kp, 
       hp=out$hp, ncells=ncells, MPA_width=MPA_width, l.cell=out$l.cell, r.cell=out$r.cell)
  
  results <- data.frame (X=NA, P=NA, H_X=NA, H_P=NA)
  
  for (i in c(t0:t)){
    pop <- pred_prey(X=X, P=P, pars=pars, i=i, t=t)
    
    results <- rbind(results, pop$summary)
    
  }
  
  return(list(results[-1,], pop))
}
  
  

