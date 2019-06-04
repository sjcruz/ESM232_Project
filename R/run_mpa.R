

run_model <- function(rx = 1, Kx = 100, ax = 0.03, hx = 0.65,
                        c = 0.05, ay = 0.03, dp = 0.25, Kp = 25,
                        hp = 0.325, time = 40, X, P, ncells=10, MPA_width) {
  out <- vectors_fun(hx=hx, ncells=ncells, MPA_width=MPA_width, hp=hp, X=X, P=P)
  
  values <- list(X=out$X, P=out$P)
  
  pars <- list(rx=rx, Kx=Kx, ax=ax, hx=out$hx, c=c, ay=ay, dp=dp, Kp=Kp, 
               hp=out$hp, ncells=ncells, MPA_width=MPA_width, l.cell=out$l.cell, r.cell=out$r.cell)
  
  results <- data.frame (X=NA, P=NA, H_X=NA, H_P=NA)

  for(t in c(1:time)) {
  pop <- pred_prey(values = values, pars=pars)%>%data.frame
    
  result <- rbind(results, pop)
  }
  return(results) 
  }

