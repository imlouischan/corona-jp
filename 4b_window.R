## time window #############################################################
# input: incidence data (to select interventions during the period)
# input: interventions
# input: overlapping (Rt) or non-overlapping (Rn)
# output: time window
window <- function(incid, inter, overlap) {
  
  # 1st criterion
  t_first <- 1            # first case
  tau7 <- 7               # weekly time window
  t_tau <- t_first + tau7 # one time window (Day 8)
  
  # 2nd criterion
  mean_si <- 4.5                     # mean of serial inteval
  t_si <- t_first + ceiling(mean_si) # one generation (Day 6)
  
  # 3rd criterion
  t_cum <- which(cumsum(incid$E$I) >= 12)[1] # at least 12 cumulative cases
  
  # three criteria
  t_crit <- max(t_cum, t_tau, t_si)
  
  if(overlap) { # option A: weekly time window (overlapping)
    
    day <- nrow(incid$E) # length of duration
    
    # time window
    ( t_end <- t_crit:day )
    ( t_start <- t_end - tau7 + 1 )
    
  } else { # option B: non-overlapping time window
    
    # t1 <- incid$E$dates[1]       # Day 1
    t2 <- incid$E$dates[2]       # Day 2
    tn <- tail(incid$E$dates, 1) # last day
    tc <- incid$E$dates[t_crit]  # starting point by the above criteria
    
    t_inter <- unique(inter$dates)       # interventions
    # t_inter <- t_inter[t_inter > t2]     # interventions after Day 2
    t_inter <- t_inter[t_inter > tc] # interventions after the starting day
    t_inter <- t_inter[t_inter < tn]     # interventions before the last day
    
    # time window
    t_start <- c(t2, t_inter)   # from Day 2
    t_end   <- c(t_inter-1, tn) # to the last day
    ( t_start <- as.integer(t_start - t2 + 2) )
    ( t_end   <- as.integer(t_end   - t2 + 2) )
    
  }
  
  # output
  return(list(t_start = t_start, t_end = t_end))
  
}