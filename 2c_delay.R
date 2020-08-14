## convolution of two distributions ############################################
# input: two distributions: infection-to-symptomatic and symptomatic-to-confirmation
# output: convoluted distribution of time delay from infection to confirmation
delay <- function(tau) {
  
  # time from infection to symptomic / incubation period (p)
  p <- tau$ES
  pl <- length(p)
  # time from symptomic to confirmed (u)
  u <- tau$SC
  ul <- length(u)
  # time from infection to confirmed (d)
  dl <- ul+pl-1
  
  # convolution matrix, P
  P <- matrix(0, nrow = dl, ncol = dl)
  for ( di in 1:dl ) {
    if ( di <= ul ) {
      P[di:(di+dl-ul), di] <- p
    } else { # later points
      P[di:dl, di] <- p[1:(dl-di+1)]
    }
  }
  
  U <- c(u, rep(0, pl-1)) # extend for convolution
  D <- P %*% U # convolution
  d <- as.vector(D)
  
  # time from infection to confirmed
  tauEC <- d
  
  # normalization
  tauEC <- tauEC / sum(tauEC)
  
  # mean & SD
  ( mean <- sum( tauEC * ((1:dl)-1) ) )                  # 11.55912
  ( sd   <- sqrt(sum( tauEC * ((1:dl)-1)^2 ) - mean^2) ) # 4.636569
  
  # time lag
  tau$EC <- tauEC
  
  # output
  return(tau)
  
}