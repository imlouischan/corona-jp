## detection time from symptomatic to positive test ############################
# input: data of detection time from Japan
# output: distribution of time from symptom onset to confirmation
detection <- function(detec) {
  
  # date of confirmed
  confirmed <- detec$C
  # date of symptomatic
  symptomatic <- detec$S
  # time lag
  taui <- as.numeric(confirmed - symptomatic)
  taui <- taui[!is.na(taui)] # remove NA
  taui <- taui[taui >= 0] # remove negative
  taui <- taui[taui <= 30] # remove outliers
  
  # PMF, following gamma distribution by default
  pmf_ddgamma <- function(tau, theta){
    pgamma(tau+1, shape = theta[1], scale = theta[2]) -
      pgamma(tau, shape = theta[1], scale = theta[2]) }
  
  # negative log-likelihood
  nlogL <- function(theta) -sum(log(pmf_ddgamma(taui, theta)))
  
  # parameter estimation using optim
  theta_optim <- suppressWarnings( optim(c(1, 1), nlogL) )
  ( nlogL_mle <- theta_optim$value ) # 24527.6
  ( theta_mle <- theta_optim$par ) # 2.82105 2.54680
  
  # mean and sd of gamma distribution
  ( mean <- theta_mle[1]*theta_mle[2] ) # 7.184649
  ( sd <- sqrt(theta_mle[1]*theta_mle[2]^2) ) # 4.2776
  
  # time lag
  tau <- 0:21
  
  # time from symptomic to confirmed
  tauSC <- pmf_ddgamma(tau, theta_mle)
  
  # normalization
  tauSC <- tauSC / sum(tauSC)
  
  # time lag
  tau <- list(SC = tauSC)
  
  # output
  return(tau)
  
}