## incubation period ###########################################################
# input: distribution of time from symptom onset to confirmation
# input: alternative distribution c("lnorm", "gamma", "weibull")
# output: adding distribution of incubation period
incubation <- function(tau, distr) {
  
  # time lag
  taui <- 0:14
  
  # three alternative distributions, ref: lauer2020incubation
  incubation_ddlnorm <-
    plnorm(taui+1, meanlog = 1.621, sdlog = 0.418) -
    plnorm(taui  , meanlog = 1.621, sdlog = 0.418)
  
  incubation_ddgamma <-
    pgamma(taui+1, shape = 5.807, scale = 0.948) -
    pgamma(taui  , shape = 5.807, scale = 0.948)
  
  incubation_ddweibull <-
    pweibull(taui+1, shape = 2.453, scale = 6.258) -
    pweibull(taui  , shape = 2.453, scale = 6.258)
  
  # take one
  if( distr == "lnorm" )
    tauES <- incubation_ddlnorm
  
  if( distr == "gamma" )
    tauES <- incubation_ddgamma
  
  if( distr == "weibull" )
    tauES <- incubation_ddweibull
  
  # normalization
  tauES <- tauES / sum(tauES)
  
  # time lag
  tau$ES <- tauES
  
  # output
  return(tau)
  
}