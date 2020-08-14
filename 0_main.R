## new script ##################################################################
cat("\014") # clear the console
rm(list=ls()) # remove all variables
graphics.off() # close all plots
set.seed(2020) # fix randomness
Sys.setenv(TZ = "Japan"); ( timer0 <- Sys.time() ) # start timer
## time delay ##################################################################
source("1c_incidence_jp.R") # detection time from Japan
detec <- incidence(online = F, detection = T)

source("2a_detection.R")    # detection time (symptomatic-to-confirmation)
tau <- detection(detec)

source("2b_incubation.R")   # incubation period (infection-to-symptomatic)
tau <- incubation(tau, distr = "gamma")

source("2c_delay.R")        # time delay (infection-to-confirmation)
tau <- delay(tau)
source("2c_delay_plot.R")
delay_plot(tau)

source("1_countrynames.R")  # country names
countries <- countrynames()

for( ic in c("shif", "unif") ) { # deconvolution initial condition
  for( i in 1:50 ) { # 50 countries, i
    
    ## incidence #################################################################
    country <- countries[i, ] # target
    if        ( country$selected == "hop" ) {
      source("1a_incidence_hopkins.R") # incidence from Johns Hopkins
      incid <- incidence(online = F, country)
    } else if ( country$selected == "who" ) {
      source("1b_incidence_who.R")     # incidence from WHO
      incid <- incidence(country)
    } else if ( country$selected == "jap" ) {
      source("1c_incidence_jp.R")      # incidence from Japan
      incid <- incidence(online = F, detection = F)
    }
    
    ## Richardson–Lucy deconvolution #############################################
    source("3a_deconv.R") # Richardson–Lucy deconvolution
    incid <- deconvolution(country, ic, incid, tau)
    source("3a_deconv_plot.R")
    fig <- deconvolution_plot(incid)
    
    ## reproduction number #######################################################
    source("4a_intervention.R") # interventions
    inter <- interventions(online = F, country = country, national = T)
    
    source("4b_window.R")       # time window
    Rn_window <- window(incid, inter, overlap = F)
    Rt_window <- window(incid, inter, overlap = T)
    
    source("4c_Rt.R")           # reproduction number
    Rn <- reproduction(incid, Rn_window, method = "parametric_si")
    Rt <- reproduction(incid, Rt_window, method = "parametric_si")
    source("4c_Rt_plot.R")
    fig <- reproduction_plot(fig, incid, Rn, Rt)
    
    # effectiveness ##############################################################
    source("5a_effectiveness.R")   # effectiveness of implementation
    eta <- effectiveness(Rn)
    source("5a_effectiveness_plot.R")
    fig <- effectiveness_plot(fig, incid, eta)
    
    source("5b_measures.R")        # effectiveness of measure
    inter <- measures(eta, inter)
    source("5b_measures_plot.R")
    fig <- measures_plot(fig, incid, inter)
    
    source("8_four_plot.R")        # four selected countries
    if(!exists("f")) f <- list(list())
    f <- four_plot(country, fig, f)
    
    source("5c_measures_matrix.R") # effectiveness in matrix form
    if(!exists("log10etamn")) log10etamn <- list()
    log10etamn <- measures_matrix(log10etamn, inter)
    
  } # 50 countries, i
  
  ( filename <- paste0("log10etamn_", i, "_selected", "_", ic, ".Rdata") ) # save 50 matrix as Rdata
  save(log10etamn, file = filename)
  
  source("5d_theme_plot.R") # effectiveness of measure theme
  fig <- theme_plot(log10etamn)
  
} # deconvolution initial condition, ic
################################################################################
source("6_pair.R")
source("7a_pareto.R")
source("7b_pareto.R")
source("9_map.R")
################################################################################
( timer1 <- Sys.time() ); print( timer01 <- timer1 - timer0 ) # stop timer