## reproduction number #########################################################
# input: incidence
# input: time window
# input: alternative method c("parametric_si", "uncertain_si")
# output: reproduction number
reproduction <- function(incid, t_window, method) {
  
  
  if(method == "parametric_si") { # estimate R using parametric_si
    
    R_si <- EpiEstim::estimate_R(incid$E,
                                 method = "parametric_si",
                                 config = EpiEstim::make_config(list(
                                   t_start = t_window$t_start,
                                   t_end = t_window$t_end,
                                   mean_si = 4.5,
                                   std_si = 4.5)))
    
  } else if(method == "uncertain_si") { # estimate R using uncertain_si
    
    R_si <- EpiEstim::estimate_R(incid$E,
                                 method = "uncertain_si",
                                 config = EpiEstim::make_config(list(
                                   t_start = t_window$t_start,
                                   t_end = t_window$t_end,
                                   mean_si = 4.5, std_mean_si = 0.5,
                                   min_mean_si = 2.5, max_mean_si = 7.5,
                                   std_si = 4.5, std_std_si = 0.5,
                                   min_std_si = 4, max_std_si = 5)))
    
  }
  
  # # reproduction number
  # R <- data.frame(t_start = R_si$R$t_start,
  #                 t_end   = R_si$R$t_end,
  #                 R_mean  = R_si$R$`Mean(R)`,
  #                 R_025   = R_si$R$`Quantile.0.025(R)`,
  #                 R_975   = R_si$R$`Quantile.0.975(R)`)
  
  # output
  return(R_si)
  
}