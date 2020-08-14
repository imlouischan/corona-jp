## Richardson–Lucy deconvolution ###############################################
# input: target country
# input: initial condition: shifting / uniform
# input: incidence of confirmed cases (d) = data, convoluted assumption
# input: delay time from infection to confirmed (p)
# output: incidence of infection (u) = deconvoluted
deconvolution <- function(country, ic, incid, tau) {
  
  # time delay (p)
  p <- tau$EC
  pl <- length(p)
  
  # incidence of confirmed cases (d)
  incid_d <- incid$C
  d <- incid_d$I
  dl <- length(d)
  
  # incidence of infection (u)
  ul <- dl + (pl - 1)
  u <- rep(NA, ul)
  
  # output data frame
  if        ( country$selected == "hop" ) { # data from Johns Hopkins
    shiftdata <- 1 # suppose one day delay to Johns Hopkins
  } else if ( country$selected == "who" ) { # data from WHO
    shiftdata <- 2 # suppose two day delay to WHO
  } else if ( country$selected == "jap" ) { # data from Japan
    shiftdata <- 0 # suppose no time delay
  }
  incid_u <- data.frame(dates = as.Date(-(ul-1):0, origin = tail(incid_d$dates, 1)) - shiftdata,
                        I = u)
  
  # convolution matrix, P1
  P1 <- matrix(0, nrow = ul, ncol = ul)
  for ( ui in 1:ul ) {
    if ( ui <= dl ) {
      P1[ui:(ui+ul-dl), ui] <- p
    } else { # later points
      P1[ui:ul, ui] <- p[1:(ul-ui+1)]
    }
  }
  P1_RL <- P1[pl:ul, ]
  
  # option A: original approach
  # conservation of total cases
  q <- 1
  
  # option B: ref: 225_goldstein2009reconstructing
  # unobserved cases at the beginning and end
  q <- colSums(P1_RL)
  
  # option C: louis proposal
  # suppose no confirmed cases from (Day -34) to (Day 0)
  q_conv <- c(q[1:(pl-1)], rep(1, dl))
  
  # RL deconvolution
  r <- 30                                   # iterations
  d_RL <- matrix(NA, nrow = r+1, ncol = dl) # expected convolution
  u_RL <- matrix(NA, nrow = r+1, ncol = ul) # deconvoluted incidence
  
  if        ( ic == "shif" ) {
    # option A: initial condition using shifted incidence
    s <- which.max(p) - 1                                 # shift back by 10 days
    u_RL[1, ] <- c(rep(1, pl-1-s), d, rep(1, s))          # one case for other 35 days
    u_RL[1, ] <- c(rep(1, pl-1-s), d, rep(tail(d, 1), s)) # rep last day 10 times
  } else if ( ic == "unif" ) {
    # option B: uniform initial condition
    u_RL[1, ] <- rep(1, ul)
  }
  
  d_RL[1, ] <- P1_RL %*% ( u_RL[1, ] / q_conv ) # convolution for the initial condition
  
  for ( ri in 1:r ) { # RL iteration
    
    d_ratio <- d/d_RL[ri, ] # ratio of deconvoluted
    d_ratio[d == 0] <- 0    # avoid NaN
    u_RL[ri+1, ] <- u_RL[ri, ] / q * ( t(P1_RL) %*% d_ratio ) # deconvolution
    d_RL[ri+1, ] <- P1_RL %*% ( u_RL[ri+1, ] / q_conv )       # convolution (for next step)
    
  }
  
  # number of iterations to stop
  source("3b_deconv_iter.R")
  rc <- deconvolution_iter(r, incid_d, incid_u, d_RL, u_RL)
  
  # RL deconvoluted incidence of infection
  ( u <- u_RL[rc, ] )         # selected iteration step
  ( incidence_u <- round(u) ) # integer incidence
  incid_u$I <- incidence_u    # data frame aligning with dates
  
  # remove zeros at the head
  if( incid_u$I[1] == 0 )
    incid_u <- tail(incid_u, -(which(incid_u$I > 0)[1] - 1))
  
  # option A: remove zeros at the tail
  # if( rev(incid_u$I)[1] == 0 )
  #   incid_u <- head(incid_u, -(which(rev(incid_u$I) > 0)[1] - 1))
  
  # option B: remove latest period, for example 10 days as the shifted curve
  # incid_u <- head(incid_u, -10)
  
  # length of duration
  # ( ul <- nrow(incid_u) )
  
  # incidence and cumulative curve of infection
  incid$E   <- incid_u           # incidence
  incid$E$C <- cumsum(incid$E$I) # cumulative
  
  # two incidence curves of infection and confirmation
  # I.x is incidence of infection    (incidE)
  # I.y is incidence of confirmation (incidC)
  ( incid$EC <- merge(incid$E, incid$C, by = "dates", all = T) )
  
  # output
  return(incid)
  
}