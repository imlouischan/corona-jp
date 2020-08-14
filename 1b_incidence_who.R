## (1) incidence from WHO ######################################################
# input: target country
# output: daily incidence of confirmed cases
incidence <- function(country) {
  
  # call data, from WHO
  confirmed_who <- read.csv(paste0(
    "data/",
    "confirmed_who-2020-06-07.csv"),
    check.names = F)
  
  ## incidence #################################################################
  
  ( confirmed <- subset(confirmed_who, Country == country$name_who) ) # data of the target country
  
  ( cumulative <- as.numeric(confirmed[, -(1:2)]) ) # cumulative of confirmed cases
  cumulative0 <- c(0, cumulative)                   # add a zero at the beginning
  cumulative0[is.na(cumulative0)] <- 0              # replace NA by zero
  
  ( incidence <- diff(cumulative0) )                    # incidence of confirmed cases
  ( dates <- as.Date(colnames(confirmed)[-(1:2)]) )     # date from 25-Jan-2020
  
  # incidence of confirmed cases
  ( incid <- data.frame(dates = dates, I = incidence) )
  # cumulative
  incid$C <- cumsum(incid$I)
  
  # cleaning ###################################################################
  
  # remove zeros at the beginning
  if( incid$I[1] == 0 )
    incid <- incid[-(1:(which(incid$I > 0)[1] - 1)), ]
  
  # length of duration
  ( day <- nrow(incid) )
  
  ## correction on negative incidence ########################################
  
  if( sum(incid$I < 0) ) {
    
    incid$nI <- incid$I # negative incidence
    incid$nC <- incid$C # raw data, wrong cumulative
    
    while( sum(incid$I < 0) ) { # while existing negative incidence
      
      ( tm <- which(incid$I < 0)[1] )                   # time of negative incidence
      m <- 1                                            # number of misreporting days
      while( sum( incid$I[(tm-m):tm] ) < 0 )
        m <- m + 1
      
      ( s <- sum( incid$I[(tm-m):tm] ) )                      # sum of incidence shared with (m+1) days
      im <- rep(0, m+2)                                       # 1st element as 0 for convenient
      for( mi in 1:(m+1))                                     # from former to latter
        im[mi+1] <- ceiling( (s - sum(im[1:mi])) / (m+2-mi) ) # distribute descending incidence
      
      incid$I[(tm-m):tm] <- im[-1]                            # re-calculated incidence of (m+1) days
      
    } # while
    
    # corrected cumulative
    incid$C <- cumsum(incid$I)
    # show negative incidence
    print( cbind(country$index,
                 country$name_who,
                 incid[(tm-m):tm, ]) )
    
  } # if
  
  ## output ################################################################
  # incidence
  incid <- list(C = incid)
  
  # output
  return(incid)
  
}