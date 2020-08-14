## effectiveness in matrix form ################################################
# input: effectiveness in matrix form
# input: intervention with effectiveness
# output: adding effectiveness in matrix form
measures_matrix <- function(log10etamn, inter) {
  
  if( nrow(inter) ) { # normal situation for most countries
    
    # print in matrix form
    log10etanmM <-
      reshape2::acast(inter, measures1 ~ dates, sum,
                      margins = F, drop = F,
                      value.var = "log10etamn")
    
    # remove non-implemented measures
    log10etanmM[log10etanmM == 0] <- NA
    
    # temp plot matrix
    # lattice::levelplot(t(log10etanmM))
    
    # margins
    ( log10etan <- colSums(log10etanmM, na.rm = T) )
    ( log10etam <- rowSums(log10etanmM, na.rm = T) )
    
    # output
    log10etamn[[paste0(country$name_who, " (", country$name_iso, ")")]] <- log10etanmM
    
  } else { # special situation: no measure in "Syrian Arab Republic"
    
    # output
    log10etamn[[paste0(country$name_who, " (", country$name_iso, ")")]] <- NA
    
  }
  
  # output
  return(log10etamn)
  
}