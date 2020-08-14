## effectiveness of measure ####################################################
# input: effectiveness of implementation
# input: interventions
# output effectiveness of measure
measures <- function(eta, inter) {
  
  # effectiveness of measures, etamn
  repn   <- match(inter$dates, eta$t_start + 0.5) # replicate etan
  inter <- cbind(inter, eta[repn, ])              # match with measures
  inter <- inter[!is.na(inter$etan), ]            # remove earlier measures
  inter$log10etan  <- log10(inter$etan)                           # all measures
  inter$repm       <- rep(table(inter$dates), table(inter$dates)) # number of measures
  inter$log10etamn <- log10(inter$etan) / inter$repm              # one measure
  inter$etamn      <- 10^(inter$log10etamn)                       # one measure
  
  # output
  return(inter)
  
}