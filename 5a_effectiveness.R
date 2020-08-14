## effectiveness of implementation #############################################
# input: reproduction number
# output: effectiveness of implementation
effectiveness <- function(Rn) {
        
        # effectiveness, etan
        etan <- head(Rn$R$`Mean(R)`, -1) / Rn$R$`Mean(R)`[-1]
        
        # effectiveness, eta
        eta <- data.frame(t_start = Rn$dates[Rn$R$t_start][-1] - 0.5,
                          t_end   = Rn$dates[Rn$R$t_end][-1]   + 0.5,
                          etan     = etan)
        
        # output
        return(eta)
        
}