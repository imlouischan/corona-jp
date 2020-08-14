## plot ########################################################################
# input: three distributions
# output: plot
delay_plot <- function(tau) {
        
        # time from infection to confirmation
        plot(1:length(tau$EC)-1, tau$EC,
             type = "o",
             col = 1, pch = 1,
             lwd = 2, cex = 2,
             cex.lab = 2, cex.axis = 1.5,
             ylim = c(0, max(tau$ES, tau$SC, tau$EC)),
             xlab = "Days",
             ylab = "")
        title(ylab = "Probability", line = 2.5, cex.lab = 2)
        title(main = "Time delay distribution", adj = 0, cex.main = 2)
        
        # time from symptom onset to confirmation
        lines(1:length(tau$SC)-1, tau$SC,
              type = "o",
              col = 2, pch = 2,
              lwd = 2, cex = 2)
        
        # time from infection to symptom onset, incubation period
        lines(1:length(tau$ES)-1, tau$ES,
              type = "o",
              col = 4, pch = 4,
              lwd = 2, cex = 2)
        
        # legend
        legend(x = c(8, 35), y = c(0.10, 0.21),
               c("infection-to-confirmation",              # tauEC
                 "symptomatic-to-confirmation",            # tauSC
                 "infection-to-symptomatic (incubation)"), # tauES
               box.lty = 0 , bg = NA,
               col = c(1,2,4), pch = c(1,2,4),
               pt.lwd = 2, pt.cex = 2,
               cex = 2, x.intersp = 0.2, y.intersp = 0.3)
        
        # save
        ( filename <- paste0("2c_delay", ".pdf") )
        dev.print(pdf, filename, width = 11, height = 8.5)
        
}