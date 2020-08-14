## plot ########################################################################
deconvolution_plot <- function(incid) {
  
  ## plot ######################################################################
  
  # # incidence of confirmed cases
  # plot(incid$EC$dates, incid$EC$I.y,
  #      type = "l",
  #      col = "red",
  #      lwd = 3,
  #      ylab = "Incidence",
  #      xlab = "Time",
  #      main = "Incidence curves of infection and confirmed cases")
  # # incidence of infection
  # lines(incid$EC$dates, incid$EC$I.x,
  #       type = "l",
  #       col = "black",
  #       lwd = 3)
  # # legend
  # legend("topleft", c("Infection", "Confirmation"),
  #        col = c("black", "red"),
  #        lwd = 3)
  #
  # # save
  # print( filename <- paste0("figures/3_incidEC_", country$index, ".pdf") )
  # dev.print(pdf, filename, width = 11, height = 8.5)
  
  ## ggplot ####################################################################
  library(ggplot2)
  
  # plot cumulative
  ( fig_cumulative <-
      ggplot(incid$EC, aes(x = dates)) +
      geom_line(aes(y = C.y, col = "Confirmation"), size = 1.5) +
      geom_line(aes(y = C.x, col = "Infection"),    size = 1.5) +
      scale_color_manual(values = c("Infection" = "black", "Confirmation" = "red")) +
      ylab("Cumulative \n curves") +
      xlab("Time") +
      ggtitle(paste0(country$name_who, " (", country$name_iso, ")")) +
      scale_x_date(date_labels = "%d-%b-%Y",
                   minor_breaks = "1 day",
                   breaks = incid$EC$dates[round(seq(1, length(incid$EC$dates), length.out = 10))],
                   limits = c(incid$EC$dates[1], tail(incid$EC$dates, 1) + 1)) +
      theme_bw(base_size = 20) +
      theme(legend.position = c(0.2, 0.85),
            legend.key.width = unit(3, "line"),
            legend.background = element_rect(fill = NA),
            legend.key = element_rect(fill = NA, color = NA),
            legend.title = element_blank(),
            # plot.title   = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x  = element_blank())
  )
  
  # plot incidence
  ( fig_incidence <-
      ggplot(incid$EC, aes(x = dates)) +
      geom_line(aes(y = I.y, col = "Confirmation"), size = 1.5) +
      geom_line(aes(y = I.x, col = "Infection"),    size = 1.5) +
      scale_color_manual(values = c("Infection" = "black", "Confirmation" = "red")) +
      ylab("Incidence \n curves") +
      xlab("Time") +
      ggtitle(paste0(country$name_who, " (", country$name_iso, ")")) +
      scale_x_date(date_labels = "%d-%b-%Y",
                   minor_breaks = "1 day",
                   breaks = incid$EC$dates[round(seq(1, length(incid$EC$dates), length.out = 10))],
                   limits = c(incid$EC$dates[1], tail(incid$EC$dates, 1) + 1)) +
      theme_bw(base_size = 20) +
      theme(legend.position = c(0.2, 0.85),
            legend.key.width = unit(3, "line"),
            legend.background = element_rect(fill = NA),
            legend.key = element_rect(fill = NA, color = NA),
            legend.title = element_blank(),
            plot.title   = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x  = element_blank())
  )
  
  # two figures
  fig <- list(cumulative = fig_cumulative,
              incidence  = fig_incidence)
  # plot together
  ( p <- cowplot::plot_grid(fig$cumulative, fig$incidence,
                            ncol = 1, rel_heights = c(1, 0.9),
                            align = "v") )
  
  # save
  # print( filename <- paste0("figures/3_incidEC_", country$index, ".pdf") )
  # ggplot2::ggsave(file = filename, plot = p, width = 8.5, height = 11, device = cairo_pdf)
  
  # output
  return(fig)
  
}