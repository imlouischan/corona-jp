## plot ########################################################################
effectiveness_plot <- function(fig, incid, eta) {
  
  # effectiveness, eta
  df_eta <- reshape2::melt(eta, id = c("etan"))
  df_eta$group <- as.factor(rep(seq_len(length(eta$t_start)),
                                dim(df_eta)[1] / length(eta$t_start)))
  
  # plot effectiveness
  ( fig_effectiveness <-
      ggplot(df_eta, aes(x = value, y = as.numeric(etan), group = as.factor(group))) +
      # etan = 1
      geom_hline(yintercept = 1, linetype = "dashed", col = "darkgreen", size = 0.5) +
      # etan
      geom_line(aes(y = etan), size = 1.5) +
      # options
      ylab("Portfolio \n effectiveness") +
      xlab("Time") +
      ggtitle("Effectiveness") +
      scale_y_continuous(trans = scales::log10_trans(),
                         breaks = scales::trans_breaks("log10", function(x) 10^x),
                         labels = scales::trans_format("log10", scales::math_format(10^.x))) +
      scale_x_date(date_labels = "%d-%b",
                   minor_breaks = "1 day",
                   breaks = incid$EC$dates[round(seq(1, length(incid$EC$dates), length.out = 10))],
                   limits = c(incid$EC$dates[1], tail(incid$EC$dates, 1) + 1)) +
      theme_bw(base_size = 20) +
      theme(plot.title   = element_blank(),
            axis.title.x = element_blank(),
            # axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1))
            axis.text.x  = element_blank())
  )
  
  # add 4th figure
  fig$effectiveness <- fig_effectiveness
  # plot together
  ( p <- cowplot::plot_grid(fig$cumulative, fig$incidence, fig$reproduction, fig$effectiveness,
                            ncol = 1, rel_heights = c(1, 0.9, 0.9, 1.2),
                            align = "v") )
  
  # save
  # print( filename <- paste0("figures/5a_etan_", country$index, ".pdf") )
  # ggplot2::ggsave(file = filename, plot = p, width = 8.5, height = 11, device = cairo_pdf)
  
  # output
  return(fig)
  
}