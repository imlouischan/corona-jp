## plot ########################################################################
reproduction_plot <- function(fig, incid, Rn, Rt) {
  
  # reproduction number Rt
  RRt <- data.frame(t_start = Rt$dates[Rt$R$t_start],
                    t_end   = Rt$dates[Rt$R$t_end],
                    R_mean  = Rt$R$`Mean(R)`,
                    R_025   = Rt$R$`Quantile.0.025(R)`,
                    R_975   = Rt$R$`Quantile.0.975(R)`)
  # reproduction number Rn
  RRn <- data.frame(t_start = Rn$dates[Rn$R$t_start] - 0.5,
                    t_end   = Rn$dates[Rn$R$t_end]   + 0.5,
                    R_mean  = Rn$R$`Mean(R)`,
                    R_025   = Rn$R$`Quantile.0.025(R)`,
                    R_975   = Rn$R$`Quantile.0.975(R)`)
  df_Rn <- reshape2::melt(RRn, id = c("R_mean", "R_025", "R_975"))
  df_Rn$group <- as.factor(rep(seq_len(length(Rn$R$t_start)),
                               dim(df_Rn)[1] / length(Rn$R$t_start)))
  
  # plot two reproduction number, Rn & Rt
  ( fig_reproduction <-
      ggplot(df_Rn, aes(x = value, y = as.numeric(R_mean), group = as.factor(group))) +
      # R = 1
      geom_hline(yintercept = 1, linetype = "dashed", col = "darkgreen", size = 0.5) +
      # Rn
      geom_ribbon(aes(ymin = R_025, ymax = R_975), fill = alpha("black", 0.3)) +
      geom_line(aes(y = R_mean, col = "Rn (non-overlapping)"), size = 1.5) +
      # Rt
      geom_ribbon(data = RRt, aes(x = t_end, ymin = R_025, ymax = R_975, group = 1), fill = alpha("blue", 0.3)) +
      geom_line(  data = RRt, aes(x = t_end, y = R_mean, group = 1, col = "Rt (overlapping)"), size = 0.75) +
      # scale_fill_manual(values = c("Rn" = alpha("black", 0.3), "Rt" = alpha("blue", 0.3))) +
      scale_color_manual(values = c("Rn (non-overlapping)" = "black", "Rt (overlapping)" = "blue")) +
      
      # options
      ylim(0, max(Rt$R$`Quantile.0.975(R)`, Rn$R$`Quantile.0.975(R)`)) +
      ylab("\n Reproduction \n numbers") +
      xlab("Time") +
      ggtitle("Reproduction number") +
      scale_x_date(date_labels = "%d-%b",
                   minor_breaks = "1 day",
                   breaks = incid$EC$dates[round(seq(1, length(incid$EC$dates), length.out = 10))],
                   limits = c(incid$EC$dates[1], tail(incid$EC$dates, 1) + 1)) +
      # geom_vline(xintercept = t_inter - 0.5) + # interventions
      # geom_vline(xintercept = R_si$dates[t_index] - 0.5,
      #            linetype = "dashed") + # index case
      # geom_vline(xintercept = R_si$dates[day] + 0.5,
      #            linetype = "dashed") + # now
      theme_bw(base_size = 20) +
      # theme(legend.position = "none") +
      theme(legend.position = c(0.7, 0.85),
            legend.key.width = unit(3, "line"),
            legend.background = element_rect(fill = NA),
            legend.key = element_rect(fill = NA, color = NA),
            legend.title = element_blank(),
            plot.title   = element_blank(),
            axis.title.x = element_blank(),
            # axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1))
            axis.text.x  = element_blank())
  )
  
  # range of Rt
  ( c(RRt$t_start[1], tail(RRt$t_end, 1)) )
  
  # add 3rd figure
  fig$reproduction <- fig_reproduction
  # plot together
  ( p <- cowplot::plot_grid(fig$cumulative, fig$incidence, fig$reproduction,
                            ncol = 1, rel_heights = c(1, 0.9, 1.2),
                            align = "v") )
  
  # save
  # print( filename <- paste0("figures/4_R_", country$index, ".pdf") )
  # ggplot2::ggsave(file = filename, plot = p, width = 8.5, height = 11, device = cairo_pdf)
  
  # output
  return(fig)
  
}