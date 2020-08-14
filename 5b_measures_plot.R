## plot ########################################################################
measures_plot <- function(fig, incid, inter) {
  
  # effectivess of 8 measures
  if( nrow(inter) ) { # normal situation for most countries
    ( log10etam <- aggregate(inter$log10etamn, list(m1 = inter$m1), sum) )
  } else { # special situation: no measure in "Syrian Arab Republic"
    ( log10etam <- data.frame(m1 = 0, x = 0) )
  }
  
  # plot effectiveness of measures
  ( fig_measures <-
      ggplot(inter) +
      geom_rect(aes(xmin = t_start, ymin = m1 - 0.5,
                    xmax = t_end,   ymax = m1 + 0.5,
                    fill = log10etamn), col = "black") +
      geom_point(data = log10etam,
                 aes(x = incid$EC$dates[1], y = m1,
                     fill = x),
                 shape = 21, size = 5, stroke = 1, col = "black") +
      # options
      ylab("Intervention \n themes") +
      xlab("Time") +
      ggtitle("Measure") +
      # scale_fill_gradient2() +
      scale_fill_gradient2(low  = "red",  # bad measures
                           high = "blue", # good measures
                           mid  = "white",
                           na.value = "black",
                           # breaks = -5:5, limits = c(-5, 5),
                           guide = guide_colorbar(title = "log10(Effectiveness)",
                                                  title.position = "left", title.vjust = 1,
                                                  label.position = "bottom", label.vjust = 3,
                                                  barwidth = 15, barheight = 0.5,
                                                  # nbin = 11, raster = F,
                                                  frame.colour = "black",
                                                  frame.linewidth = 1.5,
                                                  ticks.colour = "black",
                                                  ticks.linewidth = 1.5,
                                                  direction = "horizontal")) +
      scale_y_continuous(breaks = 1:8, labels = tolower(as.roman(1:8)), limits = c(1-0.5, 8+0.5)) +
      scale_x_date(date_labels = "%d-%b",
                   minor_breaks = "1 day",
                   breaks = incid$EC$dates[round(seq(1, length(incid$EC$dates), length.out = 10))],
                   limits = c(incid$EC$dates[1], tail(incid$EC$dates, 1) + 1)) +
      theme_bw(base_size = 20) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.box.spacing = unit(0, "cm"),
        legend.background = element_rect(fill = NA),
        legend.margin = margin(0., 0., -0.15, 0., "cm"),
        # legend.position = c(0.35, 1.05), # c(0.65, 1.05),
        legend.title = element_text(size = 15),
        legend.text  = element_text(size = 12),
        plot.title   = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1))
  )
  
  
  # add 5th figure
  fig$measures <- fig_measures
  # plot together
  ( p <- cowplot::plot_grid(fig$cumulative, fig$incidence, fig$reproduction, fig$effectiveness, fig$measures,
                            labels = "AUTO", label_size = 24, label_fontface = "plain", hjust = 0, vjust = 1,
                            ncol = 1, rel_heights = c(1.2, 1, 1, 1, 1.6),
                            align = "v") )
  
  # save
  # print( filename <- paste0("figures/5b_etamn_", country$index, ".pdf") )
  # print( filename <- paste0("figures/5b_etamn_", country$index, "_", country$name_who, "_jap_0_shif.pdf") )
  # print( filename <- paste0("figures/5b_etamn_", country$index, "_", country$name_who, "_jap_0_unif.pdf") )
  # print( filename <- paste0("figures/5b_etamn_", country$index, "_", country$name_who, "_hop_1_shif.pdf") )
  # print( filename <- paste0("figures/5b_etamn_", country$index, "_", country$name_who, "_hop_1_unif.pdf") )
  # print( filename <- paste0("figures/5b_etamn_", country$index, "_", country$name_who, "_who_2_shif.pdf") )
  # print( filename <- paste0("figures/5b_etamn_", country$index, "_", country$name_who, "_who_2_unif.pdf") )
  print( filename <- paste0("figures/5b_etamn",
                            "_", country$index,
                            "_", country$selected,
                            "_", ic,
                            "_", country$name_who, ".pdf") )
  ( filename <- paste0("figures/5b_etamn_", country$index, "_", ic, ".pdf") )
  ggplot2::ggsave(file = filename, plot = p, width = 8.5, height = 11)
  
  # output
  return(fig)
  
}