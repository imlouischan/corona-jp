## plot ########################################################################
theme_plot <- function(log10etamn) {
  
  # effectiveness of measure theme (50 matrix in a list)
  log10etamL <- lapply(log10etamn, rowSums, na.rm = T)
  
  # convert list to matrix
  log10etamM <- matrix(unlist(log10etamL), ncol = length(log10etamn), byrow = F)
  rownames(log10etamM) <- names(log10etamL[[1]]) # measures
  colnames(log10etamM) <- names(log10etamL)      # countries
  
  # remove non-implemented measures
  log10etamM[log10etamM == 0] <- NA
  
  # temp plot matrix
  # lattice::levelplot(t(log10etamM))
  
  # convert matrix to data frame
  log10etam <- reshape2::melt(log10etamM, value.name = "log10etam", varnames = c("measures1", "countries"))
  log10etam$m1 <- as.numeric(log10etam$measures1)
  
  # remove non-implemented measures
  log10etam <- subset(log10etam, !is.na(log10etam))
  
  # add group mean and median values
  log10etam$log10etam.mean   <- ave(log10etam$log10etam, log10etam$m1)
  log10etam$log10etam.median <- ave(log10etam$log10etam, log10etam$m1, FUN = median)
  
  # temp - checking
  subset(log10etam, countries == "Italy")
  round( subset(log10etam, countries == "Italy")$log10etam.median * 100 )/100
  
  ## ggplot ####################################################################
  # (https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/)
  library(ggplot2)
  
  # plot matrix
  ( fig_matrix <-
      ggplot(log10etam, aes(x = countries, y = m1, fill = log10etam)) +
      # geom_tile() +
      geom_point(shape = 21, size = 5, stroke = 1, col = "black") +
      # options
      ylab("Intervention \n themes") +
      xlab("Country") +
      ggtitle("") +
      scale_fill_gradient2(low  = "red",  # bad measures
                           high = "blue", # good measures
                           mid  = "white",
                           na.value = "black",
                           # breaks = -5:5, limits = c(-5, 5),
                           guide = guide_colorbar(title = "log10(Effectiveness)",
                                                  title.position = "left", title.vjust = 1,
                                                  label.position = "bottom", label.vjust = 3,
                                                  barwidth = 30, barheight = 1,
                                                  # nbin = 11, raster = F,
                                                  frame.colour = "black",
                                                  frame.linewidth = 1.5,
                                                  ticks.colour = "black",
                                                  ticks.linewidth = 1.5,
                                                  direction = "horizontal")) +
      scale_y_continuous(breaks = 1:8, labels = tolower(as.roman(1:8)), limits = c(1-0.5, 8+0.5)) +
      theme_bw(base_size = 20) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.box.spacing = unit(0, "cm"),
        legend.background = element_rect(fill = NA),
        legend.margin = margin(0., 0., 0., 0., "cm"),
        # legend.title = element_blank(),
        # legend.text  = element_text(size = 12),
        plot.title   = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14))
  )
  
  # plot boxplot
  ( fig_boxplot <-
      ggplot(log10etam, aes(x = m1, y = log10etam, group = measures1)) +
      # etam = 1
      geom_hline(yintercept = 0, linetype = "dashed", col = "darkgreen", size = 0.5) +
      # etam
      # stat_summary(fun.y = mean, geom = "point", shape = 22, size = 3, color = "black", fill = "darkgreen") +
      # geom_violin(fill = "darkgray", color = NA) +
      # geom_boxplot(aes(fill = log10etam.mean)) +
      geom_boxplot(fill = NA) +
      coord_flip() +
      # options
      ylab("log10(Effectiveness)") +
      xlab("Intervention \n themes") +
      ggtitle("") +
      scale_fill_gradient2(low  = "red",  # bad measures
                           high = "blue", # good measures
                           mid  = "white",
                           na.value = "black",
                           # limits = c(min(log10etam$log10etam), max(log10etam$log10etam)),
                           guide = guide_colorbar(title = "log10(Effectiveness)",
                                                  title.position = "left", title.vjust = 1,
                                                  label.position = "bottom", label.vjust = 3,
                                                  barwidth = 30, barheight = 1,
                                                  # nbin = 11, raster = F,
                                                  frame.colour = "black",
                                                  frame.linewidth = 1.5,
                                                  ticks.colour = "black",
                                                  ticks.linewidth = 1.5,
                                                  direction = "horizontal")) +
      scale_x_continuous(breaks = 1:8, labels = tolower(as.roman(1:8)), limits = c(1-0.5, 8+0.5)) +
      theme_bw(base_size = 20) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.box.spacing = unit(0, "cm"),
        legend.background = element_rect(fill = NA),
        legend.margin = margin(0., 0., 0., 0., "cm"),
        # legend.title = element_blank(),
        # legend.text  = element_text(size = 12),
        plot.title   = element_blank())
  )
  
  # plot together
  ( p <- cowplot::plot_grid(fig_matrix, fig_boxplot,
                            labels = "AUTO", label_size = 24, label_fontface = "plain", #hjust = 0, vjust = 1,
                            ncol = 1, rel_heights = c(2, 1.2),
                            align = "v") )
  
  # save
  print( filename <- paste0("figures/5d_etam", "_selected", "_", ic, ".pdf") )
  ggplot2::ggsave(file = filename, plot = p, width = 11, height = 8.5)
  
  # output
  return(fig)
  
}