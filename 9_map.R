# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
library("ggplot2", "sf")
library("rnaturalearth", "rnaturalearthdata")

# call data
world <- ne_countries(scale = "medium", returnclass = "sf")

# matching names (using "6_pair.R")
match_index <- match(rank$name_iso, world$iso_a2)
match_index[26]  <- 119 # Kosovo

fig <- list()
for( sub in colnames(rank)[8:17] ) {
  
  # target values
  world$pop_est <- rep(NA, 241) # empty column
  world$pop_est[match_index] <- rank[, names(rank) == sub]
  
  # plot
  # theme_set(theme_bw())
  fig[[sub]] <-
    ggplot(data = world) +
    geom_sf(aes(fill = pop_est)) +
    scale_fill_gradient2(low  = "red",  # bad measures
                         high = "blue", # good measures
                         mid  = "white",
                         na.value = "darkgray",
                         # breaks = -5:5, limits = c(-5, 5),
                         guide = guide_colorbar(title = ifelse(sub == "log10.total",
                                                               "log10(Total confirmed cases)",
                                                               ifelse(sub == "etan.frac",
                                                                      "Fraction of effective portfolios",
                                                                      "log10(Effectiveness)")),
                                                title.position = "left", title.vjust = 1,
                                                label.position = "bottom", label.vjust = 3,
                                                barwidth = ifelse(sub == "log10.total" | sub == "etan.frac", 12, 15),
                                                barheight = 0.5,
                                                # nbin = 11, raster = F,
                                                frame.colour = "black",
                                                frame.linewidth = 1.5,
                                                ticks.colour = "black",
                                                ticks.linewidth = 1.5,
                                                direction = "horizontal")) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map") +
    theme_bw(base_size = 24) +
    theme(
      legend.position = "top",
      legend.justification = "right",
      legend.box.spacing = unit(0, "cm"),
      legend.background = element_rect(fill = NA),
      legend.margin = margin(0., 0., 0., 0., "cm"),
      legend.title = element_text(size = 15),
      legend.text  = element_text(size = 12),
      plot.title   = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank())
}

# plot together
p <- cowplot::plot_grid(plotlist = fig,
                        ncol = 2,
                        labels = c("(A)", "(B)",
                                   "(i)", "(ii)", "(iii)", "(iv)",
                                   "(v)", "(vi)", "(vii)", "(viii)"),
                        hjust = 0,
                        label_size = 24, label_fontface = "plain",
                        align = "hv")
# save
print( filename <- paste0("figures/9_map.pdf") )
ggplot2::ggsave(file = filename, plot = p, width = 8.5*1.5, height = 11*1.5)