## plot ########################################################################
four_plot <- function(country, fig, f) {
  
  # 4 selected countries
  if( any(country$name_iso == c("NZ", "IT", "TH", "US")) & ic == "unif" ) { # any(country$index == c(34, 23, 48, 50))
    
    # plot incidence
    f[[country$name_iso]][["incidence"]] <-
      fig$incidence +
      theme(plot.title = element_text(size = 20, hjust = 0, vjust = 1))
    
    # plot effectiveness of measures
    f[[country$name_iso]][["measures"]] <-
      fig$measures
    
    if( any(country$name_iso == c("NZ")) ) { # plot legend on the right
      
      # plot incidence
      f[[country$name_iso]][["incidence"]] <- f[[country$name_iso]][["incidence"]] +
        theme(legend.position = c(0.8, 0.85))
      
    }
    
    if( any(country$name_iso == c("IT", "US")) ) { # plot without y-axis
      
      # plot incidence
      f[[country$name_iso]][["incidence"]] <- f[[country$name_iso]][["incidence"]]  +
        theme(axis.title.y = element_blank())
      
      # plot effectiveness of measures
      f[[country$name_iso]][["measures"]] <- f[[country$name_iso]][["measures"]] +
        theme(axis.title.y = element_blank())
      
      if(country$name_iso == "US") { # plot together
        
        # plot together
        ( p <- cowplot::plot_grid(f[["NZ"]][["incidence"]], f[["IT"]][["incidence"]],
                                  f[["NZ"]][["measures"]],  f[["IT"]][["measures"]],
                                  f[["TH"]][["incidence"]], f[["US"]][["incidence"]],
                                  f[["TH"]][["measures"]],  f[["US"]][["measures"]],
                                  labels = c("A", "B", "", "", "C", "D", "", ""),
                                  label_size = 24, label_fontface = "plain", #hjust = 0, vjust = 1,
                                  ncol = 2,
                                  align = "hv") )
        # save
        ( filename <- paste0("figures/8_it_etamn.pdf") )
        ggplot2::ggsave(file = filename, plot = p, width = 11*1.5, height = 8.5*1.5)
        
      }
    }
  }
  
  # output
  return(f)
  
}