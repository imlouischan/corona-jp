## convergence of Richardson–Lucy deconvolution ################################
deconvolution_iter <- function(r, incid_d, incid_u, d_RL, u_RL) {
  
  # two stopping criteria
  chi2 <- rep(NA, r) # option A: chi-square statistic (chi2)
  rmsd <- rep(NA, r) # option B: root-mean-squared difference (RMSD)
  
  for( ri in 1:r ) {
    
    chi2[ri] <- mean((d_RL[ri+1, ] - incid_d$I)^2 / d_RL[ri+1, ])
    rmsd[ri] <- sqrt(mean( (u_RL[ri+1, ] - u_RL[ri, ])^2 ))
    
  }
  
  # subplot
  options <- c("Chi2", "RMSD") # two criteria
  options <- "RMSD"            # one criterion
  par( mfrow = c(length(options)+1, 1) )
  
  for( option in options ) { # optionA/B: chi2/rmsd
    
    # x
    iteration <- 1:r
    # y
    if(option == "Chi2") ( criterion <- chi2 )  else ( criterion <- rmsd )
    
    # curvature (theta)
    theta <- rep(NA, r)
    
    for(ri in 2:(r-1)) {
      
      # three points
      xa <- iteration[ri-1]; ya <- criterion[ri-1];
      xb <- iteration[ri  ]; yb <- criterion[ri  ];
      xc <- iteration[ri+1]; yc <- criterion[ri+1];
      # angles between vectors
      theta_ba <- atan2(ya-yb, xa-xb)
      theta_bc <- atan2(yc-yb, xc-xb)
      theta_cba <- theta_ba - theta_bc
      # inner angle_CBA [0, 360)
      theta[ri] <- ifelse(theta_cba < 0,
                          theta_cba + 2*pi,
                          theta_cba)
      
    }
    ( theta <- theta * 180/pi ) # convert from radian to degree
    ( theta <- 360 - theta )    # outer angle_ABC [0, 360)
    ( rc <- which(theta == max(theta, na.rm = T)) ) # number of iterations maximize curvature
    
    # # plot stopping criterion
    # par(mar=c(5, 5, 2, 5) + 0.1, mgp=c(3, 1, 0), las=0)
    # plot(iteration, criterion,
    #      type = "o", lty = "solid",
    #      col = "black",
    #      lwd = 2, cex = 2,
    #      cex.axis = 1.5,
    #      xlim = c(min(iteration), max(iteration)),
    #      ylim = c(min(criterion), max(criterion)),
    #      xaxt = "n",
    #      xlab = "",
    #      ylab = "",
    #      log = "xy")
    # for( ri in 1:r )
    #   points(iteration[ri], criterion[ri],
    #          col = ri+1, pch = 19,
    #          lwd = 3, cex = 1)
    # axis(1, at = iteration, labels = iteration, cex.axis = 1.5)
    # title(xlab = "Iteration", line = 2.5, cex.lab = 2)
    # title(ylab = option,      line = 2.5, cex.lab = 2)
    # title(main = paste0("rc = ", rc), adj = 0, cex.main = 2)
    #
    # # plot curvature
    # par(new = T)
    # plot(iteration, theta,
    #      type = "o", lty = "dashed",
    #      col = "darkgray",
    #      lwd = 2, cex = 2,
    #      xlim = c(min(iteration), max(iteration)),
    #      ylim = c(90, 270),
    #      xaxt = "n",
    #      yaxt = "n",
    #      xlab = "",
    #      ylab = "",
    #      log = "x")
    # abline(h = 180,
    #        lty = "dotted",
    #        col = "darkgray",
    #        lwd = 2)
    # for(ri in 2:(r-1))
    #   points(iteration[ri], theta[ri],
    #          col = ri+1, pch = 19,
    #          lwd = 3, cex = 1)
    # axis(4, at = seq(90, 270, length.out = 5), cex.axis = 1.5,
    #      col = "darkgray", col.ticks = "darkgray", col.axis = "darkgray")
    # mtext(side = 4, "Curvature", line = 2.5, cex = 2, col = "darkgray")
    
  } # optionA/B: chi2/rmsd
  
  
  # # plot incidence of confirmed cases (empty frame)
  # plot(incid_d$dates, incid_d$I,
  #      xlab = "",
  #      ylab = "Incidence",
  #      type = "n",
  #      xaxt = "n",
  #      col = "red",
  #      lwd = 3, cex = 2,
  #      cex.lab = 2, cex.axis = 1.5,
  #      ylim = c(0, max(incid_d$I, u_RL[rc+1, ])),
  #      xlim = c(min(incid_u$dates), max(incid_u$dates)))
  # axis(1, at = incid_u$dates, format(incid_u$dates, "%d-%b"), cex.axis = 1.5, las = 2)
  # title(main = country$name_who, adj = 0, cex.main = 2)
  # # plot expected convolution (all iterations)
  # for( ri in 1:(r+1) )
  #   lines(incid_d$dates, d_RL[ri, ],
  #         type = "l", lty = "dashed",
  #         col = ri,
  #         lwd = 1.5, cex = 1)
  # # plot deconvoluted incidence of infection (all iterations)
  # for( ri in 1:(r+1) )
  #   lines(incid_u$dates, u_RL[ri, ],
  #         type = "l",
  #         col = ri,
  #         lwd = 1.5, cex = 1)
  # # plot incidence of confirmed cases
  # lines(incid_d$dates, incid_d$I,
  #       type = "l",
  #       col = "red",
  #       lwd = 3, cex = 2)
  # # plot deconvoluted incidence of infection (last iteration)
  # lines(incid_u$dates, u_RL[rc+1, ],
  #       type = "l",
  #       col = "black",
  #       lwd = 3, cex = 1)
  #
  # # 1x1 plot
  # par( mfrow = c(1, 1) )
  #
  # # save deconvolution plot
  # ( filename <- paste0("figures/3_deconv_iter_", country$index, ".pdf") )
  # # dev.print(pdf, filename, width = 11, height = 8.5)
  
  # deconvoluted, u
  rownames(u_RL) <- (1:nrow(u_RL)) - 1 # iterations
  colnames(u_RL) <- incid_u$dates      # dates
  
  incid_u_RL <- reshape2::melt(u_RL, value.name = "I", varnames = c("ri", "dates"))
  incid_u_RL$ri    <- (incid_u_RL$ri)
  incid_u_RL$dates <- as.Date(incid_u_RL$dates, origin="1970-01-01")
  
  # convoluted, d
  rownames(d_RL) <- (1:nrow(d_RL)) - 1 # iterations
  colnames(d_RL) <- incid_d$dates      # dates
  
  incid_d_RL <- reshape2::melt(d_RL, value.name = "I", varnames = c("ri", "dates"))
  incid_d_RL$ri    <- (incid_d_RL$ri)
  incid_d_RL$dates <- as.Date(incid_d_RL$dates, origin="1970-01-01")
  
  ## ggplot ####################################################################
  library(ggplot2)
  
  # plot incidence curves
  fig_incidence <-
    ggplot() +
    # confirmation curves
    geom_line(data = incid_d_RL, aes(x = dates, y = I, group = ri, col = ri, linetype = "Confirmation (Iterative)"), show.legend = T) +
    # infection curves
    geom_line(data = incid_u_RL, aes(x = dates, y = I, group = ri, col = ri, linetype = "Infection (Iterative)")) +
    # confirmation curve (observed data)
    geom_line(data = incid_d,                      aes(x = dates, y = I, fill = "Confirmation (data)"), size = 1.5, col = "red") +
    # infection curve (selected)
    geom_line(data = subset(incid_u_RL, ri == rc), aes(x = dates, y = I, fill = "Infection (selected)"), size = 1.5, col = "black") +
    # options
    scale_fill_manual(element_blank(), values = c(1, 1), guide = guide_legend(override.aes = list(color = c("red", "black"), size = 1.5))) +
    scale_linetype_manual(element_blank(), values = c("Infection (Iterative)" = "solid", "Confirmation (Iterative)" = "dashed")) +
    scale_color_gradient("Iterations", guide = guide_colorbar(direction = "horizontal", title.position = "top")) +
    # guides(linetype = guide_legend(order = 2)) +
    ylab("Incidence curves") +
    xlab("Time") +
    ggtitle(paste0(country$name_who, " (", country$name_iso, ")")) +
    scale_x_date(date_labels = "%d-%b",
                 minor_breaks = "1 day",
                 breaks = subset(incid_u_RL, ri == rc)$dates[round(seq(1, length(subset(incid_u_RL, ri == rc)$dates), length.out = 10))],
                 limits = c(subset(incid_u_RL, ri == rc)$dates[1], tail(subset(incid_u_RL, ri == rc)$dates, 1) + 1)) +
    theme_bw(base_size = 20) +
    theme(legend.position = c(0.25, 0.6),
          legend.margin = margin(0., 0., 0., 0., "cm"),
          legend.background = element_rect(fill = NA),
          legend.key.width = unit(3, "line"),
          legend.key = element_rect(fill = NA, color = NA),
          # legend.title = element_blank(),
          # plot.title   = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1))
  
  ## ggplot ####################################################################
  library(ggplot2)
  
  # data frame of stopping criterion
  stop <- data.frame(iteration, criterion, theta)
  
  # plot criterion, (RMSD)
  fig_criterion <-
    ggplot(stop, aes(x = iteration, y = criterion)) +
    geom_line() +
    geom_point() +
    geom_point(data = subset(stop, iteration == rc), aes(x = iteration, y = criterion), size = 3, col = "red") +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    ylab("RMSD") +
    xlab("Iterations") +
    theme_bw(base_size = 20)
  
  # plot theta, (curvature)
  fig_theta <-
    ggplot(stop, aes(x = iteration, y = theta)) +
    geom_line() +
    geom_point() +
    geom_point(data = subset(stop, iteration == rc), aes(x = iteration, y = theta), size = 3, col = "red") +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    ylab("Curvature") +
    xlab("Iterations") +
    theme_bw(base_size = 20)
  
  # plot together
  ( p <- cowplot::plot_grid(fig_incidence, fig_criterion, fig_theta,
                            labels = "AUTO", label_size = 24, label_fontface = "plain", hjust = 0, vjust = 1,
                            ncol = 1, rel_heights = c(2, 1, 1),
                            align = "v") )
  
  # save
  print( filename <- paste0("figures/3b_deconv_iter",
                            "_", country$index,
                            "_", country$selected,
                            "_", ic,
                            "_", country$name_who, ".pdf") )
  ( filename <- paste0("figures/3b_deconv_iter_", country$index, "_", ic, ".pdf") )
  ggplot2::ggsave(file = filename, plot = p, width = 8.5, height = 11)
  
  return(rc)
}