# load data
load("/Users/louis/Dropbox/HU_IST/corona/R_code/temp_fig/figures-13-print/log10etamn_50_selected_unif.Rdata")

# effectiveness of measure theme (50 matrix in a list)
log10etamL <- lapply(log10etamn, rowSums, na.rm = T)

# convert list to matrix
log10etamM <- matrix(unlist(log10etamL), ncol = length(log10etamn), byrow = F)
rownames(log10etamM) <- names(log10etamL[[1]]) # measures
colnames(log10etamM) <- names(log10etamL)      # countries

# remove non-implemented measures
log10etamM[log10etamM == 0] <- NA

# mean and variance
u <- apply(log10etamM, 1, mean, na.rm = T) # same as: u <- rowMeans(log10etamM, na.rm = T)
v <- log10(apply(10^log10etamM, 1, var,  na.rm = T)) # instead of log10-scale: v <- apply(log10etamM, 1, var,  na.rm = T)
uv <- data.frame(u = u, v = v)

# plot
plot(u, v)

library(ggplot2)
p2 <-
  ggplot(uv, aes(x = u, y = v)) +
  geom_point(shape = 1, size = 10, col = "red") +
  geom_text(aes(label = tolower(as.roman(1:8))), hjust = 0.5, vjust = 0.5, size = 5) +
  geom_smooth(method = "lm", se = T, col = "black", size = 1.5) +
  xlab("Mean of aggregated effectiveness") +
  ylab("Variance of aggregated effectiveness") +
  scale_x_continuous(breaks = seq(-0.05, 0.15, 0.05), labels = scales::math_format(10^.x), limits = c(-0.05, 0.15)) +
  scale_y_continuous(breaks = -3:1, labels = scales::math_format(10^.x), limits = c(-3, 1.5)) +
  # scale_y_continuous(breaks = seq(-0.01, 0.06, 0.01), labels = scales::math_format(10^.x), limits = c(-0.01, 0.06)) +
  theme_bw(base_size = 24)

# save
print( filename <- paste0("figures/7b_pareto.pdf") )
ggplot2::ggsave(file = filename, plot = p2, width = 11, height = 8.5)