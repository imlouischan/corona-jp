# load data
load("/Users/louis/Dropbox/HU_IST/corona/R_code/temp_fig/figures-13-print/log10etamn_50_selected_unif.Rdata")

# portfolio effectiveness (50 vectors in a list)
log10etanL <- lapply(log10etamn, colSums, na.rm = T)
etanL <- lapply(log10etanL, function(x){10^x})

# mean and variance
u <- unlist(lapply(log10etanL, mean))
v <- log10(unlist(lapply(etanL, var))) # instead of log10-scale: unlist(lapply(log10etanL, var))
uv <- data.frame(u = u, v = v)

# ISO2 code
name_iso <-
  c("AL", "AT", "BE", "BA", "BR", "CA", "HR",
    "CZ", "DK", "EC", "SV", "EE", "FI", "FR",
    "DE", "GR", "HN", "HU", "IS", "IN", "ID",
    "IE", "IT", "JP", "KZ", "XK", "KW", "LT",
    "MY", "MU", "MX", "ME", "NL", "NZ", "MK",
    "NO", "PT", "KR", "RO", "RS", "SG", "SK",
    "SI", "ES", "SE", "CH", "SY", "TH", "GB", "US")
rownames(uv) <- name_iso

# plot
plot(u, v)

library(ggplot2)
p1 <-
  ggplot(uv, aes(x = u, y = v)) +
  geom_point(shape = 1, size = 10, col = "red") +
  geom_text(aes(label = name_iso), hjust = 0.5, vjust = 0.5, size = 5) +
  # geom_smooth(method = "lm", se = T, col = "black", size = 1.5) +
  xlab("Mean of portfolio effectiveness") +
  ylab("Variance of portfolio effectiveness") +
  scale_x_continuous(breaks = seq(0, 0.07, 0.01), labels = scales::math_format(10^.x), limits = c(0, 0.065)) +
  scale_y_continuous(breaks = -3:0, labels = scales::math_format(10^.x), limits = c(-3, 0)) +
  theme_bw(base_size = 24)

# save
print( filename <- paste0("figures/7a_pareto.pdf") )
ggplot2::ggsave(file = filename, plot = p1, width = 11, height = 8.5)