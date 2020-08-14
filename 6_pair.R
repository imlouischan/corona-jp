# load data
load("/Users/louis/Dropbox/HU_IST/corona/R_code/temp_fig/figures-13-print/log10etamn_50_selected_unif.Rdata")

# data frame
rank <- data.frame()

source("1_countrynames.R")  # country names
countries <- countrynames()

for( i in 1:50 ) { # 50 countries, i
  
  ## incidence #################################################################
  country <- countries[i, ] # target
  if        ( country$selected == "hop" ) {
    source("1a_incidence_hopkins.R") # incidence from Johns Hopkins
    incid <- incidence(online = F, country)
  } else if ( country$selected == "who" ) {
    source("1b_incidence_who.R")     # incidence from WHO
    incid <- incidence(country)
  } else if ( country$selected == "jap" ) {
    source("1c_incidence_jp.R")      # incidence from Japan
    incid <- incidence(online = F, detection = F)
  }
  
  ## measures ##################################################################
  # total number of confirmed cases
  country$total <- tail(incid$C$C, 1)
  country$log10.total <- log10(country$total)
  
  # margins
  ( log10etan <- colSums(log10etamn[[i]], na.rm = T) )
  # fraction of effective implementations
  country$etan.frac <- sum(log10etan > 0) / length(log10etan)
  
  # margins
  ( log10etam <- rowSums(log10etamn[[i]], na.rm = T) )
  log10etam[log10etam == 0] <- NA
  names(log10etam) <- tolower(as.roman(1:8))
  # effectiveness of each theme
  country <- cbind(country, t(log10etam))
  
  # data frame
  rank <- rbind(rank, country)
  
}

# selected columns
rank2 <- rank[c("name_iso", "name_who", "log10.total", "etan.frac", "v")]

library(ggplot2)
# plot
p <-
  ggplot(rank2, aes(x = etan.frac, y = log10.total)) +
  geom_point(shape = 1, size = 10, col = "red") +
  geom_text(aes(label = name_iso), hjust = 0.5, vjust = 0.5, size = 5) +
  geom_smooth(method = "lm", se = T, col = "black", size = 1.5) +
  xlab("Fraction of effective portfolios") +
  ylab("Total confirmed cases") +
  scale_y_continuous(breaks = 2:6, labels = 10^(2:6), limits = c(2, 6.5)) +
  theme_bw(base_size = 24)

# save
print( filename <- paste0("figures/6a_pair.pdf") )
ggplot2::ggsave(file = filename, plot = p, width = 11, height = 8.5)

# plot
p2 <-
  ggplot(rank2, aes(x = v, y = log10.total)) +
  geom_point(shape = 1, size = 10, col = "red") +
  geom_text(aes(label = name_iso), hjust = 0.5, vjust = 0.5, size = 5) +
  # geom_smooth(method = "lm", se = T, col = "black", size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "darkgreen", size = 0.5) +
  xlab("The effectiveness of risk communication") +
  ylab("Total confirmed cases") +
  scale_x_continuous(breaks = seq(-0.5, 0.75, 0.25), labels = scales::math_format(10^.x), limits = c(-0.5, 0.75)) +
  scale_y_continuous(breaks = 2:6, labels = 10^(2:6), limits = c(2, 6.5)) +
  theme_bw(base_size = 24)

# save
print( filename <- paste0("figures/6b_pair.pdf") )
ggplot2::ggsave(file = filename, plot = p2, width = 11, height = 8.5)