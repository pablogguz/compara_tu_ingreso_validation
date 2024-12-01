
#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comparatuingreso.es
#* This script: compares mixture to national log-normal distribution
#-------------------------------------------------------------

packages_to_load <- c(
    "tidyverse",
    "data.table",
    "haven",
    "fst"
)

package.check <- lapply(
  packages_to_load,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

lapply(packages_to_load, require, character=T)
#-------------------------------------------------------------------

# ------------------------------- National parameters -------------------------------
# National-level statistics from EU-SILC
national_mean <- 20676  # From EU-SILC 2022
national_gini <- 31.5   # From EU-SILC 2022

national_sigma <- sqrt(2) * qnorm((national_gini/100 + 1)/2)
national_mu <- log(national_mean) - national_sigma^2/2

# ------------------------------- Mixture distribution -------------------------------
mixture_densities <- read.fst("data/density_curve.fst")
national_percentiles <- read_fst("data/national_percentiles.fst")

# ------------------------------- National log-normal -------------------------------
x_grid <- seq(0, max(mixture_densities$x) + 30000, length.out = 1000)

# Evaluate national log-normal density
national_densities <- dlnorm(x_grid, meanlog = national_mu, sdlog = national_sigma)
national_densities <- tibble(
  x = x_grid,
  y = c(national_densities)
)

# ------------------------------- Plot -------------------------------
# Combine national and mixture densities into a single data frame
plot_data <- bind_rows(
  mixture_densities %>% mutate(Distribution = "Mixture"),
  national_densities %>% mutate(Distribution = "National log-normal")
) %>%
  rename(
    density = y
  )

# Create plot
p <- ggplot(plot_data, aes(x = x, y = density, color = Distribution)) +
  geom_line(size = 1) +
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
    limits = c(0, max(atlas_all$net_income_equiv) + 30000)
  ) +
  scale_color_manual(
    values = c(
      "Mixture" = "#2C3E50",  # Navy
      "National log-normal" = "#58A2EC"                
    ),
    name = NULL,
    guide = guide_legend(keywidth = unit(1, "cm"))
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "Net equivalised income (€)",
    y = "Density"
  ) +
  theme_minimal(
    base_family = "Open Sans"
  ) +
  theme(
    legend.title = element_blank(),  
    text = element_text(size = 14),  
    legend.position = "top",         
    legend.justification = c(0, 1),  
    legend.background = element_rect(fill = "white", color = NA),
    legend.margin = margin(t = -20, r = 10, b = 10, l = 0),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank()   
  )

ggsave(
    "output/tract_vs_national_income_distribution.png", 
    p, 
    bg = "white",
    width = 10, 
    height = 6
)

# Verify means ----

# Mean of the mixture distribution
mixture_mean <- sum(atlas_all$weight * atlas_all$net_income_equiv)

# Mean of the national log-normal distribution
national_mean <- exp(national_mu + (national_sigma^2 / 2))

# Print and compare
cat("Mean of Mixture Distribution:", format(mixture_mean, big.mark = ".", decimal.mark = ","), "\n")
cat("Mean of National Log-Normal Distribution:", format(national_mean, big.mark = ".", decimal.mark = ","), "\n")

# Compare medians 
mixture_median <- national_percentiles %>%
  filter(percentile == 50) %>%
  pull(value)

national_median <- exp(national_mu)

cat("Median of Mixture Distribution:", format(mixture_median, big.mark = ".", decimal.mark = ","), "\n") 
cat("Median of National Log-Normal Distribution:", format(national_median, big.mark = ".", decimal.mark = ","), "\n")
