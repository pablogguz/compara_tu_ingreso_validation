
#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comparatuingreso.es
#* This script: compares mixture to national log-normal distribution
#-------------------------------------------------------------

packages_to_load <- c(
    "tidyverse",
    "data.table",
    "haven"
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

# ------------------------------- Load data -------------------------------
# Assuming 'atlas_all' contains tract-level data with Gini, net_income_equiv, and population
atlas_all <- read_dta("data-raw/atlas_all.dta")

# Filter for relevant data
atlas_all <- atlas_all %>%
  filter(!is.na(gini), !is.na(net_income_equiv), !is.na(population), population > 0)

# Calculate weights
atlas_all <- atlas_all %>%
  mutate(weight = population / sum(population))

# ------------------------------- National parameters -------------------------------
# Calculate national-level Gini and mean income
national_gini <- sum(atlas_all$gini * atlas_all$weight)
national_mean_income <- sum(atlas_all$net_income_equiv * atlas_all$population) / sum(atlas_all$population)

# Calculate parameters for national log-normal distribution
national_sigma <- sqrt(2) * qnorm((national_gini / 100 + 1) / 2)
national_mu <- log(national_mean_income) - national_sigma^2 / 2

# ------------------------------- Tract-Level parameters -------------------------------
# Calculate tract-level log-normal parameters
atlas_all <- atlas_all %>%
  mutate(
    sigma = sqrt(2) * qnorm((gini / 100 + 1) / 2),
    mu = log(net_income_equiv) - sigma^2 / 2
  )

# ------------------------------- Mixture distribution -------------------------------
# Function to evaluate mixture density
mixture_density <- function(x, data) {
  sapply(x, function(xi) {
    sum(data$weight * dlnorm(xi, meanlog = data$mu, sdlog = data$sigma))
  })
}

# Generate x values and evaluate densities
x_grid <- seq(0, max(atlas_all$net_income_equiv) + 30000, length.out = 1000)
mixture_densities <- mixture_density(x_grid, atlas_all)

# ------------------------------- National log-normal -------------------------------
# Evaluate national log-normal density
national_densities <- dlnorm(x_grid, meanlog = national_mu, sdlog = national_sigma)

# ------------------------------- Plot -------------------------------
# Combine data for plotting
plot_data <- tibble(
  x = rep(x_grid, 2),
  density = c(mixture_densities, national_densities),
  Distribution = rep(c("Mixture", "National log-normal"), each = length(x_grid))
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
mixture_median <- quantile(atlas_all$net_income_equiv, 0.5, weights = atlas_all$weight)
national_median <- exp(national_mu)

cat("Median of Mixture Distribution:", format(mixture_median, big.mark = ".", decimal.mark = ","), "\n") 
cat("Median of National Log-Normal Distribution:", format(national_median, big.mark = ".", decimal.mark = ","), "\n")
