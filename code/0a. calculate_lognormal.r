#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comparatuingreso.es
#* This script: example of how to calculate the individual-level
#*   income distribution from tract-level data using a mixture of
#*   lognormals
#-------------------------------------------------------------

packages_to_load <- c(
    "tidyverse",
    "data.table",
    "ineAtlas",
    "extrafont",
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

# Load atlas data
atlas_all <- merge(
    setDT(ineAtlas::get_atlas("income", "tract")),
    setDT(ineAtlas::get_atlas("demographics", "tract"))
) %>%
    filter(year == 2022)

gini <- setDT(ineAtlas::get_atlas("gini_p80p20", "tract")) %>%
    filter(year == 2022) %>%
    select(tract_code, gini, p80p20)

atlas_all <- merge(atlas_all, gini, by = "tract_code")

# verify % of missings for each variable 
missing_percent <- atlas_all %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100))
print(missing_percent)

# Save file for summary stats 
write_dta(atlas_all, paste0("data-raw/atlas_all.dta"))

# Keep only tracts where we have the Gini available
known_tracts <- atlas_all %>%
    filter(!is.na(gini),
           !is.na(net_income_equiv),
           !is.na(population),
           population > 0)

# ------------------------------- Validation -------------------------------
check_data <- known_tracts %>%
    mutate(
        # Get sigma from Gini
        sigma = sqrt(2) * qnorm((gini/100 + 1)/2),
        # Get mu from mean income and sigma
        mu = log(net_income_equiv) - sigma^2/2,
        # Expected variables under lognormal
        expected_p80p20 = exp(sigma * (qnorm(0.8) - qnorm(0.2))),
        expected_median = exp(mu),
        # Relative difference
        rel_diff_p8p20 = abs(p80p20 - expected_p80p20)/p80p20,
        rel_diff_median = abs(median_income_equiv - expected_median)/median_income_equiv
    )

# Save expected values 
tosave <- check_data %>% select(tract_code, expected_p80p20, expected_median)
write_dta(check_data, paste0("data-raw/expected.dta"))

# ------------------------------- Calculate individual distribution -------------------------------
# Get lognormal parameters for each tract
known_tracts <- known_tracts %>%
    mutate(
        # Get sigma from Gini
        sigma = sqrt(2) * qnorm((gini/100 + 1)/2),
        # Get mu to preserve mean
        mu = log(net_income_equiv) - sigma^2/2,
        # Population weights
        weight = population/sum(population)
    )

# Function to evaluate density of mixture of lognormals at point x
mixture_density <- function(x, tracts) {
    # For each x, sum up weighted densities from each tract's lognormal
    sapply(x, function(xi) {
        sum(tracts$weight * dlnorm(xi, tracts$mu, tracts$sigma))
    })
}

# Create evaluation points
x_grid <- seq(0, max(known_tracts$net_income_equiv)+10000, length.out = 1000)

# Calculate densities
individual_density <- mixture_density(x_grid, known_tracts)

# Plot
p <- ggplot() +
    # Tract-level distribution
    geom_density(
        data = known_tracts,
        aes(x = net_income_equiv, 
            weight = population/sum(population),
            color = "Tract means"),
        size = 1
    ) +
    # Analytical individual distribution
    geom_line(
        data = data.frame(x = x_grid, y = individual_density),
        aes(x = x, y = y, color = "Log-normal"),
        size = 1
    ) +
    scale_x_continuous(
        labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
        limits = c(0, max(known_tracts$net_income_equiv)+10000)
    ) +
    scale_color_manual(
        values = c("Tract means" = "#2C3E50", 
                  "Log-normal" = "#E74C3C"),
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
        text = element_text(size = 14),  # Increase size of all text
        legend.position = "top",  # Position legend at top-left
        legend.justification = c(0, 1), # Anchor point for legend
        legend.background = element_rect(fill = "white", color = NA),
        legend.margin = margin(t = -20, r = 10, b = 10, l = 0),
        panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        panel.grid.minor.x = element_blank()   # Remove minor vertical grid lines
    )

# ------------------------------------- Some checks -------------------------------------

# 1. Mean comparison (should match)
analytical_mean <- sum(known_tracts$weight * known_tracts$net_income_equiv)
observed_mean <- weighted.mean(known_tracts$net_income_equiv, known_tracts$population)

# 2. Analytical median - need to find value where CDF = 0.5
# Function to evaluate CDF of mixture of lognormals at point x
mixture_cdf <- function(x, tracts) {
    sapply(x, function(xi) {
        sum(tracts$weight * plnorm(xi, tracts$mu, tracts$sigma))
    })
}

# Find median by solving CDF(x) = 0.5
find_median <- function(x) {
    mixture_cdf(x, known_tracts) - 0.5
}

# Use root finding to get analytical median
analytical_median <- uniroot(find_median, 
                           interval = c(0, max(known_tracts$net_income_equiv)))$root

cat("\nDistribution Statistics:\n")
cat("Analytical mean:", format(analytical_mean, big.mark=".", decimal.mark=","), "\n")
cat("Observed mean:", format(observed_mean, big.mark=".", decimal.mark=","), "\n")
cat("Analytical median:", format(analytical_median, big.mark=".", decimal.mark=","), "\n")