#-------------------------------------------------------------
# Simulate different scenarios to understand when mixture and 
# single log-normal distributions differ substantially
#-------------------------------------------------------------

library(tidyverse)

# Helper functions
calc_gini_params <- function(mean_income, gini) {
    sigma <- sqrt(2) * qnorm((gini/100 + 1)/2)
    mu <- log(mean_income) - sigma^2/2
    return(list(mu = mu, sigma = sigma))
}

mixture_density <- function(x, mus, sigmas, weights) {
    sapply(x, function(xi) {
        sum(weights * dlnorm(xi, mus, sigmas))
    })
}

# Case 1: Similar parameters (should give similar distributions)
similar_params <- tibble(
    tract = 1:100,
    mean_income = rnorm(100, mean = 20000, sd = 1000),  # Small variation
    gini = rnorm(100, mean = 30, sd = 2),               # Small variation
    weight = rep(1/100, 100)
)

# Case 2: Bimodal - Two distinct clusters (should give different distributions)
bimodal_params <- tibble(
    tract = 1:100,
    mean_income = c(
        rnorm(50, mean = 15000, sd = 1000),   # Low income cluster
        rnorm(50, mean = 40000, sd = 1000)    # High income cluster
    ),
    gini = c(
        rnorm(50, mean = 25, sd = 2),         # Low inequality cluster
        rnorm(50, mean = 35, sd = 2)          # High inequality cluster
    ),
    weight = rep(1/100, 100)
)

# Case 3: Heavy tail - Few very rich tracts (should give different distributions)
heavy_tail_params <- tibble(
    tract = 1:100,
    mean_income = c(
        rnorm(90, mean = 20000, sd = 1000),   # Most tracts
        rnorm(10, mean = 80000, sd = 5000)    # Rich tracts
    ),
    gini = c(
        rnorm(90, mean = 30, sd = 2),
        rnorm(10, mean = 40, sd = 2)
    ),
    weight = rep(1/100, 100)
)

# Function to compare mixture vs single log-normal
compare_distributions <- function(params, title) {
    # Calculate parameters for each tract
    tract_params <- params %>%
        mutate(
            params = map2(mean_income, gini, calc_gini_params),
            mu = map_dbl(params, "mu"),
            sigma = map_dbl(params, "sigma")
        )
    
    # Calculate national parameters
    national_mean <- sum(params$mean_income * params$weight)
    national_gini <- 30  # Simplified for simulation
    national_params <- calc_gini_params(national_mean, national_gini)
    
    # Evaluate densities
    x_grid <- seq(0, 100000, length.out = 1000)
    
    # Mixture density
    mixture_dens <- mixture_density(
        x_grid, 
        tract_params$mu, 
        tract_params$sigma, 
        tract_params$weight
    )
    
    # Single log-normal density
    single_dens <- dlnorm(x_grid, national_params$mu, national_params$sigma)
    
    # Create plot
    plot_data <- tibble(
        x = rep(x_grid, 2),
        y = c(mixture_dens, single_dens),
        Distribution = rep(c("Mixture", "Single Log-normal"), each = length(x_grid))
    )
    
    ggplot(plot_data, aes(x = x, y = y, color = Distribution)) +
        geom_line(size = 1) +
        scale_x_continuous(
            labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
            limits = c(0, 100000)
        ) +
        scale_color_manual(
            values = c(
                "Mixture" = "#2C3E50",
                "Single Log-normal" = "#58A2EC"
            )
        ) +
        labs(
            title = title,
            x = "Income",
            y = "Density"
        ) +
        theme_minimal()
}

# Create plots
p1 <- compare_distributions(similar_params, "Case 1: Similar Parameters")
p2 <- compare_distributions(bimodal_params, "Case 2: Bimodal Distribution")
p3 <- compare_distributions(heavy_tail_params, "Case 3: Heavy Tail")

# Arrange plots
gridExtra::grid.arrange(p1, p2, p3, ncol = 1)

# Calculate summary stats for real data
real_tract_stats <- atlas_params %>%
    summarise(
        mean_mu = mean(mu),
        sd_mu = sd(mu),
        mean_sigma = mean(sigma),
        sd_sigma = sd(sigma),
        cv_mu = sd_mu / mean_mu,
        cv_sigma = sd_sigma / mean_sigma
    )

print("Real data parameter variation:")
print(real_tract_stats)