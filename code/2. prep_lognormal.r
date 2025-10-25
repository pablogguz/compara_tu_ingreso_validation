
#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comaparatuingreso.es
#* This script: prepares the data for the app using log-normal
#*  assumption within tracts
#-------------------------------------------------------------

packages_to_load <- c(
    "tidyverse",
    "data.table",
    "ineAtlas",
    "Hmisc",
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

#-------------------------------------------------------------
# 1. Load and merge data
#-------------------------------------------------------------
print("Loading and merging datasets...")

# Load base atlas data
atlas_all <- merge(
    setDT(ineAtlas::get_atlas("income", "tract")),
    setDT(ineAtlas::get_atlas("demographics", "tract"))
) %>%
    filter(year == 2023)

# Impute missing net_income_equiv values
atlas_all <- atlas_all %>%
    mutate(is_imputed = as.integer(is.na(net_income_equiv) & !is.na(net_income_pc))) %>%
    group_by(prov_code) %>%
    mutate(
        # Calculate provincial ratio for imputation
        ratio = weighted.mean(net_income_equiv / net_income_pc, w = population, na.rm = TRUE),
        # Impute missing values
        net_income_equiv = if_else(
            is.na(net_income_equiv),
            net_income_pc * ratio,
            net_income_equiv
        )
    ) %>%
    ungroup()

# Report imputation statistics
n_imputed <- sum(atlas_all$is_imputed)
total_obs <- nrow(atlas_all)
    
print(paste0(
    "Imputed observations: ", n_imputed,
    " (", round(n_imputed/total_obs * 100, 2), "% of total)"
))

# Load Gini data and predictions
gini_observed <- setDT(ineAtlas::get_atlas("gini_p80p20", "tract")) %>%
    filter(year == 2023) %>%
    select(tract_code, gini) %>%
    drop_na()

gini_predicted <- read_fst("data-raw/gini_predicted.fst")

# Combine observed and predicted Gini values
all_gini <- gini_observed %>%
    bind_rows(gini_predicted) %>%
    distinct(tract_code, .keep_all = TRUE)

# Merge everything
atlas_year <- merge(atlas_all, all_gini, by = "tract_code", all.x = TRUE)

# verify % of missings for each variable in atlas_all 
missing_percent <- atlas_year %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100))

print(missing_percent)

#-------------------------------------------------------------
# 2. Calculate log-normal parameters for each tract
#-------------------------------------------------------------
print("Calculating log-normal parameters...")

atlas_params <- atlas_year %>%
    filter(!is.na(gini) & !is.na(net_income_equiv) & !is.na(population)) %>%
    mutate(
        # Get sigma from Gini (using relationship for log-normal distribution)
        sigma = sqrt(2) * qnorm((gini/100 + 1)/2),
        # Get mu to preserve mean
        mu = log(net_income_equiv) - sigma^2/2,
        # Population weights
        weight = population/sum(population)
    )

#-------------------------------------------------------------
# 3. Calculate national density curve
#-------------------------------------------------------------
print("Calculating national income density curve...")

# Function to evaluate density of mixture of lognormals
mixture_density <- function(x, data) {
    sapply(x, function(xi) {
        sum(data$weight * dlnorm(xi, data$mu, data$sigma))
    })
}

# Create evaluation points
x_grid <- seq(0, 160000, length.out = 1000)

# Calculate national density
density_points <- data.frame(
    x = x_grid,
    y = mixture_density(x_grid, atlas_params)
)

# Save national density curve
write_fst(density_points, "data/density_curve.fst")

#-------------------------------------------------------------
# 4a. Calculate provincial density curves
#-------------------------------------------------------------
print("Calculating provincial density curves...")

provincial_density_data <- atlas_params %>%
  group_by(prov_code) %>%
  group_map(function(data, group) {
    # Recalculate weights within province
    data$weight <- data$population/sum(data$population)
    # Calculate provincial density
    data.frame(
      prov_code = group$prov_code,
      x = x_grid,
      y = mixture_density(x_grid, data)
    )
  }) %>%
  bind_rows()

write_fst(provincial_density_data, "data/density_curve_prov.fst")

#-------------------------------------------------------------
# 4b. Calculate municipality-level density curves
#-------------------------------------------------------------
print("Calculating provincial density curves...")

mun_density_data <- atlas_params %>%
  group_by(mun_code, prov_code) %>%
  group_map(function(data, group) {
    # Recalculate weights within province
    data$weight <- data$population/sum(data$population)
    # Calculate provincial density
    data.frame(
      mun_code = group$mun_code,
      prov_code = group$prov_code,
      x = x_grid,
      y = mixture_density(x_grid, data)
    )
  }) %>%
  bind_rows()

dir.create("data/density_curve_mun", showWarnings = FALSE)

# Save municipality-level density curves by province
mun_density_data %>%
  group_by(prov_code) %>%
  group_walk(function(data, group) {
    file_path <- paste0("data/density_curve_mun/mun_", group$prov_code, ".fst")
    write_fst(data, file_path)
  })

#-------------------------------------------------------------
# 5. Calculate percentiles
#-------------------------------------------------------------
print("Calculating national-level percentiles...")

# Function to calculate percentiles from mixture of log-normals
mixture_quantile <- function(p, data) {
    f <- function(x) {
        # CDF of mixture minus target probability
        sum(data$weight * plnorm(x, data$mu, data$sigma)) - p
    }
    # Use root finding to get percentile
    uniroot(f, c(0, 1e6))$root
}

# Calculate national percentiles
national_percentiles <- data.frame(
    percentile = 1:99,
    value = sapply(seq(0.01, 0.99, 0.01), 
                  function(p) mixture_quantile(p, atlas_params))
)

print("Calculating provincial-level percentiles...")

# Calculate provincial percentiles
provincial_percentiles <- atlas_params %>%
    group_by(prov_code) %>%
    group_map(function(data, group) {
        # Recalculate weights within province
        data$weight <- data$population/sum(data$population)
        # Calculate percentiles
        setNames(
            as.list(sapply(seq(0.01, 0.99, 0.01), 
                          function(p) mixture_quantile(p, data))),
            paste0("p", 1:99)
        )
    }) %>%
    bind_rows() %>%
    mutate(prov_code = unique(atlas_params$prov_code)) %>%
    data.table::transpose(keep.names = "percentile", make.names = "prov_code")

print("Calculating municipality-level percentiles...")

# Calculate municipality-level percentiles
mun_percentiles <- atlas_params %>%
    group_by(mun_code) %>%
    group_map(function(data, group) {
        # Recalculate weights within municipality
        data$weight <- data$population/sum(data$population)
        # Calculate percentiles
        setNames(
            as.list(sapply(seq(0.01, 0.99, 0.01), 
                          function(p) mixture_quantile(p, data))),
            paste0("p", 1:99)
        )
    }) %>%
    bind_rows() %>%
    mutate(mun_code = unique(atlas_params$mun_code)) %>%
    data.table::transpose(keep.names = "percentile", make.names = "mun_code")

#-------------------------------------------------------------
# 6. Create municipality lookup
#-------------------------------------------------------------

print("Creating municipality lookup...")

municipality_lookup <- atlas_year %>%
    select(mun_code, mun_name, prov_code, prov_name) %>%
    distinct() %>%
    mutate(
        prov_name = ifelse(prov_name == "Avila", "Ávila", prov_name)
    )

#-------------------------------------------------------------
# 7. Save output files
#-------------------------------------------------------------
print("Saving files...")

write_fst(national_percentiles, "data/national_percentiles.fst")
write_fst(provincial_percentiles, "data/provincial_percentiles.fst")
write_fst(mun_percentiles, "data/mun_percentiles.fst")
write_fst(municipality_lookup, "data/municipality_lookup.fst")

#-------------------------------------------------------------
# 8. Verify saved files
#-------------------------------------------------------------
print("Verifying saved files...")

# Read and verify national percentiles
nat <- read_fst("data/national_percentiles.fst")
print("National percentiles structure:")
print(dim(nat))
print(head(nat))

# Read and verify provincial percentiles
prov <- read_fst("data/provincial_percentiles.fst")
print("\nProvincial percentiles structure:")
print(dim(prov))
print(head(prov[, 1:5]))

# Read and verify municipality lookup
mun <- read_fst("data/municipality_lookup.fst")
print("\nMunicipality lookup structure:")
print(dim(mun))
print(head(mun))

# Basic sanity checks
print("\nSanity checks:")
print(paste("Number of provinces:", ncol(prov) - 1))
print(paste("Number of municipalities:", nrow(mun)))
print(paste("Income range:", min(nat$value), "to", max(nat$value)))

# Load the density curve data
density_data <- read_fst("data/density_curve.fst")

# Plot for verification
ggplot(density_data, aes(x = x, y = y)) +
    geom_line() +
    labs(x = "Income", y = "Density",
         title = "Income Distribution (Log-Normal Mixture)") +
    theme_minimal()
