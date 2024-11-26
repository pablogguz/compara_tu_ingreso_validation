
#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comaparatuingreso.es
#* This script: variance decomposition (national level)
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

# Create a weighted variance function
weighted.var <- function(x, w) {
    sum(w * (x - weighted.mean(x, w))^2) / sum(w)
}

# ------------------------------- National-level Decomposition -------------------------------

national_decomp <- atlas_all %>%
    filter(
        !is.na(net_income_equiv) & 
        !is.na(population)
    ) %>%
    # Add CCAA information
    left_join(ccaa_mapping, by = "prov_code") %>%
    mutate(
        log_income = log(net_income_equiv),
        # Calculate sigma from Gini for within-tract variance
        sigma = sqrt(2) * qnorm((gini / 100 + 1) / 2),
        within_var = sigma^2
    ) %>%
    # Calculate means at each level
    group_by(prov_code) %>%
    mutate(prov_mean = weighted.mean(log_income, population)) %>%
    group_by(ccaa_name) %>%
    mutate(ccaa_mean = weighted.mean(log_income, population)) %>%
    ungroup() %>%
    group_by(mun_code) %>%
    mutate(mun_mean = weighted.mean(log_income, population)) %>%
    ungroup() %>%
    # Summarise for national decomposition
    summarise(
        # Print diagnostics
        n = n(),
        mean_log_income = mean(log_income),
        mean_within_var = mean(within_var),
        
        # Total variance (now correctly including within-tract component)
        total_var = weighted.var(log_income, population) + weighted.mean(within_var, population),
        
        # Between provinces variance
        between_prov = weighted.var(prov_mean, population),
        
        # Between communities variance (across Autonomous Communities)
        between_ccaa = weighted.var(ccaa_mean, population),
        
        # Between municipalities (within provinces)
        between_mun = weighted.var(log_income, population) - weighted.var(prov_mean, population),
        
        # Between tracts (within municipalities)
        between_tract = weighted.var(log_income, population) - weighted.var(mun_mean, population),
        
        # Within tract
        within_tract = weighted.mean(within_var, population),
        
        # Population for sorting
        population = sum(population)
    ) %>%
    # Calculate shares
    mutate(
        across(c(between_prov, between_ccaa, between_mun, between_tract, within_tract),
              ~ . / total_var * 100,
              .names = "{.col}_share"),
        total_share = between_prov_share + between_ccaa_share + 
                      between_mun_share + between_tract_share + within_tract_share
    )

# Print full results
print(national_decomp)

# Print simplified results with rounded values
national_decomp %>%
    select(ends_with("share")) %>%
    mutate(across(where(is.numeric), round, 1)) %>%
    pivot_longer(cols = everything(), names_to = "variance_component", values_to = "share") 