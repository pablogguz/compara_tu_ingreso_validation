#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comparatuingreso.es
#* This script: calculates census tract-level stats
#-------------------------------------------------------------

packages_to_load <- c(
    "tidyverse",
    "data.table",
    "ineAtlas",
    "Hmisc",
    "fst",
    "sf"
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

# ------------------------------ Atlas ----------------------------- #
atlas_income <- merge(
    setDT(ineAtlas::get_atlas("income", "tract")),
    setDT(ineAtlas::get_atlas("demographics", "tract"))
) %>%
    filter(year == 2023) %>%
    select(
        tract_code, mun_code, prov_code,
        prov_name, mun_name, net_income_equiv, 
        net_income_pc, population
    )

# Check municipalities in which all tracts have missing income data
missing_mun <- atlas_income %>% 
    group_by(mun_code, mun_name) %>% 
    summarise(
        missing = sum(is.na(net_income_pc)),
        total = n()
    ) %>% 
    filter(missing == total)

# Save mun_code and mun_name for municipalities with missing data 
missing_mun <- missing_mun %>%
    select(mun_code, mun_name)
write.fst(missing_mun, "data/missing_mun.fst")

atlas_income_sources <- get_atlas(
    "income_sources",
    level = "tract"
) %>%
    filter(year == 2023) %>%
    select(tract_code, wage, pension)

# ---------------------------- Census ------------------------------- #

tract_data_census <- get_census(level = "tract") %>%
    mutate(
        # Calculate derived population metrics
        pop_16_64 = total_pop * pct_16to64,
        pop_over_16 = total_pop * (1 - pct_under16),
        total_employed = employment_rate * pop_over_16,
        total_pensioners = pct_retirement_pension * pop_over_16,
        workers_per_pensioner = total_employed / total_pensioners,
    ) %>%
    mutate(
        # recalculate emp. rate as % of pop. 16-64
        employment_rate = total_employed / pop_16_64
    ) %>%
    select(
        tract_code, total_pop, total_employed, employment_rate,
        pct_foreign_born, pct_higher_ed_completed, unemployment_rate,
        pct_female, pct_single
    )

# ------------------------------ Combine ----------------------------- #
# Base processing that applies to all indicators
processed_data <- atlas_income %>%
    left_join(atlas_income_sources) %>%
    left_join(tract_data_census) %>%
    mutate(
        # calculate avg. gross salary
        total_salaries = wage * total_pop,
        avg_salary = round(total_salaries / total_employed, 0),
    ) %>%
    mutate(
        # Convert percentages to 0-100 scale
        across(
            c(
                pct_higher_ed_completed, pct_foreign_born, pct_single, 
                unemployment_rate, employment_rate, pct_female
            ),
            ~ . * 100
        )
    ) %>%
    mutate(
        # Calculate national ratio for imputation
        ratio = weighted.mean(net_income_equiv / net_income_pc, w = population, na.rm = TRUE),
        # Impute missing values
        net_income_equiv = if_else(
            is.na(net_income_equiv),
            net_income_pc * ratio,
            net_income_equiv
        )
    ) 

# Helper function to find percentile position
find_percentile <- function(value, percentiles) {
    if (value <= min(percentiles)) return(0)
    if (value >= max(percentiles)) return(100)
    max(which(percentiles <= value))
}

# Load national percentiles
national_percentiles <- read_fst("data/national_percentiles.fst")

# Calculate national percentile for each tract
tracts_with_percentiles <- processed_data %>%
    mutate(
        national_percentile = sapply(net_income_equiv, function(x) {
            if(is.na(x)) return(NA)
            find_percentile(x, national_percentiles$value)
        })
    )

# Update tract stats with percentiles
tracts_stats <- tracts_with_percentiles %>%
    select(
        mun_code, prov_code, tract_code, avg_salary,
        net_income_equiv, employment_rate, pct_foreign_born,
        pct_higher_ed_completed, national_percentile
    )

write.fst(tracts_stats, "data/tracts_stats.fst")

# Create lookup tables for faster geocoding
tract_lookup <- tracts_stats %>%
    select(tract_code, mun_code, prov_code) %>%
    as.data.table()
write.fst(tract_lookup, "data/tract_lookup.fst")

# Load tract geometries 
tract_geoms <- get_tract_geom(2023)

tract_geoms_codes <- tract_geoms %>% 
    as.data.frame() %>%
    select(-geom)

mapping <- get_atlas(
    level = "municipality", 
    category = "income"
) %>% 
    select(mun_code, prov_code, mun_name, prov_name) %>%
    distinct()

# Create lookup that maps municipality and province names to codes
name_to_code_lookup <- tract_lookup %>%
    left_join(mapping) %>%
    select(tract_code, mun_code, prov_code)

# Join geometries with codes
tract_geoms_with_codes <- tract_geoms %>%
    rename( 
        mun_name = municipality,
        prov_name = province
    ) %>%
    left_join(name_to_code_lookup)

check_miss <- tract_geoms_with_codes %>% 
    filter(is.na(mun_code) | is.na(prov_code))

# Create directories if they don't exist
dir.create("data/tract_geoms", showWarnings = FALSE)
dir.create("data/tract_stats", showWarnings = FALSE)

# Get unique provinces
provinces <- unique(tract_geoms_with_codes$prov_code)

# Split and save geometries and stats by province
for(prov in provinces) {
    # Save geometries
    prov_geoms <- tract_geoms_with_codes %>%
        filter(prov_code == prov) %>%
        st_simplify(dTolerance = 1) %>%
        st_transform(4326)
    
    saveRDS(prov_geoms, sprintf("data/tract_geoms/tracts_%s.rds", prov))
    
    # Save statistics
    prov_stats <- tracts_stats %>%
        filter(prov_code == prov)
    
    write.fst(prov_stats, sprintf("data/tract_stats/tracts_%s.fst", prov))
}
