
#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comaparatuingreso.es
#* This script: calculates municipality-level stats
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

# ------------------------------ Atlas ----------------------------- #
atlas_income <- merge(
    setDT(ineAtlas::get_atlas("income", "municipality")),
    setDT(ineAtlas::get_atlas("demographics", "municipality"))
) %>%
    filter(year == 2022) %>%
    select(
        mun_code, prov_code,
        prov_name, mun_name, net_income_equiv, 
        net_income_pc, population
    )

atlas_income_sources <- get_atlas(
    "income_sources",
    level = "municipality"
) %>%
    filter(year == 2021) %>%
    select(mun_code, wage, pension)

# ---------------------------- Census ------------------------------- #

mun_data_census <- get_census(level = "municipality") %>%
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
        mun_code, total_pop, total_employed, employment_rate,
        pct_foreign_born, pct_higher_ed_completed, unemployment_rate,
        pct_female, pct_single
    )

# ------------------------------ Combine ----------------------------- #
# Base processing that applies to all indicators
processed_data <- atlas_income %>%
    left_join(atlas_income_sources) %>%
    left_join(mun_data_census) %>%
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
    ) %>%
    # replace with provincial average if missing
    mutate(
        # Add imputation flags
        across(
            c(avg_salary, net_income_equiv, employment_rate, 
              pct_foreign_born, pct_higher_ed_completed),
            list(is_imputed = ~as.integer(is.na(.)))
        )
    ) %>%
    group_by(prov_code) %>%
    mutate(
        across(
            c(avg_salary, net_income_equiv, employment_rate, 
              pct_foreign_born, pct_higher_ed_completed),
            ~ ifelse(
                is.na(.),
                weighted.mean(., total_pop, na.rm = TRUE),
                .
            )
        )
    ) %>%
    ungroup() %>%
    select(
        mun_code, prov_code, 
        avg_salary, net_income_equiv, employment_rate,
        pct_foreign_born, pct_higher_ed_completed,
        ends_with("is_imputed")
    )

# ------------------------------ Save ----------------------------- #

# Calculate number of imputations per municipality
imputation_stats <- processed_data %>%
  select(ends_with("is_imputed")) %>%
  mutate(
    total_imputed = rowSums(across(everything()))
  ) %>%
  count(total_imputed) %>%
  mutate(
    pct = n/sum(n) * 100
  )

# Print summary
print("Distribution of imputed variables per municipality:")
print(imputation_stats)

# Save
municipality_stats <- processed_data %>%
    mutate(
        is_imputed = as.integer(rowSums(across(ends_with("_is_imputed"))) > 0)
    ) %>%
    select(
        mun_code, prov_code, avg_salary,
        net_income_equiv, employment_rate, pct_foreign_born, 
        pct_higher_ed_completed, is_imputed
    )

write.fst(municipality_stats, "data/municipality_stats.fst")
