
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
    "fst", 
    "ineapir"
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
    filter(year == 2023) %>%
    select(
        mun_code, prov_code,
        prov_name, mun_name, net_income_equiv, 
        net_income_pc, population
    )

atlas_income_sources <- get_atlas(
    "income_sources",
    level = "municipality"
) %>%
    filter(year == 2023) %>%
    select(mun_code, wage, pension)

# ---------------------------- Census ------------------------------- #

path_wikibarrio <- paste0("C:/Users/pablo/Documents/GitHub/wikibarrio/data-raw/")

foreign <- fread(paste0(path_wikibarrio, "/tract_foreign_raw.csv")) %>%
  filter(`Municipios` != "" & `Secciones` == "") %>%
  mutate(
    mun_code = substr(gsub("[^0-9]", "", Municipios), 1, 5),
    pop = gsub("[^0-9]", "", Total)
  ) %>%
  filter(`Periodo` == 2024 & Sexo == "Total") %>%
  select(
    mun_code, pop, `Lugar de nacimiento`
  ) %>%
  filter(`Lugar de nacimiento` != "Total") %>%
  group_by(mun_code) %>%
  mutate(
    pop = as.numeric(pop),
    total_pop = sum(pop),
    pct_foreign_born = 100 * pop / total_pop
  ) %>%
  filter(`Lugar de nacimiento` == "Extranjera") %>%
  select(pct_foreign_born, mun_code) %>%
  ungroup()

educ <- fread(paste0(path_wikibarrio, "/tract_educ_raw.csv")) %>%
  filter(`Municipios` != "" & `Secciones` == "") %>%
  mutate(
    mun_code = substr(gsub("[^0-9]", "", Municipios), 1, 5),
    value = as.numeric(gsub("[^0-9]", "", Total))
  ) %>%
  filter(`Periodo` == 2023) %>%
  filter(`Sexo` == "Total") %>%
  # Filter only the categories we need
  filter(`Nivel de formación alcanzado` %in% 
         c("Total", "Educación primaria e inferior", "Educación superior")) %>%
  # Reshape to wide format
  pivot_wider(
    id_cols = mun_code,
    names_from = `Nivel de formación alcanzado`,
    values_from = value
  ) %>%
  # Calculate shares
  mutate(
    share_primary = 100*`Educación primaria e inferior` / Total,
    pct_higher_ed_completed = 100*`Educación superior` / Total
  ) %>%
  # Select only the columns we want
  select(mun_code, pct_higher_ed_completed)

# mun_data_census <- ineAtlas::get_census(level = "municipality") %>%
#     mutate(
#         # Calculate derived population metrics
#         pop_16_64 = total_pop * pct_16to64,
#         pop_over_16 = total_pop * (1 - pct_under16),
#         total_employed = employment_rate * pop_over_16,
#         total_pensioners = pct_retirement_pension * pop_over_16,
#         workers_per_pensioner = total_employed / total_pensioners,
#     ) %>%
#     mutate(
#         # recalculate emp. rate as % of pop. 16-64
#         employment_rate = total_employed / pop_16_64
#     ) %>%
#     select(
#         mun_code, total_pop, total_employed, employment_rate,
#         pct_foreign_born, pct_higher_ed_completed, unemployment_rate,
#         pct_female, pct_single
#     )

# ------------------------------ Combine ----------------------------- #
# Base processing that applies to all indicators
processed_data <- atlas_income %>%
    left_join(atlas_income_sources) %>%
    left_join(foreign) %>%
    left_join(educ) %>%
    # mutate(
    #     # calculate avg. gross salary
    #     total_salaries = wage * total_pop,
    #     avg_salary = round(total_salaries / total_employed, 0),
    # ) %>%
    # mutate(
    #     # Convert percentages to 0-100 scale
    #     across(
    #         c(
    #             pct_single, 
    #             unemployment_rate, employment_rate, pct_female
    #         ),
    #         ~ . * 100
    #     )
    # ) %>%
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
            c(
                net_income_equiv, 
                pct_foreign_born, pct_higher_ed_completed
            ),
            list(is_imputed = ~as.integer(is.na(.)))
        )
    ) %>%
    group_by(prov_code) %>%
    mutate(
        across(
            c(net_income_equiv, 
              pct_foreign_born, pct_higher_ed_completed),
            ~ ifelse(
                is.na(.),
                weighted.mean(., population, na.rm = TRUE),
                .
            )
        )
    ) %>%
    ungroup() %>%
    select(
        mun_code, prov_code, 
        net_income_equiv, pct_foreign_born, pct_higher_ed_completed,
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
    select(
        mun_code, prov_code,
        net_income_equiv, pct_foreign_born, 
        pct_higher_ed_completed, ends_with("_is_imputed")
    )

write.fst(municipality_stats, "data/municipality_stats.fst")
