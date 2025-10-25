#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comparatuingreso.es
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

lapply(packages_to_load, require, character.only = TRUE)

# ------------------------------- Helpers -------------------------------

# Population-weighted (population) variance (no Bessel correction)
weighted.var <- function(x, w) {
  mu <- weighted.mean(x, w)
  sum(w * (x - mu)^2) / sum(w)
}

# ------------------------------- Load data -------------------------------

# Create province to CCAA mapping
ccaa_mapping <- data.frame(
    prov_code = c(
        "01", "20", "48", # País Vasco
        "02", "13", "16", "19", "45", # Castilla-La Mancha
        "28", # Madrid
        "03", "12", "46", # Comunidad Valenciana
        "04", "11", "14", "18", "21", "23", "29", "41", # Andalucía
        "05", "09", "24", "34", "37", "40", "42", "47", "49", # Castilla y León
        "06", "10", # Extremadura
        "07", # Baleares
        "08", "17", "25", "43", # Cataluña
        "15", "27", "32", "36", # Galicia
        "22", "44", "50", # Aragón
        "26", # La Rioja
        "30", # Murcia
        "31", # Navarra
        "33", # Asturias
        "35", "38", # Canarias
        "39", # Cantabria
        "51", "52" # Ceuta y Melilla
    ),
    ccaa_name = c(
        rep("Basque Country", 3),            # País Vasco 
        rep("Castile-La Mancha", 5),         # Castilla-La Mancha
        "Madrid",                            # Madrid
        rep("Valencian Community", 3),       # Comunidad Valenciana
        rep("Andalusia", 8),                 # Andalucía
        rep("Castile and Leon", 9),          # Castilla y León
        rep("Extremadura", 2),               # Extremadura
        "Balearic Islands",                  # Baleares
        rep("Catalonia", 4),                 # Cataluña
        rep("Galicia", 4),                   # Galicia
        rep("Aragon", 3),                    # Aragón
        "La Rioja",                          # La Rioja
        "Murcia",                            # Murcia 
        "Navarre",                           # Navarra
        "Asturias",                          # Asturias
        rep("Canary Islands", 2),            # Canarias
        "Cantabria",                         # Cantabria
        rep("Ceuta and Melilla", 2)          # Ceuta y Melilla
    )
)

# Load atlas data
atlas_all <- merge(
    setDT(ineAtlas::get_atlas("income", "tract")),
    setDT(ineAtlas::get_atlas("demographics", "tract"))
) %>%
    as_tibble() %>%
    filter(year == 2022)

gini <- setDT(ineAtlas::get_atlas("gini_p80p20", "tract")) %>%
    as_tibble() %>%
    filter(year == 2022) %>%
    select(tract_code, gini, p80p20)

atlas_all <- atlas_all %>%
    left_join(gini, by = "tract_code")

# ------------------------------- Prepare tract moments -------------------------------

# Using median to get mu_log; Gini -> sigma; within_var = sigma^2
atlas_all <- atlas_all %>%
    filter(!is.na(median_income_equiv),
           !is.na(population),
           !is.na(gini)) %>%
    mutate(
        # For a lognormal, G = 2*Phi(sigma/sqrt(2)) - 1  =>  sigma = sqrt(2)*qnorm((G+1)/2)
        sigma       = sqrt(2) * qnorm((gini/100 + 1)/2),
        within_var  = sigma^2,
        # Median = exp(mu) → mu = log(median)
        mu_log      = log(median_income_equiv)
    )

# ------------------------------- National-level Decomposition -------------------------------

national_decomp <- atlas_all %>%
    # Add CCAA information
    left_join(ccaa_mapping, by = "prov_code") %>%
    # Calculate means at each level with population weights
    group_by(prov_code) %>%
    mutate(prov_mean = weighted.mean(mu_log, population)) %>%
    ungroup() %>%
    group_by(ccaa_name) %>%
    mutate(ccaa_mean = weighted.mean(mu_log, population)) %>%
    ungroup() %>%
    group_by(mun_code) %>%
    mutate(mun_mean = weighted.mean(mu_log, population)) %>%
    ungroup() %>%
    # Summarise for national decomposition
    summarise(
        # Print diagnostics
        n = n(),
        mean_mu_log = weighted.mean(mu_log, population),
        mean_within_var = weighted.mean(within_var, population),
        
        # total between (Var[E[log Y | tract]])
        between_total = weighted.var(mu_log, population),
        
        # Between communities variance 
        between_ccaa = weighted.var(ccaa_mean, population),
        
        # Between provinces variance (within Autonomous Communities)
        between_prov = weighted.var(prov_mean, population) - between_ccaa,

        # Between municipalities (within provinces)
        between_mun = weighted.var(mun_mean, population) - weighted.var(prov_mean, population),
        
        # Between tracts (within municipalities)
        between_tract = between_total - weighted.var(mun_mean, population),
        
        # Within tract
        within_tract = weighted.mean(within_var, population),
        
        # total variance
        total_var = within_tract + between_total,
        
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