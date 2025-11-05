#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comparatuingreso.es
#* This script: variance decomposition (log income)
#-------------------------------------------------------------

packages_to_load <- c(
  "tidyverse","data.table","ineAtlas","extrafont","haven"
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

dir.create("output", showWarnings = FALSE, recursive = TRUE)

# ------------------------------- Helpers -------------------------------

# Population-weighted (population) variance (no Bessel correction)
weighted.var <- function(x, w) {
  mu <- weighted.mean(x, w)
  sum(w * (x - mu)^2) / sum(w)
}

# ------------------------------- Load data -------------------------------

# Base tract-level data
atlas_all <- merge(
  setDT(ineAtlas::get_atlas("income", "tract")),
  setDT(ineAtlas::get_atlas("demographics", "tract"))
) %>%
  as_tibble() %>%
  filter(year == 2023)

# Gini / P80P20 (same year)
gini_tbl <- ineAtlas::get_atlas("gini_p80p20", "tract") %>%
  as_tibble() %>%
  filter(year == 2023) %>%
  select(tract_code, gini, p80p20)

atlas_all <- atlas_all %>%
  left_join(gini_tbl, by = "tract_code")

# ------------------------------- Mapping: province -> CCAA -------------------------------

ccaa_mapping <- tibble(
  prov_code = c(
    "01","20","48",                          # País Vasco
    "02","13","16","19","45",                # Castilla-La Mancha
    "28",                                    # Madrid
    "03","12","46",                          # C. Valenciana
    "04","11","14","18","21","23","29","41",# Andalucía (14, not 16)
    "05","09","24","34","37","40","42","47","49", # Castilla y León
    "06","10",                               # Extremadura
    "07",                                    # Baleares
    "08","17","25","43",                     # Cataluña
    "15","27","32","36",                     # Galicia
    "22","44","50",                          # Aragón
    "26",                                    # La Rioja
    "30",                                    # Murcia
    "31",                                    # Navarra
    "33",                                    # Asturias
    "35","38",                               # Canarias
    "39",                                    # Cantabria
    "51","52"                                # Ceuta y Melilla
  ),
  ccaa_name = c(
    rep("Basque Country", 3),
    rep("Castile-La Mancha", 5),
    "Madrid",
    rep("Valencian Community", 3),
    rep("Andalusia", 8),
    rep("Castile and Leon", 9),
    rep("Extremadura", 2),
    "Balearic Islands",
    rep("Catalonia", 4),
    rep("Galicia", 4),
    rep("Aragon", 3),
    "La Rioja",
    "Murcia",
    "Navarre",
    "Asturias",
    rep("Canary Islands", 2),
    "Cantabria",
    rep("Ceuta and Melilla", 2)
  )
)

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

# ------------------------------- Decomposition -------------------------------

variance_decomp <- atlas_all %>%
  left_join(ccaa_mapping, by = "prov_code") %>%
  # level means of mu_log with population weights
  group_by(prov_code) %>%
  mutate(prov_mean = weighted.mean(mu_log, population)) %>%
  ungroup() %>%
  group_by(mun_code) %>%
  mutate(mun_mean  = weighted.mean(mu_log, population)) %>%
  ungroup() %>%
  group_by(ccaa_name) %>%
  summarise(
    # diagnostics (weighted)
    n_trac            = n(),
    mean_mu_log       = weighted.mean(mu_log, population),
    mean_within_var   = weighted.mean(within_var, population),

    # total between (Var[E[log Y | tract]])
    between_total     = weighted.var(mu_log, population),

    # hierarchical split of between_total
    between_prov      = weighted.var(prov_mean, population),
    between_mun       = weighted.var(mun_mean, population) - between_prov,
    between_tract     = between_total - weighted.var(mun_mean, population),

    # within (E[Var(log Y | tract)])
    within_tract      = weighted.mean(within_var, population),

    # total variance
    total_var         = within_tract + between_total,

    # pop for ordering
    population        = sum(population),
    .groups = "drop"
  ) %>%
  mutate(
    across(c(between_prov, between_mun, between_tract, within_tract),
           ~ . / total_var * 100, .names = "{.col}_share"),
    total_share = between_prov_share + between_mun_share + 
                  between_tract_share + within_tract_share
  ) %>%
  arrange(desc(population))

# ------------------------------- Outputs -------------------------------

print(variance_decomp)

print(
  variance_decomp %>%
    select(ccaa_name, ends_with("share")) %>%
    mutate(across(where(is.numeric), ~round(., 1)))
)

# Plot
plot_data <- variance_decomp %>%
  select(ccaa_name, ends_with("share"), population) %>%
  pivot_longer(
    cols = ends_with("share"),
    names_to = "component",
    values_to = "share"
  ) %>%
  mutate(
    component = factor(
      component,
      levels = c("within_tract_share", "between_tract_share",
                 "between_mun_share", "between_prov_share"),
      labels = c("Within tract",
                 "Between tracts\n(within municipality)",
                 "Between municipalities\n(within province)",
                 "Between provinces")
    )
  ) %>%
  drop_na() %>%
  # remove ceuta and melilla
  filter(ccaa_name != "Ceuta and Melilla")

within_tract_order <- variance_decomp %>%
  arrange(desc(within_tract_share)) %>%
  pull(ccaa_name)

p <- ggplot(plot_data,
            aes(x = factor(ccaa_name, levels = within_tract_order),
                y = share, fill = component)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(x = "", y = "Per cent of total variance", fill = "") +
  theme_minimal(base_family = "Open Sans") +
  theme(
    text = element_text(size = 16, color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(size = 12, color = "black"),
    legend.position = "top",
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.margin = margin(t = 0, r = 10, b = 10, l = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggsave("output/variance_decomp_2023.png", p, bg = "white", width = 10, height = 6)

# ------------------------------- Sanity checks -------------------------------

# Identity: components sum to total variance (≈0 numeric noise)
print(
  variance_decomp %>%
    transmute(ccaa_name,
              identity_check = (within_tract + between_tract + between_mun + between_prov) - total_var)
)

# Shares sum to ~100
print(
  variance_decomp %>%
    select(ccaa_name, total_share)
)
