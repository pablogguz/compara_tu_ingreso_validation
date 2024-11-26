
#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comaparatuingreso.es
#* This script: variance decomposition
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
        rep("País Vasco", 3),
        rep("Castilla-La Mancha", 5),
        "Madrid",
        rep("C. Valenciana", 3),
        rep("Andalucía", 8),
        rep("Castilla y León", 9),
        rep("Extremadura", 2),
        "Baleares",
        rep("Cataluña", 4),
        rep("Galicia", 4),
        rep("Aragón", 3),
        "La Rioja",
        "Murcia",
        "Navarra",
        "Asturias",
        rep("Canarias", 2),
        "Cantabria",
        rep("Ceuta y Melilla", 2)
    )
)

# Now the decomposition
variance_decomp <- atlas_all %>%
    filter(
        !is.na(net_income_equiv) & 
        !is.na(population)
    ) %>%
    # Add CCAA information
    left_join(ccaa_mapping, by = "prov_code") %>%
    mutate(
        log_income = log(net_income_equiv),
        # Calculate sigma from Gini for within-tract variance
        sigma = sqrt(2) * qnorm((gini/100 + 1)/2),
        within_var = sigma^2
    ) %>%
    # Calculate means at each level first
    group_by(prov_code) %>%
    mutate(prov_mean = weighted.mean(log_income, population)) %>%
    group_by(mun_code) %>%
    mutate(mun_mean = weighted.mean(log_income, population)) %>%
    # Now group by CCAA and calculate variance components
    group_by(ccaa_name) %>%
    summarise(
        # Print diagnostics
        n = n(),
        mean_log_income = mean(log_income),
        mean_within_var = mean(within_var),
        
        # Total variance (now correctly including within-tract component)
        total_var = weighted.var(log_income, population) + weighted.mean(within_var, population),
        
        # Between province variance
        between_prov = weighted.var(prov_mean, population),
            
        # Between municipality (within province)
        between_mun = weighted.var(mun_mean, population) - weighted.var(prov_mean, population),
            
        # Between tract (within municipality)
        between_tract = weighted.var(log_income, population) - weighted.var(mun_mean, population),
                
        # Within tract
        within_tract = weighted.mean(within_var, population),
        
        # Population for sorting
        population = sum(population)
    ) %>%
    # Calculate shares
    mutate(
        across(c(between_prov, between_mun, between_tract, within_tract),
              ~ . / total_var * 100,
              .names = "{.col}_share"),
        total_share = between_prov_share + between_mun_share + 
                     between_tract_share + within_tract_share
    ) %>%
    # Sort by population
    arrange(desc(population))

# Print full results
print(variance_decomp)

# Print results
print(variance_decomp %>%
    select(ccaa_name, ends_with("share")) %>%
    mutate(across(where(is.numeric), round, 1)))

# Create visualization
plot_data <- variance_decomp %>%
    select(ccaa_name, ends_with("share")) %>%
    pivot_longer(
        cols = ends_with("share"),
        names_to = "component",
        values_to = "share"
    ) %>%
    mutate(
        component = factor(component,
            levels = c("within_tract_share", "between_tract_share", 
                      "between_mun_share", "between_prov_share"),
            labels = c("Within tract", "Between tracts\n(within municipality)", 
                      "Between municipalities\n(within province)", "Between provinces"))
    )

# Plot data
plot_data <- plot_data %>%
    drop_na()

p <- ggplot(plot_data, 
      aes(x = reorder(ccaa_name, share), 
          y = share, 
          fill = component)) +
   geom_bar(stat = "identity") +
   coord_flip() +
   scale_fill_brewer(palette = "Blues", direction = -1) +
   labs(
       x = "",
       y = "Share of total variance (%)",
       fill = ""
   ) +
   theme_minimal(
       base_family = "Open Sans"
   ) +
   theme(  
       text = element_text(size = 14),
       legend.text = element_text(size = 10),  
       legend.position = "top",
       legend.justification = c(0, 1),
       legend.background = element_rect(fill = "white", color = NA),
       legend.margin = margin(t = 0, r = 10, b = 10, l = 0),
       panel.grid.major.x = element_blank(),
       panel.grid.minor.x = element_blank()
   )

p 

ggsave(
    "output/variance_decomp.png", 
    p, 
    bg = "white",
    width = 10, 
    height = 6
)

# Stats 
plot_data %>% 
    group_by(component) %>% 
    summarise(
        mean_share = mean(share),
        max_share = max(share),
        min_share = min(share)
    )
