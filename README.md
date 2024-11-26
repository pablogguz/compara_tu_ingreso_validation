# Methodology note for [comparatuingreso.es](https://comparatuingreso.es/)

> [!NOTE]  
> [🇪🇸] La nota metodológica no está disponible en español por dos motivos. En primer lugar, el inglés facilita la colaboración con otros investigadores y la reutilización de los scripts. En segundo lugar, si eres lo suficientemente curioso como para haber llegado hasta aquí, leer en inglés no debería ser un problema. ¡Gracias por tu interés!

<!-- [🇪🇸] Este repositorio contiene los scripts y documentación metodológica utilizados para validar las estimaciones de distribución de ingresos presentadas en [comparatuingreso.es](https://comparatuingreso.es/). El objetivo es garantizar la transparencia y rigor metodológico en la estimación de la posición relativa en la distribución de ingresos de España.  -->

[🇬🇧] This repository contains the scripts and methodological documentation used to validate the income distribution estimates presented at [comparatuingreso.es](https://comparatuingreso.es/). The goal is to ensure transparency and methodological rigor in estimating households' relative position in Spain's income distribution.

## Methodology overview

The methodology combines several statistical approaches to estimate income distributions at various geographical levels:

1. **Data sources**
   - _Atlas de Distribución de Renta de los Hogares_ ([ADRH](https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177088&menu=ultiDatos&idp=1254735976608)) from INE at the municipality and census tract levels
   - [Census 2021](https://www.ine.es/censos2021/) data at the municipality and census tract levels

2. **Core assumptions**
   - Log-normal distribution of income within census tracts
   - OECD-modified equivalence scale for household income adjustment (1 first adult, 0.5 additional adults, 0.3 children)
   - Imputation of missing mean equivalised incomes using provincial ratios between equivalised and per-capita income
   - ML modeling (XGBoost) for missing Gini coefficients using socio-demographic variables as predictors
   - Population-weighted mixture of log-normal distributions for aggregating across geographies

## Code structure

| File name | Description | Input data required | Output |
|-----------|-------------|---------------------|---------|
| `0a. calculate_lognormal.r` | Example of how to calculate log-normal mixture | ADRH tract data | `output/tract_vs_individual_income_distribution.png` |
| `0b. summary_stats.do` | Summary statistics | ADRH data | `output/summary_stats.tex` |
| `0c. validation.do` | Validation metrics | ADRH data | `output/binned_scatter_p80p20.png`, `output/binned_scatter_median.png` |
| `1. predict_gini_ml.r` | Implements ML model to predict missing Gini coefficients | ADRH tract data, demographic variables | `gini_predicted.fst` |
| `2. prep_lognormal.r` | Prepares income distributions using log-normal mixture | ADRH data, predicted Gini | National and provincial distributions |
| `3a. mun_stats.r` | Calculates municipal statistics | ADRH municipal data, Census data | Municipal summary statistics |
| `3b. tract_stats.r` | Processes tract-level statistics | ADRH tract data, Census tract data | Tract-level statistics |
| `4a. variance_decomp.r` | Calculates hierarchical variance decomposition | ADRH data | `output/variance_decomp.png` |
| `4b. variance_decomp.r` | Calculates hierarchical variance decomposition (national level) | ADRH data |  |

