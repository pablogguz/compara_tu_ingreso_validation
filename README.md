# Methodology note for [comparatuingreso.es](https://comparatuingreso.es/)

> [!NOTE]  
> [🇪🇸] La nota metodológica está disponible en inglés para facilitar la colaboración con otros investigadores y la reutilización de los scripts. ¡Gracias por tu interés!

<!-- [🇪🇸] Este repositorio contiene los scripts y documentación metodológica utilizados para validar las estimaciones de distribución de ingresos presentadas en [comparatuingreso.es](https://comparatuingreso.es/). El objetivo es garantizar la transparencia y rigor metodológico en la estimación de la posición relativa en la distribución de ingresos de España.  -->

This repository contains the scripts and methodological documentation used to validate the income distribution estimates presented at [comparatuingreso.es](https://comparatuingreso.es/), a publicly available web platform that enables Spanish households to calculate their relative position within the income distribution.

## Methodology overview

The methodology combines several statistical approaches to estimate income distributions at various geographical levels:

1. **Data sources**
   - _Atlas de Distribución de Renta de los Hogares_ ([ADRH](https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177088&menu=ultiDatos&idp=1254735976608)) from the Spanish Statistical Office at the municipality and census tract levels
   - [Census 2021](https://www.ine.es/censos2021/) data at the municipality and census tract levels

2. **Core assumptions**
   - Log-normal distribution of income within census tracts
   - ML modeling (XGBoost) for missing Gini coefficients using socio-demographic variables as predictors
   - Population-weighted mixture of log-normal distributions for aggregating across geographies

## Code structure

| File name | Description | Input data required | Output |
|-----------|-------------|---------------------|---------|
| `0a. calculate_lognormal.r` | Example of how to calculate log-normal mixture, save `.dta` file for summary stats and validation | ADRH tract data | `data-raw/atlas_all.dta` |
| `0b. summary_stats.do` | Summary statistics | ADRH data | `output/summary_stats.tex` |
| `0c. validation.do` | Validation metrics | ADRH data | `output/binned_scatter_p80p20.png`, `output/binned_scatter_median.png` |
| `1. predict_gini_ml.r` | Implements ML model to predict missing Gini coefficients | ADRH tract data, demographic variables | `gini_predicted.fst` |
| `2. prep_lognormal.r` | Prepares income distributions using log-normal mixture | ADRH data, predicted Gini | National and provincial distributions |
| `3a. mun_stats.r` | Calculates municipal statistics | ADRH municipal data, Census data | Municipal summary statistics |
| `3b. tract_stats.r` | Processes tract-level statistics | ADRH tract data, Census tract data | Tract-level statistics |
| `4a. variance_decomp.r` | Calculates hierarchical variance decomposition | ADRH data | `output/variance_decomp.png` |
| `4b. variance_decomp_all.r` | Calculates hierarchical variance decomposition (national level) | ADRH data | Figures for the text in the methodological note |
| `5. mixture_vs_national.r` | Compares mixture vs. national log-normal | ADRH data | `tract_vs_national_income_distribution` |

All necessary packages will be installed automatically when running the R scripts. For `ineAtlas`, you will need to install the development version from GitHub:

```r
pak::pak("pablogguz/ineAtlas")
```

The do-files require the `estout` and `binsreg` packages. If you don't have them installed, you can do so by running:

```stata
ssc install estout 
ssc install binsreg
```

A full methodology note is available [here](./tex/note.pdf).