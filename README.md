# Validation for [comparatuingreso.es](https://comparatuingreso.es/)

[🇪🇸] Este repositorio contiene los scripts y métricas de validación utilizados para verificar la metodología detrás de la aplicación Compara Tu Ingreso. El propósito es asegurar la precisión y consistencia de los cálculos que sustentan las estimaciones de ingreso y desigualdad.

[🇬🇧] This repository contains the validation scripts and metrics used to verify the methodology behind the Compara Tu Ingreso application. The purpose is to ensure the accuracy and consistency of the calculations underpinning the income and inequality estimates.

## File Descriptions

| **File Name**              | **Description**                                                                                           |
|-----------------------------|-----------------------------------------------------------------------------------------------------------|
| `0. calculate_lognormal.r` | Contains functions to calculate lognormal distributions for income data.                                  |
| `1. predict_gini_ml.r`     | Implements machine learning models to predict Gini coefficients based on input features.                  |
| `2. prep_lognormal.r`      | Prepares income data for further analysis by applying transformations based on lognormal assumptions.     |
| `3a. mun_stats.r`          | Generates municipal-level summary statistics for validation and comparison purposes.                      |
| `3b. tract_stats.r`        | Generates tract-level summary statistics for validation and comparison purposes.                          |
| `_create_comparison_plot.r`| Creates visualizations to compare predicted and actual metrics, aiding in validation.                     |
| `README.md`                | Documentation file describing the repository structure, purpose, and usage.                              |
