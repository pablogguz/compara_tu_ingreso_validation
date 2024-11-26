
/*-----------------------------------------------------------------------------
Project: validation metrics for www.comaparatuingreso.es
Autor: Pablo Garcia Guzman
This do-file: within-between decomposition
------------------------------------------------------------------------------*/

**# Set paths
	global root "C:\Users\\`c(username)'\Documents\GitHub\compara_tu_ingreso_validation\" 

	global output  "$root\output\"
    global raw     "$root\data-raw\"

	clear all
	set maxvar 120000

*--------------------------------------------------------------------------------

**# Load data
    use "$raw/atlas_all.dta", clear

    lab var net_income_pc "Net income per capita"
    lab var net_income_equiv "Net income per equivalent adult"
    lab var mean_age "Mean age"
    lab var gini "Gini"
    lab var pct_single_hh "Single-person households (\%)"
    lab var population "Population"

    // Calculate sigma
    gen sigma = sqrt(2) * invnorm((gini / 100 + 1) / 2)
    gen within_var = sigma^2

    // Calculate mu
    gen mu = log(net_income_equiv) - (sigma^2 / 2)

**# Mean income and between-tract variance
    g log_income = log(net_income_equiv)
    su log_income [aw=population], d
    local overall_mean = r(mean)
    local between_var = r(Var)

**# Within-tract variance and total 
    su within_var [aw=population], d
    local within_var_total = r(mean)
    local total_var = `within_var_total' + `between_var'

*--------------------------------------------------------------------------------
**# Display results
    local within_share = `within_var_total' / `total_var'
    local between_share = `between_var' / `total_var'
    di "Variance decomposition:"
    di "Total variance:        " %8.4f `total_var'
    di "Between-tract variance:" %8.4f `between_var'
    di "Within-tract variance: " %8.4f `within_var_total'
    di "Between-tract share:   " %5.4f `between_share'
    di "Within-tract share:    " %5.4f `within_share'