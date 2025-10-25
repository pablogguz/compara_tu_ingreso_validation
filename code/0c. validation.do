/*-----------------------------------------------------------------------------
Project: validation metrics for www.comparatuingreso.es
Autor: Pablo Garcia Guzman
This do-file: generates validation plots
------------------------------------------------------------------------------*/

**# Set paths
	global root "C:\Users\\`c(username)'\Documents\GitHub\compara_tu_ingreso_validation\" 

	global output  "$root\output\"
    global raw     "$root\data-raw\"

	clear all
	set maxvar 120000
    cap set scheme theme_pablogguz 

*--------------------------------------------------------------------------------

**# Load data
    use "$raw/atlas_all.dta", clear

    merge 1:1 tract_code using "$raw\expected.dta", nogen

    lab var expected_p80p20 "Expected P80/P20 ratio from log-normality"
    lab var expected_mean "Expected mean from log-normality"

    lab var net_income_equiv "Observed mean income per equivalent adult"
    lab var median_income_equiv "Observed median income per equivalent adult"
    lab var p80p20 "Observed P80/P20 ratio"

**# Calculate mean relative differences
    su rel_diff_mean rel_diff_p8p20 [aw=population], d

**# Regression and chart for mean income
    reghdfe expected_mean net_income_equiv  [aw=population], noabsorb vce(cl prov_code)

    // Extract R² and beta
    local r2 = e(r2)
    local beta = _b[net_income_equiv]

    // Generate binsreg chart
    binsreg expected_mean net_income_equiv [aw=population], ///
        xtitle("`: var lab net_income_equiv'") ///
        ytitle("`: var lab expected_mean'") polyreg(1) ///
        ttext(38000 15000 "R² = `: display %6.3f `r2''") ///
        ttext(36000 15000 "β = `: display %6.3f `beta''") ///
        bycolors(midblue%50) polyregplotopt(lcolor(midblue%50) lwidth(0.5)) ///
        dotsplotopt(msize(1)) ///
        xlab(, format(%12.0gc)) ylab(, format(%12.0gc))
    graph export "$output/binned_scatter_mean_2023.png", width(2000) replace

**# Regression and chart for P80/P20
    reghdfe expected_p80p20 p80p20 [aw=population], noabsorb vce(cl prov_code)

    // Extract R² and beta
    local r2 = e(r2)
    local beta = _b[p80p20]

    // Generate binsreg chart
    binsreg expected_p80p20 p80p20 [aw=population], ///
        xtitle("`: var lab p80p20'") ///
        ytitle("`: var lab expected_p80p20'") polyreg(1) ///
        ttext(3.4 2.2 "R² = `: display %6.3f `r2''") ///
        ttext(3.3 2.2 "β = `: display %6.3f `beta''") ///
        bycolors(midblue%50) polyregplotopt(lcolor(midblue%50) lwidth(0.5)) ///
        dotsplotopt(msize(1))
    graph export "$output/binned_scatter_p80p20_2023.png", width(2000) replace



