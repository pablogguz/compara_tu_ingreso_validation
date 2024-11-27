
/*-----------------------------------------------------------------------------
Project: validation metrics for www.comparatuingreso.es
Autor: Pablo Garcia Guzman
This do-file: generates summary stats
------------------------------------------------------------------------------*/

**# Set paths
	global root "C:\Users\\`c(username)'\Documents\GitHub\compara_tu_ingreso_validation\" 

	global output  "$root\output\"
    global raw     "$root\data-raw\"

	clear all
	set maxvar 120000

*--------------------------------------------------------------------------------

**# Calculate utilities for LiTS IV SSA
    use "$raw/atlas_all.dta", clear

    merge 1:1 tract_code using "$raw\expected.dta", nogen

    lab var expected_p80p20 "Expected P80/P20 ratio from log-normality"
    lab var expected_median "Expected median from log-normality"

    lab var median_income_equiv "Observed median income per equivalent adult"
    lab var p80p20 "Observed P80/P20 ratio"

**# Calculate mean relative differences 
    su rel_diff_median rel_diff_p8p20 [aw=population], d
    
**# Regression and chart for median income
    reghdfe expected_median median_income_equiv  [aw=population], noabsorb vce(cl prov_code)

    // Extract R² and beta
    local r2 = e(r2)
    local beta = _b[median_income_equiv]

    // Generate binsreg chart
    binsreg expected_median median_income_equiv [aw=population], ///
        xtitle("`: var lab median_income_equiv'") ///
        ytitle("`: var lab expected_median'") polyreg(1) ///
        ttext(40000 10000 "R² = `: display %6.3f `r2''") ///
        ttext(38000 10000 "β = `: display %6.3f `beta''") ///
        bycolors(midblue%50) polyregplotopt(lcolor(midblue%50) lwidth(0.7)) ///
        dotsplotopt(msize(2))
    graph export "$output/binned_scatter_median.png", width(2000) replace

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
        bycolors(midblue%50) polyregplotopt(lcolor(midblue%50) lwidth(0.7)) ///
        dotsplotopt(msize(2))
    graph export "$output/binned_scatter_p80p20.png", width(2000) replace



