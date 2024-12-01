
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

**# Load data
    use "$raw/atlas_all.dta", clear

**# Check: municipalities with just 1 tract
    bys mun_code: g n_tracts = _N 
    preserve 
        gcollapse (sum) population (max) n_tracts, by(mun_code prov_name)
        su n_tracts 
        tab n_tracts 
        
        gcollapse (sum) population, by(n_tracts)
        gegen total_pop = sum(population)
        gen pct = population/total_pop
        sum pct if n_tracts == 1
        di "Pop. share in mun. with just 1 tract: `r(mean)'*100"
    restore 

**# Prepare variables 
    g dependency_ratio = (pct_under18 + pct_over65)/(100 - pct_under18 - pct_over65)

    lab var net_income_pc "Net income per capita"
    lab var net_income_equiv "Net income per equivalent adult"
    lab var mean_age "Mean age"
    lab var gini "Gini"
    lab var pct_single_hh "Single-person households (\%)"
    lab var dependency_ratio "Dependency ratio"
    lab var population "Population"

**# Code for table
    cap file close fh
	file open fh using "${output}\summary_stats.tex", write replace
		file write fh _n  "\begin{tabular}{@{}lccccc@{}}"
		file write fh _n "\toprule" 
		file write fh _n  "" " " " & " "(1)" " & " "(2)" " & " "(3)" " & " " (4)" " & " " (5) " "\\"
		file write fh _n  "" " " "& " "Min" " & " "Max" " & " "Mean" " & " "SD" " & " "\% missing" "\\"
		file write fh _n "\midrule" 
		
        file write fh _n  "\qquad \textit{Income distribution} \\" 

            foreach var in net_income_pc net_income_equiv gini {
                su `var', d
                local mean = r(mean)
                local sd = r(sd)
                local min = r(min)
                local max = r(max)

                count if `var' == .
                local miss = 100*r(N)/_N 
                di `miss'

                file write fh _n `"`: var label `var''"'  " & " %9.2fc (`min') " & " %9.2fc (`max') " & " %9.2fc (`mean') " & " %9.2fc (`sd') " & " %9.2fc (`miss')  "\\"
            }

        file write fh _n  "\qquad \textit{Demographics} \\" 

            foreach var in population dependency_ratio mean_age pct_single_hh {
                su `var', d
                local mean = r(mean)
                local sd = r(sd)
                local min = r(min)
                local max = r(max)

                count if `var' == .
                local miss = 100*r(N)/_N 
                di `miss'

                file write fh _n `"`: var label `var''"'  " & " %9.2fc (`min') " & " %9.2fc (`max') " & " %9.2fc (`mean') " & " %9.2fc (`sd') " & " %9.2fc (`miss')  "\\"
        }

        distinct tract_code

         file write fh _n "\midrule"
        file write fh _n  "No. of tracts"    " & " %15.0fc (`r(ndistinct)')  " \\"
        file write fh _n "\midrule" 

        file write fh _n "\end{tabular}"
        file close fh
        type "${output}\summary_stats.tex"
        
        
        