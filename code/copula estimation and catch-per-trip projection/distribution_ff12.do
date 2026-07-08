
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////This file creates the datasets ffdays12_areaXX_20XX.xlsx and normalized_ssb.xlsx////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

cd "[enter data directory]"

/*Creating the dataset with number of days fished in the last 12 months*/
local years 2019 2020 2021
local areas 1 2
local newmodes 2 3

foreach year of local years {
    foreach area of local areas {
        foreach newmode of local newmodes {
            use "Cod_Haddock_catch_Regression_Tobit2_results_April4_2025.dta", clear
            keep if year == `year' & area == `area' & newmode == `newmode'
            keep ffdays12
            drop if mi(ffdays12)
            export excel using "ffdays12_area`area'`newmode'_`year'.xlsx", firstrow(variables) replace
        }
    }
}

/*Creating the dataset with the normalized ssb for cod and haddock for 2019-2021, which also includes the mean bottom temperature*/
use "Cod_Haddock_catch_Regression_Tobit2_results_April4_2025.dta", clear

keep if year >= 2019
keep year ssb_cod ssb_haddock mean_tmp

duplicates drop
export excel using "normalized_ssb.xlsx", firstrow(variables) replace