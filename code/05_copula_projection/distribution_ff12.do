
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////This file creates the datasets ffdays12_areaXX_20XX.xlsx and normalized_ssb.xlsx////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

version 17
clear all
set more off

global project_root "`c(pwd)'"

global catch_reg_outputs ///
    "${project_root}/data/processed/catch_regressions"

global copula_inputs ///
    "${project_root}/data/processed/copula_projection_inputs"

capture mkdir ///
    "${project_root}/data/processed/copula_projection_inputs"

/*Creating the dataset with number of days fished in the last 12 months*/
local years 2019 2020 2021
local areas 1 2
local newmodes 2 3

foreach year of local years {
    foreach area of local areas {
        foreach newmode of local newmodes {
		use  "${catch_reg_outputs}/cod_haddock_catch_regression_tobit2_results.dta", clear  
		keep if year == `year' & area == `area' & newmode == `newmode'
        keep ffdays12
        drop if mi(ffdays12)
		export excel using "${copula_inputs}/ffdays12_area`area'`newmode'_`year'.xlsx",  firstrow(variables) replace     
		}
    }
}

/*Creating the dataset with the normalized ssb for cod and haddock for 2019-2021, which also includes the mean bottom temperature*/
use  "${catch_reg_outputs}/cod_haddock_catch_regression_tobit2_results.dta", clear  

keep if year >= 2019
keep year ssb_cod ssb_haddock mean_tmp

duplicates drop
export excel using "${copula_inputs}/normalized_ssb.xlsx",  firstrow(variables) replace