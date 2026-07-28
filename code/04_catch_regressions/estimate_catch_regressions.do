
/////////////////////////////////////////// Preparing Data for Copula Estimation ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

version 17
clear all
set more off

global project_root "`c(pwd)'"

global processed_temp ///
    "${project_root}/data/processed/bottom_temperature"

global processed_mrip ///
    "${project_root}/data/processed/historical_mrip"

global stock_inputs ///
    "${project_root}/data/raw_public/stock_assessments"

global catch_reg_outputs ///
    "${project_root}/data/processed/catch_regressions"

global work_step04 ///
    "${project_root}/work/04_catch_regressions"

capture mkdir "${project_root}/data/processed/catch_regressions"
capture mkdir "${project_root}/work/04_catch_regressions"






*************************************Loading Data
*********************************************************************************************************


import excel using "${processed_temp}/hist_daily_bt_tmps_agg_gom.xlsx",  clear firstrow

gen month = month(date)
gen year = year(date)
collapse (mean) mean_tmp, by(month year)
tempfile monthly_bottom_temperature
save `monthly_bottom_temperature', replace


*WGOM Cod stock data
import excel using   "${stock_inputs}/SSB_WGOM_cod_2023_RT_sel3ahl_Run.xlsx",  firstrow case(lower) clear

rename ssb WGOM_ssb
tempfile WGOM_cod_stock
save `WGOM_cod_stock', replace

clear

use   "${processed_mrip}/cod_hadd_catch_data.dta", clear

merge m:1 year using "${stock_inputs}/stock_cod_haddock.dta"
drop if _merge == 2
drop _merge
destring month, replace
merge m:1 year month using `monthly_bottom_temperature'
drop if _merge ==2
drop _merge

merge m:1 year using `WGOM_cod_stock'
drop if _merge == 2

gen ssb_cod11 = WGOM_ssb
destring area_x, replace

encode mode1, generate(mode)
drop if year <2004
drop if year == 2022

*Area 1 = state
*Area 2 = federal
*Area 5 = inland
*Recoding inland as state waters
replace area_x= 1 if area_x==5

rename (ssb_haddock) (ssb_haddock1)

sum ssb_cod11
	gen ssb_cod=ssb_cod11/r(mean)

sum ssb_haddock1
	gen ssb_haddock=ssb_haddock1/r(mean)
	
sum tot_cat_cod
	gen scale_cat_cod = tot_cat_cod/r(mean)
	
sum tot_cat_had
	gen scale_cat_had = tot_cat_had/r(mean)
	
sum mean_tmp
gen bt_tmp = mean_tmp-r(mean)
	rename mean_tmp orig_bt 

*Addressing sparse data issues

encode state, gen(state1)
gen covid2019 = 0
replace covid2019 = 1 if year> 2018 & year !=.

gen season = month
replace season = 4 if season == 3
replace season = 11 if season == 12
			
gen newmode=mode

*Combining Charter & Headboats into a single mode
replace newmode=2 if newmode==1 
		
set seed 9875546

*Although it is common to model this type of data using a zero inflated negative binomial or poisson model, we employ a type II Tobit estimator due to the bimodal nature of the data

foreach w in "cod" "haddock" {
 if "`w'" == "cod" {
nehurdle tot_cat_cod c.ssb_cod bt_tmp c.bt_tmp#c.bt_tmp c.bt_tmp#c.ssb_cod c.ssb_cod#c.ssb_cod ssb_haddock c.ssb_cod#c.ssb_haddock ib7.season c.ssb_cod#ib7.season ib7.season#c.bt_tmp  ib(freq).state1 c.ffdays12#ib7.season c.ffdays12 covid2019 covid2019#ib7.season covid2019#c.ssb_cod if area == 1 & newmode == 3, heckman expon difficult
 }
			else {
nehurdle tot_cat_hadd ssb_haddock bt_tmp c.bt_tmp#c.bt_tmp c.bt_tmp#c.ssb_haddock c.ssb_haddock#c.ssb_haddock ssb_cod c.ssb_cod#c.ssb_haddock ib7.season c.ssb_haddock#ib7.season ib7.season#c.bt_tmp  ib(freq).state1 c.ffdays12#ib7.season c.ffdays12 covid2019 covid2019#c.ffdays12 covid2019#c.ssb_haddock if area == 1 & newmode == 3, heckman expon difficult
			}
estimates "${work_step04}save `w'13", replace
estimates store `w'13
		predict `w'13 if e(sample), ystar
					predict `w'13_resid if e(sample),  resstar
					predict `w'13_nonzero_prob if e(sample), psel
					gen `w'13_zero_prob =1-`w'13_nonzero_prob if e(sample)	
putexcel set "${work_step04}/parameters_area1newmode3`w'.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "${work_step04}/sigma_area1newmode3`w'.xlsx", replace	
    putexcel A1 = matrix(e(V)), colnames
estat ic
preserve
	drop if `w'13_resid == . 
export delimited `w'13_resid using "${catch_reg_outputs}/residuals_area1newmode3_`w'.txt", replace
restore
}
suest cod13 haddock13
putexcel set "${catch_reg_outputs}/parameters_SUEST_area1newmode3.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "${catch_reg_outputs}/sigma_SUEST_area1newmode3.xlsx", replace
	putexcel A1 = matrix(e(V)), colnames		
	
foreach w in "cod" "haddock" {
 if "`w'" == "cod" {
nehurdle tot_cat_cod c.ssb_cod bt_tmp c.bt_tmp#c.bt_tmp c.bt_tmp#c.ssb_cod c.ssb_cod#c.ssb_cod ssb_haddock c.ssb_cod#c.ssb_haddock ib7.season  ib(freq).state1 ib(freq).state1#ib7.season c.ffdays12 covid2019  covid2019#c.ssb_cod  if area == 1 & newmode == 2, heckman expon difficult
 }
			else {
nehurdle tot_cat_hadd c.ssb_haddock bt_tmp c.bt_tmp#c.bt_tmp c.bt_tmp#c.ssb_haddock c.ssb_haddock#c.ssb_haddock ssb_cod c.ssb_cod#c.ssb_haddock ib7.season  ib(freq).state1 c.ffdays12  covid2019  covid2019#c.ssb_haddock if area == 1 & newmode == 2, heckman expon difficult
			}
estimates save "${work_step04}/`w'12", replace
estimates store `w'12
		predict `w'12 if e(sample), ystar
					predict `w'12_resid if e(sample),  resstar
					predict `w'12_nonzero_prob if e(sample), psel
					gen `w'12_zero_prob =1-`w'12_nonzero_prob if e(sample)	
putexcel set "${work_step04}/parameters_area1newmode2`w'.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "${work_step04}/sigma_area1newmode2`w'.xlsx", replace	
    putexcel A1 = matrix(e(V)), colnames
estat ic
preserve
	drop if `w'12_resid == . 
export delimited `w'12_resid using "${catch_reg_outputs}/residuals_area1newmode2_`w'.txt", replace
restore
}
suest cod12 haddock12
putexcel set "${catch_reg_outputs}/parameters_SUEST_area1newmode2.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "${catch_reg_outputs}/sigma_SUEST_area1newmode2.xlsx", replace
	putexcel A1 = matrix(e(V)), colnames		

foreach w in "cod" "haddock" {
 if "`w'" == "cod" {
nehurdle tot_cat_cod c.ssb_cod bt_tmp c.bt_tmp#c.bt_tmp c.bt_tmp#c.ssb_cod c.ssb_cod#c.ssb_cod ssb_haddock c.ssb_cod#c.ssb_haddock ib7.season c.ssb_cod#ib7.season ib7.season#c.bt_tmp c.ssb_haddock#ib7.season ib(freq).state1 ib(freq).state1#ib7.season c.ffdays12#ib7.season c.ffdays12 ib(freq).newmode b(freq).newmode#c.ssb_cod covid2019  covid2019#c.ssb_cod  if area == 2 , heckman expon difficult
 }
			else {
nehurdle tot_cat_hadd c.ssb_haddock bt_tmp c.bt_tmp#c.bt_tmp c.bt_tmp#c.ssb_haddock c.ssb_haddock#c.ssb_haddock ssb_cod c.ssb_cod#c.ssb_haddock ib7.season c.ssb_haddock#ib7.season ib7.season#c.bt_tmp c.ssb_cod#ib7.season ib(freq).state1 ib(freq).state1#ib7.season c.ffdays12#ib7.season c.ffdays12 ib(freq).newmode ib(freq).newmode#c.ssb_haddock covid2019  covid2019#c.ssb_haddock   if area == 2 , heckman expon difficult
			}
estimates save "${work_step04}/`w'212", replace
estimates store `w'212
		predict `w'212 if e(sample), ystar
					predict `w'212_resid if e(sample),  resstar
					predict `w'212_nonzero_prob if e(sample), psel
					gen `w'212_zero_prob =1-`w'212_nonzero_prob if e(sample)	
putexcel set "${work_step04}/parameters_area2`w'.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "${work_step04}/sigma_area2`w'.xlsx", replace	
    putexcel A1 = matrix(e(V)), colnames
estat ic
preserve
	drop if `w'212_resid == . 
export delimited `w'212_resid using "${work_step04}/residuals_area2_`w'.txt", replace
restore
}
suest cod212 haddock212
	putexcel set "${catch_reg_outputs}/parameters_SUEST_area2.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "${catch_reg_outputs}/sigma_SUEST_area2.xlsx", replace
	putexcel A1 = matrix(e(V)), colnames	
	
	drop MA* ME* NH*
	drop predMAcod11 - _est_MEhaddock_Jan24
save "${catch_reg_outputs}/cod_haddock_catch_regression_tobit2_results.dta", replace

*For ease of coding the copulas, each mode was ultimately saved in a separate file
preserve
	drop if cod212_resid == . | newmode==2
export delimited cod212_resid using "${catch_reg_outputs}/residuals_area2newmode3_cod.txt", replace
export delimited haddock212_resid using "${catch_reg_outputs}/residuals_area2newmode3_haddock.txt", replace
restore
preserve
	drop if cod212_resid == .  | newmode==3
export delimited cod212_resid using "${catch_reg_outputs}/residuals_area2newmode2_cod.txt", replace
export delimited haddock212_resid using "${catch_reg_outputs}/residuals_area2newmode2_haddock.txt", replace
restore

					