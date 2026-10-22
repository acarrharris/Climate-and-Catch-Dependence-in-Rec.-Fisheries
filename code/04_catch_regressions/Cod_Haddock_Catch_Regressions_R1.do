*Note: This code has been optimized with the help of Claude Opus 5.
* Preparing Data for Copula Estimation 
*WGOM Cod stock data

* Set global directories - user edit here
global dir_root "C:\Users\andrew.carr-harris\Desktop\Git\Climate-and-Catch-Dependence-in-Rec.-Fisheries" // root to Github directory
global code_root "${dir_root}/code" 
global process_data_root "${dir_root}/data" // data directory for all other model input and output files.

cd "${process_data_root}"

*WGOM Cod stock data
import excel "${process_data_root}/Assessment_TimeSeries_Data_Western_GOM_cod_biomass", firstrow clear
save WGOM_Cod_Stock_2024, replace

import excel "${process_data_root}/Assessment_TimeSeries_Data_GOM_haddock_ssb", firstrow clear
save GOM_Haddock_Stock_2024 , replace

import excel  "${process_data_root}/daily_btemp_GoM_by_area_combined_1959_2026", firstrow clear
collapse (mean) mean_tmp_inshore mean_tmp_offshore (sd) sd_tmp_inshore=mean_tmp_inshore  sd_tmp_offshore=mean_tmp_offshore  , by(year month)
save monthly_btemp_GoM_1959_2026, replace
********Function to export data**********************************
version 19
set more off
************************************************************
*   `1' = species/fleet label (e.g. cod12, haddock12, cod212)
*   `2' =  dependent variable (tot_cat_hadd, tot_cat_cod)
************************************************************
capture program drop cpt_post
program define cpt_post
    local s "`1'"
    local y "`2'"

    *log catch linear prediction
    predict double xbv_`s'   if e(sample), xbval

    * Linear prediction for the probability of zero catch estimate: z'gamma 
    predict double xbs_`s'   if e(sample), xbsel

    * Probability of zero catch
    predict double psel_`s'  if e(sample), psel

  *Residual
    quietly gen double u_`s' = ln(`y') - xbv_`s' if `y' > 0 & e(sample)

    * Indicator for positive catch 
    quietly gen byte pos_`s' = (`y' > 0) if e(sample)

    * Positive vs zero catch counts
    quietly count if e(sample)
    local N = r(N)
    quietly count if e(sample) & `y' == 0
    display as text "COUNTS `s': N = " as result `N' ///
            as text ", censored = " as result r(N) ///
            as text ", uncensored = " as result `N' - r(N)
end

capture program drop rhosel_ll
program define rhosel_ll
    version 19
    args lnf atr
    quietly  {
        tempvar sc sh
        gen double `sc' = 2*$ML_y1 - 1        // +1 if cod caught,     -1 if not
        gen double `sh' = 2*$ML_y2 - 1        // +1 if haddock caught, -1 if not
        replace `lnf' = log(max(                                        ///
            binormal( `sc'*$ML_y3 , `sh'*$ML_y4 , `sc'*`sh'*tanh(`atr') ), ///
            1e-300))
		}
end

*************************************Loading Data
*********************************************************************************************************
clear
cd "${process_data_root}"

use "${process_data_root}/cod_hadd_catch_data.dta", clear
			gen byte codhaddgom = 0
			replace codhaddgom = 1 if gom==1 & target_cod_hadd==1 & mode !="sh"
			*drop if mode1=="sh"
merge m:1 year using "${process_data_root}/GOM_Haddock_Stock_2024.dta"
drop if _merge == 2 
drop _merge
destring month, replace
	merge m:1 year month using "${process_data_root}/monthly_btemp_GoM_1959_2026.dta"
		drop if _merge ==2 
		drop _merge	
merge m:1 year using "${process_data_root}/WGOM_Cod_Stock_2024.dta"
drop if _merge == 2 
drop if year >2024
*drop if year<1996

svyset psu_id [pweight=new_wp_int], strata(var_id) singleunit(scaled)
	
gen orig_tmp = mean_tmp_inshore if inlist(area_x,"1","5")
	replace orig_tmp = mean_tmp_offshore if area_x=="2"
	
replace ffdays12 =. if inlist(ffdays12, 999,998)
replace ffdays2 =. if inlist(ffdays2, 999,998)
replace hrsf =. if inlist(hrsf, 99.9,99.8)
	
replace WGOM_ssb = 1942 if year ==2024
replace ssb_haddock = 19946 if year ==2024
destring area_x, replace

encode mode1, generate(mode)
	gen newmode=mode
	*Combining Charter & Headboats into a single mode
		replace newmode=2 if newmode==1 

*Area 1 = state
*Area 2 = federal
*Area 5 = inland
*Recoding inland as state waters
replace area_x= 1 if area_x==5
rename (ssb_haddock) (ssb_haddock1)

svy, subpop(codhaddgom) : mean  WGOM_ssb
	gen mean_ssb_cod= e(b)[1,1]
	gen ssb_cod=WGOM_ssb/mean_ssb_cod

svy, subpop(codhaddgom) : mean ssb_haddock1
	gen mean_ssb_haddock = e(b)[1,1]
	gen ssb_haddock=ssb_haddock1/mean_ssb_haddock

svy, subpop(codhaddgom) : mean orig_tmp
gen mean_tmp = e(b)[1,1]
gen bt_tmp = orig_tmp-mean_tmp 
		
svy, subpop(codhaddgom) : mean hrsf
	gen mean_hrsf = e(b)[1,1]
	gen mhrsf = hrsf/mean_hrsf

encode state, gen(state1)

gen covid2019 = 0
	replace covid2019 = 1 if year> 2019 & year !=.  & year <=2021
	
gen str_break = 0
	replace str_break = 1 if year> 2018 & year !=.
	
*Addressing sparse data issues
gen season = month
		replace season = 4 if season == 3
		replace season = 11 if season == 12

gen byte ffcat = .
	replace ffcat = 0 if ffdays12 == 0
	replace ffcat = 1 if ffdays12 > 0 & ffdays12 <= 5
	replace ffcat = 2 if ffdays12 > 5 & ffdays12 <= 20
	replace ffcat = 3 if ffdays12 > 20 & ffdays12 < .		
	
gen byte sub13= 0
		replace sub13 = 1  if area_x == 1 & mode == 3 & codhaddgom==1
gen byte sub12 = 0
		replace sub12= 1  if area_x == 1 & mode != 3 & codhaddgom==1
gen byte sub212 = 0
		replace sub212 = 1  if area_x == 2 & codhaddgom==1

set seed 9875546
	************************Area 1 mode 3*********************************************************************************************
_pctile bt_tmp [pweight = new_wp_int] if sub13==1,  percentiles(5 35 65 95) 
	scalar k1 = r(r1)
scalar k2 =  r(r2)
scalar k3 =  r(r3)
scalar k4 = r(r4)
mkspline tsp = bt_tmp,  cubic displayknots knots(`=k1' `=k2' `=k3' `=k4')
	matrix knots_13 = r(knots)

foreach w in "cod" "haddock" {
 if "`w'" == "cod" {
svy ,  subpop(sub13): nehurdle  tot_cat_cod c.ssb_haddock ssb_cod c.ssb_haddock#c.ssb_cod tsp*  ib0.ffcat   ib0.covid2019 ib0.str_break ib7.season ib1.state1 ,  heckman expon
 }
			else {
svy ,  subpop(sub13): nehurdle tot_cat_hadd c.ssb_haddock ssb_cod c.ssb_haddock#c.ssb_cod tsp*   ib0.ffcat ib0.covid2019 ib0.str_break ib7.season ib1.state1 ,  heckman expon 
			}
			
estimates save `w'13, replace
estimates store `w'13

  cpt_post `w'13 `=cond("`w'"=="cod","tot_cat_cod","tot_cat_hadd")'
     
putexcel set "parameters_Sept_28_2026_area1newmode3`w'.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "sigma_Sept_28_2026_area1newmode3`w'.xlsx", replace	
    putexcel A1 = matrix(e(V)), colnames
}
 quietly gen byte _pc = pos_cod13
    quietly gen byte _ph = pos_haddock13
   
      ml model lf rhosel_ll (atrho: _pc _ph xbs_cod13 xbs_haddock13 = )  if year >=2019  ///
        ,   svy subpop(sub13)
ml maximize

suest cod13 haddock13, svy
test [cod13_selection]tsp1 [cod13_selection]tsp2 [cod13_selection]tsp3
test [cod13_selection]1.ffcat [cod13_selection]2.ffcat [cod13_selection]3.ffcat
test [cod13_lntot_cat_cod]tsp1 [cod13_lntot_cat_cod]tsp2 [cod13_lntot_cat_cod]tsp3
test [cod13_lntot_cat_cod]1.ffcat [cod13_lntot_cat_cod]2.ffcat [cod13_lntot_cat_cod]3.ffcat
test [cod13_lnsigma]_cons  [cod13_athrho]_cons
test [haddock13_selection]tsp1 [haddock13_selection]tsp2 [haddock13_selection]tsp3
test [haddock13_selection]1.ffcat [haddock13_selection]2.ffcat [haddock13_selection]3.ffcat
test [haddock13_lntot_cat_hadd]tsp1 [haddock13_lntot_cat_hadd]tsp2 [haddock13_lntot_cat_hadd]tsp3
test [haddock13_lntot_cat_hadd]1.ffcat [haddock13_lntot_cat_hadd]2.ffcat [haddock13_lntot_cat_hadd]3.ffcat
test [haddock13_lnsigma]_cons  [haddock13_athrho]_cons

	putexcel set "parameters_SUEST_Sept_28_2026_area1newmode3.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "sigma_SUEST_Sept_28_2026_area1newmode3.xlsx", replace
	putexcel A1 = matrix(e(V)), colnames		
putexcel set "knots_Sept_28_2026_area1newmode3.xlsx", replace	
    putexcel A1 = matrix(knots_12), colnames
	
preserve
    keep if sub13 == 1 & bt_tmp != .
		keep if xbv_cod13 != . | xbv_haddock13 != .
    gen long rowid = _n
    export delimited                                                           ///
        rowid year month season state1 tsp* ffcat  ///
		covid2019 str_break bt_tmp  ssb_cod ssb_haddock  newmode new_wp_int  ///
		psu_id var_id ///
		mean_tmp  mean_ssb_haddock mean_ssb_cod ///
        tot_cat_cod tot_cat_hadd                                               ///
        pos_cod12 pos_haddock12                                              ///
        xbv_cod12 xbv_haddock12                                               ///
        xbs_cod12 xbs_haddock12                                               ///
        psel_cod12 psel_haddock12                                            ///
        u_cod12 u_haddock12                                                    ///
        using "logresiduals_Sept_28_2026_area1newmode3.csv", replace
restore

*****Area 1 newmode 2*****************************************************************************************************
_pctile bt_tmp [pweight = new_wp_int] if sub12==1,  percentiles(5 35 65 95) 
	scalar k1 = r(r1)
scalar k2 =  r(r2)
scalar k3 =  r(r3)
scalar k4 = r(r4)

drop tsp*
mkspline tsp =bt_tmp,   cubic displayknots knots(`=k1' `=k2' `=k3' `=k4' )
	matrix knots_12 = r(knots)

foreach w in "cod" "haddock" {
 if "`w'" == "cod" {
svy ,  subpop(sub12): nehurdle  tot_cat_cod c.ssb_haddock ssb_cod c.ssb_haddock#c.ssb_cod tsp*   ib0.ffcat  ib0.covid2019 ib0.str_break ib7.season ib1.state1 ,  heckman expon
 }
			else {
svy ,  subpop(sub12): nehurdle tot_cat_hadd c.ssb_haddock ssb_cod c.ssb_haddock#c.ssb_cod tsp*  ib0.ffcat  ib0.covid2019 ib0.str_break ib7.season ib1.state1 ,     heckman expon 
			}
			
estimates save `w'12, replace
estimates store `w'12

  cpt_post `w'12 `=cond("`w'"=="cod","tot_cat_cod","tot_cat_hadd")'

putexcel set "parameters_Sept_28_2026_area1newmode2`w'.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "sigma_Sept_28_2026_area1newmode2`w'.xlsx", replace	
    putexcel A1 = matrix(e(V)), colnames
}

 drop _pc _ph
      quietly gen byte _pc = pos_cod12
    quietly gen byte _ph = pos_haddock12
   
      ml model lf rhosel_ll (atrho: _pc _ph xbs_cod12 xbs_haddock12 = ) if year >=2019 ///
        ,   svy subpop(sub12) 
ml maximize

suest cod12 haddock12, svy
test [cod12_selection]tsp1 [cod12_selection]tsp2 [cod12_selection]tsp3
test [cod12_selection]1.ffcat [cod12_selection]2.ffcat [cod12_selection]3.ffcat
test [cod12_lntot_cat_cod]tsp1 [cod12_lntot_cat_cod]tsp2 [cod12_lntot_cat_cod]tsp3
test [cod12_lntot_cat_cod]1.ffcat [cod12_lntot_cat_cod]2.ffcat [cod12_lntot_cat_cod]3.ffcat
test [cod12_lnsigma]_cons  [cod12_athrho]_cons
test [haddock12_selection]tsp1 [haddock12_selection]tsp2 [haddock12_selection]tsp3
test [haddock12_selection]1.ffcat [haddock12_selection]2.ffcat [haddock12_selection]3.ffcat
test [haddock12_lntot_cat_hadd]tsp1 [haddock12_lntot_cat_hadd]tsp2 [haddock12_lntot_cat_hadd]tsp3
test [haddock12_lntot_cat_hadd]1.ffcat [haddock12_lntot_cat_hadd]2.ffcat [haddock12_lntot_cat_hadd]3.ffcat
test [haddock12_lnsigma]_cons  [haddock12_athrho]_cons
	putexcel set "parameters_SUEST_Sept_28_2026_area1newmode2.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "sigma_SUEST_Sept_28_2026_area1newmode2.xlsx", replace
	putexcel A1 = matrix(e(V)), colnames		
putexcel set "knots_Sept_28_2026_area1newmode2.xlsx", replace	
    putexcel A1 = matrix(knots_12), colnames
	
preserve
    keep if sub12==1 & bt_tmp != . 
	keep if xbv_cod12 != . | xbv_haddock12 != .
    gen long rowid = _n
    export delimited                                                           ///
        rowid year month season state1 tsp* ffcat  ///
		covid2019 str_break bt_tmp  ssb_cod ssb_haddock  newmode new_wp_int   ///
		psu_id var_id ///
		mean_tmp   mean_ssb_haddock mean_ssb_cod ///
        tot_cat_cod tot_cat_hadd                                               ///
        pos_cod12 pos_haddock12                                               ///
        xbv_cod12 xbv_haddock12                                               ///
        xbs_cod12 xbs_haddock12                                                ///
        psel_cod12 psel_haddock12                                              ///
        u_cod12 u_haddock12                                                    ///
        using "logresiduals_Sept_28_2026_area1newmode2.csv", replace
restore
	
*******************************Area212
_pctile bt_tmp [pweight = new_wp_int] if sub212==1,  percentiles(5 35 65 95) 
	scalar k1 = r(r1)
scalar k2 =  r(r2)
scalar k3 =  r(r3)
scalar k4 = r(r4)
drop tsp*	
mkspline tsp = bt_tmp,  cubic  displayknots knots(`=k1' `=k2' `=k3' `=k4')
	matrix knots_212 = r(knots)

foreach w in "cod" "haddock" {
 if "`w'" == "cod" {
svy ,  subpop(sub212): nehurdle  tot_cat_cod c.ssb_haddock ssb_cod c.ssb_haddock#c.ssb_cod tsp*  ib0.ffcat   ib0.covid2019 ib0.str_break ib7.season ib1.state1 ib2.newmode ,  heckman expon 
 }
			else {
		svy ,  subpop(sub212): nehurdle tot_cat_hadd c.ssb_haddock ssb_cod c.ssb_haddock#c.ssb_cod tsp*   ib0.ffcat ib0.covid2019 ib0.str_break ib7.season ib1.state1 ib2.newmode ,  heckman expon
			}
			
estimates save `w'212, replace
estimates store `w'212

  cpt_post `w'212 `=cond("`w'"=="cod","tot_cat_cod","tot_cat_hadd")'

putexcel set "parameters_Sept_28_2026_area2`w'.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "sigma_Sept_28_2026_area2`w'.xlsx", replace	
    putexcel A1 = matrix(e(V)), colnames
}

drop _pc _ph
      quietly gen byte _pc = pos_cod212
    quietly gen byte _ph = pos_haddock212
   
      ml model lf rhosel_ll (atrho: _pc _ph xbs_cod212 xbs_haddock212 = ) if year >=2019 ///
        ,   svy subpop(sub212) 
ml maximize

suest cod212 haddock212,  svy
test [cod212_selection]tsp1 [cod212_selection]tsp2 [cod212_selection]tsp3
test [cod212_selection]1.ffcat [cod212_selection]2.ffcat [cod212_selection]3.ffcat
test [cod212_lntot_cat_cod]tsp1 [cod212_lntot_cat_cod]tsp2 [cod212_lntot_cat_cod]tsp3
test [cod212_lntot_cat_cod]1.ffcat [cod212_lntot_cat_cod]2.ffcat [cod212_lntot_cat_cod]3.ffcat
test [cod212_lnsigma]_cons  [cod212_athrho]_cons
test [haddock212_selection]tsp1 [haddock212_selection]tsp2 [haddock212_selection]tsp3
test [haddock212_selection]1.ffcat [haddock212_selection]2.ffcat [haddock212_selection]3.ffcat
test [haddock212_lntot_cat_hadd]tsp1 [haddock212_lntot_cat_hadd]tsp2 [haddock212_lntot_cat_hadd]tsp3
test [haddock212_lntot_cat_hadd]1.ffcat [haddock212_lntot_cat_hadd]2.ffcat [haddock212_lntot_cat_hadd]3.ffcat
test [haddock212_lnsigma]_cons  [haddock212_athrho]_cons
	putexcel set "parameters_SUEST_Sept_28_2026_area2.xlsx", replace
    putexcel A1 = matrix(e(b)), colnames
putexcel set "sigma_SUEST_Sept_28_2026_area2.xlsx", replace
	putexcel A1 = matrix(e(V)), colnames		
putexcel set "knots_Sept_28_2026_area2.xlsx", replace	
    putexcel A1 = matrix(knots_212), colnames
	
preserve
   drop if sub212 !=1 | newmode==2 | bt_tmp == .
      keep if xbv_cod212 != . | xbv_haddock212 !=.
    gen long rowid = _n
    export delimited                                                           ///
        rowid year month season state1 tsp* ffcat  ///
		mhrsf covid2019 str_break bt_tmp  ssb_cod ssb_haddock  newmode new_wp_int ///
		psu_id var_id   ///
		mean_tmp mean_hrsf  mean_ssb_haddock mean_ssb_cod ///
        tot_cat_cod tot_cat_hadd                                               ///
        pos_cod212 pos_haddock212                                               ///
        xbv_cod212 xbv_haddock212                                               ///
        xbs_cod212 xbs_haddock212                                                ///
        psel_cod212 psel_haddock212                                              ///
        u_cod212 u_haddock212                                                    ///
        using "logresiduals_Sept_28_2026_area2newmode3.csv", replace
restore

preserve
   drop if sub212 != 1 | newmode==3 | bt_tmp == .
   keep if xbv_cod212 != . | xbv_haddock212 !=.
    gen long rowid = _n
    export delimited                                                           ///
        rowid year month season state1 tsp* ffcat  ///
		 covid2019 str_break bt_tmp  ssb_cod ssb_haddock  newmode new_wp_int    ///
		psu_id var_id  ///
		mean_tmp  mean_ssb_haddock mean_ssb_cod ///
        tot_cat_cod tot_cat_hadd                                               ///
        pos_cod212 pos_haddock212                                               ///
        xbv_cod212 xbv_haddock212                                               ///
        xbs_cod212 xbs_haddock212                                                ///
        psel_cod212 psel_haddock212                                              ///
        u_cod212 u_haddock212                                                    ///
        using "logresiduals_Sept_28_2026_area2newmode2.csv", replace
restore

capture drop tsp1 tsp2 tsp3
save "analysis_file_Sept_28_2026", replace		

				