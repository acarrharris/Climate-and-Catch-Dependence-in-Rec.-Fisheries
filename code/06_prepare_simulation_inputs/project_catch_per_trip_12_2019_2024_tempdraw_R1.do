******************************************************************************
* Catch per trip predictions 2019 - 2024
*Area 1, newmode 2
*Notes: This code has been optimized with the help of Claude Opus 5.
*           This code will produce several large datasets. Please set up your data directories, particulalry, `SAVE_DIR' accordingly. 

******************************************************************************

version 19
set more off
set matsize 2000

* Set global directories - user edit here
global dir_root "C:\Users\andrew.carr-harris\Desktop\Git\Climate-and-Catch-Dependence-in-Rec.-Fisheries" // root to Github directory
global code_root "${dir_root}/code" 
global process_data_root "${dir_root}/data" // data directory for all other model input and output files.

cd "${process_data_root}"
******************************************************************************
* Configuration
******************************************************************************
set seed 58364
local TAG      "area1_newmode2"
local EST_C    "cod12"          // name used in -estimates save-
local EST_H    "haddock12"
local STUB_C   "cod12"          // stub used in the residuals export
local STUB_H   "haddock12"
local SUBPOP   "sub12"
local AREA     1
local NEWMODE  2
local KNOTFILE "knots_Sept_28_2026_area1newmode2.xlsx"
local PARAMFILE "parameters_SUEST_Sept_28_2026_area1newmode2.xlsx"
local SIGMAFILE "sigma_SUEST_Sept_28_2026_area1newmode2.xlsx"
local NREP     3000     // simulated trips per decade, month, and state
local FAMS     "Clayton Frank Gumbel Gaussian Plackett independent"

************parameter uncertainty **************************
* "none"      Simulation using point estimates. 
* "replicate" One coefficient draw fixed across all trips in a replicate. 
********************************************************
local PARAM_MODE "replicate"
local NREPL      50     // replicates
local SAVE_TRIPS "all"
local SAVE_CSV   0
local RESID_DIR  "${process_data_root}"
local SAVE_DIR  "${process_data_root}"
local TEMP_DIR     "${process_data_root}"

******************************************************************************
* Reading in file used for estimating marginal catch distributions.
******************************************************************************
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

forvalues BASEYR  = 2019/2024    {   // year whose non-climate regressors are held fixed
use "${process_data_root}/analysis_file_Sept_28_2026", clear


quietly summarize mean_tmp
    scalar MEANTMP = r(mean)

preserve
    import excel using "`KNOTFILE'", clear cellrange(A2:D2)
    quietly count
    assert r(N) == 1
    scalar k1 = A[1]
    scalar k2 = B[1]
    scalar k3 = C[1]
    scalar k4 = D[1]
restore

assert k1 < k2 & k2 < k3 & k3 < k4


quietly summarize ssb_cod     if year == `BASEYR'
    scalar SSBCOD = r(mean)
quietly summarize ssb_haddock if year == `BASEYR'
    scalar SSBHAD = r(mean)

display as text _n "{hline 70}"
display as text "CONSTANTS RECOVERED FROM ESTIMATION"
display as text "  mean_tmp  = " as result MEANTMP
display as text "  knots     = " as result k1 " " k2 " " k3 " " k4
display as text "              (read from `KNOTFILE')"
display as text "  ssb_cod(`BASEYR')     = " as result SSBCOD
display as text "  ssb_haddock(`BASEYR') = " as result SSBHAD
display as text "{hline 70}"

******************************************************************************
* Function to estimate jointness in probability of nonzero catch
******************************************************************************

preserve

    keep if `SUBPOP' == 1 & !missing(xbs_`STUB_C') & !missing(xbs_`STUB_H')

    quietly gen byte _pc = pos_`STUB_C'
    quietly gen byte _ph = pos_`STUB_H'

    ml model lf rhosel_ll (atrho: _pc _ph xbs_`STUB_C' xbs_`STUB_H' = ) ///
        [pweight = new_wp_int], maximize

    scalar ATRHO  = _b[atrho:_cons]
    scalar RHOSEL = tanh(ATRHO)
    scalar SERHO  = _se[atrho:_cons]

    display as text _n "rho_sel = " as result %7.4f RHOSEL ///
            as text "   (atanh scale: " as result %7.4f ATRHO ///
            as text " , se " as result %6.4f SERHO as text ")"
    if abs(RHOSEL) > 0.94 {
        display as error "WARNING: rho_sel is at a bound. Check the sign flip in rhosel_ll."
    }
restore

******************************************************************************
* Reading in avidity observations 
******************************************************************************
preserve
    keep if year == `BASEYR' & area == `AREA' & newmode == `NEWMODE' & codhaddgom ==1
    keep if  !missing(ffdays12)
    keep ffdays12 season month state1
    quietly count
    assert r(N) > 0
    gen long donor_id = _n
    save "donors_`TAG'_`BASEYR'", replace
restore

preserve
    use "donors_`TAG'_`BASEYR'", clear
    quietly count
    global NDONOR = r(N)
restore
display as text "donor pool: " as result $NDONOR as text " baseline-year trips"

******************************************************************************
*Reading in and preparing parameter estimates
******************************************************************************
preserve
    import excel using "`PARAMFILE'", clear cellrange(A3) allstring
    unab allv : _all
    local KALL : word count `allv'
    matrix b_ALL = J(1, `KALL', 0)
    local j 0
    foreach v of local allv {
        local ++j
        matrix b_ALL[1,`j'] = real(`v'[1])
    }
restore
local KALL = colsof(b_ALL)

preserve
    import excel using "`SIGMAFILE'", clear cellrange(A3) allstring
    unab allv : _all
    local nc : word count `allv'
    quietly count
    local nr = r(N)
    assert `nr' == `nc'
    assert `nc' == `KALL'
    matrix V_ALL = J(`nr', `nc', 0)
    forvalues i = 1/`nr' {
        local j 0
        foreach v of local allv {
            local ++j
            matrix V_ALL[`i',`j'] = real(`v'[`i'])
        }
    }
restore

display as text "stacked suest: `KALL' parameters"

matrix symeigen EVEC_ALL EVAL_ALL = V_ALL
matrix SQL_ALL = J(1, `KALL', 0)
forvalues j = 1/`KALL' {
    matrix SQL_ALL[1,`j'] = sqrt(max(EVAL_ALL[1,`j'], 0))
}

foreach sp in C H {
    if "`sp'" == "C"  local EST "`EST_C'"
    else              local EST "`EST_H'"
    estimates use `EST'
    matrix b_`sp' = e(b)
    local k_`sp' = colsof(b_`sp')
}
assert `k_C' + `k_H' == `KALL'
display as text "  cod block 1-`k_C' ; haddock block " as result `k_C'+1 as text "-`KALL'"

foreach fam of local FAMS {
	   if "`fam'" != "independent" {
    import delimited using "`RESID_DIR'/residual_draws_`fam'_`TAG'.csv", clear varnames(1)
    quietly count
    global NDRAW_`fam' = r(N)
    save "rdraw_`fam'_`TAG'", replace
    display as text "  `fam': " as result r(N) as text " draws"
}

   if "`fam'"== "independent" {
   	   import delimited using "`RESID_DIR'/residual_draws_uncond_`TAG'.csv", clear varnames(1)
    quietly count
    global NDRAW_`fam' = r(N)
    save "residual_draws_uncond_`TAG'", replace
   }
   }

	import delimited using "`RESID_DIR'/residual_draws_onespecies_`TAG'.csv", clear varnames(1)
    save "rdraw_onespecies_`TAG'", replace
******************************************************************************
*  "none"       one replicate using the point estimates
* "pertrip"    one replicate with every trip geting its own coefficient draw
*  "replicate"  NREPL passes; one coefficient draw per replication, held constant across all trips in that replicate
******************************************************************************
if "`PARAM_MODE'" == "replicate"  local NPASS = `NREPL'
else                              local NPASS = 1

tempfile agg
capture erase "`agg'"
local firstpass = 1

tempname pf
postfile `pf' int replicate str12 copula int decade ///
    double mcod double mhad double pboth ///
    using "decadal_summary_tempdraw_`TAG'_`BASEYR'", replace

forvalues rep = 1/`NPASS'  {
******************************************************************************
*Bottom temperature projections
******************************************************************************

if (`AREA' == 2) {
   use "`TEMP_DIR'/temps_2_`rep'.dta", clear
} 
else {
   use "`TEMP_DIR'/temps_1_`rep'.dta", clear
}
    tempfile temps
    save `temps'

use `temps', clear
    expand 3                                 
    bysort decade month: gen byte state_n= _n
    gen str2 state11 = cond(state_n==1,"MA",cond(state_n==2,"ME","NH"))
	encode state11, gen(state1)
    drop state_n state11
    expand `NREP'                              

gen byte area    = `AREA'
gen byte newmode = `NEWMODE'

gen byte season = month
    replace season = 4  if season == 3
    replace season = 11 if season == 12

gen double ssb_cod     = SSBCOD
gen double ssb_haddock = SSBHAD

gen byte str_break = 1
gen byte covid2019 = 0

gen double bt_tmp = proj_temp - MEANTMP

gen long donor_id = 1 + int(runiform() * $NDONOR)
merge m:1 donor_id using "donors_`TAG'_`BASEYR'", keep(match) keepusing(ffdays12) nogen

gen byte ffcat = .
    replace ffcat = 0 if ffdays12 == 0
    replace ffcat = 1 if ffdays12 > 0  & ffdays12 <= 5
    replace ffcat = 2 if ffdays12 > 5  & ffdays12 <= 20
    replace ffcat = 3 if ffdays12 > 20 & ffdays12 < .

gen long trip_id = _n
quietly count
scalar NGRID = r(N)
display as text "grid: " as result r(N) as text " simulated trips"
display as text "  NDRAW in the MATLAB step must be >= this"

******************************************************************************
* Generating cubic spline with original knots
******************************************************************************
mkspline tsp = bt_tmp, cubic knots(`=k1' `=k2' `=k3' `=k4')

quietly summarize bt_tmp
display as text "projected bt_tmp range: " as result r(min) as text " to " as result r(max)
display as text "  last knot = " as result `=k4' as text ";  beyond it the fit is LINEAR (by construction)"

save "grid_tempdraw_`TAG'_`BASEYR'_`NPASS'", replace

******************************************************************************
* Draw from coefficient distribution
******************************************************************************
     if "`PARAM_MODE'" == "none" {
        matrix bdraw_ALL = b_ALL
    }
    else {
        matrix z_ALL = J(`KALL', 1, 0)
        forvalues j = 1/`KALL' {
            matrix z_ALL[`j',1] = rnormal() * SQL_ALL[1,`j']
        }
        matrix bdraw_ALL = b_ALL + (EVEC_ALL * z_ALL)'
    }

    matrix bdraw_C = bdraw_ALL[1, 1..`k_C']
    matrix bdraw_H = bdraw_ALL[1, `=`k_C'+1'..`KALL']
    matrix colnames bdraw_C = `: colnames b_C'
    matrix coleq    bdraw_C = `: coleq b_C'
    matrix colnames bdraw_H = `: colnames b_H'
    matrix coleq    bdraw_H = `: coleq b_H'

******************************************************************************
* Conditional prbability of positive catch and log postitive catch estimates
******************************************************************************
    matrix score double xbs_c = bdraw_C, eq(#1)
    matrix score double xbv_c = bdraw_C, eq(#2)
    matrix score double xbs_h = bdraw_H, eq(#1)
    matrix score double xbv_h = bdraw_H, eq(#2)

    if "`PARAM_MODE'" == "pertrip" & `rep' == 1 {
        display as error "PARAM_MODE 'pertrip' is not supported here; using 'replicate' semantics."
    }

    matrix CORR = (1, `=RHOSEL' \ `=RHOSEL', 1)
	drawnorm z1 z2   
		
******************************************************************************
* Generating Observation weights for simulation
******************************************************************************
    if `firstpass' {
        preserve
            use "analysis_file_Sept_28_2026", clear
            keep if area == `AREA' & newmode == `NEWMODE' & year == `BASEYR' & codhaddgom ==1
            collapse (sum) popw = new_wp_int, by(month state1)
            save "cellw_`TAG'_`BASEYR'", replace
        restore
    }
    merge m:1 month state1 using "cellw_`TAG'_`BASEYR'", keep(master match) nogen
        replace popw = 0 if missing(popw)
        bysort decade: egen double _tot = total(popw)
        gen double cell_weight = popw / _tot
        drop _tot

******************************************************************************
* Merging residuals and generating joint catch draws
******************************************************************************

    foreach fam of local FAMS {

        preserve
        gen long draw_id = 1 + int(runiform() * ${NDRAW_`fam'})		   

   if "`fam'" == "independent" {
   	local RHO_USE = 0
        merge m:1 draw_id using "residual_draws_uncond_`TAG'", ///
            keep(master match) keepusing(r_cod_all r_had_all) nogen
   }
   else {
   	local RHO_USE = RHOSEL   
	merge m:1 draw_id using "rdraw_`fam'_`TAG'", ///
					keep(master match) keepusing(r_cod r_had) nogen
        merge m:1 draw_id using "rdraw_onespecies_`TAG'", ///
            keep(master match) keepusing(r_cod_only r_had_only) nogen
   }

   gen double e_c = z1
   gen double e_h = `RHO_USE'*z1+sqrt(1-`RHO_USE'^2)*z2
   
    gen byte sel_c = (xbv_c < .) & ((xbs_c + e_c) > 0)
    gen byte sel_h = (xbv_h < .) & ((xbs_h + e_h) > 0)
	
	    gen byte cell = 0
        replace cell = 1 if  sel_c &  sel_h      // both
        replace cell = 2 if  sel_c & !sel_h      // cod only
        replace cell = 3 if !sel_c &  sel_h      // haddock only

   if "`fam'" == "independent" {
         gen double u_c = r_cod_all
        gen double u_h =  r_had_all
	
   } 
   else {    
        gen double u_c = .
        gen double u_h = .
            replace u_c = r_cod       if cell == 1
            replace u_h = r_had       if cell == 1
            replace u_c = r_cod_only  if cell == 2
            replace u_h = r_had_only  if cell == 3
   }

        gen double catch_cod = 0
        gen double catch_had = 0
            replace catch_cod = exp(xbv_c + u_c) if sel_c
            replace catch_had = exp(xbv_h + u_h) if sel_h

        gen int replicate = `rep'
        gen str12 copula  = "`fam'"


        local WRITE 0
        if "`SAVE_TRIPS'" == "all"                  local WRITE 1
        if "`SAVE_TRIPS'" == "first" & `firstpass'  local WRITE 1

  if `WRITE' {

    keep replicate decade month season state1 area newmode ///
         cell catch_cod catch_had cell_weight copula
    drop if cell_weight == 0
    
    local outfile "`SAVE_DIR'/`BASEYR'/simulated_catch_tempdraw_`fam'_`TAG'_`BASEYR'"

    capture confirm file "`outfile'.dta"
    if !_rc {
        append using "`outfile'"
    }
    save "`outfile'", replace
    
    if `SAVE_CSV' {
        export delimited using "`outfile'.csv", replace
    }
}
        restore
    }

    local firstpass = 0
	
    if "`SAVE_TRIPS'" == "all" {
        display as text "  replicate `rep' of `NPASS' written"
    }
}
postclose `pf'
}