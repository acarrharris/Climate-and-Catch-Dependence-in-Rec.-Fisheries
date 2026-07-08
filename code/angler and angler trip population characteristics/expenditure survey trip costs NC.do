
***This code creates trip cost distributions based on the NOAA's 2017 trip expenditure survey data (available from Sabrina.Lovell@noaa.gov upon request )

*Enter a directory with the expenditure survey data 
cd "[enter data directory]" //data stored here
u "atl_states_2017_expsurvey.dta", clear
renvarlab *, lower


*Atlantic coast states (ME-NC) 
keep if inlist(st, 23, 33, 25, 44, 9, 36, 34, 10, 24, 51, 37)

egen n_missing_cats=rowmiss(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)
drop if n_missing==16

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)

svyset psu_id [pweight= sample_wt], strata(var_id) singleunit(certainty)

gen mode1="sh" if inlist(mode_fx, "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx, "7")

tostring st, gen(st2)

gen state="CT" if st2=="09" 
replace state="DE" if st2=="10"
replace state="ME" if st2=="23"
replace state="MD" if st2=="24"
replace state="MA" if st2=="25"
replace state="NJ" if st2=="34"
replace state="NY" if st2=="36"
replace state="NC" if st2=="37"
replace state="RI" if st2=="44"
replace state="VA" if st2=="51"
replace state="NH" if st2=="33"

*Adjust for inflation based on BLS website for each year (2010-2021) relative to survey year (2017) and compute mean trip cost
*Inflation scalars:
local vals 0.89 0.91 0.93 0.95 0.96 0.96 0.98 1 1.02 1.04 1.08 
foreach v of local vals{
gen tot_exp2= total_exp*`v'
svy: mean tot_exp2  if inlist(st,23, 33, 25) & mode1!="sh"
drop tot_exp2
}

* integrate estimated mean and se's into fishery simulation model 