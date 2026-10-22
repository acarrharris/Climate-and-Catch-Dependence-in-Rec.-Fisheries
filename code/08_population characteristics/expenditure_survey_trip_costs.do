
/*******************************************************************************
Estimate mean trip cost and standard error for ME, NH, MA
This code uses restricted NOAA survey data that are not included in the public repository
				 The entire code below uses these data and thus has been commented out. 
 Outputs: derived distribution of trip costs (included in data repo)
*OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 


*******************************************************************************/
* Set global directories - user edit here
global dir_root "C:\Users\andrew.carr-harris\Desktop\Git\Climate-and-Catch-Dependence-in-Rec.-Fisheries" // root to Github directory
global code_root "${dir_root}/code" 
global process_data_root "${dir_root}/data" // data directory for all other model input and output files.


/*
/******************************************************************************/
/******************************************************************************/
/* Section A: Load and clean the survey expenditure data */
/******************************************************************************/
/******************************************************************************/

*Enter a directory with the expenditure survey data
u "$process_data_root\gulf_atl_2022.dta", clear
renvarlab *, lower


* As per Sabrina Lovell, run the following code before using the 2022 data. This code sets certain expenditure variables to missing depending on the trip mode. 
* Begin Sabrina's code recommendation:
* For-Hire trips: set boat fuel and boat rental to missing
replace bfuelexp = . if mode == "For-Hire"
replace brentexp = . if mode == "For-Hire"

* Private Boat trips: set guide costs and crew tips to missing
replace guideexp = . if mode == "Private Boat"
replace crewexp  = . if mode == "Private Boat"

* Shore trips: set all of those to missing
replace bfuelexp = . if mode == "Shore"
replace crewexp  = . if mode == "Shore"
replace guideexp = . if mode == "Shore"
replace brentexp = . if mode == "Shore"
* End Sabrina's code recommendation

*keep only the states we need (ME, NH, MA) 
keep if inlist(st, 23, 33, 25)

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override


* Compute total trip expenditure
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp othexp) 

svyset psu_id [pweight= sample_wt], strata(var_id) singleunit(certainty)

*merge prim1 (numeric id for species) to prim1_common (common_name identified) in order to estimates trip costs for the species of interest, rather than for all species
merge m:1 prim1 using "$process_data_root\prim1.dta", keep(1 3) nogen 
merge m:1 prim2 using "$process_data_root\prim2.dta", keep(1 3) nogen 

gen st2 = string(st,"%02.0f")
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

*Sabrina's definition of for-hire mode include both headboat and charter boats
*Survey mode definitions:
	*3=shore
	*4=headboat
	*5=charter
	*7=private boat

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")


*computes trip cost distribution based on trips for cod, haddock, or pollock
gen common_dom = "1" if inlist(prim1_common, "HADDOCK", "ATLANTIC COD") | inlist(prim2_common, "HADDOCK", "ATLANTIC COD")
replace common_dom = "2" if common_dom == ""

gen domain=mode1+"_"+common_dom
encode domain, gen(domain2)


gen zero_exp = total_exp == 0
foreach m in fh pr {

    di "=========================="
    di "Mode: `m'"
    di "=========================="

    count if common_dom == "1" & mode1 == "`m'"
    count if common_dom == "1" & mode1 == "`m'" & total_exp == 0

    * Unweighted share zero cost trips
    proportion zero_exp if common_dom == "1" & mode1 == "`m'"

    * Weighted share zero cost trips
    mean zero_exp [pweight=sample_wt] if common_dom == "1" & mode1 == "`m'"

    * Distribution conditional on positive cost
    _pctile total_exp [pweight=sample_wt] ///
        if common_dom == "1" & mode1 == "`m'" & total_exp > 0, ///
        p(1 5 10 25 50 75 90 95 99)

    return list
}

* Keep trips targeting cod or haddock and modes used in simulation
keep if common_dom == "1"
keep if inlist(mode1, "fh", "pr")
drop if missing(total_exp) | missing(sample_wt) | total_exp==0

keep mode1 total_exp sample_wt
rename mode1 trip_mode
rename total_exp cost_2022
rename sample_wt cost_weight

compress
save "$process_data_root\trip_cost_distribution.dta", replace
*/

