

/********************************************************************
Project: angler survey analysis
Purpose: Prepare demographics + CE long data, estimate mixlogit, save estimates, 
         run Krinsky-Robb WTP simulations and CI plots
********************************************************************/


clear all
mata: mata clear
set more off
set matsize 10000
set maxvar 120000

/********************************************************************
Paths + inputs
********************************************************************/

****USER EDIT HERE****

global projdir "C:\Users\andrew.carr-harris\Dropbox\NMFS\survey_data\narfs_ne_conjoint_weighted_V2" // path to where survey data is stored
global outputdir "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\output_data\figures" // path to where output figures are stored

****END USER EDIT HERE****

global dta_main "$projdir\narfs_ne_conjoint_weighted.dta"  //confidential data pending NOAA review; see README.md for details
global dta_partials "$projdir\narfs_ne_conjoint_weighted_V3.dta"  //confidential data pending NOAA review; see README.md for details
global xlsx_trips "$projdir\CE_trip_attributes.xlsx"
global out_params "$projdir\cod_parameters.csv"

cd "$projdir"
use "$dta_main", clear

/********************************************************************
1) NON-CE DATA CLEANUP (decode/split labels; append partials; recodes)
********************************************************************/

* variables that appear to be labelled numeric codes
local lblvars ///
    cati_q1 cati_q3 cati_q5 ///
    q1_1 q1_2 q1_3 q1_4 ///
    q2_1 q2_2 q2_3 ///
    q4 q5 q6 q7 q8 ///
    q21_1 q21_2 q21_3 q21_4 q21_5 q21_6 ///
    q22 q24 q24_1 q24_2 q24_3 q25 ///
    state q1_inconsistency q2_skiperr q3_skiperr q4_skiperr q5_skiperr q6_skiperr q7_skiperr q8_skiperr ///
    has_phone denstr fished_for_hpc

foreach v of local lblvars {
    capture confirm numeric variable `v'
    if !_rc {
        decode `v', gen(`v'_str)
    }
}

* preserve original cati_q2 and cati_q4 codes
gen cati_q2_new1 = cati_q2
gen cati_q4_new1 = cati_q4

* convert decoded strings back to numeric codes via split
local strvars
foreach v of local lblvars {
    capture confirm variable `v'_str
    if !_rc local strvars `strvars' `v'_str
}

foreach v of local strvars {
    split `v', parse(" = ")
    drop `v'2
    destring `v'1, replace force
    drop `v'
}

* drop originals now that numeric versions live in *_str1
drop `lblvars'

drop cati_q2
drop cati_q4

renvarlab ///
    cati_q1_str cati_q2_new1 cati_q3_str cati_q4_new1 cati_q5_str ///
    q1_1_str q1_2_str q1_3_str q1_4_str ///
    q2_1_str q2_2_str q2_3_str ///
    q4_str q5_str q6_str q7_str q8_str ///
    q21_1_str q21_2_str q21_3_str q21_4_str q21_5_str q21_6_str ///
    q22_str q24_str q24_1_str q24_2_str q24_3_str q25_str ///
    state_str q1_inconsistency_str q2_skiperr_str q3_skiperr_str q4_skiperr_str q5_skiperr_str q6_skiperr_str q7_skiperr_str q8_skiperr_str ///
    has_phone_str denstr_str fished_for_hpc_str, postdrop(5)

order masterid track state zip ver mode prescreen cati_dispo incentive dispo

* append incomplete web survey records
tempfile base partials
drop if dispo=="B"
save `base', replace

preserve
use "$dta_partials", clear
keep if dispo=="B"
gen byte partial = 1
save `partials', replace
restore

append using `partials'

* recode special missing codes
replace cati_q1 = . if inlist(cati_q1, 97, 99)
replace cati_q2 = . if inlist(cati_q2, 997)
replace cati_q3 = . if inlist(cati_q3, 99)
replace cati_q4 = . if inlist(cati_q4, 9999)
replace cati_q5 = . if inlist(cati_q5, 97, 99)
replace q3pb    = 12 if inlist(q3pb, 1015)
replace q6      = .  if q6==-1

replace q24 = . if q24==-1
replace q25 = . if q25==-1

* binary 1/2 -> 1/0
foreach v in cati_q1 q1_1 q5 q8 q22 {
    replace `v' = 0 if `v'==2
}

* rename to analysis names
rename cati_q1 cati_last5
rename cati_q2 cati_trips19
rename cati_q3 cati_likely
rename cati_q4 cati_yrborn
rename cati_q5 cati_inc

rename q1_1 hadd_last5
rename q1_2 poll_last5
rename q1_3 cod_last5
rename q1_4 none_last5

rename q2_1 hadd19
rename q2_2 poll19
rename q2_3 cod19

rename q3hb hbtrips19
rename q3ch chtrips19
rename q3pb pbtrips19

rename q4 fish_pref
rename q5 own_boat
rename q6 likely
rename q7 area_most
rename q8 gompre15

rename q21_1 ANA_cost
rename q21_2 ANA_length
rename q21_3 ANA_hkeep
rename q21_4 ANA_hrel
rename q21_5 ANA_ckeep
rename q21_6 ANA_crel

rename q22 male
rename q23 yrborn
rename q24 educ
rename q25 inc

mvencode ANA*, mv(0) override

* backfill missing survey demographics with CATI if available
replace likely = cati_likely if missing(likely) & !missing(cati_likely)
replace inc    = cati_inc    if missing(inc)    & !missing(cati_inc)
replace yrborn = cati_yrborn if missing(yrborn) & !missing(cati_yrborn)

gen age = 2019 - yrborn if !missing(yrborn)
replace age = . if inlist(age, 110, 118)

egen tot_trips19 = rowtotal(hbtrips19 chtrips19 pbtrips19)
replace tot_trips19 = . if missing(hbtrips19) & missing(chtrips19) & missing(pbtrips19)
replace tot_trips19 = cati_trips19 if missing(tot_trips19) & !missing(cati_trips19)

* eligibility
gen byte eligible = .
replace eligible = 1 if cati_last5==1
replace eligible = 1 if hadd_last5==1 | poll_last5==1 | cod_last5==1
replace eligible = 1 if cati_trips19>0 & !missing(cati_trips19)
replace eligible = 1 if (hbtrips19>0 & !missing(hbtrips19)) | (chtrips19>0 & !missing(chtrips19)) | (pbtrips19>0 & !missing(pbtrips19))
replace eligible = 1 if hadd19==1 | poll19==1 | cod19==1

replace eligible = 0 if none_last5==1 & missing(eligible)
replace eligible = 0 if cati_last5==0 & missing(eligible)
replace eligible = 0 if tot_trips19==0 & missing(eligible)
replace eligible = 0 if cati_dispo=="S2" & missing(eligible)

* web partial reclassification
egen n_missing = rowmiss( ///
    hadd_last5 poll_last5 cod_last5 none_last5 ///
    hadd19 poll19 cod19 hbtrips19 chtrips19 pbtrips19 ///
    fish_pref own_boat likely area_most gompre15 male yrborn educ inc ///
    b1first b1second b2first b2second b3first b3second b4first b4second b5first b5second b6first b6second ///
)

replace dispo="NR" if dispo=="B" & n_missing==31

egen n_miss_eligible = rowmiss(hadd_last5 poll_last5 cod_last5 none_last5)
replace dispo="NR" if dispo=="B" & n_miss_eligible==4


replace dispo="C" if dispo=="B" & n_missing < 31   // answered something

* keep eligible completes
keep if dispo=="C" & eligible==1

* save demographics to merge into CE long
preserve
keep masterid state partial eligible incentive track zip mode ///
     hadd19 poll19 cod19 hbtrips19 chtrips19 pbtrips19 ///
     *last5 fish_pref own_boat likely area_most gompre15 male yrborn age educ inc ANA* tot_trips19
tempfile dems
save `dems', replace
restore


/********************************************************************
2) CE DATA: reshape to long alternatives; add opt-out; choices
********************************************************************/
keep masterid ver b1first b1second b2first b2second b3first b3second b4first b4second b5first b5second b6first b6second

forvalues i=1/6 {
    rename b`i'first  first_b`i'
    rename b`i'second second_b`i'
}
tempfile choices
save `choices', replace

import excel using "$xlsx_trips", clear firstrow
rename block ver
tempfile trips
save `trips', replace

* build block-specific trip attribute files and merge wide by ver
forvalues i=1/6 {
    use `trips', clear
    keep ver *`i'
    renvarlab, subst("`i'" "_b`i'")
    tempfile trips_b`i'
    save `trips_b`i'', replace
}

use `trips_b1', clear
forvalues i=2/6 {
    merge 1:1 ver using `trips_b`i'', nogen
}

merge 1:m ver using `choices', nogen

* reshape to long on question (block) then to long on alt
tostring *, replace
reshape long first_ second_ ///
    codkA_ codblA_ codrA_ hadkA_ hadblA_ hadrA_ ///
    codkB_ codblB_ codrB_ hadkB_ hadblB_ hadrB_ ///
    lengthA_ costA_ lengthB_ costB_ ///
    codkeepA_ codrelA_ codkeepB_ codrelB_ ///
    hadkeepA_ hadrelA_ hadkeepB_ hadrelB_, ///
    i(masterid ver) j(question) string

ds masterid ver question, not

renvarlab `r(varlist)', postdrop(1)

rename question setstr
rename ver versionstr

reshape long codk codbl codr hadk hadbl hadr length cost ///
    codkeep codrel hadkeep hadrel, ///
    i(masterid setstr versionstr first second) j(alt) string

order masterid versionstr setstr alt first second

foreach v in codk codbl codr hadk hadbl hadr length cost codkeep codrel hadkeep hadrel {
    destring `v', replace force
}
egen id = group(masterid)

* expand to include opt-out alternative (C) for each set, based on existing B rows
tempfile ce0
save `ce0', replace

use `ce0', clear
expand 2 if alt=="B", gen(_dup)
replace alt="C" if _dup==1
drop _dup
sort masterid setstr alt

foreach v in codkeep hadkeep codrel hadrel length cost codk codbl codr hadk hadbl hadr {
    replace `v' = 0 if alt=="C"
}

* drop invalid first choice
drop if inlist(first, "", "-1")

* clean second choice if invalid
replace second = "" if second=="-1" & inlist(first, "A", "B")

gen byte first_only = .
replace first_only = 1 if inlist(first,"A","B","C") & (second=="" | second=="-1")

replace first_only = 1 if first==second
replace second     = "C" if first==second & first!="C"
replace second     = ""  if first=="C" & second=="C"

* choice indicators
gen byte resp = (first==alt)
gen byte resp_rnk = .
replace resp_rnk = 1 if first==alt & missing(first_only)
replace resp_rnk = 2 if second==alt & missing(first_only)
replace resp_rnk = 3 if missing(resp_rnk) & missing(first_only)

* numeric set, alti, nofish
gen set = real(substr(setstr,2,1))
drop setstr
gen version = real(versionstr)
drop versionstr

gen alti = .
replace alti = 1 if alt=="A"
replace alti = 2 if alt=="B"
replace alti = 3 if alt=="C"
drop alt

gen byte nofish = (alti==3)

sort masterid set alti
order masterid id version set alti first_only resp resp_rnk first second

merge m:1 masterid using `dems', keep(3) nogen


/********************************************************************
3) Protest measures + counts
********************************************************************/
tempfile ce1
save `ce1', replace

preserve
keep masterid set first second
duplicates drop
gen combo = first + "_" + second

gen byte tabC = (first=="C")
bys masterid: egen sumtab = total(tabC)

gen one = 1
bys masterid combo: egen sumtab2 = total(one)
bys masterid: egen nquestions = total(one)

gen byte protest  = (sumtab2==nquestions)
gen byte protest1 = (sumtab2==nquestions & first=="C")
gen byte protest2 = (sumtab==nquestions)

replace protest = 1 if masterid=="50766457A"

keep masterid protest protest1 protest2
duplicates drop
tempfile protests
save `protests', replace
restore

merge m:1 masterid using `protests', nogen
egen gid = group(masterid set)

* number of sets answered
bys masterid set: gen byte _tag = (_n==1)
bys masterid: egen nsets = total(_tag)
drop _tag

bys masterid set: gen byte _tag = (_n==1 & first_only!=1)
bys masterid: egen nsetsrnk = total(_tag)
drop _tag

egen n_attributes_ignored = rowtotal(ANA_cost-ANA_crel)


/********************************************************************
4) Construct covariates + transformations
********************************************************************/
* education and income dummies
gen byte education_college  = inlist(educ, 3,4,5) if !missing(educ)
gen byte education_graduate = inlist(educ, 6,7,8) if !missing(educ)

gen byte income_medium = inlist(inc, 4,5,6) if !missing(inc)
gen byte income_high   = (inc>6)            if !missing(inc)

* avidity + prefs
gen byte likely_to_fish = .
replace likely_to_fish = 1 if inlist(likely,1,2)
replace likely_to_fish = 0 if inlist(likely,3,4)

gen byte fish_pref_more = .
replace fish_pref_more = 1 if fish_pref==1
replace fish_pref_more = 0 if inlist(fish_pref,2,3)

* scaling
gen cost100      = cost/100
gen length10     = length/10
gen trips2019_10 = tot_trips19/10
gen age10        = age/10

* opt-out interactions
local demo age male tot_trips19 likely_to_fish fish_pref_more income_medium income_high ///
           education_college education_graduate own_boat
foreach v of local demo {
    gen nofish_`v' = nofish*`v'
}

* rename catch vars for readability
rename codkeep ckp
rename hadkeep hkp
rename codrel  crl
rename hadrel  hrl

* sqrt transforms
foreach v in ckp hkp crl hrl {
    gen sqrt`v' = round(sqrt(`v'), .001)
}

gen sqrt_CH_keep = sqrtckp*sqrthkp

* sample indicators (kept as in original, but simplified)
gen byte sample1 = (educ<. & inc<. & age<. & male<. & tot_trips19<.)
gen byte sample2 = (tot_trips19<. & likely<.)
gen byte sample3 = (educ<. & inc<. & age<. & tot_trips19<.)
gen byte sample4 = (age<. & tot_trips19<.)
gen byte sample5 = (tot_trips19<.)
gen byte sample6 = (educ<. & inc<. & age<. & tot_trips19<. & fish_pref<. & own_boat<. & likely<. & male<.)
gen byte sample7 = (age<. & likely<. & fish_pref<.)

* area/mode indicators 
capture confirm variable area
if !_rc {
    gen byte Dgom = inlist(area,1,3)
    gen byte Dgb  = inlist(area,2)
}
gen byte mail = (mode=="mail") if !missing(mode)
gen byte gompst15 = (gompre15==0) if !missing(gompre15)

gen one = 1
egen group_q = group(masterid set)


/********************************************************************
5) Estimation: mixlogit models + export
********************************************************************/
* opt-out interaction list
local interactions ///
    nofish_tot_trips19 nofish_age nofish_income_medium nofish_income_high ///
    nofish_education_college nofish_education_graduate nofish_male ///
    nofish_likely_to_fish nofish_fish_pref_more nofish_own_boat

egen all_rowmiss = rowmiss(`interactions')

eststo clear

* baseline LL (for pseudo-R2)
clogit resp one if all_rowmiss==0 & protest!=1, group(gid)
local ll0 = e(ll)

* full interactions model
eststo m1: mixlogit resp cost `interactions' if protest!=1, ///
    group(gid) id(id) rand(sqrtckp sqrthkp sqrt_CH_keep sqrtcrl sqrthrl nofish)

local ll1 = e(ll)
estadd scalar r2_p = 1 - (`ll1'/`ll0')

distinct id if e(sample)
local n_anglers = r(ndistinct)
distinct group_q if e(sample)
local n_choices = r(ndistinct)
estadd local n_anglers "`n_anglers'"
estadd local n_choices "`n_choices'"

* reduced interactions model - model used in fishery simulation
egen rowmiss_red = rowmiss(nofish_age nofish_likely_to_fish nofish_fish_pref_more)
clogit resp one if rowmiss_red==0 & protest!=1, group(gid)
local ll0r = e(ll)

eststo m2: mixlogit resp cost nofish_age nofish_likely_to_fish nofish_fish_pref_more if protest!=1, ///
    group(gid) id(id) rand(sqrtckp sqrthkp sqrt_CH_keep sqrtcrl sqrthrl nofish)

local ll2 = e(ll)
estadd scalar r2_p = 1 - (`ll2'/`ll0r')

distinct id if e(sample)
local n_anglers2 = r(ndistinct)
distinct group_q if e(sample)
local n_choices2 = r(ndistinct)
estadd local n_anglers "`n_anglers2'"
estadd local n_choices "`n_choices2'"

esttab m1 m2 using "$out_params", replace nobase noomitted ///
    stats(n_anglers n_choices ll r2_p aic, fmt(0 0 3 3 3)) ///
    se b(%9.3f) label nogaps starlevels(* 0.10 ** 0.05 *** 0.01) ///
    order(sqrtckp sqrthkp sqrt_CH_keep sqrtcrl sqrthrl cost nofish)


/********************************************************************
6) Export demographics used by fishery simulation
********************************************************************/
preserve
keep if e(sample)
keep masterid likely_to_fish fish_pref_more
duplicates drop
gen id_export = _n
export delimited using "$projdir\angler_CE_demographics.csv", replace
restore

		
* krinksy and robb procedure for WTP calculations in paper. 
* Generate random b
global params
forv x=1/10000{
local K=e(k) //-e(krnd)
mat bfull=e(b)
mat b=bfull[1,1..`K']
mat Vfull=e(V)
mat Ve=Vfull[1..`K',1..`K']
mat cholV=cholesky(Ve)

mat iid_err=J(`K',1,0)
        
        forvalues i=1/`K' {
            mat iid_err[`i',1]=rnormal()
        }
    
        * Generate draws from vector beta
        mat beta_draw=b' + cholV * iid_err
		mat  list beta_draw

		clear 
		set obs 1
		gen mean_beta_cod_keep = beta_draw[5,1]
		gen mean_beta_hadd_keep =  beta_draw[6,1]
		gen mean_beta_cod_hadd_keep =  beta_draw[7,1]
		gen mean_beta_cost =  beta_draw[1,1]
		
		* generate wtp values based on the random params
		*wtp for first fish, increasing keep of other species 
		forv i =0/30{
			gen wtp_cod_keep_hadd`i'=-(mean_beta_cod_keep+mean_beta_cod_hadd_keep*sqrt(`i'))/(mean_beta_cost)
			gen wtp_hadd_keep_cod`i'=-(mean_beta_hadd_keep+mean_beta_cod_hadd_keep*sqrt(`i'))/(mean_beta_cost)

		}
		
		
		*take the means of the 10,000 wtp's and store them for each KR draw
		
		forv i =0/30{
		su wtp_cod_keep_hadd`i'
		local wtp_cod_keep_hadd`i'=r(mean)
		
		su wtp_hadd_keep_cod`i'
		local wtp_hadd_keep_cod`i'=r(mean)
		
		}

		clear 
		set obs 1
		
		forv i =0/30{
		gen wtp_cod_keep_hadd`i'=`wtp_cod_keep_hadd`i''
		gen wtp_hadd_keep_cod`i'=`wtp_hadd_keep_cod`i''

		}
		
		gen draw=`x'
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params

save "$projdir\k_r_draws.dta", replace


*Cod WTP 
global wtp

forv i=0/30{
	u "$projdir\k_r_draws.dta", clear 
	keep wtp_cod_keep_hadd`i'
	
	su wtp_cod_keep_hadd`i', d
	local mean=`r(mean)'
	local med=`r(p50)'
	local n = r(N)
	local c975=`n'*.975
	local c025=`n'*.025

	sort wtp_cod_keep_hadd`i'
	
	*sort the WTPs and drop the lower and upper 2.5% to get the 95% CIs
	gen id=_n
	drop if id>`c975' | id<`c025'
	
	su wtp_cod_keep_hadd`i'
	local max = `r(max)'
	local min=`r(min)'
	
	clear
	set obs 1
	
	gen wtp_mean= `mean'
	gen wtp_med= `med'
	gen upper = `max'
	gen lower = `min'
	gen wtp_n_secondary = `i'

	tempfile wtp`i'
	save `wtp`i'', replace
	global wtp "$wtp "`wtp`i''" " 

}	

clear
dsconcat $wtp

*WTP plot in paper 
global graphoptions graphregion(fcolor(white) lcolor(white) margin(large)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)
twoway (rcap upper lower wtp_n_secondary if wtp_n_secondary<=15, lcolor(black))  ///
			 (scatter wtp_med wtp_n_secondary  if wtp_n_secondary<=15, mcolor(black) $graphoptions ///
			 xtitle("Number of haddock harvested on the same trip", yoffset(-2)) ytitle("Willingness-to-pay (USD)") xlab(#16) ///
			 yline(0, lcolor(gs12) lpattern(dash)) legend(off) name(cod_mwtp, replace) ///
			 title("Willingness-to-pay for first cod harvested", size(medium)))

graph export "$outputdir/mwtp_cod_updated_7_2_26.eps", as(eps) replace



*Hadd WTP 
global wtp

forv i=0/30{
	u "$projdir\k_r_draws.dta", clear 
	keep wtp_hadd_keep_cod`i'
	
	su wtp_hadd_keep_cod`i', d
	local mean=`r(mean)'
	local med=`r(p50)'
	local n = r(N)
	local c975=`n'*.975
	local c025=`n'*.025

	sort wtp_hadd_keep_cod`i'
	
	*sort the WTPs and drop the lower and upper 2.5% to get the 95% CIs
	gen id=_n
	drop if id>`c975' | id<`c025'
	
	su wtp_hadd_keep_cod`i'
	local max = `r(max)'
	local min=`r(min)'
	
	clear
	set obs 1
	
	gen wtp_mean= `mean'
	gen wtp_med= `med'
	gen upper = `max'
	gen lower = `min'
	gen wtp_n_secondary = `i'

	tempfile wtp`i'
	save `wtp`i'', replace
	global wtp "$wtp "`wtp`i''" " 

}	

clear
dsconcat $wtp

*WTP plot in paper 
global graphoptions graphregion(fcolor(white) lcolor(white) margin(large)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)
twoway (rcap upper lower wtp_n_secondary if wtp_n_secondary<=15, lcolor(black))  ///
			 (scatter wtp_med wtp_n_secondary  if wtp_n_secondary<=15, mcolor(black) $graphoptions ///
			 xtitle("Number of cod harvested on the same trip", yoffset(-2)) ytitle("Willingness-to-pay (USD)") xlab(#16) ///
			 yline(0, lcolor(gs12)  lpattern(dash)) legend(off) name(hadd_mwtp, replace) ///
			 title("Willingness-to-pay for first haddock harvested", size(medium)) ylab(-50(50)150))
			 
graph export "$outputdir/mwtp_hadd_updated_7_2_26.eps", as(eps) replace
