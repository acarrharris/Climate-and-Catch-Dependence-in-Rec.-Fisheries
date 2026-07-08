
* create a dataset of catch draws for each future decade to be used in the simulation model

* Identify the range of observed catch-per-trip in order to truncate projected catch-per-trip to within that range

cd "[enter data directory]" //data stored here
su tot_cat_cod //max 156
su tot_cat_hadd //max 77


***********************************
***********2021*******************
***********************************

*Import the projected catch draws from the copula model

local list "correlated_sample_Clayton_Area1_newmode2 correlated_sample_Clayton_Area1_newmode3  correlated_sample_Clayton_Area2_newmode2 correlated_sample_Clayton_Area2_newmode3  correlated_sample_Frank_Area1_newmode2 correlated_sample_Frank_Area1_newmode3   correlated_sample_Frank_Area2_newmode2 correlated_sample_Frank_Area2_newmode3 correlated_sample_Gaussian_Area1_newmode2 correlated_sample_Gaussian_Area1_newmode3 	  correlated_sample_Gaussian_Area2_newmode2 correlated_sample_Gaussian_Area2_newmode3   correlated_sample_Gumbel_Area1_newmode2 correlated_sample_Gumbel_Area1_newmode3  correlated_sample_Gumbel_Area2_newmode2 correlated_sample_Gumbel_Area2_newmode3 		  correlated_sample_Plackett_Area1_newmode2 correlated_sample_Plackett_Area1_newmode3 correlated_sample_Plackett_Area2_newmode2 correlated_sample_Plackett_Area2_newmode3" 

global drawz
foreach s of local list{

local y 2021

import delimited  "`s'_`y'.csv", clear
sort decade month 
gen state=25 if ma==1
replace state=23 if me==1
replace state=33 if nh==1
gen source="`s'"
split source, parse(_)

replace cod_corr=156 if cod_corr>156
replace cod_ind=156 if cod_ind>156
replace had_corr=77 if had_corr>77
replace had_ind=77 if had_ind>77

levelsof source4, clean

if "`r(levels)'"=="Area1"{
	gen area="inshore"
}

if "`r(levels)'"=="Area2"{
	gen area="offshore"
}

levelsof source5, clean

if "`r(levels)'"=="newmode2"{
	gen mode="fh"
}

if "`r(levels)'"=="newmode3"{
	gen mode="pr"
}

gen area_tab= substr(source4, strlen(source4), 1)
gen mode_tab= substr(source5, strlen(source5), 1)
gen domain=source3+"_"+area_tab+"_"+mode_tab
levelsof domain, clean
local file="`r(levels)'"

di "`file'"

keep  month decade cod_corr had_corr cod_ind had_ind mode area state source3
sort decade month mode state area
bysort decade month mode state area: gen n=_n

levelsof source3, clean 
local cop="`r(levels)'"

ds  month decade n state area mode source3, not 
renvarlab `r(varlist)', postfix("_`cop'")
drop source
renvarlab, lower
compress

save `file'.dta, replace

}

u "Clayton_1_2.dta", clear 
append using "Clayton_2_2.dta"
append using "Clayton_1_3.dta"
append using "Clayton_2_3.dta"
tempfile clayton
save `clayton', replace 

u "Frank_1_2.dta", clear 
append using "Frank_2_2.dta"
append using "Frank_1_3.dta"
append using "Frank_2_3.dta"
tempfile Frank
save `Frank', replace 

u "Gaussian_1_2.dta", clear 
append using "Gaussian_2_2.dta"
append using "Gaussian_1_3.dta"
append using "Gaussian_2_3.dta"
tempfile Gaussian
save `Gaussian', replace 

u "Plackett_1_2.dta", clear 
append using "Plackett_2_2.dta"
append using "Plackett_1_3.dta"
append using "Plackett_2_3.dta"
tempfile Plackett
save `Plackett', replace 

u "Gumbel_1_2.dta", clear 
append using "Gumbel_2_2.dta"
append using "Gumbel_1_3.dta"
append using "Gumbel_2_3.dta"

merge 1:1 month decade n  mode state area using `Plackett', nogen 
merge 1:1 month decade n  mode state area using `clayton', nogen 
merge 1:1 month decade n  mode state area using `Gaussian', nogen 
merge 1:1 month decade n  mode state area using `Frank', nogen 
order decade month mode state area n

save "projection_catch_base.dta", replace


reshape wide cod* had*, i(month mode n state area) j(decade)
gen st="MA" if state==25
replace st="NH" if state==33 
replace st="ME" if state==23
drop state
rename st state 
rename mode mode1
rename month month1

save "projection_catch_base_wide.dta", replace


global drawz
forv i=1/100{

set seed `i'

import delimited using "directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2021

* replace trip values as zeroes in periods with high uncertainty according to the MRIP estimate query webpage
replace dtrip=0 if st == 23 & year == 2019 & inlist(month,5, 6)
replace dtrip=0 if st == 23 & year == 2019 & inlist(month,9,10)
replace dtrip=0 if st == 23 & year == 2020 & inlist(month,5, 6)
replace dtrip=0 if st == 23 & year == 2020 & inlist(month,9,10)
replace dtrip=0 if st == 25 & year == 2020 & inlist(month,11,12)
replace dtrip=0 if st == 23 & year == 2021 & inlist(month,9,10)

tostring month, gen(month1)
tostring period, gen(period1)
tostring st, replace

gen domain=month1+"_"+period1+"_"+mode+"_"+area+"_"+st

gen state="MA" if st=="25"
replace state="NH" if st=="33" 
replace state="ME" if st=="23"

keep if dtrip>0

keep month period mode period2 area st  domain state  dtrip

levelsof domain, local(doms)

tempfile base 
save `base', replace 

global domz
foreach d of local doms{
	
	u `base', clear 
	keep if domain=="`d'"

	su month
	local mon=`r(mean)'
	
	levelsof month, local(mon) clean
	levelsof mode, local(md) clean
	levelsof period2, local(pdz) clean
	levelsof area, local(areaz) clean
	levelsof state, local(stz) clean

	use  "projection_catch_base_wide.dta", clear  

	keep if month1==`mon'
	keep if mode1=="`md'"
	keep if area=="`areaz'"
	keep if state=="`stz'"
	
	count
	local n=`r(N)'
	
	if `n'>=3000 {
	
	keep had* cod* month1 
	sample 3000, count

	gen domain="`d'"
	gen mode1="`md'"
	gen period2="`pdz'"
	gen area= "`areaz'"
	gen state= "`stz'"
	gen tripid = ceil(_n / 30)
	bysort tripid: gen catch_draw = _n
	sort tripid catch_draw
	
	}
	
	else{
		
		local expand = ceil(3000/`n')+2
		expand `expand'
		sample 3000, count
		
		keep had* cod* month1 
		gen domain="`d'"
		gen mode1="`md'"
		gen period2="`pdz'"
		gen area= "`areaz'"
		gen state= "`stz'"
		
		gen tripid = ceil(_n / 30)
		bysort tripid: gen catch_draw = _n
		sort tripid catch_draw
	}
	
		tempfile domz`d'
		save `domz`d'', replace
		global domz "$domz "`domz`d''" " 
		
		
}

dsconcat $domz
ds month1 domain mode1 period2 area state, not
local vars `r(varlist)'
foreach v of local vars{
	replace `v'=round(`v')
}

gen draw=`i'
split domain, parse("_")
drop mode1 period2 area state 

rename domain1 month
rename domain2 period
rename domain3 mode
rename domain4 area
rename domain5 state
destring month period state, replace
drop month1

order domain month period mode area state  
tostring period, gen(period1)
tostring state, gen(st2)
gen period2=mode+"_"+period1+"_"+area+"_"+st2
drop st2 period1
order domain month period mode area state tripid catch_draw draw period2
export delimited using "projection_catch_draw`i'_2021.csv", replace 
}


***********************************
***********2020*******************
***********************************

local list "correlated_sample_Clayton_Area1_newmode2 correlated_sample_Clayton_Area1_newmode3  correlated_sample_Clayton_Area2_newmode2 correlated_sample_Clayton_Area2_newmode3  correlated_sample_Frank_Area1_newmode2 correlated_sample_Frank_Area1_newmode3   correlated_sample_Frank_Area2_newmode2 correlated_sample_Frank_Area2_newmode3 correlated_sample_Gaussian_Area1_newmode2 correlated_sample_Gaussian_Area1_newmode3 	  correlated_sample_Gaussian_Area2_newmode2 correlated_sample_Gaussian_Area2_newmode3   correlated_sample_Gumbel_Area1_newmode2 correlated_sample_Gumbel_Area1_newmode3  correlated_sample_Gumbel_Area2_newmode2 correlated_sample_Gumbel_Area2_newmode3 		  correlated_sample_Plackett_Area1_newmode2 correlated_sample_Plackett_Area1_newmode3 correlated_sample_Plackett_Area2_newmode2 correlated_sample_Plackett_Area2_newmode3" 

global drawz

foreach s of local list{

local y 2020

import delimited  "`s'_`y'.csv", clear
sort decade month 
gen state=25 if ma==1
replace state=23 if me==1
replace state=33 if nh==1
gen source="`s'"
split source, parse(_)

replace cod_corr=156 if cod_corr>156
replace cod_ind=156 if cod_ind>156
replace had_corr=77 if had_corr>77
replace had_ind=77 if had_ind>77

levelsof source4, clean

if "`r(levels)'"=="Area1"{
	gen area="inshore"
}

if "`r(levels)'"=="Area2"{
	gen area="offshore"
}

levelsof source5, clean

if "`r(levels)'"=="newmode2"{
	gen mode="fh"
}

if "`r(levels)'"=="newmode3"{
	gen mode="pr"
}

gen area_tab= substr(source4, strlen(source4), 1)
gen mode_tab= substr(source5, strlen(source5), 1)
gen domain=source3+"_"+area_tab+"_"+mode_tab
levelsof domain, clean
local file="`r(levels)'"
di "`file'"
keep  month decade cod_corr had_corr cod_ind had_ind mode area state source3
sort decade month mode state area
bysort decade month mode state area: gen n=_n

levelsof source3, clean 
local cop="`r(levels)'"

ds  month decade n state area mode source3, not 
renvarlab `r(varlist)', postfix("_`cop'")
drop source
renvarlab, lower
compress

save `file'.dta, replace

}

u "Clayton_1_2.dta", clear 
append using "Clayton_2_2.dta"
append using "Clayton_1_3.dta"
append using "Clayton_2_3.dta"
tempfile clayton
save `clayton', replace 

u "Frank_1_2.dta", clear 
append using "Frank_2_2.dta"
append using "Frank_1_3.dta"
append using "Frank_2_3.dta"
tempfile Frank
save `Frank', replace 

u "Gaussian_1_2.dta", clear 
append using "Gaussian_2_2.dta"
append using "Gaussian_1_3.dta"
append using "Gaussian_2_3.dta"
tempfile Gaussian
save `Gaussian', replace 

u "Plackett_1_2.dta", clear 
append using "Plackett_2_2.dta"
append using "Plackett_1_3.dta"
append using "Plackett_2_3.dta"
tempfile Plackett
save `Plackett', replace 

u "Gumbel_1_2.dta", clear 
append using "Gumbel_2_2.dta"
append using "Gumbel_1_3.dta"
append using "Gumbel_2_3.dta"

merge 1:1 month decade n  mode state area using `Plackett', nogen 
merge 1:1 month decade n  mode state area using `clayton', nogen 
merge 1:1 month decade n  mode state area using `Gaussian', nogen 
merge 1:1 month decade n  mode state area using `Frank', nogen 
order decade month mode state area n

save "projection_catch_base.dta", replace

reshape wide cod* had*, i(month mode n state area) j(decade)
gen st="MA" if state==25
replace st="NH" if state==33 
replace st="ME" if state==23
drop state
rename st state 
rename mode mode1
rename month month1

save "projection_catch_base_wide.dta", replace

global drawz
forv i=1/100{

set seed `i'


import delimited using "directed trips and regulations 2010_2020_disaggregated.csv", clear  

keep if year==2020

* replace trip values as zeroes in periods with high uncertainty according to the MRIP estimate query webpage
replace dtrip=0 if st == 23 & year == 2019 & inlist(month,5, 6)
replace dtrip=0 if st == 23 & year == 2019 & inlist(month,9,10)
replace dtrip=0 if st == 23 & year == 2020 & inlist(month,5, 6)
replace dtrip=0 if st == 23 & year == 2020 & inlist(month,9,10)
replace dtrip=0 if st == 25 & year == 2020 & inlist(month,11,12)
replace dtrip=0 if st == 23 & year == 2021 & inlist(month,9,10)

tostring month, gen(month1)
tostring period, gen(period1)
tostring st, replace

gen domain=month1+"_"+period1+"_"+mode+"_"+area+"_"+st

gen state="MA" if st=="25"
replace state="NH" if st=="33" 
replace state="ME" if st=="23"

keep if dtrip>0

keep month period mode period2 area st  domain state  dtrip

levelsof domain, local(doms)

tempfile base 
save `base', replace 

global domz
foreach d of local doms{
	
	u `base', clear 
	keep if domain=="`d'"
	
	su month
	local mon=`r(mean)'
	
	levelsof month, local(mon) clean
	levelsof mode, local(md) clean
	levelsof period2, local(pdz) clean
	levelsof area, local(areaz) clean
	levelsof state, local(stz) clean

	use  "projection_catch_base_wide.dta", clear  
	

	keep if month1==`mon'
	keep if mode1=="`md'"
	keep if area=="`areaz'"
	keep if state=="`stz'"
	
	count
	local n=`r(N)'
	
	if `n'>=3000 {
	
	keep had* cod* month1 
	sample 3000, count

	gen domain="`d'"
	gen mode1="`md'"
	gen period2="`pdz'"
	gen area= "`areaz'"
	gen state= "`stz'"
	gen tripid = ceil(_n / 30)
	bysort tripid: gen catch_draw = _n
	sort tripid catch_draw
	
	}
	
	else{
		
		local expand = ceil(3000/`n')+2
		expand `expand'
		sample 3000, count
		
		keep had* cod* month1 
		gen domain="`d'"
		gen mode1="`md'"
		gen period2="`pdz'"
		gen area= "`areaz'"
		gen state= "`stz'"
		
		gen tripid = ceil(_n / 30)
		bysort tripid: gen catch_draw = _n
		sort tripid catch_draw
	}
	
		tempfile domz`d'
		save `domz`d'', replace
		global domz "$domz "`domz`d''" " 
		
		
}

dsconcat $domz
ds month1 domain mode1 period2 area state, not
local vars `r(varlist)'
foreach v of local vars{
	replace `v'=round(`v')
}

gen draw=`i'
split domain, parse("_")
drop mode1 period2 area state 

rename domain1 month
rename domain2 period
rename domain3 mode
rename domain4 area
rename domain5 state
destring month period state, replace
drop month1

order domain month period mode area state  
tostring period, gen(period1)
tostring state, gen(st2)
gen period2=mode+"_"+period1+"_"+area+"_"+st2
drop st2 period1
order domain month period mode area state tripid catch_draw draw period2
export delimited using "projection_catch_draw`i'_2020.csv", replace 
}




***********************************
***********2019*******************
***********************************

local list "correlated_sample_Clayton_Area1_newmode2 correlated_sample_Clayton_Area1_newmode3  correlated_sample_Clayton_Area2_newmode2 correlated_sample_Clayton_Area2_newmode3  correlated_sample_Frank_Area1_newmode2 correlated_sample_Frank_Area1_newmode3   correlated_sample_Frank_Area2_newmode2 correlated_sample_Frank_Area2_newmode3 correlated_sample_Gaussian_Area1_newmode2 correlated_sample_Gaussian_Area1_newmode3 	  correlated_sample_Gaussian_Area2_newmode2 correlated_sample_Gaussian_Area2_newmode3   correlated_sample_Gumbel_Area1_newmode2 correlated_sample_Gumbel_Area1_newmode3  correlated_sample_Gumbel_Area2_newmode2 correlated_sample_Gumbel_Area2_newmode3 		  correlated_sample_Plackett_Area1_newmode2 correlated_sample_Plackett_Area1_newmode3 correlated_sample_Plackett_Area2_newmode2 correlated_sample_Plackett_Area2_newmode3" 

global drawz

foreach s of local list{

local y 2019

import delimited  "`s'_`y'.csv", clear
sort decade month 
gen state=25 if ma==1
replace state=23 if me==1
replace state=33 if nh==1
gen source="`s'"
split source, parse(_)

replace cod_corr=156 if cod_corr>156
replace cod_ind=156 if cod_ind>156
replace had_corr=77 if had_corr>77
replace had_ind=77 if had_ind>77

levelsof source4, clean

if "`r(levels)'"=="Area1"{
	gen area="inshore"
}

if "`r(levels)'"=="Area2"{
	gen area="offshore"
}

levelsof source5, clean

if "`r(levels)'"=="newmode2"{
	gen mode="fh"
}

if "`r(levels)'"=="newmode3"{
	gen mode="pr"
}

gen area_tab= substr(source4, strlen(source4), 1)
gen mode_tab= substr(source5, strlen(source5), 1)
gen domain=source3+"_"+area_tab+"_"+mode_tab
levelsof domain, clean
local file="`r(levels)'"
di "`file'"
keep  month decade cod_corr had_corr cod_ind had_ind mode area state source3
sort decade month mode state area
bysort decade month mode state area: gen n=_n

levelsof source3, clean 
local cop="`r(levels)'"

ds  month decade n state area mode source3, not 
renvarlab `r(varlist)', postfix("_`cop'")
drop source
renvarlab, lower
compress


save `file'.dta, replace

}

u "Clayton_1_2.dta", clear 
append using "Clayton_2_2.dta"
append using "Clayton_1_3.dta"
append using "Clayton_2_3.dta"
tempfile clayton
save `clayton', replace 

u "Frank_1_2.dta", clear 
append using "Frank_2_2.dta"
append using "Frank_1_3.dta"
append using "Frank_2_3.dta"
tempfile Frank
save `Frank', replace 

u "Gaussian_1_2.dta", clear 
append using "Gaussian_2_2.dta"
append using "Gaussian_1_3.dta"
append using "Gaussian_2_3.dta"
tempfile Gaussian
save `Gaussian', replace 

u "Plackett_1_2.dta", clear 
append using "Plackett_2_2.dta"
append using "Plackett_1_3.dta"
append using "Plackett_2_3.dta"
tempfile Plackett
save `Plackett', replace 

u "Gumbel_1_2.dta", clear 
append using "Gumbel_2_2.dta"
append using "Gumbel_1_3.dta"
append using "Gumbel_2_3.dta"
merge 1:1 month decade n  mode state area using `Plackett', nogen 
merge 1:1 month decade n  mode state area using `clayton', nogen 
merge 1:1 month decade n  mode state area using `Gaussian', nogen 
merge 1:1 month decade n  mode state area using `Frank', nogen 
order decade month mode state area n

save "projection_catch_base.dta", replace

reshape wide cod* had*, i(month mode n state area) j(decade)
gen st="MA" if state==25
replace st="NH" if state==33 
replace st="ME" if state==23
drop state
rename st state 
rename mode mode1
rename month month1

save "projection_catch_base_wide.dta", replace


global drawz
forv i=1/100{

set seed `i'

* replace trip values as zeroes in periods with high uncertainty according to the MRIP estimate query webpage
import delimited using "directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2019
replace dtrip=0 if st == 23 & year == 2019 & inlist(month,5, 6)
replace dtrip=0 if st == 23 & year == 2019 & inlist(month,9,10)
replace dtrip=0 if st == 23 & year == 2020 & inlist(month,5, 6)
replace dtrip=0 if st == 23 & year == 2020 & inlist(month,9,10)
replace dtrip=0 if st == 25 & year == 2020 & inlist(month,11,12)
replace dtrip=0 if st == 23 & year == 2021 & inlist(month,9,10)


tostring month, gen(month1)
tostring period, gen(period1)
tostring st, replace

gen domain=month1+"_"+period1+"_"+mode+"_"+area+"_"+st

gen state="MA" if st=="25"
replace state="NH" if st=="33" 
replace state="ME" if st=="23"

keep if dtrip>0

keep month period mode period2 area st  domain state  dtrip

levelsof domain, local(doms)

tempfile base 
save `base', replace 

global domz
foreach d of local doms{
	
	u `base', clear 
	keep if domain=="`d'"
	
	su month
	local mon=`r(mean)'
	
	levelsof month, local(mon) clean
	levelsof mode, local(md) clean
	levelsof period2, local(pdz) clean
	levelsof area, local(areaz) clean
	levelsof state, local(stz) clean

	use  "projection_catch_base_wide.dta", clear  
	

	keep if month1==`mon'
	keep if mode1=="`md'"
	keep if area=="`areaz'"
	keep if state=="`stz'"
	
	count
	local n=`r(N)'
	
	if `n'>=3000 {
	
	keep had* cod* month1 
	sample 3000, count

	gen domain="`d'"
	gen mode1="`md'"
	gen period2="`pdz'"
	gen area= "`areaz'"
	gen state= "`stz'"
	gen tripid = ceil(_n / 30)
	bysort tripid: gen catch_draw = _n
	sort tripid catch_draw
	
	}
	
	else{
		
		local expand = ceil(3000/`n')+2
		expand `expand'
		sample 3000, count
		
		keep had* cod* month1 
		gen domain="`d'"
		gen mode1="`md'"
		gen period2="`pdz'"
		gen area= "`areaz'"
		gen state= "`stz'"
		
		gen tripid = ceil(_n / 30)
		bysort tripid: gen catch_draw = _n
		sort tripid catch_draw
	}
	
		tempfile domz`d'
		save `domz`d'', replace
		global domz "$domz "`domz`d''" " 
		
		
}

dsconcat $domz
ds month1 domain mode1 period2 area state, not
local vars `r(varlist)'
foreach v of local vars{
	replace `v'=round(`v')
}

gen draw=`i'
split domain, parse("_")
drop mode1 period2 area state 

rename domain1 month
rename domain2 period
rename domain3 mode
rename domain4 area
rename domain5 state
destring month period state, replace
drop month1

order domain month period mode area state  
tostring period, gen(period1)
tostring state, gen(st2)
gen period2=mode+"_"+period1+"_"+area+"_"+st2
drop st2 period1
order domain month period mode area state tripid catch_draw draw period2
export delimited using "projection_catch_draw`i'_2019.csv", replace 
}
