
* For each baseline year, create 50 datasets of catch-per-trip by randomly sampling from the projected catch distribution. 
* Each replicate will contain catch draws for each of 8 decades. Columns will include projected catch of cod and haddock 
* by copula (including the case of independence). 

*OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 

*NOTE: this code will produce several large datasets. Please set up your data directories accordingly. 

version 17
clear all
set more off


* Set global directories - user edit here
global dir_root "C:\Users\andrew.carr-harris\Desktop\Git\Climate-and-Catch-Dependence-in-Rec.-Fisheries" // root to Github directory
global code_root "${dir_root}/code" 
global process_data_root "${dir_root}/data" // data directory for all other model input and output files.

global directed_trips_file "${process_data_root}/directed_trips_and_regulations.csv"


* Identify the range of observed catch-per-trip in order to truncate projected catch-per-trip to within that range
use "${process_data_root}/cod_hadd_catch_data.dta", clear

summarize tot_cat_cod, meanonly
local max_cod = r(max)

summarize tot_cat_hadd, meanonly
local max_hadd = r(max)

*Import the projected catch draws from the copula model
local list "simulated_catch_tempdraw_Clayton_area1_newmode2 simulated_catch_tempdraw_Clayton_area1_newmode3  simulated_catch_tempdraw_Clayton_area2_newmode2 simulated_catch_tempdraw_Clayton_area2_newmode3  simulated_catch_tempdraw_Frank_area1_newmode2 simulated_catch_tempdraw_Frank_area1_newmode3  simulated_catch_tempdraw_Frank_area2_newmode2  simulated_catch_tempdraw_Frank_area2_newmode3 simulated_catch_tempdraw_Gaussian_area1_newmode2 simulated_catch_tempdraw_Gaussian_area1_newmode3 simulated_catch_tempdraw_Gaussian_area2_newmode2 simulated_catch_tempdraw_Gaussian_area2_newmode3   simulated_catch_tempdraw_Gumbel_area1_newmode2 simulated_catch_tempdraw_Gumbel_area1_newmode3  simulated_catch_tempdraw_Gumbel_area2_newmode2 simulated_catch_tempdraw_Gumbel_area2_newmode3 simulated_catch_tempdraw_independent_area1_newmode2 simulated_catch_tempdraw_independent_area1_newmode3  simulated_catch_tempdraw_independent_area2_newmode2 simulated_catch_tempdraw_independent_area2_newmode3 simulated_catch_tempdraw_Plackett_area1_newmode2 simulated_catch_tempdraw_Plackett_area1_newmode3 simulated_catch_tempdraw_Plackett_area2_newmode2 simulated_catch_tempdraw_Plackett_area2_newmode3" 

local yrs 2019 2020 2021 2022 2023 2024

foreach y of local yrs{
foreach s of local list{
u "${process_data_root}/`s'_`y'.dta", clear

*bysort replicate decade month newmode state area: gen n=_n
*keep if n<=3000

sort decade month 
gen state=25 if state1==1
replace state=23 if state1==2
replace state=33 if state1==3

drop cell_weight 

gen source="`s'"
split source, parse(_)

replace catch_cod = `max_cod' if catch_cod > `max_cod'
replace catch_had = `max_hadd' if catch_had > `max_hadd'

drop area
levelsof source5, clean

if "`r(levels)'"=="area1"{
	gen area="inshore"
}

if "`r(levels)'"=="area2"{
	gen area="offshore"
}

levelsof source6, clean

if "`r(levels)'"=="newmode2"{
	gen mode="fh"
}

if "`r(levels)'"=="newmode3"{
	gen mode="pr"
}

gen area_tab= substr(source5, strlen(source5), 1)
gen mode_tab= substr(source6, strlen(source6), 1)
gen domain=copula+"_"+area_tab+"_"+mode_tab
levelsof domain, clean
local file="`r(levels)'"

di "`file'"

keep month decade catch_cod catch_had mode area state copula replicate
assert inrange(replicate,1,50)

sort replicate decade month mode state area
renvarlab, lower

compress

save "${process_data_root}/`file'.dta", replace

}

global directed_trips_file "${process_data_root}/directed_trips_and_regulations.csv"
local copulas "Clayton Frank Gumbel Plackett Gaussian independent"


import delimited using "${directed_trips_file}", clear

keep if year==`y'

keep if dtrip>0

tostring month, gen(month1)
tostring period, gen(period1)
tostring st, replace

gen domain=month1+"_"+period1+"_"+mode+"_"+area+"_"+st
destring st, gen(state)

keep year month period mode period2 area st domain state dtrip
encode domain, gen(domain2)

compress

assert !missing(year, month, period, mode, area, state, domain)
isid year domain

tempfile directed_base
save `directed_base', replace


quietly levelsof domain2 if year==`y', local(doms_`y')
local ndomains_`y' : word count `doms_`y''
display as text "Baseline year `y': `ndomains_`y'' simulation domains"


local catch_files "Clayton_1_2 Clayton_2_2 Clayton_1_3 Clayton_2_3 Frank_1_2 Frank_2_2 Frank_1_3 Frank_2_3 Gaussian_1_2 Gaussian_2_2 Gaussian_1_3 Gaussian_2_3 Gumbel_1_2 Gumbel_2_2 Gumbel_1_3 Gumbel_2_3 Plackett_1_2 Plackett_2_2 Plackett_1_3 Plackett_2_3 independent_1_2 independent_2_2 independent_1_3 independent_2_3"
	
local copulas "Clayton Frank Gumbel Plackett Gaussian independent"

local ncopulas : word count `copulas'
local ndecades=8
local nspecies=2
local expected_catchvars=`ncopulas'*`ndecades'*`nspecies'
set seed 123456789

    display as text "BASELINE YEAR `y'"

    use `directed_base' if year==`y', clear

    levelsof domain2, local(doms)
    local ndomains : word count `doms'

    display as text "Number of domains = `ndomains'"

    forvalues r=1/50 {

        display as text _newline "============================================================"
        display as text "YEAR `y': REPLICATE `r' OF 50"
        display as text "============================================================"


			foreach cop of local copulas {

				forvalues area_num=1/2 {

					forvalues mode_num=2/3 {

						local catchfile "`cop'_`area_num'_`mode_num'"

						display as text ///
							"  Loading `catchfile', replicate `r'"

						use catch_cod catch_had replicate decade month state ///
							using "${process_data_root}/`catchfile'.dta" ///
							if replicate==`r', clear

						drop replicate

						compress

						tempfile work_`cop'_`area_num'_`mode_num'

						save `work_`cop'_`area_num'_`mode_num'', replace
					}
				}
			}
			
        tempfile replicate_output
        local first_domain=1
        local ndom=0

        foreach d of local doms {

            local ++ndom

            display as text "  Domain `ndom' of `ndomains'"

            use `directed_base' if year==`y' & domain2==`d', clear

			assert _N==1

			local mon     = month[1]
			local md      = mode[1]
			local pd      = period[1]
			local areaz   = area[1]
			local stz     = state[1]
			local pdz     = period2[1]
			local domainz = domain[1]

			if "`md'"=="fh" local mode_num=2
			else if "`md'"=="pr" local mode_num=3
			else {
				display as error "Unknown mode: `md'"
				error 459
			}

			if "`areaz'"=="inshore" local area_num=1
			else if "`areaz'"=="offshore" local area_num=2
			else {
				display as error "Unknown area: `areaz'"
				error 459
			}
        tempfile domain_wide
        local first_decade=1

        forvalues dec=1/8 {
            tempfile decade_wide
            local first_copula=1

            foreach cop of local copulas {

			use `work_`cop'_`area_num'_`mode_num'' ///
				if decade==`dec' & month==`mon' & state==`stz', clear

			count
			local n_available=r(N)

			if `n_available'>0 {

				* Uniform random sampling with replacement.
				* Cod and haddock remain paired because entire rows are sampled.
				bsample 3000

				replace catch_cod=round(catch_cod)
				replace catch_had=round(catch_had)

				gen long sample_id=_n
			}
			else {
				display as text "    No catch data: replicate=`r', month=`mon', mode=`md', area=`areaz', state=`stz', decade=`dec', copula=`cop' -> zero catch"

				drop _all
				set obs 3000

				gen double catch_cod=0
				gen double catch_had=0
				gen long sample_id=_n
			}


                gen int tripid=ceil(sample_id/30)
				
                gen byte catch_draw=mod(sample_id-1,30)+1
                assert _N==3000
                assert inrange(tripid,1,100)
                assert inrange(catch_draw,1,30)

                bysort tripid: assert _N==30
                isid tripid catch_draw


                if "`cop'"=="Clayton" {
                    rename catch_cod catch_cod_clayton`dec'
                    rename catch_had catch_had_clayton`dec'
                }
                else if "`cop'"=="Frank" {
                    rename catch_cod catch_cod_frank`dec'
                    rename catch_had catch_had_frank`dec'
                }
                else if "`cop'"=="Gaussian" {
                    rename catch_cod catch_cod_gaussian`dec'
                    rename catch_had catch_had_gaussian`dec'
                }
                else if "`cop'"=="Gumbel" {
                    rename catch_cod catch_cod_gumbel`dec'
                    rename catch_had catch_had_gumbel`dec'
                }
                else if "`cop'"=="Plackett" {
                    rename catch_cod catch_cod_plackett`dec'
                    rename catch_had catch_had_plackett`dec'
                }
                else if "`cop'"=="independent" {
                    rename catch_cod catch_cod_independent`dec'
                    rename catch_had catch_had_independent`dec'
                }

                keep tripid catch_draw catch_cod_* catch_had_*

                if `first_copula' {
                    save `decade_wide', replace
                    local first_copula=0
                }
                else {
					merge 1:1 tripid catch_draw using `decade_wide', assert(3) nogen

                    save `decade_wide', replace
                }
            }

            use `decade_wide', clear

            assert _N==3000
            isid tripid catch_draw

            if `first_decade' {
                save `domain_wide', replace
                local first_decade=0
            }
            else {
                merge 1:1 tripid catch_draw using `domain_wide', assert(3) nogen
                save `domain_wide', replace
            }
        }

        use `domain_wide', clear

        assert _N==3000
        isid tripid catch_draw

        ds catch_cod* catch_had*
        local ncatchvars : word count `r(varlist)'
		assert `ncatchvars'==`expected_catchvars'
		
		gen baseline_year=`y'
		gen domain="`domainz'"
		gen month=`mon'
		gen period=`pd'
		gen mode="`md'"
		gen area="`areaz'"
		gen state=`stz'
		gen period2="`pdz'"
		gen int draw=`r'

		order baseline_year domain month period mode area state tripid catch_draw draw period2

        compress

        if `first_domain' {
            save `replicate_output', replace
            local first_domain=0
        }
        else {
            append using `replicate_output'
            save `replicate_output', replace
        }
    }


		use `replicate_output', clear

		sort domain tripid catch_draw

		bysort domain: assert _N==3000
		bysort domain tripid: assert _N==30
		isid domain tripid catch_draw
		assert draw==`r'
		assert baseline_year==`y'
		distinct domain
		assert r(ndistinct)==`ndomains'
		ds catch_cod* catch_had*
		local ncatchvars : word count `r(varlist)'
		assert `ncatchvars'==`expected_catchvars'
		distinct period2
		local nperiods=r(ndistinct)

		display as result "Year `y', draw `r': `ndomains' domains, `nperiods' periods, `ncatchvars' catch variables"

		compress

		export delimited using "${process_data_root}/projection_catch_draw`r'_`y'.csv", replace
    }   // end replicate loop

}       // end year loop
