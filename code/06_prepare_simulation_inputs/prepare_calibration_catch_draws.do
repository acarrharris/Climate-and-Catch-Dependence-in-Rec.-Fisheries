

* For each baseline year, create 50 datasets of baseline-year catch-per-trip by randomly sampling a subset of the observed data
* These data are used to calibrate the simulation model 

*OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 

*NOTE: this code will produce several large datasets. Please set up your data directories accordingly. 

version 17
clear all
set more off

ssc install dsconcat
ssc install renvarlab 

* Set global directories - user edit here
global dir_root "C:\Users\andrew.carr-harris\Desktop\Git\Climate-and-Catch-Dependence-in-Rec.-Fisheries" // root to Github directory
global code_root "${dir_root}/code" 
global process_data_root "${dir_root}/data" // data directory for all other model input and output files.

capture which dsconcat
if _rc {
    di as error "Required Stata command dsconcat is not installed."
    exit 199
}

capture which renvarlab
if _rc {
    di as error "Required Stata command renvarlab is not installed."
    exit 199
}

cd "${process_data_root}" //data stored here


local yrs 2019 2020 2021 2022 2023 2024
foreach y of local yrs{

	u "${process_data_root}/cod_hadd_catch_data.dta", clear
	keep if gom & target
	keep if year==`y'
	replace new_wp_int=round(new_wp_int)
	expand new_wp_int
	save "${process_data_root}/cod_hadd_catch_data_yr`y'.dta",  replace
	
}


global directed_trips_file "${process_data_root}/directed_trips_and_regulations.csv"

forvalues i = 1/50 {

    set seed `i'
	local yrs 2019 2020 2021 2022 2023 2024

    foreach y of local yrs {

        import delimited using "${directed_trips_file}", clear
        keep if year == `y'

        tostring month, gen(month_str)
        tostring period, gen(period_str)
        tostring st, replace

        gen domain = month_str + "_" + period_str + "_" + mode + "_" + area + "_" + st

        gen state = "MA" if st == "25"
        replace state = "NH" if st == "33"
        replace state = "ME" if st == "23"

        keep if dtrip > 0
        keep month period mode period2 area st domain state dtrip

        levelsof domain, local(doms)

        tempfile base results
        save `base', replace

        clear
        save `results', emptyok replace

        foreach d of local doms {

            use `base', clear
            keep if domain == "`d'"

            levelsof month, local(mon) clean
            levelsof mode, local(md) clean
            levelsof period2, local(pdz) clean
            levelsof area, local(areaz) clean
            levelsof state, local(stz) clean

            use "${process_data_root}/cod_hadd_catch_data_yr`y'.dta", clear

            gen area = "inshore" if inlist(area_x, "1", "5")
            replace area = "offshore" if area_x == "2"

            capture confirm numeric variable month
            if !_rc {
                gen month1 = month
            }
            else {
                destring month, gen(month1)
            }

            replace mode1 = "fh" if inlist(mode1, "hd", "ch")

            keep if month1 == `mon'
            keep if mode1 == "`md'"
            keep if area == "`areaz'"
            keep if state == "`stz'"

            count
            local n = r(N)

            if `n' >= 3000 {

                keep tot_cat_cod tot_cat_hadd month1 cod_release hadd_release cod_landing hadd_landing
                sample 3000, count

            }
            else if `n' == 0 {

                clear
                set obs 3000
                gen tot_cat_cod = 0
                gen tot_cat_hadd = 0
				gen cod_release = 0
                gen hadd_release = 0
				gen cod_landing = 0
                gen hadd_landing = 0
                gen month1 = `mon'

            }
            else {

                local expand_n = ceil(3000 / `n')
                expand `expand_n'
                sample 3000, count
                keep tot_cat_cod tot_cat_hadd month1 cod_release hadd_release cod_landing hadd_landing

            }

            gen domain = "`d'"
            gen mode1 = "`md'"
            gen period2 = "`pdz'"
            gen area = "`areaz'"
            gen state = "`stz'"
            gen replicate = `i'

            gen tripid = ceil(_n / 30)
            bysort tripid: gen catch_draw = _n

            append using `results'
            save `results', replace
        }

        use `results', clear
        sort domain tripid catch_draw

        * 100 tripids x 30 catch draws within every domain
        bysort domain: assert _N == 3000
        bysort domain tripid: assert _N == 30

	export delimited using "${process_data_root}/calib_catch_yr`y'_draw`i'.csv", replace
    }
}


