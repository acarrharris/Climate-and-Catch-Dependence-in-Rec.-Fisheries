

*Generate a dataset of directed trips and regulations for GoM cod and haddock 2010-2019 using MRIP public-use datasets

cd "[enter data directory]" //data stored here

local yearz 2019 2020 2021
local statez 23 33 25

global yrz
foreach y of local yearz{
clear

global fluke_effort

tempfile tl1 cl1
dsconcat $triplist
drop if strmatch(id_code, "*xx*")==1
duplicates drop 

/* dtrip will be used to estimate total directed trips, do not change it*/
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge
 
keep if inlist(year, `y') 

/* THIS IS THE END OF THE DATA MERGING CODE */

tempfile new 
save `new', replace 

foreach s of local statez{
	u `new', clear 
	
 /* ensure only relevant states */
 *keep if inlist(st,23, 33, 25)
keep if inlist(st,`s')


 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "atlanticcod") 
replace dom_id="1" if strmatch(prim1_common, "atlanticcod") 

replace dom_id="1" if strmatch(common, "haddock") 
replace dom_id="1" if strmatch(prim1_common, "haddock") 

tostring wave, gen(w2)
tostring year, gen(year2)
gen st2 = string(st,"%02.0f")

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

drop date 
gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring month1, replace
destring day1, replace


gen period ="1" if day1<=15 & month1==1
replace period ="2" if day1>15 & month1==1
replace period ="3" if day1<=15 & month1==2
replace period ="4" if day1>15 & month1==2
replace period ="5" if day1<=15 & month1==3
replace period ="6" if day1>15 & month1==3
replace period ="7" if day1<=15 & month1==4
replace period ="8" if day1>15 & month1==4
replace period ="9" if day1<=15 & month1==5
replace period ="10" if day1>15 & month1==5
replace period ="11" if day1<=15 & month1==6
replace period ="12" if day1>15 & month1==6
replace period ="13" if day1<=15 & month1==7
replace period ="14" if day1>15 & month1==7
replace period ="15" if day1<=15 & month1==8
replace period ="16" if day1>15 & month1==8
replace period ="17" if day1<=15 & month1==9
replace period ="18" if day1>15 & month1==9
replace period ="19" if day1<=15 & month1==10
replace period ="20" if day1>15 & month1==10
replace period ="21" if day1<=15 & month1==11
replace period ="22" if day1>15 & month1==11
replace period ="23" if day1<=15 & month1==12
replace period ="24" if day1>15 & month1==12

gen date2=mdy(month1,day1,year)
format date2 %td


/*Deal with Group Catch -- this bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
Then it generates a flag for claim equal to the largest claim.  
Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1  */

replace claim=0 if claim==.

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (claim): gen claim_flag=claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n

keep if count_obs1==1

*Classify as GoM or Georges bank using NOAA-NEFSC stock delineations for Massachusetts MRIP sites
preserve 
import excel using "ma_site_list_updated_SS.xlsx", clear first
keep SITE_EXTERNAL_ID NMFS_STAT_AREA
renvarlab, lower
rename site_external_id intsite
tempfile mrip_sites
save `mrip_sites', replace 
restore

merge m:1 intsite using `mrip_sites',  keep(1 3)

gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,511, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area, 521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224

keep if dom_id=="1"
keep if area_s=="GOM"

gen my_dom_id_string=mode1+"_"+period+"_"+area_x

replace my_dom_id_string=ltrim(rtrim(my_dom_id_string)) //convert this string to a number, as total with over(<overvar>) requires a numeric variable

encode my_dom_id_string, gen(my_dom_id)

replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

sort  my_dom_id  year strat_id psu_id id_code
svy: total dtrip , over(my_dom_id)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

gen year=`y'
gen st=`s'

tempfile yrz`y'`s'
save `yrz`y'`s'', replace
global yrz "$yrz "`yrz`y'`s''" " 
}
}

dsconcat $yrz
keep my b year st
split my, parse(_)
rename my_dom_id_string1 mode
rename my_dom_id_string2 period
rename my_dom_id_string3 area_x
drop my
rename b dtrip
gen area = "inshore" if inlist(area_x, "1", "5")
replace area="offshore" if inlist(area_x, "2")

collapse (sum) dtrip, by(year st mode period area)
destring period, replace

gen month=1 if inlist(period, 1, 2) 
replace month=2 if inlist(period, 3, 4) & month==.
replace month=3 if inlist(period, 5, 6) & month==.
replace month=4 if inlist(period, 7, 8) & month==.
replace month=5 if inlist(period, 9, 10) & month==.
replace month=6 if inlist(period, 11, 12) & month==.
replace month=7 if inlist(period, 13, 14) & month==.
replace month=8 if inlist(period, 15, 16) & month==.
replace month=9 if inlist(period, 17, 18) & month==.
replace month=10 if inlist(period, 19, 20) & month==.
replace month=11 if inlist(period, 21, 22) & month==.
replace month=12 if inlist(period, 23, 24) & month==.
*sort dtrip


*Exclude shore-based trips (only 258 of 947,661 (<.1%) total directed trips from 2019-2021 were shore-based))
drop if mode=="sh"
sort year mode period area
rename mode mode1

save "regs_base.dta", replace //save baseline directed trips file

u "regs_base.dta", clear 

*expand dtrip dataste to generate a full time series
clear 
set obs 18
gen period=_n
replace period = period+5
gen mode1="fh"
expand 2, gen(dup)
replace mode1="pr" if dup==1
drop dup
gen area="inshore"
expand 2, gen(dup)
replace area="offshore" if dup==1
drop dup
gen st=23
expand 2, gen(dup)
replace st=33 if dup==1
drop dup
expand 2 if st==33, gen(dup)
replace st=25 if dup==1
drop dup

expand 12
bysort st period mode area: gen year=_n
replace year=2009+year
gen month=1 if inlist(period, 1, 2) 
replace month=2 if inlist(period, 3, 4) & month==.
replace month=3 if inlist(period, 5, 6) & month==.
replace month=4 if inlist(period, 7, 8) & month==.
replace month=5 if inlist(period, 9, 10) & month==.
replace month=6 if inlist(period, 11, 12) & month==.
replace month=7 if inlist(period, 13, 14) & month==.
replace month=8 if inlist(period, 15, 16) & month==.
replace month=9 if inlist(period, 17, 18) & month==.
replace month=10 if inlist(period, 19, 20) & month==.
replace month=11 if inlist(period, 21, 22) & month==.
replace month=12 if inlist(period, 23, 24) & month==.

merge 1:1 period mode1 area year st month using "regs_base.dta"
drop _merge


***************************************************************
* Create time series of haddock fishing regulations

* Clear existing data
clear

* Set the date range to cover all periods in the document
local start_date = date("2010-01-01", "YMD")
local end_date = date("2025-04-30", "YMD")

* Create a new dataset with one row per day
local obs = `end_date' - `start_date' + 1
set obs `obs'
generate date = `start_date' + _n - 1
format date %td

* Create variables for regulations
generate season_open = 0
generate for_hire_boat_size_limit = .
generate private_boat_size_limit = .
generate for_hire_boat_bag_limit = .
generate private_boat_bag_limit = .

* Macro to replace values for a specific date range
capture program drop replace_regs
program replace_regs
    syntax, start(string) end(string) ///
            for_hire_size(real) private_size(real) ///
            [for_hire_bag(string) private_bag(string)]
    
    * Convert dates
    local start_date = date("`start'", "YMD")
    local end_date = date("`end'", "YMD")
    
    * Replace season open
    replace season_open = 1 if inrange(date, `start_date', `end_date')
    
    * Replace size limits
    replace for_hire_boat_size_limit = `for_hire_size' if inrange(date, `start_date', `end_date')
    replace private_boat_size_limit = `private_size' if inrange(date, `start_date', `end_date')
    
    * Replace bag limits (handling 'no limit' case)
    if "`for_hire_bag'" != "" & "`for_hire_bag'" != "no limit" {
        replace for_hire_boat_bag_limit = `for_hire_bag' if inrange(date, `start_date', `end_date')
    }
    else if "`for_hire_bag'" == "no limit" {
        replace for_hire_boat_bag_limit = . if inrange(date, `start_date', `end_date')
    }
    
    if "`private_bag'" != "" & "`private_bag'" != "no limit" {
        replace private_boat_bag_limit = `private_bag' if inrange(date, `start_date', `end_date')
    }
    else if "`private_bag'" == "no limit" {
        replace private_boat_bag_limit = . if inrange(date, `start_date', `end_date')
    }
end

* Apply regulations for each period
* 2010-2011
replace_regs, start(2010-01-01) end(2010-04-30) for_hire_size(18) private_size(18) ///
    for_hire_bag("no limit") private_bag("no limit")
	
replace_regs, start(2010-05-01) end(2011-04-30) for_hire_size(18) private_size(18) ///
    for_hire_bag("no limit") private_bag("no limit")

* 2011 period 1
replace_regs, start(2011-05-01) end(2012-01-05) for_hire_size(18) private_size(18) ///
    for_hire_bag("no limit") private_bag("no limit")

* 2011 period 2
replace_regs, start(2012-01-06) end(2012-04-19) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)

* 2011 period 3
replace_regs, start(2012-04-20) end(2012-04-30) for_hire_size(18) private_size(18) ///
    for_hire_bag("no limit") private_bag("no limit")

* Continue with similar detailed logic for subsequent years
* 2012
replace_regs, start(2012-05-01) end(2013-04-30) for_hire_size(18) private_size(18) ///
    for_hire_bag("no limit") private_bag("no limit")

* 2013
replace_regs, start(2013-05-01) end(2014-04-30) for_hire_size(21) private_size(21) ///
    for_hire_bag("no limit") private_bag("no limit")

* 2014
replace_regs, start(2014-05-01) end(2014-08-31) for_hire_size(21) private_size(21) ///
    for_hire_bag(3) private_bag(3)
replace_regs, start(2014-11-01) end(2015-02-28) for_hire_size(21) private_size(21) ///
    for_hire_bag(3) private_bag(3)

* 2015
replace_regs, start(2015-05-01) end(2015-08-31) for_hire_size(17) private_size(17) ///
    for_hire_bag(3) private_bag(3)
replace_regs, start(2015-11-01) end(2016-02-29) for_hire_size(17) private_size(17) ///
    for_hire_bag(3) private_bag(3)

* 2016
replace_regs, start(2016-05-01) end(2017-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2017-04-15) end(2017-04-30) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)

* 2017 period 1
replace_regs, start(2017-05-01) end(2017-07-26) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)

* 2017 period 2
replace_regs, start(2017-07-27) end(2017-09-16) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)
replace_regs, start(2017-11-01) end(2018-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)
replace_regs, start(2018-04-15) end(2018-04-30) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)

* 2018
replace_regs, start(2018-05-01) end(2018-09-16) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)
replace_regs, start(2018-11-01) end(2019-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)
replace_regs, start(2019-04-15) end(2019-07-04) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)

* 2019
replace_regs, start(2019-07-05) end(2020-02-29) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2020-04-15) end(2020-04-30) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)

* 2020-2021
replace_regs, start(2020-05-01) end(2021-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2021-04-01) end(2021-04-30) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2021-05-01) end(2022-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2022-04-01) end(2022-08-29) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)

* 2022
replace_regs, start(2022-08-30) end(2023-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(20) private_bag(20)
replace_regs, start(2023-04-01) end(2023-08-13) for_hire_size(17) private_size(17) ///
    for_hire_bag(20) private_bag(20)

* 2023
replace_regs, start(2023-08-14) end(2024-02-28) for_hire_size(18) private_size(17) ///
    for_hire_bag(15) private_bag(10)
replace_regs, start(2024-04-01) end(2024-04-30) for_hire_size(18) private_size(17) ///
    for_hire_bag(15) private_bag(10)

* 2024
replace_regs, start(2024-05-01) end(2025-02-28) for_hire_size(18) private_size(18) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2025-04-01) end(2025-04-30) for_hire_size(18) private_size(18) ///
    for_hire_bag(15) private_bag(15)

* Label variables for clarity
label variable date "Calendar Date"
label variable season_open "Fishing Season Open (1=Yes, 0=No)"
label variable for_hire_boat_size_limit "For-hire Boat Size Limit (inches)"
label variable private_boat_size_limit "Private Boat Size Limit (inches)"
label variable for_hire_boat_bag_limit "For-hire Boat Bag Limit"
label variable private_boat_bag_limit "Private Boat Bag Limit"

*keep if season_open==1

* Sort by date
sort date

* Compute how many days were open in each bi-monthly period (two per month; day 1-15, day 16-end). For periods that encompass two distinct sets of cod or haddock regulations, assign that period the set of regulations that covered the majority of days in that period
gen month=month(date)
gen day=day(date)
gen year=year(date)
drop if year>2021

gen period ="1" if day<=15 & month==1
replace period ="2" if day>15 & month==1
replace period ="3" if day<=15 & month==2
replace period ="4" if day>15 & month==2
replace period ="5" if day<=15 & month==3
replace period ="6" if day>15 & month==3
replace period ="7" if day<=15 & month==4
replace period ="8" if day>15 & month==4
replace period ="9" if day<=15 & month==5
replace period ="10" if day>15 & month==5
replace period ="11" if day<=15 & month==6
replace period ="12" if day>15 & month==6
replace period ="13" if day<=15 & month==7
replace period ="14" if day>15 & month==7
replace period ="15" if day<=15 & month==8
replace period ="16" if day>15 & month==8
replace period ="17" if day<=15 & month==9
replace period ="18" if day>15 & month==9
replace period ="19" if day<=15 & month==10
replace period ="20" if day>15 & month==10
replace period ="21" if day<=15 & month==11
replace period ="22" if day>15 & month==11
replace period ="23" if day<=15 & month==12
replace period ="24" if day>15 & month==12

replace for_hire_boat_size_limit=100 if season_open!=1
replace private_boat_size_limit=100 if season_open!=1
replace for_hire_boat_bag_limit=0 if season_open!=1
replace private_boat_bag_limit=0 if season_open!=1

replace for_hire_boat_bag_limit=100 if season_open==1 & private_boat_bag_limit==.
replace private_boat_bag_limit=100 if season_open==1 & private_boat_bag_limit==.
 

preserve 
drop for_hire_boat_size_limit for_hire_boat_bag_limit
rename private_boat_size_limit min_size
rename private_boat_bag_limit bag_limit
gen mode="pr"
tempfile pr
save `pr', replace
restore

drop private_boat_size_limit private_boat_bag_limit
rename for_hire_boat_size_limit min_size
rename for_hire_boat_bag_limit bag_limit
gen mode="fh"
append using `pr'

gen area="inshore"
expand 2, gen(dup)
replace area="offshore" if dup==1
drop dup
gen st=23
expand 2, gen(dup)
replace st=33 if dup==1
drop dup
expand 2 if st==33, gen(dup)
replace st=25 if dup==1
drop dup

drop if inlist(period, "1", "2", "3", "4")

preserve 
gen ndays=1
tostring min_size, gen(min2)
tostring bag_limit, gen(bag2)
gen min_bag=min2+"_"+bag2

egen sum_ndays=sum(ndays), by(year period mode area st )
collapse (sum) season_open (mean) sum_ndays=sum_ndays, by(year period mode area st min_size bag_limit)
gen perc_open=season_open/sum_ndays
duplicates tag year period mode area st, gen(dup)
egen max_perc=max(perc_open), by(year period mode area st dup)
keep if (dup==0) | (dup==1 & max_perc==perc_open)
keep year period mode area st  perc_open min_size bag_limit
tempfile open
save `open', replace 
restore

merge m:1 year period mode area st min_size bag_limit using `open'
destring period, replace 

keep year period mode area st perc_open min_size bag_limit _merge
duplicates drop 
destring period, replace 
sort year period mode area st
drop if inlist(period, 1, 2, 3, 4)
drop if _merge==1
duplicates report year period mode area st, gen(dup)
drop _merge
drop perc

rename min hadd_min
rename bag hadd_bag

tostring period, gen(pd2)

* Temp save data
tempfile dates_hadd
save `dates_hadd', replace 



***************************************************************


* Create time series of cod fishing regulations

* Clear existing data
clear 

* Set the date range to cover all periods in the document
local start_date = date("2010-01-01", "YMD")
local end_date = date("2025-04-30", "YMD")

* Create a new dataset with one row per day
local obs = `end_date' - `start_date' + 1
set obs `obs'
generate date = `start_date' + _n - 1
format date %td

* Create variables for regulations
generate season_open = 0
generate for_hire_boat_size_limit = .
generate private_boat_size_limit = .
generate for_hire_boat_bag_limit = .
generate private_boat_bag_limit = .

* Macro to replace values for a specific date range
capture program drop replace_regs
program replace_regs
    syntax, start(string) end(string) ///
            for_hire_size(real) private_size(real) ///
            [for_hire_bag(string) private_bag(string)]
    
    * Convert dates
    local start_date = date("`start'", "YMD")
    local end_date = date("`end'", "YMD")
    
    * Replace season open
    replace season_open = 1 if inrange(date, `start_date', `end_date')
    
    * Replace size limits
    replace for_hire_boat_size_limit = `for_hire_size' if inrange(date, `start_date', `end_date')
    replace private_boat_size_limit = `private_size' if inrange(date, `start_date', `end_date')
    
    * Replace bag limits (handling 'no limit' case)
    if "`for_hire_bag'" != "" & "`for_hire_bag'" != "no limit" {
        replace for_hire_boat_bag_limit = `for_hire_bag' if inrange(date, `start_date', `end_date')
    }
    else if "`for_hire_bag'" == "no limit" {
        replace for_hire_boat_bag_limit = . if inrange(date, `start_date', `end_date')
    }
    
    if "`private_bag'" != "" & "`private_bag'" != "no limit" {
        replace private_boat_bag_limit = `private_bag' if inrange(date, `start_date', `end_date')
    }
    else if "`private_bag'" == "no limit" {
        replace private_boat_bag_limit = . if inrange(date, `start_date', `end_date')
    }
end

* Apply regulations for each period
* 2010
replace_regs, start(2010-05-01) end(2010-10-31) for_hire_size(24) private_size(24) ///
    for_hire_bag(10) private_bag(10)
	
* 2011 period 1
replace_regs, start(2011-04-16) end(2011-04-30) for_hire_size(24) private_size(24) ///
    for_hire_bag(10) private_bag(10)

* 2011 period 2
replace_regs, start(2011-05-01) end(2011-10-31) for_hire_size(24) private_size(24) ///
    for_hire_bag(10) private_bag(10)

* 2012 period 1
replace_regs, start(2012-04-16) end(2012-04-30) for_hire_size(24) private_size(24) ///
    for_hire_bag(10) private_bag(10)

* Continue with similar detailed logic for subsequent years
* 2012 period 2
replace_regs, start(2012-05-01) end(2012-10-31) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)

* 2013 period 1
replace_regs, start(2013-04-16) end(2013-04-30) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)

* 2013 period 2
replace_regs, start(2013-05-01) end(2013-10-31) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)

* 2014 period 1
replace_regs, start(2014-04-16) end(2014-04-30) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)	
	
* 2014 period 2
replace_regs, start(2014-05-01) end(2014-08-31) for_hire_size(21) private_size(21) ///
    for_hire_bag(9) private_bag(9)
	
/* 2015 period 1
replace_regs, start(2014-04-16) end(2014-04-30) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)	
		*add different regs by state below
*/
	
* 2016 
replace_regs, start(2016-08-01) end(2016-09-30) for_hire_size(24) private_size(24) ///
    for_hire_bag(1) private_bag(1)	

* 2017 
replace_regs, start(2017-08-01) end(2017-09-30) for_hire_size(24) private_size(24) ///
    for_hire_bag(1) private_bag(1)		

*2018 closed 

* 2019	
replace_regs, start(2019-09-15) end(2019-09-30) for_hire_size(21) private_size(21) ///
    for_hire_bag(1) private_bag(1)		

* 2020 
**add different regs by mode below
	
* 2021
**add different regs by mode below

*2022 
replace_regs, start(2022-04-01) end(2022-04-14) for_hire_size(21) private_size(21) ///
    for_hire_bag(1) private_bag(1)


* Label variables for clarity
label variable date "Calendar Date"
label variable season_open "Fishing Season Open (1=Yes, 0=No)"
label variable for_hire_boat_size_limit "For-hire Boat Size Limit (inches)"
label variable private_boat_size_limit "Private Boat Size Limit (inches)"
label variable for_hire_boat_bag_limit "For-hire Boat Bag Limit"
label variable private_boat_bag_limit "Private Boat Bag Limit"

*keep if season_open==1

* Sort by date
sort date

* Compute how many days were open in each bi-monthly period (two per month; day 1-15, day 16-end). For periods that encompass two distinct sets of cod or haddock regulations, assign that period the set of regulations that covered the majority of days in that period
gen month=month(date)
gen day=day(date)
gen year=year(date)

gen period ="1" if day<=15 & month==1
replace period ="2" if day>15 & month==1
replace period ="3" if day<=15 & month==2
replace period ="4" if day>15 & month==2
replace period ="5" if day<=15 & month==3
replace period ="6" if day>15 & month==3
replace period ="7" if day<=15 & month==4
replace period ="8" if day>15 & month==4
replace period ="9" if day<=15 & month==5
replace period ="10" if day>15 & month==5
replace period ="11" if day<=15 & month==6
replace period ="12" if day>15 & month==6
replace period ="13" if day<=15 & month==7
replace period ="14" if day>15 & month==7
replace period ="15" if day<=15 & month==8
replace period ="16" if day>15 & month==8
replace period ="17" if day<=15 & month==9
replace period ="18" if day>15 & month==9
replace period ="19" if day<=15 & month==10
replace period ="20" if day>15 & month==10
replace period ="21" if day<=15 & month==11
replace period ="22" if day>15 & month==11
replace period ="23" if day<=15 & month==12
replace period ="24" if day>15 & month==12

replace for_hire_boat_size_limit=100 if season_open!=1
replace private_boat_size_limit=100 if season_open!=1
replace for_hire_boat_bag_limit=0 if season_open!=1
replace private_boat_bag_limit=0 if season_open!=1

replace for_hire_boat_bag_limit=100 if season_open==1 & private_boat_bag_limit==.
replace private_boat_bag_limit=100 if season_open==1 & private_boat_bag_limit==.
 
preserve 
drop for_hire_boat_size_limit for_hire_boat_bag_limit
rename private_boat_size_limit min_size
rename private_boat_bag_limit bag_limit
gen mode="pr"
tempfile pr
save `pr', replace
restore

drop private_boat_size_limit private_boat_bag_limit
rename for_hire_boat_size_limit min_size
rename for_hire_boat_bag_limit bag_limit
gen mode="fh"
append using `pr'

gen area="inshore"
expand 2, gen(dup)
replace area="offshore" if dup==1
drop dup
gen st=23
expand 2, gen(dup)
replace st=33 if dup==1
drop dup
expand 2 if st==33, gen(dup)
replace st=25 if dup==1
drop dup
drop if inlist(period, "1", "2", "3", "4")

preserve 
gen ndays=1
tostring min_size, gen(min2)
tostring bag_limit, gen(bag2)
gen min_bag=min2+"_"+bag2

egen sum_ndays=sum(ndays), by(year period mode area st )
collapse (sum) season_open (mean) sum_ndays=sum_ndays, by(year period mode area st min_size bag_limit)
gen perc_open=season_open/sum_ndays
duplicates tag year period mode area st, gen(dup)
egen max_perc=max(perc_open), by(year period mode area st dup)
keep if (dup==0) | (dup==1 & max_perc==perc_open)
keep year period mode area st  perc_open min_size bag_limit
tempfile open
save `open', replace 
restore

merge m:1 year period mode area st min_size bag_limit using `open'
destring period, replace 

keep year period mode area st perc_open min_size bag_limit _merge
duplicates drop 
destring period, replace 
sort year period mode area st
drop if inlist(period, 1, 2, 3, 4)
drop if _merge==1
duplicates report year period mode area st, gen(dup)
drop _merge
drop perc

rename min cod_min
rename bag cod_bag


* Merge to haddock time series
merge 1:1 year period mode area st using `dates_hadd'
sort year period mode area st
drop if year>2021

drop _merge 

** Add additional regulations 

* additional cod regulations 
* In 2015, the federal cod fishery was closed, but the closure was delayed at the state level, as per Scott Steinback: "2015 was the first closure year and it took time for the states to get on board. The fishery was closed in federal waters, but the states of ME, NH, and MA allowed harvest as long as it occurred in state waters. MA had a bag=1, min size = 19" that year. Thus, MA anglers generally just followed the state regs no matter where they caught the fish because enforcement generally occurs on shore. ME did close their state fishery as well in 2015, but not until Aug. 8 that year. NH was the big culprit where they kept the 9 fish bag limit that year."

*regulation adjustments
replace cod_bag= 1 if year==2015 & period>=8 & period<=16 & inlist(st, 25)
replace cod_min= 19 if year==2015 & period>=8  & period<=16 & inlist(st, 25)

replace cod_bag= 9 if year==2015 & period>=9 & period<=16 & st==33
replace cod_min= 21 if year==2015 & period>=9 & period<=16 & st==33

replace cod_bag= 1 if year==2015 & period>=8 & period<=15 & inlist(st, 23)
replace cod_min= 19 if year==2015 & period>=8  & period<=15 & inlist(st, 23)

*2020 
replace cod_bag= 1 if year==2020 & inlist(period, 18) & mode=="pr"
replace cod_min= 21  if year==2020 & inlist(period, 18) & mode=="pr"

replace cod_bag= 1 if year==2020 & inlist(period, 17, 18) & mode=="fh"
replace cod_min= 21  if year==2020 &  inlist(period, 17, 18) & mode=="fh"


*2021 
replace cod_bag= 1 if year==2021 & inlist(period, 7) & mode=="pr"
replace cod_min= 21  if year==2021 & inlist(period, 7) & mode=="pr"

replace cod_bag= 1 if year==2021 & inlist(period, 7) & mode=="fh"
replace cod_min= 21  if year==2021 &  inlist(period, 7) & mode=="fh"

replace cod_bag= 1 if year==2021 & inlist(period, 18) & mode=="pr"
replace cod_min= 21  if year==2021 & inlist(period, 18) & mode=="pr"

replace cod_bag= 1 if year==2021 & inlist(period, 17, 18) & mode=="fh"
replace cod_min= 21  if year==2021 &  inlist(period, 17, 18) & mode=="fh"

order year period mode area st 
sort  year period mode area st 
rename mode mode1

tostring st, gen(st2)

mvencode cod_bag hadd_bag, mv(0) override
mvencode hadd_min cod_min, mv(100)  override

merge 1:1  year period mode1 area st using  "regs_base.dta"

sort year period mode1 area st
drop  st2 
drop _merge

replace month=2 if inlist(period, 3, 4) & month==.
replace month=3 if inlist(period, 5, 6) & month==.
replace month=4 if inlist(period, 7, 8) & month==.
replace month=5 if inlist(period, 9, 10) & month==.
replace month=6 if inlist(period, 11, 12) & month==.
replace month=7 if inlist(period, 13, 14) & month==.
replace month=8 if inlist(period, 15, 16) & month==.
replace month=9 if inlist(period, 17, 18) & month==.
replace month=10 if inlist(period, 19, 20) & month==.
replace month=11 if inlist(period, 21, 22) & month==.
replace month=12 if inlist(period, 23, 24) & month==.

mvencode dtrip, mv(0) override
tab year
save "regs_base2.dta", replace

u  "regs_base2.dta", clear 


* Generate variables indicating *next year's* regulations
tempfile new
save `new', replace 

global yrz

levelsof year if year!=2021, local(yearz)

foreach y of local yearz{
local y_plus1=`y'+1

u `new', clear 
preserve 
keep if year==`y'
tempfile baseyear
save `baseyear', replace
restore

keep if year==`y_plus1'
keep period mode1 area st  month cod_bag cod_min hadd_bag hadd_min
ds period mode1 area st  month, not
renvarlab `r(varlist)', postfix(_t_plus1)
merge 1:1 period mode1 area st  month using `baseyear'

tempfile yrz`y'
save `yrz`y'', replace
global yrz "$yrz "`yrz`y''" " 

}
dsconcat $yrz
drop if year==.
tempfile new1
save `new1', replace 

*Now add the regs for 2021 
u "regs_base2.dta", clear 
keep if year==2021
append using `new1'

sort year mode area st period
order year month period mode1 area st mode1 dtrip cod_bag cod_min hadd_bag hadd_min cod_bag_t_plus1 cod_min_t_plus1 hadd_bag_t_plus1 hadd_min_t_plus1
drop _merge

tostring st, gen(st2)
drop pd2

rename mode1 mode
gen mode1=1 if mode=="fh"
gen mode2=1 if mode=="pr" 
mvencode mode1 mode2, mv(0)
tostring period, gen(period3)
gen period2 =mode+"_"+period3+"_"+area+"_"+st2
drop period3 st2
sort year month period mode1 area
export delimited using "directed trips and regulations 2010_2020_disaggregated.csv", replace 


