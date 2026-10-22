*Bottom Temperature Prediction Draws

* Set global directories - user edit here
global dir_root "C:\Users\andrew.carr-harris\Desktop\Git\Climate-and-Catch-Dependence-in-Rec.-Fisheries" // root to Github directory
global code_root "${dir_root}/code" 
global process_data_root "${dir_root}/data" // data directory for all other model input and output files.

use "${process_data_root}/decadal_bottom_temp_distributions_levels.dta", clear

set seed 31564789
local NREPL     50   
forvalues rep = 1/`NREPL'  {
	preserve
   gen proj_temp =rnormal(average_temp_offshore, sd_temp_offshore)
       keep decade month proj_temp
    drop if month < 3
   save "${process_data_root}/temps_2_`rep'.dta", replace
   restore
   
preserve
   gen proj_temp =rnormal(average_temp_inshore, sd_temp_inshore)
    keep decade month proj_temp
    drop if month < 3
    save "${process_data_root}/temps_1_`rep'.dta", replace
	restore
}