version 17
clear all
set more off


*OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 

* Set global directories - user edit here
global dir_root "C:\Users\andrew.carr-harris\Desktop\Git\Climate-and-Catch-Dependence-in-Rec.-Fisheries" // root to Github directory
global code_root "${dir_root}/code" 
global mrip_data_root "${dir_root}/mrip_data"  // store raw MRIP trip_yyyyw.dta and catch_yyyyw.dta data here
global process_data_root "${dir_root}/data" // data directory for all other model input and output files.

global code_step01 "${code_root}/01_compile_historical_mrip"


foreach command in dsconcat renvarlab xsvmat {
    capture which `command'
    if _rc {
        display as error "Required Stata command `command' is not installed."
        exit 199
    }
}

*Run these scripts in order
do "${code_step01}/mrip_data_wrapper.do"
do "${code_step01}/directed_trips_and_regs_GoM.do"
do "${code_step01}/historical_catch_per_trip.do"

display as result "Step 01 completed successfully."
display as result "Processed outputs were written to: ${process_data_root}"