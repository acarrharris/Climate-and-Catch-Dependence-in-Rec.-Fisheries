version 17
clear all
set more off

/*
Run this script from the repository root, for example:

    cd "C:/path/to/catch-dependence"

The MRIP public-use files should be placed in:
    data/external/mrip
*/

global project_root "`c(pwd)'"

global code_step01 ///
    "${project_root}/code/01_compile_historical_mrip"

global mrip_external ///
    "${project_root}/data/external/mrip"

global mrip_reference ///
    "${project_root}/data/raw_public/mrip_reference"

global processed_mrip ///
    "${project_root}/data/processed/historical_mrip"

global model_inputs ///
    "${project_root}/data/model_inputs"

global work_step01 ///
    "${project_root}/work/01_compile_historical_mrip"

/* Create generated-data directories when absent. */
capture mkdir "${project_root}/work"
capture mkdir "${work_step01}"

capture mkdir "${project_root}/data/processed"
capture mkdir "${processed_mrip}"

capture mkdir "${model_inputs}"

/* Run the scripts in dependency order. */
do "${code_step01}/mrip_data_wrapper.do"
do "${code_step01}/directed_trips_and_regs_GoM.do"
do "${code_step01}/historical_catch_per_trip.do"