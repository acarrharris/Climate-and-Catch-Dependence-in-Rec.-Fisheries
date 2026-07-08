---
editor_options: 
  markdown: 
    wrap: 72
---

# Climate-Induced Species Shift and Catch Dependence in Recreational Fisheries

This is the code and data repository for the manuscript
**"Climate-Induced Species Shift and Catch Dependence in Recreational
Fisheries"** by Andrew Carr-Harris, Jorge Holzer, and Geret DePiper,
submitted to *Nature Communications*.

## Overview

This repository contains code and, where permitted, data required to
reproduce the analyses and figures presented in the manuscript.

Workflows are organized by folder and should be run sequentially. The
analysis uses **Stata**, **R**, and **MATLAB**, so each component must
be executed using the appropriate software.

Public MRIP datasets are not redistributed and can be downloaded from
NOAA Fisheries:
<https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads>

Unless otherwise noted, intermediate datasets are generally not archived
because they can be recreated from provided scripts and inputs.

## Data availability and access

-   MRIP public-use data: available from NOAA Fisheries.
-   Some survey datasets requiring NOAA-NMFS disclosure review are
    available upon request from Sabrina Lovell
    ([sabrina.lovell\@noaa.gov](mailto:sabrina.lovell@noaa.gov){.email}).
-   Confidential spatial inputs used to construct the fishing-area
    convex hull are confidential and cannot be distributed. Derived
    shapefiles that are not subject to confidentiality restrictions are
    provided in `data/derived_shapefiles`.

## Workflow

1.  Compile historical catch-per-trip and fishing effort (Stata)
2.  Compile historical and projected bottom temperatures (R)
3.  Climatology and projection bias correction (R)
4.  Estimate catch-per-trip regressions (Stata)
5.  Estimate copulas and project catch-per-trip (Stata + MATLAB)
6.  Compile data for fishery simulation (Stata)
7.  Estimate angler utility model (Stata)
8.  Compile angler ages and trip costs for fishery simulation (Stata)
9.  Run fishery simulation model (R)
10. Analyze outputs and generate manuscript figures (R)

## Notes and conventions

-   Several code scripts, inputs, and outputs are repeated across
    different years, waves (two-month period; 1-6), baseline analysis
    years (2019, 2020, 2021), fishing area (inshore or offshore), or
    fishing mode (private boat or for-hire) strata. We label these file
    names below with the set of possible stratum values enclosed within
    brackets.
-   Some archived scripts produce intermediate files for earlier years
    that were not included in the final analysis.

## Code repository structure with folder-by-folder details

Each folder listed below contains one or mode code scripts that must be
run sequentially.

### 1. `compile historical catch-per-trip and fishing effort`

Locates MRIP public-use files; builds datasets of (1) directed Gulf of
Maine cod/haddock trips and regulations, and (2) historical
catch-per-trip and angler characteristics.

Scripts:

-   `MRIP data wrapper.do`
-   `directed trips and regs GoM.do`
-   `historical catch per trip.do`

Inputs:

-   `trip_{1982,…,2022}{1,…,6}.dta`
-   `catch_{1982,…,2022}{1,…,6}.dta`
-   `ma_site_list_updated_SS.xlsx`

Outputs:

-   `directed trips and regulations 2010_2020_disaggregated.csv`
-   `cod_hadd_catch_data_1_15.dta`

### 2. `compile historical and projected bottom temperatures`

Creates fishing-area spatial boundaries and extracts historical and
projected bottom temperature data.

Scripts:

-   `historical_daily_bt_temps_GoM.R`
-   `cod_hadd_bt_temp_deltas.R`

Inputs:

-   shapefiles in `data/Stock_areas`
-   `VTR_convex_hull.gpkg` in `data/derived_shapefiles` (for downstream
    steps)
-   `bottom_temp_combined_product_1959_2022_soe.nc`
-   `CM2.6_2xCO2_USNES_bottom_temp_delta_monthly_years_1_40.nc`
    (projected deltas)
-   `CM2.6_2xCO2_USNES_bottom_temp_delta_monthly_years_41_80.nc`
    (projected deltas)

Outputs:

-   `VTR_convex_hull.gpkg`
-   `hist_daily_bt_tmps_agg_GoM.xlsx`
-   `mean_bottom_temp_deltas_GOM_convex_hull_yrs_1_80.xlsx`

### 3. `climatology and projection bias correction`

Bias-corrects projected bottom temperature distributions.

Scripts:

-   `Bottom_temp_script_levels_GOM_cod_haddock_FINAL_NC.R`

Inputs:

-   `hist_daily_bt_tmps_agg_GoM.xlsx`

Outputs:

-   `decadal_bottom_temp_distributions_levels_3_12_2025.xlsx`

### 4. `catch-per-trip regressions`

Estimates cod and haddock catch-per-trip models using lognormal Type II
Tobit models conditional on temperature, biomass, and other covariates;
saves outputs for copula estimation and projection.

Scripts:

-   `Cod_Haddock_Catch_Regressions.do`

Inputs:

-   `hist_daily_bt_tmps_agg_GoM.xlsx`
-   `SSB_WGOM cod 2023 RT sel3ahl Run.xlsx`
-   `cod_hadd_catch_data_1_15.dta`
-   `stock_cod_haddock.dta`

Outputs:

-   `parameters_SUEST_April4_2025_area{1,2}newmode{2,3}.xlsx`
-   `sigma_SUEST_April4_2025_area{1,2}newmode{2,3}.xlsx`
-   `residuals_April4_area{1,2}newmode{2,3}cod.txt`
-   `residuals_April4_area{1,2}newmode{2,3}haddock.txt`
-   `Cod_Haddock_catch_Regression_Tobit2_results_April4_2025.dta`

### 5. `copula estimation and catch-per-trip projection`

Prepares biomass and effort distributions; estimates copulas and
generates correlated catch-per-trip projections for cod and haddock
across copulas.

Requires MATLAB Patton copula toolbox functions included in the
repository.

Scripts:

-   `distribution_ff12.do`
-   `correlated_catch_per_trip_Area*_Newmode*_*.m`

Inputs:

-   `Cod_Haddock_catch_Regression_Tobit2_results_April4_2025.dta`
-   `decadal_bottom_temp_distributions_levels_3_12_2025.xlsx`
-   `parameters_SUEST_April4_2025_area{1,2}newmode{2,3}.xlsx`
-   `sigma_SUEST_April4_2025_area{1,2}newmode{2,3}.xlsx`
-   `residuals_April4_area{1,2}newmode{2,3}cod.txt`
-   `residuals_April4_area{1,2}newmode{2,3}haddock.txt`
-   `normalized_ssb.xlsx`
-   `ffdays12_area{1,2}_{2019,2020,2021}.xlsx`
-   Patton copula toolbox functions (included in folder; must be on
    MATLAB path)

Outputs:

-   `correlated_sample_{Clayton,Frank,Gumbel,Plackett,Gaussian}_area{1,2}_newmode{2,3}_{2019,2020,2021}.csv`

### 6. `compile data for fishery simulation`

Generate 100 datasets of baseline-year catch-per-trip by randomly
sampling a subset of the observed catch-per-trip data; repeat for
projection period using projected catch-per-trip data.

Scripts:

-   `calibration catch by period.do`
-   `projection catch by period and baseline year.do`

Inputs:

-   `cod_hadd_catch_data_1_15.dta`
-   `directed trips and regulations 2010_2020_disaggregated.csv`
-   `correlated_sample_{Clayton,Frank,Gumbel,Plackett,Gaussian}_area{1,2}_newmode{2,3}_{2019,2020,2021}.csv`

Outputs:

-   `cod_hadd_catch_data_1_15_yr{2019,2020,2021}.dta`
-   `calib_catch_yr{2019,2020,2021}_draw{1,…100}.csv`
-   `projection_catch_draw{1,…100}_{2019,2020,2021}.csv`

### 7. `angler survey and choice experiment analysis`

Processes angler survey data; prepares survey demographics + choice
experiment data; estimates mixed logit model; exports parameters and
Krinsky–Robb draws for willingness-to-pay calculation; plots
willingness-to-pay calculations.

Scripts:

-   `Survey_data_analysis_NC.do`

Inputs: - `narfs_ne_conjoint_weighted.dta` (pending NOAA-NMFS review;
request from
[sabrina.lovell\@noaa.gov](mailto:sabrina.lovell@noaa.gov){.email}) -
`narfs_ne_conjoint_weighted_V3.dta` (pending NOAA-NMFS review; request
from
[sabrina.lovell\@noaa.gov](mailto:sabrina.lovell@noaa.gov){.email}) -
`trip_attributes.xlsx`

Outputs:

-   `angler_CE_demographics.csv`
-   `cod_parameters.csv`
-   `k_r_draws.dta`

### 8. `angler and angler trip population characteristics`

Estimates trip costs and angler age distributions for fishery
simulation.

Scripts:

-   `expenditure survey trip costs NC.do`
-   `age_distribution NC.do`

Inputs:

-   `atl_states_2017_expsurvey.dta` (pending NOAA-NMFS review; request
    from
    [sabrina.lovell\@noaa.gov](mailto:sabrina.lovell@noaa.gov){.email})
-   `Demographics_ME to NC.pdf` (source of hardcoded information)

Outputs:

-   estimated mean trip cost and standard errors (hardcoded in the
    fishery simulation)
-   `age_distribution_by_state.csv`

### 9. `fishery simulation`

Sets directory and globals for fishery simulation; pulls in relevant
baseline-year and projection period data; calibrates the fishery
simulation to each baseline year; runs the fishery simulation for the
projection period; processes bottom temperature data and model output
data for displaying and reporting results.

Scripts:

-   `model wrapper 2010-2020 NC.R` (calls all subsequent scripts in this
    folder)
-   `find p-star values 2010-2020.R`
-   `find p-star values 2010-2020_ind.R`
-   `calibration loop2 2010-2020.R`
-   `calibration loop2 2010-2020.R_ind.R`
-   `projection function new2.R`
-   `projection function new2_ind.R`
-   `projection function new2 - close fishery.R`

Inputs:

-   `calib_catch_yr{2019,2020,2021}_draw{1,…,100}.csv`
-   `directed trips and regulations 2010_2020_disaggregated.csv`
-   `projection_catch_draw{1,…,100}_{2019,2020,2021}.csv`
-   `Discard_Mortality.csv`
-   mixed logit parameter means and standard deviations (as reported in
    the paper) from step #7
-   `age_distribution_by_state.csv`
-   `angler_CE_demographics.csv`
-   estimated mean trip cost and standard errors from step #8
-   `age_distribution_by_state.csv`

Outputs:

-   `costs_data_{2019,2020,2021}_draw{1,…,100}.rds`
-   `costs_data_{2019,2020,2021}_draw{1,…,100}_ind.rds`
-   `calibration_data_{2019,2020,2021}_draw{1,…,100}.rds`
-   `calibration_data_{2019,2020,2021}_draw{1,…,100}_ind.rds`
-   `p_star_values_2019_2021_outliers.rds`
-   `p_star_values_2019_2021_ind_outliers.rds`
-   `model_output_y{2019,2020,2021}_outliers_newCV_2-17-26.xlsx`
-   `model_output_y{2019,2020,2021}_outliers_newCV_2-17-26.xlsx`
-   `ktau_output_y{2019,2020,2021}_outliers_newCV_2-17-26.xlsx`
-   `ktau_output_y{2019,2020,2021}_outliers_newCV_2-17-26.xlsx`
-   `model_output_y{2019,2020,2021}_outliers_newCV_2-17-26_close cod and haddock.xlsx`
-   `model_output_y{2019,2020,2021}_outliers_newCV_2-17-26_reformat_coast.rds`
-   `model_output_y{2019,2020,2021}_outliers_newCV_2-17-26_reformat_mode.rds`
-   `model_output_y{2019,2020,2021}_outliers_newCV_2-17-26_reformat_state.rds`
-   `bottom_temp_reformat1.xlsx`

### 10. `analyze and plot fishery simulation results`

Generates manuscript figures and summary statistics.

Scripts:

-   `manuscript plots.R`
-   `manuscript values.R`

Inputs:

-   `bottom_temp_reformat1.xlsx`
-   `ktau_output_y{2019,2020,2021}_outliers_10-8-25.xlsx`
-   `ktau_output_y{2019,2020,2021}_outliers_10-8-25_ind.xlsx`
-   `model_output_y{2019,2020,2021}_outliers_10-8-25_reformat_coast.rds`
-   `model_output_y{2019,2020,2021}_outliers_10-8-25_reformat_mode.rds`
-   `model_output_y{2019,2020,2021}_outliers_10-8-25_reformat_state.rds`
-   `model_output_y{2019,2020,2021}_outliers_9-21-25_close cod and haddock.xlsx`

Outputs:

-   `projection_temps.jpg`
-   `havest_cv_plot{2019,2020,2021}.jpg`
-   `ktau_plot{2019,2020,2021}.jpg`
-   `cv_by_mode_allyears.jpg`
-   `cv_by_state_allyears.jpg`
