# Climate-Induced Species Shift and Catch Dependence in Recreational Fisheries

Code and data repository for the manuscript **"Climate-Induced Species Shift and Catch Dependence in Recreational Fisheries"** by Andrew Carr-Harris, Jorge Holzer, and Geret DePiper.

## Overview

This repository contains code and, where permitted, data required to reproduce the analyses and figures presented in the manuscript.

Workflows are organized by folder and should be run sequentially. The analysis uses **Stata**, **R**, and **MATLAB**, so each component must be executed using the appropriate software.

Public MRIP datasets are not redistributed and can be downloaded from NOAA Fisheries:
https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads

Intermediate datasets are generally not archived because they can be recreated from provided scripts and inputs.

## Data availability

- MRIP public-use data: available from NOAA Fisheries.
- Survey datasets requiring NOAA-NMFS disclosure review are available upon request from Sabrina Lovell (sabrina.lovell@noaa.gov).
- Confidential spatial inputs used to construct the fishing-area convex hull cannot be distributed. Derived non-confidential shapefiles are provided in `data/derived_shapefiles`.

## Workflow

1. Compile historical catch-per-trip and fishing effort (Stata)
2. Compile historical and projected bottom temperatures (R)
3. Climatology and projection bias correction (R)
4. Estimate catch-per-trip regressions (Stata)
5. Estimate copulas and project catch-per-trip (Stata + MATLAB)
6. Compile fishery simulation inputs (Stata)
7. Estimate angler choice experiment model (Stata)
8. Compile angler ages and trip costs (Stata)
9. Run fishery simulation model (R)
10. Analyze outputs and generate manuscript figures (R)

## Repository structure

### 1. `code/compile historical catch-per-trip and fishing effort`

Builds directed Gulf of Maine cod/haddock trips, regulations, historical catch-per-trip datasets, and angler characteristics.

Scripts:
- `MRIP data wrapper.do`
- `directed trips and regs GoM.do`
- `historical catch per trip.do`

Key outputs:
- `directed trips and regulations 2010_2020_disaggregated.csv`
- `cod_hadd_catch_data_1_15.dta`

### 2. `code/compile historical and projected bottom temperatures`

Creates fishing-area spatial boundaries and extracts historical and projected bottom temperature data.

Scripts:
- `historical_daily_bt_temps_GoM.R`
- `cod_hadd_bt_tmp_deltas.R`

Key outputs:
- `VTR_convex_hull.gpkg`
- `hist_daily_bt_tmps_agg_GoM.xlsx`
- `mean_bottom_temp_deltas_GOM_convex_hull_yrs_1_80.xlsx`

### 3. `code/climatology and projection bias correction`

Bias-corrects projected bottom temperature distributions.

Script:
- `Bottom_temp_script_levels_GOM_cod_haddock_FINAL_NC.R`

Output:
- `decadal_bottom_temp_distributions_levels_3_12_2025.csv`

### 4. `code/catch-per-trip regressions`

Estimates cod and haddock catch-per-trip models using lognormal Type II Tobit models conditional on temperature, biomass, and other covariates.

Script:
- `Cod_Haddock_Catch_Regressions.do`

Outputs include regression parameters, covariance estimates, residuals, and prediction datasets.

### 5. `code/copula estimation and catch-per-trip projection`

Estimates dependence between cod and haddock catch outcomes and generates correlated future catch projections.

Scripts:
- `distribution_ff12.do`
- `correlated_catch_per_trip_Area*_Newmode*_*.m`

Requires MATLAB Patton copula toolbox functions included in the repository.

### 6. `code/compile data for fishery simulation`

Generates baseline and projected catch-per-trip draws used by the simulation model.

Scripts:
- `calibration catch by period.do`
- `projection catch by period and baseline year.do`

Outputs:
- Calibration catch draws
- Projection catch draws

### 7. `code/angler survey and choice experiment analysis`

Processes angler survey data, estimates mixed logit models, and generates willingness-to-pay estimates.

Script:
- `Survey_data_analysis_NC.do`

Outputs:
- `angler_CE_demographics.csv`
- `cod_parameters.csv`
- `k_r_draws.dta`

### 8. `code/angler and angler trip population characteristics`

Estimates trip costs and angler age distributions.

Scripts:
- `expenditure survey trip costs NC.do`
- `age_distribution NC.do`

Outputs:
- Trip cost estimates
- `age_distribution_by_state.csv`

### 9. `code/fishery simulation`

Runs the recreational fishery simulation model, including calibration, projections, welfare calculations, and management scenarios.

Main script:
- `model wrapper 2010-2020.R`

This script calls calibration and projection routines contained in the same folder.

Primary outputs:
- Calibrated simulation objects
- Projected fishery outcomes
- Welfare estimates
- Kendall's tau dependence summaries
- Reformatted model outputs for plotting

### 10. `code/analyze and plot fishery simulation results`

Generates manuscript figures and summary statistics.

Scripts:
- `manuscript plots.R`
- `manuscript values.R`

Outputs include:
- Temperature projection figures
- Harvest and welfare plots
- Catch dependence plots
- State and fishing-mode summaries

## Notes

File names containing brackets indicate repeated files across years, waves, areas, modes, or simulation draws.

Some scripts generate intermediate files from earlier development stages that were not used in final manuscript analyses.
