


# This is the simulation model wrapper 

# Install packages and set globals 
args = commandArgs(trailingOnly=TRUE)
options(future.globals.maxSize= 1000000000)

packages <- c("tidyr",  "tidyverse", "reshape2","splitstackshape",
              "doBy","WriteXLS","Rcpp", "ggplot2", "rlist","fitdistrplus",
              "MASS",  "psych", "rgl","copula", "VineCopula","scales","univariateML",
              "logspline","readr","data.table","conflicted", "readxl", "writexl", 
              "furrr", "profvis", "future", "Hmisc", "tictoc","purrr" , "rlist",
              "rmdformats", "prettydoc", "hrbrthemes", "tint", "tufte", "rstatix", "ggpubr", 
              "future.apply", "DescTools", "listenv", "akima", "reshape2", "stringr")

# Install only those not already installed
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed])
# }

lapply(packages, library, character.only = TRUE)

library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)
options(scipen = 999)


input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions


# Simulation approach:
    # 1) calibrate the model by finding the value of p* for each draw (out of 100) of catch 
    #    that explains observed harvest. Save baseline trip outcomes, # choice occasions, ktau's
    # 2) Using those p* values and output data, re-run the model with projected catch-per-trip data
    #    for each copula/decade

# Part A - A1) calibrate the model to 2021 using observed 2021 catch-per-trip data that retains inter-species catch dependence
#          A2) calibrate the model to 2021 using 2021 catch-per-trip that explicitly removes inter-species catch dependence

# Part B - B1) project outcomes using copula data based on correlated residuals 
#          B2) project outcomes using copula data based on independent residuals              

# Part C - auxiliary -  close the baseline year fishery and compute welfare
#          C1) Close cod 
#          C2) Close cod and haddock    

# Part D - combine model output and format for integreation into Results   

###### PART A ##########

#### A1) Calibration

p_star_values_i<-list()
p_star_values_i_y<-list()

yrz<- c(2019, 2020, 2021)

for (y in yrz){
  for (i in 1:100){

  seed<-i
  
  # Pull in calibration-year catch draws
  
  # code for assigning zero to uncertain MRIP estimates
  catch_data_all_split <- readr::read_csv(
    paste0(input_data_cd,  "calib_catch_yr", y, "_draw", i, ".csv"), show_col_types = FALSE) %>% 
    dplyr::mutate(
      omit = dplyr::if_else(
        state == 23 & year == 2019 & month %in% c(5, 6), 1,0)) %>%
    dplyr::mutate(
      omit = dplyr::if_else(
        state == 23 & year == 2019 & month %in% c(9,10), 1,omit)) %>%
    dplyr::mutate(
      omit = dplyr::if_else(
        state == 23 & year == 2020 & month %in% c(5,6), 1,omit)) %>%
    dplyr::mutate(
      omit = dplyr::if_else(state == 23 & year == 2020 & month %in% c(9,10), 1,omit)) %>%
    dplyr::mutate(
      omit = dplyr::if_else(
        state == 25 & year == 2020 & month %in% c(11,12), 1,omit)) %>%
    dplyr::mutate(
      omit = dplyr::if_else(
        state == 23 & year == 2021 & month %in% c(9,10), 1,omit)) %>%
    dplyr::filter(omit != 1) %>%
    filter(tripid<=100)
  
  # code for assigning zero to uncertain MRIP estimates
  directed_trips <- readr::read_csv(
    paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
    show_col_types = FALSE
  )   %>%
    dplyr::mutate(dtrip = round(dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
    dplyr::mutate(
      dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
    dplyr::filter(dtrip != 0) %>%
    dplyr::filter(year == y)
  
    directed_trips_p <- directed_trips %>% 
    mutate(period2 = as.character(period2)) %>% mutate(n_trips = floor(dtrip), n_draws = n_draws) 
  
  regs <- directed_trips_p %>% dplyr::select(period2, cod_bag, cod_min, hadd_bag, hadd_min, dtrip)
  
  # number of simulated annual periods
  n_periods<-length(unique(regs$period2))
  
  regs_check <- directed_trips_p %>% dplyr::select(period2, dtrip)
  
  MRIP_data <- catch_data_all_split %>%
    dplyr::group_by(period2) %>%
    dplyr::summarise(mean_cod_keep = mean(landing_cod),
                     mean_hadd_keep = mean(landing_hadd),
                     mean_hadd_cat = mean(tot_cat_hadd),
                     mean_cod_cat = mean(tot_cat_cod),.groups="drop") %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(regs_check, by="period2") %>% 
    mutate(cod_harvest=mean_cod_keep*dtrip, 
           hadd_harvest=mean_hadd_keep*dtrip, 
           cod_tot_cat=mean_cod_cat*dtrip, 
           hadd_tot_cat=mean_hadd_cat*dtrip) %>% 
    dplyr::summarise(cod_harvest = sum(cod_harvest),
                     hadd_harvest = sum(hadd_harvest),
                     cod_tot_cat = sum(cod_tot_cat),
                     hadd_tot_cat = sum(hadd_tot_cat),.groups="drop")
  
  # Run the p-star routine and save the output
  source(paste0(input_code_cd,"find p-star values 2010-2020.R")) # this calls "calibration loop2 2010-2020.R"
  
  p_starz<-cbind(p_star_cod_variable, p_star_hadd_variable, cod_harvest_perc_diff, cod_harvest_diff, hadd_harvest_perc_diff, hadd_harvest_diff)
  
  p_starz<-p_starz %>% as.data.frame() %>% dplyr::mutate(draw=i, year=y)
  
  p_star_values_i[[i]] <-p_starz
  

}
  p_star_values_all= rlist::list.stack(p_star_values_i, fill=TRUE)
  p_star_values_i_y[[y]]= p_star_values_all
  
}

p_star_values_all_y= rlist::list.stack(p_star_values_i_y, fill=TRUE)
#saveRDS(p_star_values_all_y, file = paste0(input_data_cd, "p_star_values_2019_2021_outliers.rds"))
saveRDS(p_star_values_all_y, file = paste0(input_data_cd, "p_star_values_2019_2021_outliers_newCV.rds"))


###### PART B ##########
rm(list = ls())
input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)
options(scipen = 999)

##### A2) Calibration

p_star_values_i<-list()
p_star_values_i_y<-list()

yrz<- c(2019, 2020, 2021)
for (y in yrz){

  for (i in 1:100){

    seed<-i

    # Pull in calibration-year catch draws
    catch_data_all_split <- readr::read_csv(
      paste0(input_data_cd,  "calib_catch_yr", y, "_draw", i, ".csv"), show_col_types = FALSE) %>%
      dplyr::mutate(
        omit = dplyr::if_else(
          state == 23 & year == 2019 & month %in% c(5, 6), 1,0)) %>%
      dplyr::mutate(
        omit = dplyr::if_else(
          state == 23 & year == 2019 & month %in% c(9,10), 1,omit)) %>%
      dplyr::mutate(
        omit = dplyr::if_else(
          state == 23 & year == 2020 & month %in% c(5,6), 1,omit)) %>%
      dplyr::mutate(
        omit = dplyr::if_else(state == 23 & year == 2020 & month %in% c(9,10), 1,omit)) %>%
      dplyr::mutate(
        omit = dplyr::if_else(
          state == 25 & year == 2020 & month %in% c(11,12), 1,omit)) %>%
      dplyr::mutate(
        omit = dplyr::if_else(
          state == 23 & year == 2021 & month %in% c(9,10), 1,omit)) %>%
      dplyr::filter(omit != 1) %>%
      filter(tripid<=100)

     catch_data_all_split <- catch_data_all_split %>%
      dplyr::group_by(state, mode, area, month) %>%
      dplyr::mutate(
        tot_cat_cod_shuffled  = sample(tot_cat_cod),
        tot_cat_hadd_shuffled = sample(tot_cat_hadd)
      ) %>%
      ungroup()

    # # Compute and display per-group correlation before and after shuffling
    # cor_by_group <- catch_data_all_split %>%
    #   group_by(state, mode, area, month) %>%
    #   slice_sample(n=1000) %>%
    #   summarise(
    #     #cor_before = cor(tot_cat_cod, tot_cat_hadd, use = "complete.obs"),
    #     #cor_after  = cor(tot_cat_cod_shuffled, tot_cat_hadd, use = "complete.obs"),
    #     cor_before = cor.test(tot_cat_cod, tot_cat_hadd, method = c("kendall"))[["estimate"]],
    #     cor_after  = cor.test(tot_cat_cod_shuffled, tot_cat_hadd, method = c("kendall"))[["estimate"]],
    #     n = n()
    #   ) %>%
    #   ungroup()
    #
    # ggplot(cor_by_group, aes(x = cor_before, y = cor_after)) +
    #   geom_point(alpha = 0.6) +
    #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    #   coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
    #   labs(x = "Before shuffling", y = "After shuffling",
    #        title = "Per-Group Correlation: tot_cat_cod vs tot_cat_hadd")


    # Shuffle catch per-trip data to remove inter-species correlation
    catch_data_all_split <- catch_data_all_split %>%
      dplyr::mutate(
        tot_cat_cod  = tot_cat_cod_shuffled,
        tot_cat_hadd = tot_cat_hadd_shuffled) %>%
      dplyr::select(-tot_cat_cod_shuffled, -tot_cat_hadd_shuffled)

    # Directed trips and regulations by period
    # code for assigning zero to uncetrain MRIP estimates
    directed_trips <- readr::read_csv(
      paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
      show_col_types = FALSE)   %>%
      dplyr::mutate(dtrip = round(dtrip)) %>%
      dplyr::mutate(
        dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
      dplyr::mutate(
        dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
      dplyr::mutate(
        dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
      dplyr::mutate(
        dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
      dplyr::mutate(
        dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
      dplyr::mutate(
        dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
      dplyr::filter(dtrip != 0) %>%
      dplyr::filter(year == y) %>%
      dplyr::select(-year)

    directed_trips_p <- directed_trips %>%
      mutate(period2 = as.character(period2)) %>% mutate(n_trips = floor(dtrip), n_draws = n_draws)

    regs <- directed_trips_p %>% dplyr::select(period2, cod_bag, cod_min, hadd_bag, hadd_min, dtrip)

    #number of periods
    n_periods<-length(unique(regs$period2))

    regs_check <- directed_trips_p %>%   dplyr::select(period2, dtrip)

    MRIP_data <- catch_data_all_split %>%
      dplyr::group_by(period2) %>%
      dplyr::summarise(mean_cod_keep = mean(landing_cod),
                       mean_hadd_keep = mean(landing_hadd),
                       mean_hadd_cat = mean(tot_cat_hadd),
                       mean_cod_cat = mean(tot_cat_cod),.groups="drop") %>%
      dplyr::ungroup() %>%
      dplyr::left_join(regs_check, by="period2") %>%
      mutate(cod_harvest=mean_cod_keep*dtrip,
             hadd_harvest=mean_hadd_keep*dtrip,
             cod_tot_cat=mean_cod_cat*dtrip,
             hadd_tot_cat=mean_hadd_cat*dtrip) %>%
      dplyr::summarise(cod_harvest = sum(cod_harvest),
                       hadd_harvest = sum(hadd_harvest),
                       cod_tot_cat = sum(cod_tot_cat),
                       hadd_tot_cat = sum(hadd_tot_cat),.groups="drop")

    # Run the p-star routine and save the output
    source(paste0(input_code_cd,"find p-star values 2010-2020_ind.R")) # this calls "calibration loop2 2010-2020.R"

    p_starz<-cbind(p_star_cod_variable, p_star_hadd_variable, cod_harvest_perc_diff, cod_harvest_diff, hadd_harvest_perc_diff, hadd_harvest_diff)

    p_starz<-p_starz %>% as.data.frame() %>% dplyr::mutate(draw=i, year=y)

    p_star_values_i[[i]] <-p_starz


 }
  p_star_values_all= rlist::list.stack(p_star_values_i, fill=TRUE)
  p_star_values_i_y[[y]]= p_star_values_all

}
p_star_values_all_y= rlist::list.stack(p_star_values_i_y, fill=TRUE)

#saveRDS(p_star_values_all_y, file = paste0(input_data_cd, "p_star_values_2019_2021_ind_outliers.rds"))
saveRDS(p_star_values_all_y, file = paste0(input_data_cd, "p_star_values_2019_2021_ind_outliers_newCV.rds"))


###### PART B ##########

#### B1 and B2) projections


# Helpers 

# Return Kendall's tau only when both variables contain variation.
# A constant variable makes Kendall's tau undefined, not zero.

safe_kendall_tau <- function(x, y) {
  
  ok <- stats::complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]
  
  n_complete <- length(x)
  
  if (
    n_complete < 2L ||
    dplyr::n_distinct(x) < 2L ||
    dplyr::n_distinct(y) < 2L
  ) {
    return(
      tibble::tibble(
        tau = NA_real_,
        p_value = NA_real_,
        n_complete = n_complete,
        valid_tau = FALSE))
  }
  
  test <- suppressWarnings(
    stats::cor.test(x,y,
      method = "kendall",
      exact = FALSE))
  
  tibble::tibble(
    tau = unname(test$estimate),
    p_value = test$p.value,
    n_complete = n_complete,
    valid_tau = TRUE)
}


# Compute keep and total-catch tau within one state-mode-area-month stratum.
compute_stratum_tau <- function(data) {
  
  keep_tau <- safe_kendall_tau(
    data$tot_keep_cod_new,
    data$tot_keep_hadd_new
  )
  
  catch_tau <- safe_kendall_tau(
    data$tot_cat_cod_new,
    data$tot_cat_hadd_new
  )
  
  tibble::tibble(
    k_tau_keep_est = keep_tau$tau,
    k_tau_keep_p = keep_tau$p_value,
    n_keep_complete = keep_tau$n_complete,
    valid_keep_tau = keep_tau$valid_tau,
    
    k_tau_catch_est = catch_tau$tau,
    k_tau_catch_p = catch_tau$p_value,
    n_catch_complete = catch_tau$n_complete,
    valid_catch_tau = catch_tau$valid_tau,
    
    med_cod = stats::median(
      data$tot_cat_cod_new,
      na.rm = TRUE
    ),
    
    med_hadd = stats::median(
      data$tot_cat_hadd_new,
      na.rm = TRUE))
}


# Average stratum-level tau estimates.
#
# The weighted mean uses projected trips in each stratum.
# The unweighted mean is retained as a diagnostic.
#
# P-values are not averaged because an average p-value has no useful
# inferential interpretation.
aggregate_stratum_tau <- function(
    data,
    reporting_vars,
    domain_label = NULL
) {
  
  reporting_vars <- rlang::syms(reporting_vars)
  
  # Rename input columns so their names cannot be overwritten sequentially
  # inside dplyr::summarise().
  data_for_summary <- data %>%
    dplyr::mutate(
      tau_keep_stratum = k_tau_keep_est,
      tau_catch_stratum = k_tau_catch_est,
      med_cod_stratum = med_cod,
      med_hadd_stratum = med_hadd,
      stratum_weight = ntrips_alt,
      
      valid_keep_weighted =
        !is.na(tau_keep_stratum) &
        !is.na(stratum_weight) &
        stratum_weight > 0,
      
      valid_catch_weighted =
        !is.na(tau_catch_stratum) &
        !is.na(stratum_weight) &
        stratum_weight > 0
    )
  
  out <- data_for_summary %>%
    dplyr::group_by(!!!reporting_vars) %>%
    dplyr::summarise(
      
      # Projected-trip-weighted average across valid strata
      k_tau_keep_est = {
        use <- valid_keep_weighted
        
        if (any(use)) {
          stats::weighted.mean(
            x = tau_keep_stratum[use],
            w = stratum_weight[use]
          )
        } else {
          NA_real_
        }
      },
      
      k_tau_catch_est = {
        use <- valid_catch_weighted
        
        if (any(use)) {
          stats::weighted.mean(
            x = tau_catch_stratum[use],
            w = stratum_weight[use]
          )
        } else {
          NA_real_
        }
      },
      
      # Equal-weight average across strata with defined tau
      k_tau_keep_unweighted = {
        use <- !is.na(tau_keep_stratum)
        
        if (any(use)) {
          mean(tau_keep_stratum[use])
        } else {
          NA_real_
        }
      },
      
      k_tau_catch_unweighted = {
        use <- !is.na(tau_catch_stratum)
        
        if (any(use)) {
          mean(tau_catch_stratum[use])
        } else {
          NA_real_
        }
      },
      
      # Sensitivity result: treat undefined tau as zero
      k_tau_keep_zero_filled = {
        use <- !is.na(stratum_weight) & stratum_weight > 0
        
        if (any(use)) {
          stats::weighted.mean(
            x = tidyr::replace_na(
              tau_keep_stratum[use],
              0
            ),
            w = stratum_weight[use]
          )
        } else {
          NA_real_
        }
      },
      
      k_tau_catch_zero_filled = {
        use <- !is.na(stratum_weight) & stratum_weight > 0
        
        if (any(use)) {
          stats::weighted.mean(
            x = tidyr::replace_na(
              tau_catch_stratum[use],
              0
            ),
            w = stratum_weight[use]
          )
        } else {
          NA_real_
        }
      },
      
      med_cod = {
        use <- !is.na(med_cod_stratum) &
          !is.na(stratum_weight) &
          stratum_weight > 0
        
        if (any(use)) {
          stats::weighted.mean(
            x = med_cod_stratum[use],
            w = stratum_weight[use]
          )
        } else {
          NA_real_
        }
      },
      
      med_hadd = {
        use <- !is.na(med_hadd_stratum) &
          !is.na(stratum_weight) &
          stratum_weight > 0
        
        if (any(use)) {
          stats::weighted.mean(
            x = med_hadd_stratum[use],
            w = stratum_weight[use]
          )
        } else {
          NA_real_
        }
      },
      
      n_strata = dplyr::n(),
      
      n_valid_keep_strata = sum(
        !is.na(tau_keep_stratum)
      ),
      
      n_valid_catch_strata = sum(
        !is.na(tau_catch_stratum)
      ),
      
      projected_trips = sum(
        stratum_weight,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # P-values should not be averaged across strata
      k_tau_keep_p = NA_real_,
      k_tau_catch_p = NA_real_
    )
  
  if (!is.null(domain_label)) {
    out <- out %>%
      dplyr::mutate(
        domain1 = domain_label
      )
  }
  
  out
}


# Main functions and execution
# Parallel process the projection scenarios 

# Load data 
directed_trips_all <- readr::read_csv(
  file.path(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    dtrip = round(dtrip),
    dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(5, 6), 0, dtrip),
    dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9, 10), 0, dtrip),
    dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(5, 6), 0, dtrip),
    dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9, 10), 0, dtrip),
    dtrip = dplyr::if_else(st == 25 & year == 2020 & month %in% c(11, 12), 0, dtrip),
    dtrip = dplyr::if_else(st == 23 & year == 2021 & month %in% c(9, 10), 0, dtrip)
  )


p_star_correlated <- readRDS(
  file.path(input_data_cd, "p_star_values_2019_2021_outliers_newCV.rds")
)

p_star_independent <- readRDS(
  file.path(input_data_cd, "p_star_values_2019_2021_ind_outliers_newCV.rds")
)

projection_years <- c(2021, 2020, 2019)

run_projection_draw <- function(y, i, dependence = c("correlated", "independent")) {

  dependence <- match.arg(dependence)
  
  directed_trips_y <- directed_trips_all |>
    dplyr::filter(year == y, dtrip != 0) |>
    dplyr::select(-year)
  
  regs <- directed_trips_y |>
    dplyr::mutate(
      period2 = as.character(period2),
      n_trips = floor(dtrip),
      n_draws = n_draws
    ) |>
    data.table::as.data.table()
  
  data.table::setkey(regs, period2)
  
  regs_check <- directed_trips_y |>
    dplyr::select(period2, dtrip)
  
  if (dependence == "correlated") {
    p_star_y <- p_star_correlated |>
      dplyr::filter(year == y)
    projection_file <- "projection function new2.R"
    suffix <- ""
  } else {
    p_star_y <- p_star_independent |>
      dplyr::filter(year == y)
    projection_file <- "projection function new2_ind.R"
    suffix <- "_ind"
  }
  
  row <- p_star_y[p_star_y$draw == i, ]
  
  if (nrow(row) != 1L) {
    stop(
      "Expected exactly one p-star row for year ",
      y,
      ", draw ",
      i,
      ", dependence ",
      dependence,
      "."
    )
  }
  
  p_star_cod_variable <- row$p_star_cod_variable
  p_star_hadd_variable <- row$p_star_hadd_variable
  
  catch_data_all <- readr::read_csv(
    file.path(
      input_data_cd,
      paste0("projection_catch_draw", i, "_", y, "_new.csv")
    ),
    show_col_types = FALSE
  )
  
  # Use a reproducible, task-specific seed.
  set.seed(
    y * 100000L +
      i * 10L +
      ifelse(dependence == "independent", 1L, 0L)
  )
  
  source(
    file.path("C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/code/09_fishery_simulation", projection_file),
    local = environment()
  )
  
  writexl::write_xlsx(
    ktaus_all,
    file.path(
      output_data_cd,
      paste0("ktau_output_y", y, "_draw", i, suffix, "_new.xlsx")
    )
  )

  writexl::write_xlsx(
    ktaus_strata_all,
    file.path(
      output_data_cd,
      paste0("ktau_output_strata_y", y, "_draw", i, suffix, "_new.xlsx")
    )
  )

  writexl::write_xlsx(
    sims_all,
    file.path(
      output_data_cd,
      paste0("model_output_y", y, "_draw", i, suffix, "_new.xlsx")
    )
  )
  
  invisible(
    list(
      year = y,
      draw = i,
      dependence = dependence,
      completed = TRUE
    )
  )
}

run_projection_year_parallel <- function(
    y,
    dependence = c("correlated", "independent"),
    draws = 1:100
) {
  
  dependence <- match.arg(dependence)
  
  future.apply::future_lapply(
    X = draws,
    FUN = function(i, projection_year, dependence_type) {
      run_projection_draw(
        y = projection_year,
        i = i,
        dependence = dependence_type
      )
    },
    projection_year = y,
    dependence_type = dependence,
    future.seed = TRUE,
    future.packages = c(
      "data.table",
      "plyr",
      "dplyr",
      "tidyr",
      "readr",
      "writexl",
      "tibble",
      "rlang", 
      "rlist"
      
    ),
    future.globals = c(
      "run_projection_draw",
      "directed_trips_all",
      "p_star_correlated",
      "p_star_independent",
      "input_data_cd",
      "input_code_cd",
      "output_data_cd",
      "n_draws",
      "safe_kendall_tau",
      "compute_stratum_tau",
      "aggregate_stratum_tau"
    )
  )
}

available_workers <- future::availableCores()
workers_to_use <- max(1L, min(4L, available_workers - 1L))

future::plan(
  future::multisession,
  workers = workers_to_use
)

data.table::setDTthreads(1L)

projection_years <- c(2021, 2020, 2019)

# Start the clock!
ptm <- proc.time()

for (y in projection_years) {
  run_projection_year_parallel(
    y = y,
    dependence = "correlated",
    draws = 1:100
  )
  
  gc()
}
proc.time() - ptm # about 2 minutes per draw


for (y in projection_years) {
  run_projection_year_parallel(
    y = y,
    dependence = "independent",
    draws = 1:100
  )
  
  gc()
}


###### PART C ##########
####  C1) close cod

rm(list = ls())
library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/code/fishery simulation/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

for(yr in c(2019, 2020, 2021)){
  
  directed_trips <- readr::read_csv(
    paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
    show_col_types = FALSE)   %>%
    dplyr::mutate(dtrip = round(dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
    dplyr::mutate(
      dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
    dplyr::filter(dtrip != 0) %>%
    dplyr::filter(year == yr) %>% 
    dplyr::select(-year) %>% 
    dplyr::mutate(cod_bag=0) # set cod bag limit to zero
  
  regs <- directed_trips %>% 
    mutate(period2 = as.character(period2)) %>% 
    mutate(n_trips = floor(dtrip), n_draws = n_draws) 
  
  regs_check <- directed_trips %>% dplyr::select(period2, dtrip)
  
  k_taus<-list()
  output<-list()
  keep_rel_pairs_i<-list()
  
  for (i in 1:100){
    
    p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2021_outliers_newCV.rds")) %>%
      dplyr::filter(year==yr, draw==i)
    
    p_star_cod_variable<- p_starz$p_star_cod_variable
    p_star_hadd_variable<- p_starz$p_star_hadd_variable
    
    catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "calib_catch_yr", yr, "_draw", i, ".csv"), show_col_types = FALSE)) %>% 
      filter(tripid<=100)
    
    source(paste0(input_code_cd,"projection function new2 - close fishery.R")) 
    
    output[[i]]<-sims_all
  }
  
  output_all<-rlist::list.stack(output, fill=TRUE)
  #write_xlsx(output_all, paste0(output_data_cd,"model_output_y", yr, "_outliers_9-21-25_close cod.xlsx"))  #original output
  write_xlsx(output_all, paste0(output_data_cd,"model_output_y", yr, "_outliers_newCV_2-17-26_close cod.xlsx"))  #updated output
  
}


#### C2) close cod and haddock 
rm(list = ls())
library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

for(yr in c(2019, 2020, 2021)){
  
  directed_trips <- readr::read_csv(
    paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
    show_col_types = FALSE)   %>%
    dplyr::mutate(dtrip = round(dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
    dplyr::mutate(
      dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
    dplyr::filter(dtrip != 0) %>%
    dplyr::filter(year == yr) %>% 
    dplyr::select(-year) %>% 
    dplyr::mutate(cod_bag=0,hadd_bag=0) # set cod and haddock bag limit to zero
  
  regs <- directed_trips %>% 
    mutate(period2 = as.character(period2)) %>% 
    mutate(n_trips = floor(dtrip), n_draws = n_draws) 
  
  regs_check <- directed_trips %>% dplyr::select(period2, dtrip)
  
  k_taus<-list()
  output<-list()
  keep_rel_pairs_i<-list()
  
  for (i in 1:100){
    
    p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2021_outliers_newCV.rds")) %>%
      dplyr::filter(year==yr, draw==i)
    
    p_star_cod_variable<- p_starz$p_star_cod_variable
    p_star_hadd_variable<- p_starz$p_star_hadd_variable
    
    
    catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "calib_catch_yr", yr, "_draw", i, ".csv"), show_col_types = FALSE)) %>% 
      filter(tripid<=100)
    
    source(paste0(input_code_cd,"projection function new2 - close fishery.R")) 
    
    output[[i]]<-sims_all
  }
  
  output_all<-rlist::list.stack(output, fill=TRUE)
  #write_xlsx(output_all, paste0(output_data_cd,"model_output_y", yr, "_outliers_9-21-25_close cod and haddock.xlsx"))  #original output
  write_xlsx(output_all, paste0(output_data_cd,"model_output_y", yr, "_outliers_newCV_2-17-26_close cod and haddock.xlsx"))  #updated output
  
}




###### PART D ##########

#### Re-format the output data
# append output files for each individual draw by year.

for(yr in c(2019, 2020, 2021)){
  
# output from catch w/residual correlation
### Catch and welfare ### 
  if(yr %in% c(2019, 2020, 2021)){
    files_expected <- file.path(
      output_data_cd,  paste0("model_output_y", yr, "_draw", 1:100, "_new.xlsx"))
  }else{
files_expected <- file.path(
  output_data_cd,  paste0("model_output_y", yr, "_outliers_newCV_7-13-26_draw", 1:100, ".xlsx"))
}
# Check which exist
exists <- file.exists(files_expected)
if (any(!exists)) {
  message("Missing files:\n", paste(basename(files_expected[!exists]), collapse = "\n"))
}

files <- files_expected[exists]

# Read + append
dt_all <- data.table::rbindlist(
  lapply(files, function(f) {
    dt <- data.table::as.data.table(readxl::read_excel(f))
    dt
  }),
  use.names = TRUE,
  fill = TRUE
)
write_xlsx(dt_all, paste0(output_data_cd,"model_output_y", yr, "_outliers_newCV_7-13-26_ALLDRAWS.xlsx"))  #save the data


# output from catch w/out residual correlation
if(yr %in% c(2019, 2020, 2021)){
  files_expected <- file.path(
    output_data_cd,  paste0("model_output_y", yr, "_draw", 1:100, "_ind_new.xlsx"))
}else{
  files_expected <- file.path(
    output_data_cd,  paste0("model_output_y", yr,"_outliers_newCV_7-13-26_draw", 1:100, "_ind.xlsx"))
  
}

# Check which exist
exists <- file.exists(files_expected)
if (any(!exists)) {
  message("Missing files:\n", paste(basename(files_expected[!exists]), collapse = "\n"))
  }

files <- files_expected[exists]

# Read + append
dt_all <- data.table::rbindlist(
  lapply(files, function(f) {
    dt <- data.table::as.data.table(readxl::read_excel(f))
    dt
  }),
  use.names = TRUE,
  fill = TRUE
)
write_xlsx(dt_all, paste0(output_data_cd,"model_output_y", yr,"_outliers_newCV_7-13-26_ALLDRAWS_ind.xlsx"))  #save the data


### Kendall's tau calculation ### 
# output from catch w/residual correlation
if(yr %in% c(2019, 2020, 2021)){
  files_expected <- file.path(
    output_data_cd,  paste0("ktau_output_y", yr, "_draw", 1:100, "_new.xlsx"))
}else{
    files_expected <- file.path(
    output_data_cd,  paste0("ktau_output_y", yr,"_outliers_newCV_7-13-26_draw", 1:100, ".xlsx"))
  
}

# Check which exist
exists <- file.exists(files_expected)
if (any(!exists)) {
  message("Missing files:\n", paste(basename(files_expected[!exists]), collapse = "\n"))
}

files <- files_expected[exists]

# Read + append
dt_all <- data.table::rbindlist(
  lapply(files, function(f) {
    dt <- data.table::as.data.table(readxl::read_excel(f))
    dt
  }),
  use.names = TRUE,
  fill = TRUE
)
write_xlsx(dt_all, paste0(output_data_cd,"ktau_output_y", yr, "_outliers_newCV_7-13-26_ALLDRAWS.xlsx"))  #save the data



# output from catch w/out residual correlation
if(yr %in% c(2019, 2020, 2021)){
  files_expected <- file.path(
    output_data_cd,  paste0("ktau_output_y", yr, "_draw", 1:100, "_ind_new.xlsx"))
}else{
  files_expected <- file.path(
    output_data_cd,  paste0("ktau_output_y", yr,"_outliers_newCV_7-13-26_draw", 1:100, "_ind.xlsx"))
  
}


# Check which exist
exists <- file.exists(files_expected)
if (any(!exists)) {
  message("Missing files:\n", paste(basename(files_expected[!exists]), collapse = "\n"))
}

files <- files_expected[exists]

# Read + append
dt_all <- data.table::rbindlist(
  lapply(files, function(f) {
    dt <- data.table::as.data.table(readxl::read_excel(f))
    dt
  }),
  use.names = TRUE,
  fill = TRUE
)
write_xlsx(dt_all, paste0(output_data_cd,"ktau_output_y", yr,"_outliers_newCV_7-13-26_ALLDRAWS_ind.xlsx"))  #save the data
}


# Coastwide/mode-wide data aggregation
disc_mortality<- read_csv(paste0(input_data_cd, "Discard_Mortality.csv"), show_col_types = FALSE)
library(haven)

for(yr in c(2019, 2020, 2021)){

yr_minus1=yr-1

#model_output<- paste0("model_output_y", yr, "_outliers_10-8-25_ind.xlsx") #original output
model_output<- paste0("model_output_y", yr, "_outliers_newCV_7-13-26_ALLDRAWS_ind.xlsx") #updated output

historical_catch <- read_dta(paste0(input_data_cd,"cod_hadd_catch_data_1_15.dta"))
historical_catch<-historical_catch %>% 
  dplyr::filter(year>=2011 & year<=yr_minus1) %>% 
  dplyr::mutate(dtrip=1) %>% 
  dplyr::group_by(year) %>%
  summarise(across(c(tot_cat_cod, tot_cat_hadd, dtrip), \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% 
  dplyr::mutate(codcattrip=tot_cat_cod/dtrip, 
                haddcattrip=tot_cat_hadd/dtrip, 
                copula="baseline", 
                correlation="baseline", decade=0) %>% 
  dplyr::ungroup()

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, 
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_ind<-base_output_summarized %>% 
    dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort,
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="independent") 

  
# Decadal projections- independent residuals
output_summarized<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, 
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized<-output_summarized %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  dplyr::mutate(correlation="independent") %>% 
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, 
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice,
                     cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Decadal projections - correlated residuals
#model_output<- paste0("model_output_y", yr,"_outliers_10-8-25.xlsx") #original output
model_output<- paste0("model_output_y", yr,"_outliers_newCV_7-13-26_ALLDRAWS.xlsx") #updated output

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
    group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_corr<-base_output_summarized %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="observed") 


output_summarized_corr<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized_corr<-output_summarized_corr %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  #dplyr::mutate(copula="independent") %>% 
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Append the baseline output
output_combined <- output_summarized_corr %>%
  plyr::rbind.fill(output_summarized, base_output_summarized_corr, 
                   base_output_summarized_ind) #, historical_catch) 

output_combined0 <- output_combined %>%
  mutate(period = case_when(
    !is.na(year) & year >= 2011 & year <= yr ~ as.character(year),
    !is.na(decade) & decade >= 1 & decade <= 8 ~ paste0("Decade ", decade),
    TRUE ~ NA_character_
  ))

output_combined0$period <- as.character(output_combined0$period)
str(output_combined0$period)

#write_rds(output_combined0, paste0(output_data_cd,"model_output_y", yr,"_outliers_10-8-25_reformat_coast.rds"))  #save the data - original
write_rds(output_combined0, paste0(output_data_cd,"model_output_y", yr,"_outliers_newCV_7-13-26_reformat_coast.rds"))  #save the data -updated

# Coastwide by fishing mode data aggregation
#model_output<- paste0("model_output_y",yr,"_outliers_10-8-25_ind.xlsx") #original output
# model_output<- paste0("model_output_y", yr, "_outliers_newCV_7-13-26_ALLDRAWS_ind.xlsx") #updated output
# 
# base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
#   separate(domain, into = c("correlation", "rest"), sep = "_") %>%
#   mutate(copula = str_extract(rest, "^[a-z]+"),
#          decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
#   select(-rest) %>% 
#   dplyr::rename(codcat=tot_cat_cod_base,
#                 haddcat=tot_cat_hadd_base,
#                 codkeep=tot_keep_cod_base,
#                 haddkeep=tot_keep_hadd_base,
#                 codrel=tot_rel_cod_base,
#                 haddrel=tot_rel_hadd_base, 
#                 ntrips=ntrips_base)  %>% 
#   dplyr::left_join(disc_mortality, by="month") %>% 
#   dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
#                 hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
#   dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
#                 hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
#   group_by(draw, copula, correlation, decade, mode) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()
# 
# base_output_summarized_ind<-base_output_summarized %>% 
#   dplyr::mutate(codcattrip=codcat/ntrips, 
#                 codkeeptrip=codkeep/ntrips, 
#                 haddcattrip=haddcat/ntrips, 
#                 haddkeeptrip=haddkeep/ntrips,
#                 codcatchoice=codcat/n_choice_occasions, 
#                 codkeepchoice=codkeep/n_choice_occasions, 
#                 haddcatchoice=haddcat/n_choice_occasions, 
#                 haddkeepchoice=haddkeep/n_choice_occasions,
#                 year=yr, decade=0) %>%
#   group_by(year, draw, decade, mode) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions, 
#                      codcattrip, codkeeptrip, 
#                      haddcattrip, haddkeeptrip, 
#                      codcatchoice, codkeepchoice, 
#                      haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate(copula="independent") 
# 
# 
# # Decadal projections- independent residuals
# output_summarized<-read_excel(paste0(output_data_cd,model_output)) %>% 
#   separate(domain, into = c("correlation", "rest"), sep = "_") %>%
#   mutate(copula = str_extract(rest, "^[a-z]+"),
#          decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
#   select(-rest) %>% 
#   dplyr::rename(codcat=tot_cat_cod_new,
#                 haddcat=tot_cat_hadd_new,
#                 codkeep=tot_keep_cod_new,
#                 haddkeep=tot_keep_hadd_new,
#                 codrel=tot_rel_cod_new,
#                 haddrel=tot_rel_hadd_new, 
#                 ntrips=ntrips_alt, 
#                 cv=change_CS)  %>% 
#   dplyr::left_join(disc_mortality, by="month") %>% 
#   dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
#                 hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
#   dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
#                 hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
#   group_by(draw, copula, correlation, decade, mode) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()  
# 
# output_summarized<-output_summarized %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate(codcattrip=codcat/ntrips, 
#                 codkeeptrip=codkeep/ntrips, 
#                 haddcattrip=haddcat/ntrips, 
#                 haddkeeptrip=haddkeep/ntrips, 
#                 codcatchoice=codcat/n_choice_occasions, 
#                 codkeepchoice=codkeep/n_choice_occasions, 
#                 haddcatchoice=haddcat/n_choice_occasions, 
#                 haddkeepchoice=haddkeep/n_choice_occasions,
#                 cvtrip=cv/ntrips,
#                 cv_choice=cv/n_choice_occasions) %>% 
#   dplyr::mutate(correlation="independent") %>% 
#   group_by(draw, copula, correlation, decade, mode) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      codcattrip, codkeeptrip, 
#                      haddcattrip, haddkeeptrip, 
#                      codcatchoice, codkeepchoice, 
#                      haddcatchoice, haddkeepchoice, 
#                      ntrips, n_choice_occasions, 
#                      cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()  
# 
# 
# #Decadal projections - correlated residuals
# #model_output<- paste0("model_output_y",yr,"_outliers_10-8-25.xlsx") #original output
# model_output<- paste0("model_output_y", yr, "_outliers_newCV_7-13-26_ALLDRAWS.xlsx") #updated output
# 
# base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
#   separate(domain, into = c("correlation", "rest"), sep = "_") %>%
#   mutate(copula = str_extract(rest, "^[a-z]+"),
#          decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
#   select(-rest) %>% 
#   dplyr::rename(codcat=tot_cat_cod_base,
#                 haddcat=tot_cat_hadd_base,
#                 codkeep=tot_keep_cod_base,
#                 haddkeep=tot_keep_hadd_base,
#                 codrel=tot_rel_cod_base,
#                 haddrel=tot_rel_hadd_base, 
#                 ntrips=ntrips_base)  %>% 
#   dplyr::left_join(disc_mortality, by="month") %>% 
#   dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
#                 hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
#   dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
#                 hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
#   group_by(draw, copula, correlation, decade, mode) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()
# 
# base_output_summarized_corr<-base_output_summarized %>% 
#   dplyr::mutate(codcattrip=codcat/ntrips, 
#                 codkeeptrip=codkeep/ntrips, 
#                 haddcattrip=haddcat/ntrips, 
#                 haddkeeptrip=haddkeep/ntrips,
#                 codcatchoice=codcat/n_choice_occasions, 
#                 codkeepchoice=codkeep/n_choice_occasions, 
#                 haddcatchoice=haddcat/n_choice_occasions, 
#                 haddkeepchoice=haddkeep/n_choice_occasions,
#                 year=yr, decade=0) %>%
#   group_by(year, draw, decade, mode) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions, 
#                      codcattrip, codkeeptrip, 
#                      haddcattrip, haddkeeptrip, 
#                      codcatchoice, codkeepchoice, 
#                      haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate(copula="observed") 
# 
# 
# output_summarized_corr<-read_excel(paste0(output_data_cd,model_output)) %>% 
#   separate(domain, into = c("correlation", "rest"), sep = "_") %>%
#   mutate(copula = str_extract(rest, "^[a-z]+"),
#          decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
#   select(-rest) %>% 
#   dplyr::rename(codcat=tot_cat_cod_new,
#                 haddcat=tot_cat_hadd_new,
#                 codkeep=tot_keep_cod_new,
#                 haddkeep=tot_keep_hadd_new,
#                 codrel=tot_rel_cod_new,
#                 haddrel=tot_rel_hadd_new, 
#                 ntrips=ntrips_alt, 
#                 cv=change_CS)  %>% 
#   dplyr::left_join(disc_mortality, by="month") %>% 
#   dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
#                 hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
#   dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
#                 hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
#   group_by(draw, copula, correlation, decade, mode) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()  
# 
# output_summarized_corr<-output_summarized_corr %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate(codcattrip=codcat/ntrips, 
#                 codkeeptrip=codkeep/ntrips, 
#                 haddcattrip=haddcat/ntrips, 
#                 haddkeeptrip=haddkeep/ntrips, 
#                 codcatchoice=codcat/n_choice_occasions, 
#                 codkeepchoice=codkeep/n_choice_occasions, 
#                 haddcatchoice=haddcat/n_choice_occasions, 
#                 haddkeepchoice=haddkeep/n_choice_occasions,
#                 cvtrip=cv/ntrips,
#                 cv_choice=cv/n_choice_occasions) %>% 
#   #dplyr::mutate(copula="independent") %>% 
#   group_by(draw, copula, correlation, decade, mode) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      codcattrip, codkeeptrip, 
#                      haddcattrip, haddkeeptrip, 
#                      codcatchoice, codkeepchoice, 
#                      haddcatchoice, haddkeepchoice, 
#                      ntrips, n_choice_occasions, 
#                      cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()  
# 
# 
# #Append the baseline output
# output_combined <- output_summarized_corr %>%
#   plyr::rbind.fill(output_summarized, base_output_summarized_corr, 
#                    base_output_summarized_ind) #, historical_catch) 
# 
# 
# output_combined0 <- output_combined %>%
#   mutate(period = case_when(
#     !is.na(year) & year >= 2011 & year <= yr ~ as.character(year),
#     !is.na(decade) & decade >= 1 & decade <= 8 ~ paste0("Decade ", decade),
#     TRUE ~ NA_character_
#   ))
# 
# output_combined0$period <- as.character(output_combined0$period)
# str(output_combined0$period)
# 
# #write_rds(output_combined0, paste0(output_data_cd,"model_output_y", yr,"_outliers_10-8-25_reformat_mode.rds"))  #save the data - original 
# write_rds(output_combined0, paste0(output_data_cd,"model_output_y", yr,"_outliers_newCV_7-13-26_reformat_mode.rds"))  #save the data - updated
# 
# 
# # Model output by state aggregation
# 
# #model_output<- paste0("model_output_y", yr,"_outliers_10-8-25_ind.xlsx") #original output
# model_output<- paste0("model_output_y", yr, "_outliers_newCV_7-13-26_ALLDRAWS_ind.xlsx") #updated output
# 
# base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
#   separate(domain, into = c("correlation", "rest"), sep = "_") %>%
#   mutate(copula = str_extract(rest, "^[a-z]+"),
#          decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
#   select(-rest) %>% 
#   dplyr::rename(codcat=tot_cat_cod_base,
#                 haddcat=tot_cat_hadd_base,
#                 codkeep=tot_keep_cod_base,
#                 haddkeep=tot_keep_hadd_base,
#                 codrel=tot_rel_cod_base,
#                 haddrel=tot_rel_hadd_base, 
#                 ntrips=ntrips_base)  %>% 
#   dplyr::left_join(disc_mortality, by="month") %>% 
#   dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
#                 hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
#   dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
#                 hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
#   group_by(draw, copula, correlation, decade, state) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()
# 
# base_output_summarized_ind<-base_output_summarized %>% 
#   dplyr::mutate(codcattrip=codcat/ntrips, 
#                 codkeeptrip=codkeep/ntrips, 
#                 haddcattrip=haddcat/ntrips, 
#                 haddkeeptrip=haddkeep/ntrips,
#                 codcatchoice=codcat/n_choice_occasions, 
#                 codkeepchoice=codkeep/n_choice_occasions, 
#                 haddcatchoice=haddcat/n_choice_occasions, 
#                 haddkeepchoice=haddkeep/n_choice_occasions,
#                 year=yr, decade=0) %>%
#   group_by(year, draw, decade, state) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions, 
#                      codcattrip, codkeeptrip, 
#                      haddcattrip, haddkeeptrip, 
#                      codcatchoice, codkeepchoice, 
#                      haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate(copula="independent") 
# 
# 
# # Decadal projections- independent residuals
# output_summarized<-read_excel(paste0(output_data_cd,model_output)) %>% 
#   separate(domain, into = c("correlation", "rest"), sep = "_") %>%
#   mutate(copula = str_extract(rest, "^[a-z]+"),
#          decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
#   select(-rest) %>% 
#   dplyr::rename(codcat=tot_cat_cod_new,
#                 haddcat=tot_cat_hadd_new,
#                 codkeep=tot_keep_cod_new,
#                 haddkeep=tot_keep_hadd_new,
#                 codrel=tot_rel_cod_new,
#                 haddrel=tot_rel_hadd_new, 
#                 ntrips=ntrips_alt, 
#                 cv=change_CS)  %>% 
#   dplyr::left_join(disc_mortality, by="month") %>% 
#   dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
#                 hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
#   dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
#                 hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
#   group_by(draw, copula, correlation, decade, state) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()  
# 
# output_summarized<-output_summarized %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate(codcattrip=codcat/ntrips, 
#                 codkeeptrip=codkeep/ntrips, 
#                 haddcattrip=haddcat/ntrips, 
#                 haddkeeptrip=haddkeep/ntrips, 
#                 codcatchoice=codcat/n_choice_occasions, 
#                 codkeepchoice=codkeep/n_choice_occasions, 
#                 haddcatchoice=haddcat/n_choice_occasions, 
#                 haddkeepchoice=haddkeep/n_choice_occasions,
#                 cvtrip=cv/ntrips,
#                 cv_choice=cv/n_choice_occasions) %>% 
#   dplyr::mutate(correlation="independent") %>% 
#   group_by(draw, copula, correlation, decade, state) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      codcattrip, codkeeptrip, 
#                      haddcattrip, haddkeeptrip, 
#                      codcatchoice, codkeepchoice, 
#                      haddcatchoice, haddkeepchoice, 
#                      ntrips, n_choice_occasions, 
#                      cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()  
# 
# 
# #Decadal projections - correlated residuals
# #model_output<- paste0("model_output_y", yr,"_outliers_10-8-25.xlsx") #original output
# model_output<- paste0("model_output_y", yr, "_outliers_newCV_7-13-26_ALLDRAWS.xlsx") #updated output
# 
# base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
#   separate(domain, into = c("correlation", "rest"), sep = "_") %>%
#   mutate(copula = str_extract(rest, "^[a-z]+"),
#          decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
#   select(-rest) %>% 
#   dplyr::rename(codcat=tot_cat_cod_base,
#                 haddcat=tot_cat_hadd_base,
#                 codkeep=tot_keep_cod_base,
#                 haddkeep=tot_keep_hadd_base,
#                 codrel=tot_rel_cod_base,
#                 haddrel=tot_rel_hadd_base, 
#                 ntrips=ntrips_base)  %>% 
#   dplyr::left_join(disc_mortality, by="month") %>% 
#   dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
#                 hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
#   dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
#                 hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
#   group_by(draw, copula, correlation, decade, state) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()
# 
# base_output_summarized_corr<-base_output_summarized %>% 
#   dplyr::mutate(codcattrip=codcat/ntrips, 
#                 codkeeptrip=codkeep/ntrips, 
#                 haddcattrip=haddcat/ntrips, 
#                 haddkeeptrip=haddkeep/ntrips,
#                 codcatchoice=codcat/n_choice_occasions, 
#                 codkeepchoice=codkeep/n_choice_occasions, 
#                 haddcatchoice=haddcat/n_choice_occasions, 
#                 haddkeepchoice=haddkeep/n_choice_occasions,
#                 year=yr, decade=0) %>%
#   group_by(year, draw, decade, state) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions, 
#                      codcattrip, codkeeptrip, 
#                      haddcattrip, haddkeeptrip, 
#                      codcatchoice, codkeepchoice, 
#                      haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate(copula="observed") 
# 
# 
# output_summarized_corr<-read_excel(paste0(output_data_cd,model_output)) %>% 
#   separate(domain, into = c("correlation", "rest"), sep = "_") %>%
#   mutate(copula = str_extract(rest, "^[a-z]+"),
#          decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
#   select(-rest) %>% 
#   dplyr::rename(codcat=tot_cat_cod_new,
#                 haddcat=tot_cat_hadd_new,
#                 codkeep=tot_keep_cod_new,
#                 haddkeep=tot_keep_hadd_new,
#                 codrel=tot_rel_cod_new,
#                 haddrel=tot_rel_hadd_new, 
#                 ntrips=ntrips_alt, 
#                 cv=change_CS)  %>% 
#   dplyr::left_join(disc_mortality, by="month") %>% 
#   dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
#                 hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
#   dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
#                 hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
#   group_by(draw, copula, correlation, decade, state) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()  
# 
# output_summarized_corr<-output_summarized_corr %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate(codcattrip=codcat/ntrips, 
#                 codkeeptrip=codkeep/ntrips, 
#                 haddcattrip=haddcat/ntrips, 
#                 haddkeeptrip=haddkeep/ntrips, 
#                 codcatchoice=codcat/n_choice_occasions, 
#                 codkeepchoice=codkeep/n_choice_occasions, 
#                 haddcatchoice=haddcat/n_choice_occasions, 
#                 haddkeepchoice=haddkeep/n_choice_occasions,
#                 cvtrip=cv/ntrips,
#                 cv_choice=cv/n_choice_occasions) %>% 
#   #dplyr::mutate(copula="independent") %>% 
#   group_by(draw, copula, correlation, decade, state) %>%
#   summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
#                      haddcat, haddkeep, haddrel, 
#                      codcattrip, codkeeptrip, 
#                      haddcattrip, haddkeeptrip, 
#                      codcatchoice, codkeepchoice, 
#                      haddcatchoice, haddkeepchoice, 
#                      ntrips, n_choice_occasions, 
#                      cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
#   dplyr::ungroup()  
# 
# 
# #Append the baseline output
# output_combined <- output_summarized_corr %>%
#   plyr::rbind.fill(output_summarized, base_output_summarized_corr, 
#                    base_output_summarized_ind) #, historical_catch) 
# 
# 
# output_combined0 <- output_combined %>%
#   mutate(period = case_when(
#     !is.na(year) & year >= 2011 & year <= yr ~ as.character(year),
#     !is.na(decade) & decade >= 1 & decade <= 8 ~ paste0("Decade ", decade),
#     TRUE ~ NA_character_
#   ))
# 
# output_combined0$period <- as.character(output_combined0$period)
# str(output_combined0$period)
# 
# #write_rds(output_combined0, paste0(output_data_cd,"model_output_y", yr,"_outliers_10-8-25_reformat_state.rds"))  #save the data - original 
# write_rds(output_combined0, paste0(output_data_cd,"model_output_y", yr,"_outliers_newCV_7-13-26_reformat_state.rds"))  #save the data - updated

}

# Load and process projected bottom temp data
# bt_temp_project <- read_csv(file.path(input_data_cd, "decadal_bottom_temp_distributions_levels_3_12_2025.csv"), show_col_types = FALSE) %>%
#   mutate(
#     lower = average_temp - 1.96 * sd_temp,
#     upper = average_temp + 1.96 * sd_temp
#   ) %>%
#   group_by(decade) %>%
#   mutate(mean_decadal_tmp = mean(average_temp)) %>%
#   rename(decade2 = decade) %>%
#   dplyr::mutate(decade = case_when(
#     decade2 == 1 ~ "2021-2030",
#     decade2 == 2 ~ "2031-2040",
#     decade2 == 3 ~ "2041-2050",
#     decade2 == 4 ~ "2051-2060",
#     decade2 == 5 ~ "2061-2070",
#     decade2 == 6 ~ "2071-2080",
#     decade2 == 7 ~ "2081-2090",
#     decade2 == 8 ~ "2091-2100"
#   )) %>%
#   ungroup() %>%
#   select(-decade2) %>%
#   dplyr::rename(mean_temp = average_temp) %>%
#   mutate(month = as.numeric(month))

# Load and process historical bottom temp data
# By decade
# bt_temp_historical <- read_excel(file.path(input_data_cd, "hist_daily_bt_tmps_agg_GoM.xlsx")) %>%
#   mutate(
#     date = as.Date(date),
#     year = as.numeric(format(date, "%Y")),
#     month = as.numeric(format(date, "%m"))
#   ) %>%
#   group_by(year, month) %>%
#   summarise(mean_tmp_ym = mean(mean_tmp), .groups = "drop") %>%
#   mutate(decade = case_when(
#     year >= 1961 & year <= 1970 ~ "1961-1970",
#     year >= 1971 & year <= 1980 ~ "1971-1980",
#     year >= 1981 & year <= 1990 ~ "1981-1990",
#     year >= 1991 & year <= 2000 ~ "1991-2000",
#     year >= 2001 & year <= 2010 ~ "2001-2010",
#     year >= 2011 & year <= 2020 ~ "2011-2020"
#   )) %>%
#   filter(!is.na(decade)) %>%
#   group_by(decade, month) %>%
#   mutate(
#     mean_temp = mean(mean_tmp_ym),
#     sd_temp = sd(mean_tmp_ym),
#     lower = mean_temp - 1.96 * sd_temp,
#     upper = mean_temp + 1.96 * sd_temp
#   ) %>%
#   ungroup() %>%
#   group_by(decade) %>%
#   mutate(mean_decadal_tmp = mean(mean_temp)) %>%
#   ungroup()
# 
# historical_and_projections <- bind_rows(bt_temp_historical, bt_temp_project)
# write_xlsx(historical_and_projections, paste0(output_data_cd,"bottom_temp_reformat1.xlsx"))  #save the data
# 
# # By year
# # Load and process historical data
# bt_temp_historical_annual <- read_excel(file.path(input_data_cd, "hist_daily_bt_tmps_agg_GoM.xlsx")) %>%
#   mutate(
#     date = as.Date(date),
#     year = as.numeric(format(date, "%Y")),
#     month = as.numeric(format(date, "%m"))
#   ) %>%
#   group_by(year, month) %>%
#   summarise(mean_tmp_ym = mean(mean_tmp), .groups = "drop") %>%
#   dplyr::mutate(decade = case_when(
#     year == 2011  ~ "2011",
#     year == 2012  ~ "2012",
#     year == 2013  ~ "2013",
#     year == 2014  ~ "2014",
#     year == 2015  ~ "2015",
#     year == 2016  ~ "2016",
#     year == 2017  ~ "2017",
#     year == 2018  ~ "2018",
#     year == 2019  ~ "2019",
#     year == 2020  ~ "2020",
#     year == 2021  ~ "2021",
#     year == 2022  ~ "2022")) %>%
#   filter(!is.na(decade)) %>%
#   group_by(decade, month) %>%
#   mutate(
#     mean_temp = mean(mean_tmp_ym),
#     sd_temp = sd(mean_tmp_ym),
#     lower = mean_temp - 1.96 * sd_temp,
#     upper = mean_temp + 1.96 * sd_temp
#   ) %>%
#   ungroup() %>%
#   group_by(decade) %>%
#   mutate(mean_decadal_tmp = mean(mean_temp)) %>%
#   ungroup()
# 
# historical_and_projections_annual <- bind_rows(bt_temp_historical_annual, bt_temp_project)
# write_xlsx(historical_and_projections_annual, paste0(output_data_cd,"bottom_temp_reformat2.xlsx"))  #save the data


# Process the input catch-per-trip data
# Read the projected catch per trip .dta
# proj_data <- read_dta("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/projection_catch_base_wide.dta") %>%
#   rename(month = month1, mode = mode1) %>%
#   mutate(state = case_when(
#     state == "MA" ~ 25,
#     state == "ME" ~ 23,
#     state == "NH" ~ 33,
#     TRUE ~ NA_real_
#   ))
#
# # Collapse (mean) of had* and cod* by month/mode/area/state
# proj_data <- proj_data %>%
#   dplyr::group_by(month, mode, area, state) %>%
#   dplyr::summarise(across(starts_with("had"), \(x) mean(x, na.rm = TRUE)),
#                    across(starts_with("cod"),\(x) mean(x, na.rm = TRUE)),
#                    .groups = "drop")
#
# # Reshape wide to long format for multiple 'decade' groups
# proj_data_long <- proj_data %>%
#   pivot_longer(
#     cols = c(
#       starts_with("cod_corr_clayton"), starts_with("cod_ind_clayton"),
#       starts_with("had_corr_clayton"), starts_with("had_ind_clayton"),
#       starts_with("cod_corr_plackett"), starts_with("cod_ind_plackett"),
#       starts_with("had_corr_plackett"), starts_with("had_ind_plackett"),
#       starts_with("cod_corr_gaussian"), starts_with("cod_ind_gaussian"),
#       starts_with("had_corr_gaussian"), starts_with("had_ind_gaussian"),
#       starts_with("cod_corr_frank"), starts_with("cod_ind_frank"),
#       starts_with("had_corr_frank"), starts_with("had_ind_frank"),
#       starts_with("cod_corr_gumbel"), starts_with("cod_ind_gumbel"),
#       starts_with("had_corr_gumbel"), starts_with("had_ind_gumbel")
#     ),
#     names_to = c(".value", "decade"),
#     names_pattern = "(cod_corr_clayton|had_corr_clayton|cod_ind_clayton|had_ind_clayton|cod_corr_plackett|had_corr_plackett|cod_ind_plackett|had_ind_plackett|cod_corr_gaussian|had_corr_gaussian|cod_ind_gaussian|had_ind_gaussian|cod_corr_frank|had_corr_frank|cod_ind_frank|had_ind_frank|cod_corr_gumbel|had_corr_gumbel|cod_ind_gumbel|had_ind_gumbel)(.+)"
#   )
#
# # Read the directed trips CSV
# directed_trips <- fread("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/directed trips and regulations 2010_2020_disaggregated.csv") %>%
#   filter(year == 2021) %>%
#   rename(state = st)
#
# # Collapse (sum) dtrip by month/mode/area/state
# directed_trips_summarized <- directed_trips %>%
#   group_by(month, mode, area, state) %>%
#   summarise(dtrip = sum(dtrip, na.rm = TRUE), .groups = "drop")
#
# # Merge with reshaped projection data
# merged_data <- directed_trips_summarized %>%
#   left_join(proj_data_long, by = c("month", "mode", "area", "state")) %>%
#   dplyr::filter(!is.na(decade)) %>%  # equivalent to dropping _merge==1
#   select(-contains("ind"))  # drop *ind*
#
# # Collapse weighted means by decade
# final_result <- merged_data %>%
#   group_by(decade) %>%
#   summarise(
#     cod_corr_clayton = weighted.mean(cod_corr_clayton, dtrip, na.rm = TRUE),
#     had_corr_clayton = weighted.mean(had_corr_clayton, dtrip, na.rm = TRUE),
#     cod_corr_frank = weighted.mean(cod_corr_frank, dtrip, na.rm = TRUE),
#     had_corr_frank = weighted.mean(had_corr_frank, dtrip, na.rm = TRUE),
#     cod_corr_gumbel = weighted.mean(cod_corr_gumbel, dtrip, na.rm = TRUE),
#     had_corr_gumbel = weighted.mean(had_corr_gumbel, dtrip, na.rm = TRUE),
#     cod_corr_plackett = weighted.mean(cod_corr_plackett, dtrip, na.rm = TRUE),
#     had_corr_plackett = weighted.mean(had_corr_plackett, dtrip, na.rm = TRUE),
#     cod_corr_gaussian = weighted.mean(cod_corr_gaussian, dtrip, na.rm = TRUE),
#     had_corr_gaussian = weighted.mean(had_corr_gaussian, dtrip, na.rm = TRUE),
#     # If cod_ind and had_ind are still there
#     #cod_ind = weighted.mean(cod_ind, dtrip, na.rm = TRUE),
#     #had_ind = weighted.mean(had_ind, dtrip, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   select(-contains("ind"))  # drop *ind*
#
# write_xlsx(final_result, paste0(output_data_cd,"model_input_proj_data.xlsx"))  #save the data


########################
#### Re-format the output data BY MONTH#### 

# Coastwide/mode-wide data aggregation
disc_mortality<- read_csv(paste0(input_data_cd, "Discard_Mortality.csv"), show_col_types = FALSE)

library(haven)
for(yr in c(2019, 2020, 2021)){

yr_minus1=yr-1

model_output<- paste0("model_output_y",yr,"_outliers_10-8-25_ind.xlsx")

historical_catch <- read_dta(paste0(input_data_cd,"cod_hadd_catch_data_1_15.dta"))
historical_catch<-historical_catch %>% 
  dplyr::filter(year>=2011 & year<=yr_minus1) %>% 
  dplyr::mutate(dtrip=1)  %>% 
  dplyr::mutate(month=as.numeric(month)) %>% 
   dplyr::group_by(year, month) %>%
  summarise(across(c(tot_cat_cod, tot_cat_hadd, dtrip), \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% 
  dplyr::mutate(codcattrip=tot_cat_cod/dtrip, 
                haddcattrip=tot_cat_hadd/dtrip, 
                copula="baseline", 
                correlation="baseline", decade=0) %>% 
  dplyr::ungroup()

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, 
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_ind<-base_output_summarized %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort,
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="independent") 


# Decadal projections- independent residuals
output_summarized<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, 
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized<-output_summarized %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  dplyr::mutate(correlation="independent") %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, 
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice,
                     cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Decadal projections - correlated residuals
model_output<- paste0("model_output_y",yr,"_outliers_10-8-25.xlsx")

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_corr<-base_output_summarized %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="observed") 


output_summarized_corr<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized_corr<-output_summarized_corr %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Append the baseline output
output_combined <- output_summarized_corr %>%
  plyr::rbind.fill(output_summarized, base_output_summarized_corr, 
                   base_output_summarized_ind) #, historical_catch) 


output_combined0 <- output_combined %>%
  mutate(period = case_when(
    !is.na(year) & year >= 2011 & year <= yr ~ as.character(year),
    !is.na(decade) & decade >= 1 & decade <= 8 ~ paste0("Decade ", decade),
    TRUE ~ NA_character_
  ))

output_combined0$period <- as.character(output_combined0$period)
str(output_combined0$period)


write_rds(output_combined0, paste0(output_data_cd,"model_output_y",yr,"_outliers_10-8-25_reformat_coast_mnth.rds"))  #save the data
}





