# Simulation model wrapper
# OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 

args <- commandArgs(trailingOnly = TRUE)

options(
  future.globals.maxSize = 1e9,
  scipen = 999
)

packages <- unique(c(
  "akima", "conflicted", "copula", "data.table", "DescTools", "doBy",
  "dplyr", "fitdistrplus", "furrr", "future", "future.apply", "ggplot2",
  "ggpubr", "here", "Hmisc", "hrbrthemes", "listenv", "logspline",
  "MASS", "plyr", "prettydoc", "profvis", "psych", "purrr", "readr",
  "readxl", "reshape2", "rgl", "rlist", "Rcpp", "rmdformats", "rstatix",
  "scales", "splitstackshape", "stringr", "tictoc", "tidyr", "tint",
  "tufte", "univariateML", "VineCopula", "WriteXLS", "writexl"
))

missing_packages <- setdiff(packages, rownames(utils::installed.packages()))

if (length(missing_packages) > 0) {
  utils::install.packages(missing_packages)
}

load_order <- c(
  setdiff(packages, c("conflicted", "plyr", "dplyr")),
  "plyr",
  "dplyr",
  "conflicted"
)

invisible(
  lapply(
    load_order,
    function(package) {
      suppressPackageStartupMessages(
        library(package, character.only = TRUE)
      )
    }
  )
)

conflicted::conflict_prefer("here", "here", quiet = TRUE)
conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("mutate", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("rename", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("summarize", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("summarise", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("count", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("arrange", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("group_by", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("ungroup", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("slice", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("lag", "dplyr", quiet = TRUE)


# adjust globals - user edit here 
input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/data/" 
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/code/09_fishery_simulation/"
proj_catch_cd <- input_data_cd
results_plot_code_cd <- input_code_cd



#number of choice ocassions per period and catch draws per period
n_draws <- 100L       # Simulated choice occasions per period
n_catch_draws <- 30L  # Catch draws per choice occasion

# Simulation approach:
    # 1) calibrate the model by finding the value of p* for each draw (out of 50) of catch 
    #    that explains observed harvest. Save baseline trip outcomes and number of choice occasions
    # 2) Using those p* values and baseline information, re-run the model with projected catch-per-trip data
    #    for each copula/decade

# Part A - A1) calibrate the model to baseline year YYYY 

# Part B - B1) project outcomes using dependent catch data
#          B2) project outcomes using independent catch data            

# Part C -  close cod and haddock in the baseline year by setting bag limits==0 and compute welfare.
#           This gives us an estimate of the total value of the fishery (of allowing angler harvest) in a base year.

# Part D - combine model output and format for integration into Results   



## State-wave combinations having directed cod or haddock trip estimates with PSEs
## greater than 50 according to MRIP data query website. This gets merged to catch
## and trip data and these strata are excluded.

zero_dtrip_combinations <- tibble::tribble(
  ~st, ~state, ~year, ~months,
  23, "ME",     2019, c(5, 6),
  23, "ME",     2019, c(9, 10),
  33, "NH",     2019, c(9, 10),
  25, "MA",     2019, c(11, 12),
  23, "ME",     2020, c(5, 6),
  23, "ME",     2020, c(9, 10),
  25, "MA",     2020, c(11, 12),
  25, "MA",     2021, c(3, 4),
  23, "ME",     2021, c(9, 10),
  25, "MA",     2021, c(11, 12),
  25, "MA",     2022, c(3, 4),
  25, "MA",     2022, c(9, 10),
  25, "MA",     2023, c(3, 4),
  23, "ME",     2023, c(5, 6),
  23, "ME",     2023, c(9, 10),
  23, "ME",     2024, c(5, 6),
  25, "MA",     2024, c(9, 10),
  25, "MA",     2024, c(11, 12)
) %>%
  tidyr::unnest_longer(months, values_to = "month") %>%
  dplyr::mutate(set_dtrip_zero = TRUE)


###### PART A ##########

#### A1) Calibration

p_star_values_i<-list()
p_star_values_i_y<-list()

yrz<- c(2019, 2020, 2021, 2022, 2023, 2024)
for (y in yrz){
  
  p_star_values_i <- vector("list", 50)
  
  for (i in 1:50){

  seed<-i
  
  checkpoint_file <- paste0(
    input_data_cd,
    "p_star_checkpoint_corr_", y, "_draw", i, ".rds"
  )
  
  if (file.exists(checkpoint_file)) {
    message("Loading completed checkpoint: year ", y, ", draw ", i)
    
    p_star_checkpoint <- readRDS(checkpoint_file)
    
    p_star_values_i[[i]] <- p_star_checkpoint %>%
      dplyr::select(
        p_star_cod_variable,
        p_star_hadd_variable,
        cod_harvest_perc_diff,
        cod_harvest_diff,
        hadd_harvest_perc_diff,
        hadd_harvest_diff,
        draw,
        year
      )
    
    next
  }
  
  # catch data 
  catch_data_all_split <- readr::read_csv(
    paste0(input_data_cd, "calib_catch_yr", y, "_draw", i, ".csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(
      tot_cat_cod = round(tot_cat_cod),
      tot_cat_hadd = round(tot_cat_hadd),
      year = y
    ) %>%
    dplyr::rename(mode = mode1, month = month1) %>%
    dplyr::left_join(
      zero_dtrip_combinations %>%
        dplyr::select(state, year, month, set_dtrip_zero),
      by = c("state", "year", "month")
    ) %>%
    dplyr::filter(is.na(set_dtrip_zero), tripid <= 100) %>%
    dplyr::select(-set_dtrip_zero)
  
  # directed trips 
  directed_trips <- readr::read_csv(
    paste0(input_data_cd, "directed_trips_and_regulations.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(dtrip = round(dtrip)) %>%
    dplyr::left_join(
      zero_dtrip_combinations %>%
        dplyr::select(st, year, month, set_dtrip_zero),
      by = c("st", "year", "month")
    ) %>%
    dplyr::mutate(dtrip = dplyr::if_else(!is.na(set_dtrip_zero), 0, dtrip)) %>%
    dplyr::select(-set_dtrip_zero) %>%
    dplyr::filter(dtrip != 0, year == y)
  
    directed_trips_p <- directed_trips %>% 
    mutate(period2 = as.character(period2)) %>% mutate(n_trips = floor(dtrip), n_draws = n_draws) 
  
  regs <- directed_trips_p %>% dplyr::select(period2, cod_bag, cod_min, hadd_bag, hadd_min, dtrip)
  
  # number of simulated annual periods
  n_periods<-length(unique(regs$period2))

  regs_check <- directed_trips_p %>% dplyr::select(period2, dtrip)
  
  MRIP_data <- catch_data_all_split %>%
    dplyr::group_by(period2) %>%
    dplyr::summarise(mean_cod_keep = mean(cod_landing),
                     mean_hadd_keep = mean(hadd_landing),
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
  source(paste0(input_code_cd,"find_p_star_values_corr.R")) # this calls "calibration_loop_corr.R"
  
  p_starz<-cbind(p_star_cod_variable, p_star_hadd_variable, cod_harvest_perc_diff, cod_harvest_diff, hadd_harvest_perc_diff, hadd_harvest_diff)
  
  p_starz<-p_starz %>% as.data.frame() %>% dplyr::mutate(draw=i, year=y)
  
  p_star_values_i[[i]] <-p_starz
  

}
  p_star_values_all= rlist::list.stack(p_star_values_i, fill=TRUE)
  p_star_values_i_y[[y]]= p_star_values_all
  
}

p_star_values_all_y= rlist::list.stack(p_star_values_i_y, fill=TRUE)
saveRDS(p_star_values_all_y, file = paste0(input_data_cd, "p_star_values_2019_2024.rds"))


###### PART B ##########

#### B1 and B2) projections

# Helpers 
# Only want Kendall's tau when both variables contain variation.

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
aggregate_stratum_tau <- function(
    data,
    reporting_vars,
    domain_label = NULL
) {
  
  reporting_vars <- rlang::syms(reporting_vars)
  
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
      
      # trip-weighted average across strata
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
# directed trips 
directed_trips_all <- readr::read_csv(
  paste0(input_data_cd, "directed_trips_and_regulations.csv"),
  show_col_types = FALSE
) %>%
  dplyr::mutate(dtrip = round(dtrip)) %>%
  dplyr::left_join(
    zero_dtrip_combinations %>%
      dplyr::select(st, year, month, set_dtrip_zero),
    by = c("st", "year", "month")
  ) %>%
  dplyr::mutate(dtrip = dplyr::if_else(!is.na(set_dtrip_zero), 0, dtrip)) %>%
  dplyr::select(-set_dtrip_zero) %>%
  dplyr::filter(dtrip != 0)


p_star_calibrated <- readRDS(
  file.path(input_data_cd, "p_star_values_2019_2024.rds")
)

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
  
  p_star_y <- p_star_calibrated |>
    dplyr::filter(year == y)
  
  if (dependence == "correlated") {
    projection_file <- "projection_corr.R"
    suffix <- ""
  } else {
    projection_file <- "projection_ind.R"
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
        file.path(proj_catch_cd, paste0("projection_catch_draw", i, "_", y, ".csv"))
        ,show_col_types = FALSE)
    
      set.seed(y*100000L+i*10L)
  
    source(
      file.path(input_code_cd, projection_file),
      local = environment()
    )
  
  writexl::write_xlsx(
    ktaus_all,
    file.path(
      input_data_cd,
      paste0("ktau_output_y", y, "_draw", i, suffix, ".xlsx")
    )
  )

  writexl::write_xlsx(
    ktaus_strata_all,
    file.path(
      input_data_cd,
      paste0("ktau_output_strata_y", y, "_draw", i, suffix, ".xlsx")
    )
  )

  writexl::write_xlsx(
    sims_all,
    file.path(
      input_data_cd,
      paste0("model_output_y", y, "_draw", i, suffix, ".xlsx")
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
    draws = 1:50
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
      "p_star_calibrated",
      "input_data_cd",
      "input_code_cd",
      "proj_catch_cd",
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


# Execution
projection_years <- c(2024, 2023, 2022, 2021, 2020, 2019)

# Start the clock!
ptm <- proc.time()

for (y in projection_years) {
  run_projection_year_parallel(
    y = y,
    dependence = "correlated",
    draws = 1:50
  )
  
  gc()
}
proc.time() - ptm # about 2 minutes per draw


for (y in projection_years) {
  run_projection_year_parallel(
    y = y,
    dependence = "independent",
    draws = 1:50
  )
  
  gc()
}


###### PART C ##########
## close cod and haddock 

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

input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/data/input_data"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/code/09_fishery_simulation/"
proj_catch_cd <- input_data_cd
results_plot_code_cd <- input_code_cd


n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions


zero_dtrip_combinations <- tibble::tribble(
  ~st, ~state, ~year, ~months,
  23, "ME",     2019, c(5, 6),
  23, "ME",     2019, c(9, 10),
  33, "NH",     2019, c(9, 10),
  25, "MA",     2019, c(11, 12),
  23, "ME",     2020, c(5, 6),
  23, "ME",     2020, c(9, 10),
  25, "MA",     2020, c(11, 12),
  25, "MA",     2021, c(3, 4),
  23, "ME",     2021, c(9, 10),
  25, "MA",     2021, c(11, 12),
  25, "MA",     2022, c(3, 4),
  25, "MA",     2022, c(9, 10),
  25, "MA",     2023, c(3, 4),
  23, "ME",     2023, c(5, 6),
  23, "ME",     2023, c(9, 10),
  23, "ME",     2024, c(5, 6),
  25, "MA",     2024, c(9, 10),
  25, "MA",     2024, c(11, 12)
) %>%
  tidyr::unnest_longer(months, values_to = "month") %>%
  dplyr::mutate(set_dtrip_zero = TRUE)

  for(yr in c(2024, 2023, 2022, 2021, 2020, 2019)){
    
    directed_trips <- readr::read_csv(
      paste0(input_data_cd, "directed_trips_and_regulations.csv"),
      show_col_types = FALSE
    ) %>%
      dplyr::mutate(dtrip = round(dtrip)) %>%
      dplyr::left_join(
        zero_dtrip_combinations %>%
          dplyr::select(st, year, month, set_dtrip_zero),
        by = c("st", "year", "month")
      ) %>%
      dplyr::mutate(dtrip = dplyr::if_else(!is.na(set_dtrip_zero), 0, dtrip)) %>%
      dplyr::select(-set_dtrip_zero) %>%
      dplyr::filter(dtrip != 0, year==yr) %>% 
    dplyr::mutate(cod_bag=0,hadd_bag=0) # set cod and haddock bag limit to zero
  
  regs <- directed_trips %>% 
    mutate(period2 = as.character(period2)) %>% 
    mutate(n_trips = floor(dtrip), n_draws = n_draws) 
  
  regs_check <- directed_trips %>% dplyr::select(period2, dtrip)
  
  k_taus<-list()
  output<-list()
  keep_rel_pairs_i<-list()
  
  for (i in 1:50){
    
    p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2024.rds")) %>%
      dplyr::filter(year==yr, draw==i)
    p_star_cod_variable<- p_starz$p_star_cod_variable
    p_star_hadd_variable<- p_starz$p_star_hadd_variable
    
    
    catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "calib_catch_yr", yr, "_draw", i, ".csv"), show_col_types = FALSE)) %>% 
      dplyr::rename(mode = mode1, month = month1)
      
    
    source(paste0(input_code_cd,"projection_close_fishery.R")) 
    
    output[[i]]<-sims_all
  }
  
  output_all<-rlist::list.stack(output, fill=TRUE)
  write_xlsx(output_all, paste0(input_data_cd,"model_output_y", yr, "_close_both.xlsx"))  #updated output
  
}




###### PART D ##########
# Format model output

years<-c(2024, 2023, 2022, 2021, 2020, 2019)

source(paste0(input_code_cd,"format_model_output.R")) 
source(paste0(input_code_cd,"manuscript_plots.R")) 
source(paste0(input_code_cd,"manuscript_values.R")) 



