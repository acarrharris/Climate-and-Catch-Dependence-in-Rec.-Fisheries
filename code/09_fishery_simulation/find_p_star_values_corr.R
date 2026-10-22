
# OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 
# adjust globals - user edit here 
input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/data/" 
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/code/09_fishery_simulation/"
proj_catch_cd <- input_data_cd
results_plot_code_cd <- input_code_cd


# Find the p-star value that explains observed harvest. 
# p-star [0-1] is the percentage of caught fish that are harvested given
# a catch-per-trip distribution and bag limit. The p-star value is derived
# iteratively, where its initial value is adjusted until simulated harvest 
# approximates MRIP harvest. 

initial_p_star <- function(harvest, total_catch, species, year, draw) {
  if (!is.finite(harvest) || !is.finite(total_catch) || harvest < 0 || total_catch < 0) {
    stop("Invalid ", species, " calibration target for year ", year, ", draw ", draw, ".")
  }
  if (total_catch == 0) {
    if (harvest == 0) return(1)
    stop("Positive ", species, " harvest with zero total catch for year ", year, ", draw ", draw, ".")
  }
  pmin(pmax(1 - harvest / total_catch, 0), 1)
}

safe_percent_difference <- function(model_value, target_value) {
  if (!is.finite(model_value) || !is.finite(target_value)) return(NA_real_)
  if (target_value == 0) return(NA_real_)
  100 * (model_value - target_value) / target_value
}

target_achieved <- function(model_value, target_value, percent_difference) {
  absolute_difference <- abs(model_value - target_value)
  as.integer(absolute_difference < 500 || (is.finite(percent_difference) && abs(percent_difference) < 5))
}

update_p_star <- function(p_star, signed_difference, percent_difference, achieved) {
  if (achieved == 1L || !is.finite(signed_difference) || signed_difference == 0) return(p_star)
  difference_size <- if (is.finite(percent_difference)) abs(percent_difference) else Inf

  if (signed_difference < 0) {
    step <- if (difference_size > 20) 0.075 else if (difference_size > 10) 0.035 else 0.015
    p_star <- p_star - step
  } else {
    step <- if (difference_size > 20) 0.070 else if (difference_size > 10) 0.030 else 0.010
    p_star <- p_star + step
  }

  pmin(pmax(p_star, 0), 1)
}

cod_target <- sum(MRIP_data$cod_harvest)
hadd_target <- sum(MRIP_data$hadd_harvest)
cod_total_catch_target <- sum(MRIP_data$cod_tot_cat)
hadd_total_catch_target <- sum(MRIP_data$hadd_tot_cat)

# Data-derived central starting values
p_star_cod_center <- initial_p_star(
  cod_target,
  cod_total_catch_target,
  "cod",
  y,
  i
)

p_star_hadd_center <- initial_p_star(
  hadd_target,
  hadd_total_catch_target,
  "haddock",
  y,
  i
)

# Alternate starting direction across draws:
# odd draws have lower p-star, producing higher initial harvest
# even draws have higher p-star, producing lower initial harvest
start_direction <- if (i %% 2L == 1L) -1 else 1
start_offset <- 0.10

p_star_cod_variable <- pmin(
  pmax(p_star_cod_center + start_direction * start_offset, 0),
  1
)

p_star_hadd_variable <- pmin(
  pmax(p_star_hadd_center + start_direction * start_offset, 0),
  1
)


# The model cannot reproduce reported illegal/out-of-season cod harvest in 2019/2021.
# Setting p* to zero allows the maximum legally possible cod harvest, while cod
# is excluded from the 2019/2021 convergence requirement.
if (y %in%  c(2019, 2021)) p_star_cod_variable <- 0

iteration <- 0L
max_iterations <- 200L
converged <- FALSE

repeat {
  iteration <- iteration + 1L

  cat("\nYear:", y, " Draw:", i, " Iteration:", iteration,
      "\nCod p*:", p_star_cod_variable,
      "\nHaddock p*:", p_star_hadd_variable, "\n")

  source(paste0(input_code_cd, "calibration_loop_corr.R"))

  calibration_check <- readRDS(paste0(input_data_cd, "calibration_data_", y, "draw", i, ".rds")) %>%
    dplyr::select(period2, n_choice_occasions)

  domains <- dplyr::n_distinct(calibration_check$period2)
  if (domains != n_periods) {
    missing_periods <- setdiff(as.character(regs$period2), as.character(calibration_check$period2))
    stop("Calibration lost periods for year ", y, ", draw ", i, ": ", paste(missing_periods, collapse = ", "))
  }

  model_cod_harvest <- sum(aggregate_trip_data$tot_keep_cod)
  model_hadd_harvest <- sum(aggregate_trip_data$tot_keep_hadd)
  cod_signed_difference <- model_cod_harvest - cod_target
  hadd_signed_difference <- model_hadd_harvest - hadd_target
  cod_harvest_diff <- abs(cod_signed_difference)
  hadd_harvest_diff <- abs(hadd_signed_difference)
  cod_harvest_perc_diff <- safe_percent_difference(model_cod_harvest, cod_target)
  hadd_harvest_perc_diff <- safe_percent_difference(model_hadd_harvest, hadd_target)

  cod_achieved <- if (y %in%  c(2019, 2021)) 1L else target_achieved(model_cod_harvest, cod_target, cod_harvest_perc_diff)
  hadd_achieved <- target_achieved(model_hadd_harvest, hadd_target, hadd_harvest_perc_diff)

  cat("Model cod harvest:", round(model_cod_harvest),
      "\nMRIP cod harvest:", round(cod_target),
      "\nCod difference:", round(cod_signed_difference),
      "\nCod % difference:", round(cod_harvest_perc_diff, 2),
      "\nModel haddock harvest:", round(model_hadd_harvest),
      "\nMRIP haddock harvest:", round(hadd_target),
      "\nHaddock difference:", round(hadd_signed_difference),
      "\nHaddock % difference:", round(hadd_harvest_perc_diff, 2),
      "\nDomains:", domains, "/", n_periods,
      "\nCod achieved:", cod_achieved,
      "\nHaddock achieved:", hadd_achieved, "\n")

  if (cod_achieved == 1L && hadd_achieved == 1L) {
    converged <- TRUE
    break
  }

  if (iteration >= max_iterations) {
    warning("Calibration did not converge after ", max_iterations, " iterations: year = ", y, ", draw = ", i,
            ", cod p* = ", round(p_star_cod_variable, 4), ", haddock p* = ", round(p_star_hadd_variable, 4),
            ", cod % difference = ", round(cod_harvest_perc_diff, 2),
            ", haddock % difference = ", round(hadd_harvest_perc_diff, 2))
    break
  }

  p_star_cod_variable <- update_p_star(p_star_cod_variable, cod_signed_difference, cod_harvest_perc_diff, cod_achieved)
  p_star_hadd_variable <- update_p_star(p_star_hadd_variable, hadd_signed_difference, hadd_harvest_perc_diff, hadd_achieved)
}

# Checkpoint after every completed year-draw calibration
p_star_checkpoint <- tibble::tibble(
  year = y,
  draw = i,
  dependence = "correlated",
  converged = converged,
  iterations = iteration,
  start_direction = ifelse(start_direction == -1, "below_p_star", "above_p_star"),
  start_offset = start_offset,
  p_star_cod_center = p_star_cod_center,
  p_star_hadd_center = p_star_hadd_center,
  p_star_cod_variable = p_star_cod_variable,
  p_star_hadd_variable = p_star_hadd_variable,
  cod_harvest_perc_diff = cod_harvest_perc_diff,
  cod_harvest_diff = cod_harvest_diff,
  hadd_harvest_perc_diff = hadd_harvest_perc_diff,
  hadd_harvest_diff = hadd_harvest_diff,
  domains = domains,
  expected_domains = n_periods
)

saveRDS(p_star_checkpoint, paste0(input_data_cd, "p_star_checkpoint_corr_", y, "_draw", i, ".rds"))

