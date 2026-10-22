
# OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 
# adjust globals - user edit here 
input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/data/" 
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/code/09_fishery_simulation/"
proj_catch_cd <- input_data_cd
results_plot_code_cd <- input_code_cd


#### Re-format the output data
# append output files for each individual draw by year.
for(yr in years){
  
  # output from catch w/residual correlation
  ### Catch and welfare ### 
  
  files_expected <- file.path(
    input_data_cd,  paste0("model_output_y", yr, "_draw", 1:50, ".xlsx"))
  
  exists <- file.exists(files_expected)
  if (any(!exists)) {
    stop(
      "Missing expected files:\n",
      paste(basename(files_expected[!exists]), collapse = "\n")
    )
  }
  
  files <- files_expected[exists]
  
  # Read and append
  dt_all <- data.table::rbindlist(
    lapply(files, function(f) {
      dt <- data.table::as.data.table(readxl::read_excel(f))
      dt
    }),
    use.names = TRUE,
    fill = TRUE
  )
  
  write_xlsx(dt_all, paste0(input_data_cd,"model_output_y", yr, "_combined.xlsx"))  #save the data
  
  # output from catch w/out residual correlation
  files_expected <- file.path(
    input_data_cd,  paste0("model_output_y", yr,"_draw", 1:50, "_ind.xlsx"))
  
  
  exists <- file.exists(files_expected)
  if (any(!exists)) {
    stop(
      "Missing expected files:\n",
      paste(basename(files_expected[!exists]), collapse = "\n")
    )
  }
  
  files <- files_expected[exists]
  
  # Read and append
  dt_all <- data.table::rbindlist(
    lapply(files, function(f) {
      dt <- data.table::as.data.table(readxl::read_excel(f))
      dt
    }),
    use.names = TRUE,
    fill = TRUE
  )
  write_xlsx(dt_all, paste0(input_data_cd,"model_output_y", yr,"_combined_ind.xlsx"))  #save the data
  
  
  ### Kendall's tau calculation ### 
  # output from catch w/residual correlation
  files_expected <- file.path(
    input_data_cd,  paste0("ktau_output_y", yr,"_draw", 1:50, ".xlsx"))
  
  
  exists <- file.exists(files_expected)
  if (any(!exists)) {
    stop(
      "Missing expected files:\n",
      paste(basename(files_expected[!exists]), collapse = "\n")
    )
  }
  
  files <- files_expected[exists]
  
  # Read and append
  dt_all <- data.table::rbindlist(
    lapply(files, function(f) {
      dt <- data.table::as.data.table(readxl::read_excel(f))
      dt
    }),
    use.names = TRUE,
    fill = TRUE
  )
  write_xlsx(dt_all, paste0(input_data_cd,"ktau_output_y", yr, "_combined.xlsx"))  #save the data
  
  # output from catch w/out residual correlation
  files_expected <- file.path(
    input_data_cd,  paste0("ktau_output_y", yr,"_draw", 1:50, "_ind.xlsx"))
  

  exists <- file.exists(files_expected)
  if (any(!exists)) {
    stop(
      "Missing expected files:\n",
      paste(basename(files_expected[!exists]), collapse = "\n")
    )
  }
  
  files <- files_expected[exists]
  
  # Read and append
  dt_all <- data.table::rbindlist(
    lapply(files, function(f) {
      dt <- data.table::as.data.table(readxl::read_excel(f))
      dt
    }),
    use.names = TRUE,
    fill = TRUE
  )
  write_xlsx(dt_all, paste0(input_data_cd,"ktau_output_y", yr,"_combined_ind.xlsx"))  #save the data
  
}



# Read the combined output, generate variables for plotting
mortality <- data.table::as.data.table(readr::read_csv(
  file.path(input_data_cd, "Discard_Mortality.csv"), show_col_types = FALSE))
stopifnot(all(c("month", "disc_mort_cod", "disc_mort_hadd_sm") %in% names(mortality)))
if (anyDuplicated(mortality$month)) stop("Duplicate mortality months")

total_cols <- c("codcat", "codkeep", "codrel", "haddcat", "haddkeep", "haddrel",
                "ntrips", "n_choice_occasions", "cod_dead_disc", "hadd_dead_disc", "cv")
safe_mean <- function(z) if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
safe_divide <- function(n, d) ifelse(!is.na(d) & d > 0, n / d, NA_real_)

read_output <- function(year, independent = FALSE) {
  ending <- if (independent) "_combined_ind.xlsx" else "_combined.xlsx"
  path <- file.path(input_data_cd, paste0("model_output_y", year, ending))
  x <- data.table::as.data.table(readxl::read_excel(path))
  required <- c("draw", "domain", "month", "mode", "state", "ntrips_base", "ntrips_alt",
                "n_choice_occasions", "change_CS",
                as.vector(outer(c("tot_cat_cod_", "tot_keep_cod_", "tot_rel_cod_",
                                  "tot_cat_hadd_", "tot_keep_hadd_", "tot_rel_hadd_"),
                                c("base", "new"), paste0)))
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols)) stop("Missing columns in ", path, ": ", paste(missing_cols, collapse = ", "))
  x[, state := as.character(state)]
  x[, decade := as.integer(sub("^.*?([0-9]+)$", "\\1", domain))]
  x[, copula := sub("[0-9]+$", "", domain)]
  if (anyNA(x$decade) || anyNA(x$copula) || any(x$copula == "")) stop("Invalid domain in ", path)
  ix <- match(x$month, mortality$month)
  if (anyNA(ix)) stop("Missing discard mortality months in ", path)
  x[, disc_mort_cod := mortality$disc_mort_cod[ix]]
  x[, disc_mort_hadd_sm := mortality$disc_mort_hadd_sm[ix]]
  if (anyNA(x$disc_mort_cod) || anyNA(x$disc_mort_hadd_sm)) stop("Missing discard mortality rates")
  x
}

read_closure <- function(year) {
  
  path <- file.path(input_data_cd, paste0("model_output_y", year, "_close_both.xlsx"))
  x <- data.table::as.data.table(readxl::read_excel(path))
  
  required <- c("draw", "period2", "change_CS")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Missing closure columns: ", paste(missing, collapse = ", "))
  
  x[, c("mode", "month", "area", "state") := data.table::tstrsplit(period2, "_", fixed = TRUE)]
  x[, `:=`(draw = as.integer(draw), month = as.integer(month), state = as.character(state))]
  
  if (anyNA(x[, .(draw, mode, month, area, state, change_CS)]))
    stop("Missing closure data in ", path)
  
  if (any(!is.finite(x$change_CS))) stop("Non-finite closure welfare in ", path)
  
  if (anyDuplicated(x, by = c("draw", "period2")))
    stop("Duplicate closure domain observations in ", path)
  
  x[]
}


  summarize_output <- function(x, dimensions, year, independent = FALSE) {
    
    x <- data.table::copy(x)
    
    # Simulation variables
    sum_vars <- c(
      "tot_cat_cod_base", "tot_cat_cod_new",
      "tot_keep_cod_base", "tot_keep_cod_new",
      "tot_rel_cod_base", "tot_rel_cod_new",
      "tot_cat_hadd_base", "tot_cat_hadd_new",
      "tot_keep_hadd_base", "tot_keep_hadd_new",
      "tot_rel_hadd_base", "tot_rel_hadd_new",
      "ntrips_base", "ntrips_alt",
      "n_choice_occasions", "change_CS"
    )
    
    # Additional trip outcome diagnostics
    bag_vars <- c(
      "n_legal_cod", "n_bag_excess_cod", "cod_catch_trip",
      "cod_bag_bind", "cod_closed_trip",
      "n_legal_hadd", "n_bag_excess_hadd", "hadd_catch_trip",
      "hadd_bag_bind", "hadd_closed_trip"
    )
    

    # Check for missing standard variables.
    missing_cols <- setdiff(sum_vars, names(x))
    if (length(missing_cols)) stop("Missing columns: ", paste(missing_cols, collapse = ", "))
    
    if (anyNA(x[, ..sum_vars])) stop("Missing simulation values in year ", year)
  
  # Aggregate base and new outcomes together.
  keys <- c("draw", "copula", "decade", dimensions)
  
  out <- x[
    ,
    lapply(.SD, sum),
    by = keys,
    .SDcols = sum_vars
  ]
  
  # Catch per trip 
  out[, codcattrip_base := tot_cat_cod_base / ntrips_base]
  out[, codcattrip_new  := tot_cat_cod_new / ntrips_alt]
  
  out[, haddcattrip_base := tot_cat_hadd_base / ntrips_base]
  out[, haddcattrip_new  := tot_cat_hadd_new / ntrips_alt]
  
  # Harvest per trip
  out[, codkeeptrip_base := tot_keep_cod_base / ntrips_base]
  out[, codkeeptrip_new  := tot_keep_cod_new / ntrips_alt]
  
  out[, haddkeeptrip_base := tot_keep_hadd_base / ntrips_base]
  out[, haddkeeptrip_new  := tot_keep_hadd_new / ntrips_alt]
  
  
  # Changes relative to baseline
  out[, delta_codcattrip := codcattrip_new - codcattrip_base]
  out[, delta_haddcattrip := haddcattrip_new - haddcattrip_base]
  
  out[, delta_codkeep := tot_keep_cod_new - tot_keep_cod_base]
  out[, delta_haddkeep := tot_keep_hadd_new - tot_keep_hadd_base]
  
  out[, delta_codkeeptrip := codkeeptrip_new - codkeeptrip_base]
  out[, delta_haddkeeptrip := haddkeeptrip_new - haddkeeptrip_base]
  

  # Variables used in plots and independence comparisons
  out[, `:=`(
    codcat   = tot_cat_cod_new,
    haddcat  = tot_cat_hadd_new,
    codkeep  = tot_keep_cod_new,
    haddkeep = tot_keep_hadd_new,
    ntrips   = ntrips_alt,
    cv       = change_CS
  )]
  
  # Retention diagnostics
  if (has_bag_vars) {
    out[, `:=`(
    pct_legal_cod = 100 * safe_divide(n_legal_cod, tot_cat_cod_new),
    pct_legal_hadd = 100 * safe_divide(n_legal_hadd, tot_cat_hadd_new),
    pct_bag_bind_cod = 100 * safe_divide(cod_bag_bind, cod_catch_trip),
    pct_bag_bind_hadd = 100 * safe_divide(hadd_bag_bind, hadd_catch_trip),
    pct_bag_excess_cod = 100 * safe_divide(n_bag_excess_cod, n_legal_cod),
    pct_bag_excess_hadd = 100 * safe_divide(n_bag_excess_hadd, n_legal_hadd),
    pct_closed_cod = 100 * safe_divide(cod_closed_trip, cod_catch_trip),
    pct_closed_hadd = 100 * safe_divide(hadd_closed_trip, hadd_catch_trip),
    pct_retained_cod = 100 * safe_divide(tot_keep_cod_new, tot_cat_cod_new),
    pct_retained_hadd = 100 * safe_divide(tot_keep_hadd_new, tot_cat_hadd_new)
  )]
  }
  # Changes in total catch, harvest, and trips relative to baseline
  out[, `:=`(
    pct_change_cod_catch = 100 * safe_divide(tot_cat_cod_new - tot_cat_cod_base, tot_cat_cod_base),
    pct_change_hadd_catch = 100 * safe_divide(tot_cat_hadd_new - tot_cat_hadd_base, tot_cat_hadd_base),
    pct_change_cod_harvest = 100 * safe_divide(tot_keep_cod_new - tot_keep_cod_base, tot_keep_cod_base),
    pct_change_hadd_harvest = 100 * safe_divide(tot_keep_hadd_new - tot_keep_hadd_base, tot_keep_hadd_base),
    pct_change_trips = 100 * safe_divide(ntrips_alt - ntrips_base, ntrips_base)
  )]

  out[, correlation := if (independent) "independent" else "correlated"]
  out[, year := year]
  
  out[]
}

# summarize fishery closure output
summarize_closure <- function(x, dimensions, year) {
  
  keys <- c("draw", dimensions)
  
  out <- x[, .(cv_closure = sum(change_CS)), by = keys]
  
  out[, `:=`(
    total_fishery_value = cv_closure,
    base_year = year
  )]
  
  if (any(!is.finite(out$total_fishery_value) | out$total_fishery_value <= 0))
    stop("Invalid total fishery value in year ", year)
  
  out[]
}


for (yr in years) {
  
  correlated <- read_output(yr)
  independent <- read_output(yr, independent = TRUE)
  closure <- read_closure(yr)
  
  for (level in c("coast", "mode", "state")) {
    
    dimensions <- switch(level, coast = character(), mode = "mode", state = "state")
    
    result <- data.table::rbindlist(list(
      summarize_output(correlated, dimensions, yr, FALSE),
      summarize_output(independent, dimensions, yr, TRUE)
    ), fill = TRUE)
    
    closure_summary <- summarize_closure(closure, dimensions, yr)
    
    result <- merge(result, closure_summary, by = c("draw", dimensions), all.x = TRUE, sort = FALSE)
    
    if (anyNA(result$cv_closure)) stop("Unmatched closure draws in year ", yr, ", level ", level)
    
    result[, `:=`(
      cv_pct_total_value = 100 * cv / total_fishery_value,
      cv_closure = cv_closure,
      base_year = yr
    )]
    
    # Pull outcomes from the corresponding independent simulation
    ind_vars <- c(
      "draw", "decade", dimensions,
      "codcat", "haddcat",
      "codkeep", "haddkeep",
      "ntrips", "cv"
    )
    
    ind <- result[
      correlation == "independent",
      ..ind_vars
    ]
    
    data.table::setnames(
      ind,
      old = c("codcat", "haddcat", "codkeep", "haddkeep", "ntrips", "cv"),
      new = c("codcat_ind", "haddcat_ind", "codkeep_ind", "haddkeep_ind",
              "ntrips_ind", "cv_ind")
    )
    
    merge_keys <- c("draw", "decade", dimensions)
    
    if (anyDuplicated(ind, by = merge_keys)) {
      stop("Independent values are not unique for the merge keys")
    }
    
    result <- merge(
      result,
      ind,
      by = merge_keys,
      all.x = TRUE,
      sort = FALSE
    )
    
    # Differences are independent minus correlated
    result[, `:=`(
      
      # Absolute differences used in Figure 3.
      diff_codcat_ind  = data.table::fifelse(
        correlation == "correlated", codcat_ind - codcat, NA_real_),
      
      diff_haddcat_ind = data.table::fifelse(
        correlation == "correlated", haddcat_ind - haddcat, NA_real_),
      
      diff_codkeep_ind = data.table::fifelse(
        correlation == "correlated", codkeep_ind - codkeep, NA_real_),
      
      diff_haddkeep_ind = data.table::fifelse(
        correlation == "correlated", haddkeep_ind - haddkeep, NA_real_),
      
      diff_cv_ind = data.table::fifelse(
        correlation == "correlated", cv_ind - cv, NA_real_),
      
      
      # Percentage differences used in manuscript_values.R
      pct_diff_ntrips_corr = data.table::fifelse(
        correlation == "correlated",
        100 * (ntrips_ind - ntrips) / ntrips,
        NA_real_
      ),
      
      pct_diff_haddkeep = data.table::fifelse(
        correlation == "correlated",
        100 * (haddkeep_ind - haddkeep) / haddkeep,
        NA_real_
      ),
      
      pct_diff_codkeep = data.table::fifelse(
        correlation == "correlated",
        100 * (codkeep_ind - codkeep) / codkeep,
        NA_real_
      ),
      
      # Welfare difference as percentage of total fishery value
      diff_cv_total_cv = data.table::fifelse(
        correlation == "correlated",
        100 * (cv_ind - cv) / total_fishery_value,
        NA_real_
      ),
      
      cv_diff_ind_corr = data.table::fifelse(
        correlation == "correlated",
        cv_ind - cv,
        NA_real_
      ),
      
      cv_diff_pct_total_value = data.table::fifelse(
        correlation == "correlated",
        100 * (cv_ind - cv) / total_fishery_value,
        NA_real_
      )
    )]
    
    # Paired independence differences as a percentage of baseline catch/harvest.
    result[, `:=`(
      pct_diff_codcat_base  = 100 * safe_divide(diff_codcat_ind, tot_cat_cod_base),
      pct_diff_haddcat_base = 100 * safe_divide(diff_haddcat_ind, tot_cat_hadd_base),
      pct_diff_codkeep_base = 100 * safe_divide(diff_codkeep_ind, tot_keep_cod_base),
      pct_diff_haddkeep_base = 100 * safe_divide(diff_haddkeep_ind, tot_keep_hadd_base)
    )]
    
    result[, pct_diff_ntrips_base := 100 * safe_divide(ntrips_ind - ntrips, ntrips_base)]
    result[correlation != "correlated", pct_diff_ntrips_base := NA_real_]
    
    
    saveRDS(as.data.frame(result),
            file.path(input_data_cd, paste0("model_output_y", yr, "_reformat_", level, ".rds")))
  }
  
}


# Kendall's tau summaries. These workbooks have annual rows (month == 0)
# for the full coast ("all"), each state, and each fishing mode.
tau_cols <- c("k_tau_keep_unweighted", "k_tau_catch_unweighted",
              "k_tau_keep_est", "k_tau_catch_est")
copula_levels <- c("independent", "clayton", "frank", "gaussian", "gumbel", "plackett")
tau_coast_list <- list()
tau_state_list <- list()
tau_mode_list <- list()

read_tau <- function(year, independent = FALSE) {
  ending <- if (independent) "_combined_ind.xlsx" else "_combined.xlsx"
  path <- file.path(input_data_cd, paste0("ktau_output_y", year, ending))
  x <- data.table::as.data.table(readxl::read_excel(path))
  required <- c("month", "domain1", "domain", "draw", tau_cols)
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols)) stop("Missing Kendall's tau columns in ", path, ": ",
                                 paste(missing_cols, collapse = ", "))
  x <- x[month == 0 & domain1 %in% c("all", "ME", "NH", "MA", "pr", "fh")]
  x[, decade := as.integer(sub("^.*?([0-9]+)$", "\\1", domain))]
  x[, copula := if (independent) "independent" else tolower(sub("[0-9]+$", "", domain))]
  x[, correlation := if (independent) "independent" else "correlated"]
  if (anyNA(x$decade) || any(x$copula == "") || anyNA(x$copula))
    stop("Unrecognized Kendall's tau domain in ", path)
  x[decade > 0]
}

format_tau <- function(x, groups, level, year) {
  result <- data.table::copy(x[domain1 %in% groups,
    c("draw", "correlation", "copula", "decade", "domain1", tau_cols), with = FALSE])
  if (level == "coast") result[, domain1 := NULL]
  if (level == "state") data.table::setnames(result, "domain1", "state")
  if (level == "mode") data.table::setnames(result, "domain1", "mode")
  result[, copula := factor(copula, levels = copula_levels)]
  result[, base_year := year]
  result[]
}

for (yr in years) {
  tau <- data.table::rbindlist(list(read_tau(yr), read_tau(yr, independent = TRUE)))
  tau_coast_list[[as.character(yr)]] <- format_tau(tau, "all", "coast", yr)
  tau_state_list[[as.character(yr)]] <- format_tau(tau, c("ME", "NH", "MA"), "state", yr)
  tau_mode_list[[as.character(yr)]] <- format_tau(tau, c("pr", "fh"), "mode", yr)
  
  saveRDS(as.data.frame(tau_coast_list[[as.character(yr)]]),
          file.path(input_data_cd, paste0("ktau_output_y", yr, "_reformat_coast.rds")))
  
  saveRDS(as.data.frame(tau_state_list[[as.character(yr)]]),
          file.path(input_data_cd, paste0("ktau_output_y", yr, "_reformat_state.rds")))
  
  saveRDS(as.data.frame(tau_mode_list[[as.character(yr)]]),
          file.path(input_data_cd, paste0("ktau_output_y", yr, "_reformat_mode.rds")))
  
}




