
# OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 
# adjust globals - user edit here 
input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/data/" 
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/code/09_fishery_simulation/"
proj_catch_cd <- input_data_cd
results_plot_code_cd <- input_code_cd


regs <- data.table::copy(data.table::as.data.table(regs))
stopifnot(all(c("cod_bag", "hadd_bag") %in% names(regs)))
regs[, `:=`(cod_bag = 0L, hadd_bag = 0L)]

p_star_cod <- p_star_cod_variable
p_star_hadd<-p_star_hadd_variable

######################################
##   Begin simulating trip outcomes ##
######################################

cod_hadd_catch_data <- catch_data_all %>%
  dplyr::right_join(regs_check, by="period2") %>% 
  dplyr::select(-dtrip) 

param_draws <- readRDS(
  file.path(input_data_cd, paste0("costs_data_", yr, "draw", i, ".rds"))
) |>
  dplyr::select(-month)

calibration_data_table <- readRDS(
  file.path(input_data_cd, paste0("calibration_data_", yr, "draw", i, ".rds"))
) |>
  dplyr::select(period2, n_choice_occasions)

param_draws <- param_draws |>
  dplyr::mutate(
    v0_fishing =
      beta_sqrt_cod_keep * sqrt(tot_keep_cod_base) +
      beta_sqrt_cod_release * sqrt(tot_rel_cod_base) +
      beta_sqrt_hadd_keep * sqrt(tot_keep_hadd_base) +
      beta_sqrt_hadd_release * sqrt(tot_rel_hadd_base) +
      beta_sqrt_cod_hadd_keep *
      (sqrt(tot_keep_cod_base) * sqrt(tot_keep_hadd_base)) +
      beta_cost * cost_base,
    
    v_opt_out =
      beta_opt_out +
      beta_opt_out_age * age +
      beta_opt_out_likely * likely_to_fish +
      beta_opt_out_prefer * fish_pref_more
  )

specs <- "calibration"
sims_results <- vector("list", 1L)
diagnostic_results <- vector("list", 1L)

cod_hadd_catch_data <- cod_hadd_catch_data %>%
  dplyr::arrange(period2, month, tripid, catch_draw)

set.seed(100000 + yr * 100 + i)

cod_hadd_catch_data <- cod_hadd_catch_data %>%
  dplyr::mutate(
    u_keep_cod = stats::runif(dplyr::n()),
    u_keep_hadd = stats::runif(dplyr::n())
  )


for (spec_index in seq_along(specs)) {

  k <- specs[[spec_index]]

  #k<-"clayton1" \\test value
  catch_data1 <- cod_hadd_catch_data %>%
    dplyr::select(mode, month, period2, state, tripid, area, catch_draw,
                  u_keep_cod, u_keep_hadd, tot_cat_cod, tot_cat_hadd) %>%
    dplyr::rename(tot_cod_catch = tot_cat_cod, tot_hadd_catch = tot_cat_hadd)

  ### Cod ###
  
  # subset trips with zero catch, as no size draws are required
  cod_zero_catch <- catch_data1 %>% 
    dplyr::filter(tot_cod_catch == 0) %>% 
    dplyr::select(-c("mode", "area", "state"))
  
  # remove trips with zero cod catch
  catch_data1a <- filter(catch_data1, tot_cod_catch > 0) 
  catch_data1a<-data.table::as.data.table(catch_data1a)
  
  setkey(catch_data1a, period2)
  setkey(regs, period2)
  
  catch_data1a <- regs[catch_data1a, nomatch = 0]
  
  # Set bag limits to zero
  catch_data1a[, cod_bag_use := data.table::fifelse(is.na(cod_bag) | cod_bag <= 0, 0L, as.integer(cod_bag))]
  
  # draw number of "eligible keeps"
  catch_data1a[, keep_raw_cod := stats::qbinom(
    u_keep_cod,
    size = tot_cod_catch,
    prob = 1 - p_star_cod
  )]
  
  catch_data1a[, tot_keep_cod := pmin(keep_raw_cod, cod_bag_use)]
  catch_data1a[, tot_rel_cod  := tot_cod_catch - tot_keep_cod]
  
  catch_data1a[, c("keep_raw_cod","cod_bag_use") := NULL]
  
  catch_data1a <- catch_data1a[, .(tripid, tot_keep_cod , tot_rel_cod, period2, catch_draw, month)]
  
  summed_catch_data <- catch_data1a %>%
    .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month" ), .SDcols = c("tot_keep_cod", "tot_rel_cod")]
  
  trip_data <- summed_catch_data %>%
    dplyr::bind_rows(cod_zero_catch) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))
  
 
  
  ### Haddock ###
  
  # subset trips with zero catch, as no size draws are required
  hadd_zero_catch <- catch_data1 %>% 
    dplyr::filter(tot_hadd_catch == 0) %>% 
    dplyr::select(-c("mode", "area", "state"))
  
  # remove trips with zero cod catch
  catch_data1a <- filter(catch_data1, tot_hadd_catch > 0) 
  catch_data1a<-data.table::as.data.table(catch_data1a)
  
  setkey(catch_data1a, period2)
  
  catch_data1a <- regs[catch_data1a, nomatch = 0]
  
  # Set bag limits to zero
  catch_data1a[, hadd_bag_use := data.table::fifelse(is.na(hadd_bag) | hadd_bag <= 0, 0L, as.integer(hadd_bag))]
  
  # draw number of "eligible keeps"
  catch_data1a[, keep_raw_hadd := stats::qbinom(
    u_keep_hadd,
    size = tot_hadd_catch,
    prob = 1 - p_star_hadd
  )]
  
  catch_data1a[, tot_keep_hadd := pmin(keep_raw_hadd, hadd_bag_use)]
  catch_data1a[, tot_rel_hadd  := tot_hadd_catch - tot_keep_hadd]
  
  catch_data1a[, c("keep_raw_hadd","hadd_bag_use") := NULL]
  
  catch_data1a <- catch_data1a[, .(tripid, tot_keep_hadd , tot_rel_hadd, period2, catch_draw, month)]
  
  summed_catch_data <- catch_data1a %>%
    .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month" ), .SDcols = c("tot_keep_hadd", "tot_rel_hadd")]
  
  trip_data_hadd <- summed_catch_data %>%
    dplyr::bind_rows(hadd_zero_catch) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))
  
  
  trip_data<-trip_data %>% 
    dplyr::left_join(trip_data_hadd, by=c("period2", "catch_draw", "tripid",  "month"))
  
  trip_data<-trip_data %>% 
    dplyr::left_join(param_draws,by=c("period2", "catch_draw", "tripid"))
  
  missing_param <- trip_data %>%
    dplyr::filter(is.na(beta_cost) | is.na(cost_base) | is.na(tot_keep_cod_base) | is.na(tot_rel_cod_base) |
                    is.na(tot_keep_hadd_base) | is.na(tot_rel_hadd_base)) %>%
    dplyr::count(period2, month, name = "n_missing")
  
  if (nrow(missing_param) > 0) {
    print("MISSING PARAM_DRAWS MATCHES:")
    print(missing_param)
  }

  trip_data<- trip_data %>% dplyr::arrange(period2, tripid, catch_draw)

  
  #  utility - prediction year
  trip_data <-trip_data %>%
    dplyr::mutate(vA=
                    beta_sqrt_cod_keep*sqrt(tot_keep_cod) +
                    beta_sqrt_cod_release*sqrt(tot_rel_cod) +  
                    beta_sqrt_hadd_keep*sqrt(tot_keep_hadd) +
                    beta_sqrt_hadd_release*sqrt(tot_rel_hadd) + 
                    beta_sqrt_cod_hadd_keep*(sqrt(tot_keep_cod)*sqrt(tot_keep_hadd)) +
                    beta_cost*cost_base,
        
   #  utility - base year
                  v0 = v0_fishing)
  
  
  # Calculate utility components before averaging across catch draws
  trip_data <- trip_data %>%
    dplyr::mutate(
      u_cod_keep = beta_sqrt_cod_keep * sqrt(tot_keep_cod),
      u_cod_release = beta_sqrt_cod_release * sqrt(tot_rel_cod),
      u_hadd_keep = beta_sqrt_hadd_keep * sqrt(tot_keep_hadd),
      u_hadd_release = beta_sqrt_hadd_release * sqrt(tot_rel_hadd),
      u_interaction = beta_sqrt_cod_hadd_keep * sqrt(tot_keep_cod) * sqrt(tot_keep_hadd),
      u_cost = beta_cost * cost_base
    )
  
mean_trip_data <- trip_data %>%
  dplyr::rename(
    tot_keep_cod_new = tot_keep_cod,
    tot_rel_cod_new = tot_rel_cod,
    tot_keep_hadd_new = tot_keep_hadd,
    tot_rel_hadd_new = tot_rel_hadd
  ) %>%
  dplyr::mutate(
    tot_cat_cod_new = tot_keep_cod_new + tot_rel_cod_new,
    tot_cat_hadd_new = tot_keep_hadd_new + tot_rel_hadd_new,
    tot_cat_cod_base = tot_keep_cod_base + tot_rel_cod_base,
    tot_cat_hadd_base = tot_keep_hadd_base + tot_rel_hadd_base
  )

# Remove variables no longer needed
mean_trip_data <- mean_trip_data %>%
  dplyr::select(
    -beta_opt_out,
    -beta_opt_out_age,
    -beta_opt_out_likely,
    -beta_opt_out_prefer,
    -beta_sqrt_cod_hadd_keep,
    -beta_sqrt_cod_keep,
    -beta_sqrt_cod_release,
    -beta_sqrt_hadd_keep,
    -beta_sqrt_hadd_release,
    -likely_to_fish,
    -fish_pref_more,
    -age,
    -catch_draw,
    -cost_base
  )

# Average fishing outcomes and utility across catch draws

all_vars <- setdiff(names(mean_trip_data), c("period2", "tripid", "month"))

data.table::setDT(mean_trip_data)

mean_trip_data <- mean_trip_data[
  , lapply(.SD, mean),
  by = .(period2, tripid, month),
  .SDcols = all_vars
]

mean_trip_data[
  ,
  `:=`(
    exp_vA = exp(vA),
    exp_v0 = exp(v0),
    exp_v_opt_out = exp(v_opt_out)
  )
]

# Calculate choice probabilities and log sums 
mean_trip_data[
  ,
  `:=`(
    probA = exp_vA / (exp_vA + exp_v_opt_out),
    prob0 = exp_v0 / (exp_v0 + exp_v_opt_out),
    log_sum_alt = log(exp_vA + exp_v_opt_out),
    log_sum_base = log(exp_v0 + exp_v_opt_out)
  )
]

# Calculate CV
mean_trip_data[
  ,
  `:=`(
    CS_base = log_sum_base / beta_cost,
    CS_alt = log_sum_alt / beta_cost
  )
]

mean_trip_data[, change_CS := CS_alt - CS_base]

# Compute catch, harvest, participation, and utility diagnostics


diagnostic_data <- mean_trip_data %>%
  dplyr::left_join(calibration_data_table, by = "period2") %>%
  dplyr::group_by(period2) %>%
  dplyr::mutate(
    n_simulated_occasions = dplyr::n(),
    expand = n_choice_occasions / n_simulated_occasions
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    trips_alt = expand * probA,
    trips_base = expand * prob0,
    cod_harvest_alt = trips_alt * tot_keep_cod_new,
    cod_harvest_base = trips_base * tot_keep_cod_base,
    hadd_harvest_alt = trips_alt * tot_keep_hadd_new,
    hadd_harvest_base = trips_base * tot_keep_hadd_base
  )

diagnostic_results[[spec_index]] <- diagnostic_data %>%
  dplyr::summarise(
    n_choice_occasions = sum(expand),
    ntrips_alt = sum(trips_alt),
    ntrips_base = sum(trips_base),
    
    # Catch per choice occasion
    mean_cod_catch_alt = weighted.mean(tot_cat_cod_new, expand),
    mean_cod_catch_base = weighted.mean(tot_cat_cod_base, expand),
    mean_hadd_catch_alt = weighted.mean(tot_cat_hadd_new, expand),
    mean_hadd_catch_base = weighted.mean(tot_cat_hadd_base, expand),
    
    # Keep per choice occasion
    mean_cod_keep_alt = weighted.mean(tot_keep_cod_new, expand),
    mean_cod_keep_base = weighted.mean(tot_keep_cod_base, expand),
    mean_hadd_keep_alt = weighted.mean(tot_keep_hadd_new, expand),
    mean_hadd_keep_base = weighted.mean(tot_keep_hadd_base, expand),
    
    # Retention rates
    cod_retention_alt = sum(expand * tot_keep_cod_new) / sum(expand * tot_cat_cod_new),
    cod_retention_base = sum(expand * tot_keep_cod_base) / sum(expand * tot_cat_cod_base),
    hadd_retention_alt = sum(expand * tot_keep_hadd_new) / sum(expand * tot_cat_hadd_new),
    hadd_retention_base = sum(expand * tot_keep_hadd_base) / sum(expand * tot_cat_hadd_base),
    
    # Trips
    mean_prob_alt = weighted.mean(probA, expand),
    mean_prob_base = weighted.mean(prob0, expand),
    
    # Total harvest
    total_cod_harvest_alt = sum(cod_harvest_alt),
    total_cod_harvest_base = sum(cod_harvest_base),
    total_hadd_harvest_alt = sum(hadd_harvest_alt),
    total_hadd_harvest_base = sum(hadd_harvest_base),
    
    # Catch per fishing trip
    cod_catch_per_trip_alt = sum(trips_alt * tot_cat_cod_new) / sum(trips_alt),
    cod_catch_per_trip_base = sum(trips_base * tot_cat_cod_base) / sum(trips_base),
    hadd_catch_per_trip_alt = sum(trips_alt * tot_cat_hadd_new) / sum(trips_alt),
    hadd_catch_per_trip_base = sum(trips_base * tot_cat_hadd_base) / sum(trips_base),
    
    # Catch per choice occasion
    cod_catch_per_choice_alt = sum(trips_alt * tot_cat_cod_new) / sum(expand),
    cod_catch_per_choice_base = sum(trips_base * tot_cat_cod_base) / sum(expand),
    hadd_catch_per_choice_alt = sum(trips_alt * tot_cat_hadd_new) / sum(expand),
    hadd_catch_per_choice_base = sum(trips_base * tot_cat_hadd_base) / sum(expand),
    
    # Harvest per fishing trip
    cod_keep_per_trip_alt = sum(cod_harvest_alt) / sum(trips_alt),
    cod_keep_per_trip_base = sum(cod_harvest_base) / sum(trips_base),
    hadd_keep_per_trip_alt = sum(hadd_harvest_alt) / sum(trips_alt),
    hadd_keep_per_trip_base = sum(hadd_harvest_base) / sum(trips_base),
    
    # Harvest per choice occasion
    cod_keep_per_choice_alt = sum(cod_harvest_alt) / sum(expand),
    cod_keep_per_choice_base = sum(cod_harvest_base) / sum(expand),
    hadd_keep_per_choice_alt = sum(hadd_harvest_alt) / sum(expand),
    hadd_keep_per_choice_base = sum(hadd_harvest_base) / sum(expand),
    

    
    # Utility components
    avg_u_cod_keep = weighted.mean(u_cod_keep, expand),
    avg_u_cod_release = weighted.mean(u_cod_release, expand),
    avg_u_hadd_keep = weighted.mean(u_hadd_keep, expand),
    avg_u_hadd_release = weighted.mean(u_hadd_release, expand),
    avg_u_interaction = weighted.mean(u_interaction, expand),
    avg_u_cost = weighted.mean(u_cost, expand),
    
    mean_v_alt = weighted.mean(vA, expand),
    mean_v_base = weighted.mean(v0, expand),
    
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    year = yr,
    draw = i,
    domain = k,
    
    delta_trips = ntrips_alt - ntrips_base,
    delta_cod_harvest = total_cod_harvest_alt - total_cod_harvest_base,
    delta_hadd_harvest = total_hadd_harvest_alt - total_hadd_harvest_base,
    
    delta_cod_catch_per_trip = cod_catch_per_trip_alt - cod_catch_per_trip_base,
    delta_hadd_catch_per_trip = hadd_catch_per_trip_alt - hadd_catch_per_trip_base,
    delta_cod_keep_per_trip = cod_keep_per_trip_alt - cod_keep_per_trip_base,
    delta_hadd_keep_per_trip = hadd_keep_per_trip_alt - hadd_keep_per_trip_base,
    
    delta_prob = mean_prob_alt - mean_prob_base,
    delta_v = mean_v_alt - mean_v_base,
    
    utility_sum = avg_u_cod_keep + avg_u_cod_release +
      avg_u_hadd_keep + avg_u_hadd_release +
      avg_u_interaction + avg_u_cost,
    
    utility_difference = mean_v_alt - utility_sum
  )

# Confirm that utility components reproduce fishing utility.
stopifnot(
  abs(diagnostic_results[[spec_index]]$utility_difference) < 1e-8
)

# Remove unneeded variables
mean_trip_data[
  ,
  c(
    "beta_cost",
    "v0",
    "vA",
    "v_opt_out",
    "exp_vA",
    "exp_v0",
    "exp_v_opt_out",
    "log_sum_base",
    "log_sum_alt"
  ) := NULL
]
  

# Multiply projected catch variables by projected probability
list_names <- c("tot_keep_cod_new","tot_rel_cod_new", "tot_cat_cod_new",
                  "tot_keep_hadd_new", "tot_rel_hadd_new" , "tot_cat_hadd_new" )
  
  mean_trip_data<-mean_trip_data %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = list_names] %>%
    .[]
  
  
  # Multiply baseline catch variables by baseline probability
  list_names <- c("tot_keep_cod_base","tot_rel_cod_base", "tot_cat_cod_base",
                  "tot_keep_hadd_base", "tot_rel_hadd_base" , "tot_cat_hadd_base"  )
  
  data.table::setDT(mean_trip_data)
  
  mean_trip_data <- mean_trip_data %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
    .[]
  
  weighted_vars <- c(
    "tot_keep_cod_new", "tot_rel_cod_new", "tot_cat_cod_new",
    "tot_keep_hadd_new", "tot_rel_hadd_new", "tot_cat_hadd_new",
    "tot_keep_cod_base", "tot_rel_cod_base", "tot_cat_cod_base",
    "tot_keep_hadd_base", "tot_rel_hadd_base", "tot_cat_hadd_base",
    "probA", "prob0", "change_CS"
  )
  
  # Merge the calibrated number of choice occasions and calculate an expansion scalar
  
  results <- mean_trip_data %>%
    dplyr::left_join(calibration_data_table, by = "period2") %>%
    dplyr::group_by(period2) %>%
    dplyr::mutate(
      n_simulated_occasions = dplyr::n(),
      expand = n_choice_occasions / n_simulated_occasions
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(weighted_vars),
        ~ .x * expand
      )
    )
  
  # Aggregate to period by month.
  sims <- results %>%
    data.table::as.data.table() %>%
    .[
      ,
      c(
        lapply(.SD, sum),
        list(n_choice_occasions = sum(expand))
      ),
      by = c("period2", "month"),
      .SDcols = weighted_vars
    ]
  
  data.table::setnames(
    sims,
    old = c("prob0", "probA"),
    new = c("ntrips_base", "ntrips_alt")
  )
  
  sims$draw<- i
  
  stopifnot(
    isTRUE(all.equal(
      sum(sims$tot_keep_cod_new),
      diagnostic_results[[spec_index]]$total_cod_harvest_alt,
      tolerance = 1e-8
    )),
    isTRUE(all.equal(
      sum(sims$tot_keep_hadd_new),
      diagnostic_results[[spec_index]]$total_hadd_harvest_alt,
      tolerance = 1e-8
    )),
    isTRUE(all.equal(
      sum(sims$ntrips_alt),
      diagnostic_results[[spec_index]]$ntrips_alt,
      tolerance = 1e-8
    ))
  )
  
  sims_results[[spec_index]] <- sims
  
}

# Fishery output; same core result schema as main projections.
sims_all <- dplyr::bind_rows(sims_results)
diagnostic_output <- dplyr::bind_rows(diagnostic_results)

saveRDS(diagnostic_output, file.path(input_data_cd,
  paste0("projection_close_fishery_diagnostics_", yr, "_draw", i, ".rds")))
