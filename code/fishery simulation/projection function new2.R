
p_star_cod <- p_star_cod_variable
p_star_hadd<-p_star_hadd_variable

######################################
##   Begin simulating trip outcomes ##
######################################
cod_hadd_catch_data <- catch_data_all %>%
  dplyr::right_join(regs_check, by="period2") %>% 
  dplyr::select(-dtrip, -period) 

param_draws<- readRDS(paste0(input_data_cd, "costs_data_", y,"draw", i,".rds")) %>% 
  select(-month)

calibration_data_table<-readRDS(paste0(input_data_cd, "calibration_data_", y,"draw",i,".rds")) %>% 
  dplyr::select(period2, n_choice_occasions)


# loop around the the suffix on the catch variables 
specs<- c("corr_clayton1", "corr_clayton2","corr_clayton3","corr_clayton4","corr_clayton5","corr_clayton6","corr_clayton7","corr_clayton8",
          "corr_frank1", "corr_frank2","corr_frank3","corr_frank4","corr_frank5","corr_frank6","corr_frank7","corr_frank8",
          "corr_gaussian1", "corr_gaussian2","corr_gaussian3","corr_gaussian4","corr_gaussian5","corr_gaussian6","corr_gaussian7","corr_gaussian8",
          "corr_gumbel1", "corr_gumbel2","corr_gumbel3","corr_gumbel4","corr_gumbel5","corr_gumbel6","corr_gumbel7","corr_gumbel8",
          "corr_plackett1", "corr_plackett2","corr_plackett3","corr_plackett4","corr_plackett5","corr_plackett6","corr_plackett7","corr_plackett8")

for (k in specs){
  #k<-"corr_clayton1"
  # Start the clock!
  catch_data1<-cod_hadd_catch_data %>% 
    dplyr::select(mode, month, period2, state, tripid, area, catch_draw, paste0("cod_", k), paste0("had_", k)) %>% 
    dplyr::rename(tot_cod_catch=paste0("cod_", k), tot_hadd_catch=paste0("had_", k))
  
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
  
  #new code to replace fish expansion 
  # Perform the equivalent of a left_join
  catch_data1a <- regs[catch_data1a, nomatch = 0]
  
  # ensure bag_use is integer >=0
  catch_data1a[, cod_bag_use := data.table::fifelse(is.na(cod_bag) | cod_bag <= 0, 0L, as.integer(cod_bag))]
  
  # draw number of “eligible keeps”
  catch_data1a[, keep_raw_cod := stats::rbinom(.N, size = tot_cod_catch, prob = 1 - p_star_cod)]
  catch_data1a[, tot_keep_cod := pmin(keep_raw_cod, cod_bag_use)]
  catch_data1a[, tot_rel_cod  := tot_cod_catch - tot_keep_cod]
  
  # drop helpers
  catch_data1a[, c("keep_raw_cod","cod_bag_use") := NULL]
  
  # Select and rename columns
  catch_data1a <- catch_data1a[, .(tripid, tot_keep_cod , tot_rel_cod, period2, catch_draw, month)]
  
  summed_catch_data <- catch_data1a %>%
    as.data.table() %>%
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
  
  #new code to replace fish expansion 
  # Perform the equivalent of a left_join
  catch_data1a <- regs[catch_data1a, nomatch = 0]
  
  # ensure bag_use is integer >=0
  catch_data1a[, hadd_bag_use := data.table::fifelse(is.na(hadd_bag) | hadd_bag <= 0, 0L, as.integer(hadd_bag))]
  
  # draw number of “eligible keeps”
  catch_data1a[, keep_raw_hadd := stats::rbinom(.N, size = tot_hadd_catch, prob = 1 - p_star_hadd)]
  catch_data1a[, tot_keep_hadd := pmin(keep_raw_hadd, hadd_bag_use)]
  catch_data1a[, tot_rel_hadd  := tot_hadd_catch - tot_keep_hadd]
  
  # drop helpers
  catch_data1a[, c("keep_raw_hadd","hadd_bag_use") := NULL]
  
  # Select and rename columns
  catch_data1a <- catch_data1a[, .(tripid, tot_keep_hadd , tot_rel_hadd, period2, catch_draw, month)]
  
  summed_catch_data <- catch_data1a %>%
    as.data.table() %>%
    .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month" ), .SDcols = c("tot_keep_hadd", "tot_rel_hadd")]
  
  trip_data_hadd <- summed_catch_data %>%
    dplyr::bind_rows(hadd_zero_catch) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))
  
  
  trip_data<-trip_data %>% 
    dplyr::left_join(trip_data_hadd, by=c("period2", "catch_draw", "tripid",  "month"))

  
  trip_data<-trip_data %>% 
    dplyr::left_join(param_draws,by=c("period2", "catch_draw", "tripid"))
  
  trip_data <- trip_data %>%
    dplyr::mutate(period = as.numeric(as.factor(period2)))
  
  period_names<-subset(trip_data, select=c("period", "period2"))
  period_names <- period_names[!duplicated(period_names), ]
  
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
                  v0 = 
                    beta_sqrt_cod_keep*sqrt(tot_keep_cod_base) +
                    beta_sqrt_cod_release*sqrt(tot_rel_cod_base) +
                    beta_sqrt_hadd_keep*sqrt(tot_keep_hadd_base) +
                    beta_sqrt_hadd_release*sqrt(tot_rel_hadd_base) +
                    beta_sqrt_cod_hadd_keep*(sqrt(tot_keep_cod_base)*sqrt(tot_keep_hadd_base)) +
                    beta_cost*cost_base)
  
  
  trip_data<-trip_data %>% 
    dplyr::arrange(period2, tripid, catch_draw )
  
  mean_trip_data <- trip_data %>%
    data.table::data.table() %>% 
    .[, group_index := .GRP, by = .(period2, catch_draw, tripid)]
  
  # Now expand the data to create two alternatives, representing the alternatives available in choice survey
  mean_trip_data <- mean_trip_data %>%
    dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
    tidyr::uncount(n_alt) %>%
    dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                  opt_out = ifelse(alt == 2, 1, 0))
  

  setDT(mean_trip_data)
  
  # Filter only alt == 2 once, and calculate vA and v0
  mean_trip_data[alt == 2, c("vA", "v0") := .(
    beta_opt_out * opt_out +
      beta_opt_out_age * (age * opt_out) +
      beta_opt_out_likely * (likely_to_fish * opt_out) +
      beta_opt_out_prefer * (fish_pref_more * opt_out)
  )]
  
  
  mean_trip_data <- mean_trip_data %>% 
    dplyr::rename(tot_keep_cod_new=tot_keep_cod, 
                  tot_rel_cod_new=tot_rel_cod, 
                  tot_keep_hadd_new=tot_keep_hadd, 
                  tot_rel_hadd_new=tot_rel_hadd) %>% 
    dplyr::mutate(tot_cat_cod_new=tot_keep_cod_new+tot_rel_cod_new, 
                  tot_cat_hadd_new=tot_keep_hadd_new+tot_rel_hadd_new, 
                  tot_cat_cod_base=tot_keep_cod_base+tot_rel_cod_base, 
                  tot_cat_hadd_base=tot_keep_hadd_base+tot_rel_hadd_base)                
  
  # save catch draws to compute correlations
  trip_data_draws <- mean_trip_data %>% 
    dplyr::filter(
      alt == 1,
      catch_draw <= 10
    ) %>% 
    dplyr::select(
      period2, month, tripid,
      tot_keep_cod_new, tot_keep_hadd_new,
      tot_rel_cod_new, tot_rel_hadd_new,
      tot_cat_cod_new, tot_cat_hadd_new, 
      tot_keep_cod_base, tot_keep_hadd_base,
      tot_rel_cod_base, tot_rel_hadd_base,
      tot_cat_cod_base, tot_cat_hadd_base,
      catch_draw
    ) %>% 
    tidyr::separate(
      period2,
      into = c("mode", "period", "area", "state"),
      sep = "_",
      remove = FALSE
    ) %>% 
    dplyr::mutate(
      month = as.integer(month),
      state = as.character(state),
      mode = as.character(mode),
      area = as.character(area),
      domain3 = paste(state, mode, area, month, sep = "_"),
      tot_cod_catch_new = tot_keep_cod_new + tot_rel_cod_new,
      tot_hadd_catch_new = tot_keep_hadd_new + tot_rel_hadd_new,
      tot_cod_hadd_cat_new = tot_cat_cod_new + tot_cat_hadd_new,
      draw = i
    )
  
  trip_data_draws$draw=i
  

  mean_trip_data <- mean_trip_data %>% 
    dplyr::select(-beta_opt_out, -beta_opt_out_age, 
                  -beta_opt_out_likely, -beta_opt_out_prefer, -beta_sqrt_cod_hadd_keep, 
                  -beta_sqrt_cod_keep, -beta_sqrt_cod_release, -beta_sqrt_hadd_keep, 
                  -beta_sqrt_hadd_release, -likely_to_fish, -fish_pref_more,-opt_out, -age,
                  -group_index,-catch_draw,  -period, -cost_base  )
  all_vars<-c()
  all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid", "alt", "month")]
  all_vars
  
  
  # average outcomes across draws
  mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
    .[,lapply(.SD, mean), by = c("period2","tripid", "alt", "month"), .SDcols = all_vars]
  
  
  # Pre-compute exponential terms
  mean_trip_data[, `:=`(exp_vA = exp(vA), exp_v0 = exp(v0))]
  
  # Group by group_index and calculate probabilities and log-sums
  mean_trip_data[, `:=`(
    probA = exp_vA / sum(exp_vA),
    prob0 = exp_v0 / sum(exp_v0),
    log_sum_base = log(sum(exp_vA)),
    log_sum_alt = log(sum(exp_v0))
  ), by  = c("tripid", "period2", "month")]
  
  # Calculate consumer surplus 
  mean_trip_data[, `:=`(
    CS_base = log_sum_base / -beta_cost,
    CS_alt = log_sum_alt / -beta_cost
  )]
  
  # Calculate change consumer surplus 
  mean_trip_data[, `:=`(
    change_CS = CS_alt - CS_base
  )]
  
  mean(mean_trip_data$change_CS)
  ls(mean_trip_data)
  
  # Get rid of things we don't need.
  mean_trip_data <- mean_trip_data %>% 
    dplyr::filter(alt==1) %>% 
    dplyr::select(-c(alt, beta_cost, v0, vA,  
                     exp_vA, exp_v0, log_sum_base, log_sum_alt))
  

  # Multiply the trip probability by each of the catch variables to get probability-weighted catch
  list_names <- c("tot_keep_cod_new","tot_rel_cod_new", "tot_cat_cod_new",
                  "tot_keep_hadd_new", "tot_rel_hadd_new" , "tot_cat_hadd_new" )
  
  mean_trip_data<-mean_trip_data %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = list_names] %>%
    .[]
  
  
  # Multiply the trip probability in baseline year by each of the catch variables in the basleine year to get probability-weighted catch
  list_names <- c("tot_keep_cod_base","tot_rel_cod_base", "tot_cat_cod_base",
                  "tot_keep_hadd_base", "tot_rel_hadd_base" , "tot_cat_hadd_base"  )
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
    .[]
  

  mean_trip_data<-mean_trip_data %>% 
    left_join(calibration_data_table, by = "period2") 
  
  mean_trip_data <-mean_trip_data %>%
    group_by(period2) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(expand=n_choice_occasions/n_draws)
  
  mean_trip_data1 <- mean_trip_data %>% 
    mutate(uniform=runif(n(), min=0, max=1)) %>% 
    dplyr::arrange(period2, uniform) %>% 
    group_by(period2) %>%
    mutate(id_within_group = row_number()) %>% 
    filter(expand<1 & id_within_group<=n_choice_occasions) 
  
  mean_trip_data2 <- mean_trip_data %>% 
    filter(expand>1)  %>% 
    mutate(expand2=ceiling(expand)) 
  
  row_inds <- seq_len(nrow(mean_trip_data2))
  
  mean_trip_data2<-mean_trip_data2 %>% 
    slice(rep(row_inds,expand2))  
  
  mean_trip_data2 <- mean_trip_data2 %>%
    mutate(uniform=runif(n(), min=0, max=1)) %>% 
    dplyr::arrange(period2, uniform) %>% 
    group_by(period2) %>%
    mutate(id_within_group = row_number()) %>% 
    filter(id_within_group<=n_choice_occasions)
  
  results<-mean_trip_data1 %>% 
    dplyr::bind_rows(mean_trip_data2) %>% 
    mutate(n_choice_occasions=1)
  

  list_names = c("tot_keep_cod_new","tot_rel_cod_new", "tot_cat_cod_new",
                 "tot_keep_hadd_new", "tot_rel_hadd_new" , "tot_cat_hadd_new",
                 "tot_keep_cod_base","tot_rel_cod_base", "tot_cat_cod_base",
                 "tot_keep_hadd_base", "tot_rel_hadd_base" , "tot_cat_hadd_base",
                 "probA","prob0", "n_choice_occasions", "change_CS")
  
  sims <- results %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, sum),  by = c("period2", "month"), .SDcols = list_names]
  
  
  names(sims)[names(sims) == "prob0"] = "ntrips_base"
  names(sims)[names(sims) == "probA"] = "ntrips_alt"
  
  stratum_trip_weights <- sims %>%
    dplyr::select(
      period2,
      month,
      ntrips_alt
    ) %>%
    tidyr::separate(
      period2,
      into = c("mode", "period", "area", "state"),
      sep = "_",
      remove = FALSE
    ) %>%
    dplyr::mutate(
      month = as.integer(month),
      state = as.character(state),
      mode = as.character(mode),
      area = as.character(area),
      domain3 = paste(state, mode, area, month, sep = "_")
    ) %>%
    dplyr::group_by(
      state,
      mode,
      area,
      month,
      domain3
    ) %>%
    dplyr::summarise(
      ntrips_alt = sum(ntrips_alt, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # Calculate Kendall's tau separately within every
  # state-mode-area-month stratum.
  #
  # Catch draws are pooled within a stratum across the retained catch_draw
  # values, but never pooled across different strata before tau is computed.
  ktaus_by_stratum <- trip_data_draws %>%
    dplyr::group_by(
      state,
      mode,
      area,
      month,
      domain3
    ) %>%
    dplyr::group_modify(
      ~ compute_stratum_tau(.x)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      stratum_trip_weights,
      by = c(
        "state",
        "mode",
        "area",
        "month",
        "domain3"
      )
    ) %>%
    dplyr::mutate(
      draw = i,
      year = y,
      domain = k
    )
  
  
  # Check that all correlation strata received projected-trip weights.
  missing_tau_weights <- ktaus_by_stratum %>%
    dplyr::filter(
      is.na(ntrips_alt) |
        ntrips_alt <= 0
    )
  
  if (nrow(missing_tau_weights) > 0L) {
    warning(
      nrow(missing_tau_weights),
      " state-mode-area-month tau strata have missing or nonpositive ",
      "projected-trip weights."
    )
  }
  
  
  #-----------------------------------------------------------------------
  # Fishery-wide annual mean of within-stratum Kendall's tau
  #-----------------------------------------------------------------------
  
  ktaus_annual <- aggregate_stratum_tau(
    data = ktaus_by_stratum,
    reporting_vars = character(0),
    domain_label = "all"
  ) %>%
    dplyr::mutate(
      month = 0L,
      draw = i,
      year = y
    )
  
  
  #-----------------------------------------------------------------------
  # Month-specific mean of within-stratum Kendall's tau
  #-----------------------------------------------------------------------
  
  ktaus_month_all <- aggregate_stratum_tau(
    data = ktaus_by_stratum,
    reporting_vars = "month"
  ) %>%
    dplyr::mutate(
      domain1 = "all",
      draw = i,
      year = y
    )
  
  
  #-----------------------------------------------------------------------
  # Mode-specific annual means
  #-----------------------------------------------------------------------
  
  ktaus_mode <- aggregate_stratum_tau(
    data = ktaus_by_stratum,
    reporting_vars = "mode"
  ) %>%
    dplyr::transmute(
      domain1 = mode,
      month = 0L,
      draw = i,
      year = y,
      
      k_tau_keep_est,
      k_tau_keep_p,
      k_tau_catch_est,
      k_tau_catch_p,
      
      k_tau_keep_unweighted,
      k_tau_catch_unweighted,
      k_tau_keep_zero_filled,
      k_tau_catch_zero_filled,
      
      med_cod,
      med_hadd,
      n_strata,
      n_valid_keep_strata,
      n_valid_catch_strata,
      projected_trips
    )
  
  
  #-----------------------------------------------------------------------
  # State-specific annual means
  #-----------------------------------------------------------------------
  
  state_labels <- c(
    "23" = "ME",
    "25" = "MA",
    "33" = "NH"
  )
  
  ktaus_state <- aggregate_stratum_tau(
    data = ktaus_by_stratum,
    reporting_vars = "state"
  ) %>%
    dplyr::mutate(
      domain1 = dplyr::recode(
        as.character(state),
        !!!state_labels,
        .default = as.character(state)
      ),
      month = 0L,
      draw = i,
      year = y
    ) %>%
    dplyr::select(
      -state
    )
  
  
  #-----------------------------------------------------------------------
  # Combine reporting levels
  #-----------------------------------------------------------------------
  
  all_ktaus <- dplyr::bind_rows(
    ktaus_annual,
    ktaus_month_all,
    ktaus_state,
    ktaus_mode
  ) %>%
    dplyr::mutate(
      domain = k
    ) %>%
    dplyr::select(
      domain1,
      month,
      draw,
      year,
      domain,
      
      k_tau_keep_est,
      k_tau_keep_p,
      k_tau_catch_est,
      k_tau_catch_p,
      
      k_tau_keep_unweighted,
      k_tau_catch_unweighted,
      k_tau_keep_zero_filled,
      k_tau_catch_zero_filled,
      
      med_cod,
      med_hadd,
      
      n_strata,
      n_valid_keep_strata,
      n_valid_catch_strata,
      projected_trips
    )
  
  
  # Optional: retain the complete state-mode-area-month results.
  assign(
    paste0("ktaus_strata_new_", k),
    ktaus_by_stratum
  )
  
  
  sims <- sims %>% 
    separate(period2, into = c("mode", "period", "area", "state"), sep = "_") %>%
    dplyr::mutate(month = as.numeric(month)) %>% 
    dplyr::mutate(draw=i) 
  
  sims<-sims %>% select(-period)
  
  sims$domain<-k
  assign(paste0("sims_new_", k), sims)
  
  all_ktaus <- dplyr::bind_rows(
    ktaus_annual,
    ktaus_month_all,
    ktaus_state,
    ktaus_mode
  ) %>%
    dplyr::mutate(
      year = y,
      draw = i,
      domain = k
    )
  
  assign(
    paste0("all_ktaus_new_", k),
    all_ktaus
  )

  
}

#Fishery output
# Retrieve the results datasets as a list
dataset_names <- ls(pattern = "^sims_new_")

datasets <- mget(dataset_names)

# Combine datasets by stacking them
sims_all <- bind_rows(datasets)

# correlations output
# Retrieve the results datasets as a list
# State-mode-area-month Kendall's tau output
dataset_names <- ls(
  pattern = "^ktaus_strata_new_"
)

datasets <- mget(dataset_names)

ktaus_strata_all <- dplyr::bind_rows(
  datasets
)

dataset_names <- ls(
  pattern = "^all_ktaus_new_"
)

datasets <- mget(dataset_names)

ktaus_all <- dplyr::bind_rows(datasets)