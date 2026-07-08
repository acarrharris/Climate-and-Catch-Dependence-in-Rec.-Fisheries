

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

#Here we can loop around the the suffix on the catch variables 
# specs<- c("corr_clayton1", "corr_clayton2","corr_clayton3","corr_clayton4","corr_clayton5","corr_clayton6","corr_clayton7","corr_clayton8",
#           "ind_clayton1", "ind_clayton2","ind_clayton3","ind_clayton4","ind_clayton5","ind_clayton6","ind_clayton7","ind_clayton8",
#           "corr_frank1", "corr_frank2","corr_frank3","corr_frank4","corr_frank5","corr_frank6","corr_frank7","corr_frank8",
#           "ind_frank1", "ind_frank2","ind_frank3","ind_frank4","ind_frank5","ind_frank6","ind_frank7","ind_frank8",
#           "corr_gaussian1", "corr_gaussian2","corr_gaussian3","corr_gaussian4","corr_gaussian5","corr_gaussian6","corr_gaussian7","corr_gaussian8",
#           "ind_gaussian1", "ind_gaussian2","ind_gaussian3","ind_gaussian4","ind_gaussian5","ind_gaussian6","ind_gaussian7","ind_gaussian8",
#           "corr_gumbel1", "corr_gumbel2","corr_gumbel3","corr_gumbel4","corr_gumbel5","corr_gumbel6","corr_gumbel7","corr_gumbel8",
#           "ind_gumbel1", "ind_gumbel2","ind_gumbel3","ind_gumbel4","ind_gumbel5","ind_gumbel6","ind_gumbel7","ind_gumbel8",
#           "corr_plackett1", "corr_plackett2","corr_plackett3","corr_plackett4","corr_plackett5","corr_plackett6","corr_plackett7","corr_plackett8",
#           "ind_plackett1", "ind_plackett2","ind_plackett3","ind_plackett4","ind_plackett5","ind_plackett6","ind_plackett7","ind_plackett8")

specs<- c("ind_clayton1", "ind_clayton2","ind_clayton3","ind_clayton4","ind_clayton5","ind_clayton6","ind_clayton7","ind_clayton8",
          "ind_frank1", "ind_frank2","ind_frank3","ind_frank4","ind_frank5","ind_frank6","ind_frank7","ind_frank8",
          "ind_gaussian1", "ind_gaussian2","ind_gaussian3","ind_gaussian4","ind_gaussian5","ind_gaussian6","ind_gaussian7","ind_gaussian8",
          "ind_gumbel1", "ind_gumbel2","ind_gumbel3","ind_gumbel4","ind_gumbel5","ind_gumbel6","ind_gumbel7","ind_gumbel8",
          "ind_plackett1", "ind_plackett2","ind_plackett3","ind_plackett4","ind_plackett5","ind_plackett6","ind_plackett7","ind_plackett8")

for (k in specs){
  #k<-"ind_frank8"
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
  
  
  #  utility (prediction year)
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
  
  # save the first catch draw to compute correlations
  trip_data_draws<-mean_trip_data %>% 
    filter(catch_draw<=10) %>% 
    select(period2, month, tripid, tot_keep_cod_new,tot_keep_hadd_new, tot_rel_cod_new, tot_rel_hadd_new, tot_cat_cod_new, tot_cat_hadd_new, 
           tot_keep_cod_base,tot_keep_hadd_base, tot_rel_cod_base, tot_rel_hadd_base, tot_cat_cod_base, tot_cat_hadd_base, catch_draw) %>% 
    tidyr::separate(period2, into = c("mode", "period", "area", "state")) %>% 
    dplyr::mutate(period2 = paste0(mode, "_", period, "_", area, "_", state), 
                  tot_cod_hadd_cat_new=tot_cat_cod_new+tot_cat_hadd_new) 
  
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
  
  
  # Assess the inter-species correlation in catch versus  correlation in keep 
  # a) Draw 10,000 catch draws in proportion to the projected number of trips across the fishing year 
  # b) Compute kendall's tau
  
  dtrip_wts<- sims %>%
    dplyr::group_by(period2) %>%
    dplyr::summarise(ntrips_alt = sum(ntrips_alt),
                     .groups="drop") %>%
    dplyr::ungroup()    %>%
    mutate(weight = ntrips_alt / sum(ntrips_alt)) 
  
  # Data for fishery-wide ktau's
  keep_rel_pairs_annual <-data.table::as.data.table(trip_data_draws) 
  
  keep_rel_pairs_annual <- keep_rel_pairs_annual %>%
    left_join(dtrip_wts, by = "period2")
  
  keep_rel_pairs_annual <- keep_rel_pairs_annual %>%
    slice_sample(n = 2000, weight_by = weight) %>%
    dplyr::mutate(tot_cod_catch_new=tot_keep_cod_new+tot_rel_cod_new,
                  tot_hadd_catch_new=tot_keep_hadd_new+tot_rel_hadd_new, 
                  tot_cod_hadd_catch_new=tot_cod_catch_new+tot_hadd_catch_new)
  
  # Data for mode-specific ktau's
  keep_rel_pairs_fh <-data.table::as.data.table(trip_data_draws) %>% 
    dplyr::filter(mode=="fh")
  
  keep_rel_pairs_fh <- keep_rel_pairs_fh %>%
    left_join(dtrip_wts, by = "period2")
  
  keep_rel_pairs_fh <- keep_rel_pairs_fh %>%
    slice_sample(n = 2000, weight_by = weight) %>%
    dplyr::mutate(tot_cod_catch_new=tot_keep_cod_new+tot_rel_cod_new,
                  tot_hadd_catch_new=tot_keep_hadd_new+tot_rel_hadd_new, 
                  tot_cod_hadd_catch_new=tot_cod_catch_new+tot_hadd_catch_new)
  
  keep_rel_pairs_pr <-data.table::as.data.table(trip_data_draws) %>% 
    dplyr::filter(mode=="pr")
  
  keep_rel_pairs_pr <- keep_rel_pairs_pr %>%
    left_join(dtrip_wts, by = "period2")
  
  keep_rel_pairs_pr <- keep_rel_pairs_pr %>%
    slice_sample(n = 2000, weight_by = weight) %>%
    dplyr::mutate(tot_cod_catch_new=tot_keep_cod_new+tot_rel_cod_new,
                  tot_hadd_catch_new=tot_keep_hadd_new+tot_rel_hadd_new, 
                  tot_cod_hadd_catch_new=tot_cod_catch_new+tot_hadd_catch_new)
  
  # Data for state-specific ktau's
  #MA
  keep_rel_pairs_MA <-data.table::as.data.table(trip_data_draws) %>% 
    dplyr::filter(state==25)
  
  keep_rel_pairs_MA <- keep_rel_pairs_MA %>%
    left_join(dtrip_wts, by = "period2")
  
  keep_rel_pairs_MA <- keep_rel_pairs_MA %>%
    slice_sample(n = 2000, weight_by = weight) %>%
    dplyr::mutate(tot_cod_catch_new=tot_keep_cod_new+tot_rel_cod_new,
                  tot_hadd_catch_new=tot_keep_hadd_new+tot_rel_hadd_new, 
                  tot_cod_hadd_catch_new=tot_cod_catch_new+tot_hadd_catch_new)
  
  #NH
  keep_rel_pairs_NH <-data.table::as.data.table(trip_data_draws) %>% 
    dplyr::filter(state==33)
  
  keep_rel_pairs_NH <- keep_rel_pairs_NH %>%
    left_join(dtrip_wts, by = "period2")
  
  keep_rel_pairs_NH <- keep_rel_pairs_NH %>%
    slice_sample(n = 2000, weight_by = weight) %>%
    dplyr::mutate(tot_cod_catch_new=tot_keep_cod_new+tot_rel_cod_new,
                  tot_hadd_catch_new=tot_keep_hadd_new+tot_rel_hadd_new, 
                  tot_cod_hadd_catch_new=tot_cod_catch_new+tot_hadd_catch_new)
  
  #ME
  keep_rel_pairs_ME <-data.table::as.data.table(trip_data_draws) %>% 
    dplyr::filter(state==23)
  
  keep_rel_pairs_ME <- keep_rel_pairs_ME %>%
    left_join(dtrip_wts, by = "period2")
  
  keep_rel_pairs_ME <- keep_rel_pairs_ME %>%
    slice_sample(n = 2000, weight_by = weight) %>%
    dplyr::mutate(tot_cod_catch_new=tot_keep_cod_new+tot_rel_cod_new,
                  tot_hadd_catch_new=tot_keep_hadd_new+tot_rel_hadd_new, 
                  tot_cod_hadd_catch_new=tot_cod_catch_new+tot_hadd_catch_new)
  
  
  # Data for month-specific ktau's
  ktaus_month<-list()
  
  for (m in unique(trip_data_draws$month)){
    trip_data_draws_month<-trip_data_draws %>% 
      dplyr::filter(month==m)  %>%
      dplyr::left_join(dtrip_wts, by = "period2") %>% 
      dplyr::group_by(month) %>%
      dplyr::mutate(total_weight_month = sum(ntrips_alt, na.rm = TRUE),
                    pct_of_month = (ntrips_alt / total_weight_month)) %>% 
      dplyr::ungroup() %>% 
      slice_sample(n = 2000, weight_by = pct_of_month) %>%
      dplyr::mutate(tot_cod_catch_new=tot_keep_cod_new+tot_rel_cod_new,
                    tot_hadd_catch_new=tot_keep_hadd_new+tot_rel_hadd_new, 
                    tot_cod_hadd_catch_new=tot_cod_catch_new+tot_hadd_catch_new) 
    
    sum_keep_cod<-sum(trip_data_draws_month$tot_keep_cod_new)
    sum_keep_hadd<-sum(trip_data_draws_month$tot_keep_hadd_new)
    sum_catch_cod<-sum(trip_data_draws_month$tot_cat_cod_new)
    sum_catch_hadd<-sum(trip_data_draws_month$tot_cat_hadd_new)
    
    
    # compute medians
    med_cod <- median(trip_data_draws_month$tot_cat_cod_new, na.rm = TRUE)
    med_hadd <- median(trip_data_draws_month$tot_cat_hadd_new, na.rm = TRUE)
    
    # Loop over percentiles and classify trips
    # Define percentiles of interest
    percentiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
    
    # Compute percentiles for cod and haddock
    cod_q <- quantile(trip_data_draws_month$tot_cat_cod_new, probs = percentiles, na.rm = TRUE)
    hadd_q <- quantile(trip_data_draws_month$tot_cat_hadd_new, probs = percentiles, na.rm = TRUE)
    
    # Loop over percentile *indices* instead of names
    results <- purrr::map_dfr(seq_along(percentiles), function(z) {
      cod_thr <- cod_q[[z]]
      hadd_thr <- hadd_q[[z]]
      
      tibble::tibble(
        percentile = percentiles[z] * 100,  # express as percent (e.g., 10, 25, 50...)
        cod_threshold = cod_thr,
        hadd_threshold = hadd_thr,
        both_above = sum(trip_data_draws_month$tot_cat_cod_new > cod_thr &
                           trip_data_draws_month$tot_cat_hadd_new > hadd_thr, na.rm = TRUE),
        both_below = sum(trip_data_draws_month$tot_cat_cod_new <= cod_thr &
                           trip_data_draws_month$tot_cat_hadd_new <= hadd_thr, na.rm = TRUE)
      )
    })
    
    # Reshape wide so there is one row
    results_wide <- results %>%
      tidyr::pivot_wider(
        names_from = percentile,
        values_from = c(cod_threshold, hadd_threshold, both_above, both_below),
        names_glue = "{.value}_{percentile}"
      )
    
    
    # classify trips relative to medians
    # meds <- trip_data_draws_month %>%
    #   dplyr::summarise(
    #     both_above = sum(tot_cat_cod_new > med_cod & tot_cat_hadd_new > med_hadd, na.rm = TRUE),
    #     both_below = sum(tot_cat_cod_new <= med_cod & tot_cat_hadd_new <= med_hadd, na.rm = TRUE)
    #   )
    # 
    # both_above<-mean(meds$both_above)
    # both_below<-mean(meds$both_below)
    
    #assign(paste0("keep_rel_pairs_", m), trip_data_draws_month)
    
    if(sum_keep_cod>0 & sum_keep_hadd>0){
      
      ktau_keep<- cor.test(trip_data_draws_month$tot_keep_cod_new,
                           trip_data_draws_month$tot_keep_hadd_new, method = c("kendall"))
      
      k_tau_keep_est<-ktau_keep[["estimate"]]
      k_tau_keep_p<- ktau_keep[["p.value"]]
      
    }
    
    if(sum_keep_cod==0 | sum_keep_hadd==0){
      
      k_tau_keep_est<-0
      k_tau_keep_p<- 1
      
    }
    
    if(sum_catch_cod>0 & sum_catch_hadd>0){
      
      ktau_catch<- cor.test(trip_data_draws_month$tot_cod_catch_new,
                            trip_data_draws_month$tot_hadd_catch_new, method = c("kendall"))
      
      k_tau_catch_est<-ktau_catch[["estimate"]]
      k_tau_catch_p<- ktau_catch[["p.value"]]
    }
    
    if(sum_catch_cod==0 | sum_catch_hadd==0){
      
      k_tau_catch_est<-0
      k_tau_catch_p<- 1
      
      
    }
    # Merge everything into one row of ktaus_annual
    ktaus_month[[m]] <- cbind(data.frame( k_tau_keep_est, k_tau_keep_p, k_tau_catch_est,k_tau_catch_p,med_cod,
                                          med_hadd),results_wide)
    # ktaus_month[[m]]<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p, 
    #                                        med_cod, med_hadd, both_above, both_below), names="TRUE")
    ktaus_month[[m]]$domain<-"all"
    ktaus_month[[m]]$draw<-i
    ktaus_month[[m]]$month<-m
    
  }
  
  ktaus_month_all<-as.data.frame(list.stack(ktaus_month,  fill=TRUE))
  
  #keep_rel_pairs_annual %>% count(period2)
  
  
  
  # # Reshape z into matrix form
  # keep_rel_pairs_annual_filtered<-keep_rel_pairs_annual %>% 
  #   dplyr::filter(tot_cod_catch_new!=0 & tot_hadd_catch_new!=0) #remove zero catch of both species
  # 
  # z_matrix <- acast(keep_rel_pairs_annual_filtered, tot_cod_catch_new ~ tot_hadd_catch_new, value.var = "tot_cod_hadd_catch_new")
  # 
  # # Get unique x and y
  # x_vals <- sort(unique(keep_rel_pairs_annual_filtered$tot_cod_catch_new))
  # y_vals <- sort(unique(keep_rel_pairs_annual_filtered$tot_hadd_catch_new))
  # 
  # # Plot
  # persp3d(x_vals, y_vals, z_matrix,
  #         col = "lightgreen", xlab = "tot_cod_catch_new", ylab = "tot_hadd_catch_new", zlab = "tot_cod_hadd_catch_new")
  # 
  
  ###Annual ktau estimates
  sum_keep_cod<-sum(keep_rel_pairs_annual$tot_keep_cod_new)
  sum_keep_hadd<-sum(keep_rel_pairs_annual$tot_keep_hadd_new)
  sum_catch_cod<-sum(keep_rel_pairs_annual$tot_cat_cod_new)
  sum_catch_hadd<-sum(keep_rel_pairs_annual$tot_cat_hadd_new)
  
  # Loop over percentiles and classify trips
  # Define percentiles of interest
  percentiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  
  # Compute percentiles for cod and haddock
  cod_q <- quantile(keep_rel_pairs_annual$tot_cat_cod_new, probs = percentiles, na.rm = TRUE)
  hadd_q <- quantile(keep_rel_pairs_annual$tot_cat_hadd_new, probs = percentiles, na.rm = TRUE)
  
  # Loop over percentile *indices* instead of names
  results <- purrr::map_dfr(seq_along(percentiles), function(z) {
    cod_thr <- cod_q[[z]]
    hadd_thr <- hadd_q[[z]]
    
    tibble::tibble(
      percentile = percentiles[z] * 100,  # express as percent (e.g., 10, 25, 50...)
      cod_threshold = cod_thr,
      hadd_threshold = hadd_thr,
      both_above = sum(keep_rel_pairs_annual$tot_cat_cod_new > cod_thr &
                         keep_rel_pairs_annual$tot_cat_hadd_new > hadd_thr, na.rm = TRUE),
      both_below = sum(keep_rel_pairs_annual$tot_cat_cod_new <= cod_thr &
                         keep_rel_pairs_annual$tot_cat_hadd_new <= hadd_thr, na.rm = TRUE)
    )
  })
  
  # Reshape wide so there is one row
  results_wide <- results %>%
    tidyr::pivot_wider(
      names_from = percentile,
      values_from = c(cod_threshold, hadd_threshold, both_above, both_below),
      names_glue = "{.value}_{percentile}"
    )
  
  
  # compute medians
  med_cod <- median(keep_rel_pairs_annual$tot_cat_cod_new, na.rm = TRUE)
  med_hadd <- median(keep_rel_pairs_annual$tot_cat_hadd_new, na.rm = TRUE)
  
  # classify trips relative to medians
  # meds <- keep_rel_pairs_annual %>%
  #   dplyr::summarise(
  #     both_above = sum(tot_cat_cod_new > med_cod & tot_cat_hadd_new > med_hadd, na.rm = TRUE),
  #     both_below = sum(tot_cat_cod_new <= med_cod & tot_cat_hadd_new <= med_hadd, na.rm = TRUE)
  #   )
  # 
  # both_above<-mean(meds$both_above)
  # both_below<-mean(meds$both_below)
  
  if(sum_keep_cod>0 & sum_keep_hadd>0){
    
    ktau_keep<- cor.test(keep_rel_pairs_annual$tot_keep_cod_new,
                         keep_rel_pairs_annual$tot_keep_hadd_new, method = c("kendall"))
    
    k_tau_keep_est<-ktau_keep[["estimate"]]
    k_tau_keep_p<- ktau_keep[["p.value"]]
  }
  
  if(sum_keep_cod==0 | sum_keep_hadd==0){
    
    k_tau_keep_est<-0
    k_tau_keep_p<- 1
  }
  
  
  if(sum_catch_cod>0 & sum_catch_hadd>0){
    
    ktau_catch<- cor.test(keep_rel_pairs_annual$tot_cod_catch_new,
                          keep_rel_pairs_annual$tot_hadd_catch_new, method = c("kendall"))
    
    k_tau_catch_est<-ktau_catch[["estimate"]]
    k_tau_catch_p<- ktau_catch[["p.value"]]
  }
  
  if(sum_catch_cod==0 | sum_catch_hadd==0){
    
    k_tau_catch_est<-0
    k_tau_catch_p<- 1
  }
  
  # Merge everything into one row of ktaus_annual
  ktaus_annual <- cbind(data.frame( k_tau_keep_est, k_tau_keep_p, k_tau_catch_est,k_tau_catch_p,med_cod,
                                    med_hadd),results_wide)
  
  # ktaus_annual<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p, 
  #                                    med_cod, med_hadd, both_above, both_below), names="TRUE")
  
  ktaus_annual$domain<-"all"
  ktaus_annual$draw<-i
  ktaus_annual$month<-0
  
  
  ###for-hire ktau estimates
  sum_keep_cod<-sum(keep_rel_pairs_fh$tot_keep_cod_new)
  sum_keep_hadd<-sum(keep_rel_pairs_fh$tot_keep_hadd_new)
  sum_catch_cod<-sum(keep_rel_pairs_fh$tot_cat_cod_new)
  sum_catch_hadd<-sum(keep_rel_pairs_fh$tot_cat_hadd_new)
  
  # compute medians
  med_cod <- median(keep_rel_pairs_fh$tot_cat_cod_new, na.rm = TRUE)
  med_hadd <- median(keep_rel_pairs_fh$tot_cat_hadd_new, na.rm = TRUE)
  
  # Loop over percentiles and classify trips
  # Define percentiles of interest
  percentiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  
  # Compute percentiles for cod and haddock
  cod_q <- quantile(keep_rel_pairs_fh$tot_cat_cod_new, probs = percentiles, na.rm = TRUE)
  hadd_q <- quantile(keep_rel_pairs_fh$tot_cat_hadd_new, probs = percentiles, na.rm = TRUE)
  
  # Loop over percentile *indices* instead of names
  results <- purrr::map_dfr(seq_along(percentiles), function(z) {
    cod_thr <- cod_q[[z]]
    hadd_thr <- hadd_q[[z]]
    
    tibble::tibble(
      percentile = percentiles[z] * 100,  # express as percent (e.g., 10, 25, 50...)
      cod_threshold = cod_thr,
      hadd_threshold = hadd_thr,
      both_above = sum(keep_rel_pairs_fh$tot_cat_cod_new > cod_thr &
                         keep_rel_pairs_fh$tot_cat_hadd_new > hadd_thr, na.rm = TRUE),
      both_below = sum(keep_rel_pairs_fh$tot_cat_cod_new <= cod_thr &
                         keep_rel_pairs_fh$tot_cat_hadd_new <= hadd_thr, na.rm = TRUE)
    )
  })
  
  # Reshape wide so there is one row
  results_wide <- results %>%
    tidyr::pivot_wider(
      names_from = percentile,
      values_from = c(cod_threshold, hadd_threshold, both_above, both_below),
      names_glue = "{.value}_{percentile}"
    )
  
  
  # classify trips relative to medians
  # meds <- keep_rel_pairs_fh %>%
  #   dplyr::summarise(
  #     both_above = sum(tot_cat_cod_new > med_cod & tot_cat_hadd_new > med_hadd, na.rm = TRUE),
  #     both_below = sum(tot_cat_cod_new <= med_cod & tot_cat_hadd_new <= med_hadd, na.rm = TRUE)
  #   )
  # 
  # both_above<-mean(meds$both_above)
  # both_below<-mean(meds$both_below)
  
  
  if(sum_keep_cod>0 & sum_keep_hadd>0){
    
    ktau_keep<- cor.test(keep_rel_pairs_fh$tot_keep_cod_new,
                         keep_rel_pairs_fh$tot_keep_hadd_new, method = c("kendall"))
    
    k_tau_keep_est<-ktau_keep[["estimate"]]
    k_tau_keep_p<- ktau_keep[["p.value"]]
  }
  
  if(sum_keep_cod==0 | sum_keep_hadd==0){
    
    k_tau_keep_est<-0
    k_tau_keep_p<- 1
  }
  
  
  if(sum_catch_cod>0 & sum_catch_hadd>0){
    
    ktau_catch<- cor.test(keep_rel_pairs_fh$tot_cod_catch_new,
                          keep_rel_pairs_fh$tot_hadd_catch_new, method = c("kendall"))
    
    k_tau_catch_est<-ktau_catch[["estimate"]]
    k_tau_catch_p<- ktau_catch[["p.value"]]
  }
  
  if(sum_catch_cod==0 | sum_catch_hadd==0){
    
    k_tau_catch_est<-0
    k_tau_catch_p<- 1
  }
  
  # Merge everything into one row of ktaus_annual
  ktaus_fh <- cbind(data.frame( k_tau_keep_est, k_tau_keep_p, k_tau_catch_est,k_tau_catch_p,med_cod,
                                med_hadd),results_wide)
  
  # ktaus_fh<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p, 
  #                                med_cod, med_hadd, both_above, both_below), names="TRUE")
  ktaus_fh$domain<-"fh"
  ktaus_fh$draw<-i
  ktaus_fh$month<-0
  
  ###pr ktau estimates
  sum_keep_cod<-sum(keep_rel_pairs_pr$tot_keep_cod_new)
  sum_keep_hadd<-sum(keep_rel_pairs_pr$tot_keep_hadd_new)
  sum_catch_cod<-sum(keep_rel_pairs_pr$tot_cat_cod_new)
  sum_catch_hadd<-sum(keep_rel_pairs_pr$tot_cat_hadd_new)
  
  # compute medians
  med_cod <- median(keep_rel_pairs_pr$tot_cat_cod_new, na.rm = TRUE)
  med_hadd <- median(keep_rel_pairs_pr$tot_cat_hadd_new, na.rm = TRUE)
  
  # Define percentiles of interest
  percentiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  
  # Compute percentiles for cod and haddock
  cod_q <- quantile(keep_rel_pairs_pr$tot_cat_cod_new, probs = percentiles, na.rm = TRUE)
  hadd_q <- quantile(keep_rel_pairs_pr$tot_cat_hadd_new, probs = percentiles, na.rm = TRUE)
  
  # Loop over percentile *indices* instead of names
  results <- purrr::map_dfr(seq_along(percentiles), function(z) {
    cod_thr <- cod_q[[z]]
    hadd_thr <- hadd_q[[z]]
    
    tibble::tibble(
      percentile = percentiles[z] * 100,  # express as percent (e.g., 10, 25, 50...)
      cod_threshold = cod_thr,
      hadd_threshold = hadd_thr,
      both_above = sum(keep_rel_pairs_pr$tot_cat_cod_new > cod_thr &
                         keep_rel_pairs_pr$tot_cat_hadd_new > hadd_thr, na.rm = TRUE),
      both_below = sum(keep_rel_pairs_pr$tot_cat_cod_new <= cod_thr &
                         keep_rel_pairs_pr$tot_cat_hadd_new <= hadd_thr, na.rm = TRUE)
    )
  })
  
  # Reshape wide so there is one row
  results_wide <- results %>%
    tidyr::pivot_wider(
      names_from = percentile,
      values_from = c(cod_threshold, hadd_threshold, both_above, both_below),
      names_glue = "{.value}_{percentile}"
    )
  
  
  if(sum_keep_cod>0 & sum_keep_hadd>0){
    
    ktau_keep<- cor.test(keep_rel_pairs_pr$tot_keep_cod_new,
                         keep_rel_pairs_pr$tot_keep_hadd_new, method = c("kendall"))
    
    k_tau_keep_est<-ktau_keep[["estimate"]]
    k_tau_keep_p<- ktau_keep[["p.value"]]
  }
  
  if(sum_keep_cod==0 | sum_keep_hadd==0){
    
    k_tau_keep_est<-0
    k_tau_keep_p<- 1
  }
  
  
  if(sum_catch_cod>0 & sum_catch_hadd>0){
    
    ktau_catch<- cor.test(keep_rel_pairs_pr$tot_cod_catch_new,
                          keep_rel_pairs_pr$tot_hadd_catch_new, method = c("kendall"))
    
    k_tau_catch_est<-ktau_catch[["estimate"]]
    k_tau_catch_p<- ktau_catch[["p.value"]]
  }
  
  if(sum_catch_cod==0 | sum_catch_hadd==0){
    
    k_tau_catch_est<-0
    k_tau_catch_p<- 1
  }
  
  # Merge everything into one row of ktaus_annual
  ktaus_pr <- cbind(data.frame( k_tau_keep_est, k_tau_keep_p, k_tau_catch_est,k_tau_catch_p,med_cod,
                                med_hadd),results_wide)
  # 
  # ktaus_pr<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p, 
  #                                med_cod, med_hadd, both_above, both_below), names="TRUE")
  ktaus_pr$domain<-"pr"
  ktaus_pr$draw<-i
  ktaus_pr$month<-0
  
  
  ###NH ktau estimates
  sum_keep_cod<-sum(keep_rel_pairs_NH$tot_keep_cod_new)
  sum_keep_hadd<-sum(keep_rel_pairs_NH$tot_keep_hadd_new)
  sum_catch_cod<-sum(keep_rel_pairs_NH$tot_cat_cod_new)
  sum_catch_hadd<-sum(keep_rel_pairs_NH$tot_cat_hadd_new)
  
  # compute medians
  med_cod <- median(keep_rel_pairs_NH$tot_cat_cod_new, na.rm = TRUE)
  med_hadd <- median(keep_rel_pairs_NH$tot_cat_hadd_new, na.rm = TRUE)
  
  # Define percentiles of interest
  percentiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  
  # Compute percentiles for cod and haddock
  cod_q <- quantile(keep_rel_pairs_NH$tot_cat_cod_new, probs = percentiles, na.rm = TRUE)
  hadd_q <- quantile(keep_rel_pairs_NH$tot_cat_hadd_new, probs = percentiles, na.rm = TRUE)
  
  # Loop over percentile *indices* instead of names
  results <- purrr::map_dfr(seq_along(percentiles), function(z) {
    cod_thr <- cod_q[[z]]
    hadd_thr <- hadd_q[[z]]
    
    tibble::tibble(
      percentile = percentiles[z] * 100,  # express as percent (e.g., 10, 25, 50...)
      cod_threshold = cod_thr,
      hadd_threshold = hadd_thr,
      both_above = sum(keep_rel_pairs_NH$tot_cat_cod_new > cod_thr &
                         keep_rel_pairs_NH$tot_cat_hadd_new > hadd_thr, na.rm = TRUE),
      both_below = sum(keep_rel_pairs_NH$tot_cat_cod_new <= cod_thr &
                         keep_rel_pairs_NH$tot_cat_hadd_new <= hadd_thr, na.rm = TRUE)
    )
  })
  
  # Reshape wide so there is one row
  results_wide <- results %>%
    tidyr::pivot_wider(
      names_from = percentile,
      values_from = c(cod_threshold, hadd_threshold, both_above, both_below),
      names_glue = "{.value}_{percentile}"
    )
  
  
  
  if(sum_keep_cod>0 & sum_keep_hadd>0){
    
    ktau_keep<- cor.test(keep_rel_pairs_NH$tot_keep_cod_new,
                         keep_rel_pairs_NH$tot_keep_hadd_new, method = c("kendall"))
    
    k_tau_keep_est<-ktau_keep[["estimate"]]
    k_tau_keep_p<- ktau_keep[["p.value"]]
  }
  
  if(sum_keep_cod==0 | sum_keep_hadd==0){
    
    k_tau_keep_est<-0
    k_tau_keep_p<- 1
  }
  
  
  if(sum_catch_cod>0 & sum_catch_hadd>0){
    
    ktau_catch<- cor.test(keep_rel_pairs_NH$tot_cod_catch_new,
                          keep_rel_pairs_NH$tot_hadd_catch_new, method = c("kendall"))
    
    k_tau_catch_est<-ktau_catch[["estimate"]]
    k_tau_catch_p<- ktau_catch[["p.value"]]
  }
  
  if(sum_catch_cod==0 | sum_catch_hadd==0){
    
    k_tau_catch_est<-0
    k_tau_catch_p<- 1
  }
  
  # Merge everything into one row of ktaus_annual
  ktaus_NH <- cbind(data.frame( k_tau_keep_est, k_tau_keep_p, k_tau_catch_est,k_tau_catch_p,med_cod,
                                med_hadd),results_wide)
  
  # ktaus_NH<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p, 
  #                                med_cod, med_hadd, both_above, both_below), names="TRUE")
  ktaus_NH$domain<-"NH"
  ktaus_NH$draw<-i
  ktaus_NH$month<-0
  
  
  ###MA ktau estimates
  
  sum_keep_cod<-sum(keep_rel_pairs_MA$tot_keep_cod_new)
  sum_keep_hadd<-sum(keep_rel_pairs_MA$tot_keep_hadd_new)
  sum_catch_cod<-sum(keep_rel_pairs_MA$tot_cat_cod_new)
  sum_catch_hadd<-sum(keep_rel_pairs_MA$tot_cat_hadd_new)
  
  # compute medians
  med_cod <- median(keep_rel_pairs_MA$tot_cat_cod_new, na.rm = TRUE)
  med_hadd <- median(keep_rel_pairs_MA$tot_cat_hadd_new, na.rm = TRUE)
  
  # Define percentiles of interest
  percentiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  
  # Compute percentiles for cod and haddock
  cod_q <- quantile(keep_rel_pairs_MA$tot_cat_cod_new, probs = percentiles, na.rm = TRUE)
  hadd_q <- quantile(keep_rel_pairs_MA$tot_cat_hadd_new, probs = percentiles, na.rm = TRUE)
  
  # Loop over percentile *indices* instead of names
  results <- purrr::map_dfr(seq_along(percentiles), function(z) {
    cod_thr <- cod_q[[z]]
    hadd_thr <- hadd_q[[z]]
    
    tibble::tibble(
      percentile = percentiles[z] * 100,  # express as percent (e.g., 10, 25, 50...)
      cod_threshold = cod_thr,
      hadd_threshold = hadd_thr,
      both_above = sum(keep_rel_pairs_MA$tot_cat_cod_new > cod_thr &
                         keep_rel_pairs_MA$tot_cat_hadd_new > hadd_thr, na.rm = TRUE),
      both_below = sum(keep_rel_pairs_MA$tot_cat_cod_new <= cod_thr &
                         keep_rel_pairs_MA$tot_cat_hadd_new <= hadd_thr, na.rm = TRUE)
    )
  })
  
  # Reshape wide so there is one row
  results_wide <- results %>%
    tidyr::pivot_wider(
      names_from = percentile,
      values_from = c(cod_threshold, hadd_threshold, both_above, both_below),
      names_glue = "{.value}_{percentile}"
    )
  
  
  if(sum_keep_cod>0 & sum_keep_hadd>0){
    
    ktau_keep<- cor.test(keep_rel_pairs_MA$tot_keep_cod_new,
                         keep_rel_pairs_MA$tot_keep_hadd_new, method = c("kendall"))
    
    k_tau_keep_est<-ktau_keep[["estimate"]]
    k_tau_keep_p<- ktau_keep[["p.value"]]
  }
  
  if(sum_keep_cod==0 | sum_keep_hadd==0){
    
    k_tau_keep_est<-0
    k_tau_keep_p<- 1
  }
  
  
  if(sum_catch_cod>0 & sum_catch_hadd>0){
    
    ktau_catch<- cor.test(keep_rel_pairs_MA$tot_cod_catch_new,
                          keep_rel_pairs_MA$tot_hadd_catch_new, method = c("kendall"))
    
    k_tau_catch_est<-ktau_catch[["estimate"]]
    k_tau_catch_p<- ktau_catch[["p.value"]]
  }
  
  if(sum_catch_cod==0 | sum_catch_hadd==0){
    
    k_tau_catch_est<-0
    k_tau_catch_p<- 1
  }
  
  # Merge everything into one row of ktaus_annual
  ktaus_MA <- cbind(data.frame( k_tau_keep_est, k_tau_keep_p, k_tau_catch_est,k_tau_catch_p,med_cod,
                                med_hadd),results_wide)
  
  # ktaus_MA<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p, 
  #                                med_cod, med_hadd, both_above, both_below), names="TRUE")
  ktaus_MA$domain<-"MA"
  ktaus_MA$draw<-i
  ktaus_MA$month<-0
  
  ###ME ktau estimates
  sum_keep_cod<-sum(keep_rel_pairs_ME$tot_keep_cod_new)
  sum_keep_hadd<-sum(keep_rel_pairs_ME$tot_keep_hadd_new)
  sum_catch_cod<-sum(keep_rel_pairs_ME$tot_cat_cod_new)
  sum_catch_hadd<-sum(keep_rel_pairs_ME$tot_cat_hadd_new)
  
  # compute medians
  med_cod <- median(keep_rel_pairs_ME$tot_cat_cod_new, na.rm = TRUE)
  med_hadd <- median(keep_rel_pairs_ME$tot_cat_hadd_new, na.rm = TRUE)
  
  # Define percentiles of interest
  percentiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  
  # Compute percentiles for cod and haddock
  cod_q <- quantile(keep_rel_pairs_ME$tot_cat_cod_new, probs = percentiles, na.rm = TRUE)
  hadd_q <- quantile(keep_rel_pairs_ME$tot_cat_hadd_new, probs = percentiles, na.rm = TRUE)
  
  # Loop over percentile *indices* instead of names
  results <- purrr::map_dfr(seq_along(percentiles), function(z) {
    cod_thr <- cod_q[[z]]
    hadd_thr <- hadd_q[[z]]
    
    tibble::tibble(
      percentile = percentiles[z] * 100,  # express as percent (e.g., 10, 25, 50...)
      cod_threshold = cod_thr,
      hadd_threshold = hadd_thr,
      both_above = sum(keep_rel_pairs_ME$tot_cat_cod_new > cod_thr &
                         keep_rel_pairs_ME$tot_cat_hadd_new > hadd_thr, na.rm = TRUE),
      both_below = sum(keep_rel_pairs_ME$tot_cat_cod_new <= cod_thr &
                         keep_rel_pairs_ME$tot_cat_hadd_new <= hadd_thr, na.rm = TRUE)
    )
  })
  
  # Reshape wide so there is one row
  results_wide <- results %>%
    tidyr::pivot_wider(
      names_from = percentile,
      values_from = c(cod_threshold, hadd_threshold, both_above, both_below),
      names_glue = "{.value}_{percentile}"
    )
  
  
  
  
  if(sum_keep_cod>0 & sum_keep_hadd>0){
    
    ktau_keep<- cor.test(keep_rel_pairs_ME$tot_keep_cod_new,
                         keep_rel_pairs_ME$tot_keep_hadd_new, method = c("kendall"))
    
    k_tau_keep_est<-ktau_keep[["estimate"]]
    k_tau_keep_p<- ktau_keep[["p.value"]]
  }
  
  if(sum_keep_cod==0 | sum_keep_hadd==0){
    
    k_tau_keep_est<-0
    k_tau_keep_p<- 1
  }
  
  
  if(sum_catch_cod>0 & sum_catch_hadd>0){
    
    ktau_catch<- cor.test(keep_rel_pairs_ME$tot_cod_catch_new,
                          keep_rel_pairs_ME$tot_hadd_catch_new, method = c("kendall"))
    
    k_tau_catch_est<-ktau_catch[["estimate"]]
    k_tau_catch_p<- ktau_catch[["p.value"]]
  }
  
  if(sum_catch_cod==0 | sum_catch_hadd==0){
    
    k_tau_catch_est<-0
    k_tau_catch_p<- 1
  }
  
  # Merge everything into one row of ktaus_annual
  ktaus_ME <- cbind(data.frame( k_tau_keep_est, k_tau_keep_p, k_tau_catch_est,k_tau_catch_p,med_cod,
                                med_hadd),results_wide)
  # ktaus_ME<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p, 
  #                                med_cod, med_hadd, both_above, both_below), names="TRUE")
  ktaus_ME$domain<-"ME"
  ktaus_ME$draw<-i
  ktaus_ME$month<-0
  
  #combine all the ktaus
  all_ktaus<-rbind.fill(ktaus_annual, ktaus_month_all, ktaus_NH, ktaus_MA, ktaus_ME, ktaus_pr, ktaus_fh)
  all_ktaus<-all_ktaus %>% 
    dplyr::mutate(year=y) %>% 
    dplyr::rename(domain1=domain)
  
  
  sims <- sims %>% 
    separate(period2, into = c("mode", "period", "area", "state"), sep = "_") %>%
    dplyr::mutate(month = as.numeric(month)) %>% 
    dplyr::mutate(draw=i) 
  
  sims<-sims %>% select(-period)
  
  sims$domain<-k
  
  assign(paste0("sims_new_", k), sims)
  
  all_ktaus$domain<-k
  assign(paste0("all_ktaus_new_", k), all_ktaus)
  
  keep_rel_pairs_annual$domain<-k
  assign(paste0("keep_rel_pairs_new_", k), keep_rel_pairs_annual)
  
  # rm(results, mean_trip_data, mean_trip_data1, mean_trip_data2, param_draws, trip_data, 
  #    cod_zero_catch, hadd_zero_catch, summed_catch_data, trip_data_draws, trip_data_hadd,
  #    catch_data1a, hadd_catch_data, catch_data1, keep_rel_pairs_month, 
  #    keep_rel_pairs_month_p)
  
}

#Fishery output
# Retrieve the results datasets as a list
dataset_names <- ls(pattern = "^sims_new_")

datasets <- mget(dataset_names)

# Combine datasets by stacking them
sims_all <- bind_rows(datasets)

# correlations output
# Retrieve the results datasets as a list
dataset_names <- ls(pattern = "^all_ktaus_new_")

# Retrieve the ktaus datasets as a list
datasets <- mget(dataset_names)

# Merge the datasets
ktaus_all <- bind_rows(datasets)

# keep_rel_pairs output
# Retrieve the results datasets as a list
dataset_names <- ls(pattern = "^keep_rel_pairs_new_")

# Retrieve the ktaus datasets as a list
datasets <- mget(dataset_names)

# Merge the datasets
#keep_rel_pairs <- bind_rows(datasets)