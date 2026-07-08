

p_star_cod <- p_star_cod_variable
p_star_hadd<-p_star_hadd_variable

######################################
##   Begin simulating trip outcomes ##
######################################
cod_hadd_catch_data <- catch_data_all %>%
  dplyr::right_join(regs_check, by="period2") %>% 
  dplyr::select(-dtrip, -period) 



  catch_data1<-cod_hadd_catch_data %>% 
    dplyr::select(mode, month, period2, state, tripid, area, catch_draw, tot_cat_cod, tot_cat_hadd) %>% 
    dplyr::rename(tot_cod_catch=tot_cat_cod, tot_hadd_catch=tot_cat_hadd)
  
  # Cod
  # subset trips with zero catch, as no size draws are required
  cod_zero_catch <- catch_data1 %>% 
    dplyr::filter(tot_cod_catch == 0) %>% 
    dplyr::select(-c("mode", "area", "state"))
  
  #remove trips with zero cod catch
  catch_data1a <- filter(catch_data1, tot_cod_catch > 0) 
  catch_data1a<-as.data.table(catch_data1a)
  
  #expand the catch so that each row represents a fish
  row_inds <- seq_len(nrow(catch_data1a))
  
  catch_data1a<-  catch_data1a %>%  
    slice(rep(row_inds,tot_cod_catch))   
  
  regs<-as.data.table(regs)
  
  setkey(catch_data1a, period2)
  setkey(regs, period2)
  
  # Perform the equivalent of a left_join
  catch_data1a <- regs[catch_data1a, nomatch = 0]
  
  # Add a uniform random number column
  catch_data1a[, uniform := runif(.N)]
  
  # Calculate posskeep
  catch_data1a[, posskeep := ifelse(uniform >= p_star_cod, 1, 0)]
  
  # Group by and calculate cumulative sum of posskeep
  catch_data1a[, csum_keep := cumsum(posskeep), by = .(period2, tripid, catch_draw)]
  
  # Calculate keep_adj
  catch_data1a[, keep_adj := fifelse(cod_bag > 0, 
                                     fifelse(csum_keep <= cod_bag & posskeep == 1, 1, 0), 
                                     0)]
  
  # Calculate keep_tot and release
  catch_data1a[, `:=`(
    keep_tot = keep_adj,
    release = fifelse(keep_adj == 0, 1, 0)
  )]
  
  # Select and rename columns
  catch_data1a <- catch_data1a[, .(tripid, keep = keep_tot, release, period2, catch_draw, month)]
  
  summed_catch_data <- catch_data1a %>%
    as.data.table() %>%
    .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month" ), .SDcols = c("keep", "release")]
  
  trip_data <- summed_catch_data %>%
    rename(tot_keep_cod = keep, 
           tot_rel_cod = release) %>% 
    dplyr::bind_rows(cod_zero_catch) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))
  
  
  # Haddock
  
  # subset trips with zero catch, as no size draws are required
  hadd_zero_catch <- catch_data1 %>% 
    dplyr::filter(tot_hadd_catch == 0) %>% 
    dplyr::select(-c("mode", "area", "state"))
  
  #remove trips with zero haddock catch
  hadd_catch_data <- filter(catch_data1, tot_hadd_catch > 0) 
  hadd_catch_data<-as.data.table(hadd_catch_data)
  
  #expand the catch data so that each row represents a fish
  row_inds <- seq_len(nrow(hadd_catch_data))
  
  hadd_catch_data<- hadd_catch_data %>%  
    slice(rep(row_inds,tot_hadd_catch))
  
  rownames(hadd_catch_data) <- NULL
  
  # Assuming hadd_catch_data and regs are already data.tables
  setkey(hadd_catch_data, period2)
  setkey(regs, period2)
  
  # Perform the equivalent of a left_join
  hadd_catch_data <- regs[hadd_catch_data, nomatch = 0]
  
  # Add a uniform random number column
  hadd_catch_data[, uniform := runif(.N)]
  
  # Calculate posskeep
  hadd_catch_data[, posskeep := ifelse(uniform >= p_star_hadd, 1, 0)]
  
  # Group by and calculate cumulative sum of posskeep
  hadd_catch_data[, csum_keep := cumsum(posskeep), by = .(tripid, period2, catch_draw)]
  
  # Calculate keep_adj
  hadd_catch_data[, keep_adj := fifelse(
    hadd_bag > 0, 
    fifelse(csum_keep <= hadd_bag & posskeep == 1, 1, 0), 
    0
  )]
  
  hadd_catch_data <- hadd_catch_data %>%
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(release = ifelse(keep_adj==0,1,0))  
  
  hadd_catch_data<- subset(hadd_catch_data, select=c(tripid, keep_adj, release, period2, catch_draw,  month)) %>% 
    rename(keep = keep_adj)
  
  summed_catch_data <- hadd_catch_data %>%
    .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month"), .SDcols = c("keep", "release")]
  
  summed_catch_data <- summed_catch_data %>%
    rename(tot_keep_hadd = keep, 
           tot_rel_hadd = release)
  
  trip_data_hadd<-summed_catch_data %>% 
    dplyr::bind_rows(hadd_zero_catch) %>%  #add the zero catch trips 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))
  
  trip_data<-trip_data %>% 
    dplyr::left_join(trip_data_hadd, by=c("period2", "catch_draw", "tripid",  "month"))
  
  param_draws<- readRDS(paste0(input_data_cd, "costs_data_",yr,"draw", i,"_ind.rds")) %>% 
    select(-month)
  
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
                  
                  #  utility (base year)
                  v0 = beta_sqrt_cod_keep*sqrt(tot_keep_cod_base) +
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
  

  calibration_data_table<-readRDS(paste0(input_data_cd, "calibration_data_",yr,"draw", i,".rds")) %>% 
    select(period2, n_choice_occasions)
  
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
  
  sims_all <- results %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, sum),  by = c("period2", "month"), .SDcols = list_names]
  
  
  names(sims_all)[names(sims_all) == "prob0"] = "ntrips_base"
  names(sims_all)[names(sims_all) == "probA"] = "ntrips_alt"
  
  sims_all <- sims_all %>% 
    separate(period2, into = c("mode", "period", "area", "state"), sep = "_") %>%
    dplyr::mutate(month = as.numeric(month)) %>% 
    dplyr::mutate(draw=i) 
  
  sims_all<-sims_all %>% select(-period)
  



