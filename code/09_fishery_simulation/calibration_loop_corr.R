
# OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 
# adjust globals - user edit here 
input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/data/" 
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/code/09_fishery_simulation/"
proj_catch_cd <- input_data_cd
results_plot_code_cd <- input_code_cd

set.seed(seed)

p_star_cod <- p_star_cod_variable
p_star_hadd<-p_star_hadd_variable


######################################
##   Begin simulating trip outcomes ##
######################################

# Cod 
catch_data <- catch_data_all_split %>%
    dplyr::right_join(regs_check, by="period2") %>% 
    dplyr::select(-dtrip) 

catch_data1<- catch_data  %>% 
  dplyr::select(catch_draw, mode,month, area, state, period2, tripid, tot_cat_cod, tot_cat_hadd) %>% 
  dplyr::rename(tot_cod_catch=tot_cat_cod, tot_hadd_catch=tot_cat_hadd)

cod_hadd_catch_data <- catch_data1

rm(catch_data)


# subset trips with zero catch, as no size draws are required
cod_zero_catch <- catch_data1 %>% 
  dplyr::filter(tot_cod_catch == 0) %>% 
  dplyr::select(-c("mode", "area", "state"))

#remove trips with zero summer flounder catch
catch_data1 <- filter(catch_data1, tot_cod_catch > 0) 
catch_data1<-as.data.table(catch_data1)

#expand the sf_catch_data so that each row represents a fish
row_inds <- seq_len(nrow(catch_data1))

catch_data1<-  catch_data1 %>%  
  slice(rep(row_inds,tot_cod_catch))   

# Assign catch as keeps and releases
regs<-as.data.table(regs)

catch_data_cod <- copy(catch_data1) # Create a copy of catch_data1
setkey(catch_data_cod, period2)
setkey(regs, period2)

catch_data_cod <- regs[catch_data_cod, nomatch = 0]

catch_data_cod[, uniform := runif(.N)]

# Calculate possible keeps
catch_data_cod[, posskeep := ifelse(uniform >= p_star_cod, 1, 0)]

# Group by and calculate cumulative sum of posskeep
catch_data_cod[, csum_keep := cumsum(posskeep), by = .(period2, tripid, catch_draw)]

# Calculate keep_adj
catch_data_cod[, keep_adj := fifelse(cod_bag > 0, 
                                     fifelse(csum_keep <= cod_bag & posskeep == 1, 1, 0),0)]

# Calculate keep_tot and release
catch_data_cod[, `:=`(
  keep_tot = keep_adj,
  release = fifelse(keep_adj == 0, 1, 0)
)]

catch_data_cod <- catch_data_cod[, .(tripid, keep = keep_tot, release, period2, catch_draw, month)]

# Aggregate a catch within a draw
summed_catch_data <- catch_data_cod %>%
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
hadd_zero_catch <- cod_hadd_catch_data %>% 
  dplyr::filter(tot_hadd_catch == 0) %>% 
  dplyr::select(-c("mode", "area", "state"))

#remove trips with zero Haddock catch
hadd_catch_data <- filter(cod_hadd_catch_data, tot_hadd_catch > 0) 
hadd_catch_data<-as.data.table(hadd_catch_data)

#expand the sf_catch_data so that each row represents a fish
row_inds <- seq_len(nrow(hadd_catch_data))

hadd_catch_data<- hadd_catch_data %>%  
  slice(rep(row_inds,tot_hadd_catch))

rownames(hadd_catch_data) <- NULL

setkey(hadd_catch_data, period2)
setkey(regs, period2)

hadd_catch_data <- regs[hadd_catch_data, nomatch = 0]

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

rm(trip_data_hadd, catch_data_cod, cod_hadd_catch_data, cod_zero_catch, hadd_zero_catch, summed_catch_data,hadd_catch_data)

# mode-specific trip costs from Stata output adjusted for inflation
inflation_adjustments <- tibble::tribble(
  ~year, ~inflation_factor,
  2019, 0.87,
  2020, 0.88,
  2021, 0.92,
  2022, 1.00,
  2023, 1.04,
  2024, 1.06
)

trip_cost_pool <- haven::read_dta(
  file.path(input_data_cd, "trip_cost_distribution.dta")
) |>
  dplyr::select(trip_mode, cost_2022, cost_weight)

if (anyNA(trip_cost_pool$cost_2022) || anyNA(trip_cost_pool$cost_weight)) {
  stop("Missing trip costs or survey weights in trip-cost distribution.")
}

if (any(trip_cost_pool$cost_weight <= 0)) {
  stop("Trip-cost survey weights must be positive.")
}


current_draw <- i

# Angler preference parameters
preference_pool <- haven::read_dta(
  file.path(input_data_cd, "preference_param_draws.dta")
) |>
  dplyr::filter(draw == current_draw) |>
  dplyr::transmute(
    id_export,
    beta_sqrt_cod_keep,
    beta_sqrt_cod_release,
    beta_sqrt_hadd_keep,
    beta_sqrt_hadd_release,
    beta_sqrt_cod_hadd_keep,
    beta_cost,
    beta_opt_out,
    beta_opt_out_age ,
    beta_opt_out_likely ,
    beta_opt_out_prefer 
  )

if (nrow(preference_pool) == 0L) {
  stop("No preference parameters found for draw ", current_draw)
}
if (anyDuplicated(preference_pool$id_export)) {
  stop("Duplicate id_export values within preference draw ", current_draw)
}

param_draws <- trip_data |>
  dplyr::distinct(period2, tripid) |>
  dplyr::mutate(
    trip_mode = substr(period2, 1, 2),
    year = y,
    id = dplyr::row_number()
  )

n <- nrow(param_draws)

sampled_parameters <- preference_pool[
  sample.int(nrow(preference_pool), n, replace = TRUE),
  ,
  drop = FALSE
]

param_draws <- dplyr::bind_cols(param_draws, sampled_parameters)


inflation_factor_y <- inflation_adjustments |>
  dplyr::filter(year == y) |>
  dplyr::pull(inflation_factor)

if (length(inflation_factor_y) != 1L) {
  stop("No unique inflation adjustment found for year ", y)
}

# Draw one trip cost per choice occasion from the empirical,
# survey-weighted 2022 cost distribution.
param_draws$cost <- NA_real_

for (m in unique(param_draws$trip_mode)) {
  
  idx <- which(param_draws$trip_mode == m)
  
  cost_pool_m <- trip_cost_pool |>
    dplyr::filter(trip_mode == m)
  
  if (nrow(cost_pool_m) == 0L) {
    stop("No trip-cost observations available for mode ", m)
  }
  
  param_draws$cost[idx] <- sample(
    cost_pool_m$cost_2022,
    size = length(idx),
    replace = TRUE,
    prob = cost_pool_m$cost_weight
  ) * inflation_factor_y
}

param_draws <- param_draws |>
  dplyr::select(-trip_mode)


# Sample angler ages from the NOAA expenditure survey-based age distribution.
age_distn <- utils::read.csv(
  file.path(input_data_cd, "age_distribution_by_state.csv")
) |>
  dplyr::filter(state == "MA") |>
  dplyr::select(-state)

if ("age_distn" %in% names(age_distn)) {
  age_distn <- dplyr::rename(age_distn, age = age_distn)
} else if (!"age" %in% names(age_distn)) {
  stop(
    "Cannot identify the age column. Columns found: ",
    paste(names(age_distn), collapse = ", ")
  )
}

sampled_ages <- age_distn[
  sample.int(nrow(age_distn), n, replace = TRUE),
  ,
  drop = FALSE
]

param_draws <- dplyr::bind_cols(param_draws, sampled_ages)

# Sample the other demographics from the choice experiment respondents
angler_dems <- utils::read.csv(
  file.path(input_data_cd, "angler_CE_demographics.csv")
)

if (nrow(angler_dems) == 0L) {
  stop("The angler CE demographics file is empty.")
}

sampled_dems <- angler_dems[
  sample.int(nrow(angler_dems), n, replace = TRUE),
  ,
  drop = FALSE
]

# Prevent source-file ID columns from conflicting with the occasion ID.
sampled_dems <- sampled_dems |>
  dplyr::select(-dplyr::any_of(c("masterid", "id_export")))

param_draws <- dplyr::bind_cols(param_draws, sampled_dems)

# All 30 catch draws for an occasion receive the same parameters and demographics.
trip_data <- trip_data |>
  dplyr::left_join(
    param_draws,
    by = c("period2", "tripid"),
    relationship = "many-to-one"
  )

if (anyNA(trip_data$beta_cost)) {
  stop("Some choice occasions did not receive preference parameters.")
}

trip_data <-trip_data %>%
  dplyr::arrange(period2, tripid, catch_draw)

trip_data<-  trip_data%>% 
  dplyr::mutate(vA=
                  beta_sqrt_cod_keep*sqrt(trip_data$tot_keep_cod) +
                  beta_sqrt_cod_release*sqrt(trip_data$tot_rel_cod) +  
                  beta_sqrt_hadd_keep*sqrt(trip_data$tot_keep_hadd) +
                  beta_sqrt_hadd_release*sqrt(trip_data$tot_rel_hadd) + 
                  beta_sqrt_cod_hadd_keep*(sqrt(trip_data$tot_keep_cod)*sqrt(trip_data$tot_keep_hadd)) +
                  beta_cost*trip_data$cost) %>% 
  dplyr::select(-id)


costs_new = subset(trip_data, select=c(tripid, period2, cost, catch_draw, tot_keep_cod, tot_rel_cod,
                                       tot_keep_hadd, tot_rel_hadd, beta_cost, beta_opt_out, beta_sqrt_cod_keep, 
                                       beta_sqrt_cod_release, beta_sqrt_cod_hadd_keep, 
                                       beta_sqrt_hadd_keep, beta_sqrt_hadd_release,beta_opt_out_age, 
                                       beta_opt_out_likely, beta_opt_out_prefer, 
                                       likely_to_fish, fish_pref_more, age, month))

names(costs_new)[names(costs_new) == "tot_keep_cod"] = "tot_keep_cod_base"
names(costs_new)[names(costs_new) == "tot_rel_cod"] = "tot_rel_cod_base"
names(costs_new)[names(costs_new) == "tot_keep_hadd"] = "tot_keep_hadd_base"
names(costs_new)[names(costs_new) == "tot_rel_hadd"] = "tot_rel_hadd_base"
names(costs_new)[names(costs_new) == "cost"] = "cost_base"

saveRDS(costs_new, file = paste0(input_data_cd, "costs_data_", y,"draw", i, ".rds"))

trip_data <- trip_data %>%
  dplyr::mutate(period = as.numeric(as.factor(period2)))

# calculate probability of each alternative per choice occasion
mean_trip_data <- trip_data %>% data.table::data.table() %>% 
  .[, group_index := .GRP, by = .(period2, catch_draw, tripid)]

# expand the data to create two alternatives (fishin and non-fishing)
mean_trip_data <- mean_trip_data %>%
  dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
  tidyr::uncount(n_alt) %>%
  dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                opt_out = ifelse(alt == 2, 1, 0))

# calculate the expected utility 
setDT(mean_trip_data)

# calculate opt-out utility (vA) 
mean_trip_data[alt == 2, "vA" := .(
  beta_opt_out * opt_out +
    beta_opt_out_age * (age * opt_out) +
    beta_opt_out_likely * (likely_to_fish * opt_out) +
    beta_opt_out_prefer * (fish_pref_more * opt_out)
)]


mean_trip_data <- mean_trip_data %>% 
  dplyr::select(-beta_cost,-beta_opt_out, -beta_opt_out_age, 
                -beta_opt_out_likely, -beta_opt_out_prefer, -beta_sqrt_cod_hadd_keep, 
                -beta_sqrt_cod_keep, -beta_sqrt_cod_release, -beta_sqrt_hadd_keep, 
                -beta_sqrt_hadd_release, -likely_to_fish, -fish_pref_more,-opt_out, -age,
                -group_index,-catch_draw, -cost, -period  )
all_vars<-c()
all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid", "alt", "month")]
all_vars

# average outcomes across draws
mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
  .[,lapply(.SD, mean), by = c("period2","tripid", "alt", "month"), .SDcols = all_vars]

mean_trip_data[, `:=`(exp_vA = exp(vA))]

# Group by group_index and calculate probabilities and log-sums
mean_trip_data[, `:=`(
  probA = exp_vA / sum(exp_vA)), by = c("tripid", "period2", "month") ]


mean_trip_data<- subset(mean_trip_data, alt==1) %>% 
  dplyr::mutate(tot_cat_cod=tot_keep_cod+tot_rel_cod, 
                tot_cat_hadd=tot_keep_hadd+tot_rel_hadd) %>% 
  dplyr::select(-alt )

# multiply the  trip probability (probA) by each catch variable to get probability-weighted catch
list_names <- c("tot_keep_cod","tot_keep_hadd", "tot_rel_cod", "tot_rel_hadd", "tot_cat_cod", "tot_cat_hadd")

mean_trip_data <- mean_trip_data %>%
  as.data.table() %>%
  .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
  .[]


# Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
# mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents 
# a certain number of choice occasions. Sum of trip alternative probabilities will approximate 
# directed-trip totals.

dtrips <- directed_trips_p %>%
  dplyr::select(dtrip, period2)

mean_trip_data <- mean_trip_data %>%
  dplyr::left_join(dtrips, by = "period2")


results <- mean_trip_data %>%
  dplyr::group_by(period2) %>%
  dplyr::mutate(
    mean_prob = mean(probA),
    sims = round(dtrip / mean_prob),
    n_simulated_occasions = dplyr::n(),
    expand = sims / n_simulated_occasions
  ) %>%
  dplyr::ungroup()

weighted_vars <- c(
  "tot_cat_cod", "tot_keep_cod", "tot_rel_cod",
  "tot_cat_hadd", "tot_keep_hadd", "tot_rel_hadd",
  "probA"
)

results <- results %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(weighted_vars),
      ~ .x * expand
    )
  )

aggregate_trip_data <- results %>%
  data.table::as.data.table() %>%
  .[
    ,
    c(
      lapply(.SD, sum),
      list(n_choice_occasions = sum(expand))
    ),
    by = "period2",
    .SDcols = weighted_vars
  ]

data.table::setnames(
  aggregate_trip_data,
  old = "probA",
  new = "estimated_trips"
)

saveRDS(aggregate_trip_data, file = paste0(input_data_cd, "calibration_data_", y,"draw", i, ".rds")) 


###Compare calibration model output with MRIP 

#cod
#print("model cod harvest")
sum(aggregate_trip_data$tot_keep_cod)
#print("MRIP cod harvest")
sum(MRIP_data$cod_harvest)
#print("percent diff cod harvest")
cod_harvest_perc_diff<-((sum(aggregate_trip_data$tot_keep_cod)-sum(MRIP_data$cod_harvest))/sum(MRIP_data$cod_harvest))*100
cod_harvest_perc_diff
cod_harvest_diff=abs(sum(MRIP_data$cod_harvest)-sum(aggregate_trip_data$tot_keep_cod))

sum(aggregate_trip_data$tot_cat_cod)
sum(MRIP_data$cod_tot_cat)
cod_tot_cat_perc_diff<-((sum(aggregate_trip_data$tot_cat_cod)-sum(MRIP_data$cod_tot_cat))/sum(MRIP_data$cod_tot_cat))*100
cod_tot_cat_perc_diff




##haddock
#print("model hadd harvest")
sum(aggregate_trip_data$tot_keep_hadd)
#print("mrip hadd harvest")
sum(MRIP_data$hadd_harvest)
#print("percent diff hadd harvest")
hadd_harvest_perc_diff<-((sum(aggregate_trip_data$tot_keep_hadd)-sum(MRIP_data$hadd_harvest))/sum(MRIP_data$hadd_harvest))*100
hadd_harvest_perc_diff
hadd_harvest_diff=abs(sum(MRIP_data$hadd_harvest)-sum(aggregate_trip_data$tot_keep_hadd))


sum(aggregate_trip_data$tot_cat_hadd)
sum(MRIP_data$hadd_tot_cat)
hadd_tot_cat_perc_diff<-((sum(aggregate_trip_data$tot_cat_hadd)-sum(MRIP_data$hadd_tot_cat))/sum(MRIP_data$hadd_tot_cat))*100
hadd_tot_cat_perc_diff


rm(catch_data1, mean_trip_data, param_draws, trip_data, results)

