# OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 
# adjust globals - user edit here 
input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/data/" 
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/code/09_fishery_simulation/"
proj_catch_cd <- input_data_cd
results_plot_code_cd <- input_code_cd



#### VALUES FOR MAIN TEXT ####
coast_all <- dplyr::bind_rows(lapply(years, function(yr) {
  readRDS(file.path(input_data_cd, paste0("model_output_y", yr, "_reformat_coast.rds")))
}))


"The annual median value of these losses across the five copula specifications 
in Table \ref{table:Copulas}}, decades, and post-Covid baseline years (2022-2024) is"

welfare_stats_all <- coast_all %>%
  dplyr::filter(correlation == "correlated" & base_year>=2022) %>%
  dplyr::mutate(
    annual_welfare_m = cv / 1e6
  ) %>%
  dplyr::select(base_year, draw, copula, decade,
                annual_welfare_m) %>%
  tidyr::pivot_longer(cols = c(annual_welfare_m),
                      names_to = "statistic", values_to = "value") %>%
  dplyr::filter(
    statistic == "annual_welfare_m" ) %>%
  dplyr::group_by(statistic) %>%
  dplyr::summarise(
    n = sum(is.finite(value)),
    median = stats::median(value, na.rm = TRUE),
    p2_5 = stats::quantile(value, 0.025, na.rm = TRUE),
    q1 = stats::quantile(value, 0.25, na.rm = TRUE),
    q3 = stats::quantile(value, 0.75, na.rm = TRUE),
    p97_5 = stats::quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

print(welfare_stats_all, n = Inf)

#"The total value of the cod and haddock recreational fishery in 2024, 
#computed as the median welfare loss from setting bag limits for both 
#species to zero in this year, is":  

# "Expressing the warming-induced loss as a share of this total value, 
# the median annual welfare loss from warming waters is"

welfare_stats2024 <- coast_all %>%
  dplyr::filter(correlation == "correlated") %>%
  dplyr::mutate(
    annual_welfare_m = cv / 1e6,
    total_value_m = cv_closure / 1e6,
    welfare_pct = cv_pct_total_value
  ) %>%
  dplyr::select(base_year, draw, copula, decade,
                annual_welfare_m, total_value_m, welfare_pct) %>%
  tidyr::pivot_longer(cols = c(annual_welfare_m, total_value_m, welfare_pct),
                      names_to = "statistic", values_to = "value") %>%
  dplyr::filter(
    statistic == "annual_welfare_m" |
      (base_year == 2024 & statistic %in% c("total_value_m", "welfare_pct"))
  ) %>%
  dplyr::group_by(statistic) %>%
  dplyr::summarise(
    n = sum(is.finite(value)),
    median = stats::median(value, na.rm = TRUE),
    p2_5 = stats::quantile(value, 0.025, na.rm = TRUE),
    q1 = stats::quantile(value, 0.25, na.rm = TRUE),
    q3 = stats::quantile(value, 0.75, na.rm = TRUE),
    p97_5 = stats::quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

print(welfare_stats2024, n = Inf)


# Percentage of paired draws with positive and negative independence differences.
diff_sign_summary <- coast_all %>%
  dplyr::filter(correlation == "correlated") %>%
  dplyr::select(
    base_year, draw, copula, decade,
    diff_codcat_ind, diff_haddcat_ind,
    diff_codkeep_ind, diff_haddkeep_ind
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("diff_"),
    names_to = "outcome",
    values_to = "difference"
  ) %>%
  dplyr::group_by(base_year, copula, decade, outcome) %>%
  dplyr::summarise(
    n_draws = sum(is.finite(difference)),
    pct_positive = 100 * mean(difference[is.finite(difference)] > 0),
    pct_negative = 100 * mean(difference[is.finite(difference)] < 0),
    pct_zero = 100 * mean(difference[is.finite(difference)] == 0),
    median_difference = stats::median(difference, na.rm = TRUE),
    n_positive = sum(difference > 0, na.rm = TRUE),
    n_negative = sum(difference < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(base_year, copula, decade, outcome)

diff_sign_summary

writexl::write_xlsx(diff_sign_summary,
  path = file.path(input_data_cd, "diff_sign_table.xlsx")
)


# "Across the eight decades, ignoring cross-species dependence results in sizeable discrepancies 
# in the catch of both cod (Figure 5; median: −11.6%, 2.5th – 97.5th percentiles: −29.2%, −1.4%;
# cod catch-per-trip was lower for the independent case than for the dependent specification 
# in 99% of all simulation runs) and haddock (median: 2%, 2.5th – 97.5th percentiles: 0.6%, 4.2%; 
# haddock catch-per-trip was higher for the independent case in over 99% of all runs), 
# and in a relatively small discrepancy in the annual number of trips taken (less than 1%)."

independence_2024 <- coast_all %>%
  dplyr::filter(base_year == 2024, correlation == "correlated") %>%
  dplyr::select(
    base_year, draw, copula, decade,
    pct_diff_codcat_base, pct_diff_haddcat_base,
    pct_diff_codkeep_base, pct_diff_haddkeep_base,
    pct_diff_ntrips_base,
    diff_codcat_ind, diff_haddcat_ind,
    diff_codkeep_ind, diff_haddkeep_ind, diff_cv_ind
  )

# Percentage differences in catch, harvest, and trips
independence_pct_summary <- independence_2024 %>%
  dplyr::select(draw, copula, decade, dplyr::starts_with("pct_diff_")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("pct_diff_"), names_to = "outcome", values_to = "value") %>%
  dplyr::group_by(outcome) %>%
  dplyr::summarise(
    n = sum(is.finite(value)),
    median = stats::median(value, na.rm = TRUE),
    p2_5 = stats::quantile(value, 0.025, na.rm = TRUE),
    q1 = stats::quantile(value, 0.25, na.rm = TRUE),
    q3 = stats::quantile(value, 0.75, na.rm = TRUE),
    p97_5 = stats::quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

print(independence_pct_summary, n = Inf)

# Frequency of positive and negative independence differences
independence_sign_summary <- independence_2024 %>%
  dplyr::select(draw, copula, decade, dplyr::starts_with("diff_")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("diff_"), names_to = "outcome", values_to = "difference") %>%
  dplyr::filter(is.finite(difference)) %>%
  dplyr::group_by(outcome) %>%
  dplyr::summarise(
    n = dplyr::n(),
    pct_positive = 100 * mean(difference > 0),
    pct_negative = 100 * mean(difference < 0),
    pct_zero = 100 * mean(difference == 0),
    .groups = "drop"
  )

print(independence_sign_summary, n = Inf)


independence_all <- coast_all %>%
  dplyr::filter(correlation == "correlated") %>%
  dplyr::select(
    base_year, draw, copula, decade,
    pct_diff_codcat_base, pct_diff_haddcat_base,
    pct_diff_codkeep_base, pct_diff_haddkeep_base,
    pct_diff_ntrips_base,
    diff_codcat_ind, diff_haddcat_ind,
    diff_codkeep_ind, diff_haddkeep_ind, diff_cv_ind
  )

"Interestingly, independence yields lower cod catch than the dependent specifications 
in every decade and baseline year, with the cod gap widening as bottom temperatures rise."

independence_all_summary <- independence_all %>%
  dplyr::select(draw, copula, base_year, decade, dplyr::starts_with("pct_diff_")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("pct_diff_"), names_to = "outcome", values_to = "value") %>%
  dplyr::group_by(outcome, decade, base_year) %>%
  dplyr::summarise(
    n = sum(is.finite(value)),
    median = stats::median(value, na.rm = TRUE),
    p2_5 = stats::quantile(value, 0.025, na.rm = TRUE),
    q1 = stats::quantile(value, 0.25, na.rm = TRUE),
    q3 = stats::quantile(value, 0.75, na.rm = TRUE),
    p97_5 = stats::quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

print(independence_all_summary, n = Inf)


# "For example, when utilizing 2019 as the baseline conditions, anglers 
# actually experience welfare gains every decade"

welfare2019 <- coast_all %>%
  dplyr::filter(base_year == 2019, correlation == "correlated") %>%
  group_by(decade) %>% 
  dplyr::summarise(
    n = sum(is.finite(cv)),
    median = stats::median(cv, na.rm = TRUE) / 1e6,
    p2_5 = stats::quantile(cv, 0.025, na.rm = TRUE) / 1e6,
    q1 = stats::quantile(cv, 0.25, na.rm = TRUE) / 1e6,
    q3 = stats::quantile(cv, 0.75, na.rm = TRUE) / 1e6,
    p97_5 = stats::quantile(cv, 0.975, na.rm = TRUE) / 1e6
  )

print(welfare2019)


# "Decomposing the results, most of the underestimation in cod catch and overestimation 
# in haddock catch from assuming independence originates from private vessels
# (contributing 98% and 90% of cod catch and haddock catch discrepancy across draws, 
# decades, and copulas) and trips taken in Massachusetts (contributing 63% and 60% 
# of cod catch and haddock catch discrepancy across draws, decades, and copulas)."


summarize_bias_contributions <- function(level, year = 2024) {
  
  x <- readRDS(file.path(input_data_cd, paste0("model_output_y", year, "_reformat_", level, ".rds")))
  
  x %>%
    dplyr::filter(correlation == "correlated", decade %in% 1:8) %>%
    dplyr::select(dplyr::all_of(level), draw, copula, decade,
                  diff_codcat_ind, diff_haddcat_ind) %>%
    tidyr::pivot_longer(
      cols = c(diff_codcat_ind, diff_haddcat_ind),
      names_to = "species",
      values_to = "difference"
    ) %>%
    dplyr::mutate(
      species = dplyr::recode(species,
                              diff_codcat_ind = "Cod",
                              diff_haddcat_ind = "Haddock")
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(level, "species")))) %>%
    dplyr::summarise(
      total_difference = sum(difference, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(species) %>%
    dplyr::mutate(
      total_all_strata = sum(total_difference),
      pct_contribution = dplyr::if_else(
        abs(total_all_strata) > 1e-10,
        100 * total_difference / total_all_strata,
        NA_real_
      )
    ) %>%
    dplyr::ungroup()
}

bias_by_mode <- summarize_bias_contributions("mode", 2024)
bias_by_state <- summarize_bias_contributions("state", 2024)

print(bias_by_mode, n = Inf)
print(bias_by_state, n = Inf)


