
input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"



# Values for results section of manuscript
# Pull in all the data and reformat for plotting 

set.seed(03211990)

output_summarized1_list<-list()
plot_data_list<-list()
cvtrip_diff_list<-list()
cv_pct_list<-list()
diff_list<-list()
closure_list<-list()

years<-c(2019, 2020, 2021)
for(yr in years){
  
  # output to compute for total welfare loss 
  #output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", yr,"_outliers_10-8-25_reformat_coast.rds"))  %>% # original output
  output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", yr,"_outliers_newCV_2-17-26_reformat_coast.rds"))  %>% #updated output
      
    dplyr::mutate(codreltrip=codrel/ntrips,
                  haddreltrip=haddrel/ntrips,
                  codrelchoice=codrel/n_choice_occasions,
                  haddrelchoice=haddrel/n_choice_occasions,
                  cod_hadd_keep=codkeep+haddkeep,
                  cod_hadd_cat=codcat+haddcat)

  output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
  output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"
  output_summarized1$base_year<-yr
  output_summarized1_list[[yr]]<-output_summarized1

  plot_data<-output_summarized1 %>%
    dplyr::filter(decade!=0)
  
  plot_data$base_year<-yr
  plot_data_list[[yr]]<-plot_data
  
  # output of closure of both fisheries
  #closure<-read_excel(paste0(output_data_cd,"model_output_y", yr, "_outliers_9-21-25_close cod and haddock.xlsx")) %>% 
  closure<-read_excel(paste0(output_data_cd,"model_output_y", yr, "_outliers_newCV_2-17-26_close cod and haddock.xlsx")) %>% 
    dplyr::rename(cv_closure=change_CS) %>% 
    dplyr::group_by(draw)  %>% 
    summarise(across(c(cv_closure), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
    dplyr::ungroup()  
  
  closure$base_year<-yr
  closure_list[[yr]]<-closure
  
  #output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", yr,"_outliers_10-8-25_reformat_coast.rds"))  %>%
  output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", yr,"_outliers_newCV_2-17-26_reformat_coast.rds"))  %>%
    dplyr::mutate(codreltrip=codrel/ntrips,
                  haddreltrip=haddrel/ntrips, 
                  codrelchoice=codrel/n_choice_occasions,
                  haddrelchoice=haddrel/n_choice_occasions, 
                  cod_hadd_keep=codkeep+haddkeep, 
                  cod_hadd_cat=codcat+haddcat)
  
  output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
  
  output_summarized2<-output_summarized1 %>% 
    dplyr::mutate(copula = ifelse(correlation == "independent", "independent", copula))
  
  output_summarized_corr<-output_summarized1 %>% 
    dplyr::filter(correlation=="corr") %>% 
    dplyr::filter(decade!=0) 
  
  output_summarized_ind<-output_summarized1 %>% 
    dplyr::filter(correlation=="independent" ) %>% 
    dplyr::filter(decade!=0) 
  
  # CV differences correlated versus independent
  cvtrip_diff_corr <- output_summarized_corr %>%
    rename_with(~ paste0(.x, "_corr"), c(cvtrip, cv_choice, cv, ntrips)) %>%
    dplyr::select("draw", "copula","decade","cvtrip_corr", "cv_choice_corr", "cv_corr", "ntrips_corr") %>%     dplyr::filter(decade!=0 )
  
  cvtrip_diff_ind <- output_summarized_ind %>%
    rename_with(~ paste0(.x, "_ind"), c(cvtrip, cv_choice, cv, ntrips)) %>%
    dplyr::select("draw", "copula","decade","cvtrip_ind", "cv_choice_ind", "cv_ind", "ntrips_ind") %>%     dplyr::filter(decade!=0 )
  
  cvtrip_diff<-cvtrip_diff_corr %>% 
    left_join(cvtrip_diff_ind, by=c("draw", "copula","decade" )) %>%
    mutate(diff_ntrips=ntrips_ind-ntrips_corr, 
           diff_cvtrip=cvtrip_ind-cvtrip_corr , 
           diff_cv_choice=cv_choice_ind-cv_choice_corr , 
           diff_cv=cv_ind-cv_corr)
  
  
  cvtrip_diff$base_year<-yr
  cvtrip_diff_list[[yr]]<-cvtrip_diff
  
  # merge data to closure data to get bias as percent total value
  cv_pct<-  output_summarized2 %>% 
    dplyr::filter(decade!=0) %>% 
    dplyr::select("draw", "copula","decade","cv") %>% 
    dplyr::left_join(closure, by="draw") %>% 
    dplyr::mutate(cv_pct_total_value=(cv/cv_closure)*100) %>% 
    dplyr::mutate(base_year=yr)
  
  cv_pct_list[[yr]]<-cv_pct
  

  # differences correlated versus independent
  diff_corr <- output_summarized_corr %>%
    rename_with(~ paste0(.x, "_corr"), c(codkeep, haddkeep, codcat, haddcat, cv,
                                         cod_hadd_keep, cod_hadd_cat, cod_tot_mort, hadd_tot_mort)) %>%
    dplyr::select("draw", "copula","decade","codkeep_corr", "haddkeep_corr", "cv_corr",
                  "codcat_corr", "haddcat_corr", "cod_hadd_keep_corr", "cod_hadd_cat_corr", 
                  "cod_tot_mort_corr", "hadd_tot_mort_corr") %>%   
    dplyr::filter(decade!=0 )
  
  diff_ind <- output_summarized_ind %>%
    rename_with(~ paste0(.x, "_ind"), c(codkeep, haddkeep, codcat, haddcat, cv,
                                        cod_hadd_keep, cod_hadd_cat, cod_tot_mort, hadd_tot_mort)) %>%
    dplyr::select("draw", "copula","decade","codkeep_ind", "haddkeep_ind", "cv_ind",
                  "codcat_ind", "haddcat_ind", "cod_hadd_keep_ind", "cod_hadd_cat_ind", 
                  "cod_tot_mort_ind", "hadd_tot_mort_ind") %>%   
    dplyr::filter(decade!=0 )
  
  diff<-diff_corr %>% 
    left_join(diff_ind, by=c("draw", "copula","decade" )) %>%
    mutate(
      # bbsolute difference
      diff_cv           = cv_ind - cv_corr,
      diff_codkeep      = codkeep_ind - codkeep_corr,
      diff_haddkeep     = haddkeep_ind - haddkeep_corr,
      diff_codcat       = codcat_ind - codcat_corr,
      diff_haddcat      = haddcat_ind - haddcat_corr,
      diff_cod_hadd_keep = cod_hadd_keep_ind - cod_hadd_keep_corr,
      diff_cod_hadd_cat = cod_hadd_cat_ind - cod_hadd_cat_corr,
      diff_cod_mort     = cod_tot_mort_ind - cod_tot_mort_corr,
      diff_hadd_mort    = hadd_tot_mort_ind - hadd_tot_mort_corr,
      
      # percentage difference: ((ind - corr) / corr) * 100
      pct_diff_codkeep   = ((codkeep_ind - codkeep_corr) / codkeep_corr) * 100,
      pct_diff_haddkeep  = ((haddkeep_ind - haddkeep_corr) / haddkeep_corr) * 100,
      pct_diff_codcat    = ((codcat_ind - codcat_corr) / codcat_corr) * 100,
      pct_diff_haddcat   = ((haddcat_ind - haddcat_corr) / haddcat_corr) * 100,
      pct_diff_cod_hadd_keep = ((cod_hadd_keep_ind - cod_hadd_keep_corr) / cod_hadd_keep_corr) * 100,
      pct_diff_cod_hadd_cat  = ((cod_hadd_cat_ind - cod_hadd_cat_corr) / cod_hadd_cat_corr) * 100,
      pct_diff_cod_mort  = ((cod_tot_mort_ind - cod_tot_mort_corr) / cod_tot_mort_corr) * 100,
      pct_diff_hadd_mort = ((hadd_tot_mort_ind - hadd_tot_mort_corr) / hadd_tot_mort_corr) * 100,
      
      base_year = yr)
  
  diff_list[[yr]]<-diff
  
}

closure_combined <- dplyr::bind_rows(closure_list)
diff_combined <- dplyr::bind_rows(diff_list)
plot_data_combined <- dplyr::bind_rows(plot_data_list)
cvtrip_diff_combined <- dplyr::bind_rows(cvtrip_diff_list)
cv_pct_combined <- dplyr::bind_rows(cv_pct_list)
output_summarized1_combined<-dplyr::bind_rows(output_summarized1_list)

cv_pct_combined_filtered<-cv_pct_combined %>% 
  dplyr::filter(copula!="independent") %>% 
  dplyr::select(copula, draw, decade, cv_closure, base_year)

cvtrip_diff_combined<-cvtrip_diff_combined %>% 
  dplyr::left_join(cv_pct_combined_filtered, by=c("copula", "draw", "decade", "base_year")) %>% 
  dplyr::mutate(diff_cv_total_cv=(diff_cv/cv_closure)*100, 
                cv_ind_total_cv=(cv_ind/cv_closure)*100, 
                cv_corr_total_cv=(cv_corr/cv_closure)*100) 

# ntrips base_years
ntrips<-output_summarized1_combined %>% 
  dplyr::filter(copula=="observed") %>% 
  dplyr::select(draw, base_year, ntrips) %>% 
  dplyr::rename(ntrips_base=ntrips)

# merge to differences in trips corr. versus ind.  
cvtrip_diff_combined<-cvtrip_diff_combined %>% 
  dplyr::left_join(ntrips, by=c("draw",  "base_year")) %>% 
  dplyr::mutate(diff_trips_base_trips=(diff_ntrips/ntrips_base)*100) %>% 
  dplyr::mutate(pct_diff_ntrips_corr=((diff_ntrips)/ntrips_corr)*100)

# number of independent draws must equal number of dependent draws
cv_pct_combined_ind<-cv_pct_combined %>% 
  dplyr::filter(copula=="independent") %>%
  dplyr::group_by(copula, draw, decade, base_year) %>% 
  dplyr::slice_sample(n = 1) %>%
  dplyr::ungroup()


cv_pct_combined<-cv_pct_combined %>% 
  dplyr::filter(copula!="independent") %>%
  #dplyr::bind_rows(cv_pct_combined_ind) %>% 
  dplyr::group_by(copula, draw, decade, base_year) %>% 
  ungroup()

cv_pct_combined %>% 
  count(copula, decade, base_year) %>% 
  print(n=144)


#### VALUES FOR MAIN TEXT ####

# "The annual median value of these losses across all copula specifications, 
#  decades, and baseline years (2019,2020, and 2021) is": 

summary_stats <- cv_pct_combined %>%
  summarise(
    Median_Value = median(cv, na.rm = TRUE),
    IQR_Value    = IQR(cv, na.rm = TRUE),
    Q1           = quantile(cv, 0.25, na.rm = TRUE),
    Q3           = quantile(cv, 0.75, na.rm = TRUE),
    .groups = 'drop' 
  )

#print(summary_stats)
stat1<-c("annual_med_loss", 
         round(((summary_stats$Median_Value)/1000000),3), 
         round(((summary_stats$Q1)/1000000),3), 
         round(((summary_stats$Q3)/1000000),3))
stat1

# "For reference,the total annual value of the cod and haddock recreational fisheries, computed as the
# median welfare loss from setting bag limits for both species to zero in the baseline year, is":  

summary_stats <- closure_combined %>%
  summarise(
    Median_Value = median(cv_closure, na.rm = TRUE),
    IQR_Value    = IQR(cv_closure),
    Q1           = quantile(cv_closure, 0.25, na.rm = TRUE),
    Q3           = quantile(cv_closure, 0.75, na.rm = TRUE),
    .groups = 'drop' 
  )

print(summary_stats)
stat2<-c("total_value", round(((summary_stats$Median_Value)/1000000),3), 
         round(((summary_stats$Q1)/1000000),3), 
         round(((summary_stats$Q3)/1000000),3))
stat2
# "Thus, median annual welfare losses from warming waters as a percent of the total value of the fishery is": 

summary_stats <- cv_pct_combined %>%
  summarise(
    Median_Value = median(cv_pct_total_value, na.rm = TRUE),
    IQR_Value    = IQR(cv_pct_total_value, na.rm = TRUE),
    Q1           = quantile(cv_pct_total_value, 0.25, na.rm = TRUE),
    Q3           = quantile(cv_pct_total_value, 0.75, na.rm = TRUE),
    .groups = 'drop' 
  )

#print(summary_stats)
stat3<-c("loss_pct_total_value", round(summary_stats$Median_Value, 1), round(summary_stats$Q1, 1), round(summary_stats$Q3, 1))
stat3

# "Ignoring dependence in unconditional catch leads to an underestimation of
# these loses on the order of X% of the total consumer welfare
# generated by the fishery":

summary_stats <- cvtrip_diff_combined %>%
  summarise(
    Median_Value = median(diff_cv_total_cv, na.rm = TRUE),
    IQR_Value    = IQR(diff_cv_total_cv, na.rm = TRUE),
    Q1           = quantile(diff_cv_total_cv, 0.25, na.rm = TRUE),
    Q3           = quantile(diff_cv_total_cv, 0.75, na.rm = TRUE),
    .groups = 'drop' 
  )

#print(summary_stats)
stat4<-c("underestimate_pct_total_value", round(summary_stats$Median_Value,1), round(summary_stats$Q1,1), round(summary_stats$Q3,1))
stat4
# "Bias due to the independence assumption results in a relatively 
# small difference in the annual number of trips taken, less than X% of total trips":


summary_stats <- cvtrip_diff_combined %>%
  summarise(
    Median_Value = median(pct_diff_ntrips_corr, na.rm = TRUE),
    IQR_Value    = IQR(pct_diff_ntrips_corr, na.rm = TRUE),
    Q1           = quantile(pct_diff_ntrips_corr, 0.25, na.rm = TRUE),
    Q3           = quantile(pct_diff_ntrips_corr, 0.75, na.rm = TRUE),
    .groups = 'drop' 
  )

#print(summary_stats)
stat5<-c("underestimate_pct_total_trips", round(summary_stats$Median_Value,2), round(summary_stats$Q1,2), round(summary_stats$Q3,2))
stat5
# "but annual haddock harvest is overestimated on the order of":

summary_stats <- diff_combined %>%
  summarise(
    Median_Value = median(pct_diff_haddkeep , na.rm = TRUE),
    IQR_Value    = IQR(pct_diff_haddkeep, na.rm = TRUE),
    Q1           = quantile(pct_diff_haddkeep, 0.25, na.rm = TRUE),
    Q3           = quantile(pct_diff_haddkeep, 0.75, na.rm = TRUE),
    .groups = 'drop' #
  )

#print(summary_stats)
stat6<-c("overestimate_pct_haddock_harv",round(summary_stats$Median_Value,2), round(summary_stats$Q1,2), round(summary_stats$Q3,2))
stat6

# "Assuming independence in unconditional catch leads to an underestimation of welfare losses. 
# By the eighth decade this bias is on the order of X\% of the value of the fishery, 
# when the baseline year is 2021.": 

summary_stats <- cvtrip_diff_combined %>%
  dplyr::group_by(decade, base_year) %>% 
  dplyr::filter(decade==8 & base_year==2021) %>% 
  summarise(
    Median_Value = median(diff_cv_total_cv, na.rm = TRUE),
    IQR_Value    = IQR(diff_cv_total_cv, na.rm = TRUE),
    Q1           = quantile(diff_cv_total_cv, 0.25, na.rm = TRUE),
    Q3           = quantile(diff_cv_total_cv, 0.75, na.rm = TRUE),
    .groups = 'drop' 
  ) 

stat7<-c("eigth_dec_underestimation", round(summary_stats$Median_Value,2), round(summary_stats$Q1,2), round(summary_stats$Q3,2))
stat7
# "For example, when utilizing 2019 as the baseline conditions, 
# anglers actually experience a welfare gain by the fifth decade":
summary_stats <- cv_pct_combined %>%
  group_by(base_year, decade) %>%
  dplyr::filter(decade==5 & base_year==2019) %>% 
  summarise(
    Median_Value = median(cv, na.rm = TRUE),
    IQR_Value    = IQR(cv, na.rm = TRUE),
    Q1           = quantile(cv, 0.25, na.rm = TRUE),
    Q3           = quantile(cv, 0.75, na.rm = TRUE),
    .groups = 'drop' 
  ) 

#print(summary_stats) %>% print(n=24)
stat8<-c("fifth_dec_welfare_base2019", round(((summary_stats$Median_Value)/1000000),3), 
         round(((summary_stats$Q1)/1000000),3), 
         round(((summary_stats$Q3)/1000000),3))
summary_stats

out <- rbind(stat1, stat2, stat3, stat4, stat5, stat6, stat7, stat8) |>
  as.data.frame(stringsAsFactors = FALSE)

names(out) <- c("stat", "Median_value", "Quartile1", "Quartile3")
out$Median_value <- as.numeric(out$Median_value)

out


