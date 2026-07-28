# Install packages and set globals 
args = commandArgs(trailingOnly=TRUE)
options(future.globals.maxSize= 1000000000)

packages <- c("tidyr",  "tidyverse", "reshape2","splitstackshape", "patchwork",
              "doBy","WriteXLS","Rcpp", "ggplot2", "rlist","fitdistrplus",
              "MASS",  "psych", "rgl","copula", "VineCopula","scales","univariateML",
              "logspline","readr","data.table","conflicted", "readxl", "writexl", 
              "furrr", "profvis", "future", "Hmisc", "tictoc","purrr" , "rlist",
              "rmdformats", "prettydoc", "hrbrthemes", "tint", "tufte", "rstatix", "ggpubr", 
              "future.apply", "DescTools", "listenv", "akima", "reshape2", "stringr", "ggh4x")

# Install only those not already installed
#installed <- packages %in% rownames(installed.packages())
#if (any(!installed)) {
 # install.packages(packages[!installed])
#}

lapply(packages, library, character.only = TRUE)
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
options(scipen = 999)


input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

set.seed(03211990)


# Bottom temps
historical_and_projections<- read_excel(paste0(output_data_cd,"bottom_temp_reformat1.xlsx"))  
projection_temps<-historical_and_projections %>% 
  dplyr::filter(is.na(year)) %>% 
  dplyr::mutate(decade = dplyr::recode(decade,"2091-3000" = "2091-2100"))

temp_plot <- ggplot(projection_temps, aes(month, mean_temp)) +
  geom_point(size = 1.2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 0.35) +
  facet_wrap(~decade, nrow = 1) +
  geom_hline(aes(yintercept = mean_decadal_tmp), color = "red", linewidth = 0.35) +
  labs(x = NULL, y = "Ocean bottom temperature (°C)") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 12, margin = margin(t = 7, b = 7)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

temp_plot

ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/projection_temps_July1.eps"), plot = temp_plot, width = 8, height = 5, units = "in")

####

k_tau_data_combined_coast_list<-list()
k_tau_data_combined_state_list<-list()
k_tau_data_combined_mode_list<-list()

years<-c(2019, 2020, 2021)

for(yr in years){
  
  #ktau_data<-read_excel(paste0(output_data_cd,"ktau_output_y", yr, "_outliers_10-8-25.xlsx")) #original output
  ktau_data<-read_excel(paste0(output_data_cd,"ktau_output_y", yr, "_outliers_newCV_7-13-26_ALLDRAWS.xlsx")) #updated output
  
  ktau_annual<-ktau_data %>% filter(month==0 & domain1=="all") 
  
  ktau_annual_long <- ktau_annual %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_unweighted, k_tau_catch_unweighted, k_tau_keep_est, k_tau_catch_est)
  
  #ktau_data_ind<-read_excel(paste0(output_data_cd,"ktau_output_y", yr,"_outliers_10-8-25_ind.xlsx")) #original output
  ktau_data_ind<-read_excel(paste0(output_data_cd,"ktau_output_y", yr,"_outliers_newCV_7-13-26_ALLDRAWS_ind.xlsx")) #updated output
  
  ktau_annual_ind<-ktau_data_ind %>% filter(month==0 & domain1=="all")
  
  ktau_annual_ind_long <- ktau_annual_ind %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_unweighted, k_tau_catch_unweighted, k_tau_keep_est, k_tau_catch_est) %>% 
    dplyr::mutate(copula2=copula) %>% 
    dplyr::mutate(copula="independent")
  
  k_tau_data_combined_coast<-ktau_annual_ind_long %>%
    plyr::rbind.fill(ktau_annual_long) 
  
  k_tau_data_combined_coast<- k_tau_data_combined_coast %>% 
    dplyr::filter(decade!=0) %>% 
    dplyr::filter(!is.na(decade))
  
  k_tau_data_combined_coast$copula <- factor(k_tau_data_combined_coast$copula,
                                             levels = c("independent", setdiff(unique(k_tau_data_combined_coast$copula), "independent")))
  k_tau_data_combined_coast$base_year<-yr
  
  k_tau_data_combined_coast_list[[yr]]<-k_tau_data_combined_coast
  
  
  # coastwide by state ktau data 
  #ktau_data<-read_excel(paste0(output_data_cd,"ktau_output_y", yr, "_outliers_10-8-25.xlsx")) #original output
  ktau_data<-read_excel(paste0(output_data_cd,"ktau_output_y", yr, "_outliers_newCV_7-13-26_ALLDRAWS.xlsx")) #updated output
  
  ktau_annual<-ktau_data %>% filter(month==0 & domain1 %in% c("ME", "NH", "MA")) %>% 
    dplyr::rename(state=domain1)
  
  ktau_annual_long <- ktau_annual %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_unweighted, k_tau_catch_unweighted, k_tau_keep_est, k_tau_catch_est)
  
  
  #ktau_data_ind<-read_excel(paste0(output_data_cd,"ktau_output_y", yr,"_outliers_10-8-25_ind.xlsx")) #original output
  ktau_data_ind<-read_excel(paste0(output_data_cd,"ktau_output_y", yr,"_outliers_newCV_7-13-26_ALLDRAWS_ind.xlsx")) #updated output
  
  ktau_annual_ind<-ktau_data_ind %>% filter(month==0 & domain1 %in% c("ME", "NH", "MA")) %>% 
    dplyr::rename(state=domain1) 
  
  ktau_annual_ind_long <- ktau_annual_ind %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_unweighted, k_tau_catch_unweighted, k_tau_keep_est, k_tau_catch_est, state) %>% 
    dplyr::mutate(copula="independent")
  
  k_tau_data_combined_state<-ktau_annual_ind_long %>% 
    plyr::rbind.fill(ktau_annual_long) 
  
  k_tau_data_combined_state<- k_tau_data_combined_state %>% 
    dplyr::filter(decade!=0) %>% 
    dplyr::filter(!is.na(decade))
  
  k_tau_data_combined_state$copula <- factor(k_tau_data_combined_state$copula,
                                             levels = c("independent", setdiff(unique(k_tau_data_combined_state$copula), "independent")))
  k_tau_data_combined_state$base_year<-yr
  
  k_tau_data_combined_state_list[[yr]]<-k_tau_data_combined_state
  
  # coastwide by mode ktau data 
  #ktau_data<-read_excel(paste0(output_data_cd,"ktau_output_y", yr,"_outliers_10-8-25.xlsx")) #original output
  ktau_data<-read_excel(paste0(output_data_cd,"ktau_output_y", yr,"_outliers_newCV_7-13-26_ALLDRAWS.xlsx"))  #updated output
  
  ktau_annual<-ktau_data %>% filter(month==0 & domain1 %in% c("pr", "fh")) %>% 
    dplyr::select(-k_tau_catch_p, -k_tau_keep_p) %>% 
    dplyr::rename(mode=domain1) 
  ktau_annual_long <- ktau_annual %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade,k_tau_keep_unweighted, k_tau_catch_unweighted, k_tau_keep_est, k_tau_catch_est, mode)
  
  
  #ktau_data_ind<-read_excel(paste0(output_data_cd,"ktau_output_y", yr, "_outliers_10-8-25_ind.xlsx")) #original output
  ktau_data_ind<-read_excel(paste0(output_data_cd,"ktau_output_y", yr, "_outliers_newCV_7-13-26_ALLDRAWS_ind.xlsx")) #updated output
  
  ktau_annual_ind<-ktau_data_ind %>% filter(month==0 & domain1 %in% c("pr", "fh")) %>% 
    dplyr::select(-k_tau_catch_p, -k_tau_keep_p) %>% 
    dplyr::rename(mode=domain1) 
  
  ktau_annual_ind_long <- ktau_annual_ind %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_unweighted, k_tau_catch_unweighted, k_tau_keep_est, k_tau_catch_est, mode) %>% 
    dplyr::mutate(copula="independent")
  
  k_tau_data_combined_mode<-ktau_annual_ind_long %>% 
    plyr::rbind.fill(ktau_annual_long) 
  
  k_tau_data_combined_mode<- k_tau_data_combined_mode %>% 
    dplyr::filter(decade!=0) %>% 
    dplyr::filter(!is.na(decade))
  
  k_tau_data_combined_mode$copula <- factor(k_tau_data_combined_mode$copula,
                                            levels = c("independent", 
                                                       setdiff(unique(k_tau_data_combined_mode$copula), "independent")))
  
  k_tau_data_combined_mode$base_year<-yr
  
  k_tau_data_combined_mode_list[[yr]]<-k_tau_data_combined_mode
  
 
  
}

k_tau_data_combined_coast <- dplyr::bind_rows(k_tau_data_combined_coast_list)
k_tau_data_combined_state <- dplyr::bind_rows(k_tau_data_combined_state_list)
k_tau_data_combined_mode <- dplyr::bind_rows(k_tau_data_combined_mode_list)

# k_tau_corr<-k_tau_data_combined_coast %>% 
#   dplyr::filter(correlation=="corr") %>% 
#   dplyr::select(draw, copula, decade, base_year) 
# 
# k_tau_ind<-k_tau_data_combined_coast %>% 
#   dplyr::filter(correlation=="ind") %>% 
#   dplyr::select(-copula, -correlation) %>% 
#   dplyr::rename(copula=copula2) %>% 
#   dplyr::select(draw, copula, decade, base_year) 
# 
# ktau_combine2<-k_tau_corr %>% 
#   dplyr::left_join(k_tau_ind, by=c("draw", "copula", "decade","base_year"))
# 
# ktau_combine2<-ktau_combine2 %>% 
#   dplyr::mutate(pct_diff_num_above10=((both_above_10-both_above_10_corr)/both_above_10_corr)*100, 
#                 pct_diff_num_below10=((both_below_10-both_below_10_corr)/both_below_10_corr)*100, 
#                 pct_diff_num_above25=((both_above_25-both_above_25_corr)/both_above_25_corr)*100, 
#                 pct_diff_num_below25=((both_below_25-both_below_25_corr)/both_below_25_corr)*100, 
#                 pct_diff_num_above50=((both_above_50-both_above_50_corr)/both_above_50_corr)*100, 
#                 pct_diff_num_below50=((both_below_50-both_below_50_corr)/both_below_50_corr)*100, 
#                 pct_diff_num_above75=((both_above_75-both_above_75_corr)/both_above_75_corr)*100, 
#                 pct_diff_num_below75=((both_below_75-both_below_75_corr)/both_below_75_corr)*100, 
#                 pct_diff_num_above90=((both_above_90-both_above_90_corr)/both_above_90_corr)*100, 
#                 pct_diff_num_below90=((both_below_90-both_below_90_corr)/both_below_90_corr)*100)


# Main results plots in the paper
output_list_y<-list()
k=1
yrz<- c(2019, 2020, 2021)

for (y in yrz){

projection_temps<-historical_and_projections %>% 
  dplyr::filter(is.na(year)) %>% 
  dplyr::mutate(decade = dplyr::recode(decade,"2091-3000" = "2091-2100"))

#output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_10-8-25_reformat_coast.rds"))  %>% #original output
output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_newCV_7-13-26_reformat_coast.rds"))  %>%  #updated output
  dplyr::mutate(codreltrip=codrel/ntrips,
                haddreltrip=haddrel/ntrips, 
                codrelchoice=codrel/n_choice_occasions,
                haddrelchoice=haddrel/n_choice_occasions, 
                cod_hadd_keep=codkeep+haddkeep, 
                cod_hadd_cat=codcat+haddcat)

output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"

output2<-output_summarized1 %>%
  dplyr::filter(!is.na(decade))  %>%
  dplyr::filter(decade!=0)  %>%
  dplyr::mutate(decade = dplyr::recode(decade,
                                       `1` = "2021-2030",
                                       `2` = "2031-2040",
                                       `3` = "2041-2050",
                                       `4` = "2051-2060",
                                       `5` = "2061-2070",
                                       `6` = "2071-2080",
                                       `7` = "2081-2090",
                                       `8` = "2091-2100"
  )) 

df_subset <- output2 %>%
  # keep only the independent/independent rows
  dplyr::filter(copula == "independent", correlation == "independent") %>%
  # stratified sample: 100 per decade
  dplyr::group_by(decade) %>%
  dplyr::slice_sample(n = 100) %>%
  dplyr::ungroup()

output2<-output2 %>% 
  dplyr::filter(copula!="independent") 

output2<-output2 %>% 
  plyr::rbind.fill(df_subset) %>% 
  dplyr::select(-year) %>% 
  dplyr::mutate(codkeep=codkeep/1000, haddkeep=haddkeep/1000, cv=cv/1000000,
                codcat=codcat/1000, haddcat=haddcat/1000,)

# Reorder factor levels so "independent" is first
output2$copula <- factor(output2$copula,
                         levels = c("independent", setdiff(unique(output2$copula), "independent")))

# Shared color scale: no title, single row legend
copula_colors <- scale_color_discrete(
  name = NULL,
  guide = guide_legend(nrow = 1)
)

output_list_y[[k]]<-output2
k=k+1
w<-.5
sz=.3
osz=.5
# 1. Temperatures with decade strip labels
p1 <- ggplot(projection_temps, aes(month, mean_temp)) +
  geom_point(size = .9) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=w, linewidth=sz) +
  facet_wrap(~decade, nrow = 1) +
  geom_hline(aes(yintercept = mean_decadal_tmp), color = "red", size = .2) +
  labs(x = NULL, y = "Ocean bottom\ntemperature (°C)") +
  copula_colors +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 12, margin = margin(t = 5, b = 5)),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# 2. Cod harvest
p2 <- ggplot(output2, aes(x = factor(copula), y = codkeep, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  labs(x = NULL, y = "Cod harvest\n('000s)") +
  copula_colors +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text    = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# 3. Haddock harvest
p3 <- ggplot(output2, aes(x = factor(copula),y = haddkeep, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  labs(x = NULL, y = "Haddock harvest\n('000s)") +
  copula_colors +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text    = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  
  )


# Check if 0 is between the minimum lower bound and the maximum upper bound
zero_line_layer <- if (min(output2$cv) < 0 && max(output2$cv) > 0) {
  # Add the line if 0 is within the range. Using a light grey and dashed line.
  geom_hline(yintercept = 0, color = "grey40", size = 0.2)
} else {
  # Otherwise, return NULL, which ggplot will ignore.
  NULL
}

# 4. Compensating variation
p4 <- ggplot(output2, aes(x = factor(copula), y = cv, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  zero_line_layer +
  labs(x=NULL,  y = "Compensating\nvariation ($M)") +
  copula_colors +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text    = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )+
  scale_y_continuous(breaks = seq(
    floor(min(output2$cv, na.rm = TRUE)), 
    ceiling(max(output2$cv, na.rm = TRUE)), 
    by = 1   # tick every 1 unit
  )) 


# Combine plots with shared legend
final_plot <- (p1 / p2 / p3 / p4) +
  plot_layout(heights = c(1, 1, 1, 1), guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.key = element_blank()
  )

final_plot
#ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/havest_cv_plot", y,"_updated_2-17-26_.jpg"), plot = final_plot, width = 12, height = 8, units = "in")
#ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/havest_cv_plot", y,"_updated_7-13-26_.eps"), plot = final_plot, width = 12, height = 8, units = "in")
ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/havest_cv_plot", y,"_updated_7-24-26_.jpg"), plot = final_plot, width = 12, height = 8, units = "in")


#plot 2 
# 1. Cod catch
p1 <- ggplot(output2, aes(x = factor(copula), y = codcattrip, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  labs(x = NULL, y = " \nCod\ncatch-per-trip ") +
  copula_colors +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 12, margin = margin(t = 5, b = 5)),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )+
  facet_wrap(~decade, nrow = 1)
  

# 2. Haddock catch
p2 <- ggplot(output2, aes(x = factor(copula), y = haddcattrip, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  labs(x = NULL, y = " \nHaddock\ncatch-per-trip ") +
  copula_colors +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text    = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# 2. k-tau catch 
ktau_yr<-k_tau_data_combined_coast %>% 
  dplyr::filter(base_year==y)


#y_min <- floor(min(ktau_yr$k_tau_catch_unweighted, ktau_yr$k_tau_keep_unweighted, na.rm = TRUE)*10)/ 10
#y_max <- ceiling(max(ktau_yr$k_tau_catch_unweighted, ktau_yr$k_tau_keep_unweighted, na.rm = TRUE)*10)/ 10

y_min <- floor(min(ktau_yr$k_tau_catch_est, ktau_yr$k_tau_keep_est, na.rm = TRUE)*10)/ 10
y_max <- ceiling(max(ktau_yr$k_tau_catch_est, ktau_yr$k_tau_keep_est, na.rm = TRUE)*10)/ 10

# 2. Define the common breaks sequence
y_breaks <- seq(y_min, y_max, by = 0.1)

# Check if 0 is between the minimum lower bound and the maximum upper bound
zero_line_layer <- if (min(ktau_yr$k_tau_catch_est, ktau_yr$k_tau_keep_est) < 0 && max(ktau_yr$k_tau_catch_est, ktau_yr$k_tau_keep_est) > 0) {
  # Add the line if 0 is within the range. Using a light grey and dashed line.
  geom_hline(yintercept = 0, color = "grey40", size = 0.2)
} else {
  # Otherwise, return NULL, which ggplot will ignore.
  NULL
}

p3 <- ggplot(ktau_yr, aes(x = factor(copula), y = k_tau_catch_est, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  labs(x = NULL, y = "Correlation in\ncatch-per-trip\n(Kendall's tau)") +
  copula_colors +
  # Add the horizontal line at y = 0
  zero_line_layer +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text    = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )+
# Apply common limits and breaks
  scale_y_continuous(
    limits = c(y_min, y_max),
   breaks = y_breaks)

# 2. k-tau harvest 
p4 <- ggplot(ktau_yr, aes(x = factor(copula), y = k_tau_keep_est, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  labs(x = NULL, y = "Correlation in\n harvest-per-trip\n(Kendall's tau)") +
  copula_colors +
  # Add the horizontal line at y = 0
  zero_line_layer +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text    = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )+
  # Apply common limits and breaks
  scale_y_continuous(
    limits = c(y_min, y_max),
    breaks = y_breaks)
final_plot <- (p1 / p2 / p3 / p4) +
  plot_layout(heights = c(1, 1, 1, 1), guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.key = element_blank()
  )

final_plot

#ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/ktau_plot", y,".jpg"), plot = final_plot, width = 12, height = 8, units = "in")
#ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/ktau_plot_", y,"_updated_2-17-26_.jpg"), plot = final_plot, width = 12, height = 8, units = "in")
#ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/ktau_plot_", y,"_updated_7-13-26_.eps"), plot = final_plot, width = 12, height = 8, units = "in")
ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/ktau_plot_", y,"_updated_7-24-26_.jpg"), plot = final_plot, width = 12, height = 8, units = "in")

}

output_allyears <- dplyr::bind_rows(output_list_y)


#######################
#######################
#PLOTS by mode
# output2_list<-list()
# k=1
# yrz<- c(2019, 2020, 2021)
# for (y in yrz){
#   projection_temps<-historical_and_projections %>% 
#     dplyr::filter(is.na(year))
#   
#   
#   #output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_10-8-25_reformat_mode.rds"))  %>%
#   output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_newCV_7-13-26_reformat_mode.rds"))  %>%
#     
#     dplyr::mutate(codreltrip=codrel/ntrips,
#                   haddreltrip=haddrel/ntrips, 
#                   codrelchoice=codrel/n_choice_occasions,
#                   haddrelchoice=haddrel/n_choice_occasions, 
#                   cod_hadd_keep=codkeep+haddkeep, 
#                   cod_hadd_cat=codcat+haddcat)
#   
#   output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
#   output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"
#   
#   output2<-output_summarized1 %>%
#     dplyr::filter(!is.na(decade))  %>%
#     dplyr::filter(decade!=0)  %>%
#     dplyr::mutate(decade = dplyr::recode(decade,
#                                          `1` = "2021-2030",
#                                          `2` = "2031-2040",
#                                          `3` = "2041-2050",
#                                          `4` = "2051-2060",
#                                          `5` = "2061-2070",
#                                          `6` = "2071-2080",
#                                          `7` = "2081-2090",
#                                          `8` = "2091-2100"
#     )) 
#   
#   df_subset <- output2 %>%
#     # keep only the independent/independent rows
#     dplyr::filter(copula == "independent", correlation == "independent") %>%
#     # stratified sample: 100 per decade
#     dplyr::group_by(decade) %>%
#     dplyr::slice_sample(n = 100) %>%
#     dplyr::ungroup()
#   
#   output2<-output2 %>% 
#     dplyr::filter(copula!="independent") 
#   
#   output2<-output2 %>% 
#     plyr::rbind.fill(df_subset) %>% 
#     dplyr::select(-year) %>% 
#     dplyr::mutate(codkeep=codkeep/1000, haddkeep=haddkeep/1000, cv=cv/1000000,
#                   codcat=codcat/1000, haddcat=haddcat/1000,base_year=y)
#   
#   
#   library(ggplot2)
#   library(patchwork)
#   
#   # Reorder factor levels so "independent" is first
#   output2$copula <- factor(output2$copula,
#                            levels = c("independent", setdiff(unique(output2$copula), "independent")))
#   
#   output2_list[[k]]<-output2
#   k=k+1
#   
#   # Shared color scale: no title, single row legend
#   copula_colors <- scale_color_discrete(
#     name = NULL,
#     guide = guide_legend(nrow = 1)
#   )
# }
# output2_allyrs <- dplyr::bind_rows(output2_list)
# output2_allyrs <- output2_allyrs %>%
#   dplyr::mutate(
#     decade = dplyr::recode(
#       decade,
#       "2021-2030" = "2020s",
#       "2031-2040" = "2030s",
#       "2041-2050" = "2040s",
#       "2051-2060" = "2050s",
#       "2061-2070" = "2060s",
#       "2071-2080" = "2070s",
#       "2081-2090" = "2080s",
#       "2091-2100" = "2090s"
#     )
#   )
# # 1. Define the Custom Labeller Function
# # This function takes the original variable name (labels) and returns the new desired labels.
# mode_labeller <- function(variable, value) {
#   # 'value' contains the unique levels of the faceting variable (e.g., "fh", "pr")
#   
#   # Create a named vector mapping the old values to the new display names
#   new_labels <- c(
#     "fh" = "Party & Charter", 
#     "pr" = "Private",
#     # Add any other mode values you have and their labels here (e.g., "sh" = "Shore")
#     "sh" = "Shore" 
#   )
#   
#   # Look up the new labels based on the old values
#   return(new_labels[value])
# }
# 
# 
# 
# # Check if 0 is between the minimum lower bound and the maximum upper bound
# zero_line_layer <- if (min(output2$cv) < 0 && max(output2$cv) > 0) {
#   # Add the line if 0 is within the range. Using a light grey and dashed line.
#   geom_hline(yintercept = 0, color = "grey40", size = 0.2)
# } else {
#   # Otherwise, return NULL, which ggplot will ignore.
#   NULL
# }
# 
# 
# #new 
# # -------------------------
# # Shared plot styling
# # -------------------------
# 
# w   <- 0.5
# sz  <- 0.3
# osz <- 0.5
# 
# copula_colors <- scale_color_discrete(
#   name = NULL,
#   guide = guide_legend(nrow = 1)
# )
# 
# clean_theme <- theme_bw(base_size = 12) +
#   theme(
#     strip.text = element_text(size = 12, margin = margin(t = 5, b = 5)),
#     axis.title.y = element_text(size = 12),
#     axis.text.y = element_text(size = 12),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     legend.position = "none"
#   )
# 
# theme_centered_title <- theme(
#   plot.title = element_text(
#     size = 12,
#     hjust = 0.5,
#     margin = margin(b = 8)
#   )
# )
# 
# 
# # -------------------------
# # CV plots by mode
# # -------------------------
# 
# output2_all2019 <- output2_allyrs %>%
#   dplyr::filter(base_year == 2019) %>%
#   dplyr::mutate(base_year = "Baseline Year = 2019")
# 
# output2_all2020 <- output2_allyrs %>%
#   dplyr::filter(base_year == 2020) %>%
#   dplyr::mutate(base_year = "Baseline Year = 2020")
# 
# output2_all2021 <- output2_allyrs %>%
#   dplyr::filter(base_year == 2021) %>%
#   dplyr::mutate(base_year = "Baseline Year = 2021")
# 
# global_min_y <- floor(min(output2_allyrs$cv, na.rm = TRUE))
# global_max_y <- ceiling(max(output2_allyrs$cv, na.rm = TRUE))
# 
# zero_line_layer <- if (
#   min(output2_allyrs$cv, na.rm = TRUE) < 0 &&
#   max(output2_allyrs$cv, na.rm = TRUE) > 0
# ) {
#   geom_hline(yintercept = 0, color = "grey40", linewidth = 0.2)
# } else {
#   NULL
# }
# 
# 
# make_cv_mode_plot <- function(dat, title_text, show_x = FALSE, show_y = FALSE, show_strip = TRUE) {
#   
#   strip_layer <- if (show_strip) {
#     ggh4x::facet_nested(
#       cols = vars(mode, decade),
#       labeller = labeller(
#         mode = as_labeller(c(
#           "fh" = "Party & Charter",
#           "pr" = "Private",
#           "sh" = "Shore"
#         ))
#       ),
#       nest_line = element_line(linewidth = 0.3)
#     )
#   } else {
#     ggh4x::facet_nested(
#       cols = vars(mode, decade),
#       labeller = labeller(
#         mode = as_labeller(c(
#           "fh" = "Party & Charter",
#           "pr" = "Private",
#           "sh" = "Shore"
#         ))
#       ),
#       nest_line = element_blank()
#     )
#   }
#   
#   ggplot(dat, aes(x = factor(copula), y = cv, color = factor(copula))) +
#     geom_boxplot(
#       width = 0.65,
#       linewidth = sz,
#       outlier.size = osz
#     ) +
#     zero_line_layer +
#     copula_colors +
#     strip_layer +
#     scale_y_continuous(
#       breaks = seq(global_min_y, global_max_y, by = 2),
#       limits = c(global_min_y, global_max_y)
#     ) +
#     labs(
#       x = NULL,
#       y = if (show_y) "Compensating variation ($M)" else NULL,
#       title = title_text
#     ) +
#     clean_theme +
#     theme_centered_title +
#     theme(
#       axis.text.x = element_blank(),
#       axis.ticks.x = element_blank(),
#       strip.text.x = if (show_strip) element_text(size = 9, angle = 0) else element_blank(),
#       strip.text.x.top = if (show_strip) element_text(size = 10) else element_blank(),
#       panel.spacing.x = unit(0.25, "lines"),
#       legend.position = "none"
#     )
# }
# p1 <- make_cv_mode_plot(
#   output2_all2019,
#   "Baseline Year = 2019",
#   show_x = FALSE,
#   show_y = FALSE,
#   show_strip = TRUE
# )
# 
# p2 <- make_cv_mode_plot(
#   output2_all2020,
#   "Baseline Year = 2020",
#   show_x = FALSE,
#   show_y = TRUE,
#   show_strip = TRUE
# )
# 
# p3 <- make_cv_mode_plot(
#   output2_all2021,
#   "Baseline Year = 2021",
#   show_x = FALSE,
#   show_y = FALSE,
#   show_strip = TRUE
# )
# 
# 
# final_plot_mode <- (p1 / p2 / p3) +
#   plot_layout(heights = c(1, 1, 1), guides = "collect") &
#   theme(
#     legend.position = "bottom",
#     legend.direction = "horizontal",
#     legend.text = element_text(size = 16),
#     legend.title = element_text(size = 12),
#     legend.key = element_blank()
#   )
# 
# final_plot_mode
# 
# 
# 
# #ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/cv_by_mode_allyears_updated_2-17-26.jpg"), plot = final_plot, width = 12, height = 8, units = "in")
# #ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/cv_by_mode_allyears_updated_7-1-26.jpg"), plot = final_plot, width = 12, height = 8, units = "in")
# ggsave(
#   paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/", "cv_by_mode_allyears_updated_7-13-26.eps"),
#   plot = final_plot_mode,
#   width = 12,
#   height = 8,
#   units = "in"
# )
# #######################
# #######################
# 
# #######################
# #######################
# #PLOTS by state
# 
# # This function takes the original variable name (labels) and returns the new desired labels.
# 
# output2_list<-list()
# k=1
# yrz<- c(2019, 2020, 2021)
# for (y in yrz){
#   projection_temps<-historical_and_projections %>% 
#     dplyr::filter(is.na(year))
#   
#   
#   #output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_10-8-25_reformat_state.rds"))  %>%
#   output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_newCV_7-13-26_reformat_state.rds"))  %>%
#     
#     dplyr::mutate(codreltrip=codrel/ntrips,
#                   haddreltrip=haddrel/ntrips, 
#                   codrelchoice=codrel/n_choice_occasions,
#                   haddrelchoice=haddrel/n_choice_occasions, 
#                   cod_hadd_keep=codkeep+haddkeep, 
#                   cod_hadd_cat=codcat+haddcat)
#   
#   output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
#   output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"
#   
#   output2<-output_summarized1 %>%
#     dplyr::filter(!is.na(decade))  %>%
#     dplyr::filter(decade!=0)  %>%
#     dplyr::mutate(decade = dplyr::recode(decade,
#                                          `1` = "2021-2030",
#                                          `2` = "2031-2040",
#                                          `3` = "2041-2050",
#                                          `4` = "2051-2060",
#                                          `5` = "2061-2070",
#                                          `6` = "2071-2080",
#                                          `7` = "2081-2090",
#                                          `8` = "2091-2100"
#     )) %>%
#     dplyr::mutate(state = dplyr::recode(state,
#                                         `33` = "New Hampshire",
#                                         `25` = "Massachusetts",
#                                         `23` = "Maine")) 
#   
#   df_subset <- output2 %>%
#     # keep only the independent/independent rows
#     dplyr::filter(copula == "independent", correlation == "independent") %>%
#     # stratified sample: 100 per decade
#     dplyr::group_by(decade, state, correlation, copula) %>%
#     dplyr::slice_sample(n = 100) %>%
#     dplyr::ungroup()
#   
# 
#   output2<-output2 %>% 
#     dplyr::filter(copula!="independent") 
#   
#   output2<-output2 %>% 
#     plyr::rbind.fill(df_subset) %>% 
#     dplyr::select(-year) %>% 
#     dplyr::mutate(codkeep=codkeep/1000, haddkeep=haddkeep/1000, cv=cv/1000000,
#                   codcat=codcat/1000, haddcat=haddcat/1000,base_year=y)
#   
#   
#   library(ggplot2)
#   library(patchwork)
#   
#   # Reorder factor levels so "independent" is first
#   output2$copula <- factor(output2$copula,
#                            levels = c("independent", setdiff(unique(output2$copula), "independent")))
#   
#   output2_list[[k]]<-output2
#   k=k+1
#   
#   # Shared color scale: no title, single row legend
#   copula_colors <- scale_color_discrete(
#     name = NULL,
#     guide = guide_legend(nrow = 1)
#   )
# }
# output2_allyrs <- dplyr::bind_rows(output2_list)
# output2_allyrs <- output2_allyrs %>%
#   dplyr::mutate(
#     decade = dplyr::recode(
#       decade,
#       "2021-2030" = "2020s",
#       "2031-2040" = "2030s",
#       "2041-2050" = "2040s",
#       "2051-2060" = "2050s",
#       "2061-2070" = "2060s",
#       "2071-2080" = "2070s",
#       "2081-2090" = "2080s",
#       "2091-2100" = "2090s"
#     )
#   )
# # Check if 0 is between the minimum lower bound and the maximum upper bound
# zero_line_layer <- if (min(output2$cv) < 0 && max(output2$cv) > 0) {
#   # Add the line if 0 is within the range. Using a light grey and dashed line.
#   geom_hline(yintercept = 0, color = "grey40", size = 0.2)
# } else {
#   # Otherwise, return NULL, which ggplot will ignore.
#   NULL
# }
# 
# # 4. Compensating variation
# output2_all2019<-output2_allyrs %>% dplyr::filter(base_year==2019) %>% 
#   dplyr::mutate(base_year = "Baseline Year = 2019")
# 
# 
# output2_all2020<-output2_allyrs %>% dplyr::filter(base_year==2020) %>% 
#   dplyr::mutate(base_year = "Baseline Year = 2020")
# 
# 
# output2_all2021<-output2_allyrs %>% dplyr::filter(base_year==2021) %>% 
#   dplyr::mutate(base_year = "Baseline Year = 2021")
# 
# # Define the common theme settings for centering the plot title
# theme_centered_title <- theme(
#   plot.title = element_text(
#     size = 11, 
#     hjust = 0.5, # Centers the plot title
#     margin = margin(b = 10) 
#   )
# )
# 
# global_min_y <- floor(min(output2_allyrs$cv, na.rm = TRUE))
# global_max_y <- ceiling(max(output2_allyrs$cv, na.rm = TRUE))
# 
# #new
# 
# make_cv_state_plot <- function(dat, title_text, show_x = FALSE, show_y = FALSE, show_strip = TRUE) {
#   
#   strip_layer <- if (show_strip) {
#     ggh4x::facet_nested(
#       cols = vars(state, decade),
#       nest_line = element_line(linewidth = 0.3)
#     )
#   } else {
#     ggh4x::facet_nested(
#       cols = vars(state, decade),
#        nest_line = element_blank()
#     )
#   }
#   
#   ggplot(dat, aes(x = factor(copula), y = cv, color = factor(copula))) +
#     geom_boxplot(
#       width = 0.65,
#       linewidth = sz,
#       outlier.size = osz
#     ) +
#     zero_line_layer +
#     copula_colors +
#     strip_layer +
#     scale_y_continuous(
#       breaks = seq(global_min_y, global_max_y, by = 2),
#       limits = c(global_min_y, global_max_y)
#     ) +
#     labs(
#       x = NULL,
#       y = if (show_y) "Compensating variation ($M)" else NULL,
#       title = title_text
#     ) +
#     clean_theme +
#     theme_centered_title +
#     theme(
#       axis.text.x = element_blank(),
#       axis.ticks.x = element_blank(),
#       strip.text.x = if (show_strip) element_text(size = 9, angle = 0) else element_blank(),
#       strip.text.x.top = if (show_strip) element_text(size = 10) else element_blank(),
#       panel.spacing.x = unit(0.25, "lines"),
#       legend.position = "none"
#     )
# }
# p1 <- make_cv_state_plot(
#   output2_all2019,
#   "Baseline Year = 2019",
#   show_x = FALSE,
#   show_y = FALSE,
#   show_strip = TRUE
# )
# 
# p2 <- make_cv_state_plot(
#   output2_all2020,
#   "Baseline Year = 2020",
#   show_x = FALSE,
#   show_y = TRUE,
#   show_strip = TRUE
# )
# 
# p3 <- make_cv_state_plot(
#   output2_all2021,
#   "Baseline Year = 2021",
#   show_x = FALSE,
#   show_y = FALSE,
#   show_strip = TRUE
# )
# 
# 
# final_plot_state <- (p1 / p2 / p3) +
#   plot_layout(heights = c(1, 1, 1), guides = "collect") &
#   theme(
#     legend.position = "bottom",
#     legend.direction = "horizontal",
#     legend.text = element_text(size = 16),
#     legend.title = element_text(size = 12),
#     legend.key = element_blank()
#   )
# 
# final_plot_state
# 
# 
# #ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/cv_by_state_allyears_updated_2-17-26.jpg"), plot = final_plot, width = 12, height = 8, units = "in")
# ggsave(
#   paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/cv_by_state_allyears_updated_7-13-26.eps"),
#   plot = final_plot_state,
#   width = 12,
#   height = 8,
#   units = "in"
#)
#######################
#######################
