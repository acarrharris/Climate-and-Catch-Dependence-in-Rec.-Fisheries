#Climatology and projection bias correction
#Geret DePiper
#February 16, 2021

library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

#There are 3 steps to creating log-normal distributions for the decadal bottom 
#temperature projections. These were discussed with Vince Saba and Hubert duPontavice
#on February 14, 2023. The steps include 1) Using Hubert's dataset as derived 
#in this paper https://doi.org/10.1016/j.pocean.2022.102948 to create a climatology of 
#average monthly temperatures for the period 1991 - 2020. 2) Using the climatology to
#bias correct the predictions from CM2.6 as discussed in Vince's paper here 
#https://onlinelibrary.wiley.com/doi/pdf/10.1002/2015JC011346 3) Generating log-normal
# random distributions for all decades in the 80 prediction time horizon

# Set global directories - user edit here
project_dir<-"C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries"

process_data_dir <- file.path(project_dir,"data")

climatology_years <- c(1991:2023)

# drive_download(
#   as_id("https://docs.google.com/spreadsheets/d/1aaYblqkcCRF83tjLfM0R4ocM60cZpq4W/edit?usp=share_link&ouid=104786272712659294835&rtpof=true&sd=true"),
# path= "bottom_temp.xlsx", overwrite=TRUE)

bottom_temp <- readxl::read_excel(file.path(process_data_dir, "daily_btemp_GoM_by_area_1993_2026.xlsx")) %>% 
  mutate(month=as.numeric(month(date)),
         year=as.numeric(year(date)))

#Step_1. Create baseline climatology from years 1991 - 2020

climatology <- bottom_temp %>%
              filter(year %in% climatology_years) %>%
              group_by(month) %>%
            summarise(mean_climatology_inshore=mean((mean_tmp_inshore),na.rm=TRUE),
                      mean_climatology_offshore=mean((mean_tmp_offshore),na.rm=TRUE)) %>% ungroup()

write.csv(climatology,(file.path(process_data_dir, "Climatology_1991_2023_08-22-2026.csv")))


climatology_sd <- bottom_temp %>%
                    group_by(month) %>%
                    summarise(c_std_dev_inshore=sd(mean_tmp_inshore,na.rm=TRUE),
                              c_std_dev_offshore=sd(mean_tmp_inshore,na.rm=TRUE))%>% ungroup()


baseline <- bottom_temp %>%
            filter(year %in% climatology_years) %>%
            mutate(bt_tmp_inshore = mean_tmp_inshore,
              bt_tmp_offshore = mean_tmp_offshore) %>%
            select(date,year,month,day,bt_tmp_inshore,bt_tmp_offshore) %>%
            left_join(bottom_temp %>% filter(year %in% climatology_years) %>%
            select(year) %>% unique %>%
            mutate(y1 = order(year)))

#Step_2 bias correct variance from projections using baseline
# drive_download(
#   as_id("https://docs.google.com/spreadsheets/d/1kjU-W_0LWjrjVflmyp4j0tAzekVcbKJY/edit?usp=share_link&ouid=104786272712659294835&rtpof=true&sd=true"),
#   path= "bottom_temp_deltas.xlsx", overwrite=TRUE)

bottom_temp_deltas <- readxl::read_excel(file.path(process_data_dir, "mean_btemp_proj_deltas_GoM_by_area.xlsx")) %>%
                        mutate(year=as.numeric(year))

ggplot(bottom_temp_deltas, aes(x=year, y=mean_temp_inshore))+geom_line()+
  facet_wrap(vars(month))

ggplot(bottom_temp_deltas, aes(x=year, y=mean_temp_offshore))+geom_line()+
  facet_wrap(vars(month))


#Here we add the deltas for years 1-30 to the baseline values for the years
#1991 - 2020. We create a correction factor by the ratio of the 
#baseline standard deviation to the projection standard deviation.
#We then subtract the 30 year mean of the projected levels from 
#the projected observations, multiply that difference by the correction factor
#and add the 30 year mean of the projected levels back to the projected observation.
#We do all of this on the logged temperatures to create a log-normal distribution.
bottom_temp_levels <- bottom_temp_deltas  %>%
                    filter(year %in% c(1:30)) %>%
                    mutate(y1=year) %>%
                    select(-year) %>%
                    left_join(baseline, by=c("y1","month")) %>%
                    mutate(projection_inshore = bt_tmp_inshore+mean_temp_inshore,
                           projection_offshore = bt_tmp_offshore+mean_temp_offshore) %>%
                    group_by(month) %>%
                    mutate(sd_proj_inshore = sd(projection_inshore, na.rm=TRUE),
                           sd_proj_offshore = sd(projection_offshore, na.rm=TRUE),
                           mean_proj_inshore=mean(projection_inshore, na.rm=TRUE),
                           mean_proj_offshore=mean(projection_offshore, na.rm=TRUE)) %>%
                    ungroup() %>% 
                    left_join(climatology_sd,by=c("month")) %>%
                    mutate(sd_ratio_inshore = c_std_dev_inshore/sd_proj_inshore,
                           sd_ratio_offshore = c_std_dev_offshore/sd_proj_offshore,
                           new_projection_inshore=(projection_inshore-mean_proj_inshore)*sd_ratio_inshore+mean_proj_inshore,
                           new_projection_offshore=(projection_offshore-mean_proj_offshore)*sd_ratio_offshore+mean_proj_offshore) %>%
                    group_by(month) %>% 
                    mutate(new_avg_inshore=mean(new_projection_inshore),
                           new_avg_offshore=mean(new_projection_offshore)) %>%
                    ungroup()
#Subsetting the correction factors
Corrections <- bottom_temp_levels %>%
  select(month,sd_ratio_inshore,sd_ratio_offshore) %>% unique

Distributions <- bottom_temp_levels %>% 
        group_by(cut(y1,breaks=seq(1,30, by=10),right=F),month) %>%
        summarise(average_temp_inshore=mean(new_projection_inshore),
                  sd_temp_inshore=sd(new_projection_inshore),
                  average_temp_offshore=mean(new_projection_offshore),
                  sd_temp_offshore=sd(new_projection_offshore))%>% ungroup()
        
                    
bottom_temp_distributions <- bottom_temp_deltas  %>%
  filter(year %in% c(1:80)) %>%
  mutate(y1=year) %>%
  select(-year) %>%
  left_join(climatology, by=c("month")) %>%
  left_join(Corrections,by=c("month")) %>%
  mutate(projection_inshore = mean_climatology_inshore+mean_temp_inshore,
         projection_offshore = mean_climatology_offshore+mean_temp_offshore) %>%
    group_by(cut(y1,seq(1,80,by=10), right=F), month) %>%
    mutate(mean_proj_inshore=mean(projection_inshore, na.rm=TRUE),
    new_projection_inshore=(projection_inshore-mean_proj_inshore)*sd_ratio_inshore+mean_proj_inshore,
    mean_proj_offshore=mean(projection_offshore, na.rm=TRUE),
    new_projection_offshore=(projection_offshore-mean_proj_offshore)*sd_ratio_offshore+mean_proj_offshore) %>%
    summarise(average_temp_inshore= mean(new_projection_inshore),
              sd_temp_inshore = sd(new_projection_inshore, na.rm=TRUE),
              average_temp_offshore= mean(new_projection_offshore),
              sd_temp_offshore = sd(new_projection_offshore, na.rm=TRUE))%>% ungroup()

colnames(bottom_temp_distributions)[1] <- "Cut"

bottom_temp_distributions <- bottom_temp_distributions %>%
  left_join(bottom_temp_distributions %>% 
              ungroup %>%
              select(Cut) %>%
              unique %>%
              mutate(decade = order(Cut)))
    
  

ggplot(bottom_temp_distributions) + aes(x=decade,y=average_temp_inshore, group=month,colour=month) +
    geom_line()

ggplot(bottom_temp_distributions) + aes(x=decade,y=average_temp_offshore, group=month,colour=month) +
  geom_line()

#test to make sure means are the same, and only variances changed

bottom_temp_distributions_test <- bottom_temp_deltas  %>%
  filter(year %in% c(1:80)) %>%
  mutate(y1=year) %>%
  select(-year) %>%
  left_join(climatology, by=c("month")) %>%
  left_join(Corrections,by=c("month")) %>%
  mutate(projection_inshore = mean_climatology_inshore+mean_temp_inshore,
         projection_offshore = mean_climatology_offshore+mean_temp_offshore) %>%
  group_by(cut(y1,seq(1,80,by=10), right=F), month) %>%
  summarise(average_temp_test_inshore= mean(projection_inshore, na.rm=TRUE),
            sd_temp_test_inshore = sd(projection_inshore, na.rm=TRUE),
            average_temp_test_offshore= mean(projection_offshore, na.rm=TRUE),
            sd_temp_test_offshore = sd(projection_offshore, na.rm=TRUE)) %>% ungroup()

colnames(bottom_temp_distributions_test)[1] <- "Cut"

bottom_temp_distributions_test <- bottom_temp_distributions_test %>%
  left_join(bottom_temp_distributions) %>% 
  mutate(mean_difference_inshore = average_temp_inshore-average_temp_test_inshore,
         sd_differences_inshore = sd_temp_inshore - sd_temp_test_inshore,
         mean_difference_offshore = average_temp_offshore-average_temp_test_offshore,
         sd_differences_offshore = sd_temp_offshore - sd_temp_test_offshore)

max(bottom_temp_distributions_test$mean_difference_inshore)
max(bottom_temp_distributions_test$mean_difference_offshore)
max(bottom_temp_distributions_test$sd_differences_inshore)
max(bottom_temp_distributions_test$sd_differences_offshore)

ggplot(bottom_temp_distributions_test)+
  aes(y=sd_differences_inshore, group=as.factor(month), colour=as.factor(month))+
  geom_boxplot()

ggplot(bottom_temp_distributions_test)+
  aes(y=sd_differences_offshore, group=as.factor(month), colour=as.factor(month))+
  geom_boxplot()
#writing the results out
# bottom_temp_distributions$levels <- 

bottom_temp_distributions <- bottom_temp_distributions[,-1]

write.csv(bottom_temp_distributions,file.path(process_data_dir, "decadal_bottom_temp_distributions.csv"))
haven::write_dta(bottom_temp_distributions,file.path(process_data_dir, "decadal_bottom_temp_distributions.dta"))

