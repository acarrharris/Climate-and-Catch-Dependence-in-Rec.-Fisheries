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

cd <-"[enter data directory]" 

climatology_years <- c(1991:2021)


bottom_temp <- readxl::read_excel(paste0(cd, "hist_daily_bt_tmps_agg_GoM.xlsx")) %>% 
  mutate(month=as.numeric(month(date)),
         year=as.numeric(year(date)),
         bt_tmp=mean_tmp)

#Step_1. Create baseline climatology from years 1991 - 2020

climatology <- bottom_temp %>%
              filter(year %in% climatology_years) %>%
              group_by(month) %>%
            summarise(mean_climatology=mean((bt_tmp),na.rm=TRUE)) %>% ungroup()

write.csv(climatology,"Climatology_1991_2020_12-02-2024.csv")


climatology_sd <- bottom_temp %>%
                    group_by(month) %>%
                    summarise(c_std_dev=sd(bt_tmp,na.rm=TRUE))%>% ungroup()


baseline <- bottom_temp %>%
            filter(year %in% climatology_years) %>%
            left_join(bottom_temp %>% filter(year %in% climatology_years) %>%
            select(year) %>% unique %>%
            mutate(y1 = order(year)))

#Step_2 bias correct variance from projections using baseline


bottom_temp_deltas <- readxl::read_excel(paste0(cd, "mean_bottom_temp_deltas_GOM_convex_hull_yrs_1_80.xlsx")) %>%
                        mutate(year=as.numeric(year),month=month-(year-1)*12)

ggplot(bottom_temp_deltas, aes(x=year, y=mean_temp))+geom_line()+
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
                    mutate(projection = bt_tmp+mean_temp) %>%
                    group_by(month) %>%
                    mutate(sd_proj = sd(projection, na.rm=TRUE),
                           mean_proj=mean(projection, na.rm=TRUE)) %>%
                    ungroup() %>% 
                    left_join(climatology_sd,by=c("month")) %>%
                    mutate(sd_ratio = c_std_dev/sd_proj,
                           new_projection=(projection-mean_proj)*sd_ratio+mean_proj) %>%
                    group_by(month) %>% 
                    mutate(new_avg=mean(new_projection)) %>%
                    ungroup()
#Subsetting the correction factors
Corrections <- bottom_temp_levels %>%
  select(month,sd_ratio) %>% unique

Distributions <- bottom_temp_levels %>% 
        group_by(cut(y1,breaks=seq(1,30, by=10),right=F),month) %>%
        summarise(average_temp=new_projection,
                  sd_temp=sd(new_projection))%>% ungroup()
        
                    
bottom_temp_distributions <- bottom_temp_deltas  %>%
  filter(year %in% c(1:80)) %>%
  mutate(y1=year) %>%
  select(-year) %>%
  left_join(climatology, by=c("month")) %>%
  left_join(Corrections,by=c("month")) %>%
  mutate(projection = mean_climatology+mean_temp) %>%
    group_by(cut(y1,seq(1,80,by=10), right=F), month) %>%
    mutate(mean_proj=mean(projection, na.rm=TRUE),
    new_projection=(projection-mean_proj)*sd_ratio+mean_proj) %>%
    summarise(average_temp= mean(new_projection),
              sd_temp = sd(new_projection, na.rm=TRUE))%>% ungroup()

colnames(bottom_temp_distributions)[1] <- "Cut"

bottom_temp_distributions <- bottom_temp_distributions %>%
  left_join(bottom_temp_distributions %>% 
              ungroup %>%
              select(Cut) %>%
              unique %>%
              mutate(decade = order(Cut)))
    
  

ggplot(bottom_temp_distributions) + aes(x=decade,y=average_temp, group=month,colour=month) +
    geom_line()

#test to make sure means are the same, and only variances changed

bottom_temp_distributions_test <- bottom_temp_deltas  %>%
  filter(year %in% c(1:80)) %>%
  mutate(y1=year) %>%
  select(-year) %>%
  left_join(climatology, by=c("month")) %>%
  left_join(Corrections,by=c("month")) %>%
  mutate(projection = mean_climatology+mean_temp) %>%
  group_by(cut(y1,seq(1,80,by=10), right=F), month) %>%
  summarise(average_temp_test= mean(projection, na.rm=TRUE),
            sd_temp_test = sd(projection, na.rm=TRUE)) %>% ungroup()

colnames(bottom_temp_distributions_test)[1] <- "Cut"

bottom_temp_distributions_test <- bottom_temp_distributions_test %>%
  left_join(bottom_temp_distributions) %>% 
  mutate(mean_difference = average_temp-average_temp_test,
         sd_differences = sd_temp - sd_temp_test )

max(bottom_temp_distributions_test$mean_difference)
max(bottom_temp_distributions_test$sd_differences)

ggplot(bottom_temp_distributions_test)+
  aes(y=sd_differences, group=as.factor(month), colour=as.factor(month))+
  geom_boxplot()

#writing the results out
# bottom_temp_distributions$levels <- 

bottom_temp_distributions <- bottom_temp_distributions[,-1]

write_excel(bottom_temp_distributions,paste0(cd,"decadal_bottom_temp_distributions_levels_3_12_2025.xlsx"))

