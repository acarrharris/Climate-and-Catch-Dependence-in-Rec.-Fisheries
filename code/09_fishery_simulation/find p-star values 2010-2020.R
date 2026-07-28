

# starting points for calibration

if (!(y %in% c(2018, 2019))){
  p_star_cod_variable<-MRIP_data$cod_harvest/MRIP_data$cod_tot_cat
  p_star_hadd_variable<-MRIP_data$hadd_harvest/MRIP_data$hadd_tot_cat
}

if (y==2018){
  p_star_cod_variable<-1
  p_star_hadd_variable<-0.49069481
}

if (y==2019){
  p_star_cod_variable<-0
  p_star_hadd_variable<-0.22310685
}


# In 2018, the federal cod fishery was closed with no states deviating from the federal closure. 
# However, there is a small amount of observed harvest in the MRIP data in this year. 
# Since this trip simulation does not account for illegal harvest, we instead calibrate the model to only haddock 
# (not haddock and cod) harvest in 2018 

# Similarly, in 2019, there was a two-week open cod season from September 15-30 with a 1-fish bag limit. 
# Total cod harvest from MRIP in this year was 16,613 fish, much of it outside the open season (about 44% of the cod landings 
# in 2019 occurred by Maine private boat anglers in the first half of June). This again indicates illegal harvest
# that we cannot account for here. Therefore, like 2018, we calibrate the simulation model in 2019 to the MRIP estimate of
# haddock only and allow the maximum harvest of cod in the period during which cod was officially open.

cod_achieved<-0
hadd_achieved<-0

  repeat {
    
    print("p_star_cod_variable")
    print(p_star_cod_variable)
    print("p_star_hadd_variable")
    print(p_star_hadd_variable)
    print(y)
    
    source(paste0(input_code_cd,"calibration loop2 2010-2020.R"))
    
    calibration_check<-readRDS(paste0(output_data_cd, "calibration_data_",y,"draw", i,".rds")) %>% 
      select(period2, n_choice_occasions)
    
    domains<- n_distinct(calibration_check$period2)

    #cod
    print(paste0("model cod harvest:", round(sum(aggregate_trip_data$tot_keep_cod))))
    print(paste0("MRIP cod harvest:", round(sum(MRIP_data$cod_harvest))))
    print(paste0("diff cod harvest:", sum(aggregate_trip_data$tot_keep_cod)-sum(MRIP_data$cod_harvest)))
    print(paste0("% diff cod harvest:", round(((sum(aggregate_trip_data$tot_keep_cod)-sum(MRIP_data$cod_harvest))/sum(MRIP_data$cod_harvest)*100), 2)))

    print(paste0("model hadd harvest:", round(sum(aggregate_trip_data$tot_keep_hadd))))
    print(paste0("MRIP hadd harvest:", round(sum(MRIP_data$hadd_harvest))))
    print(paste0("diff hadd harvest:", sum(aggregate_trip_data$tot_keep_hadd)-sum(MRIP_data$hadd_harvest)))
    print(paste0("% diff hadd harvest:", round(((sum(aggregate_trip_data$tot_keep_hadd)-sum(MRIP_data$hadd_harvest))/sum(MRIP_data$hadd_harvest)*100), 2)))
    
    
    if (y %in%  c(2018, 2019)){
      cod_achieved<-1
      hadd_achieved <- ifelse(hadd_harvest_diff<500 | abs(hadd_harvest_perc_diff)<5, 1, 0) 
      
    }
    
    if (!(y %in%  c(2018, 2019))){
      cod_achieved <- ifelse(cod_harvest_diff<500 | abs(cod_harvest_perc_diff)<5, 1, 0) 
      hadd_achieved <- ifelse(hadd_harvest_diff<500 | abs(hadd_harvest_perc_diff)<5, 1, 0) 
      
    }

    
    if  ((cod_achieved==1 & hadd_achieved==1 & domains==n_periods))  break

    if  ((domains!=n_periods))  {
      seed<-seed+1
    }
    
    if (cod_harvest_perc_diff<0 & cod_achieved!=1){
      
      if (cod_harvest_perc_diff< -20){
        p_star_cod_variable<-p_star_cod_variable - 0.075
      }
      
      if (cod_harvest_perc_diff< -10 & cod_harvest_perc_diff>= -20){
        p_star_cod_variable<-p_star_cod_variable - 0.035
      }
      
      if (cod_harvest_perc_diff< -5 & cod_harvest_perc_diff>= -10){
        p_star_cod_variable<-p_star_cod_variable - 0.015
      }
      
    } 
    
    
    if (hadd_harvest_perc_diff<0 & hadd_achieved!=1){
      
      if (hadd_harvest_perc_diff< -20){
        p_star_hadd_variable<-p_star_hadd_variable - 0.075
      }
      
      if (hadd_harvest_perc_diff< -10 & hadd_harvest_perc_diff>= -20){
        p_star_hadd_variable<-p_star_hadd_variable - 0.035
      }
      
      if (hadd_harvest_perc_diff< -5 & hadd_harvest_perc_diff>= -10){
        p_star_hadd_variable<-p_star_hadd_variable - 0.015
      }
      
    } 
    
    
    if (cod_harvest_perc_diff>0 & cod_achieved!=1){
      
      if (cod_harvest_perc_diff> 20){
        p_star_cod_variable<-p_star_cod_variable + 0.07
      }
      
      if (cod_harvest_perc_diff> 10 & cod_harvest_perc_diff<= 20){
        p_star_cod_variable<-p_star_cod_variable + 0.03
      }
      
      if (cod_harvest_perc_diff> 5 & cod_harvest_perc_diff<= 10){
        p_star_cod_variable<-p_star_cod_variable + 0.01
      }
      
    } 
    
    if (hadd_harvest_perc_diff>0 & hadd_achieved!=1){
      
      if (hadd_harvest_perc_diff> 20){
        p_star_hadd_variable<-p_star_hadd_variable + 0.07
      }
      
      if (hadd_harvest_perc_diff> 10 & hadd_harvest_perc_diff<= 20){
        p_star_hadd_variable<-p_star_hadd_variable + 0.03
      }
      
      if (hadd_harvest_perc_diff> 5 & hadd_harvest_perc_diff<= 10){
        p_star_hadd_variable<-p_star_hadd_variable + 0.01
      }
      
    } 
    
  }
  
  
  p_star_cod_variable<- p_star_cod
  p_star_hadd_variable<- p_star_hadd
  
