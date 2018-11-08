#Postprocess Niwot hourly RH
#Version 2

#Stations: C1, D1, SDL
#Future iterations may also include Arikaree, Subnivean, and GLV stations

#Time period: 1990-01-01 to 2013-12-31
#Time period to be expanded when further data available

#This step is done to remove >100% values created by regressing dewpoint and converting to RH

#Keith Jennings
#ksjennin@gmail.com
#2016-06-10

#Import workspace
load("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/nwt_hrly_qc_infill_RH_V2_THRUFILL2.RData")


#Use means of monthly maxima from C1 2010-2013
#Highest quality RH data (see preprocess script for more info)



#The monthly mean RH maxima will be used to calculate the deviation and correct infilling deviation
rh_drift2 <- list()
for (i in 1: length(rh)){
  #Add rh data per station into rh_drift by finding the max per yr_mon
  rh_drift2[[i]] <- data.frame("yr_mon" = unique(rh[[i]]$yr_mon),
                              "rh_max_obsa" = as.numeric(with(rh[[i]], tapply(rh_FILL2a, yr_mon, max, na.rm = TRUE))),
                              "rh_max_obsb" = as.numeric(with(rh[[i]], tapply(rh_FILL2b, yr_mon, max, na.rm = TRUE))))
  rh_drift2[[i]]$mon <- as.numeric(substr(rh_drift2[[i]]$yr_mon, start = 5, stop = 6))
  #Merge with rh_max_mon2
  rh_drift2[[i]] <- merge(rh_drift2[[i]], rh_max_mon2, by = "mon", all.x = TRUE)
  #Calculate difference of rh_max observed and the monthly mean rh_max
  #This will be used to create scaling factor to correct for sensor drift based
  #on deviation from monthly mean rh_max
  rh_drift2[[i]]$rh_max_diffa <- with(rh_drift2[[i]],
                                    rh_max - rh_max_obsa)
  rh_drift2[[i]]$rh_max_diffb <- with(rh_drift2[[i]],
                                      rh_max - rh_max_obsb)
  #Calculate deviation as a decimal fraction
  rh_drift2[[i]]$rh_max_deva <- with(rh_drift2[[i]],
                                   rh_max_diffa / rh_max_obsa)
  rh_drift2[[i]]$rh_max_devb <- with(rh_drift2[[i]],
                                     rh_max_diffb / rh_max_obsb)
  #Merge with relative humidity data
  rh[[i]] <- merge(rh[[i]], rh_drift2[[i]][ , c("yr_mon", "rh_max_deva", "rh_max_obsa", "rh_max_devb", "rh_max_obsb")], 
                   by = "yr_mon", all.x = TRUE)
  #Find deviation scaling factor by dividing observed rh by the max for that month
  #Makes it so lower RH values are scaled less
  rh[[i]]$rh_max_dev_scalea <- with(rh[[i]],
                                   rh_FILL2a / rh_max_obsa)
  rh[[i]]$rh_max_dev_scaleb <- with(rh[[i]],
                                    rh_FILL2b / rh_max_obsb)
  rh[[i]]$rh_scalea <- with(rh[[i]],
                           rh_max_dev_scalea * rh_max_deva)
  rh[[i]]$rh_scaleb <- with(rh[[i]],
                            rh_max_dev_scaleb * rh_max_devb)
  #Correct for sensor drift based on deviation from monthly mean rh_max
  rh[[i]]$rh_FILL_POSTa <- with(rh[[i]],
                             rh_FILL2a * (1 + rh_scalea))
  rh[[i]]$rh_FILL_POSTb <- with(rh[[i]],
                                rh_FILL2b * (1 + rh_scaleb))
  
  
}



#Plot and save filled and postprocessed RH data
ggplot(rh[["c1"]], aes(datetime, rh_FILL_POSTa, color = as.factor(flag_FILL1a))) + 
  geom_point() +
  labs(x = "Date", y = "RH (%)", title = "C1 Hourly RH (QC + Infill Post 2a)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_c1_hr_QC_INFILL_POST2a.png")
ggplot(rh[["d1"]], aes(datetime, rh_FILL_POSTa, color = as.factor(flag_FILL1a))) + 
  geom_point()+
  labs(x = "Date", y = "RH (%)", title = "D1 Hourly RH (QC + Infill Post 2a)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_d1_hr_QC_INFILL_POST2a.png")
ggplot(rh[["sdl"]], aes(datetime, rh_FILL_POSTa, color = as.factor(flag_FILL1a))) + 
  geom_point() +
  labs(x = "Date", y = "RH (%)", title = "SDL Hourly RH (QC + Infill Post 2a)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_sdl_hr_QC_INFILL_POST2a.png")


ggplot(rh[["c1"]], aes(datetime, rh_FILL_POSTb, color = as.factor(flag_FILL1b))) + 
  geom_point() +
  labs(x = "Date", y = "RH (%)", title = "C1 Hourly RH (QC + Infill Post 2b)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_c1_hr_QC_INFILL_POST2b.png")
ggplot(rh[["d1"]], aes(datetime, rh_FILL_POSTb, color = as.factor(flag_FILL1b))) + 
  geom_point()+
  labs(x = "Date", "RH (%)", title = "D1 Hourly RH (QC + Infill Post 2b)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_d1_hr_QC_INFILL_POST2b.png")
ggplot(rh[["sdl"]], aes(datetime, rh_FILL_POSTb, color = as.factor(flag_FILL1b))) + 
  geom_point() +
  labs(x = "Date", y = "RH (%)", title = "SDL Hourly RH (QC + Infill Post 2b)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_sdl_hr_QC_INFILL_POST2b.png")
