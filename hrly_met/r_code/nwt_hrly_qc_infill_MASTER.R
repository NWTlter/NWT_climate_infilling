#Script for creating master file of Niwot Ridge QCd/infilled hourly climate data
#See individual variable files for QC and infill processing steps

#Version 2

#Stations: C1, D1, SDL
#Future iterations may also include Arikaree, Subnivean, and GLV stations

#Time period: 1990-01-01 to 2013-12-31
#Time period to be expanded when further data available

#Keith Jennings
#2016-06-13
#ksjennin@gmail.com

#Import the variable workspaces
setwd("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/") 
var.wrkspace <- c("nwt_hrly_qc_infill_RH_V2_THRUPOST.RData",
                  "nwt_hrly_qc_infill_SOLAR_V2.RData",
                  "nwt_hrly_qc_infill_TEMP_V2.RData",
                  "nwt_hrly_qc_infill_WINDSPD_V2.RData",
                  "nwt_hrly_qc_infill_LW_V2.RData",
                  "nwt_hrly_ppt.RData")

for(i in 1:length(var.wrkspace)) load(file = var.wrkspace[i])

#Remove all objects but the variable lists
rm(list = setdiff(ls(), 
                  c("rh", "solar", "temp", "wind", "lw", "ppt_h")))

#Create master list (each entry = 1 station)
#Note: This will only include relevant date information and the final data product for each variable
#See the individual variables for the full QCing, infilling, and flagging

master <- list()
for(i in 1:3){
  #Merge temperature and relative humidity data in the master list
  master[[i]] <- merge(temp[[i]][, c("datetime", "doy", "site", "temp_FILL2", "flag_FILL", "fill_type")],
                       rh[[i]][, c("datetime", "rh_FILL_POSTa", "rh_FILL_POSTb", "flag_FILL1a", "fill_type")],
                       by = "datetime")
  colnames(master[[i]])[c(5,6, 9, 10)] <- c("temp_FILL_FLAG", "temp_FILLTYPE", "rh_FILL_FLAG", "rh_FILLTYPE")
  
  #Merge with solar data
  master[[i]] <- merge(master[[i]],
                       solar[[i]][, c("datetime", "aman", "solar_FILL3", "flag_FILL2", "fill_type")],
                       by = "datetime")
  colnames(master[[i]])[c(13,14)] <- c("solar_FILL_FLAG", "solar_FILLTYPE")
  #Convert solar to watts per meter squared
  master[[i]]$solar_FILL3_W <-
    master[[i]]$solar_FILL3 * 1000000 / 3600
  
  #Merge with wind data
  master[[i]] <- merge(master[[i]],
                       wind[[i]][, c("datetime", "wind_FILL3", "flag_FILL2", "fill_type")],
                       by = "datetime")
  colnames(master[[i]])[c(17,18)] <- c("wind_FILL_FLAG", "wind_FILLTYPE")
 
   #Merge with longwave data
  master[[i]] <- merge(master[[i]],
                       lw[[i]][[1]][, c("datetime", "lw_all_ang", "lw_all_dil")],
                       by = "datetime")
  colnames(master[[i]])[c(19,20)] <- c("lw_all_anga", "lw_all_dila")
  master[[i]] <- merge(master[[i]],
                       lw[[i]][[2]][, c("datetime", "lw_all_ang", "lw_all_dil")],
                       by = "datetime")
  colnames(master[[i]])[c(21,22)] <- c("lw_all_angb", "lw_all_dilb")
  
  #Merge with precipitation data
  master[[i]] <- merge(master[[i]],
                       ppt_h[[i]][, c("datetime", "ppt_h", "ppt_FLAG")],
                       by = "datetime")
 
  #Arrange by datetime
  master[[i]] <- arrange(master[[i]], datetime)
  
  #Add date information
  master[[i]]$hour <- as.numeric(format(master[[i]]$datetime, "%H"))
  master[[i]]$date <- as.character(format(master[[i]]$datetime, "%Y-%m-%d"))
  master[[i]]$time <- as.character(format(master[[i]]$datetime, "%H:%M"))
  master[[i]]$year <- as.numeric(format(master[[i]]$datetime, "%Y"))
  
  #Put all date info in left columns
  #Order is year - date - doy - hour - time - datetime
  master[[i]] <- master[[i]][ , c(28, 26, 2, 25, 27, 1,
                                  3:24)]
  
  #Export as .csv
  setwd(dir = "~/Documents/research/site_data/niwot/nwt_hrly_qc/processed_hrly_QC_infill/")
  write.csv(master[[i]], paste0(master[[i]][1, "site"], "_hrly_met_data_master_QC_infill.csv"), 
            row.names = FALSE, quote = FALSE)

}

#Leave only the master list
rm(list = setdiff(ls(), 
                  "master"))
