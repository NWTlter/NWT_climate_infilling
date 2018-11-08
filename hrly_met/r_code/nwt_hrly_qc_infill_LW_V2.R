#Niwot hourly downwelling radiation script
#Uses version 2 of QCd, infilled Niwot hourly climate data to estimate lw_in

#Stations: C1, D1, SDL
#Future iterations may also include Arikaree, Subnivean, and GLV stations

#Time period: 1990-01-01 to 2013-12-31
#Time period to be expanded when further data available

#Estimation protocol from Flerchinger et al. (2009)
#At Niwot Angstrom (1918) and Dilley and O'Brien (1998) equations produced best estimates
#When combined with the Crawford and Duchon (1999) cloud-correction algorithm

#Need relative humidity, temperature, and solar to estimate atmospheric emissivity
#Then downwelling longwave is estimated using the Stefan Boltzmann equation

#Keith Jennings
#ksjenni@gmail.com
#2016-06-13

#Import the variable workspaces
setwd("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/") 
var.wrkspace <- c("nwt_hrly_qc_infill_RH_V2_THRUPOST.RData",
                  "nwt_hrly_qc_infill_SOLAR_V2.RData",
                  "nwt_hrly_qc_infill_TEMP_V2.RData")

for(i in 1:length(var.wrkspace)) load(file = var.wrkspace[i])

#Remove all objects but the variable lists
rm(list = setdiff(ls(), 
                  c("rh", "solar", "temp")))

#Add all variables to a list
lw_tmp <- list()
lw <- list()
for(i in 1:3){
  lw_tmp[[i]] <- merge(temp[[i]][, c("datetime", "doy", "site", "temp_FILL2", "flag_FILL", "fill_type")],
                       rh[[i]][, c("datetime", "rh_FILL_POSTa", "rh_FILL_POSTb", "flag_FILL1a", "fill_type")],
                       by = "datetime")
  colnames(lw_tmp[[i]])[c(5, 6, 9, 10)] <- c("temp_FILL_FLAG", "temp_FILLTYPE", "rh_FILL_FLAG", "rh_FILLTYPE")
  
  lw_tmp[[i]] <- merge(lw_tmp[[i]],
                       solar[[i]][, c("datetime", "aman", "sw_in_tot_mj_SYN", "solar_FILL3", "flag_FILL2", "fill_type")],
                       by = "datetime")
  colnames(lw_tmp[[i]])[c(14, 15)] <- c("solar_FILL_FLAG", "solar_FILLTYPE")
  
  lw_tmp[[i]] <- arrange(lw_tmp[[i]], datetime)
  
  #Separate the two rh fills (POSTa and POSTb) into two separate lists so lw_in can be estimated for both
  lw[[i]] <- list()
  
  lw[[i]][[1]] <- #FILL_POSTa
    select(lw_tmp[[i]], -rh_FILL_POSTb)
  colnames(lw[[i]][[1]])[7] <- "rh_FILL_POST"
  
  lw[[i]][[2]] <-  #FILL_POSTb
    select(lw_tmp[[i]], -rh_FILL_POSTa)
  colnames(lw[[i]][[2]])[7] <- "rh_FILL_POST"
  
  #Name the list entries
  names(lw)[i] <- lw_tmp[[i]][1, "site"]
  names(lw[[i]])[1] <- paste0(lw_tmp[[i]][1, "site"], "a")
  names(lw[[i]])[2] <- paste0(lw_tmp[[i]][1, "site"], "b")
  
  
}

#remove the other variable lists
rm(list = setdiff(ls(), 
                  "lw"))

#Calculate clear sky atmospheric emissivity using Angstrom and Dilley
#Then cloudiness index and all-sky atmospheric emissivity
#Then downwelling longwave with Stefan Boltzmann

stefboltz = 0.0000000567

#estimate clear sky radiation at earth's surface
lat = 40.05
sol_dec <- data.frame("doy" = 1:366)
sol_dec$sol_dec <- 0.4102 * sin((((2 * pi)/365)) * (sol_dec$doy - 80) ) * (180/pi)
time_doy <- data.frame("time" = rep(1:24, times = 366),
                        "doy" = rep(1:366, each = 24))
sin_alpha <- left_join(time_doy, sol_dec,
                       by = "doy")
#Use package aspace for doing sin and cos in degrees
library(aspace)
sin_alpha$sin_alpha <- sin_d(lat) * sin_d(sin_alpha$sol_dec) +
  cos_d(lat) * cos_d(sin_alpha$sol_dec) * cos_d((360 * ((sin_alpha$time - 12) / 24)))
sin_alpha$zenith <- asin_d(sin_alpha$sin_alpha)



#Add average barometric pressure (kPa) for each site (estimated)
#Used for estimating atmospheric transmission
press <- data.frame("site" = c("c1", "d1", "sdl"),
                    "press" = c(71, 65, 67))

for (i in 1:length(lw)){
  #Dewpoint temperature estimated from RH and Td
  #http://andrew.rsmas.miami.edu/bmcnoldy/Humidity.html
  #Then vapor pressure calculated from dewpoint temperature
  #http://iridl.ldeo.columbia.edu/dochelp/QA/Basic/dewpoint.html
  for (j in 1: length(lw[[i]])){ 
    lw[[i]][[j]]$temp_dew <- #calculate dewpoint temp
      243.04 * (log(lw[[i]][[j]]$rh_FILL_POST / 100) + ((17.625 * lw[[i]][[j]]$temp_FILL2) / (243.04 + lw[[i]][[j]]$temp_FILL2))) /
      (17.625 - log(lw[[i]][[j]]$rh_FILL_POST /100) - ((17.625 * lw[[i]][[j]]$temp_FILL2) / (243.04 + lw[[i]][[j]]$temp_FILL2))) 
    lw[[i]][[j]]$vap_press <- #calculate vapor pressure
      0.611 * exp(5423 * ((1 / 273.15) - (1 / (lw[[i]][[j]]$temp_dew + 273.15))))
    lw[[i]][[j]]$temp_k <- #calculate temp in Kelvin
      lw[[i]][[j]]$temp_FILL2 + 273.15
    lw[[i]][[j]]$w <- #calculate precipitable water (from Prata, 1996)
      4650 * lw[[i]][[j]]$vap_press / lw[[i]][[j]]$temp_k
    
    lw[[i]][[j]]$e_clr_ang <- #calculate angstrom clear sky emissivity
      0.83 - 0.18 * 10 ^ (-0.067 * lw[[i]][[j]]$vap_press)
    lw[[i]][[j]]$lw_in_clr_dil <- #calculate dilley clear sky longwave
      59.38 + (113.7 * (lw[[i]][[j]]$temp_k / 273.16) ^ 6) + (96.96 * sqrt(lw[[i]][[j]]$w / 25))
    lw[[i]][[j]]$e_clr_dil <- #calculate dilley clear sky emissivity
      with(lw[[i]][[j]], lw_in_clr_dil / (stefboltz * (temp_k ^ 4)))
    
    #Estimate max potential sw_in based on solar position and atmospheric transmission
    lw[[i]][[j]]$time <- 
      as.numeric(format(lw[[i]][[j]]$datetime, "%H"))
    lw[[i]][[j]] <- 
      left_join(lw[[i]][[j]], sin_alpha, by = c("doy", "time"))
    lw[[i]][[j]] <- 
      arrange(lw[[i]][[j]], datetime)
    lw[[i]][[j]]$m <- 
      35 * lw[[i]][[j]]$sin_alpha *
      ((1224 * lw[[i]][[j]]$sin_alpha ^ 2 + 1) ^ -0.5)
    lw[[i]][[j]]$taur_taupg <-
      1.021 - 0.084 * ((lw[[i]][[j]]$m * (0.00949 * press[i, "press"] + 0.051)) ^ 0.5)
    lw[[i]][[j]]$tauw <-
      1 - (0.077 * (lw[[i]][[j]]$m * lw[[i]][[j]]$w) ^ 0.3)
    lw[[i]][[j]]$taua <-
      0.935 ^ lw[[i]][[j]]$m
    lw[[i]][[j]]$sw_in_clearsky <-
      ifelse(lw[[i]][[j]]$sin_alpha > 0,
             1360 * with(lw[[i]][[j]], sin_alpha * taur_taupg * tauw * taua),
             0)
    
    #Estimate cloudiness by ratioing sw_in (observed) with sw_in_clearsky (potential max)
    lw[[i]][[j]]$sw_in_w <- 
      lw[[i]][[j]]$solar_FILL3 * 1000000 / 3600
    lw[[i]][[j]]$sw_in_w_SYN <- 
      lw[[i]][[j]]$sw_in_tot_mj_SYN * 1000000 / 3600
    lw[[i]][[j]]$s_ratio <- 
      with(lw[[i]][[j]], sw_in_w / sw_in_clearsky)
    lw[[i]][[j]]$s_ratio2 <-
      ifelse(lw[[i]][[j]]$s_ratio > 1,
             1, 
             ifelse(lw[[i]][[j]]$s_ratio < 0, 
                    0,
                    lw[[i]][[j]]$s_ratio))
    
    #Calculate 24 h moving average of s_ratio2
    lw[[i]][[j]]$date <- as.Date(lw[[i]][[j]]$date)
    lw[[i]][[j]] <- #add average daily solar ratio using pipes and dplyr (more efficient than with and merge)
      lw[[i]][[j]] %>% 
      group_by(date) %>% 
      mutate(s_ratio2_avg = mean(s_ratio2, na.rm = TRUE)) 
    #Note the s_ratio might be too low
    #Think of bias correcting and running sensitivity analysis
    
    #Estimate all sky emissivities for angstrom and dilley
    lw[[i]][[j]]$e_all_ang <-
      with(lw[[i]][[j]], (1 - s_ratio2_avg) + s_ratio2_avg * e_clr_ang)
    lw[[i]][[j]]$e_all_dil <-
      with(lw[[i]][[j]], (1 - s_ratio2_avg) + s_ratio2_avg * e_clr_dil)
    
    #Estimate incoming longwave radiation with stefan boltzmann
    lw[[i]][[j]]$lw_all_ang <-
      stefboltz * lw[[i]][[j]]$e_all_ang * (lw[[i]][[j]]$temp_k ^ 4)
    lw[[i]][[j]]$lw_all_dil <-
      stefboltz * lw[[i]][[j]]$e_all_dil * (lw[[i]][[j]]$temp_k ^ 4)
    
    }
  
    
}
