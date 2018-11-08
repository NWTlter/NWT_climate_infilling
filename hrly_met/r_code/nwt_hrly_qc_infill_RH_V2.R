#Niwot hourly QC and infill script
#Version 2

#Stations: C1, D1, SDL
#Future iterations may also include Arikaree, Subnivean, and GLV stations

#Time period: 1990-01-01 to 2013-12-31
#Time period to be expanded when further data available

#QC Protocol based on Meek and Hatfield (1994)
#Any deviations will be noted within each section

#Temporal, single station infilling (gap <= 72 h) based on Liston and Elder (2006) — aka, MicroMet
#Spatial infilling (gap > 72 h) using linear regression or climatic mean

#Keith Jennings
#ksjenni@gmail.com
#2016-06-08


##############################
##############################
#QC variable 4: RELATIVE HUMIDITY
##############################

#Note: QC1 and preprocessing have already been performed in:
#nwt_hrly_qc_infill_RH_V2_PREPROCESS
#This was done to correct the pre-2000 sensor drift

#Import the preprocess workspace:
load("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/nwt_hrly_qc_infill_RH_V2_PREPROCESS.RData")


#RH is being converted to vapor pressure (kPa) for the second QC protocol
#from Meek and Hatfield (rate of change)
#Requires air temperature (using infilled data here)
#Import temperature data
load("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/nwt_hrly_qc_infill_TEMP_V2.RData")

#Dewpoint temperature estimated from RH and Td
#http://andrew.rsmas.miami.edu/bmcnoldy/Humidity.html
#Then vapor pressure calculated from dewpoint temperature
#http://iridl.ldeo.columbia.edu/dochelp/QA/Basic/dewpoint.html

for (i in 1: length(rh)){
  rh[[i]] <- #Join RH and temp data for dewpoint and vap press calcs
    left_join(rh[[i]], temp[[i]][ , c("datetime", "temp_FILL2")], 
              by = "datetime")
  rh[[i]]$temp_dew1 <- 
    243.04 * (log(rh[[i]]$rh_preproc / 100) + ((17.625 * rh[[i]]$temp_FILL2) / (243.04 + rh[[i]]$temp_FILL2))) /
    (17.625 - log(rh[[i]]$rh_preproc /100) - ((17.625 * rh[[i]]$temp_FILL2) / (243.04 + rh[[i]]$temp_FILL2))) 
  rh[[i]]$vap_press1 <- 
    0.611 * exp(5423 * ((1 / 273.15) - (1 / (rh[[i]]$temp_dew1 + 273.15))))
  rh[[i]]$temp_dew2 <- 
    243.04 * (log(rh[[i]]$rh_preproc2 / 100) + ((17.625 * rh[[i]]$temp_FILL2) / (243.04 + rh[[i]]$temp_FILL2))) /
    (17.625 - log(rh[[i]]$rh_preproc2 /100) - ((17.625 * rh[[i]]$temp_FILL2) / (243.04 + rh[[i]]$temp_FILL2))) 
  rh[[i]]$vap_press2 <- 
    0.611 * exp(5423 * ((1 / 273.15) - (1 / (rh[[i]]$temp_dew2 + 273.15))))
}


#Apply second QC protocol
#Remove anomalous values on rate of change threshold (1 kPa per h)
#Note: this rate of change threshold might be too high for an arid
#cool site like Niwot Ridge
#Consider changing or using anomaly detection
#Note: the preprocessed values are being used here
for (i in 1:length(rh)){
  rh[[i]]$diff_1ha <- c(0, diff(rh[[i]]$vap_press1))
  
  rh[[i]]$rh_QC2a <- ifelse(rh[[i]]$diff_1ha <= 1 & rh[[i]]$diff_1ha >= -1,
                           rh[[i]]$rh_preproc,
                           NA)
  
  rh[[i]]$rh_FLAG2a <- ifelse(is.na(rh[[i]]$rh_preproc) == is.na(rh[[i]]$rh_preproc),
                             NA,
                             2) #flag 2 means rh outside rate-of-change threshold
  rh[[i]]$diff_1hb <- c(0, diff(rh[[i]]$vap_press2))
  
  rh[[i]]$rh_QC2b <- ifelse(rh[[i]]$diff_1hb <= 1 & rh[[i]]$diff_1hb >= -1,
                            rh[[i]]$rh_preproc2,
                            NA)
  
  rh[[i]]$rh_FLAG2b <- ifelse(is.na(rh[[i]]$rh_preproc2) == is.na(rh[[i]]$rh_preproc2),
                              NA,
                              2) #flag 2 means rh outside rate-of-change threshold
}

#Apply third quality control protocol
#rh sensor is stuck
for (i in 1: length(rh)){
  #Create shifted rh columns
  #If the shifted values equal one another, then rh is stuck
  rh[[i]]$shift1a <- c(NA, rh[[i]][1: (length(rh[[i]]$rh_preproc) - 1), "rh_preproc"])
  rh[[i]]$shift2a <- c(NA, NA, rh[[i]][1: (length(rh[[i]]$rh_preproc) - 2), "rh_preproc"])
  rh[[i]]$shift3a <- c(NA, NA, NA, rh[[i]][1: (length(rh[[i]]$rh_preproc) - 3), "rh_preproc"])
  
  rh[[i]]$rh_QC3a <- ifelse(rh[[i]]$rh_preproc ==  rh[[i]]$shift1a &
                             rh[[i]]$shift1a ==  rh[[i]]$shift2a &
                             rh[[i]]$shift2a ==  rh[[i]]$shift3a,
                           NA,
                           rh[[i]]$rh_QC2a)
  
  rh[[i]][1, "rh_QC3a"] = #Replace first value with QC2 (artificial NA at t = 0)
    rh[[i]][1, "rh_QC2a"]
  
  rh[[i]]$rh_FLAG3a <- ifelse(is.na(rh[[i]]$rh_QC2a) == is.na(rh[[i]]$rh_QC3a) ,
                             NA,
                             3) #flag 3 means rh sensor stuck
  
  rh[[i]]$shift1b <- c(NA, rh[[i]][1: (length(rh[[i]]$rh_preproc2) - 1), "rh_preproc2"])
  rh[[i]]$shift2b <- c(NA, NA, rh[[i]][1: (length(rh[[i]]$rh_preproc2) - 2), "rh_preproc2"])
  rh[[i]]$shift3b <- c(NA, NA, NA, rh[[i]][1: (length(rh[[i]]$rh_preproc2) - 3), "rh_preproc2"])
  
  rh[[i]]$rh_QC3b <- ifelse(rh[[i]]$rh_preproc2 ==  rh[[i]]$shift1b &
                              rh[[i]]$shift1b ==  rh[[i]]$shift2b &
                              rh[[i]]$shift2b ==  rh[[i]]$shift3b,
                            NA,
                            rh[[i]]$rh_QC2b)
  
  rh[[i]][1, "rh_QC3b"] = #Replace first value with QC2 (artificial NA at t = 0)
    rh[[i]][1, "rh_QC2b"]
  
  rh[[i]]$rh_FLAG3b <- ifelse(is.na(rh[[i]]$rh_QC2b) == is.na(rh[[i]]$rh_QC3b) ,
                              NA,
                              3) #flag 3 means rh sensor stuck
  
}

##############################
##############################
#INFILL variable 4, protocol 1: RELATIVE HUMIDITY (univariate time series infill, as in MicroMet)
##############################

#This protocol follows a hierarchical selection
#As in Liston et al. (2006) and Henn et al. (2012)
#1 Subset to only missing data after final QC protocol
#2 Difference the time and assign each observation to a gap number (gap number increases by 1 when gap > 1h)
#3 Merge gap number and gap length with missing data
#4 Split missing data into list with each entry corresponding into one gap
#5 Fill 1 h gaps using linear interpolation
#6 Fill 2 to 24 h gaps using average of previous and next observation (24 h before and after)
#7a Fill 25 to 72 h gaps using ARIMA model
#7b Add the preceding n hours and following n hours to the data (where n = gap length)
#7c Provide warning if data before or after are missing and fill as appropriate
#7d ARIMA format is 1,1,1 and seasonal 0,1,1 period = 24 h
#7e Forecast and backcast using the previous and following data (backcast requires reversing data)
#7f Fill in gap using linearly weighted forecast/backcast data
#8 73+ h time gaps filled as regressions between stations

#The protocol will be run twice, once for each preprocessed dataset (one with regular scale, one with transform)

version = c("a", "b")


#####################################################################
#####################################################################
#Note, the below needs to be run by manually entering the loop numbers
#There is an issue with the arima for c1 ARIMA #4
#And SDL ARIMA 10
#Run i individually
#1) i = 1 and q = 1, then k in 1:3
#2) then k in 5:length(rh_gap_ARIMA_l)
#3) then q = 2 and k in 1:3
#4) then k in 5:length(rh_gap_ARIMA_l)
#5) then i = 2 (run all)
#6) then i = 3 and k in 1:9

#See nwt_hrly_qc_infill_RH_V2_FILL1_stepbystep.R for more

#Don't run:
#for (i in 1:length(rh)){
 # for (q in 1: length(version)){
  #  rh_gap <- #subset to all missing observations
      rh[[i]][is.na(rh[[i]][, paste0("rh_QC3", version[q]) ]) , ]
    
    rh_gap$time_diff <- #calculate time diff in hours
      c(0, diff.POSIXt(rh_gap$datetime)) 
    
    tmp <-
      head(diff.POSIXt(rh_gap$datetime))
    
    if(attr(tmp, which = "units") == "secs") {
      #Divide time_diff by 3600 if differences are in seconds not hours
      #That diff.posix units can't be controlled is a known error in R
      rh_gap$time_diff <- rh_gap$time_diff / 3600
    }
    
    #assign dummy gap number and initiate gap counter (for numbering gaps)
    rh_gap$gap_num <- 0
    gap_counter = 1
    
    for (j in 1:length(rh_gap$time_diff)){
      if (rh_gap[j, "time_diff"] > 1) { 
        gap_counter = #increase gap counter by 1 if gap longer than 1 h
          gap_counter + 1
        rh_gap[j, "gap_num"] = #assign new gap#
          gap_counter
      } else {
        rh_gap[j, "gap_num"] = #assign gap number
          gap_counter
      }
    }
    
    if(q == 1){
      rh_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
        rh_gap %>% 
        group_by(gap_num) %>% 
        mutate(gap_length = length(rh_QC3a)) } else {
          rh_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
            rh_gap %>% 
            group_by(gap_num) %>% 
            mutate(gap_length = length(rh_QC3b)) 
        }
    
    rh_gap$fill_type <- #assign gap categorization
      fun_FILLTYPE(rh_gap$gap_length)
    
    rh[[i]] <- #Merge gap information with complete dataset
      merge(rh[[i]], rh_gap[ , c("datetime", "gap_num", "gap_length", "fill_type")],
            by = "datetime", all.x = TRUE)
    
    rh[[i]] <- #make sure df is ordered by datetime
      arrange(rh[[i]], datetime)
    
    #Add time shifted values for linear interpolation (filltype = INTERP)
    rh[[i]]$shiftpos1 <- 
      c(NA, rh[[i]][1: (length(rh[[i]]$datetime) - 1), paste0("rh_QC3", version[q])])
    rh[[i]]$shiftneg1 <- 
      c(rh[[i]][2: (length(rh[[i]]$datetime)), paste0("rh_QC3", version[q])], NA)
    
    #fill gaps = 1 h
    rh[[i]][, paste0("rh_FILL1", version[q])] <- 
      ifelse(is.na(rh[[i]]$fill_type) == TRUE,
             rh[[i]][, paste0("rh_QC3", version[q])], #if no fill needed, use rh_QC3
             ifelse(rh[[i]]$fill_type == "INTERP", 
                    (rh[[i]]$shiftpos1 + rh[[i]]$shiftneg1)/2,
                    rh[[i]][, paste0("rh_QC3", version[q])]))
    
    #Add time shifted values for 24 h pre/post averaging (filltype = AVG24)
    rh[[i]]$shiftpos24 <- 
      c(rep(NA, times = 24), rh[[i]][1: (length(rh[[i]]$datetime) - 24), paste0("rh_FILL1", version[q])])
    rh[[i]]$shiftneg24 <- 
      c(rh[[i]][25: (length(rh[[i]]$datetime)), paste0("rh_FILL1", version[q])], rep(NA, times = 24))
    
    #fill gaps > 1 h and <= 24 h
    rh[[i]][, paste0("rh_FILL1", version[q])] <- 
      ifelse(is.na(rh[[i]]$fill_type) == TRUE,
             rh[[i]][, paste0("rh_FILL1", version[q])], #if no fill needed, use previously filled value (i.e. QC3)
             ifelse(rh[[i]]$fill_type == "AVG24",
                    (rh[[i]]$shiftpos24 + rh[[i]]$shiftneg24)/2,
                    rh[[i]][, paste0("rh_FILL1", version[q])]))
    
    
    rh_gap_ARIMA_l <- #Put all ARIMA filltypes into a list
      split(filter(rh[[i]], fill_type == "ARIMA"), 
            f = filter(rh[[i]], fill_type == "ARIMA")$gap_num)
    
    #Run ARIMAs for each 25-72 h set of missing observations
    for(k in 1:length(rh_gap_ARIMA_l)){
      gap = #calculate gap length of data to be filled
        length(rh_gap_ARIMA_l[[k]]$datetime)
      
      #Acquire data before and after missing obs of equal length to missing data
      tmp_gap_pre <- 
        subset(rh[[i]], datetime < min(rh_gap_ARIMA_l[[k]]$datetime) &
                 datetime >= (min(rh_gap_ARIMA_l[[k]]$datetime) - gap * 3600))
      tmp_gap_post <- 
        subset(rh[[i]], datetime > max(rh_gap_ARIMA_l[[k]]$datetime) &
                 datetime <= (max(rh_gap_ARIMA_l[[k]]$datetime) + gap * 3600))
      tmp_gap_post <- #reverse post data for backcasting
        arrange(tmp_gap_post, desc(datetime))
      
      #Skip ARIMA if too many missing values in pre or post data
      if(length(tmp_gap_pre[is.na(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1 | 
         length(tmp_gap_post[is.na(tmp_gap_post[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1) {
        print(paste(names(rh[i]), k, version[q], "fail"))
        next
      }
      
      #Make forecast and backcast ARIMA predictions
      tmp_gap_pre_fit <- 
        arima(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
              seasonal = list(order = c(0,0,1), period = 24))
      tmp_gap_pre_forecast <- 
        predict(tmp_gap_pre_fit, n.ahead = gap)
      
      tmp_gap_post_fit <- 
        arima(tmp_gap_post[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
              seasonal = list(order=c(0,0,1),period=24))
      tmp_gap_post_backcast <- 
        predict(tmp_gap_post_fit, n.ahead = gap)
      
      #Calculate the weights to be applied to the forecast and backcast
      wt1 <- ((gap - 1):0)/gap
      wt2 <- (0:(gap - 1))/gap
      
      #Predict missing data using through weighted forecast and backcast
      rh_gap_ARIMA_l[[k]][, paste0("rh_FILL1", version[q]) ] <-  
        (tmp_gap_pre_forecast$pred * wt1) +
        (rev(tmp_gap_post_backcast$pred)* wt2)
      
      #Remove missing rows from data
      rh[[i]] <- 
        subset(rh[[i]], datetime %in% rh_gap_ARIMA_l[[k]]$datetime == FALSE)
      
      #And add infilled data in its place
      rh[[i]] <- 
        rbind(rh[[i]], rh_gap_ARIMA_l[[k]])
    }
    rh[[i]] <- #make sure df is ordered by datetime
      arrange(rh[[i]], datetime)
    
    rh[[i]][, paste0("rh_FLAG4", version[q]) ] <- 
      ifelse(is.na(rh[[i]][, paste0("rh_QC3", version[q]) ]) == is.na(rh[[i]][, paste0("rh_FILL1", version[q]) ]) ,
             NA,
             4) #flag 4 means filled with same station time series data (concatenate with fill type for more info)
    
    #remove the "gap_num", "gap_length", "fill_type" columns (to prevent issues when going between vers a and b)
    if(q == 1){
      rh[[i]][, c("gap_num", "gap_length", "fill_type")] <- NULL
    }
  }
}


##############################
##############################
#INFILL variable 4, protocol 2: RH (multi-station regression)
##############################
#Simple infilling protocol for relative humidity
#Develop linear regressions for each month and three hour time block
#There are regressions for when both other stations are or one or the other is reporting
#Gives a total of three regressions (z ~ x + y, z ~ x, z ~ y)
#When no station exists use, climatological mean for that month and time block

#Make a master list for all stations (a list of three lists—one for each stations)
#Each station list has 12 data frames (one for each month)
#Each data frame has 12 columns (3 intercepts, 3 slopes, 3 R2, 1 climatological mean, 1 month and 1 hour cat)
#Each data frame has 8 rows (1 for each 3-hour time block)

#Please note the regressions are being performed on dewpoint temperature
#This helps satisfy the normality assumption of linear regression
#RH is closer to a uniform distribution at these sites and percentages are difficult to regress
#MicroMet and MeteoIO both use dewpoint temperature as well

#Load plyr (for list manipulation)
#Note, plyr masks mutate, arrange, and other functions from dplyr
library(plyr)

#Name the hour categories and add hour cats and months to the data
#And add dewpoint temperature from filled RH data
for(i in 1:length(rh) ){
  rh[[i]]$hour_cat <- fun_HOURCAT(as.numeric(format(rh[[i]]$datetime, "%H")))
  rh[[i]]$month <- as.numeric(format(rh[[i]]$datetime, "%m"))
  rh[[i]]$dpta <-
    243.04 * (log(rh[[i]]$rh_FILL1a / 100) + ((17.625 * rh[[i]]$temp_FILL2) / (243.04 + rh[[i]]$temp_FILL2))) /
    (17.625 - log(rh[[i]]$rh_FILL1a /100) - ((17.625 * rh[[i]]$temp_FILL2) / (243.04 + rh[[i]]$temp_FILL2)))
  rh[[i]]$dptb <- 
    243.04 * (log(rh[[i]]$rh_FILL1b / 100) + ((17.625 * rh[[i]]$temp_FILL2) / (243.04 + rh[[i]]$temp_FILL2))) /
    (17.625 - log(rh[[i]]$rh_FILL1b /100) - ((17.625 * rh[[i]]$temp_FILL2) / (243.04 + rh[[i]]$temp_FILL2)))
}

#Initiate the lists of regression data (one with FILL1a and one with FILL1b)
#In code below b refers to intercept forced to 0 regression
rh_regr_a <- list() #list of three lists (one per site) that hold the data by site and by month
rh_regr_a2 <- list() #list of three data frames (one per site) with regression info by month and hour

rh_regr_b <- list() #list of three lists (one per site) that hold the data by site and by month
rh_regr_b2 <- list() #list of three data frames (one per site) with regression info by month and hour


for(i in 1: n_stations){
  rh_regr_a[[i]] <- list() #initiate list of data frames
  names(rh_regr_a)[i] <- #name the data frame by site (e.g. c1, d1, etc.)
    sites[i]
  rh_regr_b[[i]] <- list() #initiate list of data frames
  names(rh_regr_b)[i] <- #name the data frame by site (e.g. c1, d1, etc.)
    sites[i]
  
  for(j in 1:12){
    tmp <- #Subset to only month of interest
      lapply(rh, filter, as.numeric(format(datetime, "%m")) == j )#use FILTER, not SUBSET
    tmp_mrg <- #merge stn 1 with stn 2
      merge(tmp[[station_ord[i, "stn1"]]], tmp[[station_ord[i, "stn2"]]], by = "datetime")
    tmp_mrg <- #mrg stns 1 and 2 with stn 3
      merge(tmp_mrg, tmp[[station_ord[i, "stn3"]]], by = "datetime") 
    regr_a1 <- #calculate regression by stns 2 and 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(dpta.x ~ dpta.y + dpta, data = df))
    regr_a2 <- #calculate regression by stn 2
      dlply(tmp_mrg, "hour_cat", function(df)lm(dpta.x ~ dpta.y, data = df))
    regr_a3 <- #calculate regression by stn 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(dpta.x ~ dpta, data = df))
    regr_b1 <- #calculate regression by stns 2 and 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(dptb.x ~ dptb.y + dptb, data = df))
    regr_b2 <- #calculate regression by stn 2
      dlply(tmp_mrg, "hour_cat", function(df)lm(dptb.x ~ dptb.y, data = df))
    regr_b3 <- #calculate regression by stn 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(dptb.x ~ dptb, data = df))
    
    #Initialize regression data frames in each station's list
    rh_regr_a[[i]][[j]] <- data.frame(int1 = 0, slope1a = 0, slope1b = 0, rsq1 = 0,
                                      int2 = 0, slope2 = 0, rsq2 = 0,
                                      int3 = 0, slope3 = 0, rsq3 = 0)
    rh_regr_b[[i]][[j]] <- data.frame(int1 = 0, slope1a = 0, slope1b = 0, rsq1 = 0,
                                      int2 = 0, slope2 = 0, rsq2 = 0,
                                      int3 = 0, slope3 = 0, rsq3 = 0)
    for(k in 1:8){ #run for each 3 h time block
      rh_regr_a[[i]][[j]][k, "int1"] = summary(regr_a1[[k]])$coefficients[1,1]
      rh_regr_a[[i]][[j]][k, "slope1a"] = summary(regr_a1[[k]])$coefficients[2,1]
      rh_regr_a[[i]][[j]][k, "slope1b"] = summary(regr_a1[[k]])$coefficients[3,1]
      rh_regr_a[[i]][[j]][k, "rsq1"] = summary(regr_a1[[k]])$r.squared
      rh_regr_a[[i]][[j]][k, "int2"] = summary(regr_a2[[k]])$coefficients[1,1]
      rh_regr_a[[i]][[j]][k, "slope2"] = summary(regr_a2[[k]])$coefficients[2,1]
      rh_regr_a[[i]][[j]][k, "rsq2"] = summary(regr_a2[[k]])$r.squared
      rh_regr_a[[i]][[j]][k, "int3"] = summary(regr_a3[[k]])$coefficients[1,1]
      rh_regr_a[[i]][[j]][k, "slope3"] = summary(regr_a3[[k]])$coefficients[2,1]
      rh_regr_a[[i]][[j]][k, "rsq3"] = summary(regr_a3[[k]])$r.squared
      #Climatological mean for month and time block
      rh_regr_a[[i]][[j]][k, "mean"] = mean(subset(tmp[[i]], hour_cat == hour_cats[k])$dpta, na.rm = TRUE)
      rh_regr_a[[i]][[j]][k, "month"] = j
      rh_regr_a[[i]][[j]][k, "hour_cat"] = hour_cats[k]
      
      #regression info for b
      rh_regr_b[[i]][[j]][k, "int1"] = summary(regr_b1[[k]])$coefficients[1,1]
      rh_regr_b[[i]][[j]][k, "slope1a"] = summary(regr_b1[[k]])$coefficients[2,1]
      rh_regr_b[[i]][[j]][k, "slope1b"] = summary(regr_b1[[k]])$coefficients[3,1]
      rh_regr_b[[i]][[j]][k, "rsq1"] = summary(regr_b1[[k]])$r.squared
      rh_regr_b[[i]][[j]][k, "int2"] = summary(regr_b2[[k]])$coefficients[1,1]
      rh_regr_b[[i]][[j]][k, "slope2"] = summary(regr_b2[[k]])$coefficients[2,1]
      rh_regr_b[[i]][[j]][k, "rsq2"] = summary(regr_b2[[k]])$r.squared
      rh_regr_b[[i]][[j]][k, "int3"] = summary(regr_b3[[k]])$coefficients[1,1]
      rh_regr_b[[i]][[j]][k, "slope3"] = summary(regr_b3[[k]])$coefficients[2,1]
      rh_regr_b[[i]][[j]][k, "rsq3"] = summary(regr_b3[[k]])$r.squared
      #Climatological mean for month and time block
      rh_regr_b[[i]][[j]][k, "mean"] = mean(subset(tmp[[i]], hour_cat == hour_cats[k])$dptb, na.rm = TRUE)
      rh_regr_b[[i]][[j]][k, "month"] = j
      rh_regr_b[[i]][[j]][k, "hour_cat"] = hour_cats[k]
    }
  }
  rh_regr_a2[[i]] <- #Put all regressions into one data frame for each site
    ldply(rh_regr_a[[i]])
  rh_regr_b2[[i]] <- #Put all regressions into one data frame for each site
    ldply(rh_regr_b[[i]])
}


#Merge regression coefficients with rh data and infill
rh_regr_fill <- #initialize list with rh and regression coefficients
  list()

for (i in 1:length(rh)){
  rh[[i]]$dpt_FILL2a <- #make FILL2a = dpta
    rh[[i]]$dpta
  rh[[i]]$rh_FLAG5a <- #initiate flagging
    NA
  
  rh[[i]]$dpt_FILL2b <- #make FILL2b = dptb
    rh[[i]]$dptb
  rh[[i]]$rh_FLAG5b <- #initiate flagging
    NA
  
  rh_regr_fill[[i]] <- #subset to NA values (missing obs)
    subset(rh[[i]], is.na(dpt_FILL2a)) #Note a and b NAs are the same
  rh_regr_fill[[i]]  <- #merge with closest station data
    left_join(rh_regr_fill[[i]], 
              rh[[station_ord[i, "stn2"]]][ , c("datetime", "dpta", "dptb")],
              by = "datetime")
  rh_regr_fill[[i]]  <- #merge with second closest station data
    left_join(rh_regr_fill[[i]], 
              rh[[station_ord[i, "stn3"]]][ , c("datetime", "dpta", "dptb")],
              by = "datetime")
  #merging both regression data frame will introduce .x and .y into variable names
  #.x = a, .y = b
  rh_regr_fill[[i]]  <- #merge with regression coefficients
    left_join(rh_regr_fill[[i]], 
              rh_regr_a2[[i]],
              by = c("month", "hour_cat"))
  rh_regr_fill[[i]]  <- #merge with b regression coefficients
    left_join(rh_regr_fill[[i]], 
              rh_regr_b2[[i]],
              by = c("month", "hour_cat"))
  
  rh_regr_fill[[i]]$dpt_FILL2a <- #infill based on which station is available
    ifelse(is.na(rh_regr_fill[[i]]$dpta.y) == FALSE & 
             is.na(rh_regr_fill[[i]]$dpta) == FALSE,
           (rh_regr_fill[[i]]$dpta.y * rh_regr_fill[[i]]$slope1a.x +
              rh_regr_fill[[i]]$dpta * rh_regr_fill[[i]]$slope1b.x +
              rh_regr_fill[[i]]$int1.x),
           ifelse(is.na(rh_regr_fill[[i]]$dpta.y) == FALSE & 
                    is.na(rh_regr_fill[[i]]$dpta) == TRUE,
                  (rh_regr_fill[[i]]$dpta.y * rh_regr_fill[[i]]$slope2.x +
                     rh_regr_fill[[i]]$int2.x),
                  ifelse(is.na(rh_regr_fill[[i]]$dpta.y) == TRUE & 
                           is.na(rh_regr_fill[[i]]$dpta) == FALSE,
                         (rh_regr_fill[[i]]$dpta * rh_regr_fill[[i]]$slope3.x +
                            rh_regr_fill[[i]]$int3.x),
                         rh_regr_fill[[i]]$mean.x)))
  
  rh_regr_fill[[i]]$rh_FLAG5a <- #FLAG based on which station is available
    ifelse(is.na(rh_regr_fill[[i]]$dpta.y) == FALSE & 
             is.na(rh_regr_fill[[i]]$dpta) == FALSE,
           5, #filled with both stations
           ifelse(is.na(rh_regr_fill[[i]]$dpta.y) == FALSE & 
                    is.na(rh_regr_fill[[i]]$dpta) == TRUE,
                  6, #filled with closest station
                  ifelse(is.na(rh_regr_fill[[i]]$dpta.y) == TRUE & 
                           is.na(rh_regr_fill[[i]]$dpta) == FALSE,
                         7, #filled with second closest station
                         8))) #filled with mean
  
  rh_regr_fill[[i]]$dpt_FILL2b <- #infill based on which station is available
    ifelse(is.na(rh_regr_fill[[i]]$dptb.y) == FALSE & 
             is.na(rh_regr_fill[[i]]$dptb) == FALSE,
           (rh_regr_fill[[i]]$dptb.y * rh_regr_fill[[i]]$slope1a.y +
              rh_regr_fill[[i]]$dptb * rh_regr_fill[[i]]$slope1b.y +
              rh_regr_fill[[i]]$int1.y),
           ifelse(is.na(rh_regr_fill[[i]]$dptb.y) == FALSE & 
                    is.na(rh_regr_fill[[i]]$dptb) == TRUE,
                  (rh_regr_fill[[i]]$dptb.y * rh_regr_fill[[i]]$slope2.y +
                     rh_regr_fill[[i]]$int2.y),
                  ifelse(is.na(rh_regr_fill[[i]]$dptb.y) == TRUE & 
                           is.na(rh_regr_fill[[i]]$dptb) == FALSE,
                         (rh_regr_fill[[i]]$dptb * rh_regr_fill[[i]]$slope3.y +
                            rh_regr_fill[[i]]$int3.y),
                         rh_regr_fill[[i]]$mean.y)))
  
  rh_regr_fill[[i]]$rh_FLAG5b <- #FLAG based on which station is available
    ifelse(is.na(rh_regr_fill[[i]]$dptb.y) == FALSE & 
             is.na(rh_regr_fill[[i]]$dptb) == FALSE,
           5, #filled with both stations
           ifelse(is.na(rh_regr_fill[[i]]$dptb.y) == FALSE & 
                    is.na(rh_regr_fill[[i]]$dptb) == TRUE,
                  6, #filled with closest station
                  ifelse(is.na(rh_regr_fill[[i]]$dptb.y) == TRUE & 
                           is.na(rh_regr_fill[[i]]$dptb) == FALSE,
                         7, #filled with second closest station
                         8))) #filled with mean
  
  
  tmp <- #make temporary file for binding
    rh_regr_fill[[i]][, c(1:57)] #use only the columns in rh[[]]
  rh[[i]] <- #remove entries that were filled in this step
    subset(rh[[i]], 
           datetime %in% tmp$datetime == FALSE)
  rh[[i]] <- #append the new, filled entries to the dataframe
    bind_rows(rh[[i]], tmp)
  rh[[i]] <- #make sure df is ordered by datetime
    arrange(rh[[i]], datetime)
  #Convert dewpoint back to RH
  rh[[i]]$rh_FILL2a <- 
    100 * (exp((17.625 * rh[[i]]$dpt_FILL2a) / (243.04 + rh[[i]]$dpt_FILL2a)) /
             exp((17.625 * rh[[i]]$temp_FILL2) / (243.04 + rh[[i]]$temp_FILL2)))
  rh[[i]]$rh_FILL2b <- 
    100 * (exp((17.625 * rh[[i]]$dpt_FILL2b) / (243.04 + rh[[i]]$dpt_FILL2b)) /
             exp((17.625 * rh[[i]]$temp_FILL2) / (243.04 + rh[[i]]$temp_FILL2)))
  
  rh[[i]]$flag_FILL1a <- #concatenate fill values for filling
    rowSums(rh[[i]][, c("rh_FLAG4a", "rh_FLAG5a")], na.rm= TRUE)
  rh[[i]]$flag_FILL1a <- #set 0 to NA in flag
    ifelse(rh[[i]]$flag_FILL1a == 0,
           NA,
           rh[[i]]$flag_FILL1a)
  rh[[i]]$flag_FILL1b <- #concatenate fill values for filling
    rowSums(rh[[i]][, c("rh_FLAG4b", "rh_FLAG5b")], na.rm= TRUE)
  rh[[i]]$flag_FILL1b <- #set 0 to NA in flag
    ifelse(rh[[i]]$flag_FILL1b == 0,
           NA,
           rh[[i]]$flag_FILL1b)
}

########################################################
########################################################
#Addendum to above
#Recalculate "mean" infills by using mean temperature
#This will remove issue caused by using infilled temperature in conversion

for(i in 1:length(rh)){
  rh[[i]] <- merge(rh[[i]], temp_regr2[[i]][, c("month", "hour_cat", "mean")], by = c("month", "hour_cat"))
  rh[[i]]$rh_FILL2a <- ifelse(is.na(rh[[i]]$rh_FLAG5a),
                                rh[[i]]$rh_FILL2a,
                                ifelse(rh[[i]]$rh_FLAG5a == 8,
                                       100 * (exp((17.625 * rh[[i]]$dpt_FILL2a) / (243.04 + rh[[i]]$dpt_FILL2a)) /
                                                exp((17.625 * rh[[i]]$mean) / (243.04 + rh[[i]]$mean))),
                                       rh[[i]]$rh_FILL2a))
  rh[[i]]$rh_FILL2b <- ifelse(is.na(rh[[i]]$rh_FLAG5b),
                              rh[[i]]$rh_FILL2b,
                              ifelse(rh[[i]]$rh_FLAG5b == 8,
                                     100 * (exp((17.625 * rh[[i]]$dpt_FILL2b) / (243.04 + rh[[i]]$dpt_FILL2b)) /
                                              exp((17.625 * rh[[i]]$mean) / (243.04 + rh[[i]]$mean))),
                                     rh[[i]]$rh_FILL2b))
  rh[[i]] <- rh[[i]][, -64]
  rh[[i]] <- arrange(rh[[i]], datetime)
}


#remove plyr
detach("package:plyr", unload=TRUE)

#Plot and save filled RH data
ggplot(rh[["c1"]], aes(datetime, rh_FILL2a, color = as.factor(flag_FILL1a))) + 
  geom_point() +
  labs(x = "Date", y = "RH (%)", title = "C1 Hourly RH (QC + Infill 2a)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_c1_hr_QC_INFILL2a.png")
ggplot(rh[["d1"]], aes(datetime, rh_FILL2a, color = as.factor(flag_FILL1a))) + 
  geom_point()+
  labs(x = "Date", y = "RH (%)", title = "D1 Hourly RH (QC + Infill 2a)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_d1_hr_QC_INFILL2a.png")
ggplot(rh[["sdl"]], aes(datetime, rh_FILL2a, color = as.factor(flag_FILL1a))) + 
  geom_point() +
  labs(x = "Date", y = "RH (%)", title = "SDL Hourly RH (QC + Infill 2a)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_sdl_hr_QC_INFILL2a.png")


ggplot(rh[["c1"]], aes(datetime, rh_FILL2b, color = as.factor(flag_FILL1b))) + 
  geom_point() +
  labs(x = "Date", y = "RH (%)", title = "C1 Hourly RH (QC + Infill 2b)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_c1_hr_QC_INFILL2b.png")
ggplot(rh[["d1"]], aes(datetime, rh_FILL2b, color = as.factor(flag_FILL1b))) + 
  geom_point()+
  labs(x = "Date", "RH (%)", title = "D1 Hourly RH (QC + Infill 2b)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_d1_hr_QC_INFILL2b.png")
ggplot(rh[["sdl"]], aes(datetime, rh_FILL2b, color = as.factor(flag_FILL1b))) + 
  geom_point() +
  labs(x = "Date", y = "RH (%)", title = "SDL Hourly RH (QC + Infill 2b)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_rh_sdl_hr_QC_INFILL2b.png")

########################################################
########################################################
#To correct the RH values created by regressing the dewpoint temperature, post processing was performed
#see nwt_hrly_qc_infill_rh_V2_POSTPROCESS.R


