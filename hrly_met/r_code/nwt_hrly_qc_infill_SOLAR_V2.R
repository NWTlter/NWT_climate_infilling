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
#2016-06-02


##############################
##############################
#QC variable 3: Solar Radiation (sw_in_tot_mj)
#Vector is equal to or lower than scalar (incorporates direction in calculation)
##############################

#First run the initialization script
#Gathers data, loads packages, writes functions, etc.
setwd("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/")
source(file = "nwt_hrly_qc_infill_RUNFIRST.R")

#Create list with temperature data from each site
solar <- lapply(X = hrly_data, FUN = select, 
               sw_in_tot_mj, datetime, site, doy)

#Preprocessing
#Values from 1990-01-01 to 1991-01-29 need to be multiplied by 2
#As per instructions from CR21x readme files
for (i in 1:length(solar)){
  solar[[i]]$sw_in_tot_mj <- ifelse(solar[[i]]$datetime < as.POSIXct("1991-01-30 01:00"),
                                    solar[[i]]$sw_in_tot_mj * 2,
                                    solar[[i]]$sw_in_tot_mj)
}

#Apply first QC protocol
#Remove anomalous values on max/min threshold
#Here max is the max incoming solar from solaR for that month, day, and hour
#NOTE: max might be too high - consider revising to correct for atmospheric transmissivity
#NOTE2: Abrupt transition at sunrise from solar = 0 to solar > 0 in synthetic data
#Buffer is used relative to synthetic data for more conservative QCing

#Load synthetic solar data
load("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/nwt_hrly_qc_infill_SOLAR_V2_SYNTHETICDATA.RData")

#Add mon_day_hr column to observations
#Then merge the synthetic data
for(i in 1:length(solar)){
  solar[[i]]$mon_day_hr <- as.character(format(solar[[i]]$datetime, "%m-%d-%H"))
  solar[[i]] <- merge(solar[[i]], solar_synth[ , c("mon_day_hr", "aman", "sw_in_tot_mj_SYN")], by = "mon_day_hr")
  solar[[i]] <- arrange(solar[[i]], datetime)
}

#Filter out data above/below threshold

solar_buffer = #buffering for QC threshold
  0.05

for (i in 1:length(solar)){
  solar[[i]]$solar_QC1 <- ifelse(solar[[i]]$sw_in_tot_mj <= solar[[i]]$sw_in_tot_mj_SYN + solar_buffer & solar[[i]]$sw_in_tot_mj >= 0,
                                 solar[[i]]$sw_in_tot_mj,
                                 NA)
  solar[[i]]$solar_FLAG0 <- ifelse(is.na(solar[[i]]$sw_in_tot_mj),
                                   0,
                                   NA) #flag 0 means data missing from original record
  solar[[i]]$solar_FLAG1 <- ifelse(is.na(solar[[i]]$sw_in_tot_mj) == is.na(solar[[i]]$solar_QC1),
                                   NA,
                                   1) #flag 1 means solar outside of max/min threshold
}

#Apply second QC protocol
#Remove anomalous values on rate of change threshold (2 MJ/m2 per h)

for (i in 1:length(solar)){
  solar[[i]]$diff_1h <- c(0, diff(solar[[i]]$sw_in_tot_mj))
  
  solar[[i]]$solar_QC2 <- ifelse(solar[[i]]$diff_1h <= 2 & solar[[i]]$diff_1h >= -2,
                                 solar[[i]]$solar_QC1,
                                 NA)
  
  solar[[i]]$solar_FLAG2 <- ifelse(is.na(solar[[i]]$solar_QC1) == is.na(solar[[i]]$solar_QC2),
                                   NA,
                                   2) #flag 2 means solar outside rate-of-change threshold
}

#Apply third quality control protocol
#solar sensor is stuck
for (i in 1: length(solar)){
  #Create shifted solar columns
  #If the shifted values equal one another, then solar is stuck
  #Note: this only applies to daylight hours
  #Daylight is when aman == 1 (logical from solaR package saying sun is above horizon)
  solar[[i]]$shift1 <- c(NA, solar[[i]][1: (length(solar[[i]]$sw_in_tot_mj) - 1), "sw_in_tot_mj"])
  solar[[i]]$shift2 <- c(NA, NA, solar[[i]][1: (length(solar[[i]]$sw_in_tot_mj) - 2), "sw_in_tot_mj"])
  solar[[i]]$shift3 <- c(NA, NA, NA, solar[[i]][1: (length(solar[[i]]$sw_in_tot_mj) - 3), "sw_in_tot_mj"])
  
  solar[[i]]$solar_QC3 <- ifelse(solar[[i]]$sw_in_tot_mj ==  solar[[i]]$shift1 &
                                   solar[[i]]$shift1 ==  solar[[i]]$shift2 &
                                   solar[[i]]$shift2 ==  solar[[i]]$shift3 &
                                   solar[[i]]$aman == 1,
                                 NA,
                                 solar[[i]]$solar_QC2)
  
  solar[[i]][1, "solar_QC3"] = #Replace first value with QC2
    solar[[i]][1, "solar_QC2"]
  
  solar[[i]]$solar_FLAG3 <- ifelse(is.na(solar[[i]]$solar_QC2) == is.na(solar[[i]]$solar_QC3) ,
                                   NA,
                                   3) #flag 3 means solar sensor stuck
  
}


##############################
##############################
#INFILL variable 2, protocol 1: SOLAR (univariate time series infill, as in MicroMet)
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

#First fill all nighttime gaps (where aman == 0) with 0
for (i in 1:length(solar)){
 solar[[i]]$solar_PREFILL <- ifelse(solar[[i]]$aman == 0 &
                                      is.na(solar[[i]]$solar_QC3),
                                    0,
                                    solar[[i]]$solar_QC3)
 solar[[i]]$solar_PREFLAG <- ifelse(is.na(solar[[i]]$solar_QC3) == is.na(solar[[i]]$solar_PREFILL) ,
                                    NA,
                                    "PRE0") #preflag means nighttime manually set to 0
}  
  
#Implement MicroMet temporal filling procedure
for (i in 1:length(solar)){
  solar_gap <- #subset to all missing observations
    filter(solar[[i]], is.na(solar_PREFILL))
  
  solar_gap$time_diff <- #calculate time diff in hours
    c(0, diff.POSIXt(solar_gap$datetime)) 
  
  tmp <-
    head(diff.POSIXt(solar_gap$datetime))
  
  if(attr(tmp, which = "units") == "secs") {
    #Divide time_diff by 3600 if differences are in seconds not hours
    #That diff.posix units can't be controlled is a known error in R
    solar_gap$time_diff <- solar_gap$time_diff / 3600
  }
  
  #assign dummy gap number and initiate gap counter (for numbering gaps)
  solar_gap$gap_num <- 0
  gap_counter = 1
  
  for (j in 1:length(solar_gap$time_diff)){
    if (solar_gap[j, "time_diff"] > 1) { 
      gap_counter = #increase gap counter by 1 if gap longer than 1 h
        gap_counter + 1
      solar_gap[j, "gap_num"] = #assign new gap#
        gap_counter
    } else {
      solar_gap[j, "gap_num"] = #assign gap number
        gap_counter
    }
  }
  
  solar_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
    solar_gap %>% 
    group_by(gap_num) %>% 
    mutate(gap_length = length(solar_PREFILL))
  
  solar_gap$fill_type <- #assign gap categorization
    fun_FILLTYPE(solar_gap$gap_length)
  
  solar[[i]] <- #Merge gap information with complete dataset
    merge(solar[[i]], solar_gap[ , c("datetime", "gap_num", "gap_length", "fill_type")],
          by = "datetime", all.x = TRUE)
  
  solar[[i]] <- #make sure df is ordered by datetime
    arrange(solar[[i]], datetime)
  
  #Add time shifted values for linear interpolation (filltype = INTERP)
  solar[[i]]$shiftpos1 <- 
    c(NA, solar[[i]][1: (length(solar[[i]]$solar_PREFILL) - 1), "solar_PREFILL"])
  solar[[i]]$shiftneg1 <- 
    c(solar[[i]][2: (length(solar[[i]]$solar_PREFILL)), "solar_PREFILL"], NA)
  
  #fill gaps = 1 h
  solar[[i]]$solar_FILL1 <- 
    ifelse(is.na(solar[[i]]$fill_type) == TRUE,
           solar[[i]]$solar_PREFILL, #if no fill needed, use solar_PREFILL
           ifelse(solar[[i]]$fill_type == "INTERP", 
                  (solar[[i]]$shiftpos1 + solar[[i]]$shiftneg1)/2,
                  solar[[i]]$solar_PREFILL))
  
  #Add time shifted values for 24 h pre/post averaging (filltype = AVG24)
  solar[[i]]$shiftpos24 <- 
    c(rep(NA, times = 24), solar[[i]][1: (length(solar[[i]]$solar_FILL1) - 24), "solar_FILL1"])
  solar[[i]]$shiftneg24 <- 
    c(solar[[i]][25: (length(solar[[i]]$solar_FILL1)), "solar_FILL1"], rep(NA, times = 24))
  
  #fill gaps > 1 h and <= 24 h
  solar[[i]]$solar_FILL1 <- 
    ifelse(is.na(solar[[i]]$fill_type) == TRUE,
           solar[[i]]$solar_FILL1, #if no fill needed, use previously filled value (i.e. QC3)
           ifelse(solar[[i]]$fill_type == "AVG24",
                  (solar[[i]]$shiftpos24 + solar[[i]]$shiftneg24)/2,
                  solar[[i]]$solar_FILL1))
  
  #Run ARIMA code if any ARIMA fills exist
  #May not because nighttime NAs forced to 0 (sun not shining, so sw_in = 0)
  if(length(filter(solar_gap, fill_type == "ARIMA")$solar_FILL1) > 0) {
    solar_gap_ARIMA_l <- #Put all ARIMA filltypes into a list
      split(filter(solar[[i]], fill_type == "ARIMA"), 
            f = filter(solar[[i]], fill_type == "ARIMA")$gap_num)
    
    #Run ARIMAs for each 25-72 h set of missing observations
    for(k in 1:length(solar_gap_ARIMA_l)){
      gap = #calculate gap length of data to be filled
        length(solar_gap_ARIMA_l[[k]]$solar_FILL1)
      
      #Acquire data before and after missing obs of equal length to missing data
      tmp_gap_pre <- 
        subset(solar[[i]], datetime < min(solar_gap_ARIMA_l[[k]]$datetime) &
                 datetime >= (min(solar_gap_ARIMA_l[[k]]$datetime) - gap * 3600))
      tmp_gap_post <- 
        subset(solar[[i]], datetime > max(solar_gap_ARIMA_l[[k]]$datetime) &
                 datetime <= (max(solar_gap_ARIMA_l[[k]]$datetime) + gap * 3600))
      tmp_gap_post <- #reverse post data for backcasting
        arrange(tmp_gap_post, desc(datetime))
      
      #Skip ARIMA if too many missing values in pre or post data
      if(length(subset(tmp_gap_pre, is.na(solar_FILL1))$solar_FILL1) > 1 | 
         length(subset(tmp_gap_post, is.na(solar_FILL1))$solar_FILL1) > 1) {
        print(paste(names(solar[i]), k, "fail"))
        next
      }
      
      #Make forecast and backcast ARIMA predictions
      tmp_gap_pre_fit <- 
        arima(tmp_gap_pre$solar_FILL1, order = c(1,1,1), 
              seasonal = list(order = c(0,0,1), period = 24))
      tmp_gap_pre_forecast <- 
        predict(tmp_gap_pre_fit, n.ahead = gap)
      
      tmp_gap_post_fit <- 
        arima(tmp_gap_post$solar_FILL1, order = c(1,1,1), 
              seasonal = list(order=c(0,0,1),period=24))
      tmp_gap_post_backcast <- 
        predict(tmp_gap_post_fit, n.ahead = gap)
      
      #Calculate the weights to be applied to the forecast and backcast
      wt1 <- ((gap - 1):0)/gap
      wt2 <- (0:(gap - 1))/gap
      
      #Predict missing data using through weighted forecast and backcast
      solar_gap_ARIMA_l[[k]]$solar_FILL1 <-  
        (tmp_gap_pre_forecast$pred * wt1) +
        (rev(tmp_gap_post_backcast$pred)* wt2)
      
      #Remove missing rows from data
      solar[[i]] <- 
        subset(solar[[i]], datetime %in% solar_gap_ARIMA_l[[k]]$datetime == FALSE)
      
      #And add infilled data in its place
      solar[[i]] <- 
        rbind(solar[[i]], solar_gap_ARIMA_l[[k]])
    }
  }
  
  solar[[i]] <- #make sure df is ordered by datetime
    arrange(solar[[i]], datetime)
  
  solar[[i]]$solar_FLAG4 <- 
    ifelse(is.na(solar[[i]]$solar_QC3) == is.na(solar[[i]]$solar_FILL1) ,
           NA,
           4) #flag 4 means filled with same station time series data (concatenate with fill type for more info)
}

#Infilling stats in hours
#Use script format: with(subset(solar[["c1"]], is.na(solar_PREFILL)), tapply(solar_PREFILL, fill_type, length)) and
#with(subset(solar[["c1"]], is.na(solar_FILL1)), tapply(solar_FILL1, fill_type, length))

#C1 (missing obs)
#AVG24 INTERP 
#3934   2096  
#C1 (missing obs after temporal infilling)
#AVG24 
#3391 

#D1 (missing obs)
#AAVG24 INTERP 
#53465   2076 
#D1 (missing obs after temporal infilling)
#AVG24 
#51401  

#SDL (missing obs)
#AVG24 INTERP 
#51344   1855
#SDL (missing obs after temporal infilling)
#AVG24 
#50352 


##############################
##############################
#INFILL variable 2, protocol 2: SOLAR (multi-station regression)
##############################
#Simple infilling protocol for solar
#Develop linear regressions for each month and three hour time block
#There are regressions for when both other stations are or one or the other is reporting
#Gives a total of three regressions (z ~ x + y, z ~ x, z ~ y)
#When no station exists use, climatological mean for that month and time block

#Make a master list for all stations (a list of three lists—one for each stations)
#Each station list has 12 data frames (one for each month)
#Each data frame has 12 columns (3 intercepts, 3 slopes, 3 R2, 1 climatological mean, 1 month and 1 hour cat)
#Each data frame has 8 rows (1 for each 3-hour time block)

#Note: tests showed 1 h time blocks did not fit the data as well as 3 h time blocks
#My hypothesis is it helps smooth the noise caused by clouds
#Using a daily time block created the best fit, but that's likely an artifact of the zero values
#when the sun isn't shining

#Please note the solar data must be transformed in order to better match the assumption of normality
#Raw solar is heavily skewed to the right
#Data are transformed using a power transform
#Guide for effective transformations: Wilks (2011) - Statistical Methods in Atmospheric Science

#Lamda was generated by transforming and finding most normal boxplot spread and Box-Cox procedure
#0.55 seems to be best for all stations
#Future work could use different lamdas and assess sensitivity

######################################################
######################################################
#Note: Upon visual inspection, some Saddle solar entries had to be removed (missed by automatic QC)
#Remove 1991-06-12 through 1991-07-04 10:00
solar[["sdl"]][which(solar[["sdl"]]$datetime %in% seq.POSIXt(as.POSIXct("1991-06-12 01:00", tz = "MST"),
                                                      as.POSIXct("1991-07-04 10:00"), by = "1 hour") & 
                       solar[["sdl"]]$aman == 1), 
               c("solar_QC1", "solar_FLAG1", "solar_QC2", "solar_QC3", "solar_FILL1")] <-   NA
solar[["sdl"]][which(solar[["sdl"]]$datetime %in% seq.POSIXt(as.POSIXct("1991-06-12 01:00", tz = "MST"),
                                                             as.POSIXct("1991-07-04 10:00"), by = "1 hour") & 
                       solar[["sdl"]]$aman == 1), 
               c("solar_FLAG0")] <-   0


#Add lamda value for transform
lamda_trans = 0.55

#add transformed solar data
for(i in 1:length(solar)) {
  solar[[i]]$solar_TRANS <- solar[[i]]$solar_FILL1 ^ lamda_trans
}


#Load plyr (for list manipulation)
#Note, plyr masks mutate, arrange, and other functions from dplyr
library(plyr)

#Load modeest package
#For determining sun above horizon (mode of aman == 1 when above horizon)
library(modeest)

#Name the hour categories and add hour cats and months to the data
for(i in 1:length(solar) ){
  solar[[i]]$hour_cat <- fun_HOURCAT(as.numeric(format(solar[[i]]$datetime, "%H")))
  solar[[i]]$month <- as.numeric(format(solar[[i]]$datetime, "%m"))
}

#Initiate the lists of regression data (one with intercept calculation and one where int = 0)
#In code below int0 refers to intercept forced to 0 regression
solar_regr <- list() #list of three lists (one per site) that hold the data by site and by month
solar_regr2 <- list() #list of three data frames (one per site) with regression info by month and hour

solar_regr_int0 <- list() #list of three lists (one per site) that hold the data by site and by month
solar_regr_int02 <- list() #list of three data frames (one per site) with regression info by month and hour


for(i in 1: n_stations){
  solar_regr[[i]] <- list() #initiate list of data frames
  names(solar_regr)[i] <- #name the data frame by site (e.g. c1, d1, etc.)
    sites[i]
  solar_regr_int0[[i]] <- list() #initiate list of data frames
  names(solar_regr_int0)[i] <- #name the data frame by site (e.g. c1, d1, etc.)
    sites[i]
  
  for(j in 1:12){
    tmp <- #Subset to only month of interest
      lapply(solar, filter, as.numeric(format(datetime, "%m")) == j )#use FILTER, not SUBSET
    tmp_mrg <- #merge stn 1 with stn 2
      merge(tmp[[station_ord[i, "stn1"]]], tmp[[station_ord[i, "stn2"]]], by = "datetime")
    tmp_mrg <- #mrg stns 1 and 2 with stn 3
      merge(tmp_mrg, tmp[[station_ord[i, "stn3"]]], by = "datetime") 
    
    regr1 <- #calculate regression by stns 2 and 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(solar_TRANS.x ~ solar_TRANS.y + solar_TRANS, data = df))
    regr2 <- #calculate regression by stn 2
      dlply(tmp_mrg, "hour_cat", function(df)lm(solar_TRANS.x ~ solar_TRANS.y, data = df))
    regr3 <- #calculate regression by stn 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(solar_TRANS.x ~ solar_TRANS, data = df))
    regr_int01 <- #calculate regression by stns 2 and 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(solar_TRANS.x ~ 0 + solar_TRANS.y + solar_TRANS, data = df))
    regr_int02 <- #calculate regression by stn 2
      dlply(tmp_mrg, "hour_cat", function(df)lm(solar_TRANS.x ~ 0 + solar_TRANS.y, data = df))
    regr_int03 <- #calculate regression by stn 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(solar_TRANS.x ~ 0 + solar_TRANS, data = df))
    
    #Initialize regression data frames in each station's list
    solar_regr[[i]][[j]] <- data.frame(int1 = 0, slope1a = 0, slope1b = 0, rsq1 = 0,
                                      int2 = 0, slope2 = 0, rsq2 = 0,
                                      int3 = 0, slope3 = 0, rsq3 = 0,
                                      mean = 0, month = 0, hour_cat = "x",
                                      stringsAsFactors =  FALSE)
    solar_regr_int0[[i]][[j]] <- data.frame(int1 = 0, slope1a = 0, slope1b = 0, rsq1 = 0,
                                           int2 = 0, slope2 = 0, rsq2 = 0,
                                           int3 = 0, slope3 = 0, rsq3 = 0,
                                           mean = 0, month = 0, hour_cat = "x",
                                           stringsAsFactors = FALSE)
    for(k in 1:8){ #run for each 3 h time block
      
       if( mlv(subset(tmp[[i]], hour_cat == hour_cats[k])$aman, method = "mfv")$M == 0){
        #If the sun is down (aman == 0), then set int to 0 and slopes to 1
        #R cannot calculate regression for equal values (i.e., 0 as predicted by 0 and 0 doesn't work)
        #Set mean to mean for that month and time period as normal
         solar_regr[[i]][[j]][k, 1:12] <- c(0, 1, 1, NA, 0, 1, NA, 0, 1, NA, 
                                        mean(subset(tmp[[i]], hour_cat == hour_cats[k])$solar_FILL1, na.rm = TRUE),
                                        j)
         solar_regr[[i]][[j]][k, 13] <- hour_cats[k] #hour_cat entered separately or else turns all columns to chr
         solar_regr_int0[[i]][[j]][k, 1:12] <- c(0, 1, 1, NA, 0, 1, NA, 0, 1, NA, 
                                        mean(subset(tmp[[i]], hour_cat == hour_cats[k])$solar_FILL1, na.rm = TRUE),
                                        j)
         solar_regr_int0[[i]][[j]][k, 13] <- hour_cats[k] #hour_cat entered separately or else turns all columns to chr
        } else{
          solar_regr[[i]][[j]][k, "int1"] = summary(regr1[[k]])$coefficients[1,1]
          solar_regr[[i]][[j]][k, "slope1a"] = summary(regr1[[k]])$coefficients[2,1]
          solar_regr[[i]][[j]][k, "slope1b"] = summary(regr1[[k]])$coefficients[3,1]
          solar_regr[[i]][[j]][k, "rsq1"] = summary(regr1[[k]])$r.squared
          solar_regr[[i]][[j]][k, "int2"] = summary(regr2[[k]])$coefficients[1,1]
          solar_regr[[i]][[j]][k, "slope2"] = summary(regr2[[k]])$coefficients[2,1]
          solar_regr[[i]][[j]][k, "rsq2"] = summary(regr2[[k]])$r.squared
          solar_regr[[i]][[j]][k, "int3"] = summary(regr3[[k]])$coefficients[1,1]
          solar_regr[[i]][[j]][k, "slope3"] = summary(regr3[[k]])$coefficients[2,1]
          solar_regr[[i]][[j]][k, "rsq3"] = summary(regr3[[k]])$r.squared
          #Climatological mean for month and time block
          solar_regr[[i]][[j]][k, "mean"] = mean(subset(tmp[[i]], hour_cat == hour_cats[k])$solar_FILL1, na.rm = TRUE)
          solar_regr[[i]][[j]][k, "month"] = j
          solar_regr[[i]][[j]][k, "hour_cat"] = hour_cats[k]
          
          #regression info for int0
          #note the row for the coefficients changes
          #intercept usually takes slot [1,1], now slope takes [1,1]
          solar_regr_int0[[i]][[j]][k, "int1"] = 0
          solar_regr_int0[[i]][[j]][k, "slope1a"] = summary(regr_int01[[k]])$coefficients[1,1]
          solar_regr_int0[[i]][[j]][k, "slope1b"] = summary(regr_int01[[k]])$coefficients[2,1]
          solar_regr_int0[[i]][[j]][k, "rsq1"] = summary(regr_int01[[k]])$r.squared
          solar_regr_int0[[i]][[j]][k, "int2"] = 0
          solar_regr_int0[[i]][[j]][k, "slope2"] = summary(regr_int02[[k]])$coefficients[1,1]
          solar_regr_int0[[i]][[j]][k, "rsq2"] = summary(regr_int02[[k]])$r.squared
          solar_regr_int0[[i]][[j]][k, "int3"] = 0
          solar_regr_int0[[i]][[j]][k, "slope3"] = summary(regr_int03[[k]])$coefficients[1,1]
          solar_regr_int0[[i]][[j]][k, "rsq3"] = summary(regr_int03[[k]])$r.squared
          #Climatological mean for month and time block
          solar_regr_int0[[i]][[j]][k, "mean"] = mean(subset(tmp[[i]], hour_cat == hour_cats[k])$solar_FILL1, na.rm = TRUE)
          solar_regr_int0[[i]][[j]][k, "month"] = j
          solar_regr_int0[[i]][[j]][k, "hour_cat"] = hour_cats[k]
      }
    }
  }
  solar_regr2[[i]] <- #Put all regressions into one data frame for each site
    ldply(solar_regr[[i]])
  solar_regr_int02[[i]] <- #Put all regressions into one data frame for each site
    ldply(solar_regr_int0[[i]])
}


#Merge regression coefficients with solar  data and infill
solar_regr_fill <- #initialize list with solar  and regression coefficients
  list()

for (i in 1:length(solar)){
  #in below FILL2 is with calculated intercept, FILL3 is with int = 0
  solar[[i]]$solar_FILL2 <- #make FILL2 = FILL1
    solar[[i]]$solar_FILL1
  solar[[i]]$solar_FLAG5 <- #initiate flagging
    NA
  
  solar[[i]]$solar_FILL3 <- #make FILL3 = FILL1
    solar[[i]]$solar_FILL1
  solar[[i]]$solar_FLAG6 <- #initiate flagging
    NA
  
  solar_regr_fill[[i]] <- #subset to NA values (missing obs)
    subset(solar[[i]], is.na(solar_FILL2))
  solar_regr_fill[[i]]  <- #merge with closest station data
    left_join(solar_regr_fill[[i]], 
              solar[[station_ord[i, "stn2"]]][ , c("datetime", "solar_FILL1", "solar_TRANS")],
              by = "datetime")
  solar_regr_fill[[i]]  <- #merge with second closest station data
    left_join(solar_regr_fill[[i]], 
              solar[[station_ord[i, "stn3"]]][ , c("datetime", "solar_FILL1", "solar_TRANS")],
              by = "datetime")
  #merging both regression data frame will introduce .x and .y into variable names
  #.x = intercept calculated, .y = intercept 0
  solar_regr_fill[[i]]  <- #merge with regression coefficients
    left_join(solar_regr_fill[[i]], 
              solar_regr2[[i]],
              by = c("month", "hour_cat"))
  solar_regr_fill[[i]]  <- #merge with int0 regression coefficients
    left_join(solar_regr_fill[[i]], 
              solar_regr_int02[[i]],
              by = c("month", "hour_cat"))
  
  solar_regr_fill[[i]]$solar_FILL2 <- #infill based on which station is available
    ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == FALSE & 
             is.na(solar_regr_fill[[i]]$solar_FILL1) == FALSE,
           (solar_regr_fill[[i]]$solar_TRANS.y * solar_regr_fill[[i]]$slope1a.x +
              solar_regr_fill[[i]]$solar_TRANS * solar_regr_fill[[i]]$slope1b.x +
              solar_regr_fill[[i]]$int1.x) ^ (1/lamda_trans),
           ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == FALSE & 
                    is.na(solar_regr_fill[[i]]$solar_FILL1) == TRUE,
                  (solar_regr_fill[[i]]$solar_TRANS.y * solar_regr_fill[[i]]$slope2.x +
                     solar_regr_fill[[i]]$int2.x) ^ (1/lamda_trans),
                  ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == TRUE & 
                           is.na(solar_regr_fill[[i]]$solar_FILL1) == FALSE,
                         (solar_regr_fill[[i]]$solar_TRANS * solar_regr_fill[[i]]$slope3.x +
                            solar_regr_fill[[i]]$int3.x) ^ (1/lamda_trans),
                         solar_regr_fill[[i]]$mean.x)))
  solar_regr_fill[[i]]$solar_FLAG5 <- #FLAG based on which station is available
    ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == FALSE & 
             is.na(solar_regr_fill[[i]]$solar_FILL1) == FALSE,
           5, #filled with both stations
           ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == FALSE & 
                    is.na(solar_regr_fill[[i]]$solar_FILL1) == TRUE,
                  6, #filled with closest station
                  ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == TRUE & 
                           is.na(solar_regr_fill[[i]]$solar_FILL1) == FALSE,
                         7, #filled with second closest station
                         8))) #filled with mean
  
  solar_regr_fill[[i]]$solar_FILL3 <- #infill based on which station is available
    ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == FALSE & 
             is.na(solar_regr_fill[[i]]$solar_FILL1) == FALSE,
           (solar_regr_fill[[i]]$solar_TRANS.y * solar_regr_fill[[i]]$slope1a.y +
              solar_regr_fill[[i]]$solar_TRANS * solar_regr_fill[[i]]$slope1b.y +
              solar_regr_fill[[i]]$int1.y) ^ (1/lamda_trans),
           ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == FALSE & 
                    is.na(solar_regr_fill[[i]]$solar_FILL1) == TRUE,
                  (solar_regr_fill[[i]]$solar_TRANS.y * solar_regr_fill[[i]]$slope2.y +
                     solar_regr_fill[[i]]$int2.y) ^ (1/lamda_trans),
                  ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == TRUE & 
                           is.na(solar_regr_fill[[i]]$solar_FILL1) == FALSE,
                         (solar_regr_fill[[i]]$solar_TRANS * solar_regr_fill[[i]]$slope3.y +
                            solar_regr_fill[[i]]$int3.y) ^ (1/lamda_trans),
                         solar_regr_fill[[i]]$mean.y)))
  solar_regr_fill[[i]]$solar_FLAG6 <- #FLAG based on which station is available
    ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == FALSE & 
             is.na(solar_regr_fill[[i]]$solar_FILL1) == FALSE,
           9, #filled with both stations, int = 0
           ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == FALSE & 
                    is.na(solar_regr_fill[[i]]$solar_FILL1) == TRUE,
                  10, #filled with closest station, int = 0
                  ifelse(is.na(solar_regr_fill[[i]]$solar_FILL1.y) == TRUE & 
                           is.na(solar_regr_fill[[i]]$solar_FILL1) == FALSE,
                         11, #filled with second closest station, int = 0
                         12))) #filled with mean
  tmp <- #make temporary file for binding
    solar_regr_fill[[i]][, c(1:36)] #use only the columns in solar[[]]
  solar[[i]] <- #remove entries that were filled in this step
    subset(solar[[i]], 
           datetime %in% tmp$datetime == FALSE)
  solar[[i]] <- #append the new, filled entries to the dataframe
    bind_rows(solar[[i]], tmp)
  solar[[i]] <- #make sure df is ordered by datetime
    arrange(solar[[i]], datetime)
  solar[[i]]$flag_FILL1 <- #concatenate fill values for filling
    rowSums(solar[[i]][, c("solar_FLAG4", "solar_FLAG5")], na.rm= TRUE)
  solar[[i]]$flag_FILL1 <- #set 0 to NA in flag
    ifelse(solar[[i]]$flag_FILL1 == 0,
           NA,
           solar[[i]]$flag_FILL1)
  solar[[i]]$flag_FILL2 <- #concatenate fill values for filling
    rowSums(solar[[i]][, c("solar_FLAG4", "solar_FLAG6")], na.rm= TRUE)
  solar[[i]]$flag_FILL2 <- #set 0 to NA in flag
    ifelse(solar[[i]]$flag_FILL2 == 0,
           NA,
           solar[[i]]$flag_FILL2)
}

#remove plyr and modeest
detach("package:plyr", unload=TRUE)
detach("package:modeest", unload=TRUE)


#NOTE: the above code has issues with D1 solar radiation in June 1991, creating 54 NAs
#The bad Saddle solar values seem to be to blame
#I have removed them and re-run the code starting at line 356


#Plot and save filled solar  data
ggplot(solar[["c1"]], aes(datetime, solar_FILL2, color = as.factor(flag_FILL1))) + 
  geom_point() +
  labs(x = "Date", y = expression("Incoming total solar  (MJ "*m^-2*")"), title = "C1 Hourly Solar (QC + Infill 2)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_solar_c1_hr_QC_INFILL2.png")
ggplot(solar[["d1"]], aes(datetime, solar_FILL2, color = as.factor(flag_FILL1))) + 
  geom_point()+
  labs(x = "Date", y = expression("Incoming total solar  (MJ "*m^-2*")"), title = "D1 Hourly Solar (QC + Infill 2)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_solar_d1_hr_QC_INFILL2.png")
ggplot(solar[["sdl"]], aes(datetime, solar_FILL2, color = as.factor(flag_FILL1))) + 
  geom_point() +
  labs(x = "Date", y = expression("Incoming total solar  (MJ "*m^-2*")"), title = "SDL Hourly Solar (QC + Infill 2)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_solar_sdl_hr_QC_INFILL2.png")


ggplot(solar[["c1"]], aes(datetime, solar_FILL3, color = as.factor(flag_FILL2))) + 
  geom_point() +
  labs(x = "Date", y = expression("Incoming total solar  (MJ "*m^-2*")"), title = "C1 Hourly Solar (QC + Infill 3)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_solar_c1_hr_QC_INFILL3.png")
ggplot(solar[["d1"]], aes(datetime, solar_FILL3, color = as.factor(flag_FILL2))) + 
  geom_point()+
  labs(x = "Date", y = expression("Incoming total solar  (MJ "*m^-2*")"), title = "D1 Hourly Solar (QC + Infill 3)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_solar_d1_hr_QC_INFILL3.png")
ggplot(solar[["sdl"]], aes(datetime, solar_FILL3, color = as.factor(flag_FILL2))) + 
  geom_point() +
  labs(x = "Date", y = expression("Incoming total solar  (MJ "*m^-2*")"), title = "SDL Hourly Solar (QC + Infill 3)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_solar_sdl_hr_QC_INFILL3.png")




