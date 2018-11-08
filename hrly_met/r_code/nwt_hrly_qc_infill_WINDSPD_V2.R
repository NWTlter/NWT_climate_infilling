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
#2016-05-31


##############################
##############################
#QC variable 2: WIND SPEED (scalar)
#Vector is equal to or lower than scalar (incorporates direction in calculation)
##############################

#First run the initialization script
#Gathers data, loads packages, writes functions, etc.
setwd("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/")
source(file = "nwt_hrly_qc_infill_RUNFIRST.R")

#Create list with wind data from each site
wind <- lapply(X = hrly_data, FUN = select, 
               wind_spd_scalar, datetime, site, doy)

#Apply first QC protocol
#Remove anomalous values on max/min threshold

wind_thresh <- c(15,35,30) #max wind speed thresholds for C1, D1, and SDL, respectively

for (i in 1:length(wind)){
  wind[[i]]$wind_QC1 <- ifelse(wind[[i]]$wind_spd_scalar <= wind_thresh[i] & wind[[i]]$wind_spd_scalar >= 0,
                               wind[[i]]$wind_spd_scalar,
                               NA)
  wind[[i]]$wind_FLAG0 <- ifelse(is.na(wind[[i]]$wind_spd_scalar),
                                 0,
                                 NA) #flag 0 means data missing from original record
  wind[[i]]$wind_FLAG1 <- ifelse(is.na(wind[[i]]$wind_spd_scalar) == is.na(wind[[i]]$wind_QC1),
                                 NA,
                                 1) #flag 1 means wind outside of max/min threshold
}


#Apply second QC protocol
#Remove anomalous values on rate of change threshold (±10 m/s per h)

for (i in 1:length(wind)){
  wind[[i]]$diff_1h <- c(0, diff(wind[[i]]$wind_spd_scalar))
  
  wind[[i]]$wind_QC2 <- ifelse(wind[[i]]$diff_1h <= 10 & wind[[i]]$diff_1h >= -10,
                               wind[[i]]$wind_QC1,
                               NA)
  
  wind[[i]]$wind_FLAG2 <- ifelse(is.na(wind[[i]]$wind_QC1) == is.na(wind[[i]]$wind_QC2),
                                 NA,
                                 2) #flag 2 means wind outside rate-of-change threshold
}

#Apply third quality control protocol
#wind sensor is stuck
for (i in 1: length(wind)){
  #Create shifted wind columns
  #If the shifted values equal one another, then wind is stuck
  wind[[i]]$shift1 <- c(NA, wind[[i]][1: (length(wind[[i]]$wind_spd_scalar) - 1), "wind_spd_scalar"])
  wind[[i]]$shift2 <- c(NA, NA, wind[[i]][1: (length(wind[[i]]$wind_spd_scalar) - 2), "wind_spd_scalar"])
  wind[[i]]$shift3 <- c(NA, NA, NA, wind[[i]][1: (length(wind[[i]]$wind_spd_scalar) - 3), "wind_spd_scalar"])
  
  wind[[i]]$wind_QC3 <- ifelse(wind[[i]]$wind_spd_scalar ==  wind[[i]]$shift1 &
                                 wind[[i]]$shift1 ==  wind[[i]]$shift2 &
                                 wind[[i]]$shift2 ==  wind[[i]]$shift3,
                               NA,
                               wind[[i]]$wind_QC2)
  
  wind[[i]][1, "wind_QC3"] = #Replace first value with QC2 (artificial NA at t = 0)
    wind[[i]][1, "wind_QC2"] 
  
  wind[[i]]$wind_FLAG3 <- ifelse(is.na(wind[[i]]$wind_QC2) == is.na(wind[[i]]$wind_QC3) ,
                                 NA,
                                 3) #flag 3 means wind sensor stuck
  
}

##############################
##############################
#INFILL variable 2, protocol 1: WIND SPEED SCALAR (univariate time series infill, as in MicroMet)
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

for (i in 1:length(wind)){
  wind_gap <- #subset to all missing observations
    filter(wind[[i]], is.na(wind_QC3))
  
  wind_gap$time_diff <- #calculate time diff in hours
    c(0, diff.POSIXt(wind_gap$datetime)) 
  
  tmp <-
    head(diff.POSIXt(wind_gap$datetime))
  
  if(attr(tmp, which = "units") == "secs") {
    #Divide time_diff by 3600 if differences are in seconds not hours
    #That diff.posix units can't be controlled is a known error in R
    wind_gap$time_diff <- wind_gap$time_diff / 3600
  }
  
  #assign dummy gap number and initiate gap counter (for numbering gaps)
  wind_gap$gap_num <- 0
  gap_counter = 1
  
  for (j in 1:length(wind_gap$time_diff)){
    if (wind_gap[j, "time_diff"] > 1) { 
      gap_counter = #increase gap counter by 1 if gap longer than 1 h
        gap_counter + 1
      wind_gap[j, "gap_num"] = #assign new gap#
        gap_counter
    } else {
      wind_gap[j, "gap_num"] = #assign gap number
        gap_counter
    }
  }
  
  wind_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
    wind_gap %>% 
    group_by(gap_num) %>% 
    mutate(gap_length = length(wind_QC3))
  
  wind_gap$fill_type <- #assign gap categorization
    fun_FILLTYPE(wind_gap$gap_length)
  
  wind[[i]] <- #Merge gap information with complete dataset
    merge(wind[[i]], wind_gap[ , c("datetime", "gap_num", "gap_length", "fill_type")],
          by = "datetime", all.x = TRUE)
  
  wind[[i]] <- #make sure df is ordered by datetime
    arrange(wind[[i]], datetime)
  
  #Add time shifted values for linear interpolation (filltype = INTERP)
  wind[[i]]$shiftpos1 <- 
    c(NA, wind[[i]][1: (length(wind[[i]]$wind_QC3) - 1), "wind_QC3"])
  wind[[i]]$shiftneg1 <- 
    c(wind[[i]][2: (length(wind[[i]]$wind_QC3)), "wind_QC3"], NA)
  
  #fill gaps = 1 h
  wind[[i]]$wind_FILL1 <- 
    ifelse(is.na(wind[[i]]$fill_type) == TRUE,
           wind[[i]]$wind_QC3, #if no fill needed, use wind_QC3
           ifelse(wind[[i]]$fill_type == "INTERP", 
                  (wind[[i]]$shiftpos1 + wind[[i]]$shiftneg1)/2,
                  wind[[i]]$wind_QC3))
  
  #Add time shifted values for 24 h pre/post averaging (filltype = AVG24)
  wind[[i]]$shiftpos24 <- 
    c(rep(NA, times = 24), wind[[i]][1: (length(wind[[i]]$wind_FILL1) - 24), "wind_FILL1"])
  wind[[i]]$shiftneg24 <- 
    c(wind[[i]][25: (length(wind[[i]]$wind_FILL1)), "wind_FILL1"], rep(NA, times = 24))
  
  #fill gaps > 1 h and <= 24 h
  wind[[i]]$wind_FILL1 <- 
    ifelse(is.na(wind[[i]]$fill_type) == TRUE,
           wind[[i]]$wind_FILL1, #if no fill needed, use previously filled value (i.e. QC3)
           ifelse(wind[[i]]$fill_type == "AVG24",
                  (wind[[i]]$shiftpos24 + wind[[i]]$shiftneg24)/2,
                  wind[[i]]$wind_FILL1))
  
  
  wind_gap_ARIMA_l <- #Put all ARIMA filltypes into a list
    split(filter(wind[[i]], fill_type == "ARIMA"), 
          f = filter(wind[[i]], fill_type == "ARIMA")$gap_num)
  
  #Run ARIMAs for each 25-72 h set of missing observations
  for(k in 1:length(wind_gap_ARIMA_l)){
    gap = #calculate gap length of data to be filled
      length(wind_gap_ARIMA_l[[k]]$wind_FILL1)
    
    #Acquire data before and after missing obs of equal length to missing data
    tmp_gap_pre <- 
      subset(wind[[i]], datetime < min(wind_gap_ARIMA_l[[k]]$datetime) &
               datetime >= (min(wind_gap_ARIMA_l[[k]]$datetime) - gap * 3600))
    tmp_gap_post <- 
      subset(wind[[i]], datetime > max(wind_gap_ARIMA_l[[k]]$datetime) &
               datetime <= (max(wind_gap_ARIMA_l[[k]]$datetime) + gap * 3600))
    tmp_gap_post <- #reverse post data for backcasting
      arrange(tmp_gap_post, desc(datetime))
    
    #Skip ARIMA if too many missing values in pre or post data
    if(length(subset(tmp_gap_pre, is.na(wind_FILL1))$wind_FILL1) > 1 | 
       length(subset(tmp_gap_post, is.na(wind_FILL1))$wind_FILL1) > 1) {
      print(paste(names(wind[i]), k, "fail"))
      next
    }
    
    #Make forecast and backcast ARIMA predictions
    tmp_gap_pre_fit <- 
      arima(tmp_gap_pre$wind_FILL1, order = c(1,1,1), 
            seasonal = list(order = c(0,0,1), period = 24))
    tmp_gap_pre_forecast <- 
      predict(tmp_gap_pre_fit, n.ahead = gap)
    
    tmp_gap_post_fit <- 
      arima(tmp_gap_post$wind_FILL1, order = c(1,1,1), 
            seasonal = list(order=c(0,0,1),period=24))
    tmp_gap_post_backcast <- 
      predict(tmp_gap_post_fit, n.ahead = gap)
    
    #Calculate the weights to be applied to the forecast and backcast
    wt1 <- ((gap - 1):0)/gap
    wt2 <- (0:(gap - 1))/gap
    
    #Predict missing data using through weighted forecast and backcast
    wind_gap_ARIMA_l[[k]]$wind_FILL1 <-  
      (tmp_gap_pre_forecast$pred * wt1) +
      (rev(tmp_gap_post_backcast$pred)* wt2)
    
    #Remove missing rows from data
    wind[[i]] <- 
      subset(wind[[i]], datetime %in% wind_gap_ARIMA_l[[k]]$datetime == FALSE)
    
    #And add infilled data in its place
    wind[[i]] <- 
      rbind(wind[[i]], wind_gap_ARIMA_l[[k]])
  }
  wind[[i]] <- #make sure df is ordered by datetime
    arrange(wind[[i]], datetime)
  
  wind[[i]]$wind_FLAG4 <- 
    ifelse(is.na(wind[[i]]$wind_QC3) == is.na(wind[[i]]$wind_FILL1) ,
           NA,
           4) #flag 4 means filled with same station time series data (concatenate with fill type for more info)
}

#The following ARIMAs filled because of missing data:
# "c1 1 fail"
# "c1 3 fail"
# "c1 5 fail"
# "c1 10 fail"
# "c1 18 fail"
# "d1 1 fail"
# "d1 2 fail"
# "d1 7 fail"
# "d1 8 fail"
# "d1 10 fail"
# "d1 12 fail"
# "d1 14 fail"
# "d1 15 fail"
# "d1 16 fail"
# "d1 17 fail"
# "d1 18 fail"
# "d1 19 fail"
# "d1 20 fail"
# "d1 21 fail"
# "d1 22 fail"
# "d1 23 fail"
# "d1 24 fail"
# "d1 25 fail"
# "d1 26 fail"
# "d1 27 fail"
# "d1 28 fail"
# "d1 29 fail"
# "d1 30 fail"
# "d1 31 fail"
# "d1 32 fail"
# "d1 33 fail"
# "d1 34 fail"
# "d1 35 fail"
# "d1 36 fail"
# "d1 37 fail"
# "d1 38 fail"
# "d1 39 fail"
# "d1 40 fail"
# "d1 41 fail"
# "d1 42 fail"
# "d1 43 fail"
# "d1 44 fail"
# "d1 45 fail"
# "sdl 2 fail"
# "sdl 3 fail"
# "sdl 4 fail"
# "sdl 5 fail"
# "sdl 6 fail"
# "sdl 8 fail"

#Infilling stats in hours
#Use script format: with(subset(wind[["c1"]], is.na(wind_QC3)), tapply(wind_QC3, fill_type, length)) and
#with(subset(wind[["c1"]], is.na(wind_QC3)), tapply(wind_QC3, fill_type, length))

#C1 (missing obs)
#ARIMA  AVG24 INTERP   REGR 
#558    884     32   6150  
#C1 (missing obs after temporal infilling)
#ARIMA AVG24  REGR 
#162   549  6150

#D1 (missing obs)
#ARIMA  AVG24 INTERP   REGR 
#2098   1883    102  24020 
#D1 (missing obs after temporal infilling)
#ARIMA AVG24  REGR 
#1800  1331 24020  

#SDL (missing obs)
#ARIMA  AVG24 INTERP   REGR 
# 501    781    107  11199 
#SDL (missing obs after temporal infilling)
#ARIMA AVG24  REGR 
#256   334 11199


##############################
##############################
#INFILL variable 2, protocol 2: Wind Speed (multi-station regression)
##############################
#Simple infilling protocol for Wind Speed
#Develop linear regressions for each month and three hour time block
#There are regressions for when both other stations are or one or the other is reporting
#Gives a total of three regressions (z ~ x + y, z ~ x, z ~ y)
#When no station exists use, climatological mean for that month and time block

#Make a master list for all stations (a list of three lists—one for each stations)
#Each station list has 12 data frames (one for each month)
#Each data frame has 12 columns (3 intercepts, 3 slopes, 3 R2, 1 climatological mean, 1 month and 1 hour cat)
#Each data frame has 8 rows (1 for each 3-hour time block)

#Please note the wind speed must be transformed in order to better match the assumption of normality
#Raw wind speed is heavily skewed to the right
#Data are transformed using a power transform
#Guide for effective transformations: Wilks (2011) - Statistical Methods in Atmospheric Science

#Lamdas were generated by transforming and finding most normal boxplot spread
#O.35 is the best lamda for C1
#0.5 is the best lamda for D1 and SDL

#Note, it is not certain whether site-specific lamdas can be used
#This may cause issues when back-transforming
#So I am using a compromise lamda of 0.45
#Future work could use different lamdas and assess sensitivity

#Box Cox plots show similar but different values

#Add lamda value for transform
lamda_trans = 0.45

#add transformed wind data
for(i in 1:length(wind)) {
  wind[[i]]$wind_TRANS <- wind[[i]]$wind_FILL1 ^ lamda_trans
}


#Load plyr (for list manipulation)
#Note, plyr masks mutate, arrange, and other functions from dplyr
library(plyr)

#Name the hour categories and add hour cats and months to the data
for(i in 1:length(wind) ){
  wind[[i]]$hour_cat <- fun_HOURCAT(as.numeric(format(wind[[i]]$datetime, "%H")))
  wind[[i]]$month <- as.numeric(format(wind[[i]]$datetime, "%m"))
}

#Initiate the lists of regression data (one with intercept calculation and one where int = 0)
#In code below int0 refers to intercept forced to 0 regression
wind_regr <- list() #list of three lists (one per site) that hold the data by site and by month
wind_regr2 <- list() #list of three data frames (one per site) with regression info by month and hour

wind_regr_int0 <- list() #list of three lists (one per site) that hold the data by site and by month
wind_regr_int02 <- list() #list of three data frames (one per site) with regression info by month and hour


for(i in 1: n_stations){
  wind_regr[[i]] <- list() #initiate list of data frames
  names(wind_regr)[i] <- #name the data frame by site (e.g. c1, d1, etc.)
    sites[i]
  wind_regr_int0[[i]] <- list() #initiate list of data frames
  names(wind_regr_int0)[i] <- #name the data frame by site (e.g. c1, d1, etc.)
    sites[i]
  
  for(j in 1:12){
    tmp <- #Subset to only month of interest
      lapply(wind, filter, as.numeric(format(datetime, "%m")) == j )#use FILTER, not SUBSET
    tmp_mrg <- #merge stn 1 with stn 2
      merge(tmp[[station_ord[i, "stn1"]]], tmp[[station_ord[i, "stn2"]]], by = "datetime")
    tmp_mrg <- #mrg stns 1 and 2 with stn 3
      merge(tmp_mrg, tmp[[station_ord[i, "stn3"]]], by = "datetime") 
    regr1 <- #calculate regression by stns 2 and 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(wind_TRANS.x ~ wind_TRANS.y + wind_TRANS, data = df))
    regr2 <- #calculate regression by stn 2
      dlply(tmp_mrg, "hour_cat", function(df)lm(wind_TRANS.x ~ wind_TRANS.y, data = df))
    regr3 <- #calculate regression by stn 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(wind_TRANS.x ~ wind_TRANS, data = df))
    regr_int01 <- #calculate regression by stns 2 and 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(wind_TRANS.x ~ 0 + wind_TRANS.y + wind_TRANS, data = df))
    regr_int02 <- #calculate regression by stn 2
      dlply(tmp_mrg, "hour_cat", function(df)lm(wind_TRANS.x ~ 0 + wind_TRANS.y, data = df))
    regr_int03 <- #calculate regression by stn 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(wind_TRANS.x ~ 0 + wind_TRANS, data = df))
    
    #Initialize regression data frames in each station's list
    wind_regr[[i]][[j]] <- data.frame(int1 = 0, slope1a = 0, slope1b = 0, rsq1 = 0,
                                      int2 = 0, slope2 = 0, rsq2 = 0,
                                      int3 = 0, slope3 = 0, rsq3 = 0)
    wind_regr_int0[[i]][[j]] <- data.frame(int1 = 0, slope1a = 0, slope1b = 0, rsq1 = 0,
                                      int2 = 0, slope2 = 0, rsq2 = 0,
                                      int3 = 0, slope3 = 0, rsq3 = 0)
    for(k in 1:8){ #run for each 3 h time block
      wind_regr[[i]][[j]][k, "int1"] = summary(regr1[[k]])$coefficients[1,1]
      wind_regr[[i]][[j]][k, "slope1a"] = summary(regr1[[k]])$coefficients[2,1]
      wind_regr[[i]][[j]][k, "slope1b"] = summary(regr1[[k]])$coefficients[3,1]
      wind_regr[[i]][[j]][k, "rsq1"] = summary(regr1[[k]])$r.squared
      wind_regr[[i]][[j]][k, "int2"] = summary(regr2[[k]])$coefficients[1,1]
      wind_regr[[i]][[j]][k, "slope2"] = summary(regr2[[k]])$coefficients[2,1]
      wind_regr[[i]][[j]][k, "rsq2"] = summary(regr2[[k]])$r.squared
      wind_regr[[i]][[j]][k, "int3"] = summary(regr3[[k]])$coefficients[1,1]
      wind_regr[[i]][[j]][k, "slope3"] = summary(regr3[[k]])$coefficients[2,1]
      wind_regr[[i]][[j]][k, "rsq3"] = summary(regr3[[k]])$r.squared
      #Climatological mean for month and time block
      wind_regr[[i]][[j]][k, "mean"] = mean(subset(tmp[[i]], hour_cat == hour_cats[k])$wind_FILL1, na.rm = TRUE)
      wind_regr[[i]][[j]][k, "month"] = j
      wind_regr[[i]][[j]][k, "hour_cat"] = hour_cats[k]
      
      #regression info for int0
      #note the row for the coefficients changes
      #intercept usually takes slot [1,1], now slope takes [1,1]
      wind_regr_int0[[i]][[j]][k, "int1"] = 0
      wind_regr_int0[[i]][[j]][k, "slope1a"] = summary(regr_int01[[k]])$coefficients[1,1]
      wind_regr_int0[[i]][[j]][k, "slope1b"] = summary(regr_int01[[k]])$coefficients[2,1]
      wind_regr_int0[[i]][[j]][k, "rsq1"] = summary(regr_int01[[k]])$r.squared
      wind_regr_int0[[i]][[j]][k, "int2"] = 0
      wind_regr_int0[[i]][[j]][k, "slope2"] = summary(regr_int02[[k]])$coefficients[1,1]
      wind_regr_int0[[i]][[j]][k, "rsq2"] = summary(regr_int02[[k]])$r.squared
      wind_regr_int0[[i]][[j]][k, "int3"] = 0
      wind_regr_int0[[i]][[j]][k, "slope3"] = summary(regr_int03[[k]])$coefficients[1,1]
      wind_regr_int0[[i]][[j]][k, "rsq3"] = summary(regr_int03[[k]])$r.squared
      #Climatological mean for month and time block
      wind_regr_int0[[i]][[j]][k, "mean"] = mean(subset(tmp[[i]], hour_cat == hour_cats[k])$wind_FILL1, na.rm = TRUE)
      wind_regr_int0[[i]][[j]][k, "month"] = j
      wind_regr_int0[[i]][[j]][k, "hour_cat"] = hour_cats[k]
    }
  }
  wind_regr2[[i]] <- #Put all regressions into one data frame for each site
    ldply(wind_regr[[i]])
  wind_regr_int02[[i]] <- #Put all regressions into one data frame for each site
    ldply(wind_regr_int0[[i]])
}


#Merge regression coefficients with Wind Speed data and infill
wind_regr_fill <- #initialize list with Wind Speed and regression coefficients
  list()

#QC one more time and make sure no wind speeds are negative
#Could be caused by ARIMA
for (i in 1:length(wind)){
  wind[[i]]$wind_FILL1 <- ifelse(wind[[i]]$wind_FILL1 < 0,
                                 NA,
                                 wind[[i]]$wind_FILL1)
}

for (i in 1:length(wind)){
  #in below FILL2 is with calculated intercept, FILL3 is with int = 0
  wind[[i]]$wind_FILL2 <- #make FILL2 = FILL1
    wind[[i]]$wind_FILL1
  wind[[i]]$wind_FLAG5 <- #initiate flagging
    NA
  
  wind[[i]]$wind_FILL3 <- #make FILL3 = FILL1
    wind[[i]]$wind_FILL1
  wind[[i]]$wind_FLAG6 <- #initiate flagging
    NA
  
  wind_regr_fill[[i]] <- #subset to NA values (missing obs)
    subset(wind[[i]], is.na(wind_FILL2))
  wind_regr_fill[[i]]  <- #merge with closest station data
    left_join(wind_regr_fill[[i]], 
              wind[[station_ord[i, "stn2"]]][ , c("datetime", "wind_FILL1", "wind_TRANS")],
              by = "datetime")
  wind_regr_fill[[i]]  <- #merge with second closest station data
    left_join(wind_regr_fill[[i]], 
              wind[[station_ord[i, "stn3"]]][ , c("datetime", "wind_FILL1", "wind_TRANS")],
              by = "datetime")
  #merging both regression data frame will introduce .x and .y into variable names
  #.x = intercept calculated, .y = intercept 0
  wind_regr_fill[[i]]  <- #merge with regression coefficients
    left_join(wind_regr_fill[[i]], 
              wind_regr2[[i]],
              by = c("month", "hour_cat"))
  wind_regr_fill[[i]]  <- #merge with int0 regression coefficients
    left_join(wind_regr_fill[[i]], 
              wind_regr_int02[[i]],
              by = c("month", "hour_cat"))
  
  wind_regr_fill[[i]]$wind_FILL2 <- #infill based on which station is available
    ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == FALSE & 
             is.na(wind_regr_fill[[i]]$wind_FILL1) == FALSE,
           (wind_regr_fill[[i]]$wind_TRANS.y * wind_regr_fill[[i]]$slope1a.x +
             wind_regr_fill[[i]]$wind_TRANS * wind_regr_fill[[i]]$slope1b.x +
             wind_regr_fill[[i]]$int1.x) ^ (1/lamda_trans),
           ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == FALSE & 
                    is.na(wind_regr_fill[[i]]$wind_FILL1) == TRUE,
                  (wind_regr_fill[[i]]$wind_TRANS.y * wind_regr_fill[[i]]$slope2.x +
                    wind_regr_fill[[i]]$int2.x) ^ (1/lamda_trans),
                  ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == TRUE & 
                           is.na(wind_regr_fill[[i]]$wind_FILL1) == FALSE,
                         (wind_regr_fill[[i]]$wind_TRANS * wind_regr_fill[[i]]$slope3.x +
                           wind_regr_fill[[i]]$int3.x) ^ (1/lamda_trans),
                         wind_regr_fill[[i]]$mean.x)))
  wind_regr_fill[[i]]$wind_FLAG5 <- #FLAG based on which station is available
    ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == FALSE & 
             is.na(wind_regr_fill[[i]]$wind_FILL1) == FALSE,
           5, #filled with both stations
           ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == FALSE & 
                    is.na(wind_regr_fill[[i]]$wind_FILL1) == TRUE,
                  6, #filled with closest station
                  ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == TRUE & 
                           is.na(wind_regr_fill[[i]]$wind_FILL1) == FALSE,
                         7, #filled with second closest station
                         8))) #filled with mean
  
  wind_regr_fill[[i]]$wind_FILL3 <- #infill based on which station is available
    ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == FALSE & 
             is.na(wind_regr_fill[[i]]$wind_FILL1) == FALSE,
           (wind_regr_fill[[i]]$wind_TRANS.y * wind_regr_fill[[i]]$slope1a.y +
              wind_regr_fill[[i]]$wind_TRANS * wind_regr_fill[[i]]$slope1b.y +
              wind_regr_fill[[i]]$int1.y) ^ (1/lamda_trans),
           ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == FALSE & 
                    is.na(wind_regr_fill[[i]]$wind_FILL1) == TRUE,
                  (wind_regr_fill[[i]]$wind_TRANS.y * wind_regr_fill[[i]]$slope2.y +
                     wind_regr_fill[[i]]$int2.y) ^ (1/lamda_trans),
                  ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == TRUE & 
                           is.na(wind_regr_fill[[i]]$wind_FILL1) == FALSE,
                         (wind_regr_fill[[i]]$wind_TRANS * wind_regr_fill[[i]]$slope3.y +
                            wind_regr_fill[[i]]$int3.y) ^ (1/lamda_trans),
                         wind_regr_fill[[i]]$mean.y)))
  wind_regr_fill[[i]]$wind_FLAG6 <- #FLAG based on which station is available
    ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == FALSE & 
             is.na(wind_regr_fill[[i]]$wind_FILL1) == FALSE,
           9, #filled with both stations, int = 0
           ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == FALSE & 
                    is.na(wind_regr_fill[[i]]$wind_FILL1) == TRUE,
                  10, #filled with closest station, int = 0
                  ifelse(is.na(wind_regr_fill[[i]]$wind_FILL1.y) == TRUE & 
                           is.na(wind_regr_fill[[i]]$wind_FILL1) == FALSE,
                         11, #filled with second closest station, int = 0
                         12))) #filled with mean
  tmp <- #make temporary file for binding
    wind_regr_fill[[i]][, c(1:31)] #use only the columns in wind[[]]
  wind[[i]] <- #remove entries that were filled in this step
    subset(wind[[i]], 
           datetime %in% tmp$datetime == FALSE)
  wind[[i]] <- #append the new, filled entries to the dataframe
    bind_rows(wind[[i]], tmp)
  wind[[i]] <- #make sure df is ordered by datetime
    arrange(wind[[i]], datetime)
  wind[[i]]$flag_FILL1 <- #concatenate fill values for filling
    rowSums(wind[[i]][, c("wind_FLAG4", "wind_FLAG5")], na.rm= TRUE)
  wind[[i]]$flag_FILL1 <- #set 0 to NA in flag
    ifelse(wind[[i]]$flag_FILL1 == 0,
           NA,
           wind[[i]]$flag_FILL1)
  wind[[i]]$flag_FILL2 <- #concatenate fill values for filling
    rowSums(wind[[i]][, c("wind_FLAG4", "wind_FLAG6")], na.rm= TRUE)
  wind[[i]]$flag_FILL2 <- #set 0 to NA in flag
    ifelse(wind[[i]]$flag_FILL2 == 0,
           NA,
           wind[[i]]$flag_FILL2)
}

#remove plyr
detach("package:plyr", unload=TRUE)


#Plot and save filled Wind Speed data
ggplot(wind[["c1"]], aes(datetime, wind_FILL2, color = as.factor(flag_FILL1))) + 
  geom_point() +
  labs(x = "Date", y = expression("Wind Speed (m "*s^-1*")"), title = "C1 Hourly Wind Speed (QC + Infill 2)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_wind_c1_hr_QC_INFILL2.png")
ggplot(wind[["d1"]], aes(datetime, wind_FILL2, color = as.factor(flag_FILL1))) + 
  geom_point()+
  labs(x = "Date", y = expression("Wind Speed (m "*s^-1*")"), title = "D1 Hourly Wind Speed (QC + Infill 2)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_wind_d1_hr_QC_INFILL2.png")
ggplot(wind[["sdl"]], aes(datetime, wind_FILL2, color = as.factor(flag_FILL1))) + 
  geom_point() +
  labs(x = "Date", y = expression("Wind Speed (m "*s^-1*")"), title = "SDL Hourly Wind Speed (QC + Infill 2)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_wind_sdl_hr_QC_INFILL2.png")


ggplot(wind[["c1"]], aes(datetime, wind_FILL3, color = as.factor(flag_FILL2))) + 
  geom_point() +
  labs(x = "Date", y = expression("Wind Speed (m "*s^-1*")"), title = "C1 Hourly Wind Speed (QC + Infill 3)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_wind_c1_hr_QC_INFILL3.png")
ggplot(wind[["d1"]], aes(datetime, wind_FILL3, color = as.factor(flag_FILL2))) + 
  geom_point()+
  labs(x = "Date", y = expression("Wind Speed (m "*s^-1*")"), title = "D1 Hourly Wind Speed (QC + Infill 3)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_wind_d1_hr_QC_INFILL3.png")
ggplot(wind[["sdl"]], aes(datetime, wind_FILL3, color = as.factor(flag_FILL2))) + 
  geom_point() +
  labs(x = "Date", y = expression("Wind Speed (m "*s^-1*")"), title = "SDL Hourly Wind Speed (QC + Infill 3)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_wind_sdl_hr_QC_INFILL3.png")



