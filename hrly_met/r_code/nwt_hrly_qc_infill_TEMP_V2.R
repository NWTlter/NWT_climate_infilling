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
#2016-05-16

##############################
##############################
#QC variable 1: AIR TEMPERATURE
##############################

#First run the initialization script
#Gathers data, loads packages, writes functions, etc.
setwd("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/")
source(file = "nwt_hrly_qc_infill_RUNFIRST.R")

#Create list with temperature data from each site
temp <- lapply(X = hrly_data, FUN = select, 
               temp, datetime, site, doy)

#Apply first QC protocol
#Remove anomalous values on max/min threshold
#Threshold based on sinusoidal regression on day of year max/min

#Deviation from Meek and Hatfield
#Addition of second harmonic term in the regression
#tmax ~ cos(2*pi*doy/366) + sin(2*pi*doy/366) instead of tmax ~ cos(2*pi*doy/366)
#Provides better fit to data

for (i in 1:length(temp)){
  #Perform sinusoidal regression on daily max and min
  #From each station
  tmax_doy <- data.frame("temp" = as.numeric(with(subset(hrly_data[[i]], temp <= 30), 
                                                  tapply(temp, doy, max, na.rm = TRUE))), 
                         "doy" = 1:366,
                         "name" = "tmax_obs")
  tmax_lm <- lm(temp ~ cos(2*pi*doy/366) + sin(2*pi*doy/366), 
                data = tmax_doy)
  tmax_pred <- data.frame("temp" = fitted(tmax_lm),
                          "doy" = 1:366,
                          "name" = "tmax_fit")
  tmax_pred_plus <- data.frame("temp" = fitted(tmax_lm) + 2.5,
                          "doy" = 1:366,
                          "name" = "tmax_fit_plus2.5")
  tmin_doy <- data.frame("temp" = as.numeric(with(subset(hrly_data[[i]], temp >= -40), 
                                                  tapply(temp, doy, min, na.rm = TRUE))), 
                         "doy" = 1:366,
                         "name" = "tmin_obs")
  tmin_lm <- lm(temp ~ cos(2*pi*doy/366) + sin(2*pi*doy/366), 
                data = tmin_doy)
  tmin_pred <- data.frame("temp" = fitted(tmin_lm),
                          "doy" = 1:366,
                          "name" = "tmin_fit")
  tmin_pred_minus <- data.frame("temp" = fitted(tmin_lm) - 2.5,
                               "doy" = 1:366,
                               "name" = "tmin_fit_minus2.5")
  temp_doy_vert <- rbind(tmax_doy, tmax_pred, tmax_pred_plus,
                         tmin_doy, tmin_pred, tmin_pred_minus)
  
  #Plot the regression and save
  ggplot(temp_doy_vert, aes(doy, temp, color = name, lty = name)) + 
    geom_line(lwd = 1) +
    scale_color_manual(values = c("red", "red4", "red4", "blue", "blue4", "blue4"), name = "") + 
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "solid", "dashed", "dotted"), name = "") +
    labs(x = "Day of year", y = expression("Max. and min. air temperature ("*degree*C*")"))
  ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/", 
         filename = paste0("nwt_tmax_tmin_sin_regr_", names(temp[i]) , ".png"))
  
  #Merge the regressed data with observations
  temp[[i]] <- merge(temp[[i]], tmax_pred_plus[, c("doy", "temp")], by = "doy", all.x = TRUE)
  temp[[i]] <- merge(temp[[i]], tmin_pred_minus[, c("doy", "temp")], by = "doy", all.x = TRUE)
  colnames(temp[[i]]) <- c("doy", "temp", "datetime", "site", "tmax_fit_plus", "tmin_fit_minus")
  
  #Run threshold QC check
  temp[[i]]$temp_QC1 <- ifelse(temp[[i]]$temp <= temp[[i]]$tmax_fit_plus & 
                                 temp[[i]]$temp >= temp[[i]]$tmin_fit_minus,
                               temp[[i]]$temp,
                               NA)
  temp[[i]]$temp_FLAG0 <- ifelse(is.na(temp[[i]]$temp),
                                 0,
                                 NA) #flag 0 means data missing from original record
  temp[[i]]$temp_FLAG1 <- ifelse(is.na(temp[[i]]$temp) == is.na(temp[[i]]$temp_QC1),
                                 NA,
                                 1) #flag 1 means temp outside of max/min threshold
}


#Reorder data by datetime
#Merge procedure orders by DOY
#If items out of order, QC protocols 2 and 3 will not workf
temp <- lapply(X = temp, FUN = arrange, 
               datetime)

#Apply second QC protocol
#Remove anomalous values on rate of change threshold (±8°C per h)
#This is 2°C greater than Meek and Hatfield
#More rapid change in dry, high Rocky Mountains
#Could possibly be higher

for (i in 1:length(temp)){
  temp[[i]]$diff_1h <- c(0, diff(temp[[i]]$temp))
  
  temp[[i]]$temp_QC2 <- ifelse(temp[[i]]$diff_1h <= 8 & temp[[i]]$diff_1h >= -8,
                               temp[[i]]$temp_QC1,
                               NA)
  
  temp[[i]]$temp_FLAG2 <- ifelse(is.na(temp[[i]]$temp_QC1) == is.na(temp[[i]]$temp_QC2),
                                 NA,
                                 2) #flag 2 means temp outside rate-of-change threshold
}

#Apply third quality control protocol
#Temperature sensor is stuck
for (i in 1: length(temp)){
  #Create shifted temperature columns
  #If the shifted values equal one another, then temp is stuck
  temp[[i]]$shift1 <- c(NA, temp[[i]][1: (length(temp[[i]]$temp) - 1), "temp"])
  temp[[i]]$shift2 <- c(NA, NA, temp[[i]][1: (length(temp[[i]]$temp) - 2), "temp"])
  temp[[i]]$shift3 <- c(NA, NA, NA, temp[[i]][1: (length(temp[[i]]$temp) - 3), "temp"])
  
  temp[[i]]$temp_QC3 <- ifelse(temp[[i]]$temp ==  temp[[i]]$shift1 &
                                 temp[[i]]$shift1 ==  temp[[i]]$shift2 &
                                 temp[[i]]$shift2 ==  temp[[i]]$shift3,
                               NA,
                               temp[[i]]$temp_QC2)
  temp[[i]][1, "temp_QC3"] = #Replace first value with QC2
    temp[[i]][1, "temp_QC2"] 
  
  temp[[i]]$temp_FLAG3 <- ifelse(is.na(temp[[i]]$temp_QC2) == is.na(temp[[i]]$temp_QC3) ,
                                 NA,
                                 3) #flag 3 means temp sensor stuck
  
}

##############################
##############################
#INFILL variable 1, protocol 1: AIR TEMPERATURE (univariate time series infill, as in MicroMet)
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

for (i in 1:length(temp)){
  temp_gap <- #subset to all missing observations
    filter(temp[[i]], is.na(temp_QC3))
  
  temp_gap$time_diff <- #calculate time diff in hours
    c(0, diff.POSIXt(temp_gap$datetime)) 
  
  tmp <-
    head(diff.POSIXt(temp_gap$datetime))
  
  if(attr(tmp, which = "units") == "secs") {
    #Divide time_diff by 3600 if differences are in seconds not hours
    #That diff.posix units can't be controlled is a known error in R
    temp_gap$time_diff <- temp_gap$time_diff / 3600
  }
  
  #assign dummy gap number and initiate gap counter (for numbering gaps)
  temp_gap$gap_num <- 0
  gap_counter = 1
  
  for (j in 1:length(temp_gap$time_diff)){
    if (temp_gap[j, "time_diff"] > 1) { 
      gap_counter = #increase gap counter by 1 if gap longer than 1 h
        gap_counter + 1
      temp_gap[j, "gap_num"] = #assign new gap#
        gap_counter
    } else {
      temp_gap[j, "gap_num"] = #assign gap number
        gap_counter
    }
  }
  
  temp_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
    temp_gap %>% 
    group_by(gap_num) %>% 
    mutate(gap_length = length(temp_QC3))
  
  temp_gap$fill_type <- #assign gap categorization
    fun_FILLTYPE(temp_gap$gap_length)
  
  temp[[i]] <- #Merge gap information with complete dataset
    merge(temp[[i]], temp_gap[ , c("datetime", "gap_num", "gap_length", "fill_type")],
          by = "datetime", all.x = TRUE)
  
  temp[[i]] <- #make sure df is ordered by datetime
    arrange(temp[[i]], datetime)
  
  #Add time shifted values for linear interpolation (filltype = INTERP)
  temp[[i]]$shiftpos1 <- 
    c(NA, temp[[i]][1: (length(temp[[i]]$temp_QC3) - 1), "temp_QC3"])
  temp[[i]]$shiftneg1 <- 
    c(temp[[i]][2: (length(temp[[i]]$temp_QC3)), "temp_QC3"], NA)
  
  #fill gaps = 1 h
  temp[[i]]$temp_FILL1 <- 
    ifelse(is.na(temp[[i]]$fill_type) == TRUE,
           temp[[i]]$temp_QC3, #if no fill needed, use temp_QC3
           ifelse(temp[[i]]$fill_type == "INTERP", 
                  (temp[[i]]$shiftpos1 + temp[[i]]$shiftneg1)/2,
                  temp[[i]]$temp_QC3))
  
  #Add time shifted values for 24 h pre/post averaging (filltype = AVG24)
  temp[[i]]$shiftpos24 <- 
    c(rep(NA, times = 24), temp[[i]][1: (length(temp[[i]]$temp_FILL1) - 24), "temp_FILL1"])
  temp[[i]]$shiftneg24 <- 
    c(temp[[i]][25: (length(temp[[i]]$temp_FILL1)), "temp_FILL1"], rep(NA, times = 24))
  
  #fill gaps > 1 h and <= 24 h
  temp[[i]]$temp_FILL1 <- 
    ifelse(is.na(temp[[i]]$fill_type) == TRUE,
           temp[[i]]$temp_FILL1, #if no fill needed, use previously filled value (i.e. QC3)
           ifelse(temp[[i]]$fill_type == "AVG24",
                  (temp[[i]]$shiftpos24 + temp[[i]]$shiftneg24)/2,
                  temp[[i]]$temp_FILL1))
  
  
  temp_gap_ARIMA_l <- #Put all ARIMA filltypes into a list
    split(filter(temp[[i]], fill_type == "ARIMA"), 
          f = filter(temp[[i]], fill_type == "ARIMA")$gap_num)
  
  #Run ARIMAs for each 25-72 h set of missing observations
  for(k in 1:length(temp_gap_ARIMA_l)){
    gap = #calculate gap length of data to be filled
      length(temp_gap_ARIMA_l[[k]]$temp_FILL1)
    
    #Acquire data before and after missing obs of equal length to missing data
    tmp_gap_pre <- 
      subset(temp[[i]], datetime < min(temp_gap_ARIMA_l[[k]]$datetime) &
               datetime >= (min(temp_gap_ARIMA_l[[k]]$datetime) - gap * 3600))
    tmp_gap_post <- 
      subset(temp[[i]], datetime > max(temp_gap_ARIMA_l[[k]]$datetime) &
               datetime <= (max(temp_gap_ARIMA_l[[k]]$datetime) + gap * 3600))
    tmp_gap_post <- #reverse post data for backcasting
      arrange(tmp_gap_post, desc(datetime))
    
    #Skip ARIMA if too many missing values in pre or post data
    if(length(subset(tmp_gap_pre, is.na(temp_FILL1))$temp_FILL1) > 1 | 
       length(subset(tmp_gap_post, is.na(temp_FILL1))$temp_FILL1) > 1) {
      print(paste(names(temp[i]), k, "fail"))
      next
    }
    
    #Make forecast and backcast ARIMA predictions
    tmp_gap_pre_fit <- 
      arima(tmp_gap_pre$temp_FILL1, order = c(1,1,1), 
            seasonal = list(order = c(0,0,1), period = 24))
    tmp_gap_pre_forecast <- 
      predict(tmp_gap_pre_fit, n.ahead = gap)
    
    tmp_gap_post_fit <- 
      arima(tmp_gap_post$temp_FILL1, order = c(1,1,1), 
            seasonal = list(order=c(0,0,1),period=24))
    tmp_gap_post_backcast <- 
      predict(tmp_gap_post_fit, n.ahead = gap)
    
    #Calculate the weights to be applied to the forecast and backcast
    wt1 <- ((gap - 1):0)/gap
    wt2 <- (0:(gap - 1))/gap
    
    #Predict missing data using through weighted forecast and backcast
    temp_gap_ARIMA_l[[k]]$temp_FILL1 <-  
      (tmp_gap_pre_forecast$pred * wt1) +
      (rev(tmp_gap_post_backcast$pred)* wt2)
    
    #Remove missing rows from data
    temp[[i]] <- 
      subset(temp[[i]], datetime %in% temp_gap_ARIMA_l[[k]]$datetime == FALSE)
    
    #And add infilled data in its place
    temp[[i]] <- 
      rbind(temp[[i]], temp_gap_ARIMA_l[[k]])
  }
  temp[[i]] <- #make sure df is ordered by datetime
    arrange(temp[[i]], datetime)
  
  temp[[i]]$temp_FLAG4 <- 
    ifelse(is.na(temp[[i]]$temp_QC3) == is.na(temp[[i]]$temp_FILL1) ,
           NA,
           4) #flag 4 means filled with same station time series data (concatenate with fill type for more info)
}

#The following ARIMAs filled because of missing data:
#"c1 2 fail"
#"c1 7 fail"
#"c1 15 fail"
#"d1 1 fail"
#"d1 3 fail"
#"d1 4 fail"
#"d1 5 fail"
#"d1 9 fail"
#"d1 10 fail"
#"d1 13 fail"
#"d1 15 fail"
#"d1 16 fail"
#"d1 20 fail"
#"d1 21 fail"

#Infilling stats in hours

#C1 (missing obs)
#ARIMA  AVG24 INTERP   REGR 
#428   1117    283   6151 
#C1 (missing obs after temporal infilling)
#ARIMA AVG24  REGR 
#99   583  6151 

#D1 (missing obs)
#ARIMA  AVG24 INTERP   REGR 
#918   1796     22  14240 
#D1 (missing obs after temporal infilling)
#ARIMA AVG24  REGR 
#486   861 14240 

#SDL (missing obs)
#ARIMA  AVG24 INTERP   REGR 
# 227    803     37  16159 
#SDL (missing obs after temporal infilling)
#AVG24  REGR 
#163 16159


##############################
##############################
#INFILL variable 1, protocol 2: AIR TEMPERATURE (multi-station regression)
##############################
#Simple infilling protocol for temperature
#Develop linear regressions for each month and three hour time block
#There are regressions for when both other stations are or one or the other is reporting
#Gives a total of three regressions (z ~ x + y, z ~ x, z ~ y)
#When no station exists use, climatological mean for that month and time block

#Make a master list for all stations (a list of three lists—one for each stations)
#Each station list has 12 data frames (one for each month)
#Each data frame has 12 columns (3 intercepts, 3 slopes, 3 R2, 1 climatological mean, 1 month and 1 hour cat)
#Each data frame has 8 rows (1 for each 3-hour time block)

#Load plyr (for list manipulation)
#Note, plyr masks mutate, arrange, and other functions from dplyr
library(plyr)

#Name the hour categories and add hour cats and months to the data

for(i in 1:length(temp) ){
  temp[[i]]$hour_cat <- fun_HOURCAT(as.numeric(format(temp[[i]]$datetime, "%H")))
  temp[[i]]$month <- as.numeric(format(temp[[i]]$datetime, "%m"))
}

#Initiate the two lists of regression data
temp_regr <- list() #list of three lists (one per site) that hold the data by site and by month
temp_regr2 <- list() #list of three data frames (one per site) with regression info by month and hour

for(i in 1: n_stations){
  temp_regr[[i]] <- list() #initiate list of data frames
  names(temp_regr)[i] <- #name the data frame by site (e.g. c1, d1, etc.)
    sites[i]

  for(j in 1:12){
    tmp <- #Subset to only month of interest
      lapply(temp, filter, as.numeric(format(datetime, "%m")) == j )#use FILTER, not SUBSET
    tmp_mrg <- #merge stn 1 with stn 2
      merge(tmp[[station_ord[i, "stn1"]]], tmp[[station_ord[i, "stn2"]]], by = "datetime")
    tmp_mrg <- #mrg stns 1 and 2 with stn 3
      merge(tmp_mrg, tmp[[station_ord[i, "stn3"]]], by = "datetime") 
    regr1 <- #calculate regression by stns 2 and 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(temp_FILL1.x ~ temp_FILL1.y + temp_FILL1, data = df))
    regr2 <- #calculate regression by stn 2
      dlply(tmp_mrg, "hour_cat", function(df)lm(temp_FILL1.x ~ temp_FILL1.y, data = df))
    regr3 <- #calculate regression by stn 3
      dlply(tmp_mrg, "hour_cat", function(df)lm(temp_FILL1.x ~ temp_FILL1, data = df))
    
    #Initialize regression data frames in each station's list
    temp_regr[[i]][[j]] <- data.frame(int1 = 0, slope1a = 0, slope1b = 0, rsq1 = 0,
                                    int2 = 0, slope2 = 0, rsq2 = 0,
                                    int3 = 0, slope3 = 0, rsq3 = 0)
    for(k in 1:8){ #run for each 3 h time block
      temp_regr[[i]][[j]][k, "int1"] = summary(regr1[[k]])$coefficients[1,1]
      temp_regr[[i]][[j]][k, "slope1a"] = summary(regr1[[k]])$coefficients[2,1]
      temp_regr[[i]][[j]][k, "slope1b"] = summary(regr1[[k]])$coefficients[3,1]
      temp_regr[[i]][[j]][k, "rsq1"] = summary(regr1[[k]])$r.squared
      temp_regr[[i]][[j]][k, "int2"] = summary(regr2[[k]])$coefficients[1,1]
      temp_regr[[i]][[j]][k, "slope2"] = summary(regr2[[k]])$coefficients[2,1]
      temp_regr[[i]][[j]][k, "rsq2"] = summary(regr2[[k]])$r.squared
      temp_regr[[i]][[j]][k, "int3"] = summary(regr3[[k]])$coefficients[1,1]
      temp_regr[[i]][[j]][k, "slope3"] = summary(regr3[[k]])$coefficients[2,1]
      temp_regr[[i]][[j]][k, "rsq3"] = summary(regr3[[k]])$r.squared
      #Climatological mean for month and time block
      temp_regr[[i]][[j]][k, "mean"] = mean(subset(tmp[[i]], hour_cat == hour_cats[k])$temp_FILL1, na.rm = TRUE)
      temp_regr[[i]][[j]][k, "month"] = j
      temp_regr[[i]][[j]][k, "hour_cat"] = hour_cats[k]
    }
  }
  temp_regr2[[i]] <- #Put all regressions into one data frame for each site
    ldply(temp_regr[[i]])
}


#Merge regression coefficients with temperature data and infill
temp_regr_fill <- #initialize list with temperature and regression coefficients
  list()
for (i in 1:length(temp)){
  temp[[i]]$temp_FILL2 <- #make FILL2 = FILL1
    temp[[i]]$temp_FILL1
  temp[[i]]$temp_FLAG5 <- #initiate flagging
    NA
  temp_regr_fill[[i]] <- #subset to NA values (missing obs)
    subset(temp[[i]], is.na(temp_FILL2))
  temp_regr_fill[[i]]  <- #merge with closest station data
    left_join(temp_regr_fill[[i]], 
               temp[[station_ord[i, "stn2"]]][ , c("datetime", "temp_FILL1")],
               by = "datetime")
  temp_regr_fill[[i]]  <- #merge with second closest station data
    left_join(temp_regr_fill[[i]], 
               temp[[station_ord[i, "stn3"]]][ , c("datetime", "temp_FILL1")],
               by = "datetime")
  temp_regr_fill[[i]]  <- #merge with regression coefficients
    left_join(temp_regr_fill[[i]], 
               temp_regr2[[i]],
               by = c("month", "hour_cat"))
  temp_regr_fill[[i]]$temp_FILL2 <- #infill based on which station is available
    ifelse(is.na(temp_regr_fill[[i]]$temp_FILL1.y) == FALSE & 
             is.na(temp_regr_fill[[i]]$temp_FILL1) == FALSE,
           temp_regr_fill[[i]]$temp_FILL1.y * temp_regr_fill[[i]]$slope1a +
             temp_regr_fill[[i]]$temp_FILL1 * temp_regr_fill[[i]]$slope1b +
             temp_regr_fill[[i]]$int1,
           ifelse(is.na(temp_regr_fill[[i]]$temp_FILL1.y) == FALSE & 
                    is.na(temp_regr_fill[[i]]$temp_FILL1) == TRUE,
                  temp_regr_fill[[i]]$temp_FILL1.y * temp_regr_fill[[i]]$slope2 +
                    temp_regr_fill[[i]]$int2,
                  ifelse(is.na(temp_regr_fill[[i]]$temp_FILL1.y) == TRUE & 
                           is.na(temp_regr_fill[[i]]$temp_FILL1) == FALSE,
                         temp_regr_fill[[i]]$temp_FILL1 * temp_regr_fill[[i]]$slope3 +
                           temp_regr_fill[[i]]$int3,
                         temp_regr_fill[[i]]$mean)))
  temp_regr_fill[[i]]$temp_FLAG5 <- #FLAG based on which station is available
    ifelse(is.na(temp_regr_fill[[i]]$temp_FILL1.y) == FALSE & 
             is.na(temp_regr_fill[[i]]$temp_FILL1) == FALSE,
           5, #filled with both stations
           ifelse(is.na(temp_regr_fill[[i]]$temp_FILL1.y) == FALSE & 
                    is.na(temp_regr_fill[[i]]$temp_FILL1) == TRUE,
                  6, #filled with closest station
                  ifelse(is.na(temp_regr_fill[[i]]$temp_FILL1.y) == TRUE & 
                           is.na(temp_regr_fill[[i]]$temp_FILL1) == FALSE,
                         7, #filled with second closest station
                         8))) #filled with mean
  tmp <- #make temporary file for binding
    temp_regr_fill[[i]][, c(1:30)] #use only the columns in temp[[]]
  temp[[i]] <- #remove entries that were filled in this step
    subset(temp[[i]], 
           datetime %in% tmp$datetime == FALSE)
  temp[[i]] <- #append the new, filled entries to the dataframe
    bind_rows(temp[[i]], tmp)
  temp[[i]] <- #make sure df is ordered by datetime
    arrange(temp[[i]], datetime)
  temp[[i]]$flag_FILL <- #concatenate fill values for filling
    rowSums(temp[[i]][, c("temp_FLAG4", "temp_FLAG5")], na.rm= TRUE)
  temp[[i]]$flag_FILL <- #set 0 to NA in flag
    ifelse(temp[[i]]$flag_FILL == 0,
                                NA,
                                temp[[i]]$flag_FILL)
  
}

#remove plyr
detach("package:plyr", unload=TRUE)

#Plot and save filled temperature data
ggplot(temp[["c1"]], aes(datetime, temp_FILL2, color = as.factor(flag_FILL))) + geom_point() + ylim(-40, 40) +
  labs(x = "Date", y = expression("Air temperature ("*degree*C*")"), title = "C1 Hourly Temperature (QC + Infill)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_temp_c1_hr_QC_INFILL.png")
ggplot(temp[["d1"]], aes(datetime, temp_FILL2, color = as.factor(flag_FILL))) + geom_point() + ylim(-40, 40)+
  labs(x = "Date", y = expression("Air temperature ("*degree*C*")"), title = "D1 Hourly Temperature (QC + Infill)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_temp_d1_hr_QC_INFILL.png")
ggplot(temp[["sdl"]], aes(datetime, temp_FILL2, color = as.factor(flag_FILL))) + geom_point() + ylim(-40, 40)+
  labs(x = "Date", y = expression("Air temperature ("*degree*C*")"), title = "SDL Hourly Temperature (QC + Infill)")+
  scale_color_discrete(name = "Infill Protocol") +
  theme(legend.position = "bottom")
ggsave(path = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/nwt_hrly_climate_plots_FILL2/", 
       filename = "nwt_temp_sdl_hr_QC_INFILL.png")




