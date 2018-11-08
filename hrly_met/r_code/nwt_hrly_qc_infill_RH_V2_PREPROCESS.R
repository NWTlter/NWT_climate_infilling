#Preprocess Niwot hourly RH
#Version 2

#Stations: C1, D1, SDL
#Future iterations may also include Arikaree, Subnivean, and GLV stations

#Time period: 1990-01-01 to 2013-12-31
#Time period to be expanded when further data available

#This step is done to remove drift from the RH sensors

#Keith Jennings
#ksjennin@gmail.com
#2016-06-02

#First run the initialization script
#Gathers data, loads packages, writes functions, etc.
setwd("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/")
source(file = "nwt_hrly_qc_infill_RUNFIRST.R")

#Create list with temperature data from each site
rh <- lapply(X = hrly_data, FUN = select, 
               rh, datetime, site, doy)


#Remove dates of erroneous values that may be missed by checks
rh[["c1"]]$rh <- ifelse(rh[["c1"]]$datetime >= as.POSIXct("2001-03-24") &  rh[["c1"]]$datetime <= as.POSIXct("2001-05-31"),
                        NA,
                        rh[["c1"]]$rh)

#Apply first QC protocol
#Remove anomalous values on max/min threshold
#This is done before drift
for (i in 1:length(rh)){
  rh[[i]]$rh_QC1 <- ifelse(rh[[i]]$rh <= 120 & rh[[i]]$rh >= 5,
                           rh[[i]]$rh,
                           NA)
  rh[[i]]$rh_FLAG0 <- ifelse(is.na(rh[[i]]$rh),
                             0,
                             NA) #flag 0 means data missing from original record
  rh[[i]]$rh_FLAG1 <- ifelse(is.na(rh[[i]]$rh) == is.na(rh[[i]]$rh_QC1),
                             NA,
                             1) #flag 1 means rh outside of max/min threshold
}

#Evaluate means of monthly maxima 
#Subset C1 to 2010-2013
#Highest quality RH data

#Add year_mon and month columns to data frame
#For syncing with scaling values (calculated per month)
for (i in 1:length(rh)){
  rh[[i]]$yr_mon <-  as.character(format(rh[[i]]$datetime, "%Y%m"))
  rh[[i]]$mon <- as.numeric(format(rh[[i]]$datetime, "%m"))
}

#Subset C1
#And calculate max rh per yr_month
rh_max_mon <- data.frame("yr_mon" = unique(subset(rh[["c1"]], as.numeric(format(datetime, "%Y")) >= 2010)$yr_mon),
                         "rh_max" = as.numeric(with(subset(rh[["c1"]], as.numeric(format(datetime, "%Y")) >= 2010),
                                                    tapply(rh_QC1, yr_mon, max, na.rm = TRUE))))
rh_max_mon$mon <- as.numeric(substr(rh_max_mon$yr_mon, start = 5, stop = 6))

#Find the mean max rh for each month
rh_max_mon2 <- data.frame("mon" = 1:12,
                          "rh_max" = as.numeric(with(rh_max_mon, tapply(rh_max, mon, mean))))

#The monthly mean RH maxima will be used to calculate the deviation and correct sensor drift
rh_drift <- list()
for (i in 1: length(rh)){
  #Add rh data per station into rh_drift by finding the max per yr_mon
  rh_drift[[i]] <- data.frame("yr_mon" = unique(rh[[i]]$yr_mon),
                              "rh_max_obs" = as.numeric(with(rh[[i]], tapply(rh_QC1, yr_mon, max, na.rm = TRUE))))
  rh_drift[[i]]$mon <- as.numeric(substr(rh_drift[[i]]$yr_mon, start = 5, stop = 6))
  #Merge with rh_max_mon2
  rh_drift[[i]] <- merge(rh_drift[[i]], rh_max_mon2, by = "mon", all.x = TRUE)
  #Calculate difference of rh_max observed and the monthly mean rh_max
  #This will be used to create scaling factor to correct for sensor drift based
  #on deviation from monthly mean rh_max
  rh_drift[[i]]$rh_max_diff <- with(rh_drift[[i]],
                                    rh_max - rh_max_obs)
  #Calculate deviation as a decimal fraction
  rh_drift[[i]]$rh_max_dev <- with(rh_drift[[i]],
                                   rh_max_diff / rh_max_obs)
  #Merge with relative humidity data
  rh[[i]] <- merge(rh[[i]], rh_drift[[i]][ , c("yr_mon", "rh_max_dev", "rh_max_obs")], by = "yr_mon", all.x = TRUE)
  #Find deviation scaling factor by dividing observed rh by the max for that month
  #Makes it so lower RH values are scaled less
  rh[[i]]$rh_max_dev_scale <- with(rh[[i]],
                                   rh_QC1 / rh_max_obs)
  rh[[i]]$rh_scale <- with(rh[[i]],
                           rh_max_dev_scale * rh_max_dev)
  #Correct for sensor drift based on deviation from monthly mean rh_max
  rh[[i]]$rh_preproc <- with(rh[[i]],
                             rh_QC1 * (1 + rh_scale))
  
}


#Note: the above code creates RH values that are generally too low in the drift-corrected data (pre-2000)
#The low and mid values do not seem to scale appropriately
#The drift appears to be nonlinear (i.e. the lower, but not lowest RH values drift more)
#See nwt_hrly_qc_infill_rh_issues.docx for more info

#Various attempts were made to correct this
#Including transforming the scale factor
#detrending
#Working with dewpoint data
#Etc.
#It may be a result of different instruments being used
#Old one often returns to a minimum (~25±) after spiking high (i.e. not much mid range)
#New instrument shows a cycling as different air massess move in and out (i.e. the low for different days is often > 25%)

#There will be two preprocessed columns
#One is as generated above
#Two is using a square root transformed scale as in the below code
#This keeps the maximum scale at 1, while increasing the scale towards the minima

for(i in 1:length(rh)){
  rh[[i]]$rh_max_dev_scale2 <- rh[[i]]$rh_max_dev_scale ^ .5
  rh[[i]]$rh_scale2 <- with(rh[[i]],
                            rh_max_dev_scale2 * rh_max_dev)
  #Correct for sensor drift based on deviation from monthly mean rh_max
  rh[[i]]$rh_preproc2 <- with(rh[[i]],
                              rh_QC1 * (1 + rh_scale2))
  
}

#Relative humidity is wonky at D1 from beginning of record to July 1, 2000
#From metadata:  "all values are bad in the absolute sense. The relative values are accurate."
#The range of the data is quite small, so the minimums need to be scaled during this time period
#Like the maximums were above
#However, the data is unusably bad
#Attempts were made to correct it in V1 of the qc/infill code
#It is unworkable and is omitted from further processing

#Note these data are being omitted and will be gap filled
rh[["d1"]]$rh_preproc <- ifelse(rh[["d1"]]$datetime <= as.POSIXct("2000-07-01"),
                                NA,
                                rh[["d1"]]$rh_preproc)
rh[["d1"]]$rh_preproc2 <- ifelse(rh[["d1"]]$datetime <= as.POSIXct("2000-07-01"),
                                NA,
                                rh[["d1"]]$rh_preproc2)







########################################################
########################################################
#The below code includes some steps that were performed in exploratory data analysis
#Of the RH data
#None of the code below was used in the final preprocessed product











#Try converting to dewpoint to look at distributions



sens_age = 1
0.0666 + 0.8 * sens_age - 0.104 * sens_age ^ 2

#Plot distributions by year
ggplot(test_dp, aes(temp_dp)) + geom_histogram() + facet_wrap(~year)


test_dp$rh_max_dev_scale2 <- test$rh_max_dev_scale ^ .5
test_dp$rh_scale2 <- with(test_dp,
                         rh_max_dev_scale2 * rh_max_dev)
#Correct for sensor drift based on deviation from monthly mean rh_max
test_dp$rh_preproc2 <- with(test_dp,
                           rh_QC1 * (1 + rh_scale2))

test_dp$rh_max_dev_scale3 <- test$rh_max_dev_scale ^ .2
test_dp$rh_scale3 <- with(test_dp,
                          rh_max_dev_scale3 * rh_max_dev)
#Correct for sensor drift based on deviation from monthly mean rh_max
test_dp$rh_preproc3 <- with(test_dp,
                            rh_QC1 * (1 + rh_scale3))

test_dp$rh_max_dev_scale4 <- test$rh_max_dev_scale ^ .01
test_dp$rh_scale4 <- with(test_dp,
                          rh_max_dev_scale4 * rh_max_dev)
#Correct for sensor drift based on deviation from monthly mean rh_max
test_dp$rh_preproc4 <- with(test_dp,
                            rh_QC1 * (1 + rh_scale4))

test_dp$rh_max_dev_scale5 <- test$rh_max_dev_scale ^ .001
test_dp$rh_scale5 <- with(test_dp,
                          rh_max_dev_scale5 * rh_max_dev)
#Correct for sensor drift based on deviation from monthly mean rh_max
test_dp$rh_preproc5 <- with(test_dp,
                            rh_QC1 * (1 + rh_scale5))


#Convert to dewpoint temperature, detrend, and then recalculate RH
#Add temperature workspace
load("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/nwt_hrly_qc_infill_TEMP_V2.RData")

#Merge c1 rh and temp data
test_dp <- merge(rh[[1]], temp[[1]], by = "datetime")
test_dp$year <- as.numeric(format(test_dp$datetime, "%Y"))

#Calculate dewpoint temperature
#From http://andrew.rsmas.miami.edu/bmcnoldy/Humidity.html
test_dp$temp_dp <- 243.04 * (log(test_dp$rh_preproc / 100) + ((17.625 * test_dp$temp_FILL2) / (243.04 + test_dp$temp_FILL2)))/
  (17.625 - log(test_dp$rh_preproc / 100) - ((17.625 * test_dp$temp_FILL2) / (243.04 + test_dp$temp_FILL2)))
test_dp$temp_dp2 <- 243.04 * (log(test_dp$rh_QC1 / 100) + ((17.625 * test_dp$temp_FILL2) / (243.04 + test_dp$temp_FILL2)))/
  (17.625 - log(test_dp$rh_QC1 / 100) - ((17.625 * test_dp$temp_FILL2) / (243.04 + test_dp$temp_FILL2)))


test_dp$temp_dp_pred <- predict(test_dp_lm)
test_dp$obs_num <- 1:length(test_dp$datetime)
summary(lm(temp_dp~obs_num, data = test_dp))
test_dp$temp_dp_pred <- -9.914 + test_dp$obs_num * 2.621e-05
test_dp$temp_dp_detrend <- test_dp$temp_dp - test_dp$temp_dp_pred

test_dp$temp_dp_pred2 <- test_dp$obs_num * 2.621e-05
test_dp$temp_dp_detrend2 <- test_dp$temp_dp - test_dp$temp_dp_pred2

test_dp$rh_preproc_detrend <- 100 * (exp((17.625 * test_dp$temp_dp_detrend) / (243.04 + test_dp$temp_dp_detrend))/
                                       exp((17.625 * test_dp$temp_FILL2) / (243.04 + test_dp$temp_FILL2))) 

test_dp$rh_preproc_detrend2 <- 100 * (exp((17.625 * test_dp$temp_dp_detrend2) / (243.04 + test_dp$temp_dp_detrend2))/
                                       exp((17.625 * test_dp$temp_FILL2) / (243.04 + test_dp$temp_FILL2))) 

