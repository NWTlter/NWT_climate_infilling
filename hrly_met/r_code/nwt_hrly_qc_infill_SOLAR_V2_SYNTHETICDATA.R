#Script for generating synthetic solar radiation data
#Used for maximum threshold in QC procedure

#Keith Jennings
#ksjennin@gmail.com
#2016-06-02

#Load solaR package
#For generating synthetic solar radiation data
#Note solaR sets R environment to UTC time zone
#Package needs to be unloaded and R restarted for datetimes to go back to normal
#Just resetting the time zone with Sys.setenv doesn't seem to work
library(solaR)

#Set latitude and generate series of hourly clear sky solar
lat <- 40.05
BTd <- fBTd(mode = "serie") #Creates a 1 year, daily, blank time series
SolD <- fSolD(lat, BTd = BTd) #Calculates daily sun movements for latitude on dates given by BTD
SolI <- fSolI(SolD, sample = "hour") #Calculates hourly solar variables from SolD

#Convert zoo object to data frame and include datetime
solar_synth <- as.data.frame(SolI)
solar_synth$datetime <- time(SolI) #take datetime from SolI and add it to solar_synth

#Add new datetime (on MST, not UTC) and convert solar to sw_in_tot_mj
solar_synth$datetime2 <- seq.POSIXt(from = as.POSIXct("2016-01-01 01:00:00 MST"), by = "1 hour", length.out = 8784)
solar_synth$sw_in_tot_mj_SYN <- solar_synth$Bo0 * 3600 / 1000000

#Add character column of month-day-hour
#This will be used for setting the max incoming solar for each month-day-hour in the
#threshold QC step
solar_synth$mon_day_hr <- as.character(format(solar_synth$datetime2, "%m-%d-%H"))