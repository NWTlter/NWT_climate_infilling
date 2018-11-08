#Script to be run when initializing Niwot hourly data QC/infilling scripts

#Includes packages, file import, functions

#Keith Jennings
#2015-05-27
#ksjennin@gmail.com

#Load necessary packages
library(ggplot2) #plotting
library(dplyr) #use for subsetting and data frame manipulation

#Set working directory and import datasets
setwd(dir = "~/Documents/research/site_data/niwot/nwt_hrly_qc/processed_hrly_NOQC/")
files <- list.files(pattern = ".csv")
sites <- c("c1", "d1", "sdl")
hrly_data <- list()

#Import
#File in list based on site order (i.e. C1 = 1, D1 = 2, SDL = 3)
for (i in 1:length(files)){
  hrly_data[[i]] <- 
    read.csv(files[i])
  hrly_data[[i]]$datetime <- 
    as.POSIXct(hrly_data[[i]]$datetime, 
               tz = "MST")
  hrly_data[[i]]$site <-  
    sites[i]
  names(hrly_data)[i] <- 
    sites[i]
}

#Trim to overlapping dates
hrly_data <- lapply(X = hrly_data, FUN = subset, 
                    datetime >= as.POSIXct("1990-01-01 01:00") &
                      datetime <= as.POSIXct("2013-12-31 23:00"))

#Set plotting theme
previous_theme <- theme_set(theme_classic(base_size = 16))


##############################
##############################
#Functions for QC and infill process
##############################
fun_FILLTYPE <- #Assigns the type of infilling to be performed based on gap length (x)
  function(x){ifelse(x > 72, 
                     "REGR", 
                     ifelse(x <= 72 & x >= 25,
                            "ARIMA",
                            ifelse(x == 1,
                                   "INTERP",
                                   "AVG24")))}

fun_HOURCAT <- #Assigns category to each 3 h time block
  function(x){cut(x,
                  breaks = seq(0, 24, by = 3),
                  labels = letters[seq(1, 8)],
                  right = FALSE)}


#Define number of stations and
#Make data frame of station order for regressions
#Each station has its closest partner listed first (consistently produces best regressions)
#Consider using more flexible format if more stations added
n_stations = 
  3 #make a variable in case more added later
station_ord <- data.frame("stn1" = c("c1", "d1", "sdl"),
                          "stn2" = c("sdl", "sdl", "d1"),
                          "stn3" = c("d1", "c1", "c1"), 
                          stringsAsFactors = FALSE)

#Name the hour categories and add hour cats and months to the data
hour_cats <- 
  letters[seq(1,8, by = 1)]

