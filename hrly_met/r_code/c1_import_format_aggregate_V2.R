#Script for updating the datetime on the C1 hourly data input
#Change all MDT to MST
#Note: C1 has the 0000 = 2400 problem, but it was corrected on the original import

#Keith Jennings
#ksjennin@gmail.com
#2016-05-13

#Import original C1 hourly workspace
load("~/Documents/research/site_data/r_code_workspaces/nwt_hrly_raw_import_FULLRUN.RData")

#Format the datetime from the ASCII files
for (i in 1:length(c1_hrly_sub_l_2)){
  c1_hrly_sub_l_2[[i]]$datetime <-
    as.POSIXct(paste(c1_hrly_sub_l_2[[i]]$date, c1_hrly_sub_l_2[[i]]$time), 
               format = "%Y-%m-%d %H:%M",
               tz = "MST")
}

#Format the datetime from the "wonky" ASCII files
for (i in 1:length(c1_hrly_wonky_sub_l_2)){
  c1_hrly_wonky_sub_l_2[[i]]$datetime <-
    as.POSIXct(paste(c1_hrly_wonky_sub_l_2[[i]]$date, c1_hrly_wonky_sub_l_2[[i]]$time), 
               format = "%Y-%m-%d %H:%M",
               tz = "MST")
}

#Create a new data frame with the Niwot C1 hourly data
#For this step, we will use all variables and fill in NAs where appropriate
c1_hrly_df <- data.frame("dummy" = 0)

for (i in 1:length(c1_vars)){
  
  vartmp <- c1_vars[i] #name the variable for import
  lengthtmp <- 1 #create temporary length value (to be used for assigning rows on data import)
  
  for (j in 1: length(c1_hrly_sub_l_2)){
    tmpfile <- names(c1_hrly_sub_l_2)[j]
    if (vartmp %in% colnames(c1_hrly_sub_l_2[[j]]) == TRUE){ #if variable in the column names
      
      c1_hrly_df[lengthtmp: (length(c1_hrly_sub_l_2[[j]]$datetime) + lengthtmp - 1), vartmp] <- #import variable
        c1_hrly_sub_l_2[[j]][ , vartmp]
      
      print(paste(tmpfile, vartmp, "TRUE"))
    } else {
      
      c1_hrly_df[lengthtmp: (length(c1_hrly_sub_l_2[[j]]$datetime) + lengthtmp - 1), vartmp] <- #else, set all as NA
        NA
      print(paste(tmpfile, vartmp, "FALSE"))
    }
    
    lengthtmp <- lengthtmp + length(c1_hrly_sub_l_2[[j]]$datetime)
  }
}

#Add all "wonky" format data to one data frame

c1_hrly_wonky_df <- data.frame("dummy" = 0)

for (i in 1:length(c1_vars)){
  
  vartmp <- c1_vars[i] #name the variable for import
  lengthtmp <- 1 #create temporary length value (to be used for assigning rows on data import)
  
  for (j in 1: length(c1_hrly_wonky_sub_l_2)){
    tmpfile <- names(c1_hrly_wonky_sub_l_2)[j]
    if (vartmp %in% colnames(c1_hrly_wonky_sub_l_2[[j]]) == TRUE){ #if variable in the column names
      
      c1_hrly_wonky_df[lengthtmp: (length(c1_hrly_wonky_sub_l_2[[j]]$datetime) + lengthtmp - 1), vartmp] <- #import variable
        c1_hrly_wonky_sub_l_2[[j]][ , vartmp]
      
      print(paste(tmpfile, vartmp, "TRUE"))
    } else {
      
      c1_hrly_wonky_df[lengthtmp: (length(c1_hrly_wonky_sub_l_2[[j]]$datetime) + lengthtmp - 1), vartmp] <- #else, set all as NA
        NA
      print(paste(tmpfile, vartmp, "FALSE"))
    }
    
    lengthtmp <- lengthtmp + length(c1_hrly_wonky_sub_l_2[[j]]$datetime)
  }
}


#Combine the standard and wonky format data, and remove dummy column
c1_hrly_all <- rbind(c1_hrly_df, c1_hrly_wonky_df)
c1_hrly_all$dummy <- NULL

#Format datetime
c1_hrly_all$datetime <- as.POSIXct(c1_hrly_all$datetime, origin = "1970-01-01 00:00.00 UTC",
                                   tz = "MST")

#Order columns with datetime first
c1_hrly_all <- c1_hrly_all[c(22, 1:21)]

#Change the conflicting column names (id/cr21x and wind_spd_vector/wind_spd)
colnames(c1_hrly_all)[2] <- "id"
colnames(c1_hrly_all)[9] <- "wind_spd_vector"


#Format the datetime from the CSV files
#Requires re-import because datetime was formatted from original datetime column
#List files in directory
setwd("~/Documents/research/site_data/niwot/c1_hrly/")
nwt_c1_hrly_files <- list.files(pattern = "*.csv")

#Import headers
#The C1 climate data is split into different years
#Different years have different headers and column numbers
#Note: the order of the 2013 headers is different than the order of the 2013 files in a Mac finder window
#Mac finder thinks cr23 comes before cr1000, while R reads in the files the opposite way
nwt_c1_hrly_headers <- read.csv("~/Documents/research/site_data/niwot/c1_hrly_headers.csv", header=FALSE, stringsAsFactors = FALSE)

#Import files and name the columns
nwt_c1_hrly_l <- list()
for (i in 1: length(nwt_c1_hrly_files)){
  nwt_c1_hrly_l[[i]] <- read.csv(nwt_c1_hrly_files[i], header=TRUE)
  colnames(nwt_c1_hrly_l[[i]])[1: length(nwt_c1_hrly_l[[i]])] <- nwt_c1_hrly_headers[i, ]
}


#Add a consistent datetime format to each dataset
#In the raw datasets different years use different formats
#Use doy > date + hour > time method for 1996 to 2012 and 2013 part 2
#Use datetime method for 2013 parts 1+2 and 2014
for (i in c(1: 17)){
  nwt_c1_hrly_l[[i]]$date <- as.Date(paste(nwt_c1_hrly_l[[i]]$year , 
                                           format(strptime(nwt_c1_hrly_l[[i]]$doy, format="%j"), 
                                                  format = "%m-%d"), #This turns a day of year date to month-day format
                                           sep = "-"))
  nwt_c1_hrly_l[[i]]$hour <- ifelse(nwt_c1_hrly_l[[i]]$hour <1000, 
                                    paste0("0", nwt_c1_hrly_l[[i]]$hour), 
                                    nwt_c1_hrly_l[[i]]$hour) 
  nwt_c1_hrly_l[[i]]$time <- paste(substr(nwt_c1_hrly_l[[i]]$hour, 1, 2), 
                                   substr(nwt_c1_hrly_l[[i]]$hour, 3, 4), 
                                   sep = ":")
  nwt_c1_hrly_l[[i]]$datetime <- as.POSIXct(paste(nwt_c1_hrly_l[[i]]$date, 
                                                  nwt_c1_hrly_l[[i]]$time), 
                                            format = "%Y-%m-%d %H:%M",
                                            tz = "MST")
}

nwt_c1_hrly_l[[18]]$datetime <- as.POSIXct(nwt_c1_hrly_l[[18]]$datetime,
                                           format = "%Y-%m-%d %H:%M",
                                           tz = "MST")
nwt_c1_hrly_l[[19]]$datetime <- as.POSIXct(nwt_c1_hrly_l[[19]]$date_time,
                                           format = "%Y-%m-%d %H:%M",
                                           tz = "MST")
nwt_c1_hrly_l[[20]]$datetime <- as.POSIXct(nwt_c1_hrly_l[[20]]$datetime,
                                           format = "%m/%d/%y %H:%M",
                                           tz = "MST")

#Unlist newer hourly data into dataframe

#Replace "array_id", "logger", and "arrayid_hourly" with "id"
#"cr21x_inst" will be changed in c1_hrly_all to "id" 
for (i in 1:length(nwt_c1_hrly_l)){
  for (j in 1:length(nwt_c1_hrly_l[[i]])){
    if(colnames(nwt_c1_hrly_l[[i]])[j] %in% c("array_id", "logger", "arrayid_hourly", "cr21x_inst") == TRUE){
      colnames(nwt_c1_hrly_l[[i]])[j] <- "id"
    }
  }
}

#Create a new data frame with the newer Niwot C1 hourly data
#For this step, we will use all variables (found in the older data) and fill in NAs where appropriate
c1_hrly_newer_df <- data.frame("dummy" = 0)

for (i in 1:length(c1_vars2)){
  
  vartmp <- c1_vars2[i] #name the variable for import
  lengthtmp <- 1 #create temporary length value (to be used for assigning rows on data import)
  
  for (j in 1: length(nwt_c1_hrly_l)){
    tmpfile <- names(nwt_c1_hrly_l)[i]
    if (vartmp %in% colnames(nwt_c1_hrly_l[[j]]) == TRUE){ #if variable in the column names
      
      c1_hrly_newer_df[lengthtmp: (length(nwt_c1_hrly_l[[j]]$datetime) + lengthtmp - 1), vartmp] <- #import variable
        nwt_c1_hrly_l[[j]][ , vartmp]
      
      print(paste(tmpfile, vartmp, "TRUE"))
    } else {
      
      c1_hrly_newer_df[lengthtmp: (length(nwt_c1_hrly_l[[j]]$datetime) + lengthtmp - 1), vartmp] <- #else, set all as NA
        NA
      print(paste(tmpfile, vartmp, "FALSE"))
    }
    
    lengthtmp <- lengthtmp + length(nwt_c1_hrly_l[[j]]$datetime)
  }
}

#Remove dummy
c1_hrly_newer_df$dummy <- NULL

#Format datetime
c1_hrly_newer_df$datetime <- as.POSIXct(c1_hrly_newer_df$datetime, origin = "1970-01-01 00:00.00 UTC",
                                        tz = "MST")

#Reorder to be consistent with older data
c1_hrly_newer_df <- c1_hrly_newer_df[c(22, 1:21)]


#Combine data frames
c1_hrly_COMPLETE <- rbind(c1_hrly_all, c1_hrly_newer_df)

####################################################################################################
# CREATE REGULAR 1 h TIME SERIES AND BIND DATA TO REGULAR SERIES
###################################################################################################


#Make regular sequence covering dataset at 1 h timestep
#Starting with water year 1989
#Previous data at 2 h and 30 min time steos
c1_datetime2 <- data.frame("datetime" = seq.POSIXt(from = as.POSIXct("1988-10-01 01:00", 
                                                                      tz = "MST"), #tz must be set
                                                    to = max(c1_hrly_COMPLETE$datetime, na.rm = TRUE),
                                                    by = "1 hour"))

#Merge data observations with regular time series
c1_hrly_merged2 <- merge(c1_datetime2, c1_hrly_COMPLETE, by = "datetime", all.x = TRUE)


####################################################################################################
# ERROR CHECK AND EXPORT
####################################################################################################


#Remove duplicate rows
c1_hrly_merged3 <- unique(c1_hrly_merged2)

#Check for timestamp duplicates
#This needs to be done because there are lots of errors in the Niwot data files
#Duplicate timestamps with different data will be exposed by checking all instances
#where timediff = 0
c1_hrly_merged4 <- c1_hrly_merged3
c1_hrly_merged4$timediff <- c(0, diff(c1_hrly_merged4$datetime))

c1_dupes <- subset(c1_hrly_merged3, duplicated(c1_hrly_merged3$datetime))
write.csv(c1_dupes, "c1_dupes.csv")

#######################
########################
#Figure out duplication issues
#All 1988-10-01 and 10-02 dupes are from a miscoded 30sep88 file
#Entries from those days with id #120 were removed
c1_hrly_merged4 <- subset(c1_hrly_merged4, as.numeric(format(datetime, "%Y%m%d")) != 19881001 |
                            as.numeric(format(datetime, "%Y%m%d")) != 19881002 &
                            id != 120)
c1_hrly_merged4 <- subset(c1_hrly_merged4, row.names(c1_hrly_merged4) != 48)

####
#1990-04-01 dupes caused by repeat 2400 line in 31mar90 file
#i.e. code added an extra day when it hit the repeat
#Extra line was removed from file 
####
#1991-02-02 12:00:00 dupe caused by repeat line in 28feb91 file
#extra line (37) removed 
####
#1992 September and October dupes caused by overlap in 30sep92 and 23jan93 files
#Lines 1826 through 1834 in 30sep92 duplicate previous and following day
#Lines deleted
####
#1993 June and July dupes caused by improper start dates listed in C1CR21X file
#Start dates changed in daterange file for 22jul and 19aug 93 files
c1_dateranges[39,2] <- as.Date("1993-06-28")
c1_dateranges[40,2] <- as.Date("1993-07-22")
####
#1994 April dupes caused by wrong start date for 12jun94 file
#Should be 1994-04-16, not 1994-04-15
#Start dates changed in daterange file 
c1_dateranges[48,2] <- as.Date("1994-04-16")
c1_dateranges[48,3] <- 100
#1995 September dupes caused by erroneous final lines in 11sep95 files
#All entries after 1995-09-11 2400 wrong
#Bad entries removed
#####
#For above errors, lines 57 through 98 and 133 to 166 in nwt_hrly_raw_import_FULLRUN were rerun
#Generates new c1_hrly_sub_l_2 file

####
#2013-03 errors caused by bad date coding in lines 126 to 151 above
#Fixed and rerun

c1_dupes2 <- subset(c1_hrly_merged4, duplicated(c1_hrly_merged4$datetime))
#No more duplicates, fixes worked


#Extract DOY from datetime and order the columns
c1_hrly_merged4$doy <- as.numeric(format(c1_hrly_merged4$datetime, "%j"))
c1_hrly_merged4$hour <- as.numeric(format(c1_hrly_merged4$datetime, "%H"))
c1_hrly_merged4$date <- as.character(format(c1_hrly_merged4$datetime, "%Y-%m-%d"))
c1_hrly_merged4$time <- as.character(format(c1_hrly_merged4$datetime, "%H:%M"))
c1_hrly_merged4$year <- as.numeric(format(c1_hrly_merged4$datetime, "%Y"))

#Put all date info in left columns
c1_hrly_merged4 <- c1_hrly_merged4[ , c(27,25,24,12,26,1,
                                        2:11,
                                        13:23)]

#Replace -6999 and 6999 with NA
c1_hrly_merged4[c1_hrly_merged4 == -6999] <- NA
c1_hrly_merged4[c1_hrly_merged4 == 6999] <- NA

#Export
setwd(dir = "~/Documents/research/site_data/niwot/c1_hourly_raw_data_files/")
write.csv(c1_hrly_merged4, "c1_hrly_met_data_all_NOQC.csv", row.names = FALSE, quote = FALSE)

