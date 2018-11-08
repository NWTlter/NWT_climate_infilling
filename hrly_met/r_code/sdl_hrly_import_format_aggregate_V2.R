#Script for importing and preprocessing Saddle hourly data files
#All files for import have been preprocessed
#See workflow word doc for more info

##################################
##################################
#This is version 2 of the script
#Corrects daylight savings time issue

#Keith Jennings
#ksjennin@gmail.com
#2016-02-24

#appended on 2016-05-10
#added more date information and reordered columns


####################################################################################################
#STEP 1: IMPORT PREPROCESSED DATA INTO LIST
####################################################################################################

#Import preprocessed sdl climate data
#Already have headers and DOY
setwd("~/Documents/research/site_data/niwot/sdl_hourly_raw_data_files/preprocessed/")
sdl_list2 <- list.files()
sdl_hrly_pre_l <- list()

#Vector of files that need 0000 time stamp changed to 2400
#All other preprocessed files have 0000 as first entry of day
#Or 2400 as last entry of day
#This will keep proper order when as.POSIXct is used
sdl_00_to_24_files <- c("1990a.",
                        "1991b.",
                        "1991e.",
                        "1991e2",
                        "1991f.",
                        "1992a.",
                        "1992b.",
                        "1992c.",
                        "1993.s",
                        "1994.s",
                        "1995.s")

for (i in 1:length(sdl_list2)){
  sdl_hrly_pre_l[[i]] <- #import the data files into the list
    read.table(as.character(sdl_list2[i]), sep = ",", header = TRUE)
  
  #Change 0 to 2400 if file is in the list of files to be changed
  if (substr(sdl_list2[i], 1, 6) %in% sdl_00_to_24_files){
    sdl_hrly_pre_l[[i]]$hour <- ifelse(sdl_hrly_pre_l[[i]]$hour == 0,
                                       2400,
                                       sdl_hrly_pre_l[[i]]$hour)
  }
  
  if (substr(sdl_list2[i], 1, 4) != 2009){ #all pre-2009 files need datetime concatenated first
    sdl_hrly_pre_l[[i]]$date <- #create date using year and julian day from file
      as.Date(paste(substr(sdl_list2[i], 1, 4), sdl_hrly_pre_l[[i]]$doy, sep = "-"),
              format = "%Y-%j")
    
    sdl_hrly_pre_l[[i]]$time <- #add leading zeroes to columns that need it
      ifelse(sdl_hrly_pre_l[[i]]$hour < 30, paste0("000", sdl_hrly_pre_l[[i]]$hour), 
             ifelse(sdl_hrly_pre_l[[i]]$hour < 100, paste0("00", sdl_hrly_pre_l[[i]]$hour), 
                    ifelse(sdl_hrly_pre_l[[i]]$hour < 1000, paste0("0", sdl_hrly_pre_l[[i]]$hour), sdl_hrly_pre_l[[i]]$hour)))
    
    sdl_hrly_pre_l[[i]]$time <- #format time with colon
      paste(substr(sdl_hrly_pre_l[[i]]$time, 1, 2), 
            substr(sdl_hrly_pre_l[[i]]$time, 3, 4),
            sep = ":")
    
    sdl_hrly_pre_l[[i]]$datetime <-
      as.POSIXct(paste(sdl_hrly_pre_l[[i]]$date, sdl_hrly_pre_l[[i]]$time), 
                 format = "%Y-%m-%d %H:%M", tz = "MST")
    #tz = "MST" must be set or else daylight savings time will set
    #that is bad because the data are collected at MST no matter day of year
    
  } else{ #The 2009 to 2013 file already has datetime
    sdl_hrly_pre_l[[i]]$datetime <-
      as.POSIXct(sdl_hrly_pre_l[[i]]$datetime, 
                 format = "%m/%d/%y %H:%M", tz = "MST")
  }
  
  
  names(sdl_hrly_pre_l)[i] <- 
    as.character(sdl_list2[i]) #name the data frame in the list with the name of file
  
  
}


####################################################################################################
#STEP 2: DETERMINE ALL VARIABLE/COLUMN NAMES
####################################################################################################


#Pull out the appropriate columns from each data frame in the list and bind them
#Make dummy data frame
sdl_vars2 <- data.frame("vars" = as.character(""))

#Loop through the list of hourly Saddle data
#Extract the column names and bind to existing column names
for (i in 1:length(sdl_hrly_pre_l)){
  tmp <- data.frame("vars" = as.character(colnames(sdl_hrly_pre_l[[i]])))
  sdl_vars2 <- rbind(sdl_vars2, tmp)
}

#Subset to only the unique column names
sdl_vars2 <- unique(sdl_vars2$vars)

#Remove "" (1), "wind_spd_scalar.1" (9), and "blank" (19) column names
sdl_vars2 <- as.character(sdl_vars2[-c(1, 9, 19)])


####################################################################################################
#STEP 3: BIND ALL DATA FROM LIST INTO ONE DATA FRAME
####################################################################################################


#Create dummy data frame
sdl_hrly_df2 <- data.frame("dummy" = 0)

#Loop through each variable name for each list entry
for (i in 1:length(sdl_vars2)){
  
  vartmp <- sdl_vars2[i] #name the variable for import
  lengthtmp <- 1 #create temporary length value (to be used for assigning rows on data import)
  
  for (j in 1: length(sdl_hrly_pre_l)){
    
    tmpfile <- names(sdl_hrly_pre_l)[j] #get name of data frame in list being used
    
    if (vartmp %in% colnames(sdl_hrly_pre_l[[j]]) == TRUE){ #if variable in the column names
      
      sdl_hrly_df2[lengthtmp: (length(sdl_hrly_pre_l[[j]]$datetime) + lengthtmp - 1), vartmp] <- #import variable
        sdl_hrly_pre_l[[j]][ , vartmp]
      
      print(paste(tmpfile, vartmp, "TRUE"))
    } else {
      
      sdl_hrly_df2[lengthtmp: (length(sdl_hrly_pre_l[[j]]$datetime) + lengthtmp - 1), vartmp] <- #else, set all as NA
        NA
      print(paste(tmpfile, vartmp, "FALSE"))
    }
    
    #Make the lengthtmp longer by the length of the data just added (so entries are placed in new rows)
    lengthtmp <- lengthtmp + length(sdl_hrly_pre_l[[j]]$datetime)
  }
}

#Remove dummy column
sdl_hrly_df2$dummy <- NULL

#Format datetime
sdl_hrly_df2$datetime <- as.POSIXct(sdl_hrly_df2$datetime, 
                                    origin = "1970-01-01 00:00.00 UTC", 
                                    tz = "MST") #time zone must be set to MST

####################################################################################################
#STEP 4: CREATE REGULAR 1 h TIME SERIES AND BIND DATA TO REGULAR SERIES
###################################################################################################


#Make regular sequence covering dataset at 1 h timestep
sdl_datetime2 <- data.frame("datetime" = seq.POSIXt(from = as.POSIXct("1990-01-01 01:00", 
                                                                      tz = "MST"), #tz must be set
                                                    to = max(sdl_hrly_df2$datetime, na.rm = TRUE),
                                                    by = "1 hour"))

#Merge data observations with regular time series
sdl_hrly_merged2 <- merge(sdl_datetime2, sdl_hrly_df2, by = "datetime", all.x = TRUE)


####################################################################################################
#STEP 5: ERROR CHECK AND EXPORT
####################################################################################################


#Remove duplicate rows
sdl_hrly_merged3 <- unique(sdl_hrly_merged2)

#Check for timestamp duplicates
#This needs to be done because there are lots of errors in the Niwot data files
#Duplicate timestamps with different data will be exposed by checking all instances
#where timediff = 0
sdl_hrly_merged4 <- sdl_hrly_merged3
sdl_hrly_merged4$timediff <- c(0, diff(sdl_hrly_merged4$datetime))

#Extract DOY from datetime and order the columns
sdl_hrly_merged3$doy <- as.numeric(format(sdl_hrly_merged3$datetime, "%j"))
sdl_hrly_merged3$hour <- as.numeric(format(sdl_hrly_merged3$datetime, "%H"))
sdl_hrly_merged3$date <- as.character(format(sdl_hrly_merged3$datetime, "%Y-%m-%d"))
sdl_hrly_merged3$time <- as.character(format(sdl_hrly_merged3$datetime, "%H:%M"))
sdl_hrly_merged3$year <- as.numeric(format(sdl_hrly_merged3$datetime, "%Y"))

#Put all date info in left columns
sdl_hrly_merged3 <- sdl_hrly_merged3[ , c(19, 15, 2, 14, 16, 1,
                                      3:13,
                                      17:18,
                                      20:47)]

#Replace -6999 and 6999 with NA
sdl_hrly_merged3[sdl_hrly_merged3 == -6999] <- NA
sdl_hrly_merged3[sdl_hrly_merged3 == 6999] <- NA

#Export
setwd(dir = "~/Documents/research/site_data/niwot/sdl_hourly_raw_data_files/")
write.csv(sdl_hrly_merged3, "sdl_hrly_met_data_all_NOQC.csv", row.names = FALSE, quote = FALSE)



####################################################################################################
#APPENDIX ISSUES
#THE ABOVE CODE WAS ITERATED TO REMOVE THE BELOW PROBLEMS
####################################################################################################



#Note: There are some entries from the original files with duplicate time stamps
#But different data entries
#1991-07-21 00:00 to 1991-08-01 10:00 
#Time series is shifted by a day 
#the 31aug91.sdl file begins on DOY 211 not DOY 212
#There were lots of repeat rows and altered days
#Caused by the file having the wrong start daye
#Shifting it one day brings data into alignment 
#Changed DOY and deleted repeat rows (2016-02-27)
#1991-08-08 00:00 to 1991-08-09 23:00
#Fixed with above file (1991e.pre.sdl.hr)
#1992-03-17 05:00 to 1992-03-17 12:00
#Second set of entries were actually for 03-23 (fixed in preprocessed file 1992a.pre.sdl.hr)
#1992-06-05 10:00
#Removed second anomalous entry (1992b.pre.sdl.hr)
#1993-03-10 11:00
#Removed second anomalous entry (1993.pre.sdl.hr)
#1993-08-25 00:00 to 1993-08-25 23:00
#DOY skipped 1 ahead on DOY 203 @ 1000 to DOY 204
#This skip lasted until DOY 237, which repeated
#DOY 204 1000 to DOY 237 2400 were corrected to DOY 203 1000 to DOY 236 2400 (1993.sdl.pre.hr)
#1997-10-13 00:00
#File went from using 2400 to 0 as last entry of day on DOY 286
#All last-of-day 0s changed to 2400 (1997c.sdl.pre.csv)


#Niwot reports 00:00 as the last entry of a given day in some files, but POSIX thinks it's the first entry of the day
#00:00 needs to be changed to 24:00 for:
#1990a
#1991b
#1991e
#1991e2
#1991f
#1992 to 1995


