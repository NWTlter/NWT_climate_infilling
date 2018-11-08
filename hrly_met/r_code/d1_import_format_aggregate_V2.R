#Script for importing and processing D1 hourly met files
#Includes old format (no day of year, must assign based on file start date)
#Info found in D1 CR21X file
#And newer files as .csv

##########################
#Note: this is V2 of the import script
#This version handles daylight savings time (sets all POSIX datetimes to MST)
#Corrects the 0000 end to beginning switch for files that list 0000 as end of day
#Corrects missing data from V1 by removing lengthtmp error (line 241 in old code)
##########################

#Keith Jennings
#ksjennin@gmail.com
#2016-05-09

#Set working directory
setwd("~/Documents/research/site_data/niwot/d1_hourly_raw_data_files/old_format/")

#list files
d1_list <- 
  data.frame("files" = list.files(pattern = "*.d1")) #list all files with .D1 in name
d1_list$date <- 
  as.Date(substr(d1_list$files, 1, 7), 
                        format = "%d%b%y") #%b corresponds to R's three-letter month abbreviation

#Import format file
#Shows start and end dates of different file formats
#Applies to .d1 files from early in record
#Formats have been changed from Niwot LTER readme to reflect ACTUAL dates of header changes
d1_formats <- 
  read.csv("~/Documents/research/site_data/niwot/d1_hourly_raw_data_files/d1_hrly_formatlist.csv")

#Format dates for applying correct file format to 
d1_formats$startdate <- as.Date(d1_formats$startdate, 
                                format = "%m/%d/%y")
d1_formats$enddate <- as.Date(d1_formats$enddate, 
                              format = "%m/%d/%y")

d1_list$format <- ifelse(d1_list$files == "30jun88.d1.3" |d1_list$files ==  "30sep88.d1.2", 
                         "b", 
                         "a")
#Note D1 ASCII files don't follow date-dictated header pattern
#There are only two (see above) with the b format


#Import headers file
#File made from "Formats" folder 
d1_headers <- read.csv(file = "/Users/keje6048/Documents/research/site_data/niwot/d1_hourly_raw_data_files/nwt_d1_hrly_headers_oldformat.csv")

#Import all files from list and add headers
d1_hrly_l <- list()


for (i in 1: length(d1_list$files)){
  d1_hrly_l[[i]] <- #import the data files into the list
    read.table(as.character(d1_list[i, "files"]), fill = TRUE, sep = ",")
  
  
  colnames(d1_hrly_l[[i]])[1: length(d1_hrly_l[[i]])] <- #add column names based on the data's format
    as.character(d1_headers[ , d1_list[i , "format"]])
  
  
  names(d1_hrly_l)[i] <- 
    as.character(d1_list[i, "files"]) #name the data frame with the name of file
}

#In [[4]] sw_in and sw_in_tot are switched
colnames(d1_hrly_l[[4]]) <- c("id",
                              "temp",
                              "rh",
                              "press",
                              "wind_spd_scalar",
                              "sw_in_mj",
                              "sw_in_tot_mj",
                              "wind_spd_scalar",
                              "wind_spd_vector",
                              "wind_dir",
                              "wind_dir_sd",
                              "blank",
                              "blank2",
                              "hour")



#Add dates and datetimes to each file
#This is tricky because no files list the date
#Date must be inferred from the file name and ranges given in the metadata
#According to file structure, hour 0 is hour 2400 (i.e., the last hour in a given day, not the first)
d1_dateranges <- 
  read.csv("~/Documents/research/site_data/niwot/d1_hourly_raw_data_files/nwt_d1_file_dateranges.csv")
d1_dateranges$startdate <- 
  as.Date(d1_dateranges$startdate, format = "%m/%d/%y")
d1_dateranges$enddate <- 
  as.Date(d1_dateranges$enddate, format = "%m/%d/%y")

for (i in 1:length(d1_hrly_l)){
  tmpfile <- #get name of file
    names(d1_hrly_l)[i] 
  q = #search for filename in date ranges to assign start and end date
    which(grepl(tmpfile, d1_dateranges$filename))
  start <- #assign start date
    d1_dateranges[q, "startdate"]
  end <- #assign end date
    d1_dateranges[q, "enddate"]
  d1_hrly_l[[i]]$date <- #initiate date column with start date
    start
  datecounter = 0
  #Change all 0 entries to 2400
  d1_hrly_l[[i]]$hour <- ifelse(d1_hrly_l[[i]]$hour == 0,
                                2400,
                                d1_hrly_l[[i]]$hour)
  #Assign date to each entry and advance by 1 when hour = 2400
  #Note, dates must be doublechecked because 2400 sometimes missing
  for (j in 1:length(d1_hrly_l[[i]]$hour)){
    d1_hrly_l[[i]][j, "date"] <- as.Date(d1_hrly_l[[i]][j, "date"] + datecounter)
    if (d1_hrly_l[[i]][j, "hour"] == 2400) {
      datecounter = datecounter + 1 #advance date by 1 every time 2400 is reached
    }
  }
  
  #Print whether the coded end date equals the specified end date
  if (d1_hrly_l[[i]][j, "date"] == end){
    print(paste(tmpfile, "TRUE"))
  } else{
    print(paste(tmpfile, "FALSE"))
  }
  #Add time column
  d1_hrly_l[[i]]$time <- 
    ifelse(d1_hrly_l[[i]]$hour < 30, paste0("000", d1_hrly_l[[i]]$hour), 
           ifelse(d1_hrly_l[[i]]$hour < 100, paste0("00", d1_hrly_l[[i]]$hour), 
                  ifelse(d1_hrly_l[[i]]$hour < 1000, paste0("0", d1_hrly_l[[i]]$hour), d1_hrly_l[[i]]$hour)))
  d1_hrly_l[[i]]$time <- 
    paste(substr(d1_hrly_l[[i]]$time, 1, 2), 
          substr(d1_hrly_l[[i]]$time, 3, 4),
          sep = ":")
  #Concatenate date and time for date time
  d1_hrly_l[[i]]$datetime <-
    as.POSIXct(paste(d1_hrly_l[[i]]$date, d1_hrly_l[[i]]$time), 
               format = "%Y-%m-%d %H:%M",
               tz = "MST") #tz = MST must be specified to keep in standard time
}

#Check for duplicate time stamps and correct
#Run this code and inspect subset(d1_hrly_l[[i]], duplicated(d1_hrly_l[[i]]$datetime) == TRUE)
#Where i = the data frame number in the list
#Only duplicates are in [[1]] (rows 24:46, 1989-07-01 2400 missing)
#And [[10]] (row 724 a repeat of 723)
tmp <- d1_hrly_l[[1]][24:1655,]
tmp$date <- tmp$date + 1 #add 1 day to all dates in tmp
tmp$datetime <- tmp$datetime + 86400 #add 1 day to all datetimes in tmp
d1_hrly_l[[1]] <- rbind(d1_hrly_l[[1]][1:23,], tmp) #bind with original data

d1_hrly_l[[10]]<- #remove duplicate row from [[10]]
  rbind(d1_hrly_l[[10]][1:723,], d1_hrly_l[[10]][725:1991,])

#Create a new data frame with the Niwot d1 hourly data
#For this step, we will use all variables and fill in NAs where appropriate

d1_vars <- #make vector with all unique header entries
  as.character(unique(unlist(d1_headers)))

d1_vars <- d1_vars[-c(12, 13,14)] #remove entries 12 ("") and 13/14 ("blank")

d1_vars[12] <- "datetime" #add datetime

d1_hrly_df <- data.frame("dummy" = 0)

for (i in 1:length(d1_vars)){
  
  vartmp <- d1_vars[i] #name the variable for import
  lengthtmp <- 1 #create temporary length value (to be used for assigning rows on data import)
  
  for (j in 1: length(d1_hrly_l)){
    tmpfile <- names(d1_hrly_l)[j]
    if (vartmp %in% colnames(d1_hrly_l[[j]]) == TRUE){ #if variable in the column names
      
      d1_hrly_df[lengthtmp: (length(d1_hrly_l[[j]]$datetime) + lengthtmp - 1), vartmp] <- #import variable
        d1_hrly_l[[j]][ , vartmp]
      
      print(paste(tmpfile, vartmp, "TRUE"))
    } else {
      
      d1_hrly_df[lengthtmp: (length(d1_hrly_l[[j]]$datetime) + lengthtmp - 1), vartmp] <- #else, set all as NA
        NA
      print(paste(tmpfile, vartmp, "FALSE"))
    }
    
    lengthtmp <- lengthtmp + length(d1_hrly_l[[j]]$datetime)
  }
}

#Remove dummy column
d1_hrly_df$dummy <- NULL

#Format datetime
d1_hrly_df$datetime <- as.POSIXct(d1_hrly_df$datetime, origin = "1970-01-01 00:00.00 UTC", tz = "MST")




#Create a data frame of datetimes from the start to the end of the ASCII files
#First time steps are 2 hours
#This is to deal with DST
#Then 0.5 hours
#Then 1 hours

#NOTE: There are issues with the handling of times below
#All NWT data is reported as MST (regardless of time of year)
#And R cannot handle this
#It wants to force everything into DST, even if TZ is set as UTC
#This will need to be fixed

#Fix is probably to change times on import
#Then make the regular time series without worrying about DST 
#Because all observations will have shifted accordingly

#d1_datetime <- data.frame("datetime" = c(seq.POSIXt(from = as.POSIXct("1987-09-03 10:00"),
#                                                    to = as.POSIXct("1987-10-25 00:00"),
#                                                   by = "2 hour"),
#                                       seq.POSIXt(from = as.POSIXct("1987-10-25 02:00"),
#                                                    to = as.POSIXct("1988-04-03 02:00"),
#                                                    by = "2 hour"),
#                                         seq.POSIXt(from = as.POSIXct("1988-04-03 04:00"),
#                                                    to = as.POSIXct("1988-06-13 12:00"),
#                                                   by = "2 hour"),
#                                         seq.POSIXt(from = as.POSIXct("1988-06-13 12:30"),
#                                                    to = as.POSIXct("1988-08-22 10:00"),
#                                                   by = "30 min"),
#                                         seq.POSIXt(from = as.POSIXct("1988-08-22 11:00"),
#                                                   to = max(d1_hrly_df$datetime, na.rm = TRUE),
#                                                    by = "1 hour")))

#Merge data observations with regular time series
#d1_hrly_merged <- merge(d1_datetime, d1_hrly_df, by = "datetime", all.x = TRUE)


#Import preprocessed D1 climate data
#Already have headers and DOY
setwd("~/Documents/research/site_data/niwot/d1_hourly_raw_data_files/preprocessed/")

#Make vector of files where 0000 = 2400 and must be changed
d1_pre_0000_2400 <- c("1990.","1991.","1992a","1992b","1993a","1993b","1994.","1995.","1999b","1999c","2000a")

#Get list of files to import
d1_list2 <- list.files()
d1_hrly_pre_l <- list()

for (i in 1:length(d1_list2)){

  d1_hrly_pre_l[[i]] <- #import the data files into the list
    read.table(as.character(d1_list2[i]), sep = ",", header = TRUE)
  
  #Find the 0000 to 2400 conversion files and change the 0 timestamp
  if(substr(d1_list2[i], start = 1, stop = 5) %in% d1_pre_0000_2400){
    d1_hrly_pre_l[[i]]$hour <- ifelse(d1_hrly_pre_l[[i]]$hour == 0,
                                      2400,
                                      d1_hrly_pre_l[[i]]$hour)
  }
  
  d1_hrly_pre_l[[i]]$date <- 
    as.Date(paste(substr(d1_list2[i], 1, 4), d1_hrly_pre_l[[i]]$doy, sep = "-"),
            format = "%Y-%j")
  
  d1_hrly_pre_l[[i]]$time <- 
    ifelse(d1_hrly_pre_l[[i]]$hour < 30, paste0("000", d1_hrly_pre_l[[i]]$hour), 
           ifelse(d1_hrly_pre_l[[i]]$hour < 100, paste0("00", d1_hrly_pre_l[[i]]$hour), 
                  ifelse(d1_hrly_pre_l[[i]]$hour < 1000, paste0("0", d1_hrly_pre_l[[i]]$hour), d1_hrly_pre_l[[i]]$hour)))
  
  d1_hrly_pre_l[[i]]$time <- 
    paste(substr(d1_hrly_pre_l[[i]]$time, 1, 2), 
          substr(d1_hrly_pre_l[[i]]$time, 3, 4),
          sep = ":")
  
  d1_hrly_pre_l[[i]]$datetime <-
    as.POSIXct(paste(d1_hrly_pre_l[[i]]$date, d1_hrly_pre_l[[i]]$time), 
               format = "%Y-%m-%d %H:%M", tz = "MST")
  
  names(d1_hrly_pre_l)[i] <- 
    as.character(d1_list2[i]) #name the data frame with the name of file
  
  
}

#Pull out the appropriate columns from each data frame in the list and bind them
d1_vars2 <- data.frame("vars" = as.character(""))
for (i in 1:length(d1_hrly_pre_l)){
  tmp <- data.frame("vars" = as.character(colnames(d1_hrly_pre_l[[i]])))
  d1_vars2 <- rbind(d1_vars2, tmp)
}

d1_vars2 <- unique(d1_vars2$vars)
d1_vars2 <- as.character(d1_vars2[-c(1, 9, 25:30)])

d1_hrly_df2 <- data.frame("dummy" = 0)

for (i in 1:length(d1_vars2)){
  
  vartmp <- d1_vars2[i] #name the variable for import
  lengthtmp <- 1 #create temporary length value (to be used for assigning rows on data import)
  
  for (j in 1: length(d1_hrly_pre_l)){
    tmpfile <- names(d1_hrly_pre_l)[j]
    if (vartmp %in% colnames(d1_hrly_pre_l[[j]]) == TRUE){ #if variable in the column names
      
      d1_hrly_df2[lengthtmp: (length(d1_hrly_pre_l[[j]]$datetime) + lengthtmp - 1), vartmp] <- #import variable
        d1_hrly_pre_l[[j]][ , vartmp]
      
      print(paste(tmpfile, vartmp, "TRUE"))
    } else {
      
      d1_hrly_df2[lengthtmp: (length(d1_hrly_pre_l[[j]]$datetime) + lengthtmp - 1), vartmp] <- #else, set all as NA
        NA
      print(paste(tmpfile, vartmp, "FALSE"))
    }
    
    lengthtmp <- lengthtmp + length(d1_hrly_pre_l[[j]]$datetime)
  }
}

#Remove dummy column
d1_hrly_df2$dummy <- NULL


#1991 temperatures were imported as factors
#Fixed by removing ???? entry from DOY 114 and rerunning above code

#Format datetime
d1_hrly_df2$datetime <- as.POSIXct(d1_hrly_df2$datetime, 
                                   origin = "1970-01-01 00:00.00 UTC", 
                                   tz = "MST")

#Remove all duplicate rows
d1_hrly_df2 <- unique(d1_hrly_df2)

#Search for duplicate time stamps
d1_pre_dupes <- subset(d1_hrly_df2, duplicated(d1_hrly_df2$datetime))

#After removing previous dupes, the only ones left are from 1993-08-25 and 2012-12-16 @1100

#1993 dupes caused by a shift in the DOY in the file
#Data gap in July, data begins recording on DOY 201, but listed as 202 on file
#Correct DOY numbering starts on DOY 237, giving two DOY 237 entries (the 1993-08-25 dupes)
#DOY 202 to DOY 237 (first set) must be corrected by subtracting 1
d1_1993_error <- subset(d1_hrly_df2, doy >= 202 & doy <= 237 & as.numeric(format(datetime, "%Y")) == 1993)
d1_1993_error[1:853, "doy"] <- d1_1993_error[1:853, "doy"] - 1
d1_1993_error$date <- 
  as.Date(paste("1993", d1_1993_error$doy, sep = "-"),
          format = "%Y-%j")
d1_1993_error$datetime <-  as.POSIXct(paste(d1_1993_error$date, d1_1993_error$time), 
                                      format = "%Y-%m-%d %H:%M", tz = "MST")

d1_hrly_df2 <- subset(d1_hrly_df2, datetime <= as.POSIXct("1993-07-20 00:00:00") | datetime >= as.POSIXct("1993-08-26 01:00:00"))
d1_hrly_df2 <- rbind(d1_hrly_df2, d1_1993_error)
d1_hrly_df2 <- unique(d1_hrly_df2)
d1_hrly_df2 <- subset(d1_hrly_df2, row.names(d1_hrly_df2) != 31155)

#2012 dupe caused by data recorder (likely)
#Lots of data gaps around end of 2012
#Second 2012-12-16 @1100 entry removed
#Records nearly identical, probably recorded at near same time
d1_hrly_df2 <- subset(d1_hrly_df2, row.names(d1_hrly_df2) != 195292)



#Combine old and new data into a list
d1_hrly_all_l <- list()
d1_hrly_all_l[[1]] <- d1_hrly_df
d1_hrly_all_l[[2]] <- d1_hrly_df2

#Make blank data frame and import all data into 1
d1_hrlydf3 <- data.frame("dummy" = 0)

for (i in 1:length(d1_vars2)){
  
  vartmp <- d1_vars2[i] #name the variable for import
  lengthtmp <- 1 #create temporary length value (to be used for assigning rows on data import)
  
  for (j in 1: length(d1_hrly_all_l)){
    tmpfile <- names(d1_hrly_all_l)[j]
    
    if (vartmp %in% colnames(d1_hrly_all_l[[j]]) == TRUE){ #if variable in the column names
      
      d1_hrlydf3[lengthtmp: (length(d1_hrly_all_l[[j]]$datetime) + lengthtmp - 1), vartmp] <- #import variable
        d1_hrly_all_l[[j]][ , vartmp]
      
      print(paste(tmpfile, vartmp, "TRUE"))
    } else {
      
      d1_hrlydf3[lengthtmp: length(d1_hrly_all_l[[j]]$datetime), vartmp] <- #else, set all as NA
        NA
      print(paste(tmpfile, vartmp, "FALSE"))
    }
    
    lengthtmp <- lengthtmp + length(d1_hrly_all_l[[j]]$datetime)
  }
}

#Remove dummy column
d1_hrlydf3$dummy <- NULL

#Format datetime
d1_hrlydf3$datetime <- as.POSIXct(d1_hrlydf3$datetime, origin = "1970-01-01 00:00.00 UTC", tz = "MST")

#Make regular sequence covering dataset at 1 h timestep
#Hourly data run from 1988-08-22 11:00 through end of 2014
#Earlier 2h and 30min data will be ignored for now
d1_datetime3 <- data.frame("datetime" = seq.POSIXt(from = as.POSIXct("1988-08-22 11:00", tz = "MST"),
                                                   to = as.POSIXct("2014-12-31 23:00"),
                                                   by = "1 hour"))


#Merge data observations with regular time series
#And sort by datetime
d1_hrly_merged <- merge(d1_datetime3, d1_hrlydf3, by = "datetime", all.x = TRUE)
d1_hrly_merged <- d1_hrly_merged[ order(d1_hrly_merged$datetime),]

#Add in date info
d1_hrly_merged$doy <- as.numeric(format(d1_hrly_merged$datetime, "%j"))
d1_hrly_merged$hour <- as.numeric(format(d1_hrly_merged$datetime, "%H"))
d1_hrly_merged$date <- as.character(format(d1_hrly_merged$datetime, "%Y-%m-%d"))
d1_hrly_merged$time <- as.character(format(d1_hrly_merged$datetime, "%H:%M"))
d1_hrly_merged$year <- as.numeric(format(d1_hrly_merged$datetime, "%Y"))

#Put all date info in left columns
d1_hrly_merged <- d1_hrly_merged[ , c(15, 13, 2, 12, 14, 1,
                                      3:11,
                                      16:65)]

#Replace all -6999 and 6999 entries w/ NA
d1_hrly_merged[d1_hrly_merged == -6999] <- NA
d1_hrly_merged[d1_hrly_merged == 6999] <- NA

#Export data as CSV
setwd(dir = "~/Documents/research/site_data/niwot/d1_hourly_raw_data_files/")
write.csv(d1_hrly_merged, "d1_hrly_met_data_all_NOQC.csv", row.names = FALSE, quote = FALSE)
