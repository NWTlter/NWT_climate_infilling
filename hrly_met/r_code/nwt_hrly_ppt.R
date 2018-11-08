#Niwot hourly precipitation script

#Stations: C1, D1, SDL
#Future iterations may also include Arikaree, Subnivean, and GLV stations

#Time period: 1990-01-01 to 2013-12-31
#Time period to be expanded when further data available

#D1 and C1 1990 to 2010 come from Tim Kittel's dataset
#SDL 1990 to 2008 has been infilled by LTER (i.e. missing days with precip were determined by scaling from D1 and C1 precip)

#No quality checks or infilling on precip from 2011 to 2013 at C1 and D1
#2009 to 2013 for Saddle
#This will be a priority for future work

#Daily precip is temporally disaggregated to hourly by ppt_hr = ppt_dly/24
#More advanced disaggregation may be performed in future

#Keith Jennings
#ksjenni@gmail.com
#2016-06-15

#Load dply for data frame manipulation
library(dplyr)

#Import the relevant datasets
setwd("~/Documents/research/site_data/niwot/nwt_ppt_daily_allsites/")
files <- list.files(pattern = "*.csv")

ppt <- list()
for(i in 1:length(files)){
  ppt[[i]] <- #import files and put into list
    read.csv(file = files[i], header = TRUE)
  ppt[[i]]$site <- #name the sites
    ifelse(substr(files[i], 3, 3) == "_",
           substr(files[i], 1, 2),
           substr(files[i], 1, 3))
  names(ppt)[i] <- #name the list object
    ppt[[i]][1, "site"]
  #Make date column
  #Objects 1 and 3 (Kittel dataset) have month day year columns that must be concatenated
  if(i == 1 | i == 3){
    ppt[[i]]$date <- as.Date(paste(ppt[[i]]$year, ppt[[i]]$month, ppt[[i]]$day, sep = "-"))
  } else{
    ppt[[i]]$date <- as.Date(ppt[[i]]$date, format = "%m/%d/%y")
  }
  #Subset to same period as hourly climate obs
  ppt[[i]] <- subset(ppt[[i]], date >= as.Date("1990-01-01") &
                       date <= as.Date("2013-12-31"))
}

#Make second list
#Add sites to one list object (C1 and D1 take up two in previous list because of Kittel and LTER datasets)
ppt2 <- list()
ppt2[["c1"]] <- full_join(ppt[[1]], ppt[[2]], 
                          by = "date")
ppt2[["d1"]] <- full_join(ppt[[3]], ppt[[4]], 
                          by = "date")
ppt2[["sdl"]] <- ppt[[5]]

#Use Kittel ppt before 2011 and LTER after
ppt2[["c1"]]$ppt <- ifelse(ppt2[["c1"]]$date <= as.Date("2010-12-31"),
                           ppt2[["c1"]]$ppt.x,
                           ppt2[["c1"]]$ppt.y)
ppt2[["c1"]]$ppt_FLAG <-#flag by the dataset type
  ifelse(ppt2[["c1"]]$date <= as.Date("2010-12-31"),
         paste(ppt2[["c1"]]$flag1, ppt2[["c1"]]$flag2 , "K", sep = "-"),
         paste(ppt2[["c1"]]$qdays , "noQC", sep = "-"))

#Use Kittel ppt before 2011 and LTER after
ppt2[["d1"]]$ppt <- ifelse(ppt2[["d1"]]$date <= as.Date("2010-12-31"),
                           ppt2[["d1"]]$ppt.x,
                           ppt2[["d1"]]$ppt.y)
ppt2[["d1"]]$ppt_FLAG <-  #flag by the dataset type
  ifelse(ppt2[["d1"]]$date <= as.Date("2010-12-31"),
         paste(ppt2[["d1"]]$flag1, ppt2[["d1"]]$flag2 , "K", sep = "-"),
         paste(ppt2[["d1"]]$qdays , "noQC", sep = "-"))

#SDL only has one ppt dataset
#SDL experiences overcatch from blowing snow, so PPT must be reduced to 39% in non-summer months
#See Williams et al (1998)
ppt2[["sdl"]]$ppt_PRE <- ppt2[["sdl"]]$ppt
ppt2[["sdl"]]$ppt <- #reduce ppt in non-summer months
  ifelse(as.numeric(format(ppt[["sdl"]]$date, "%m")) %in% 6:9,
         ppt2[["sdl"]]$ppt_PRE,
         ppt2[["sdl"]]$ppt_PRE * 0.39)
ppt2[["sdl"]]$ppt_FLAG <-  ifelse(ppt2[["sdl"]]$date <= as.Date("2008-12-31"),
                                 paste(ppt2[["sdl"]]$qdays, ppt2[["sdl"]]$method_flag , "QC", "RED", sep = "-"),
                                 paste(ppt2[["sdl"]]$qdays , "noQC", "RED", sep = "-"))

ppt_h <- list()
stations <- c("c1", "d1", "sdl")
for(i in 1:length(ppt2)){
  ppt_h[[i]] <- data.frame("datetime" = seq.POSIXt(from = as.POSIXct("1990-01-01 00:00", tz = "MST"),
                                                   to = as.POSIXct("2013-12-31 23:00", tz = "MST"),
                                                   by = "1 hour"))
  ppt_h[[i]]$date <- as.Date(format(ppt_h[[i]]$datetime, "%Y-%m-%d"))
  ppt_h[[i]] <- left_join(ppt_h[[i]], ppt2[[i]][, c("date", "ppt", "ppt_FLAG")],
                          by = "date")
  ppt_h[[i]] <- #add hourly ppt using pipes and dplyr (more efficient than with and merge)
    ppt_h[[i]] %>% 
    group_by(date) %>% 
    mutate(ppt_h = ppt/24)
  ppt_h[[i]]$ppt_h <- #set all NAs to 0
    ifelse(is.na( ppt_h[[i]]$ppt_h),
           0,
           ppt_h[[i]]$ppt_h)
  ppt_h[[i]] <- arrange(ppt_h[[i]], datetime)
  #Add station name
  ppt_h[[i]]$site <- stations[i]
  names(ppt_h)[i] <- stations[i]
}