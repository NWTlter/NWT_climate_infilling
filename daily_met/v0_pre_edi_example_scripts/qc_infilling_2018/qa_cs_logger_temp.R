# compile and qa campbell scientific saddle logger temp, all years
# (streamlined version after comparing all saddle temperature datasets)


# script purpose:
# read in all campbell scientific logger datasets, and sdl, d1, and c1 chart temp datasets for comparison
# tidy datasets
# determine routine checks to flag suspect values relative to nearby temp trends (i.e. comparative datasets)
# flag values
# write out QA dataset with flags for next step (infilling)


# notes:
# color scheme in panel figures:
# blue = d1 chart, purple = sdl chart, green = c1 chart, black = sdl cr loggers



# -- SETUP ------
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN")

# set pathway to extsum analysis folder
datpath <- "extended_summer/analysis/"

# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")



# -- GET DATA -----
#campbell logger 1990- # no colnames in data file so read in from copy/paste url
cr21x <- read_csv("http://pasta.lternet.edu/package/data/eml/knb-lter-nwt/78/2/74edfb5a907b5d4960d1e1dbe08faba4", col_names = FALSE, na = na_vals, trim_ws = T) %>% as.data.frame()
cr21xeml <- readLines("https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-nwt.78.2&contentType=application/xml")
cr21x_names <- cr21xeml[grep("attributeName", cr21xeml)] %>%
  str_extract(">.*<") %>% 
  gsub("<|>", "",.)
#set colnames for cr21x data
colnames(cr21x) <- cr21x_names

# cr23x and cr1000 logger data
#crlogs <- getTabular() # most current logger dataset not on EDI yet, provided by SCE
crlogs <- read.csv(paste0(datpath,"/raw_data/sdlcr23x-cr1000.daily.ml.data.csv"),
                   strip.white = T, na.strings = na_vals)
# sdl chart
sdl <- getTabular(413) %>% as.data.frame()
# d1 chart
d1 <- getTabular(412) %>% as.data.frame()
# c1 chart
c1 <- getTabular(411) %>% as.data.frame()

# d1 cr loggers -- look at d1 logger for delta diff qa
## d1 cr21x, 1986-2000
d1cr21 <- getTabular(70)
## d1 cr23x and cr1000, 2000 - ongoing
d1cr <- getTabular(402) %>% as.data.frame() 



# -- REVIEW DATA -----
# review how dats read in
glimpse(cr21x) # no flag cols, date is date class
glimpse(crlogs) # flag cols present, date is character
glimpse(sdl) # flags cols present
glimpse(d1) # flag cols present
glimpse(c1) # flag cols present
glimpse(d1cr) #flag cols present
glimpse(d1cr21) # flag cols present, colnames similar to others -- time cols but no flags

# clean up sdl cr23x and cr1000 data
# drop cols not needed in cr logger dataset (i.e. only need airtemp cols up thru avg airtemp, flag cols not useful since only n or NA)
crlogs <- crlogs[,1:12]
# convert date to Date class
crlogs$date <- as.Date(crlogs$date, format = "%Y-%m-%d")
# unique values of flags? and frequency of their occurrence?
sapply(crlogs[grepl("flag", colnames(crlogs))], function(x) summary(as.factor(x))) # only n's, correspond to no flag -- not useful
# drop flag cols since nothing flagged, add month col, and clean up names
crlogs <- mutate(crlogs, mon = month(date)) %>%
  rename(doy = jday,
         yr = year) %>%
  dplyr::select(LTER_site:date, yr, mon, doy, airtemp_max, airtemp_min, airtemp_avg)

# clean up 21x data 
# drop non airtemp cols in cr21x
cr21x <- cr21x[grepl("date|Julian|maximum temp|minimum temp|average temp", colnames(cr21x))] %>%
  # add logger, LTER_site, and local_site cols so simlar to other datasets
  mutate(LTER_site = "NWT",
         local_site = "sdl", 
         logger = "cr21x",
         yr = year(date),
         mon = month(date)) %>%
  # rename cols to match other datasets
  rename(airtemp_max = 'maximum temperature',
         airtemp_min = 'minimum temperature',
         airtemp_avg = 'average temperature',
         doy = 'Julian day') %>%
  # select same cols as in cr23x and cr1000 datasets + time of temp cols
  dplyr::select(c(colnames(crlogs), cr21x_names[grepl("^time.*temperature", cr21x_names)])) %>%
  rename(time_airtemp_max = 'time of maximum temperature',
         time_airtemp_min = 'time of minimum temperature')
# clean up 
rm(cr21x_names, cr21xeml)


# clean up d1 logger datasets
# cr23x and cr1000 data logger, drop cols not needed (i.e. only need airtemp cols up thru avg airtemp, flag cols not useful since only n or NA)
d1cr <- d1cr[,1:12]
# unique values of flags? and frequency of their occurrence?
sapply(d1cr[grepl("flag", colnames(d1cr))], function(x) summary(as.factor(x))) # only n's, correspond to no flag -- not useful
# drop flag cols since nothing flagged, add month col, and clean up names
d1cr <- mutate(d1cr, mon = month(date)) %>%
  rename(doy = jday,
         yr = year) %>%
  dplyr::select(LTER_site:date, yr, mon, doy, airtemp_max, airtemp_min, airtemp_avg)
# d1 cr21x
d1cr21 <- d1cr21 %>%
# drop times cols, add month col, and clean up names
 mutate(mon = month(date),
        LTER_site = "NWT",
        local_site = "d1") %>%
  rename(doy = jday,
         yr = year) %>%
  dplyr::select(LTER_site, local_site, logger, date, yr, mon, doy, airtemp_max, airtemp_min, airtemp_avg)
# stack d1cr21 and d1cr23 onwards datasets
d1cr <- rbind(d1cr21, d1cr)
rm(d1cr21) #not needed anymore


# review flags in reference datasets 
# sdl
sapply(sdl[grepl("flag", colnames(sdl))], function(x) summary(as.factor(x))) # 2 types of flags, type 1 used most often
# > saddle chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)
# d1
sapply(d1[grepl("flag", colnames(d1))], function(x) summary(as.factor(x))) # 3 types of flags.. type 1 and type 2 nearly equally used
# > d1 chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available); 3 not explained
# c1
sapply(c1[grepl("flag", colnames(c1))], function(x) summary(as.factor(x))) # 2 types of flags (same as sdl), mostly infilled via type 1
# > c1 chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)
# >> conclusion: keep flag cols in chart temp datasets, useful info






# -- TIDY TEMPERATURE DATASETS -----
# function to tidy temp (this could be made generic for ppt too..)
tidytemp <- function(dat, datasource = NA, sep = "_", special = "flag", dropcol = NA){
  #if cols to drop, drop
  if(!is.na(dropcol)){
    dat <- dat[!colnames(dat) %in% dropcol] 
  }
  
  # gather temp and any special cols
  # id start of temp cols
  temp_pos <- min(grep("temp", colnames(dat)))
  dat_long <- dat %>%
    gather(met, temp, temp_pos:ncol(.)) %>%
    arrange(met, date)
  
  # if special cols exist, pull out special cols and rejoin wide-form
  if(!is.na(special)){
    tempspecial <- dat_long %>%
      filter(grepl(special, met)) %>%
      mutate(met = gsub(paste0(special,"_"), "", met))
    # rename temp col as special val
    colnames(tempspecial)[which(colnames(tempspecial) == "temp")] <- special
    
    # drop special vals from long-form dat and join wide to temp vals
    dat_long <- subset(dat_long, !grepl(special, met)) %>%
      # add month and year
      mutate(yr = year(date),
             mon = month(date),
             doy = yday(date)) %>%
      left_join(tempspecial) %>%
      dplyr::select(LTER_site:date, yr:doy, met:ncol(.)) 
  }
  
  # if desired, prefix temp and special col colname with datasource
  if(!is.na(datasource)){
    colnames(dat_long)[colnames(dat_long) %in% c("temp", special)] <- paste(datasource, colnames(dat_long)[colnames(dat_long) %in% c("temp", special)], sep = sep)
  }
  
  # return tidy dataset and clean up environment
  return(dat_long)
  rm(tempspecial, temp_pos)
}

# tidy chart temp datatsets
sdl_long <- tidytemp(sdl, datasource = "sdl", dropcol = "airtemp_avg")
d1_long <- tidytemp(d1, datasource = "d1", dropcol = "airtemp_avg")
c1_long <- tidytemp(c1, datasource = "c1", dropcol = "airtemp_avg")

# tidy logger datasets
cr21x_long <- tidytemp(cr21x, datasource = "cr", special = "time", dropcol = "airtemp_avg")
crlogs_long <- tidytemp(crlogs, datasource = "cr", special = NA, dropcol = "airtemp_avg")
d1cr_long <- tidytemp(d1cr, datasource = "d1cr", special = NA, dropcol = "airtemp_avg")

# screen any obvious outliers in chart or d1 logger datasets (i.e. don't use these values to compare with sdl logger data points)
## d1 chart
with(d1_long, sapply(split(d1_temp, met), summary))
with(d1_long, lapply(split(d1_temp[met == "airtemp_max"], mon[met=="airtemp_max"]), summary))
with(d1_long, sapply(split(d1_temp, met), function(x) tail(sort(x))))
with(d1_long, sapply(split(d1_temp, met), function(x) head(sort(x)))) # all okay
## d1 logger
with(d1cr_long, lapply(split(d1cr_temp[met == "airtemp_max"], logger[met=="airtemp_max"]), summary)) # 90??
with(d1cr_long, lapply(split(d1cr_temp[met == "airtemp_max"], logger[met=="airtemp_max"]), function(x) tail(sort(x), n = 15))) # 90??
# it looks like there are still F units in the d1 cr21x dataset.. not fixing because this is only for qualitative plotting info, and don't use d1 logger often
# metadata says Hope changed d1 cr21x units to C in Apr 2017, but seem like all got converted?
boxplot(d1cr_temp ~ yr, data = subset(d1cr_long, logger == "cr21x" & met == "airtemp_max")) #1997 & 1999 questionable
boxplot(d1cr_temp ~ mon, data = subset(d1cr_long, logger == "cr21x" & met == "airtemp_max" & yr == 1997)) #aug - oct 1997
boxplot(d1cr_temp ~ mon, data = subset(d1cr_long, logger == "cr21x" & met == "airtemp_max" & yr == 1999)) #sep & oct again
boxplot(d1cr_temp ~ yr, data = subset(d1cr_long, logger == "cr21x" & met == "airtemp_min")) #1997
boxplot(d1cr_temp ~ mon, data = subset(d1cr_long, logger == "cr21x" & met == "airtemp_min" & yr == 1997)) #sep & oct again
with(d1cr_long, lapply(split(d1cr_temp[met == "airtemp_min"], logger[met=="airtemp_min"]), function(x) head(sort(x), n = 25))) # -6999, some clear sensor fails in the cr21x logger
# bc just looking at d1 logger qualitatively with sdl logger, for simplicity NA anything < -50 and > 40 + sep + oct in 1997 and 1999
# NA temp < -50
d1cr_long$d1cr_temp[d1cr_long$d1cr_temp < -50 & !is.na(d1cr_long$d1cr_temp)] <- NA
d1cr_long$d1cr_temp[d1cr_long$d1cr_temp > 40 & !is.na(d1cr_long$d1cr_temp)] <- NA


## sdl chart
with(sdl_long, sapply(split(sdl_temp, met), summary))
with(sdl_long, lapply(split(sdl_temp[met == "airtemp_max"], mon[met=="airtemp_max"]), summary))
with(sdl_long, sapply(split(sdl_temp, met), function(x) tail(sort(x))))
with(sdl_long, sapply(split(sdl_temp, met), function(x) head(sort(x)))) # all okay
## c1 chart
with(c1_long, sapply(split(c1_temp, met), summary))
with(c1_long, sapply(split(c1_temp[met == "airtemp_max"], mon[met=="airtemp_max"]), summary))
with(c1_long, sapply(split(c1_temp, met), function(x) tail(sort(x))))
with(c1_long, sapply(split(c1_temp, met), function(x) head(sort(x)))) # all okay              


# how many tmax temps occurred between 11pm and 1am?
nrow(subset(cr21x_long, met == "airtemp_max" & cr_time > 2300 | cr_time < 100)) #711! boo.
View(subset(cr21x_long, met == "airtemp_max" & cr_time > 2300 | cr_time < 100))
# what is the breakdown of count tmax where timestamp between 2300 & 0100 by month?
group_by(subset(cr21x_long, met == "airtemp_max" & cr_time > 2300 | cr_time < 100), mon) %>%
  summarize(ct = length(cr_time)) # most occur in winter months (e.g. 118 in december), but there are a decent amount in summer months
# what is the breakdown of count tmin where timestamp between 900 & 1700 by month?
group_by(subset(cr21x_long, met == "airtemp_min" & cr_time > 900 & cr_time < 1700), mon) %>%
  summarize(ct = length(cr_time)) # not so many.. but expect 0 (esp in summer months)
# conclusion: time is perhaps not reliable for flagging if clock sometimes malfunctioning. drop and combine cr21x with cr23x and cr1000 logger


# all cr logger datasets -- stack both cr21x and cr23x/cr1000 datasets then tidy
## stack, removing time cols from cr21x dataset
crall_long <- rbind(cr21x_long[,!grepl("time", colnames(cr21x_long))], crlogs_long) %>%
  arrange(met,date)
# clean up
rm(cr21x_long, crlogs_long)

# join comparative chart datasets to cr logger data
crall_long_master <- left_join(crall_long, sdl_long) %>%
  left_join(dplyr::select(d1_long, -local_site)) %>%
  left_join(dplyr::select(c1_long, -local_site))


# quick visual of all logger data as they are from EDI/SCE
ggplot(crall_long, aes(date, cr_temp, col = logger)) +
  geom_point(alpha =0.5) +
  scale_color_viridis_d() +
  facet_wrap(~met) # serious tmin outliers present..

# plot without outliers
# quick visual of all logger data as they are from EDI/SCE - no outliers
crallfig <- ggplot(subset(crall_long, cr_temp > -50 & date < max(sdl_long$date)), aes(date, cr_temp, col = logger)) +
  geom_point(alpha =0.5) +
  scale_color_viridis_d() +
  scale_x_date(breaks = "5 years", date_labels = "%Y") +
  theme(legend.position = "top",
        axis.title.x = element_blank()) + 
  facet_wrap(~met) # serious tmin outliers present..
# raw sdl chart
sdlfig <- ggplot(subset(sdl_long, date > min(crall_long$date)), aes(date, sdl_temp)) +
  geom_point(alpha =0.5) +
  scale_x_date(breaks = "5 years", date_labels = "%Y") +
  facet_wrap(~met)
# plot together for comparison
plot_grid(crallfig, sdlfig, nrow = 2,
          rel_heights = c(1.1,1))



# -- FUNCTION FOR PLOTTING SUSPECT TEMP VALUES -----
# function to panel plot flagged data
visual_qa <- function(dat, qadat, sorttime = "date", add_d1cr = FALSE){
  # initiate list for storing ggplots
  plot_list <- list()
  # id temperature cols in reference data frame
  tempcols <- colnames(dat)[grepl("temp", colnames(dat))]
  
  for(m in c("airtemp_max", "airtemp_min")){
    tempdf <- qadat[qadat$met == m,] %>% as.data.frame()
    # order by preferred time sort (default is date)
    tempdf <- tempdf[order( tempdf[,which(colnames(tempdf) == sorttime)]),]
    
    for(d in as.character(tempdf$date)){
      d <- as.Date(d, format = "%Y-%m-%d")
      tempplot <- ggplot(data = subset(dat, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1))) +
        geom_line(aes(date, cr_temp)) +
        geom_point(aes(date, cr_temp)) +
        # circle the flagged value in red
        geom_point(data = subset(dat, met == m & date == as.Date(d)),
                   aes(date, cr_temp), col = "red", pch  = 1, size = 3) +
        labs(y = gsub("airtemp_", "T", m),
             x = d) +
        # add sdl chart temp for comparison (purple dots)
        geom_line(aes(date, sdl_temp), col = "purple") +
        geom_point(aes(date, sdl_temp), col = "purple", pch = 1) +
        geom_line(aes(date, d1_temp), col = "steelblue2") +
        geom_point(aes(date, d1_temp), col = "steelblue4", pch = 1) +
        geom_line(aes(date, c1_temp), col = "forestgreen") +
        geom_point(aes(date, c1_temp), col = "darkgreen", pch = 1) +
        theme_bw()
      
      if(add_d1cr){
        tempplot <- tempplot + geom_line(data = subset(d1cr_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)),
                                         aes(date, d1cr_temp), col = "goldenrod1") + 
          geom_point(data = subset(d1cr_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)),
                     aes(date, d1cr_temp), col = "goldenrod4", pch = 1)
      }
      
      # store plot in a list
      plot_list[length(plot_list)+1] <- list(tempplot)
    }
  }
  return(plot_list)
}


# -- QA SENSOR FAILS (OBVIOUS OUTLIERS) -----
# create working copy
working_dat <- crall_long_master
# add empty col for qa flags added
working_dat$qa_flag <- NA

# look at tails for any obvious bad values
## logger temp, split by logger
with(working_dat, lapply(split(cr_temp, paste(met, logger)), function(x) tail(sort(x)))) #31.45.. Jen Morse said she thinks max T shouldn't exceed 30
with(working_dat, lapply(split(cr_temp, paste(met, logger)), function(x) tail(sort(x, decreasing = T)))) #cr21x: -75 and -6999 in airtemp_min; cr1000: -187 tmin

## chart data
#sdl
with(working_dat, lapply(split(sdl_temp, met), function(x) tail(sort(x))))
with(working_dat, lapply(split(sdl_temp, met), function(x) tail(sort(x, decreasing = T)))) #-38 is kind of a jump from other tmin, but not impossible value
#d1
with(working_dat, lapply(split(d1_temp, met), function(x) tail(sort(x))))
with(working_dat, lapply(split(d1_temp, met), function(x) tail(sort(x, decreasing = T))))
#c1
with(working_dat, lapply(split(c1_temp, met), function(x) tail(sort(x))))
with(working_dat, lapply(split(c1_temp, met), function(x) tail(sort(x, decreasing = T))))

# flag: -187 in cr1000; anything < -70 in cr21x (-75 and -6999)
working_dat$qa_flag[working_dat$cr_temp < -50 & !is.na(working_dat$cr_temp)] <- "sensor fail"
# remove bad values from working cr_temp before moving on
working_dat$cr_temp[!is.na(working_dat$qa_flag)] <- NA


# -- FLAG DAILY DEPARTURES FROM COMPARATIVE DATASETS -----
# function to difference daily logger temp from comparative chart dataset daily temps
diff_daily <- function(dat){
  dat %>%
    mutate(cr_diff_sdl = abs(cr_temp-sdl_temp),
           cr_diff_d1 = abs(cr_temp-d1_temp),
           cr_diff_c1 = abs(cr_temp-c1_temp)) %>%
    group_by(logger, met, mon) %>%
    # set threshold for sdl logger deviance at 3sd away from the absolute average difference (by logger, metric, and month)
    mutate(thresh_diff_sdl = mean(cr_diff_sdl, na.rm = T) + (3*sd(cr_diff_sdl, na.rm = T)),
           thresh_diff_d1 = mean(cr_diff_d1, na.rm = T) + (3*sd(cr_diff_d1, na.rm = T)),
           thresh_diff_c1 = mean(cr_diff_c1, na.rm = T) + (3*sd(cr_diff_c1, na.rm = T)),
           sd_diff_sdl = sd(cr_diff_sdl, na.rm = T),
           sd_diff_d1 = sd(cr_diff_d1, na.rm = T),
           sd_diff_c1 = sd(cr_diff_c1, na.rm = T)) %>%
    ungroup() %>%
    # flag logger value if exceeds daily diff threshold for chart comparative datasets
    mutate(flag_diffsdl = cr_diff_sdl > thresh_diff_sdl,
           deviance_sdl = cr_diff_sdl/sd_diff_sdl,
           flag_diffd1 = cr_diff_d1 > thresh_diff_d1,
           deviance_d1 = cr_diff_d1/sd_diff_d1,
           flag_diffc1 = cr_diff_c1 > thresh_diff_c1,
           deviance_c1 = cr_diff_c1/sd_diff_c1)
}

# round 1 diff daily and flagging
working_dat <- diff_daily(working_dat)  
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff1 <- filter(working_dat, flag_diffsdl == T & flag_diffd1 == T & flag_diffc1 == T)
# run through visual qa
qa_daily_diff1 <- visual_qa(working_dat, check_daily_diff1)
plot_grid(plotlist = qa_daily_diff1) 
# round 1: all look bad excep tmin on 1996-12-19 (looks kind of of like temps ot shifted fwd one day for that week?) 
flag_dailydiff1 <- subset(check_daily_diff1, date!= "1996-12-19")

# function for flagging and removing high values in working dataset
flag_temp <- function(flagdat, error = "comparative deviance"){
  tempdat <- flagdat
  for(row in 1:nrow(tempdat)){
    pos <- with(working_dat, which(met == tempdat$met[row] & logger == tempdat$logger[row] & date == tempdat$date[row]))
    working_dat$qa_flag[pos] <- error
    working_dat$cr_temp[pos] <- NA 
  }
  return(working_dat)
}

# flag and remove values in working dataset
working_dat <- flag_temp(flag_dailydiff1, error = "comparative deviance")

# round 2 daily diff (recalc mean and deviance with bad deviance values from round 1 removed)
working_dat <- diff_daily(working_dat)
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff2 <- filter(working_dat, flag_diffsdl == T & flag_diffd1 == T & flag_diffc1 == T)
# run through visual qa
qa_daily_diff2 <- visual_qa(working_dat, check_daily_diff2)
plot_grid(plotlist = qa_daily_diff2) 
# > round 2: 
# > 1990-08-18: flag and remove, other datasets were staying flat or getting warmer
# > leave 1992-07-02 value (low, but entire 20 day period shows logger cooler in tmin than chart sources)
# > 1996-12-19: same from round 1, shift 1996-12-17 to 1996-12-30 back by 1 day so lines up with chart trends

working_dat <- flag_temp(check_daily_diff2[check_daily_diff2$date == "1990-08-18",], "comparative deviance")
# manual adjustment to cr_temp tmin vals in dec 1996
## view both tmin and tmax before adjusting
subset(working_dat, date > "1996-12-13" & date < "1997-01-13") %>%
  dplyr::select(date, met, cr_temp, sdl_temp) %>%
  gather(datsource, temp_c, cr_temp:ncol(.)) %>%
  ggplot(aes(date, temp_c, col = datsource)) +
  geom_line() +
  geom_point(alpha = 0.6) +
  scale_color_viridis_d() +
  facet_wrap(~met)
# > it looks like both tmin and tmax could be shifted back 1 day
# > select 1996-12-17 through 1997-jan-06 since everything in early jan is NA anyway (i.e. doesn't matter if NA is shifted to an NA)  
View(subset(working_dat, date %in% seq(as.Date("1996-12-15"), as.Date("1997-01-06"), 1)))
# looks like start 12/17 to 1/5, shift back 1 day
shift_dates <- seq(as.Date("1996-12-17"), as.Date("1997-01-05"), 1)
working_dat$qa_flag[working_dat$date %in% (shift_dates-1)] <- "shifted temp -1 day"
working_dat$cr_temp[working_dat$met == "airtemp_max" & working_dat$date %in% (shift_dates-1)] <- working_dat$cr_temp[working_dat$met == "airtemp_max" & working_dat$date %in% (shift_dates)]
working_dat$cr_temp[working_dat$met == "airtemp_min" & working_dat$date %in% (shift_dates-1)] <- working_dat$cr_temp[working_dat$met == "airtemp_min" & working_dat$date %in% (shift_dates)]

# plot again to ensure check temps adjusted as expected
subset(working_dat, date > "1996-12-13" & date < "1997-01-13") %>%
  dplyr::select(date, met, cr_temp, sdl_temp) %>%
  gather(datsource, temp_c, cr_temp:ncol(.)) %>%
  ggplot(aes(date, temp_c, col = datsource)) +
  geom_line() +
  geom_point(alpha = 0.6) +
  scale_color_viridis_d() +
  facet_wrap(~met) #yes


# round 3: check extreme sdl deviance values
working_dat <- diff_daily(working_dat)
# check logger temps that exceed daily deviance more than 5sds for sdl_chart and d1_chart
check_daily_diff3 <- filter(working_dat, flag_diffsdl == TRUE & flag_diffd1 == T & round(deviance_c1) >= 4)
# run through visual qa
qa_daily_diff3 <- visual_qa(working_dat, check_daily_diff3)
plot_grid(plotlist = qa_daily_diff3)
# > sdl chart flatlines in tmin so ignore Sep 1990 tmin, leave 1992 tmin be
# > all tmax vals can be flagged and removed -- are clear spikes or otherwise off
working_dat <- flag_temp(check_daily_diff3[check_daily_diff3$met == "airtemp_max",], "comparative deviance")

# move on to checking grand and monthly tmax and tmin vals..
#clean up environment
rm(check_daily_diff1, flag_dailydiff1, check_daily_diff2, check_daily_diff3,
   qa_daily_diff1, qa_daily_diff2, qa_daily_diff3)



# -- QA EXTREMES (TMIN/TMAX) -----
## IMPORTANTE!!: 
## logger lifecycles: cr21x = 1980s-2000; cr23x = 2000 - 2012; cr1000 = dec 2012 - ongoing
## panel arrangement: cr21x = left panel, cr23x = middle, cr1000 = right panel

# visual qa grand max and min
# max temps by logger
check_max1 <- working_dat %>%
  group_by(logger, met) %>%
  filter(cr_temp == max(cr_temp, na.rm = T))
qa_max1 <- visual_qa(working_dat, check_max1)
plot_grid(plotlist = qa_max1) 
# >round 1: cr23x tmax 2000-09-06 seems unlikely given trends in c1, sdl and d1, all else fine 
flag_max1 <- filter(check_max1, met == "airtemp_max" & logger == "cr23x" & date == "2000-09-06")
working_dat <- flag_temp(flag_max1, error = "high value")
# run through grand max once more
check_max2 <- working_dat %>%
  group_by(logger, met) %>%
  filter(cr_temp == max(cr_temp, na.rm = T))
qa_max2 <- visual_qa(working_dat, check_max2)
plot_grid(plotlist = qa_max2) #looks okay

# min temps by logger
check_min1 <- working_dat %>%
  group_by(logger, met) %>%
  filter(cr_temp == min(cr_temp, na.rm = T))
qa_min1 <- visual_qa(working_dat, check_min1)
plot_grid(plotlist = qa_min1) #temperature minimums look good

# move on to monthlies...
#clean up env
rm(check_max1, check_max2, qa_max1, qa_max2,
   check_min1, qa_min1)



# -- QA MONTHLY MIN/MAX -----
# monthly max temps of tmin and tmax
check_monthly_max1 <- working_dat %>%
  group_by(logger, met, mon) %>%
  filter(cr_temp == max(cr_temp, na.rm = T))
# run through visual qa function
qa_mon_max1 <- visual_qa(working_dat, check_monthly_max1, sorttime = "mon")

# visualize logger monthly maximums, by logger and metric
## max of airtemp_max
plot_grid(plotlist = qa_mon_max1[grep("cr21x.*airtemp_max", qa_mon_max1)]) #flag sep (1987-09-01) and nov (11-29-1999) 
plot_grid(plotlist = qa_mon_max1[grep("cr23x.*airtemp_max", qa_mon_max1)]) #flag dec (2001-12-26)
plot_grid(plotlist = qa_mon_max1[grep("cr1000.*airtemp_max", qa_mon_max1)]) #okay
## max of airtemp_min
plot_grid(plotlist = qa_mon_max1[grep("cr21x.*airtemp_min", qa_mon_max1)]) #okay
plot_grid(plotlist = qa_mon_max1[grep("cr23x.*airtemp_min", qa_mon_max1)]) #okay
plot_grid(plotlist = qa_mon_max1[grep("cr1000.*airtemp_min", qa_mon_max1)]) #okay.. dec looks a little different, but since sdl chart missing not taking action

# monthly min temps of tmin and tmax
check_monthly_min <- working_dat %>%
  group_by(logger, met, mon) %>%
  filter(cr_temp == min(cr_temp, na.rm = T))
# run through visual qa function
qa_mon_min1 <- visual_qa(working_dat, check_monthly_min, sorttime = "mon")

# visualize logger monthly minimums, by logger and metric
## min of airtemp_max
plot_grid(plotlist = qa_mon_min1[grep("cr21x.*airtemp_max", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr23x.*airtemp_max", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr1000.*airtemp_max", qa_mon_min1)]) #okay
## min of airtemp_min
plot_grid(plotlist = qa_mon_min1[grep("cr21x.*airtemp_min", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr23x.*airtemp_min", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr1000.*airtemp_min", qa_mon_min1)]) #okay


# > general observation: seems like deviances from chart data occurs more in tmax vals than tmin vals (even max of tmin not so bad, but lots of problems in max of tmax)
# flag and remove noted monthly max values from cr21x (sep and nov) and cr23x (dec)
flag_mon_max1 <- subset(check_monthly_max1, met == "airtemp_max" & (logger == "cr21x" & mon %in% c(9,11) |
                                                                     logger == "cr23x" & mon == 12))
# note the diffs are flagged for sdl and d1 in these observations:
dplyr::select(flag_mon_max1, date, logger, met, flag_diffsdl:ncol(flag_mon_max1))
# flag and remove from working copy
working_dat <- flag_temp(flag_mon_max1, error = "high value")

# check monthly max again to be sure
check_monthly_max2 <- working_dat %>%
  group_by(logger, met, mon) %>%
  filter(cr_temp == max(cr_temp, na.rm = T))
# run through visual qa function
qa_mon_max2 <- visual_qa(working_dat, check_monthly_max2, sorttime = "mon")
# visualize cr21x and cr23x only (what was adjusted)
plot_grid(plotlist = qa_mon_max2[grep("cr21x.*airtemp_max", qa_mon_max2)]) #okay
plot_grid(plotlist = qa_mon_max2[grep("cr23x.*airtemp_max", qa_mon_max2)]) #okay

# clean up environment and move on..
rm(flag_mon_max1, check_monthly_max1, check_monthly_max2, check_monthly_min,
   qa_mon_max1, qa_mon_max2, qa_mon_min1)



# -- QA LAG SPIKES/FLATLINES WITHIN LOGGER DATASET ----
# look for consecutive day temp spikes/declines that exceed 40C within the same metric
# also look for flatlining in temperature (jen morse said try 5-consec days)

# save copy of working dat in case mess up (so don't need to re-run from top)
working_dat_copy <- working_dat

## add lag temp
working_dat <- working_dat %>%
  arrange(met, date) %>%
  group_by(logger, met) %>%
  mutate(lag1_crtemp = lag(cr_temp)) %>%
  ungroup() %>%
  mutate(lag1_diffcr = abs(cr_temp - lag1_crtemp))
# check distribution of absolute difference with current day's temp and yesterday's temp
boxplot(working_dat$lag1_diffcr)  
sapply(split(working_dat$lag1_diffcr, working_dat$met), function(x) tail(sort(x))) # at most swung 19 degrees.. could happen
sapply(split(working_dat$lag1_diffcr, working_dat$logger), function(x) tail(sort(x))) # loggers swing similarly

# can visualize swings to be sure
swing_check1 <- subset(working_dat, lag1_diffcr >= 15) 
qa_swings <- visual_qa(working_dat, swing_check1)
plot_grid(plotlist = qa_swings)
# > conclusion: other sources swung similar amounts too, is fine

# look for 4+ consecutive days of 0 change
count0 <- rle(working_dat$lag1_diffcr)
consec0 <- count0$lengths[count0$values == 0]
consec0[!is.na(consec0)] # 0 change in consec days only ever occurs for runs of 1 day, which is fine

# to be sure, compare lead delta
working_dat %>%
  arrange(met, date) %>%
  group_by(logger, met) %>%
  mutate(lead1_crtemp = lead(cr_temp)) %>%
  ungroup() %>%
  mutate(lead_diffcr = abs(cr_temp - lead1_crtemp)) %>%
  subset(lead_diffcr == 0 & lag1_diffcr == 0) %>%
  nrow() # nada, all clear

# clean up environment
rm(count0, consec0, swing_check1, qa_swings)



# -- QA DAY-TO-DAY DELTA DEVIANCE -----
# diff current from lag temp in sdl chart, d1 and c1, then compare daily deltas with logger daily deltas
# pull out observations where delta deviates more than 3sd of logger-other source diff on day-to-day fluxes
working_dat_copy <- working_dat

working_dat <- working_dat %>%
  # lag by metric (already ordered by date above)
  group_by(met) %>%
  # lag sdl, d1, and c1 chart
  mutate(lag1_sdltemp = lag(sdl_temp),
         lag1_d1temp = lag(d1_temp),
         lag1_c1temp = lag(c1_temp)) %>%
  ungroup() %>%
  # take difference from current day - prior day temp
  mutate(lag1_diffsdl = abs(sdl_temp - lag1_sdltemp),
         lag1_diffd1 = abs(d1_temp - lag1_d1temp),
         lag1_diffc1 = abs(c1_temp - lag1_c1temp),
         delta_lag_crsdl = lag1_diffcr - lag1_diffsdl,
         delta_lag_crd1 = lag1_diffcr - lag1_diffd1,
         delta_lag_crc1 = lag1_diffcr - lag1_diffc1) %>%
  #diff the lag differences and assess whether data-source differences outside normal range of difference given the month and logger
  group_by(logger, met) %>%
  mutate(mean_lagdiff_crsdl = mean(delta_lag_crsdl, na.rm = T),
         sd_lagdiff_crsdl = sd(delta_lag_crsdl, na.rm = T),
         thresh_lagdiff_crsdl = (mean_lagdiff_crsdl) + (3 * sd_lagdiff_crsdl),
         mean_lagdiff_crd1 = mean(delta_lag_crd1, na.rm = T),
         sd_lagdiff_crd1 = sd(delta_lag_crd1, na.rm = T),
         thresh_lagdiff_crd1 = (mean_lagdiff_crd1) + (3 * sd_lagdiff_crd1),
         mean_lagdiff_crc1 = mean(delta_lag_crc1, na.rm = T),
         sd_lagdiff_crc1 = sd(delta_lag_crc1, na.rm = T),
         thresh_lagdiff_crc1 = (mean_lagdiff_crc1) + (3 * sd_lagdiff_crc1)) %>%
  ungroup() %>%
  mutate(flag_deltadiff_sdl = delta_lag_crsdl > thresh_lagdiff_crsdl,
         flag_deltadiff_d1 = delta_lag_crd1 > thresh_lagdiff_crd1,
         flag_deltadiff_c1 = delta_lag_crc1 > thresh_lagdiff_crc1)

check_deltadiff1 <- subset(working_dat, flag_deltadiff_sdl == TRUE & flag_deltadiff_d1 == T )
#View(subset(working_dat, flag_deltadiff_sdl == TRUE))
qa_lagdiffs1 <- visual_qa(working_dat, check_deltadiff1, add_d1cr = TRUE)
plot_grid(plotlist = qa_lagdiffs1[grep("airtemp_min", qa_lagdiffs1)]) #tmin okay.. some drops but leave be until review better info
plot_grid(plotlist = qa_lagdiffs1[grep("cr21x.*airtemp_max", qa_lagdiffs1)])
plot_grid(plotlist = qa_lagdiffs1[grep("cr23x.*airtemp_max", qa_lagdiffs1)])
plot_grid(plotlist = qa_lagdiffs1[grep("cr10.*airtemp_max", qa_lagdiffs1)])
# differences not compelling enough with d1 logger data added to flag anything.. but perhaps something to come back to
# for the most part logger data tracks with chart at sdl and d1, but there are some tmax spikes in the sdl logger-- are these real spikes or sensor issues?


# -- RUN THROUGH DIFF DAILY ONE MORE TIME AND REVIEW QA'D DATA  ----
# final check
working_dat <- diff_daily(working_dat)
par(mfrow=c(1,2))
boxplot(working_dat$deviance_sdl, main = "daily deviance from sdl")
boxplot(working_dat$deviance_d1, main = "daily dev. from d1")
par(mfrow=c(1,1))
# look at more extreme deviance in both sdl and d1
check_diffdaily <- subset(working_dat, deviance_sdl > 6 & deviance_d1 > 6) 
qa_diffdaily <- visual_qa(working_dat, check_diffdaily, add_d1cr = TRUE)
plot_grid(plotlist = qa_diffdaily)
# > conclusion: flag tmax on 2001-03-30 bc high, departure from all trends, and data missing day after (maybe logger malfunction?)

# flag values and clean up environment
working_dat <- flag_temp(subset(check_diffdaily, date == "2001-03-30" & met == "airtemp_max"))
rm(check_diffdaily, qa_diffdaily)

# one more check, less restrictive
check_diffdaily <- subset(working_dat, flag_diffsdl == T & flag_diffd1 == T) 
qa_diffdaily <- visual_qa(working_dat, check_diffdaily, add_d1cr = TRUE)
plot_grid(plotlist = qa_diffdaily[grep("cr21", qa_diffdaily)])
plot_grid(plotlist = qa_diffdaily[grep("cr23", qa_diffdaily)]) 
# no cr1000 deviances, only in cr21x and cr23x
# > conclusion: flag 2002-11-04, leave other points as they are
working_dat <- flag_temp(subset(check_diffdaily, date == "2002-11-04" & met == "airtemp_max"))
rm(check_diffdaily, qa_diffdaily)


# plot qa'd sdl logger data for review
# logger values with sdl chart values plotted behind in grey as comparison for ranges
ggplot(working_dat) + 
  geom_point(aes(date, sdl_temp), alpha = 0.5, col = "grey80") +
  geom_point(aes(date, cr_temp, col = logger), alpha = 0.6) +
  geom_smooth(aes(date, cr_temp, group = logger), method = "glm", col = "black") +
  scale_color_viridis_d() +
  facet_wrap(~met)

# look at late 80s, early 90s
#tmax
ggplot(subset(working_dat, yr < 1995 & met == "airtemp_max")) + 
  geom_point(aes(doy, sdl_temp), alpha = 0.5, col = "grey80") +
  geom_point(aes(doy, cr_temp, col = logger), alpha = 0.6) +
  geom_smooth(aes(doy, sdl_temp), col = "black") +
  geom_smooth(aes(doy, cr_temp), col = "purple", fill = "orchid") +
  scale_color_viridis_d() +
  facet_wrap(~yr)
#tmin
ggplot(subset(working_dat, yr < 1995 & met == "airtemp_min")) + 
  geom_point(aes(doy, sdl_temp), alpha = 0.5, col = "grey80") +
  geom_point(aes(doy, cr_temp, col = logger), alpha = 0.6) +
  geom_smooth(aes(doy, sdl_temp), col = "black") +
  geom_smooth(aes(doy, cr_temp), col = "purple", fill = "orchid") +
  scale_color_viridis_d() +
  facet_wrap(~yr)
# conclusion: okay to use early years of cr21x, just needs to be infilled with sdl chart regression


# how many points flagged?
with(working_dat, sapply(split(qa_flag, met), function(x) summary(!is.na(x)))) #40 points in both tmax and tmin flagged..
# review
subset(working_dat, !is.na(qa_flag)) %>%
  arrange(date) %>% View()  # turns out coincidentally 40 pts in tmax and tmin flagged, but not necessarily on same day. script worked as expected!



# -- COMPILE AND WRITE OUT FLAGGED/QA'D SDL CR DATASET -----
# want to write out old data with qa'd data for comparison
# maybe also write out final working_dat for documentation

crall_old_new <- rename(crall_long,raw_temp = cr_temp) %>%
  left_join(working_dat) %>%
  rename(qa_temp = cr_temp,
         cr_temp = raw_temp) # rename for plotting purposes

# plot what was corrected/modified using visual_qa
qa_results <- subset(crall_old_new, !is.na(qa_flag))
qa_show <- visual_qa(crall_old_new, qa_results, add_d1cr = TRUE)
ex23_fig <- plot_grid(plotlist = qa_show[grep("cr23.*airtemp_max", qa_show)])
ggdraw(ex23_fig) + draw_label("QA example: SDL CR23x flagged values (red)", x = 0.65, y = 0.1)
ggsave(paste0(datpath, "figs/qa_example.png"), scale = 1.5)


# moving on.. clean up old_new and write out along with final working_dat data frame for reference/documentation
crall_old_new <- dplyr::select(crall_old_new,
                               LTER_site:qa_temp, qa_flag)
# write out qa'd dataset
write_csv(crall_old_new, paste0(datpath, "output_data/prep_data/qa_sdlcr_temp.csv"))
#if want to write out working dat with all flag columns, uncomment this next line (warning: +12MB file)
#write_csv(working_dat, paste0(datpath, "output_data/prep_data/qa_sdlcr_temp_workingreference.csv"))
