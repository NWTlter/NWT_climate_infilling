# qa sdl chart for anomalous values

# 2019-06-25: 
# ctw found an outlier june 1982 max T value in the saddle chart that was never flagged and was used in the NWT renewal.
# this is an extra quality check to screen for any other questionable/unreasonable sdl chart temp values 1982-ongoing that are not flagged in the published dataset

# script borrows from qa script develoepd for sdl cr logger datasets

# checks:
# excedances beyond practical thresholds (e.g. jen morse says temp range at sdl 40C to 30C)
# tmin, tmax +3SD departures from monthly means
# tmin, tmax +3SD deparatures from day to day deltas
# flat lines (no change in temp for several day runs)
# comparative deviance (+3SD) from 3 or more nearby temp sources (e.g. D1, C1, loggers) -- this requires some caution because there could be bad values in those datasets as well
# diurnal temp +3SD departure from monthly mean (could signal unusual value in tmax or tmin)

# note:
# extended summer, as of summer 2019, will now rely on sdl logger data instead of chart data *however* sdl chart is used to project logger temp in years 1982-1986 before first cr logger launched
# sdl chart also used to infill sdl logger data when missing



# -- SETUP -----
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
# functions to qa temp
source(paste0(datpath,"scripts/qa_functions.R"))



# -- GET DATA -----
# sdl chart
sdl <- getTabular(413) %>% data.frame()
# d1 chart
d1 <- getTabular(412) %>% data.frame()
# c1 chart
c1 <- getTabular(411) %>% data.frame()

# ctw qa'd sdl logger dataset (ignore infilled values, just used unflaggged published dataset values)
sdllog_qa <- read_csv(paste0(datpath, "output_data/prep_data/qa_sdlcr_temp.csv")) %>% data.frame()

# d1 cr loggers -- look at d1 logger for delta diff qa
## d1 cr21x, 1986-2000
d1cr21 <- getTabular(70) %>% data.frame()
## d1 cr23x and cr1000, 2000 - ongoing
d1cr <- getTabular(402) %>% data.frame() 


# -- REVIEW DATA -----
# review how dats read in
glimpse(sdl) # flags cols present
glimpse(d1) # flag cols present
glimpse(c1) # flag cols present
glimpse(sdllog_qa)
glimpse(d1cr) #flag cols present
glimpse(d1cr21) # flag cols present, colnames similar to others -- time cols but no flags

# remove unneeded cols from d1 logger datasets and stack to form master d1 logger dataset
d1log <- dplyr::select(d1cr21, logger:airtemp_avg) %>%
  mutate(LTER_site = "NWT", local_site = "d1") %>%
  dplyr::select(LTER_site, local_site, names(.)[!grepl("^time", names(.))]) %>%
  rbind(d1cr[names(.)]) %>%
  arrange(date)


# -- TIDY TEMP DATASETS -----
# tidy chart temp datatsets
sdl_long <- tidytemp(sdl, datasource = "sdl", dropcol = "airtemp_avg")
d1_long <- tidytemp(d1, datasource = "d1", dropcol = "airtemp_avg")
c1_long <- tidytemp(c1, datasource = "c1", dropcol = "airtemp_avg")

# tidy logger datasets (sdl logger already long-form)
d1cr_long <- tidytemp(d1log, datasource = "d1cr", special = NA, dropcol = "airtemp_avg")

# rename sdlcr colnames
sdllog_qa <- sdllog_qa %>%
  rename(sdllogger = logger,
       sdlcr_temp = cr_temp,
       sdlcr_qatemp = qa_temp,
       sdlcr_qaflag = qa_flag)

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




  # -- PREP MASTERS DATASET FOR QA COMPARISONS -----
# create working copy
working_dat <- sdl_long %>%
  # add empty col for qa flags added
  mutate(qa_flag = NA) %>%
  # join comparative data sets %>%
  left_join(sdllog_qa) %>%
  left_join(dplyr::select(d1_long, date:ncol(d1_long))) %>%
  left_join(dplyr::select(c1_long, date:ncol(c1_long))) %>%
  left_join(dplyr::select(d1cr_long, date:ncol(d1cr_long)))



# -- FLAG DAILY DEPARTURES FROM COMPARATIVE DATASETS -----
# round 1 diff daily and flagging
working_dat <- diff_daily(working_dat)  
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff1 <- filter(working_dat, flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff1 <- visual_qa(working_dat, check_daily_diff1, add_fourth = "d1cr_temp")
plot_grid(plotlist = qa_daily_diff1) 
# round 1:
# all but tmin on 2011-06-24 look like they should be flagged
flag_dailydiff1 <- subset(check_daily_diff1, date!= "2011-06-24")
# flag and remove values in working dataset
working_dat <- flag_temp(flag_dailydiff1, error = "comparative deviance")

# clean up environment
rm(qa_daily_diff1, flag_dailydiff1, check_daily_diff1)



# -- QA EXTREMES (TMIN/TMAX) -----
## IMPORTANTE!!: 
## logger lifecycles: cr21x = 1980s-2000; cr23x = 2000 - 2012; cr1000 = dec 2012 - ongoing
## panel arrangement: cr21x = left panel, cr23x = middle, cr1000 = right panel

# visual qa grand max and min
# max temps by logger
check_max1 <- check_extreme(working_dat, groupvars = "met", max)
qa_max1 <- visual_qa(working_dat, check_max1)
plot_grid(plotlist = qa_max1) 
# >round 1: 
# tmax in 1982-06-28 looks like too big of a jump in comparison to d1 and c1 behavior, all else okay
flag_max1 <- filter(check_max1, met == "airtemp_max" & date == "1982-06-28")
working_dat <- flag_temp(flag_max1, error = "high value")
# run through grand max once more
check_max2 <- check_extreme(working_dat)
qa_max2 <- visual_qa(working_dat, check_max2)
plot_grid(plotlist = qa_max2) #looks okay

# min temps by logger
check_min1 <- check_extreme(working_dat, metric=min)
qa_min1 <- visual_qa(working_dat, check_min1)
plot_grid(plotlist = qa_min1) #temperature minimums look good

# move on to monthlies...
#clean up env
rm(check_max1, check_max2, qa_max1, qa_max2,
   check_min1, qa_min1, flag_max1)



# -- QA MONTHLY MIN/MAX -----
# monthly max temps of tmin and tmax
check_monthly_max1 <- check_extreme(working_dat, c("met", "mon"))
# run through visual qa function
qa_mon_max1 <- visual_qa(working_dat, check_monthly_max1, sorttime = "mon", add_fourth = "d1cr_temp")

# visualize logger monthly maximums, by logger and metric
## max of airtemp_max
plot_grid(plotlist = qa_mon_max1[grep("airtemp_max", qa_mon_max1)]) #flag 2001-02-28 & 2017-02-20 tmax, all else looks okay
## max of airtemp_min
plot_grid(plotlist = qa_mon_max1[grep("airtemp_min", qa_mon_max1)]) #look at 2011-02-15 & 2015-03-15 tmin, all else fine
plot_grid(plotlist = qa_mon_max1[grep("2011|2015", qa_mon_max1)]) #flag 2011-02-15 & 2015-03-15 tmin, all else fine
# flag values
flag_mon_max1 <- subset(check_monthly_max1, (met == "airtemp_max" & mon == 2) | 
                                  (met == "airtemp_min" & as.character(date) %in% c("2011-02-15", "2015-03-15")))
working_dat <- flag_temp(flag_mon_max1)

# check monthly max again to be sure
check_monthly_max2 <- check_extreme(working_dat, c("met", "mon"))
# run through visual qa function
qa_mon_max2 <- visual_qa(working_dat, check_monthly_max2, sorttime = "mon", add_fourth = "d1cr_temp")

# visualize what was adjusted
plot_grid(plotlist = qa_mon_max2[grep("airtemp_max", qa_mon_max2)]) #okay
plot_grid(plotlist = qa_mon_max2[grep("airtemp_min", qa_mon_max2)]) #okay


# monthly min temps of tmin and tmax
check_monthly_min <- check_extreme(working_dat, c("met", "mon"), min)
# run through visual qa function
qa_mon_min1 <- visual_qa(working_dat, check_monthly_min, sorttime = "mon", add_fourth = "d1cr_temp")

# visualize logger monthly minimums, by logger and metric
## min of airtemp_max
plot_grid(plotlist = qa_mon_min1[grep("airtemp_max", qa_mon_min1)]) #okay
## min of airtemp_min
plot_grid(plotlist = qa_mon_min1[grep("airtemp_min", qa_mon_min1)]) #okay



# clean up environment and move on..
rm(flag_mon_max1, check_monthly_max1, check_monthly_max2, check_monthly_min,
   qa_mon_max1, qa_mon_max2, qa_mon_min1)



# -- QA LAG SPIKES/FLATLINES WITHIN LOGGER DATASET ----
# look for consecutive day temp spikes/declines that exceed 40C within the same metric
# also look for flatlining in temperature (jen morse said try 5-consec days)

# save copy of working dat in case mess up (so don't need to re-run from top)
working_dat_copy <- working_dat

## add lag temp
working_dat <- deltamet(working_dat)
# check distribution of absolute difference with current day's temp and yesterday's temp
boxplot(working_dat$lag1_diffmain)  
sapply(split(working_dat$lag1_diffmain, working_dat$met), function(x) tail(sort(x))) # at most swung 22 degrees in max, 18 in min.. could happen?

# can visualize swings to be sure
swing_check1 <- subset(working_dat, lag1_diffmain >= 15) 
qa_swings <- visual_qa(working_dat, swing_check1, add_fourth = "d1cr_temp")
plot_grid(plotlist = qa_swings[grep("airtemp_max", qa_swings)]) # would flag dec 1 1981 *and* nov 30 1981
plot_grid(plotlist = qa_swings[grep("airtemp_min", qa_swings)]) # seems okay

# look for 4 or more consecutive days of 0 change
working_dat <- check_flatline(working_dat)
flat_check <- subset(working_dat, !is.na(flatline))
qa_flatline <- visual_qa(working_dat, flat_check)
plot_grid(plotlist = qa_flatline) 
# > idk enough to know whether any of these warrant a flag? maybe just flag with warning
# flag but don't remove values
for(i in 1:nrow(flat_check)){
  temp_df <- flat_check[i, ]
  # build a sequence of dates to flag
  current_date <- temp_df$date
  start_date <- current_date - 1
  end_date <- start_date + temp_df$flatline
  working_dat$qa_flag[working_dat$date %in% seq.Date(start_date, end_date,1) & working_dat$met == temp_df$met] <- "warning: 4+ day flatline"
}

# clean up environment
rm(count0, swing_check1, qa_swings, flat_check, qa_flatline)


# -- SCREEN DELTA DEVIANCES COMPARATIVELY ------
working_dat <- deltadev(working_dat)

check_deltadiff1 <- subset(working_dat, flag_deltadiff1 == TRUE & flag_deltadiff2 == T )
#View(subset(working_dat, flag_deltadiff_sdl == TRUE))
qa_lagdiffs1 <- visual_qa(working_dat, check_deltadiff1, add_fourth = "d1cr_temp")
plot_grid(plotlist = qa_lagdiffs1[grep("airtemp_min", qa_lagdiffs1)]) #looks okay enough.. leave alone
plot_grid(plotlist = qa_lagdiffs1[grep("airtemp_max", qa_lagdiffs1)]) # flag 2009-01-01, 2012-02-13 and 2017-11-07.. not sure others deviate enough to flag

flag_lagdiff1 <- subset(check_deltadiff1, met == "airtemp_max" & as.character(date) %in% c("2009-01-01", "2012-02-13", "2017-11-07"))
working_dat <- flag_temp(flag_lagdiff1, error = "comparative deviance: 2-day rate of change")

# I think that's it for qa..
rm(qa_lagdiffs1, flag_lagdiff1, check_deltadiff1)



# -- REVIEW FLAGGING ----
summary(as.factor(working_dat$qa_flag))
# summary of flagging pre consistent-logger data:
summary(as.factor(working_dat$qa_flag[is.na(working_dat$comp1)])) #4 values flagged, 23 warnings for flatlines
# what are the dates?
working_dat %>%
  subset(!is.na(qa_flag) & is.na(comp1)) %>%
  dplyr::select(met, date, qa_flag) %>%
  print(n = Inf)


# -- WRITE OUT FLAGGED DATASET -----

sdl_qa <- sdl_long %>%
  left_join(dplyr::select(working_dat, LTER_site:main, qa_flag, flatline)) %>%
  rename(sdl_qatemp = main,
         daysflat = flatline) # rename for plotting purposes

write_csv(x = sdl_qa, path = paste0(datpath, "output_data/prep_data/qa_sdlchart_temp.csv"))
