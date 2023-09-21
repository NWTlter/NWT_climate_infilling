# infilling for c1 and d1 ppt 2010-current date on EDI

# script purpose:
# read in ppt datasets that need infilling and source station datasets + TK infilled C1 and D1 ppt datasets
# run quick logic checks on qdays for all stations with a qdays column (CTW does not have a QA script for ppt, so these are only QA checks)
# infill ppt at C1 and D1 for years 2011-ongoing using TK methodology:
## natural log transform all non-zero ppt values where qdays == 1 at target station and source station
## run 2 infill methods (short-term seasonal infill, long-term multi-yr infill) using *each* available source station
## choose best model (pval <= 0.05 and highest r2), and infill using that model [models are simple linear regression]
## if period to infill is accumulated days period, apportion accumulated amt backwards in time based on relative contribution of daily exponentiated predicted amt 
## if period to infill does not have accumulated days info (i.e. is truly missing data), use exponentiated predicted value
# review TK flagging (below) for other cases where models can't infill for various reasons (e.g. no model with non-zero ppt during an accumulated days period)
# if none of the above works, can default to old NWT metadata methods? (below)

# TK short "seasonal" infill models seems to use the data: -31d < target infill date < +30d [SAME YEAR] (but CTW can't nail this down perfectly, see below in replication section)
## this seasonal infill methodology is different that the seasonal/short-term methodology used to infill temp
# TK long term method model uses the data: -3d < target mm-dd < +3d [ALL YRS AVAILABLE]
## long infill method is the same used for temp (except ppt models only use ln(non-zero ppt) whereas no math trans done on temp vals before modeling)




# -- NWT/ EDI METADATA METHODS -----------
# hierarchy of infill methods for precip
# 1) method 1 (ratio method) using D1
# 2) method 1 (ratio method) using C1

# some rules:
# use same precip data used in NSF proposal for years 1950s-2010 (i.e TK infilled data)
# for 2010-current, only keep raw chart data where qdays = 1; qdays > 1 --> NA (infill)
# ratios calculated based on days both c1 and infill source site had observations

## from methods section in NWT saddle chart precip metadata:
# (Method Flag 1) Daily Ratio Methodology:
# Total precipitation for the date in question was calculated for the the period 1981-2008, for both the 'Known Site' and the 'Unknown Site', only for days where daily values existed for both sites (missing values and QD's > 1 were removed). 
# A ratio was determined [Unknown Site : Known Site] based on these totals.  
# This ratio was then multiplied by the value for that date at the 'Known Site' in order to determine the 'Unknown Site' value.  

# Precipitation data adjacency hierarchy:
# C1:
# >> CTW prioritizing C1 snotel station first; according to NWT metadata hierarchy is:
# 1) Allenspark (AP)
# 2) D1
# 3) Saddle (if not in winter [OCT-MAY] due to bias that blowing snow can have)
# 4) Winter Park (WP)

# > CTW calculated ratios by month instead of by day, didn't trust variation day to day enough, wanted to generate more conservative infilling estimates 
# -- end NWT METADATA methods notes #



# other notes:
# no blowing snow correction factor needed for c1 or d1...

# snotel flags are..
# Quality Control flags included:
#
# Flag    Name                Description
#  V      Valid               Validated Data
#  N      No Profile          No profile for automated validation
#  E      Edit                Edit, minor adjustment for sensor noise
#  B      Back Estimate       Regression-based estimate for homogenizing collocated Snow Course and Snow Pillow data sets
#  K      Estimate            Estimate
#  X      External Estimate   External estimate
#  S      Suspect             Suspect data
# 
# Quality Assurance flags included:
#
# Flag    Name                Description
#  U      Unknown             Unknown
#  R      Raw                 No Human Review
#  P      Provisional         Preliminary Human Review
#  A      Approved            Processing and Final Review Completed


# c1 cr23x and cr1000x data logger ppt flags (from EDI metadata):
# flag for total precipitation: 
# n=no flag; m=missing; q=questionable; e=estimated
# All flag column values were set to "n" for 2000 through 9 September 2014 because no QAQC flagging was conducted during this time
# In March 2013, the CR23X logger was replaced with a CR1000 logger.


# notes from TK draft ppt manuscript/nwt renewal:
# Flag 1:
# A - Data recorded at Target Station (see Source Station field for specific gauge information)
# B - Data infilled using Method 1 with a p value <.05
# C - Data infilled using Method 1 with a p value >.05
# D - Data infilled using Method 2 with a p value <.05
# E - Data infilled using Method 2 with a p value >.05

# Flag 2:
# A - Daily value, not adjusted for any period total
# B - Daily value, adjusted for period total as recorded by Belfort Recording Gauge: Shielded.
# C - Daily value, adjusted for period total as recorded by US Weather Bureau Totalizing Gauge: Unshielded
# D - Daily value, adjusted for period total as recorded by US Weather Bureau Totalizing Gauge: Shield-corrected
# E - Daily value, adjusted for period total as recorded by Other Belfort Recording Gauge dataset - 1960: Different instrument & shield than in 'B'
# F - Daily value, adjusted for period total as recorded by Other Belfort Recording Gauge dataset - 1964: Unshielded-period for gauge in 'B'
# G - Period Total of zero recorded at Target Station, so all days in period given zero value
# *H - Period Total recorded at Target Station divided by number of days in period 
# **I - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by Belfort Recording Gauge: Shielded.
# **J - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by US Weather Bureau Totalizing Gauge: Unshielded
# **K - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by US Weather Bureau Totalizing Gauge: Shield-corrected
# **L - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by Other Belfort Recording Gauge dataset - 1960: Different instrument & shield than in 'B'
# **M - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by Other Belfort Recording Gauge dataset - 1964: Unshielded-period for gauge in 'B'
# 
# * occurs only when there are no valid, non-zero data from any of the source stations with which to infill and match the period total
# ** these occur when the "best" infilled values for each day of a period are all zeros even though there is a non-zero total recorded for the period.



# ctw notes on jennings infilled c1cr dataset:
# looking at keith's code, annotates that ppt disaggregated to hourly by dividing daily totals by 24, so this must be why doesn't sum perfectly to match daily chart ppt?
# i'm not sure how keith got ppt data for 2013 at c1 (i.e. no huge gaps in data)

# KJ ppt no good to use:
# e.g. see 2013-05-07. qdays in chart = 7, ppt = 43mm. kj divided that by 24 and one hour got added to day prior (must be something in his loop?), all missing days prior got assigned 0 ppt.
# that is not necessarily accurate. could be all of that rainfall fell on one day, but also could have been apportioned over the 7 days, need to compare with other stations to determine what (likely) happened.
# > going to discard KJ ppt from consideration for c1 and d1 infilling (didn't use for saddle so not an issue there)



# -- SETUP ------
# clean environment, load needed libraries, modify default settings
rm(list = ls())
library(tidyverse)
library(lubridate)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals = c("NP", "NA", NA, "NaN", NaN, ".")

#set path to climate data qa'd prep data folder
datpath <- "climate_d1_c1/output_data/prep_data/"

# -- UTILITY FUNCTIONS -----
source("utility_functions/utility_functions_all.R")

# -- QA FUNCTIONS -----
# function to verify all dates within a qdays > 1 period don't have ppt values recorded
check_qdays <- function(dat){
  # initiate empty vector for storing dates to check
  check_dates <- NULL
  
  # ID qdays > 1 date
  qdays_dates <- subset(dat, qdays > 1) %>%
    dplyr::select(date, ppt_tot, qdays)
  
  for(i in 1:nrow(qdays_dates)){
    # specify date seq to backfill
    event_dates <- seq.Date(qdays_dates$date[i] - (qdays_dates$qdays[i]-1), qdays_dates$date[i], 1)
    temp_df <- subset(dat, date %in% event_dates)
    
    # verify all ppt_tot values NA in event series (minus the accumulated date)
    ppttally <- sum(temp_df$ppt_tot[temp_df$date != qdays_dates$date[i]], na.rm = T) 
    if(!is.na(ppttally)){
      dates_fail <- as.character(temp_df$date[temp_df$date != qdays_dates$date[i] & !is.na(temp_df$ppt_tot)])
      # add to check_dates
      check_dates <- c(check_dates, dates_fail)
    }
  }
  if(length(check_dates) != 0){
    print("Inconsistencies with qdays found. Returning bad dates.")
    return(check_dates)
  } else{
    print("No inconsistencies with qdays found!")
  }
}

# NA any nonsensical qdays values and corresponding ppt (e.g. anything other than NA or numeric qdays >= 1)
clean_qdays <- function(dat){
  bad_qdays <- unique(c(dat$qdays[dat$qdays < 1 & !is.na(dat$qdays)], dat$qdays[grepl("[a-z][A-Z]", dat$qdays)]))
  if(length(bad_qdays) != 0){
    needs_correct <- which(dat$qdays %in% bad_qdays)
    for(n in needs_correct){
      print(paste("Correcting", dat$qdays[n], "qdays value on", dat$date[n]))
      dat$ppt_tot[n] <- NA
      dat$qdays[n] <- NA
    }
    
  } else{
    print("All qdays NA or 1+ days.")
  }
  return(dat)
}


# -- GET DATA -----
# data used in NSF proposal
# from NWT renewal dropbox
tkd1 <- read.csv("../../Dropbox/NWT_data/d1_infilled_daily_ppt.csv", na.strings = na_vals)
tkc1 <- read.csv("../../Dropbox/NWT_data/c1_infilled_daily_ppt.csv", na.strings = na_vals)


# read in  data from EDI data portal
# c1 chart precip data
c1_chartpcp <- getTabular(414, col_class = cols("c", "c", "D", "d", "d", "c"))
# d1 chart precip data
d1_chartpcp <- getTabular(415, col_class = cols("c", "c", "D", "d", "d", "c"))
# sdl chart precip data
sdl_chartpcp <- getTabular(416, col_class = cols("c", "c", "D", "d", "d", "c"))
# c1 cr logger data (for ppt)
c1cr <- getTabular(401, na_vals = na_vals) %>% data.frame()
# d1 cr logger data (for ppt?)
d1cr <- getTabular(402, na_vals = na_vals) %>% data.frame() # no ppt in this dataset
# sdl cr logger data
sdlcr <- getTabular(405, na_vals = na_vals) %>% data.frame() # no ppt in this dataset either
# sdl NSF data to check whether EDI data has been winterized (applied m. william correction factor oct - may, altho emily's data had it applied sep-may)
sdlnsf <- read.csv("~/Dropbox/NWT_data/Saddle_precip_temp_formoisturedeficit.csv")

# ameriflux (p. blanken et al. use ppt data from noaa station less than 1km away from c1 ameriflux site)
noaa_crn <- read_table("https://www1.ncdc.noaa.gov/pub/data/uscrn/products/daily01/2018/CRND0103-2018-CO_Boulder_14_W.txt", col_names = FALSE)
noaa_meta <- read_lines("ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/daily01/HEADERS.txt") %>%
  data.frame()
headers <- strsplit(noaa_meta[2,], " ")
headers[[1]]
colnames(noaa_crn) <- headers[[1]]
noaa_crn$date <- as.Date(as.character(noaa_crn$LST_DATE), format = "%Y%m%d")

# ctw prepped data
#sdl_nsf <- read.csv("extended_summer/analysis/output_data/prep_data/sdl_ppt_infill_19822018_nsfctw.csv")
snotel_ppt <- read_csv(paste0(datpath, "nwt_snotel_ppt.csv"))
uc_ppt <- read_csv(paste0(datpath, "nwt_univcamp_ppt.csv"))



# -- REVIEW DATA -----
glimpse(d1_chartpcp)
glimpse(c1_chartpcp)
glimpse(sdl_chartpcp)
glimpse(c1cr)
glimpse(snotel_ppt)
glimpse(uc_ppt)
glimpse(tkd1)
glimpse(tkc1)

# QA chart ppt before adjust colnames
# c1
c1_chartpcp <- clean_qdays(c1_chartpcp)
check_qdays(c1_chartpcp)
# sdl 
sdl_chartpcp <- clean_qdays(sdl_chartpcp)
check_qdays(sdl_chartpcp)
# check bad dates -- since only using sdl to infill, NA inconsistent vals to be sure not using bad data to infill c1 and d1
# "2013-04-30"
View(subset(sdl_chartpcp, date > "2013-04-15")) # 0 ppt, 1 qday
# "2014-07-22"
View(subset(sdl_chartpcp, date > "2014-07-01")) # 0 ppt, 1 qday
# "2015-06-30"
View(subset(sdl_chartpcp, date > "2015-06-01")) # 0 ppt, 1 qday
# 2016-12-19
View(subset(sdl_chartpcp, date > "2016-12-01")) # 0 ppt, NA qday
# for loop clean up
for(d in c("2013-04-30", "2014-07-22", "2015-06-30", "2016-12-19")){
  sdl_chartpcp$ppt_tot[sdl_chartpcp$date == as.Date(d)] <- NA
  sdl_chartpcp$qdays[sdl_chartpcp$date == as.Date(d)] <- NA
  
}
# verify sdl chart again
check_qdays(sdl_chartpcp) #good

# d1 
d1_chartpcp <- clean_qdays(d1_chartpcp)
check_qdays(d1_chartpcp)
# what is the d1 value on bad date?
d1_chartpcp$ppt_tot[d1_chartpcp$date == "2011-12-29"] # falls in backfill period for "2012-01-16
# what is the ppt value at other stations on 2011-12-29?
snotel_ppt$ppt_mm[snotel_ppt$date == "2011-12-29"]
sdl_chartpcp$ppt_tot[sdl_chartpcp$date == "2011-12-29"]
c1_chartpcp$ppt_tot[c1_chartpcp$date == "2011-12-29"]
uc_ppt$ppt_mm[uc_ppt$date == "2011-12-29"]
# since other stations have a precip value that day and d1 ppt val is non-zero, adjust qdays number
d1_chartpcp$qdays[d1_chartpcp$date == "2012-01-16"] <- (d1_chartpcp$qdays[d1_chartpcp$date == "2012-01-16"] -1) 
# check qdays again
check_qdays(subset(d1_chartpcp, year(date) > 2010)) # good

# adjust names for sdl, d1, and snotel ppt for clarity
colnames(sdl_chartpcp)[4:6] <- paste("sdl", colnames(sdl_chartpcp)[4:6], sep = "_")
colnames(d1_chartpcp)[4:6] <- paste("d1", colnames(d1_chartpcp)[4:6], sep = "_")
colnames(snotel_ppt)[7:9] <- paste("sno", colnames(snotel_ppt)[7:9], sep = "_")
colnames(uc_ppt)[7:9] <- paste("uc", colnames(uc_ppt)[7:9], sep = "_")

# extract ppt cols from c1 cr logger
c1cr_ppt <- dplyr::select(c1cr, LTER_site:jday, ppt_tot, flag_ppt_tot) %>%
  mutate(yr = year(date),
         mon = month(date),
         doy = yday(date),
         # c1cr ppt in cm; convert to mm to match chart
         ppt_tot = ppt_tot * 10) %>%
  select(LTER_site, local_site, logger, date, yr, mon, doy, ppt_tot, flag_ppt_tot)
# check for duplicate dates created by logger instruments overlapping
summary(duplicated(c1cr_ppt$date)) # apparently none (maybe SCE fixed when posted latest version of data?)
#rename c1cr ppt_tot for clarity
colnames(c1cr_ppt)[grepl("ppt", colnames(c1cr_ppt))] <- paste("c1cr", colnames(c1cr_ppt)[grepl("ppt", colnames(c1cr_ppt))], sep = "_")
# clean up
rm(c1cr, d1cr, sdlcr)

# review logic checks on c1 and d1
# c1
# check min number qdays is 1
sort(unique(c1_chartpcp$qdays)) # good
# check range in ppt values so be sure nothing too crazy
sort(unique(c1_chartpcp$ppt_tot[c1_chartpcp$qdays == 1 & !is.na(c1_chartpcp$qdays)])) #mm, looks fine
# d1
# check min number qdays is 1
sort(unique(d1_chartpcp$d1_qdays)) # good
# check range in ppt values so be sure nothing too crazy
sort(unique(d1_chartpcp$d1_ppt_tot[d1_chartpcp$d1_qdays == 1 & !is.na(d1_chartpcp$d1_qdays)])) # 134 mm...
d1_chartpcp$date[d1_chartpcp$d1_ppt_tot == 134 & !is.na(d1_chartpcp$d1_ppt_tot)] # high value in january, plus is a tim kittel val so will leave alone



# -- VISUALIZE DATA FOR OVERVIEW OF WHERE STARTING/RELATIONSHIPS BTWN STATIONS -----
# append 2011 ppt EDI data to tk infilled 1950s-2010 dataset, review relationship btwn the two
c1tknwt <- tkc1 %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  left_join(c1_chartpcp, by = "date") %>%
  mutate(diff = `Precipitation..mm.` - ppt_tot)

ggplot(subset(c1tknwt, qdays == 1 & grepl("C1", Source.Station)), aes(`Precipitation..mm.`, ppt_tot)) +
  geom_point()

ggplot(subset(c1tknwt, qdays == 1 & grepl("C1", Source.Station)), aes(date, diff)) +
  geom_point() +
  scale_x_date(date_breaks = "5 years")

ggplot(subset(c1tknwt, qdays == 1 & grepl("C1", Source.Station) & Year == 1964), aes(date, diff)) +
  geom_line() +
  geom_point() # something weird going on in 1964.. is not the year for PDO switch

# use tk's infilled ppt dataset 1950s-2010 agrees perfectly with c1 qdays = 1 dataset (except for wnt/spr 1964), then append EDI data
c1tknwt2 <- c1tknwt %>%
  mutate(LTER_site = "NWT",
         local_site = "c1") %>%
  dplyr::select(LTER_site, local_site, date, `Precipitation..mm.`:Source.Station, qdays, flag_ppt_tot) %>%
  rename(ppt_tot = `Precipitation..mm.`,
         tk_flag1 = Flag.1,
         tk_flag2 = Flag.2,
         source = Source.Station) %>%
  dplyr::select(colnames(c1_chartpcp), tk_flag1, tk_flag2, source) %>%
  rbind(cbind(c1_chartpcp[c1_chartpcp$date > as.Date("2010-12-31"),],
              tk_flag1 = NA,
              tk_flag2 = NA,
              source = "C1 Belfort Shielded")) %>%
  mutate(yr = year(date),
         mon = month(date), 
         doy = yday(date)) %>%
  dplyr::select(LTER_site, local_site, date, yr:doy, ppt_tot:ncol(.)) %>%
  # remove 2019 bc not complete
  filter(yr < 2019)

# where are the gaps?
with(subset(c1tknwt2, yr > 2010), sapply(split(qdays, yr), function(x) summary(x == 1)))
# 2013 has most missing days by far.. 
with(subset(c1tknwt2, yr > 2010), sapply(split(ppt_tot, mon), function(x) summary(is.na(x))))
# summer months missing data..
# what about months with qdays?
with(subset(c1tknwt2, yr > 2010), sapply(split(qdays, mon), function(x) summary(x > 1))) # june and may have the most

# plot c1 chart ppt with nearest source stations 
ggplot(subset(c1tknwt2, yr > 2010 & qdays == 1), aes(date, ppt_tot)) +
  geom_point(data= subset(snotel_ppt, yr %in% 2011:2018), aes(date, sno_ppt_mm), col = "dodgerblue2", alpha = 0.5) +
  geom_point(data= subset(c1cr_ppt, yr > 2010), aes(date, c1cr_ppt_tot), col = "orchid", alpha = 0.5) +
  geom_point(alpha = 0.5) +
  ggtitle("c1 chart daily ppt with snotel ppt (blue) and c1 logger ppt (purple)") +
  facet_wrap(~yr, scales = "free")
# > snotel data available... but where it's non-zero perhaps doesn't match up super well with c1 (oddly..)
# c1 cr logger dat also covers gaps, but not QA QC'd for most of 2010 onwards record.. at least snotel has flag values?

# look at the relationship btwn c1 chart ppt and other stations on 1:1 line
## snotel
left_join(c1tknwt2, dplyr::select(snotel_ppt, -local_site)) %>%
  # look at some of tim's years with more recent, and only qdays == 1
  subset(yr > 2005 & qdays == 1) %>%
  ggplot(aes(ppt_tot, sno_ppt_mm)) +
  geom_point(alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0), col = "red") +
  labs(y = "snotel ppt (mm)", x = "c1 chart ppt (mm)") +
  ggtitle("snotel vs. c1 chart ppt") +
  facet_wrap(~yr, scales = "free") # more noise than anticipated around the 1:1 line..

# c1 cr logger
left_join(c1tknwt2, c1cr_ppt) %>%
  # look at some of tim's years with more recent, and only qdays == 1
  subset(yr > 2005 & qdays == 1) %>%
  ggplot(aes(ppt_tot, c1cr_ppt_tot)) +
  geom_point(alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0), col = "red") +
  labs(y = "c1 logger ppt (mm)", x = "c1 chart ppt (mm)") +
  ggtitle("c1 cr logger vs. c1 chart ppt (2013 = logger change)") +
  facet_wrap(~yr, scales = "free") 






# -- REPLICATE TIM KITTEL INFILL RESULTS -----
# TK used log-transformed single-station regression to infill ppt (i.e. simple linear regression, but ln ppt at both stations bc precip non-normal)
# he ran two versions and picked the regression with p<= 0.05 and the highest R2
# v1) same-year, same-season
# v2) same date, over all years (e.g. if missing date is 2015-07-07, then all July 7ths available in the histor record)
# > the same date, all yrs method was selected over 90% of the time so going to start with that first..

# test log regression on something in tk's dataset to be sure can replicate similar value (if not perfect match, similar)
tkc1_noinfill <- tkc1 %>%
  mutate(c1_clean = ifelse(grepl("C1", Source.Station), `Precipitation..mm.`, NA)) %>%
  # add ln ppt for non-zero ppt
  mutate(ln_c1 = ifelse(c1_clean > 0, log(c1_clean), NA))
colnames(tkc1_noinfill)[4:11] <- paste0("c1", colnames(tkc1_noinfill)[4:11])

# append tk d1
tkd1_noinfill <- tkd1 %>%
  mutate(d1_clean = ifelse(grepl("D1", Source.Station), D1.mm.ppt, NA)) %>%
  # add ln ppt for non-zero ppt
  mutate(ln_d1 = ifelse(d1_clean > 0, log(d1_clean), NA))
colnames(tkd1_noinfill)[5:12] <- paste0("d1", colnames(tkd1_noinfill)[5:12])

# also append SDL to see if can replicate
sdl_noinfill <- subset(sdl_chartpcp, date < as.Date("2011-01-01")) %>%
  mutate(sdl_clean = ifelse(is.na(sdl_flag_ppt_tot) & sdl_qdays == 1, sdl_ppt_tot, NA),
         sdl_ln = ifelse(sdl_clean > 0, log(sdl_clean), NA))

# put it all together
cleanjoin <- left_join(dplyr::select(tkc1_noinfill, Month:Year, c1_clean, ln_c1),
                       dplyr::select(tkd1_noinfill, Month:Year, d1_clean, ln_d1)) %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-")),
         doy = yday(date)) %>%
  left_join(sdl_noinfill[c("date", "sdl_clean", "sdl_ln")])

## 1) replicate most recent date infilled
unique(tkc1$Source.Station[tkc1$Year >= 2000]) #use D1; 51681 is coal creek climate station
d1_dates <- c1tknwt$date[c1tknwt2$source == "D1"]
testdate1 <- max(d1_dates)
View(subset(c1tknwt, Month == month(testdate1) & Year == year(testdate1)))
# qdays = 2, infill job was on those 2 days and infilled amt sums to accumulated amt corresponding to qdays = 2..
# infilled via method 2 (all yrs, same day (but going to try same month to get higher n))


# what is anticipated regression and number obs used?
# y=0.741632293012709x + 0.129268684333437 for test date, 69 obs, r2 of 0.3906
# > different regression equation for day prior so it can't be a seasonal regression, must be all yrs
# 2) allyrs, same date regression
testdf_allyrs1 <- subset(cleanjoin, Day %in% c(day(testdate1)-3):(day(testdate1)+3) & Month == month(testdate1))
# how many obs?
nrow(subset(testdf_allyrs1, !is.na(ln_c1) & !is.na(ln_d1))) # 69, good
yrslm1 <-  lm(ln_c1 ~ ln_d1, data = testdf_allyrs1) 
summary(yrslm1) # perfect match

# match june 13 2010 regression
## pval: 3.61E-10, r2: 0.446185538, eq: y=0.759398319246367x+0.0211868881680028
testdf_allyrs1.2 <- subset(cleanjoin, Day %in% c(day(testdate1-1)-3):(day(testdate1-1)+3) & Month == month(testdate1))
# how many obs?
nrow(subset(testdf_allyrs1.2, !is.na(ln_c1) & !is.na(ln_d1))) # 69, good
yrslm1.2 <-  lm(ln_c1 ~ ln_d1, data = testdf_allyrs1.2) 
summary(yrslm1.2) # perfect match

# try to rep infill values (6-14: 3.391353, 6-13: 12.608647, total accum is 16 in 2 qdays)
val1 <- predict(yrslm1, newdata = subset(testdf_allyrs1, date == (testdate1)))
val2 <- predict(yrslm1.2, newdata = subset(testdf_allyrs1.2, date == (testdate1-1)))
# calculate relative percent -- must exponentiate predicted vals before relativize
rel1 <- exp(val1)/(exp(val1) + exp(val2)) #6/14/2010 
rel2 <- exp(val2)/(exp(val1) + exp(val2)) #6/13/2010
# calculate infilled days based on relative apportionment
# test 6/14 match
round((16*rel1)[[1]],4) == round(c1tknwt$Precipitation..mm.[c1tknwt$date == testdate1],4) #good
# test 6/13 match
round((16*rel2)[[1]],4) == round(c1tknwt$Precipitation..mm.[c1tknwt$date == (testdate1-1)],4) #good


## 2) try next date infilled based on D1
testdate2 <- rev(d1_dates)[3]
View(subset(c1tknwt, date %in% seq.Date(testdate2-5, testdate2+5, 1))) # infilling based on best model for THAT DAY, TK did not infill entire missing time windows based on one model (as with temp dataset)
View(subset(c1_chartpcp, date %in% seq.Date(testdate2-5, testdate2+5, 1))) # e.g. 7 days of accumulated rainfall, NAs preceding, all infilled by different models

# looking for: y=0.344154044383719x+0.509627029281193, 120 obs, r2 = 0.112871076
testdf_allyrs2 <- subset(cleanjoin, Day %in% c(day(testdate2)-3):(day(testdate2)+3) & Month == month(testdate2))

# how many obs?
nrow(subset(testdf_allyrs2, !is.na(ln_c1) & !is.na(ln_d1))) # 120
yrslm2 <-  lm(ln_c1 ~ ln_d1, data = testdf_allyrs2)
summary(yrslm2) # bingo! perfect match
# to replicate would need to determine models for other days missing but don't have those stations. can assume infilled same way as above (exp. prediction, calculate relative amt of that day and multiply by total accumulated rainfall)

## 3) try a date infilled by method 1 to be sure can rep that too (e.g. 5/24 and 5/23/1991)
testdate3 <- d1_dates[year(d1_dates)==1991][1]
View(subset(c1tknwt, Year == 1991 & Month == 5))
# regression looking for: y=1.21372208701196x+-0.52940226746366, nobs = 15, r2 = 0.669403143
testdf_season <- subset(cleanjoin, date > (testdate3)-31 & date < (testdate3)+30)

# how many obs?
nrow(subset(testdf_season, !is.na(ln_c1) & !is.na(ln_d1))) # 15
seasonlm <-  lm(ln_c1 ~ ln_d1, data = testdf_season)
summary(seasonlm) # bingo! perfect match


# 4) try repping SDL infill on C1 and D1
# c1: 10-17-1989, method 2 (multi-yr), looking for: y=0.45544113603806x+0.225439057123895 & 55 nobs, r2 = 0.20930045
testdf_sdlc1 <- subset(cleanjoin, Month == 10 & Day %in% seq((17-3), (17+3), 1))
sdlc1_lm <- lm(ln_c1 ~ sdl_ln, data = testdf_sdlc1)
summary(sdlc1_lm) # it's a match! this means TK didn't apply a winter correction factor (if he did, oct vals would have been changed)
nrow(sdlc1_lm$model) #55 nobs

# d1: try 01-29-1982, method 2 (multiyr)
# looking for: y=0.360657900412621x+0.803717508912493, 83 nobs, r2: 0.2134611
# try with doy match to see how compares to subsetting month and day
d1doy <- yday(as.Date("1982-01-29"))
testdf_sdld1 <- mutate(cleanjoin, doy = yday(date)) %>%
  subset(doy %in% seq((d1doy-3), (d1doy+3),1))
sdld1_lm <- lm(ln_d1 ~ sdl_ln, data = testdf_sdld1)
summary(sdld1_lm)
nrow(sdld1_lm$model) #bingo!

# try seasonal d1 (1-31-1982)
# looking for: y=0.461399077332979x+0.439308352818744, 23 nobs, r2 of 0.2282324, pval 0.021138455
testdf_sdld1.2 <- mutate(cleanjoin, doy = yday(date)) %>%
  subset(date > (as.Date("1982-01-31")-30) & date < (as.Date("1982-01-31")+31))
sdld1_lm.2 <- lm(ln_d1 ~ sdl_ln, data = testdf_sdld1.2)
summary(sdld1_lm.2)
nrow(sdld1_lm.2$model) #is 22 when use 30 on both sides, but 23 when use 30 days before and 31 days after..

# try one more short fill to resolve method: 11-4-2001 thru 11-8-01
nov_dates <- seq.Date(as.Date("2001-11-04"), as.Date("2001-11-08"), 1)
testdate.sdl <- nov_dates[1]
testdf_sdld1.3 <- subset(cleanjoin, date > (testdate.sdl-31) & date < (testdate.sdl+30))
sdld1_lm.3 <- lm(ln_d1 ~ sdl_ln, data = testdf_sdld1.3)
summary(sdld1_lm.3)
nrow(sdld1_lm.3$model) # all a match playing with dates except for 11-5-2001. its model corresponds to 11-6-2001, when i run code corresponds to 11-4-2001. maybe TK copied wrong model into cells?

# feb 8 2010, pval: 0.010858606, r2: 0.2411122, nobs = 26, eq: y=0.480042699729679x+0.187972582396084
# feb 9 2010, pval: 0.006082156, r2: 0.2840832, eq: y=0.510282333188236x+0.184372824398804, nobs = 25
feb_dates <- seq.Date(as.Date("2010-02-08"), as.Date("2010-02-09"), 1)
testdate.sdl.2 <- feb_dates[1]
testdf_sdld1.4 <- subset(cleanjoin, date > (testdate.sdl.2-31) & date < (testdate.sdl.2+18))
sdld1_lm.4 <- lm(ln_d1 ~ sdl_ln, data = testdf_sdld1.4)
summary(sdld1_lm.4) # this matches, -31 days on left side, +18 on right
nrow(sdld1_lm.4$model) # matches

# repeat 2/9 separately
testdate.sdl.2 <- feb_dates[2]
testdf_sdld1.4 <- subset(cleanjoin, date > (testdate.sdl.2-31) & date < (testdate.sdl.2+17))
sdld1_lm.4 <- lm(ln_d1 ~ sdl_ln, data = testdf_sdld1.4)
summary(sdld1_lm.4) # this matches, -30 days on left side, +17 on right
nrow(sdld1_lm.4$model) # matches.. not sure what the seasonal pattern is.. sometimes -31/30 on left, sometimes +30 to +17 on right...
# only consistency is models are run by day, not over time windows

# 4/16/2002, 4/15/2002; 15 nobs
apr_dates <- c(as.Date("2002-04-15"), as.Date("2002-04-16"))
testdate.sdl.3 <- apr_dates[1]
start_date <- apr_dates[2]-31
end_date <- apr_dates[2]+31
testdf_sdld1.5 <- subset(cleanjoin, date > start_date & date < end_date)
sdld1_lm.5 <- lm(ln_d1 ~ sdl_ln, data = testdf_sdld1.5)
summary(sdld1_lm.5) # in this case, 31d before and after each target date .. maybe something to do with number of days in month?
nrow(sdld1_lm.5$model) 

# > conclusions:
# tk's infill generally seems to be:
# if "seasonal" (method 1), window within 31 days before to 30 days after target infill date -- FOR THAT SAME YEAR, non-zero ppt only ---
# > this seasonal window varies, but until can figure out what TK did, use [-31d < x date > +30d] on all
# if all yrs (method 2), window within 3 days before to 3 days after target MONTH-DAY to infill -- ACROSS ALL YRS, non-zero ppt only
# if it's infilling qdays > 1, run regressions, make predictions and then adjusting rainfall amts based on qdays total *BUT* if source station has 0 rainfall, target station gets 0 rainfall
# to decide method and source station, pick whichever has smallest pval and highest r2, even if that means in multi-day infill window, different source stations informing values on different days
# ^ this perhaps up for debate (e.g. important to preserve whatever dynamics going on at source station in that infill window?) but following tk methods for figure update
# each day gets regression re-run (work backwards from qdays total).. altho i'm not doing this bc it would run regression off of infilled value, only using clean c1 and d1 data to do regressions
# also, SDL precip used for infilling is uncorrected for winter months (oct-may)



# -- PREP DATA -----
# make a master ppt data frame as done with temp data
# calculate a "clean" col for each station where non-zero ppt ln'd, and remove any infilled values
# also specify infill hierarchy (even tho will choose best model in the end)
# can copy/modify temp infill functions

# prep c1 and d1 chart -----
# start with c1, make master data frame with chart ppt > y2010 rbinded to tk dataset
# want both tk flag 1 == A (C1 is source) and flag2  == A (uninfilled data), G (0 precip) or NA (for yrs >2010)
c1_prep <- c1tknwt2 %>%
  mutate(c1_clean = ifelse(grepl("^C1", source) & tk_flag2 %in% c("A", "G", NA), ppt_tot, NA),
         # NA c1_clean if yr > 2010 and qdays > 1, else leave be
         c1_clean = ifelse(date > as.Date("2010-12-31") & qdays > 1, NA, c1_clean)) %>%
  # add ln ppt for non-zero ppt
  mutate(c1_ln = ifelse(c1_clean > 0, log(c1_clean), NA),
         day = day(date)) %>%
  dplyr::select(date, doy, yr, mon, day, c1_clean, c1_ln)

# prep d1
d1_prep <- tkd1 %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  left_join(d1_chartpcp, by = "date") %>%
  dplyr::select(date, D1.mm.ppt, d1_qdays, d1_flag_ppt_tot, Flag.1, Flag.2, Source.Station) %>%
  rename(d1_ppt_tot = D1.mm.ppt) %>%
  rbind(cbind(d1_chartpcp[d1_chartpcp$date > as.Date("2010-12-31"), 3:6],
              Flag.1 = NA,
              Flag.2 = NA,
              Source.Station = "D1 Belfort Shielded")) %>%
  mutate(d1_clean = ifelse(grepl("^D1", Source.Station) & Flag.2 %in% c("A", "G", NA), d1_ppt_tot, NA),
         # NA d1_clean if yr > 2010 and qdays > 1, else leave be
         d1_clean = ifelse(date > as.Date("2010-12-31") & d1_qdays > 1, NA, d1_clean)) %>%
  # add ln ppt for non-zero ppt
  mutate(d1_ln = ifelse(d1_clean > 0, log(d1_clean), NA),
         day = day(date),
         mon = month(date),
         yr = year(date),
         doy = yday(date)) %>%
  dplyr::select(date, doy, yr, mon, day, d1_clean, d1_ln)  

# join c1 and d1 and re-test TK infill to be sure get same results (i.e. NA'd everything correctly)
masterppt <- left_join(c1_prep, d1_prep) %>%
  filter(yr < 2019)

# repeat tk replications from above
# multi-yr infill
retestdf_allyrs1 <- subset(masterppt, day %in% c(day(testdate1)-3):(day(testdate1)+3) & mon == month(testdate1)) %>%
  filter(yr < 2011)
# looking for:
# y=0.741632293012709x + 0.129268684333437 for test date, 69 obs, r2 of 0.3906
summary(lm(c1_ln ~ d1_ln, data = retestdf_allyrs1)) # checks out

# short window infill
# regression looking for: y=1.21372208701196x+-0.52940226746366, nobs = 15, r2 = 0.669403143
retestdf_season <- subset(masterppt, date > testdate3-30 & date < testdate3+30 & yr < 2011)
summary(lm(c1_ln ~ d1_ln, data = retestdf_season)) # also checks out
# proceed!

# prep sdl chart -----
# ctw confirmed TK used uncorrected winter ppt (did NOT multiply oct through may by 0.39 per m. williams paper.. although data funky/transition in 1996 with wind shield change)
# in manually checking sdl chart vs. sdl nsf data, edi dataset is NOT winter corrected (e.g. see May 31 1995 in both.. edi = 165, nsf = 64.35)
# TK also used SDL data as they are (did not do any correction for wind shield change in 1996/1997)
## bc TK used SDL ppt as is, ctw will also do the same.. but perhaps something for NWT IM and others to revisit in future
sdl_prep <- mutate(sdl_chartpcp,
                   # select no infill, and only qdays == 1 values
                   ## note: there is a qdays == 0 in the sdl ppt dataset (2017-11-15); other days around it are missing so going to exclude/ignore as well
                   ## unsure of veracity of qdays == 0 bc lowest qdays value should be 1
                   sdl_clean = ifelse(sdl_qdays == 1 & is.na(sdl_flag_ppt_tot), sdl_ppt_tot, NA),
                   sdl_ln = ifelse(sdl_clean > 0, log(sdl_clean), NA)) %>%
  dplyr::select(date, sdl_clean, sdl_ln)

# prep c1 logger -----
c1cr_prep <- mutate(c1cr_ppt,
                    c1cr_clean = ifelse(c1cr_flag_ppt_tot != "n", NA, c1cr_ppt_tot),
                    c1cr_ln = ifelse(c1cr_clean >0, log(c1cr_clean), NA)) %>%
  dplyr::select(date:doy, logger, c1cr_clean, c1cr_ln) %>%
  rename(c1cr_logger = logger)


# prep snotel -----
# NA B, K, X or S in qc flag (means estimated or suspect)
snotel_est <- c("B", "K", "X", "S")
# c1 snotel
sno_prep <- mutate(snotel_ppt, 
                   sno_clean = ifelse(sno_ppt_qcflag %in% snotel_est,  NA, sno_ppt_mm),
                   sno_ln = ifelse(sno_clean > 0, log(sno_clean), NA)) %>%
  dplyr::select(date:doy, sno_clean, sno_ln)

# university camp snotel
uc_prep <- mutate(uc_ppt, 
                  uc_clean = ifelse(uc_ppt_qcflag %in% snotel_est,  NA, uc_ppt_mm),
                  uc_ln = ifelse(uc_clean > 0, log(uc_clean), NA)) %>%
  dplyr::select(date:doy, uc_clean, uc_ln)


# combine all -----
masterppt <- left_join(masterppt, sdl_prep) %>%
  left_join(c1cr_prep) %>%
  left_join(sno_prep) %>%
  left_join(uc_prep)

# clean up environment before infilling
rm(c1tknwt, tkd1_noinfill, tkc1_noinfill, sdl_noinfill,
   retestdf_allyrs1, retestdf_season, testdate.sdl, testdate.sdl.2,
   testdate1, testdate2, testdate3, testdf_allyrs1, testdf_allyrs1.2, 
   testdf_allyrs2, yrslm, yrslm1, yrslm1.2, yrslm2, seasonlm, testdf_season,
   rel1, rel2, val1, val2, sdlc1_lm, sdld1_lm, sdld1_lm.2, sdld1_lm.3, sdld1_lm.4,
   testdf_sdlc1, testdf_sdld1, testdf_sdld1.2, testdf_sdld1.3, testdf_sdld1.4)


# -- INFILL FUNCTIONS -----
# as with temp infill, need to iterate through each station choice, run each type of infill, and choose best model based on r2 assuming pval <= 0.05
# if pval not under 0.05, choose model with smallest pval? (in temp, chose model with highest r2 so will keep that here)
# not using the movingfill in this script yet.. but keeping in case decide later on to use (will need to be updated for ppt, is written for temp)
tk_ppt_movingfill <- function(dat, target_site, missing_dates, site_order, window_days=13){
# initiate empty data frame for storing predictions and regression info
infill_df_m1 <- data.frame()
# initiate counter at position 1
i <- 1
# initiate infill event counter at 1
e <- 1
while(i <=length(missing_dates)){
  
  # print dates being infilled to keep track of progress
  print(paste("Predicting missing values for", missing_dates[i]))
  
  # specify time window
  # find first date not NA looking backwards and count days specified back from that
  #subset df to all dates before missing date  
  ## specify xcol
  xcol <- paste0(target_site, "_tmean")
  firstnotNA <- max(dat$date[dat$date < missing_dates[i] & !is.na(dat[[xcol]])])
  lastnotNA <- min(dat$date[dat$date > missing_dates[i] & !is.na(dat[[xcol]])])
  #specify dates this applies to
  time_window <- missing_dates[which(missing_dates > firstnotNA & missing_dates < lastnotNA)]
  
  start_date <- firstnotNA - window_days # subtract 13 bc including the first and last not NA dates, so 14 days total
  end_date <- lastnotNA + window_days
  
  # subset dat
  temp_df <- subset(dat, date >= start_date & date <= end_date)
  
  # iterate through infill hiearchy and pick best r2 with pval < 0.05
  r2_df <- data.frame()
  for(site in site_order){
    ycol <- paste0(site, "_tmean")
    # check there are observations > 0 for explanatory site
    if(all(is.na(temp_df[ycol]))){
      next
    }
    # check for multiple loggers if infill source a logger -- if multiple, skip infill with logger
    if(grepl("cr", site)){
      # id logger col
      logcol <- paste0(gsub("cr", "", site), "_logger")
      #store logger val
      logger <- unique(temp_df[[logcol]])
      logger <- logger[!is.na(logger)] %>% str_flatten(collapse = " ")
    } else{
      logger <- NA
    }
    mod <- lm(formula = paste0(target_site, "_tmean ~ ", ycol), data = temp_df)
    r2_df <- rbind(r2_df, 
                   data.frame(cbind(site = site,
                                    logger = logger, 
                                    r2 = summary(mod)$r.squared,
                                    pval = summary(mod)$coefficients[8])))
    
  }
  
  # make stored r2 and pval numeric
  r2_df$r2 <- as.numeric(r2_df$r2) 
  r2_df$pval <- as.numeric(r2_df$pval)
  # select best
  best <- subset(r2_df, pval <= 0.05 & r2 == max(r2, na.rm = T))
  # if nothing has signif pval, select best r2
  if(nrow(best) == 0){
    best <- subset(r2_df, r2 == max(r2, na.rm = T))
  }
  
  # infill missing values in time_window based on best model
  best_mod <- lm(formula = paste0(target_site, "_", m, " ~ ", best$site, "_", m), data = temp_df)
  tempinfill <- predict(best_mod, newdata = subset(dat, date %in% time_window))
  tempinfill_df <- cbind(date = as.character(time_window), infill = tempinfill) %>%
    as.data.frame %>%
    mutate(metric = m,
           source.station = best$site,
           logger = best$logger,
           pval = summary(best_mod)$coefficients[8],
           r2 = summary(best_mod)$r.squared,
           n.obs = nrow(best_mod$model),
           equation = paste0("y = ",  best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
           infillrun = length(time_window),
           infillevent = e,
           method = paste0((window_days+1),"-d moving window")) %>%
    # convert date to Date class
    mutate(date = as.Date(date, format = "%Y-%m-%d"))
  
  # append model infill to master infill df
  infill_df_m1 <- rbind(infill_df_m1, tempinfill_df)
  
  # indicate how many days infilled to update progress
  print(paste(length(time_window), "days predicted"))
  
  # once infilled, update i (date after last date infilled) and increment infill event
  last_infilled <- which(missing_dates == max(time_window))
  i <- last_infilled +1
  e <- e+1
  # clean up things that shouldn't be recycled
  rm(r2_df, tempinfill_df, best, time_window, start_date, end_date)
}
# return infilled df when finished
return(infill_df_m1)
}



# build multi-year regression, -31 and +30 days before and after target infill date
tk_ppt_seasonalfill <- function(dat, target_site, missing_dates, site_order, nobs_limit = 14){
  
  # initiate data frame for storing results
  infill_df_season <- data.frame()
  
  # iterate through each date with missing temp data
  for(d in as.character(missing_dates)){
    # print out to console to keep track of progress
    print(paste("Predicting",d))
    
    d <- as.Date(d, format = "%Y-%m-%d")
    
    # iterate through infill hiearchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in site_order){
      
      # subset dat
      temp_df <- subset(dat, date > (d-31) & date < (d+30))
      if(grepl("cr", site)){
        # id logger col
        logcol <- paste0(site, "_logger") 
        #id logger deployed at time of missing observation
        logger <- dat[[logcol]][dat$date == d]
        #subset to that logger
        temp_df <- filter(temp_df, get(logcol) == logger)
      } else{
        logger <- NA
      }
      ycol <- paste0(site, "_ln") # specify logged ycol
      ycol_clean <- paste0(site, "_clean") # specify clean un-logged ycol
      # check there are observations > 0 for explanatory site
      if(all(is.na(temp_df[ycol]))){
        next
      }
      # check that there is a value for x (clean col, 0s okay) on the missing date
      if(is.na(temp_df[[ycol_clean]][temp_df$date == d])){
        next
      }
      # check that there are days where both target site and source station have value so can run lm model
      if(all(is.na(temp_df[[paste0(target_site, "_ln")]][!is.na(temp_df[ycol])]))){
        next
      }
      # run lm model and store results
      mod <- lm(formula = paste0(target_site,"_ln ~ ", ycol), data = temp_df)
      r2_df <- rbind(r2_df, 
                     data.frame(cbind(site = site,
                                      logger = logger, 
                                      r2 = summary(mod)$r.squared,
                                      pval = summary(mod)$coefficients[8],
                                      nobs = nrow(mod$model))))
    }
    
    # make numeric cols numeric
    r2_df$nobs <- as.numeric(r2_df$nobs)
    r2_df$r2 <- as.numeric(r2_df$r2)
    r2_df$pval <- as.numeric(r2_df$pval)
    
    # remove any row that has an r2 of 1 (unrealistic) and filter out any model nobs less than limit specified (TK has a min of 14)
    if(nrow(r2_df)>0){
      r2_df <- subset(r2_df, r2 != 1 & nobs >= nobs_limit)
    }
    # if r2_df empty because no models could be run, state that in infill df and move on
    if(nrow(r2_df) == 0){
      tempinfill_df <- data.frame(date = d, infill = NA,
                                  source.station = NA,
                                  logger = NA, 
                                  pval = NA,
                                  r2 = NA,
                                  n.obs = NA,
                                  equation = NA,
                                  infillrun = NA,
                                  method = "seasonal")
      # append model infill to master infill df
      infill_df_season <- rbind(infill_df_season, tempinfill_df)
      # clean up things that shouldn't recycled and move on
      rm(r2_df, tempinfill_df)
      # move on to next date
      next
    }
    
    
    # select best model
    best <- subset(r2_df, pval <= 0.05 & r2 == max(r2, na.rm = T))
    # if nothing has signif pval, select best r2
    if(nrow(best) == 0){
      best <- subset(r2_df, r2 == max(r2, na.rm = T))
    }
    # if there is a tie in r2, choose lowest pval (unless tie is r2==0)
    if(nrow(best) > 1 & !all(is.na(best$pval))){
      best <- subset(r2_df, r2 == max(r2, na.rm = T) & pval == max(pval, na.rm = T)) 
    }
    if(nrow(best) > 1 & all(is.na(best$pval))){
      # choose closest station by order hierarchy
      temp_order <- sapply(best$site, function(x) grep(x, c1_order))
      best_site <- names(temp_order)[min(temp_order)]
      best <- subset(r2_df, site == best_site)
    }
    
    # infill missing values based on best model
    ## re-subset temp_df based on best model
    temp_df <- subset(dat, date > (d-31) & date < (d+30))
    # if infill source is a logger, subset data to that logger only
    if(grepl("cr", best$site)){
      # id logger col
      logcol <- paste0(best$site, "_logger") 
      #subset to that logger
      temp_df <- filter(temp_df, get(logcol) == best$logger)
    } else{
      logger <- NA
    }
    # iterate through mean T and DTR, run lm, predict missing values, and store results in data frame
    best_mod <- lm(formula = paste0(target_site, "_ln", " ~ ", best$site, "_ln"), data = temp_df)
    # if source station had 0 rainfall on missing date, record -Inf for target (exponentiates to 0), else predict value
    if(dat[[paste0(best$site,"_clean")]][dat$date == d] == 0){
      tempinfill <- -Inf
    }else{
      tempinfill <- predict(best_mod, newdata = subset(dat, date == d))
    }
    tempinfill_df <- data.frame(date = d, infill = tempinfill,
                                source.station = best$site,
                                logger = best$logger, 
                                pval = summary(best_mod)$coefficients[8],
                                r2 = summary(best_mod)$r.squared,
                                n.obs = nrow(best_mod$model),
                                equation = paste0("y = ", best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                infillrun = length(d),
                                method = "seasonal")
    
    # append model infill to master infill df
    infill_df_season <- rbind(infill_df_season, tempinfill_df)
    
    # clean up things that shouldn't be recycled
    rm(r2_df, tempinfill_df, best, logger)
    
  }
  # return infilled df
  return(infill_df_season)
}


# build multi-year regression, +- 3 days on each side of target infill date
tk_ppt_historicfill <- function(dat, target_site, missing_dates, site_order, nobs_limit = 14){
  
  # initiate data frame for storing results
  infill_df_m2 <- data.frame()
  
  # iterate through each date with missing temp data
  for(d in as.character(missing_dates)){
    # print out to console to keep track of progress
    print(paste("Predicting",d))
    
    d <- as.Date(d, format = "%Y-%m-%d")
    # specify time window
    tempdoy <- dat$doy[dat$date == d]
    doyrange <- (tempdoy-3):(tempdoy+3)
    #adjust if at beginning or end of year -- going to ignore extra day in leap years, otherwise have to choose based on month and day, this is easier and close enough
    if(any(doyrange<1)){
      doyrange[doyrange<1] <- doyrange[doyrange<1] +365
    }
    if(any(doyrange>365)){
      doyrange[doyrange>365] <- doyrange[doyrange>365] - 365
    }
    
    # iterate through infill hiearchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in site_order){
      # subset dat
      temp_df <- subset(dat, doy %in% doyrange)
      if(grepl("cr", site)){
        # id logger col
        logcol <- paste0(site, "_logger") 
        #id logger deployed at time of missing observation
        logger <- dat[[logcol]][dat$date == d]
        #subset to that logger
        temp_df <- filter(temp_df, get(logcol) == logger)
      } else{
        logger <- NA
      }
      ycol <- paste0(site, "_ln") # specify logged ycol
      ycol_clean <- paste0(site, "_clean") # specify clean un-logged ycol
      # check there are observations > 0 for explanatory site
      if(all(is.na(temp_df[ycol]))){
        next
      }
      # check that there is a value for x (clean col, 0s okay) on the missing date
      if(is.na(temp_df[[ycol_clean]][temp_df$date == d])){
        next
      }
      # check that there are days where both target site and source station have value so can run lm model
      if(all(is.na(temp_df[[paste0(target_site, "_ln")]][!is.na(temp_df[ycol])]))){
        next
      }
      # run lm model and store results
      mod <- lm(formula = paste0(target_site,"_ln ~ ", ycol), data = temp_df)
      r2_df <- rbind(r2_df, 
                     data.frame(cbind(site = site,
                                      logger = logger, 
                                      r2 = summary(mod)$r.squared,
                                      pval = summary(mod)$coefficients[8],
                                      nobs = nrow(mod$model))))
      
    }
    # make numeric cols numeric
    r2_df$nobs <- as.numeric(r2_df$nobs)
    r2_df$r2 <- as.numeric(r2_df$r2)
    r2_df$pval <- as.numeric(r2_df$pval)
    
    # remove any row that has an r2 of 1 (unrealistic) and filter out any model nobs less than limit specified (TK has a min of 14)
    if(nrow(r2_df)>0){
      r2_df <- subset(r2_df, r2 != 1 & nobs >= nobs_limit)
    }
    # if r2_df empty because no models could be run, state that in infill df and move on
    if(nrow(r2_df) == 0){
      tempinfill_df <- data.frame(date = d, infill = NA,
                                  source.station = NA,
                                  logger = NA, 
                                  pval = NA,
                                  r2 = NA,
                                  n.obs = NA,
                                  equation = NA,
                                  infillrun = NA,
                                  method = "multi-yr")
      # append model infill to master infill df
      infill_df_m2 <- rbind(infill_df_m2, tempinfill_df)
      # clean up things that shouldn't recycled and move on
      rm(r2_df, tempinfill_df)
      # move on to next date
      next
    }
    
    # select best model
    best <- subset(r2_df, pval <= 0.05 & r2 == max(r2, na.rm = T))
    # if nothing has signif pval, select best r2
    if(nrow(best) == 0){
      best <- subset(r2_df, r2 == max(r2, na.rm = T))
    }
    
    # infill missing values based on best model
    ## re-subset temp_df based on best model
    temp_df <- subset(dat, doy %in% doyrange)
    # if infill source is a logger, subset data to that logger only
    if(grepl("cr", best$site)){
      # id logger col
      logcol <- paste0(best$site, "_logger") 
      #subset to that logger
      temp_df <- filter(temp_df, get(logcol) == best$logger)
    } else{
      logger <- NA
    }
    # iterate through mean T and DTR, run lm, predict missing values, and store results in data frame
    best_mod <- lm(formula = paste0(target_site, "_ln", " ~ ", best$site, "_ln"), data = temp_df)
    # if source station had 0 rainfall on missing date, record -Inf for target (exponentiates to 0), else predict value
    if(dat[[paste0(best$site,"_clean")]][dat$date == d] == 0){
      tempinfill <- -Inf
    }else{
      tempinfill <- predict(best_mod, newdata = subset(dat, date == d))
    }
    tempinfill_df <- data.frame(date = d, infill = tempinfill,
                                source.station = best$site,
                                logger = best$logger, 
                                pval = summary(best_mod)$coefficients[8],
                                r2 = summary(best_mod)$r.squared,
                                n.obs = nrow(best_mod$model),
                                equation = paste0("y = ", best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                infillrun = length(d),
                                method = "multi-yr")
    
    # append model infill to master infill df
    infill_df_m2 <- rbind(infill_df_m2, tempinfill_df)
    
    # clean up things that shouldn't be recycled
    rm(r2_df, tempinfill_df, best, logger)
    
  }
  # return infilled df
  return(infill_df_m2)
}


# function to backfill days with accumulated ppt
backfill_ppt <- function(dat){
  # ID qdays > 1 date
  qdays_dates <- subset(dat, qdays > 1) %>%
    dplyr::select(date, ppt_tot, qdays)
  
  # add empty col for storing backfilled c1 ppt vals, source of backfill, and empty date vector for dates where no complete companion station date available for backfilling
  dat$backfill <- NA
  dat$Flag.1 <- NA
  dat$Flag.2 <- NA
  nobackfill <- NULL
  
  # test for loop
  for(i in 1:nrow(qdays_dates)){
    print(paste("Backfilling", qdays_dates$date[i]))
    
    # specify date seq to backfill
    event_dates <- seq.Date(qdays_dates$date[i] - (qdays_dates$qdays[i]-1), qdays_dates$date[i], 1)
    temp_df <- subset(dat, date %in% event_dates)
    
    # if accumulated ppt is 0, backfill all missing dates with 0
    if(qdays_dates$ppt_tot[i] == 0){
      dat$backfill[dat$date %in% event_dates] <- 0
      dat$infilldays[dat$date %in% event_dates] <- qdays_dates$qdays[i]
      dat$Flag.1[dat$date %in% event_dates] <- "A"
      dat$Flag.2[dat$date %in% event_dates] <- "G"
      print("0 accumulated ppt, 0s backfilled")
      next
    }
    
    # else, if accumulated ppt non-zero, backfill using infill values
    # sum project infill total
    infill_tot <- sum(temp_df$exp.ppt)
    # if infill sum not 0 when total accumulated is non-zero, proceed
    if(!(infill_tot ==0 & qdays_dates$ppt_tot[i] >0)){
      # calculate daily relative contribution of snotel ppt
      temp_df$relppt <- temp_df$exp.ppt/infill_tot
      dat$backfill[dat$date %in% event_dates] <- qdays_dates$ppt_tot[i] * temp_df$relppt
      dat$infilldays[dat$date %in% event_dates] <- qdays_dates$qdays[i]
      dat$Flag.2[dat$date %in% event_dates] <- "B"
      print("chart ppt backfilled based on source station")
      next
    } else{
      # store date for infill
      nobackfill <- c(nobackfill, as.character(qdays_dates$date[i]))
      dat$Flag.2[dat$date %in% event_dates] <- "0 ppt accumulated at all source stations"
      print("0 accumulated at source station")
    }
  }
  # infill Flag 1 for values backfilled by a source station
  Flag2B_dates <- dat$date[dat$Flag.2 == "B" & !is.na(dat$Flag.2)]
  for(f in Flag2B_dates){
    if(dat$method[dat$date == f] == "seasonal"){
      dat$Flag.1[dat$date == f] <- ifelse(dat$pval[dat$date == f] <= 0.05, "B", "C") #B < 0.05, C > 0.05 for meth1
    }else{
      # will be method 2 (historic fill)
      dat$Flag.1[dat$date == f] <- ifelse(dat$pval[dat$date == f] <= 0.05, "D", "E") #D < 0.05, E > 0.05 for meth2
    }
  }
  # infill missing days
  print("Infilling missing days (no period total/accumulated ppt):")
  dat <- infill_singles(dat)
  print("0 accumulated at source station for:")
  print(nobackfill)
  return(dat)
}


# function to infill days that are truly missing (no accumulated ppt)
## needs to be run AFTER backfill days (or can tuck function inside backfill ppt at the end)
infill_singles <- function(dat){
  single_dates <- dat$date[is.na(dat$Flag.2) & !is.na(dat$exp.ppt)]
  dat$backfill[dat$date %in% single_dates] <- dat$exp.ppt[dat$date %in% single_dates]
  dat$Flag.2[dat$date %in% single_dates] <- "A" # not adjustd for any period total (accumlated ppt)
  # assign Flag 1 based on method and pval
  for(f in as.character(single_dates)){
    print(paste("Infilling", f))
    f <- as.Date(f)
    if(dat$method[dat$date == f] == "seasonal"){
      dat$Flag.1[dat$date == f] <- ifelse(dat$pval[dat$date == f] <= 0.05, "B", "C") #B < 0.05, C > 0.05 for meth1
    }else{
      # will be method 2 (historic fill)
      dat$Flag.1[dat$date == f] <- ifelse(dat$pval[dat$date == f] <= 0.05, "D", "E") #D < 0.05, E > 0.05 for meth2
    }
  }
  return(dat)
}




# -- INFILL C1 PPT -----
# specify station hierarchies
c1_order <- c("c1cr", "sno", "sdl", "uc", "d1")
# specify missing dates -- only need to infill 2011 onwards
c1_missing_dates <- masterppt$date[is.na(masterppt$c1_clean) & masterppt$yr > 2010]

c1_seasonfill <- tk_ppt_seasonalfill(masterppt, target_site = "c1", c1_missing_dates, c1_order)
c1_longfill <- tk_ppt_historicfill(masterppt, "c1", c1_missing_dates, c1_order)

# iterate through each date and choose best model (pval under 0.05 and highest r2)
c1_choose <- data.frame()
for(d in c1_missing_dates){
  temp_df <- rbind(c1_seasonfill[c1_seasonfill$date == d,],
                   c1_longfill[c1_longfill$date == d,])
  best <- subset(temp_df, r2 == max(r2, na.rm = T) & pval <= 0.05)
  if(nrow(best) ==0){
    # choose highest r2 if no pval under 0.05
    best <- subset(temp_df, r2 == max(r2, na.rm = T) & !is.na(pval))
  }
  c1_choose <- rbind(c1_choose, best)
}
# check all dates there
summary(c1_missing_dates %in% c1_choose$date) # all there
# exponentiate infill values
c1_choose$exp.ppt <- exp(c1_choose$infill) 

#infill c1 with backfill function (backfill fxn contains infill_singles fxn to infill missing days)
# only needs to be done for 2011-01-01 onwards..
c1_2011 <- subset(c1_chartpcp, date >= "2011-01-01") %>%
  # pair c1 cr logger
  left_join(c1_choose)
# infill
c1_2011.2 <- backfill_ppt(c1_2011)
# need to re-run predictions for following dates with different source station than in best model:
# [1] "0 accumulated at source station for:"
# [1] "2013-06-11" "2013-06-18" "2013-06-25" "2014-06-13" "2014-06-17" "2018-03-06"

# look at masterppt for June 2013 to see if any stations have non-zero ppt
View(subset(masterppt, yr == 2013 & mon == 6))
# only c1 cr logger has non-zero in june 2013
# look at masterppt for June 2014
View(subset(masterppt, yr == 2014 & mon == 6))
# only c1 cr has non-zero values
# look at masterppt for Mar 2018
View(subset(masterppt, yr == 2018 & mon == 3))
# c1cr, d1 and uc have non-zero values in missing window

# see if can use functions are they are, just with missing dates and only c1cr station (since closest to c1)
c1_dates_remain <- c1_missing_dates[!c1_missing_dates %in% c1_2011.2$date[!is.na(c1_2011.2$backfill)]]
# check if these are the dates expected to infill
c1_dates_remain #yes

c1_remain_seasonfill <- tk_ppt_seasonalfill(masterppt, target_site = "c1", c1_dates_remain, "c1cr", nobs_limit = 1)
c1_remain_longfill <- tk_ppt_historicfill(masterppt, "c1", c1_dates_remain, "c1cr", nobs_limit = 1)
# > note: need remove limit on nobs or else c1cr as source.station doesn't pass checks in infill script
# > better than nothing or dividing accumulated by number of days to infill?

# iterate through each date and choose best model (pval under 0.05 and highest r2)
c1_remain_choose <- data.frame()
for(d in c1_dates_remain){
  temp_df <- rbind(c1_remain_seasonfill[c1_remain_seasonfill$date == d,],
                   c1_remain_longfill[c1_remain_longfill$date == d,])
  best <- subset(temp_df, r2 == max(r2, na.rm = T) & pval <= 0.05)
  if(nrow(best) ==0){
    # choose highest r2 if no pval under 0.05
    best <- subset(temp_df, r2 == max(r2, na.rm = T) & !is.na(pval))
  }
  c1_remain_choose <- rbind(c1_remain_choose, best)
}
# check all dates there
summary(c1_dates_remain %in% c1_remain_choose$date) # all there
# exponentiate infill values
c1_remain_choose$exp.ppt <- exp(c1_remain_choose$infill) 

# only needs to be done for 2011-01-01 onwards..
c1_remain_2011 <- subset(c1_chartpcp, date %in% c1_dates_remain) %>%
  # pair c1 cr logger
  left_join(c1_remain_choose)
# infill
c1_2011.remain <- backfill_ppt(c1_remain_2011)
# update flag 2 to I
c1_2011.remain$Flag.2 <- "I" #infilled from same source (but wasn't the top choice model), that has non-zero total during c1 accumulated period

# remove c1_2011.remain rows from c1_2011.2 and rbind c1_2011.remain
c1_2011.3 <- subset(c1_2011.2, !date %in% c1_2011.remain$date) %>%
  rbind(c1_2011.remain) %>%
  arrange(date)
# be sure no dates duplicated
summary(duplicated(c1_2011.3$date)) # nope, all good

#infill flags for dates not infilled (flag.1 = A and flag.2 = A)
View(subset(c1_2011.3, is.na(Flag.1)))
c1_2011.3 <- mutate(c1_2011.3,
                    Flag.1 = ifelse(is.na(Flag.1) & is.na(equation), "A", Flag.1),
                    Flag.2 = ifelse(is.na(Flag.2) & is.na(equation), "A", Flag.2),
                    # specify C1 as source
                    source.station = ifelse(Flag.1 == "A" & Flag.2 == "A", "C1 Belfort Shielded", source.station))





# -- INFILL D1 PPT -----
# specify station hierarchies
d1_order <- c("sdl", "uc", "c1cr", "c1", "sno")
# specify missing dates -- only need to infill 2011 onwards
d1_missing_dates <- masterppt$date[is.na(masterppt$d1_clean) & masterppt$yr > 2010]

d1_seasonfill <- tk_ppt_seasonalfill(masterppt, "d1", d1_missing_dates, d1_order)
d1_longfill <- tk_ppt_historicfill(masterppt, "d1", d1_missing_dates, d1_order)

# iterate through each date and choose best model (pval under 0.05 and highest r2)
d1_choose <- data.frame()
for(d in d1_missing_dates){
  temp_df <- rbind(d1_seasonfill[d1_seasonfill$date == d,],
                   d1_longfill[d1_longfill$date == d,])
  best <- subset(temp_df, r2 == max(r2, na.rm = T) & pval <= 0.05)
  if(nrow(best) ==0){
    # choose highest r2 if no pval under 0.05
    best <- subset(temp_df, r2 == max(r2, na.rm = T) & !is.na(pval))
  }
  d1_choose <- rbind(d1_choose, best)
}
# check all dates there
summary(d1_missing_dates %in% d1_choose$date) # all there
# exponentiate infill values
d1_choose$exp.ppt <- exp(d1_choose$infill) 

#infill c1 with backfill function (backfill fxn contains infill_singles fxn to infill missing days)
# only needs to be done for 2011-01-01 onwards..
d1_2011 <- subset(d1_chartpcp, date >= "2011-01-01") %>%
  # pair c1 cr logger
  left_join(d1_choose)
# remove "d1_" from colnames so backfill fxn runs
colnames(d1_2011) <- gsub("d1_", "", colnames(d1_2011))
# infill
d1_2011.2 <- backfill_ppt(d1_2011) # do not need to re-run any predictions, all the best models okay for infilling
# think this happened bc so many missing obs (lack of accumulated periods)

#infill flags for dates not infilled (flag.1 = A and flag.2 = A)
View(subset(d1_2011.2, is.na(Flag.1)))
d1_2011.2 <- mutate(d1_2011.2,
                    Flag.1 = ifelse(is.na(Flag.1) & is.na(equation), "A", Flag.1),
                    Flag.2 = ifelse(is.na(Flag.2) & is.na(equation), "A", Flag.2),
                    # specify D1 as source
                    source.station = ifelse(Flag.1 == "A" & Flag.2 == "A", "D1 Belfort Shielded", source.station))



# -- REVIEW INFILLED VALUES -----
# c1
mutate(c1_2011.3,
       backfill = ifelse(is.na(backfill), ppt_tot, backfill)) %>%
  ggplot(aes(date, backfill, col = is.na(equation))) +
  geom_point(alpha = 0.5) # biggest value is an infilled value..
# > biggest value was on sep 12, 2013, which was the 2013 floods (sep 11-13), so that makes sense
# > was also backfilled from an accumulated total so more likely a real value (or close enough)

mutate(c1_2011.3,
       backfill = ifelse(is.na(backfill), ppt_tot, backfill),
       doy = yday(date),
       yr = year(date)) %>%
  ggplot(aes(doy, backfill, col = is.na(equation))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~yr) # looks reasonable

# look at vals by month
mutate(c1_2011.3,
       backfill = ifelse(is.na(backfill), ppt_tot, backfill),
       doy = yday(date),
       yr = year(date),
       mon = month(date)) %>%
  ggplot(aes(doy, backfill, col = is.na(equation))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~mon, scales = "free_x") # looks reasonable

# d1
mutate(d1_2011.2,
       backfill = ifelse(is.na(backfill), ppt_tot, backfill)) %>%
  ggplot(aes(date, backfill, col = is.na(equation))) +
  geom_point(alpha = 0.5)

mutate(d1_2011.2,
       backfill = ifelse(is.na(backfill), ppt_tot, backfill),
       doy = yday(date),
       yr = year(date)) %>%
  ggplot(aes(doy, backfill, col = is.na(equation))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~yr) # seems reasonable.. there is a high value summer 2012, but maybe that event actually occured if predicted by other stations?

# look at vals by month
mutate(d1_2011.2,
       backfill = ifelse(is.na(backfill), ppt_tot, backfill),
       doy = yday(date),
       yr = year(date),
       mon = month(date)) %>%
  ggplot(aes(doy, backfill, col = is.na(equation))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~mon, scales = "free_x") # 2 relatively high infilled values for july and aug, but highest is 1.5inch/day which maybe could happen
# also looking at the value, it came from accumulated precip backfill, so more likely it's a real value (or close enough)



# -- COMPILE INFILLED, COMPLETE DATASETS -----
# append infill 2011 onwards datasets to Tim Kittel infilled datasets

# make list of functions to clean/standardize station names across both datasets
clean_stations <- list(function(x) gsub("elford", "elfort", x),
                       function(x) gsub(" Cr", "-cr", x),
                       function(x) gsub(" Dp", "-dp", x),
                       function(x) gsub("cr-", "-", x),
                       function(x) gsub("c1", "C1", x),
                       function(x) gsub("slddp", "SDL-dp", x),
                       function(x) gsub("d1", "D1", x),
                       function(x) gsub("sdl", "SDL",x),
                       function(x) gsub("_NA", "", x),
                       function(x) gsub("loch", "Loch", x),
                       function(x) gsub("sno", "Niwot Snotel (663)", x),
                       function(x) gsub("uc", "University Camp Snotel (838)", x))

# c1
c1_compiled <- tkc1 %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d"),
         doy = yday(date),
         # add LTER and site cols
         LTER_site = "NWT",
         local_site = "C1") %>%
  rename_all(function(x) casefold(x)) %>%
  # clean up double periods in colname (nobs and precip col)
  rename_all(function(x) gsub("x..", "x.", x)) %>%
  rename_all(function(x) gsub("..m", ".m", x)) %>%
  rename_all(function(x) gsub("mm.", "mm", x)) %>%
  #re-arrange cols
  dplyr::select(lter_site, local_site, date, doy, year, month, day, precipitation.mm:regression.equation) %>%
  rename(LTER_site = lter_site)

# prep qa'd c1 dat for joining (exclude dates infilled)
c1_2011prep <- c1_2011.3 %>%
  # add cols in TK dataset
  mutate(day = day(date),
         year = year(date),
         month = month(date),
         doy = yday(date),
         # append logger if logger used to infill
         source.station = ifelse(!is.na(logger), paste(source.station, logger, sep = "-"), source.station),
         # create final ppt col
         precipitation.mm = ifelse(!is.na(backfill), backfill, ppt_tot)) %>%
  #rearrange cols to match tk's
  dplyr::select(LTER_site:date,doy, year, month, day, precipitation.mm, Flag.1, Flag.2, source.station, pval, r2, n.obs, equation) %>%
  # capitalize local site to match TK temp datasets
  mutate(local_site = "C1")

# rename cols in predicted c1 dat and rbind all (all dates should be accounted for and only 1 row per date)
colnames(c1_2011prep) <- colnames(c1_compiled)
glimpse(c1_2011prep)

c1_compiled <- rbind(c1_compiled, c1_2011prep) %>%
  arrange(date)
# check only 1 row per date
summary(duplicated(c1_compiled$date)) # good
# check for missing dates
summary(c1_compiled$date %in% seq.Date(min(c1_compiled$date), max(c1_chartpcp$date), 1)) # everything there
# append raw dataset for user comparison
c1_compiled2 <- left_join(c1_compiled, c1_chartpcp[c("date", "ppt_tot", "qdays")]) %>%
  rename(raw_ppt_tot = ppt_tot,
         raw_qdays = qdays) %>%
  # remove 2019 -- is incomplete year
  filter(year < 2019)

# clean up station names
for(i in clean_stations){
  c1_compiled2$source.station <- sapply(c1_compiled2$source.station, i)
}
# check station names
unique(c1_compiled2$source.station) #good
unique(c1_compiled2$local_site) #good

glimpse(c1_compiled2)
summary(c1_compiled2)



# d1
d1_compiled <- tkd1 %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d"),
         doy = yday(date),
         # add LTER and site cols
         LTER_site = "NWT",
         local_site = "D1") %>%
  rename_all(function(x) casefold(x)) %>%
  # clean up X obs colname
  rename_all(function(x) gsub("x..", "x.", x)) %>%
  rename(precipitation.mm = d1.mm.ppt) %>%
  #re-arrange cols
  dplyr::select(lter_site, local_site, date, doy, year, month, day, precipitation.mm:regression.equation) %>%
  rename(LTER_site = lter_site)

# prep qa'd c1 dat for joining (exclude dates infilled)
d1_2011prep <- d1_2011.2 %>%
  # add cols in TK dataset
  mutate(day = day(date),
         year = year(date),
         month = month(date),
         doy = yday(date),
         # append logger if logger used to infill
         source.station = ifelse(!is.na(logger), paste(source.station, logger, sep = "-"), source.station),
         # create final ppt col
         precipitation.mm = ifelse(!is.na(backfill), backfill, ppt_tot)) %>%
  #rearrange cols to match tk's
  dplyr::select(LTER_site:date,doy, year, month, day, precipitation.mm, Flag.1, Flag.2, source.station, pval, r2, n.obs, equation) %>%
  # capitalize local site to match TK temp datasets
  mutate(local_site = "D1")

# rename cols in predicted d1 dat and rbind all (all dates should be accounted for and only 1 row per date)
colnames(d1_2011prep) <- colnames(d1_compiled)
glimpse(d1_2011prep)

d1_compiled <- rbind(d1_compiled, d1_2011prep) %>%
  arrange(date)
# check only 1 row per date
summary(duplicated(d1_compiled$date)) # good
# check for missing dates
summary(d1_compiled$date %in% seq.Date(min(d1_compiled$date), max(d1_chartpcp$date), 1)) # everything there
# append raw dataset for user comparison
d1_compiled2 <- left_join(d1_compiled, d1_chartpcp[c("date", "d1_ppt_tot", "d1_qdays")]) %>%
  rename(raw_ppt_tot = d1_ppt_tot,
         raw_qdays = d1_qdays) %>%
  # remove 2019 -- is incomplete year
  filter(year < 2019)

# clean up station names
for(i in clean_stations){
  d1_compiled2$source.station <- sapply(d1_compiled2$source.station, i)
}
# check station names
unique(d1_compiled2$source.station) #good
unique(d1_compiled2$local_site) #good

glimpse(d1_compiled2)
summary(d1_compiled2)



# -- WRITE OUT INFILLED DATASETS ----
# c1
write_csv(c1_compiled2, "climate_d1_c1/output_data/c1_dailyppt_infilled_1952-2018.csv")

# d1
write_csv(d1_compiled2, "climate_d1_c1/output_data/d1_dailyppt_infilled_1952-2018.csv")




























# -- OLD CODE TO INFILL BY CHART METADATA METHODS (station hierarchy) -----
# code below here not used to infill, was initially tested by ctw before figuring out TK's method and replicating his results
# ctw used tk method (above) to infill C1 and D1 chart precip



# old notes:
#.. perhaps try using snotel ppt to inform when it was raining (percent of rainfall for that event)
# then apportion ppt backwards based on that, where data are available

# what i need to do is calculate rainfall events (total rain per storm), and determine relative contribution of each day, from start storm-event date to end storm-event date, to total event ppt
# .. or could run for loop to iterate through qdays > 1, extract dates in qdays window, then lookup rel ppt contribution in snotel to apportion in c1 chart..


# -- FOR LOOP TO BACKFILL QDAYS > 1 -----
# only needs to be done for 2011-01-01 onwards..
c1_2011 <- subset(c1_chartpcp, date >= "2011-01-01") %>%
  # pair c1 cr logger
  left_join(c1cr_ppt) %>%
  #pair snotel (niwot 663 station near C1)
  left_join(dplyr::select(snotel_ppt, -local_site)) %>%
  # pair raw saddle
  left_join(dplyr::select(sdl_chartpcp, -c(local_site, sdl_flag_ppt_tot))) %>%
  # join d1 raw
  left_join(dplyr::select(d1_chartpcp, -c(local_site, d1_flag_ppt_tot))) %>%
  # join snotel university camp station (near albion)
  left_join(dplyr::select(uc_ppt, -c(local_site))) %>%
  # reorganize cols
  select(LTER_site:date, yr:doy, ppt_tot:ncol(.))
# for correlative purposes, mark whether both sites recorded rain/no-rain on same days
c1_2011$c1sno_rained <- with(c1_2011, (ppt_tot > 0 & sno_ppt_mm > 0) | 
                               ppt_tot == 0 & sno_ppt_mm == 0)
summary(c1_2011$c1sno_rained[c1_2011$qdays == 1]) # stations agree in rain/no rain roughly 77% of the time on days w/out NAs
# when stations don't agree, what does the discrep look like? [discarding days that might not match bs qdays > 1]
with(c1_2011, plot(ppt_tot[c1sno_rained == FALSE & qdays == 1], sno_ppt_mm[c1sno_rained == FALSE & qdays == 1]))
# > typically differs by a small amt (<10mm)

# how many flags per yr? 
sapply(split(c1_2011$qdays, year(c1_2011$date)), function(x) summary(x > 1))
# ID qdays > 1 date
qdays_dates <- subset(c1_2011, qdays > 1) %>%
  dplyr::select(date, ppt_tot, qdays, c1cr_ppt_tot, c1cr_flag_ppt_tot, sno_ppt_mm, sdl_ppt_tot, sdl_qdays, d1_ppt_tot, d1_qdays)

# add empty col for storing backfilled c1 ppt vals, source of backfill, and empty date vector for dates where no complete companion station date available for backfilling
c1_2011$c1_backfill <- NA
c1_2011$c1_backsource <- NA
nobackfill <- NULL

# test for loop
for(i in 1:nrow(qdays_dates)){
  # specify date seq to backfill
  event_dates <- seq.Date(qdays_dates$date[i] - (qdays_dates$qdays[i]-1), qdays_dates$date[i], 1)
  temp_df <- subset(c1_2011, date %in% event_dates)
  
  # if accumulated ppt is 0, backfill all missing dates with 0
  if(qdays_dates$ppt_tot[i] == 0){
    c1_2011$c1_backfill[c1_2011$date %in% event_dates] <- 0
    c1_2011$c1_backsource[c1_2011$date %in% event_dates] <- "backfill: 0 accumulated (c1)"
    print("0 accumulated ppt, 0s backfilled")
    next
  }
  
  # else, if accumulated ppt non-zero, backfill using site adjacency as hierarchy:
  # 1) start with c1 cr logger
  # if no NAs and no estimated values, proceed
  if(sum(is.na(temp_df$c1cr_ppt_tot))==0 & !("e" %in% temp_df$c1cr_flag_ppt_tot)){
    crtot <- sum(temp_df$c1cr_ppt_tot)
    # calculate daily relative contribution of snotel ppt
    temp_df$relppt <- temp_df$c1cr_ppt_tot/crtot
    c1_2011$c1_backfill[c1_2011$date %in% event_dates] <- qdays_dates$ppt_tot[i] * temp_df$relppt
    c1_2011$c1_backsource[c1_2011$date %in% event_dates] <- paste("backfill:", unique(temp_df$logger)[1])
    print("c1 chart ppt backfilled based on c1 cr logger")
    next
  }
  
  # 1) start with snotel site...
  ## tally snotel ppt in infill window
  snotot <- sum(temp_df$sno_ppt_mm)
  ## if snotel tally non-zero & c1 accumulated ppt non-zero, proceed with backfill:
  if(snotot > 0 & !is.na(snotot)){
    # calculate daily relative contribution of snotel ppt
    temp_df$relppt <- temp_df$sno_ppt_mm/snotot
    c1_2011$c1_backfill[c1_2011$date %in% event_dates] <- qdays_dates$ppt_tot[i] * temp_df$relppt
    c1_2011$c1_backsource[c1_2011$date %in% event_dates] <- "backfill: Snotel Niwot 663"
    print("c1 chart ppt backfilled based on snotel")
    next
  }
  
  # 2) sdl next...
  # note: no winter correction applied in raw sdl ppt data, but shouldn't matter here as backfilling is based on relative amts
  ## first check that sdl has no missing data in time period (else move on to d1:
  if(all(temp_df$sdl_qdays == 1) & !sum(temp_df$sdl_ppt_tot) %in% c(NA, 0)){
    sdltot <- sum(temp_df$sdl_ppt_tot)
    temp_df$relppt <- temp_df$sdl_ppt_tot/sdltot
    c1_2011$c1_backfill[c1_2011$date %in% event_dates] <- qdays_dates$ppt_tot[i] * temp_df$relppt
    c1_2011$c1_backsource[c1_2011$date %in% event_dates] <- "backfill: sdl"
    print("c1 chart ppt backfilled based on sdl")
    next
  }
  
  # 3) d1 next ...
  if(all(temp_df$d1_qdays == 1) & !sum(temp_df$d1_ppt_tot) %in% c(NA, 0)){
    d1tot <- sum(temp_df$d1_ppt_tot)
    temp_df$relppt <- temp_df$d1_ppt_tot/d1tot
    c1_2011$c1_backfill[c1_2011$date %in% event_dates] <- qdays_dates$ppt_tot[i] * temp_df$relppt
    c1_2011$c1_backsource[c1_2011$date %in% event_dates] <- "backfill: d1"
    print("c1 chart ppt backfilled based on d1")
    next
  } 
  
  #4) university camp snotel...
  if(sum(temp_df$uc_ppt_mm)>0){
    uctot <- sum(temp_df$uc_ppt_mm)
    temp_df$relppt <- temp_df$uc_ppt_mm/uctot
    c1_2011$c1_backfill[c1_2011$date %in% event_dates] <- qdays_dates$ppt_tot[i] * temp_df$relppt
    c1_2011$c1_backsource[c1_2011$date %in% event_dates] <- "backfill: Snotel Univ. Camp (838)"
    print("c1 chart ppt backfilled based on Snotel University Camp")
  } else{
    # store date for infill
    nobackfill <- c(nobackfill, as.character(qdays_dates$date[i]))
    c1_2011$c1_backsource[c1_2011$date %in% event_dates] <- "no stations available for backfill"
  }
}

# are there any dates that didn't get backfilled?
nobackfill # no!

# creat clean col of ppt with chart temp where qdays == 1 and backfilled ppt
c1_2011 <- mutate(c1_2011,
                  c1_ppt_clean = ifelse(!is.na(c1_backfill), c1_backfill, ppt_tot))

# what is still missing?
with(c1_2011, sapply(split(c1_ppt_clean, yr), function(x) summary(is.na(x))))
# 2011 -- 1 day
# 2013 -- 15 days
# 2014 -- 7 days
# 2015 -- 11 days
# 2016 -- 8 days... (42 days total still missing ppt)


# proceed with chart metadata infill methods...


# -- INFILL NO RAIN -----
# if all comparative stations at c1 (cr logger and snotel) have 0 rainfall recorded, assign 0 to c1 chart
c1_2011$norain <- with(c1_2011, is.na(c1_ppt_clean) & c1cr_ppt_tot %in% c(NA,0) & sno_ppt_mm == 0)
c1_2011$c1_ppt_clean[c1_2011$norain] <- 0 

# what is still missing?
with(c1_2011, sapply(split(c1_ppt_clean, yr), function(x) summary(is.na(x))))
# 2011 -- 1 day
# 2013 -- 12 days
# 2014 -- 6 days
# 2015 -- 9 days
# 2016 -- 4 days... 32 in total still NA



