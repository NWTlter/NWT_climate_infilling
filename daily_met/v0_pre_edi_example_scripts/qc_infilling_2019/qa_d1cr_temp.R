# qa d1 logger temp (for d1 chart infilling)


# script purpose:
# read in all campbell scientific logger datasets, and sdl, d1, and c1 chart temp datasets for comparison
# tidy datasets
# apply routine checks to flag suspect values relative to nearby temp trends (i.e. comparative datasets)
# flag values
## > issue warnings for vals that meet flagging criteria but aren't clearly improbably enough (NA while working through working_dat, but replace with original value before writing out)
## > flagged vals that are clearly improbable get NA'd
# write out QA dataset with flags for next step (infill d1 chart)


# notes:
# color scheme in panel figures:
# black = d1 logger, purple = d1 chart, blue = sdl cr loggers, green = sdl chart, yellow = c1 chart

# jen morse writes campbell 207 temp/rh probe was used at saddle (presumably d1 + c1 too) until dec 1999, when switched to cs500
# cs 500 switched to Vaisala HMP 155a in 2017
# campbell scientific website does not list range detection limits for 207 probe (bc retired?) but writes the 207 was camporable to 107 and 108 temp probes
# detection limit for 107 is -35C to +50C, detection limit for 108 is -5C to +95C
# operating range for cs 500 probe = -40° to +60°C (https://www.campbellsci.com/cs500-l)

# note about script warnings.. 
# can ignore warnings throughout. will either issue warnings when missing data plotted (e.g. ggplot tells you how many pts removed due to missing data)
# warnings also issued over "d1cr_temp" col uninitialised. junky message from using function? (likely due to object being a tibble or some other hadley object)



# -- SETUP ------
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN")

# set pathway to climate output data folder (qa'd data lives in prep_data)
datpath <- "climate_d1_c1/output_data/"

# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")
# source qa functions
source("extended_summer/analysis/scripts/qa_functions.R")


# -- GET DATA -----
# d1 cr loggers  -- most current dataset not on EDI yet
## d1 cr21x, 1986-2000
d1cr21 <- getTabular(70) %>% data.frame()
## d1 cr23x and cr1000, 2000 - ongoing
#d1cr <- getTabular(402) %>% data.frame() 
d1logs <- read_csv("~/Documents/nwt_lter/unpub_data/d-1cr23x-cr1000.daily.ml.data.csv",
                   na = na_vals, trim_ws = T) # updated set not on EDI yet, from SCE 6/23/19

# ctw qa'd sdl logger data
sdllog_qa <- read_csv("extended_summer/analysis/output_data/prep_data/qa_sdlcr_temp.csv",
                      na = na_vals, trim_ws = T) %>% data.frame() %>%
  # need to remove overlapping dates in sdl cr23x and cr21x
  subset(!(logger == "cr23x" & date %in% seq.Date(as.Date("2000-06-24"), as.Date("2000-06-27"), 1)))

# sdl chart
#sdl <- getTabular(413) %>% data.frame()
# ctw qa'd sdl chart temp data
sdl_qa <- read_csv("extended_summer/analysis/output_data/prep_data/qa_sdlchart_temp.csv",
                   na = na_vals, trim_ws = T) %>% data.frame()

# d1 chart
d1 <- getTabular(412) %>% data.frame()
# c1 chart
c1 <- getTabular(411) %>% data.frame()



# -- REVIEW DATA -----
# review how dats read in
glimpse(d1cr21) # no flag cols, date is date class
glimpse(d1logs) # flag cols present, date is date class
glimpse(sdllog_qa) # date = date
glimpse(sdl_qa) # flags cols present
glimpse(d1) # flag cols present
glimpse(c1) # flag cols present

# clean up d1 cr23x and cr1000 data
# drop cols not needed in cr logger dataset (i.e. only need airtemp cols up thru avg airtemp, flag cols not useful since only n or NA)
d1logs <- d1logs[,1:12]
# unique values of flags? and frequency of their occurrence?
sapply(d1logs[grepl("flag", colnames(d1logs))], function(x) summary(as.factor(x))) # only n's, correspond to no flag -- not useful

# clean up 21x data 
# drop non airtemp cols in cr21x
d1cr21 <- d1cr21 %>%
  # add logger, LTER_site, and local_site cols so simlar to other datasets
  mutate(LTER_site = "NWT",
         local_site = "d1", 
         flag_airtemp_max = NA,
         flag_airtemp_min = NA,
         flag_airtemp_avg = NA) %>%
  # select same cols as in cr23x and cr1000 datasets + time of temp cols
  dplyr::select(c(colnames(d1logs)))

#stack data
d1crall <- rbind(d1cr21, d1logs) %>% arrange(date)


# review flags in reference datasets 
# sdl
sapply(sdl_qa[grepl("flag", colnames(sdl_qa))], function(x) summary(as.factor(x))) # 2 types of flags, type 1 used most often
# > saddle chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)
# d1
sapply(d1[grepl("flag", colnames(d1))], function(x) summary(as.factor(x))) # 3 types of flags.. type 1 and type 2 nearly equally used
# > d1 chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available); 3 not explained
# c1
sapply(c1[grepl("flag", colnames(c1))], function(x) summary(as.factor(x))) # 2 types of flags (same as sdl), mostly infilled via type 1
# > c1 chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)
# >> conclusion: keep flag cols in chart temp datasets, useful info


# tidy chart temp datatsets (qa'd sdl chart and sdl logger already long-form)
d1crall_long <- tidytemp(d1crall, datasource = "d1cr", dropcol = "airtemp_avg")
d1_long <- tidytemp(d1, datasource = "d1", dropcol = "airtemp_avg")
c1_long <- tidytemp(c1, datasource = "c1", dropcol = "airtemp_avg")

# prep names in ctw qa'd datasets to join with d1 cr logger datasets (i.e. no redundant colnames across datasets)
names(sdl_qa)
names(sdllog_qa)

colnames(sdl_qa)[11:12] <- c("sdl_qaflag", "sdl_daysflat")
colnames(sdllog_qa)[9:ncol(sdllog_qa)]<- c("sdlcr_temp", "sdlcr_qatemp", "sdlcr_qaflag")



# -- SCREEN OBVIOUS OUTLIERS IN REFERENCE DATASETS -----
# screen any obvious outliers in chart (non ctw qa'd) datasets (i.e. don't use these values to compare with d1 logger data points)
## d1 chart
with(d1_long, sapply(split(d1_temp, met), summary))
with(d1_long, lapply(split(d1_temp[met == "airtemp_max"], mon[met=="airtemp_max"]), summary))
with(d1_long, sapply(split(d1_temp, met), function(x) tail(sort(x))))
with(d1_long, sapply(split(d1_temp, met), function(x) head(sort(x)))) # all okay
## c1 chart
with(c1_long, sapply(split(c1_temp, met), summary))
with(c1_long, sapply(split(c1_temp[met == "airtemp_max"], mon[met=="airtemp_max"]), summary))
with(c1_long, sapply(split(c1_temp, met), function(x) tail(sort(x))))
with(c1_long, sapply(split(c1_temp, met), function(x) head(sort(x)))) # all okay              


# join d1 logger and all comparative temp datasets
d1crall_long_master <- mutate(d1crall_long, qa_flag = NA) %>% # add empty qa flag col first
  left_join(d1_long) %>%
  left_join(dplyr::select(sdllog_qa, -c(local_site, logger))) %>%
  left_join(dplyr::select(sdl_qa, -local_site)) %>%
  left_join(dplyr::select(c1_long, -local_site)) %>%
  ungroup() %>%
  data.frame()




# -- QA SENSOR FAILS (OBVIOUS OUTLIERS) -----
# create working copy
working_dat <- d1crall_long_master %>%
  # exclude flatline sdl chart vals from consideration
  mutate(sdl_qatemp = ifelse(grepl("flat", sdl_qaflag), NA, sdl_qatemp))

# look at tails for any obvious bad values
## logger temp, split by logger
with(working_dat, lapply(split(d1cr_temp, paste(met, logger)), function(x) tail(sort(x), n = 20))) 
# > looks like some values in Farenheit?
with(working_dat, lapply(split(d1cr_temp[met == "airtemp_min"], logger[met=="airtemp_min"]), function(x) head(sort(x), n = 25)))
# > -6999, also some Farenheit values?

# metadata says Hope changed d1 cr21x units to C in Apr 2017, but seem like all got converted?
# visualize
boxplot(d1cr_temp ~ yr, data = subset(working_dat, logger == "cr21x" & met == "airtemp_max")) #1997 & 1999 questionable
boxplot(d1cr_temp ~ mon, data = subset(working_dat, logger == "cr21x" & met == "airtemp_max" & yr == 1997)) #aug - oct 1997
boxplot(d1cr_temp ~ mon, data = subset(working_dat, logger == "cr21x" & met == "airtemp_max" & yr == 1999)) #sep & oct again
boxplot(d1cr_temp ~ yr, data = subset(working_dat, logger == "cr21x" & met == "airtemp_min")) #1997
boxplot(d1cr_temp ~ mon, data = subset(working_dat, logger == "cr21x" & met == "airtemp_min" & yr == 1997)) #sep & oct again

# > conclusions..
# campbell sci lit says range detection is -35C to +50C
# even if 80s/90s are Farenheit, that's really warm for D1 (e.g. Saddle doesn't get that warm in its record)
# flag and remove all values outside detection limit since not sure how else to treat (i.e. be strict, don't want to use those values for infilling)

# how many values exceed limits?
summary(working_dat$d1cr_temp > 50) #tmax
summary(working_dat$d1cr_temp < -35) #tmin
#review
sort(working_dat$d1cr_temp[working_dat$d1cr_temp > 50])
sort(working_dat$d1cr_temp[working_dat$d1cr_temp < -35])

# for context, what is min and max in d1 chart?
head(sort(working_dat$d1_temp), n = 20) # since d1 chart drops to -40, screen anything less than -50 for now..
tail(sort(working_dat$d1_temp), n = 20) #d1 chart doesn't get above 22

# flagging..
# -6999 = null data
working_dat$qa_flag[working_dat$d1cr_temp == -6999 & !is.na(working_dat$d1cr_temp)] <- "null data"
# remove -6999 values
working_dat$d1cr_temp[!is.na(working_dat$qa_flag)] <- NA
# check expected
with(working_dat, working_dat[!is.na(qa_flag), c("d1cr_temp", "qa_flag")]) #yes

# flag values outside detection limit
working_dat$qa_flag[(working_dat$d1cr_temp > 50 | working_dat$d1cr_temp < -50) & !is.na(working_dat$d1cr_temp)] <- "outside detection limit"
# remove bad values 
working_dat$d1cr_temp[grepl("outside", working_dat$qa_flag) & !is.na(working_dat$qa_flag)] <- NA



# -- FLAG DAILY DEPARTURES FROM COMPARATIVE DATASETS -----
# round 1 diff daily and flagging
working_dat <- diff_daily(working_dat, main = "d1cr_temp", comp1 = "d1_temp", comp2 = "sdlcr_qatemp", comp3 = "sdl_qatemp", 
                          groupvars = c("logger", "met", "mon"))  
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff1 <- filter(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff1 <- visual_qa(working_dat, check_daily_diff1, add_fourth = "c1_temp") # add c1 chart as fourth comparison
# tmax -- only cr21x and cr23x have flags (cr1000 vals okay)
plot_grid(plotlist = qa_daily_diff1[grep("cr21.*airtemp_max", qa_daily_diff1)]) #flag all
# what is the high value in Aug 1997 that's not flagged?
View(subset(working_dat, yr == 1997 & mon ==8)) #Aug-7-1997; one comparative source is NA but others issue flags
working_dat[working_dat$date == "1997-08-07" & working_dat$met == "airtemp_max", ] # add to list to flag
plot_grid(plotlist = qa_daily_diff1[grep("cr23.*airtemp_max", qa_daily_diff1)]) # flag all

# tmin -- cr21x is only logger with tmin flags
plot_grid(plotlist = qa_daily_diff1[grep("airtemp_min", qa_daily_diff1)])
# > flag 1992-08-31, 1997-06-16, all 1998, 1999 vals.. leave others alone for now (issue warnings only)


# round 1: all look bad excep tmin on 1996-12-19 (looks kind of of like temps ot shifted fwd one day for that week?) 
flag_dailydiff1 <- subset(check_daily_diff1, met == "airtemp_max" |
                            (met == "airtemp_min" & yr %in% 1998:1999) |
                            (met == "airtemp_min" & as.character(date) %in% c("1992-08-31", "1997-06-16"))) %>%
  #append 1997-08-07 tmax
  rbind(subset(working_dat, date == "1997-08-07" & met == "airtemp_max"))
# subset vals that will get warned but not NA'd in written out dataset
warn_dailydiff1 <- anti_join(check_daily_diff1, flag_dailydiff1)

# flag and remove values in working dataset
working_dat <- flag_temp(flag_dailydiff1, error = "comparative deviance") %>% data.frame()
working_dat <- flag_temp(warn_dailydiff1, error = "warning: comparative deviance") %>% data.frame()

# round 2 daily diff (recalc mean and deviance with bad deviance values from round 1 removed)
working_dat <- diff_daily(working_dat, rename = F, groupvars = c("logger", "met", "mon"))
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff2 <- filter(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff2 <- visual_qa(working_dat, check_daily_diff2, add_fourth = "c1_temp")
# only cr21x and cr23x have flags (cr1000 vals okay)
plot_grid(plotlist = qa_daily_diff2) #flag all tmax; flag only 1998-08-10 tmin, warn other tmin

# specify vals to flag and warn
flag_dailydiff2 <- subset(check_daily_diff2, met == "airtemp_max" | (met == "airtemp_min" & yr == 1998))
warn_dailydiff2 <- anti_join(check_daily_diff2, flag_dailydiff2)
# flag and warn
working_dat <- flag_temp(flag_dailydiff2, "comparative deviance") %>% data.frame()
working_dat <- flag_temp(warn_dailydiff2, "warning: comparative deviance") %>% data.frame()


# round 3: check extreme sdl deviance values
# what the distribution of deviances?
par(mfrow = c(1,3))
boxplot(working_dat$deviance_1, main = "d1 dev.")
boxplot(working_dat$deviance_2, main = "sdl cr dev.")
boxplot(working_dat$deviance_3, main = "sdl dev.")
par(mfrow = c(1,1))
# > d1 logger seems to have a more constant relationship with chart data.. makes sense since logger dat can spike

working_dat <- diff_daily(working_dat, rename = F, groupvars = c("logger", "met", "mon"))
# check logger temps that exceed daily deviance more than 5sds for sdl_chart and d1_chart
check_daily_diff3 <- filter(working_dat, round(deviance_1) >= 3 & round(deviance_2) >= 6 & round(deviance_3) >= 4)
# run through visual qa
qa_daily_diff3 <- visual_qa(working_dat, check_daily_diff3)
plot_grid(plotlist = qa_daily_diff3)
# flag tmin 1992-07-17, leave others alone
working_dat <- flag_temp(subset(check_daily_diff3, met == "airtemp_min" & date == "1992-07-17"))


# move on to checking grand and monthly tmax and tmin vals..
#clean up environment
rm(check_daily_diff1, flag_dailydiff1, warn_dailydiff1,
   check_daily_diff2, flag_dailydiff2, warn_dailydiff2,
   check_daily_diff3,
   qa_daily_diff1, qa_daily_diff2, qa_daily_diff3)



# -- QA EXTREMES (TMIN/TMAX) -----
## IMPORTANTE!!: 
## logger lifecycles: cr21x = 1980s-2000; cr23x = 2000 - 2012; cr1000 = dec 2012 - ongoing
## panel arrangement: cr21x = left panel, cr23x = middle, cr1000 = right panel

# visual qa grand max and min
# max temps by logger
check_max1 <- check_extreme(working_dat, groupvars = c("logger", "met"))
qa_max1 <- visual_qa(working_dat, check_max1, add_fourth = "c1_temp")
plot_grid(plotlist = qa_max1) 
# > there's no way D1 is that much warmer than C1 in 1988 -- flag 5 days in that stretch and high value in 2018
# what are the dates with high vals in Aug 1988?
View(subset(working_dat, met == "airtemp_max" & yr == 1988 & mon == 8)) #8/22 - 8/26

# >round 1:  flag 1997 and 2018 tmax, all else fine
flag_max1 <- filter(check_max1, met == "airtemp_max" & yr == 2018) %>% as.data.frame() %>%
  # append 5 days in Aug 1988 for tmax
  rbind(subset(working_dat, met == "airtemp_max" & date %in% seq.Date(as.Date("1988-08-22"), as.Date("1988-08-26"), 1)))
working_dat <- flag_temp(flag_max1, error = "high value")
# run through grand max once more
check_max2 <- check_extreme(working_dat, groupvars = c("logger", "met")) %>% as.data.frame()
qa_max2 <- visual_qa(working_dat, check_max2, add_fourth = "c1_temp")
plot_grid(plotlist = qa_max2) # doesn't seem like d1 should be warmer than c1 in august..
# ! however, since have chart infilled through 2010, and can infill with keith's data 2010-2013, going to focus more on scrutinizing values 2010-2018 (will flag earlier values when it's clear they should be flagged)
# flag July 1989 and move on
working_dat <- flag_temp(check_max2[check_max2$date == "1989-07-22" & check_max2$met == "airtemp_max",], error = "high value")

# min temps by logger
check_min1 <- check_extreme(working_dat, groupvars = c("logger", "met"), min)
qa_min1 <- visual_qa(working_dat, check_min1, add_fourth = "c1_temp")
plot_grid(plotlist = qa_min1) #dec 1996 is a low value, but generally looks alright. leave alone
# does 1996 have flags?
data.frame(check_min1[check_min1$date == as.Date("1996-12-17"),]) # deviance is over 3sd from D1 logger, and other sources are flagged
# > flag 1996 value for comparative deviance and move on
working_dat <- flag_temp(subset(check_min1, date == "1996-12-17"))

# move on to monthlies...
#clean up env
rm(check_max1, check_max2, qa_max1, qa_max2,
   check_min1, qa_min1, flag_max1)



# -- QA MONTHLY MIN/MAX -----
# monthly max temps of tmin and tmax
check_monthly_max1 <- check_extreme(working_dat, groupvars = c("logger", "met", "mon"))
# run through visual qa function
qa_mon_max1 <- visual_qa(working_dat, check_monthly_max1, sorttime = "mon", add_fourth = "c1_temp")

# visualize logger monthly maximums, by logger and metric
## max of airtemp_max
plot_grid(plotlist = qa_mon_max1[grep("cr21x.*airtemp_max", qa_mon_max1)]) # flag on 1991-08-10, 1990-09-08, and 1997-10-11 (all warmer than c1), and given trends with others, unrealistic. sdl logger data missing, so that's why not flagged in daily diff
plot_grid(plotlist = qa_mon_max1[grep("cr23x.*airtemp_max", qa_mon_max1)]) # flag 2007-04-30, all others okay
plot_grid(plotlist = qa_mon_max1[grep("cr1000.*airtemp_max", qa_mon_max1)]) # all okay
## max of airtemp_min
plot_grid(plotlist = qa_mon_max1[grep("cr21x.*airtemp_min", qa_mon_max1)]) #mar 1987.. vals look shifted fwd one day compared to other datasets, june 1989 looks like it had a sensor fail on unflagged day..
plot_grid(plotlist = qa_mon_max1[grep("cr23x.*airtemp_min", qa_mon_max1)]) # all okay
plot_grid(plotlist = qa_mon_max1[grep("cr1000.*airtemp_min", qa_mon_max1)]) #okay.. looks like d1 chart (purple line) consistently colder than d1 logger in 2014.. maybe through 2016. something to check on in temporal analysis (maybe were off for a period as in saddle chart?)

# gen obs > cr21x looks like it has the most funky values (spikes, errant vals), but cr23x and cr1000 look pretty good usually relative to the other datasets
# may not be much of an issue because can infill recent missing chart dataset with keith's infilled data, and by 2013, cr1000 comes onboard and is generally pretty good/reliable

# flag max monthlies
flag_maxmon1 <- subset(check_monthly_max1, date %in% as.Date(c("1991-08-10", "1990-09-08", "1997-10-11", "2007-04-30")) & met == "airtemp_max")
working_dat <- flag_temp(flag_maxmon1) # looking at flags, they are all +3SD outside normal variation range, so flag as comparative deviance                       

# look at mar mar 1987 min temp for shift fwd by 1 day, check if max off too
mar87_shift <- subset(working_dat, yr == 1987 & mon %in% c(2:5))
ggplot(mar87_shift, aes(date, main, group = met)) +
  geom_line() +
  geom_point() +
  geom_line(aes(date,comp1), col = "steelblue1") +
  geom_point(aes(date,comp1), col = "steelblue1") +
  geom_line(aes(date,comp2), col = "orchid") +
  geom_point(aes(date,comp2), col = "orchid") +
  geom_line(aes(date,comp3), col = "forestgreen") +
  geom_point(aes(date,comp3), col = "forestgreen") +
  geom_line(aes(date,c1_temp), col = "goldenrod") +
  geom_point(aes(date,c1_temp), col = "goldenrod") +
  facet_grid(met~.)
# > there is a break in logger data in late feb and early may, looks like logger data is shifted 1 day fwd for the period in between, also for some days after NA
# what is date of last NA in Mar?
start_date <- max(with(mar87_shift, date[mon == 2 & is.na(main) & met == "airtemp_min"]))
# visually inspect data for end date
View(subset(mar87_shift, mon == 5 & met == "airtemp_min")) # looks like 5-11 is stop date for shift
end_date <- as.Date("1987-05-11")            

# visually makes sure those are the values to adjust
ggplot(subset(mar87_shift, date >= (start_date-5) & date <= end_date), aes(date, main)) +
  geom_line(aes(date, main), alpha = 0.5) +
  #geom_point(aes(date, main), alpha = 0.5) +
  # add in proposed adjusted values
  geom_line(aes(date-1, main), col = "chocolate2", lwd = 2, alpha = 0.7) +
  #geom_point(aes(date-1, main), col = "chocolate2", size = 2) +
  geom_line(aes(date,comp1), col = "steelblue1", alpha = 0.5) +
  geom_point(aes(date,comp1), col = "steelblue1", alpha = 0.5) +
  geom_line(aes(date,comp2), col = "orchid", alpha = 0.5) +
  geom_point(aes(date,comp2), col = "orchid", alpha = 0.5) +
  geom_line(aes(date,comp3), col = "forestgreen", alpha = 0.5) +
  geom_point(aes(date,comp3), col = "forestgreen", alpha = 0.5) +
  #geom_line(aes(date,c1_temp), col = "goldenrod", alpha = 0.5) +
  #geom_point(aes(date,c1_temp), col = "goldenrod", alpha = 0.5) + ç
  scale_x_date(date_breaks = "2 weeks") +
  facet_grid(met~.) # looks more reasonable, and looks like tmax should be shift too

# shift temp values for period in between back one day
#iterate by metric
for(m in c("airtemp_max", "airtemp_min")){
  for(d in seq.Date(start_date, end_date, 1)){
    working_dat$main[working_dat$date == d & working_dat$met == m] <- working_dat$main[working_dat$date == (d+1) & working_dat$met == m]
    working_dat$qa_flag[working_dat$date == d & working_dat$met == m] <- "temp shifted backward 1 day (corrected)"
    # NA last day in sequence
    if(d == end_date){
      working_dat$main[working_dat$date == (d) & working_dat$met == m] <- NA
    }
  }
}

# plot again to verify shift correctly applied and high value dates in tmax to NA (where sensor failed in early may)
ggplot(data = subset(working_dat, yr == 1987 & mon %in% 2:5), aes(date, main)) +
  geom_vline(aes(xintercept = start_date), col = "chocolate2", lwd = 2, alpha = 0.5) +
  geom_vline(aes(xintercept = end_date), col = "chocolate2", lwd = 2, alpha = 0.5) +
  geom_line() +
  geom_point() +
  geom_line(aes(date,comp1), col = "steelblue1", alpha = 0.5) +
  geom_point(aes(date,comp1), col = "steelblue1", alpha = 0.5) +
  geom_line(aes(date,comp2), col = "orchid", alpha = 0.5) +
  geom_point(aes(date,comp2), col = "orchid", alpha = 0.5) +
  geom_line(aes(date,comp3), col = "forestgreen", alpha = 0.5) +
  geom_point(aes(date,comp3), col = "forestgreen", alpha = 0.5) +
  geom_line(aes(date,c1_temp), col = "goldenrod", alpha = 0.5) +
  geom_point(aes(date,c1_temp), col = "goldenrod", alpha = 0.5) +
  scale_x_date(date_breaks = "1 month") +
  facet_grid(met~.) # shift adjustment looks good
# ID tmax breaks first week in may (day before and day after NA)
subset(working_dat, yr == 1987 & mon == 5 & met == "airtemp_max")
NA_date <- min(with(working_dat, date[ yr == 1987 & mon == 5 & met == "airtemp_max" & is.na(main)]))
working_dat$main[working_dat$date %in% c((NA_date-1), (NA_date+1)) & working_dat$met == "airtemp_max"] <- NA
working_dat$qa_flag[working_dat$date %in% c((NA_date-1), (NA_date+1)) & working_dat$met == "airtemp_max"] <- "high value (sensor break)"
# run plot above to check (looks good)


# move on to monthly mins...
# monthly min temps of tmin and tmax
check_monthly_min <- check_extreme(working_dat, groupvars = c("logger", "met", "mon"), min)
# run through visual qa function
qa_mon_min1 <- visual_qa(working_dat, check_monthly_min, sorttime = "mon")

# visualize logger monthly minimums, by logger and metric
## min of airtemp_max
plot_grid(plotlist = qa_mon_min1[grep("cr21x.*airtemp_max", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr23x.*airtemp_max", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr1000.*airtemp_max", qa_mon_min1)]) #okay (again, 2014-2016 d1 chart looks consistently colder than d1 logger, in way I don't see in other years)
## min of airtemp_min
plot_grid(plotlist = qa_mon_min1[grep("cr21x.*airtemp_min", qa_mon_min1)]) # not okay, may-sep tmins look like sensor fails (in aug 1989, 2 days not just one circled as tmin)
plot_grid(plotlist = qa_mon_min1[grep("cr23x.*airtemp_min", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr1000.*airtemp_min", qa_mon_min1)]) #okay

flag_monmin1 <- subset(check_monthly_min, mon %in% 5:9 & logger == "cr21x" & met == "airtemp_min") %>%
  data.frame() %>%
  # append 2nd day in aug that looks like a sensor fail
  rbind(subset(working_dat, met == "airtemp_min" & date == "1989-08-09"))

# > general observation: seems like deviances from chart data occurs more in tmax vals than tmin vals (even max of tmin not so bad, but lots of problems in max of tmax)
# flag and remove noted monthly min values from cr21x and cr23x

# note the diffs are flagged for d1 in sdl logger these observations (sdl chart data missing, so that's why didn't pass diff_daily screen):
dplyr::select(flag_monmin1, date, logger, met, flag_diff1:ncol(flag_monmin1))
# flag and remove from working copy
working_dat <- flag_temp(flag_monmin1)


# run through monthly max and min one more time for logger period that were problematic
# monthly max temps of tmin and tmax
check_monthly_max2 <- check_extreme(working_dat, groupvars = c("logger", "met", "mon"))
# run through visual qa function
qa_mon_max2 <- visual_qa(working_dat, check_monthly_max2, sorttime = "mon", add_fourth = "c1_temp")
## max of airtemp_max
plot_grid(plotlist = qa_mon_max2[grep("cr21x.*airtemp_max", qa_mon_max2)]) # looks good
plot_grid(plotlist = qa_mon_max2[grep("cr23x.*airtemp_max", qa_mon_max2)]) # looks good

# monthly min temps of tmin and tmax
check_monthly_min2 <- check_extreme(working_dat, groupvars = c("logger", "met", "mon"), min)
# run through visual qa function
qa_mon_min2 <- visual_qa(working_dat, check_monthly_min2, sorttime = "mon")
## min of airtemp_min
plot_grid(plotlist = qa_mon_min2[grep("cr21x.*airtemp_min", qa_mon_min2)]) # flag july and aug -- shouldn't be that cold in summer, esp in context of logger relationship to stations on other dates
# apply flags to jul + aug
flag_minmon2 <- subset(check_monthly_min2, met == "airtemp_min" & logger == "cr21x" & mon %in% 7:8) # flagged for deviance in both stations available so can apply flag label "comparative deviance"
working_dat <- flag_temp(flag_minmon2)

# check once more
check_monthly_min3 <- check_extreme(working_dat, groupvars = c("logger", "met", "mon"), min)
# run through visual qa function
qa_mon_min3 <- visual_qa(working_dat, check_monthly_min3, sorttime = "mon")
## min of airtemp_min
plot_grid(plotlist = qa_mon_min3[grep("cr21x.*airtemp_min", qa_mon_min3)]) # flag july
# apply flags to jul
flag_minmon3 <- subset(check_monthly_min3, met == "airtemp_min" & logger == "cr21x" & mon == 7) # flagged for deviance in both stations available so can apply flag label "comparative deviance"
working_dat <- flag_temp(flag_minmon3)

# clean up environment and move on..
rm(flag_mon_max1, check_monthly_max1, check_monthly_max2,
   check_monthly_min, check_monthly_min2, check_monthly_min3,
   qa_mon_max1, qa_mon_max2, qa_mon_min1, qa_mon_min2, qa_mon_min3,
   flag_monmin1, flag_maxmon1, flag_minmon2, flag_minmon3, mar87_shift)




# -- QA LAG SPIKES/FLATLINES WITHIN LOGGER DATASET ----
# look for consecutive day temp spikes/declines that exceed 40C within the same metric
# also look for flatlining in temperature (jen morse said try 5-consec days)

# save copy of working dat in case mess up (so don't need to re-run from top)
working_dat_copy <- as.data.frame(working_dat)

## add lag temp
working_dat <- deltamet(working_dat, groupvars = c("logger", "met"))
# check distribution of absolute difference with current day's temp and yesterday's temp
boxplot(working_dat$lag1_diffmain)  
sapply(split(working_dat$lag1_diffmain, working_dat$met), function(x) tail(sort(x))) # max swings are mostly 19 degrees, except for one 22C swing in tmin
sapply(split(working_dat$lag1_diffmain, working_dat$logger), function(x) tail(sort(x))) # cr21x just looks like trouble compared to the others (cr23 and cr1000 swing similarly)

# can visualize swings to be sure
swing_check1 <- subset(working_dat, lag1_diffmain >= 15) 
qa_swings1 <- visual_qa(working_dat, swing_check1, add_fourth = "c1_temp")
plot_grid(plotlist = qa_swings1[grep("cr21x.*airtemp_min", qa_swings1)]) # 22C tmin swing is 1990-12-31 (sub 40C value).. is lower than d1 chat data point tho..
plot_grid(plotlist = qa_swings1[grep("cr23", qa_swings1)]) # flag 2002-02-01 and 2007-01-08, but 
plot_grid(plotlist = qa_swings1[grep("cr1000", qa_swings1)]) #fine

# does 1990-12-30 fail diff daily for d1 and sdl logger?
subset(working_dat, date == "1990-12-30" & met == "airtemp_min") %>% dplyr::select(main, flag_diff1:ncol(.))
# > decide to warn sub 40C value on 1990-12-30
working_dat <- flag_temp(subset(working_dat, date == "1990-12-30" & met == "airtemp_min"), error = "warning: comparative deviance, low value")
# flag swings in cr23
flag_swings1 <- subset(swing_check1, logger == "cr23x" & as.character(date) %in% c("2002-02-01", "2007-01-08"))
# > note these dates also are more than 3SD outside diff_daily for all stations where data available
working_dat <- flag_temp(flag_swings1, error = "comparative deviance, daily rate change spike")


# look for 4+ consecutive days of 0 change
count0 <- rle(working_dat$lag1_diffmain)
consec0 <- count0$lengths[count0$values == 0]
consec0[!is.na(consec0)] # 0 change in consec days only ever occurs for runs of 1 day, which is fine

# to be sure, compare lead delta
working_dat %>%
  arrange(met, date) %>%
  group_by(logger, met) %>%
  mutate(lead1_crtemp = lead(main)) %>%
  ungroup() %>%
  mutate(lead_diffcr = abs(main - lead1_crtemp)) %>%
  subset(lead_diffcr == 0 & lag1_diffmain == 0) %>%
  nrow() # nada, all clear

# clean up environment
rm(count0, consec0, swing_check1, qa_swings1, flag_swings1)



# -- QA DAY-TO-DAY DELTA DEVIANCE -----
# diff current from lag temp in sdl chart, d1 and c1, then compare daily deltas with logger daily deltas
# pull out observations where delta deviates more than 3sd of logger-other source diff on day-to-day fluxes
working_dat_copy <- as.data.frame(working_dat)

working_dat <- deltadev(working_dat, groupvars = c("met", "logger"))
check_deltadiff1 <- subset(working_dat, flag_deltadiff1 == TRUE & flag_deltadiff2 == T & flag_deltadiff3 == T)
#View(subset(working_dat, flag_deltadiff_sdl == TRUE))
qa_lagdiffs1 <- visual_qa(working_dat, check_deltadiff1)
plot_grid(plotlist = qa_lagdiffs1[grep("airtemp_min", qa_lagdiffs1)]) #tmin okay.. some drops but leave be until review better info
plot_grid(plotlist = qa_lagdiffs1[grep("cr21x.*airtemp_max", qa_lagdiffs1)]) # do 1993, 1997 and 1998 dates have diff_daily flags on day prior to flagged days? 
plot_grid(plotlist = qa_lagdiffs1[grep("cr23x.*airtemp_max", qa_lagdiffs1)]) # alright enough
# > there are no problems with cr1000 logger
# differences not compelling enough with d1 logger data added to flag anything.. but perhaps something to come back to
# for the most part logger data tracks with chart at sdl and d1, but there are some tmax spikes in the sdl logger-- are these real spikes or sensor issues?

subset(working_dat, date %in% (check_deltadiff1$date[check_deltadiff1$logger == "cr21x" & check_deltadiff1$yr %in% c(1993,1997,1998)]-1)) %>%
  dplyr::select(met, date, main, flag_diff1:ncol(.))
# not sure, just run another diff_daily check 

# clean up environment


# -- RUN THROUGH DIFF DAILY ONE MORE TIME AND REVIEW QA'D DATA  ----
# final check
working_dat <- diff_daily(working_dat, rename = F)
par(mfrow=c(1,3))
boxplot(working_dat$deviance_1, main = "daily dev. from d1 chart")
boxplot(working_dat$deviance_2, main = "daily dev. from sdl logger")
boxplot(working_dat$deviance_3, main = "daily dev. from sdl chart")
par(mfrow=c(1,1))
# look at more extreme deviance in both sdl and d1
check_diffdaily <- subset(working_dat, deviance_1 >= 8 | deviance_2 >= 8 | deviance_3 >= 8) 
qa_diffdaily <- visual_qa(working_dat, check_diffdaily, add_fourth = "c1_temp")
plot_grid(plotlist = qa_diffdaily[grepl("cr21.*airtemp_min", qa_diffdaily)]) # not compelling enough to flag
plot_grid(plotlist = qa_diffdaily[grepl("cr21.*airtemp_max", qa_diffdaily)]) #ok
plot_grid(plotlist = qa_diffdaily[grepl("cr23", qa_diffdaily)]) # ok (see spikes in d1 chart temp)
plot_grid(plotlist = qa_diffdaily[grepl("cr10", qa_diffdaily)]) # see flatline in april 2017 data for d1 chart

# look at regular test one last time
check_diffdaily <- subset(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T) 
qa_diffdaily <- visual_qa(working_dat, check_diffdaily, add_fourth = "c1_temp")
plot_grid(plotlist = qa_diffdaily)
# > flag all plus tmax 1999-05-26 and tmin 1998-04-06
flag_diffdaily1 <- check_diffdaily %>%
  as.data.frame() %>%
  rbind(subset(working_dat, as.character(date) == c("1999-05-26") & met == "airtemp_max")) %>%
  rbind(subset(working_dat, as.character(date) == c("1998-04-06") & met == "airtemp_min"))

working_dat <- flag_temp(flag_diffdaily1)
# clean up environment
rm(check_diffdaily, qa_diffdaily, flag_diffdaily1)



# -- REVIEW AND CLEAN UP WORKING DAT -----
# plot qa'd data for review
# logger values with d1 chart values plotted behind in grey as comparison for ranges
ggplot(working_dat) + 
  geom_point(aes(date, comp1), alpha = 0.6, col = "grey80") +
  geom_point(aes(date, main, col = logger), alpha = 0.5) +
  #geom_smooth(aes(date, cr_temp, group = logger), method = "glm", col = "black") +
  scale_color_viridis_d() +
  facet_wrap(~met)

# how many points flagged?
with(working_dat, sapply(split(qa_flag, met), function(x) summary(!is.na(x)))) #207 points in tmax, 150 in tmin flagged..
with(working_dat, sapply(split(qa_flag, logger), function(x) summary(!is.na(x)))) # by far earliest logger (cr21x) most problematic, only 32 in cr23x and 4 points in cr1000 flagged

# what is relationship between loggers and d1 chart?
dplyr::select(working_dat, met, date, doy, yr, logger, main, comp1) %>%
  mutate(logger = factor(logger, levels = c("cr21x", "cr23x", "cr1000"))) %>%
  ggplot(aes(main, comp1)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0), col = "red") +
  facet_grid(met ~ logger, scales = "free") # is d1 chart drifting over time? relationship changes..
# > some funkiness in tmin relationship for cr1000 and d1 chart (grouping? maybe when d1 chart values drop in 2014-2016)



# -- COMPILE AND WRITE OUT FLAGGED/QA'D SDL CR DATASET -----
# want to write out old data with qa'd data for comparison
# maybe also write out final working_dat for documentation

d1crall_old_new <- left_join(d1crall_long, working_dat)

# plot what was corrected/modified using visual_qa -- for cr1000 only
qa_results <- subset(d1crall_old_new, !is.na(qa_flag) & logger != "cr21x")
qa_show <- visual_qa(d1crall_old_new, qa_results, add_fourth = "d1cr_temp")
plot_grid(plotlist = qa_show[grepl("cr23", qa_show)])
plot_grid(plotlist = qa_show[grepl("cr10", qa_show)])


# moving on.. clean up old_new and write out along with final working_dat data frame for reference/documentation
d1crall_old_new <- dplyr::select(d1crall_old_new,
                               LTER_site:qa_flag) %>%
  rename(qa_temp = main)

# write out qa'd dataset
write_csv(d1crall_old_new, "climate_d1_c1/output_data/prep_data/qa_d1cr_temp.csv")
#if want to write out working dat with all flag columns, uncomment this next line (warning: large file)
#write_csv(working_dat, "climate_d1_c1/output_data/prep_data/qa_sdlcr_temp_workingreference.csv")
