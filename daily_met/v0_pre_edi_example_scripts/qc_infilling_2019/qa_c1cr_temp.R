# qa c1 logger temp (for c1 chart infilling)


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
# black = c1 logger, purple = c1 chart, blue = niwot snotel, green = sdl chart, yellow = sdl logger

# jen morse writes campbell 207 temp/rh probe was used at saddle (presumably d1 + c1 too) until dec 1999, when switched to cs500
# cs 500 switched to Vaisala HMP 155a in 2017
# campbell scientific website does not list range detection limits for 207 probe (bc retired?) but writes the 207 was camporable to 107 and 108 temp probes
# detection limit for 107 is -35C to +50C, detection limit for 108 is -5C to +95C
# operating range for cs 500 probe = -40° to +60°C (https://www.campbellsci.com/cs500-l)

# note about script warnings.. 
# can ignore warnings throughout. will either issue warnings when missing data plotted (e.g. ggplot tells you how many pts removed due to missing data)
# warnings also issued over an uninitialised temp column. junky message from using function? (likely due to object being a tibble or some other hadley object)



# -- SETUP ------
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN", "NP")

# set pathway to climate output_data folder (qa'd data written out to output_data/prep_data)
datpath <- "climate_d1_c1/output_data/"

# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")
# qa functions
source("extended_summer/analysis/scripts/qa_functions.R")


# -- GET DATA -----
# c1 cr loggers  -- most current dataset not edi
## c1 cr21x, 1986-2000
c1cr21 <- getTabular(400) %>% data.frame()
## c1 cr23x and cr1000, 2000 - ongoing
#c1cr <- getTabular(401) %>% data.frame()  <-- only goes through 12/31/2016
c1logs <- read_csv("~/Documents/nwt_lter/unpub_data/c-1cr23x-cr1000.daily.ml.data.csv",
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
# snotel data
snotel <- read_csv(paste0(datpath,"prep_data/nwt_snotel_temp.csv"),
                   na = na_vals, trim_ws = T) %>% data.frame()
# c1 chart
c1 <- getTabular(411) %>% data.frame()
# keith jennings et al infilled logger dataset
kj <- getTabular(168) %>% data.frame()

  

# -- REVIEW DATA -----
# review how dats read in
glimpse(c1cr21) # no flag cols, date is date class
glimpse(c1logs) # flag cols present, date is date class
glimpse(sdllog_qa) # date = date
glimpse(sdl_qa) # flags cols present
glimpse(snotel) # no flags
glimpse(c1) # flag cols present
glimpse(kj)

# clean up c1 cr23x and cr1000 data
# drop cols not needed in cr logger dataset (i.e. only need airtemp cols up thru avg airtemp, flag cols not useful since only n or NA)
c1logs <- c1logs[,1:12] # as of 2019-07-13, there isn't any hmp data to compare with temp so can drop those airtemp cols
# unique values of flags? and frequency of their occurrence?
sapply(c1logs[grepl("flag", colnames(c1logs))], function(x) summary(as.factor(x))) # mostly n's, correspond to no flag -- not useful -- some "m" (missing), and 1 q (i think means questionable?, my flagging scripts will catch if it is)

# clean up 21x data 
# drop non airtemp cols in cr21x
c1cr21 <- c1cr21 %>%
  # add logger, LTER_site, and local_site cols so simlar to other datasets
  mutate(LTER_site = "NWT",
         local_site = "c1", 
         flag_airtemp_max = NA,
         flag_airtemp_min = NA,
         flag_airtemp_avg = NA) %>%
  # select same cols as in cr23x and cr1000 datasets + time of temp cols
  dplyr::select(c(colnames(c1logs)))

#stack data
c1crall <- rbind(c1cr21, c1logs) %>% arrange(date)
str(c1crall)

# review flags in reference datasets 
# sdl
sapply(sdl_qa[grepl("flag", colnames(sdl_qa))], function(x) summary(as.factor(x))) # 2 types of flags, type 1 used most often
# > saddle chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)
# c1
sapply(c1[grepl("flag", colnames(c1))], function(x) summary(as.factor(x))) # 2 types of flags (same as sdl), mostly infilled via type 1
# > c1 chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)
# >> conclusion: keep flag cols in chart temp datasets, useful info


# tidy chart temp datatsets (qa'd sdl chart and sdl logger already long-form)
c1crall_long <- tidytemp(c1crall, datasource = "c1cr", dropcol = "airtemp_avg")
c1_long <- tidytemp(c1, datasource = "c1", dropcol = "airtemp_avg")

# prep names in ctw qa'd/prepped datasets to join with c1 cr logger datasets (i.e. no redundant colnames across datasets)
names(sdl_qa)
names(sdllog_qa)

colnames(sdl_qa)[11:12] <- c("sdl_qaflag", "sdl_daysflat")
colnames(sdllog_qa)[9:ncol(sdllog_qa)]<- c("sdlcr_temp", "sdlcr_qatemp", "sdlcr_qaflag")
snotel <- rename(snotel, sno_temp = tempC)


# -- SCREEN OBVIOUS OUTLIERS IN REFERENCE DATASETS -----
# screen any obvious outliers in chart (non ctw qa'd) datasets (i.e. don't use these values to compare with d1 logger data points)
## snotel
with(snotel, sapply(split(sno_temp, met), summary)) # -47 is about -55F and 46 is about 114F..
with(snotel, lapply(split(sno_temp[met == "airtemp_max"], mon[met=="airtemp_max"]), summary))
with(snotel, sapply(split(sno_temp, met), function(x) tail(sort(x), n = 20))) # +110 seems warm for C1, but there are may vals over 40 so will leave be 
with(snotel, sapply(split(sno_temp, met), function(x) head(sort(x), n = 20))) # -47 seems like quite the jump
# plot snotel minT to look at extreme value relative to days before and after
tmin_date <- na.exclude(snotel$date[snotel$sno_temp < -40])
ggplot(subset(snotel, date %in% seq.Date(tmin_date - 10, tmin_date + 10, 1)), aes(date, sno_temp, col = met, group = met)) +
  geom_line() +
  geom_point() # seems weird to have a tmax spike and tmin spike (in opposite direction) in same day..
# > NA tmin value so excluded from qa comparisons
snotel$sno_temp[snotel$met == "airtemp_min" & snotel$date == tmin_date] <- NA

## c1 chart
with(c1_long, sapply(split(c1_temp, met), summary))
with(c1_long, sapply(split(c1_temp[met == "airtemp_max"], mon[met=="airtemp_max"]), summary))
with(c1_long, sapply(split(c1_temp, met), function(x) tail(sort(x))))
with(c1_long, sapply(split(c1_temp, met), function(x) head(sort(x)))) # all okay              

## jennings et al. hourly
with(kj, sapply(split(airtemp_avg, local_site), summary)) #min/max at c1 much more moderate compared to chart
with(kj, lapply(split(airtemp_avg, month(date)), summary)) # seems okay
# > tmin and tmax using jennings data is going to be more moderate than daily logger data.. so maybe press fwd with QAing c1 logger and decide later how to merge with keith's data             

# min/max jennings data as extra "station" for visual qa
kjc1 <- subset(kj, local_site == "c1") %>%
  rename(doy = jday,
         yr = year) %>%
  mutate(mon = month(date)) %>%
  group_by(LTER_site, local_site, date, yr, mon, doy) %>%
  summarise(airtemp_min = min(airtemp_avg),
            airtemp_max = max(airtemp_avg)) %>%
  gather(met, kj_temp, airtemp_min, airtemp_max)


# join d1 logger and all comparative temp datasets
c1crall_long_master <- mutate(c1crall_long, qa_flag = NA) %>% # add empty qa flag col first
  left_join(c1_long) %>%
  left_join(dplyr::select(snotel, -local_site)) %>%
  # join qa'd sdl dataset, dropping raw temp col and days flat column (not needed)
  left_join(dplyr::select(sdl_qa, -c(local_site, sdl_temp, sdl_flag, sdl_daysflat))) %>%
  # remove raw sdl logger temp
  left_join(dplyr::select(sdllog_qa, -c(local_site, logger, sdlcr_temp))) %>%
  # join jennings infilled daily summarized data
  left_join(kjc1) %>%
  ungroup() %>%
  distinct() %>%
  data.frame()




# -- QA SENSOR FAILS (OBVIOUS OUTLIERS) -----
# create working copy
working_dat <- c1crall_long_master %>%
  # exclude flatline sdl chart vals from consideration
  mutate(sdl_qatemp = ifelse(grepl("flat", sdl_qaflag), NA, sdl_qatemp))

# look at tails for any obvious bad values
## logger temp, split by logger
with(working_dat, lapply(split(c1cr_temp, paste(met, logger)), function(x) tail(sort(x), n = 20))) # looks okay
# > looks like some values in Farenheit?
with(working_dat, lapply(split(c1cr_temp[met == "airtemp_min"], logger[met=="airtemp_min"]), function(x) head(sort(x), n = 25))) # -42 seems very low compared to other values..

# look at tmin in context of other tmin values
tmin_date <- na.exclude(working_dat$date[working_dat$c1cr_temp < -40])
ggplot(subset(working_dat, date %in% seq.Date(tmin_date - 10, tmin_date + 10, 1)), aes(date, c1cr_temp, col = met, group = met)) +
  geom_line() +
  geom_point() + # clealy a sensor fail.. leave tmax as it
  # add c1 chart
  geom_point(aes(date, c1_temp), shape = 2) +
  # add kj temps
  geom_line(aes(date, kj_temp, group = met), col = "grey50")
# > conclusions..
# NA both values to be conservative in infilling
working_dat$c1cr_temp[working_dat$date == tmin_date] <- NA
working_dat$qa_flag[working_dat$date == tmin_date] <- "sensor fail"

# check ranges within CS reported detection limit range (-40 to +60C)
range(working_dat$c1cr_temp, na.rm = T) # okay



# -- FLAG DAILY DEPARTURES FROM COMPARATIVE DATASETS -----
# round 1 diff daily and flagging
working_dat <- diff_daily(working_dat, main = "c1cr_temp", comp1 = "c1_temp", comp2 = "sno_temp", comp3 = "sdl_qatemp", 
                          groupvars = c("logger", "met", "mon"))  
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff1 <- filter(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff1 <- visual_qa(working_dat, check_daily_diff1, add_fourth = "sdlcr_qatemp") # add c1 chart as fourth comparison
# tmax -- only cr21x and cr23x have flags (cr1000 vals okay)
plot_grid(plotlist = qa_daily_diff1[grep("cr21.*airtemp_max", qa_daily_diff1)]) #flag all + Aug 4 1998
plot_grid(plotlist = qa_daily_diff1[grep("cr23.*airtemp_max", qa_daily_diff1)]) # flag all + 2002-01-10 + 2002-09-18

# tmin -- cr21x is only logger with tmin flags
plot_grid(plotlist = qa_daily_diff1[grep("airtemp_min", qa_daily_diff1)]) # flag all plus 1995-05-01

# round 1:
flag_dailydiff1 <- data.frame(check_daily_diff1) %>%
  ungroup() %>%
  # append tmax dates: 1998-08-04, 2002-01-10, 2002-09-18
  rbind(subset(working_dat, as.character(date) %in% c("1998-08-04", "2002-01-10", "2002-09-18") & met == "airtemp_max")) %>%
  # append temin date: 1995-01-01
  rbind(subset(working_dat, date == as.Date("1995-05-01") & met == "airtemp_min"))

# flag and remove values in working dataset
working_dat <- flag_temp(flag_dailydiff1, error = "comparative deviance") %>% data.frame()

# round 2 daily diff (recalc mean and deviance with bad deviance values from round 1 removed)
working_dat <- diff_daily(working_dat, rename = F, groupvars = c("logger", "met", "mon"))
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff2 <- filter(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff2 <- visual_qa(working_dat, check_daily_diff2, add_fourth = "sdlcr_qatemp")
# only cr23x has flags
plot_grid(plotlist = qa_daily_diff2) # warn 1997-06-13 since sdl logger also warms, and warn 2001-04-14 since keith's values close and perhaps jump of 4 degrees from sdl chart not too crazy
# try plotting with kj temps to be sure
qa_daily_diff2kj <-  visual_qa(working_dat, check_daily_diff2, add_fourth = "kj_temp")
plot_grid(plotlist = qa_daily_diff2kj) # keith's data look off by one day compared to other data sources... how does logger itself summarize daily data? (kj data not always off by one day)
# specify vals to flag and warn
flag_dailydiff2 <- subset(check_daily_diff2, !as.character(date) %in% c("1997-06-13", "2001-04-14"))
warn_dailydiff2 <- subset(check_daily_diff2, as.character(date) %in% c("1997-06-13", "2001-04-14"))
# flag and warn
working_dat <- flag_temp(flag_dailydiff2, "comparative deviance") %>% data.frame()
working_dat <- flag_temp(warn_dailydiff2, "warning: comparative deviance") %>% data.frame()


# round 3: check extreme sdl deviance values
# what the distribution of deviances?
par(mfrow = c(1,3))
boxplot(working_dat$deviance_1, main = "c1 dev.")
boxplot(working_dat$deviance_2, main = "snotel dev.")
boxplot(working_dat$deviance_3, main = "sdl dev.")
par(mfrow = c(1,1))

working_dat <- diff_daily(working_dat, rename = F, groupvars = c("logger", "met", "mon"))
# check logger temps that exceed daily deviance more than 5sds for sdl_chart and d1_chart
check_daily_diff3 <- filter(working_dat, round(deviance_1) >= 3 & round(deviance_2) >= 6 & round(deviance_3) >= 6)
# run through visual qa
qa_daily_diff3 <- visual_qa(working_dat, check_daily_diff3, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_daily_diff3) # given most in warmer months, and expect tmax to be warmer on logger, going to leave alone
# plot with keith's data to be sure
qa_daily_diff3kj <- visual_qa(working_dat, check_daily_diff3, add_fourth = "kj_temp")
plot_grid(plotlist = qa_daily_diff3kj) # keith's data spike in same places (albeit off by a day), so leave be


# move on to checking grand and monthly tmax and tmin vals..
#clean up environment
rm(check_daily_diff1, flag_dailydiff1,
   check_daily_diff2, flag_dailydiff2, warn_dailydiff2,
   check_daily_diff3, 
   qa_daily_diff1, qa_daily_diff2, qa_daily_diff2kj, qa_daily_diff3, qa_daily_diff3kj)



# -- QA EXTREMES (TMIN/TMAX) -----
## IMPORTANTE!!: 
## logger lifecycles: cr21x = 1980s-2000; cr23x = 2000 - 2012; cr1000 = dec 2012 - ongoing
## panel arrangement: cr21x = left panel, cr23x = middle, cr1000 = right panel

# visual qa grand max and min
# max temps by logger
check_max1 <- check_extreme(working_dat, groupvars = c("logger", "met"))
qa_max1 <- visual_qa(working_dat, check_max1, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_max1)
# plot with keth's data
plot_grid(plotlist = visual_qa(working_dat, check_max1, add_fourth = "kj_temp")) # flag 2006-06-08 since don't see similar spike in keith's data

# >round 1:  flag 1997 and 2018 tmax, all else fine
flag_max1 <- filter(check_max1, met == "airtemp_max" & yr == 2006) %>% as.data.frame()
working_dat <- flag_temp(flag_max1, error = "high value")
# run through grand max once more
check_max2 <- check_extreme(working_dat, groupvars = c("logger", "met")) %>% as.data.frame()
qa_max2 <- visual_qa(working_dat, check_max2, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = visual_qa(working_dat, check_max2, add_fourth = "kj_temp"))
# plot with keith's data
plot_grid(plotlist = qa_max2) # flag 2005 tmax, warn tmin 1988 (probably is an error, but since sdl chart increased, warn only)

# flag/warn and move on
working_dat <- flag_temp(check_max2[check_max2$date == "2005-09-19",], error = "high value, comparative deviance")
working_dat <- flag_temp(check_max2[check_max2$date == "1988-07-12",], error = "warning: high value, comparative deviance with two stations")

# min temps by logger
check_min1 <- check_extreme(working_dat, groupvars = c("logger", "met"), min)
qa_min1 <- visual_qa(working_dat, check_min1, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_min1) # seems fine

# move on to monthlies...
#clean up env
rm(check_max1, check_max2, qa_max1, qa_max2,
   check_min1, qa_min1, flag_max1)



# -- QA MONTHLY MIN/MAX -----
# monthly max temps of tmin and tmax
check_monthly_max1 <- check_extreme(working_dat, groupvars = c("logger", "met", "mon"))
# run through visual qa function
qa_mon_max1 <- visual_qa(working_dat, check_monthly_max1, sorttime = "mon", add_fourth = "sdlcr_qatemp")

# visualize logger monthly maximums, by logger and metric
## max of airtemp_max
plot_grid(plotlist = qa_mon_max1[grep("cr21x.*airtemp_max", qa_mon_max1)]) # looks good
plot_grid(plotlist = qa_mon_max1[grep("cr23x.*airtemp_max", qa_mon_max1)]) # definitely values to flag, it seems like some of these high values should fail the daily diff screen... maybe try again using 3SD as cutoff instead of 4
plot_grid(plotlist = qa_mon_max1[grep("cr1000.*airtemp_max", qa_mon_max1)]) # all okay
## max of airtemp_min
plot_grid(plotlist = qa_mon_max1[grep("cr21x.*airtemp_min", qa_mon_max1)]) # nov 1999 higher than other c1 stations, but sdl stations warmed that day and c1 temp slightly above 0 so leaving be
plot_grid(plotlist = qa_mon_max1[grep("cr23x.*airtemp_min", qa_mon_max1)]) # all okay
plot_grid(plotlist = qa_mon_max1[grep("cr1000.*airtemp_min", qa_mon_max1)]) # all okay

# try diff daily but screening values outside 3sd to see if high cr23x values show up there
working_dat <- diff_daily(working_dat, rename = F, groupvars = c("logger", "met", "mon"))
check_daily_diff <- filter(working_dat, deviance_1 >= 3 & deviance_2 >= 3.5 & deviance_3 >= 3.5)
qa_daily_diff <- visual_qa(working_dat, check_daily_diff, add_fourth = "sdlcr_qatemp") # add c1 chart as fourth comparison
# tmax -- only cr21x and cr23x have flags (cr1000 vals okay)
# split up plotting so not too many panels on one (check list length before plotting)
plot_grid(plotlist = qa_daily_diff[grep("cr21.*airtemp_max", qa_daily_diff)][1:19]) 
plot_grid(plotlist = qa_daily_diff[grep("cr21.*airtemp_max", qa_daily_diff)][20:38]) # both cr21x plots look okay
quartz()
plot_grid(plotlist = qa_daily_diff[grep("cr23.*airtemp_max", qa_daily_diff)])
# compare again to tmax monthlies
quartz()
plot_grid(plotlist = qa_mon_max1[grep("cr23x.*airtemp_max", qa_mon_max1)]) # diff daily covers some high values but not all

# flag diff daily before max monthly
# > not 2002-05-24, 2006-08-12, 2007-02-02, 2007-04-12 or 2013-04-03
flag_diffdaily <- subset(check_daily_diff, logger == "cr23x" & !(as.character(date) %in% c("2002-05-24", "2006-08-12", "2007-02-02", "2007-04-12", "2013-04-03")))
working_dat <- flag_temp(flag_diffdaily)
# run monthly max again but just focus on cr23x since that was the only one that was problematic above
# monthly max temps of tmin and tmax
check_monthly_max1 <- check_extreme(working_dat, groupvars = c("logger", "met", "mon")) %>% 
  subset(logger == "cr23x" & met == "airtemp_max")
# run through visual qa function
qa_mon_max1 <- visual_qa(working_dat, check_monthly_max1, sorttime = "mon", add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_mon_max1)
# flag max monthlies
flag_maxmon1 <- subset(check_monthly_max1, mon %in% c(1:4,10:12)) %>%
  #remove any that clear all deviance flags
  subset(!(flag_diff1 == F & flag_diff2 == F & flag_diff3 == F))
working_dat <- flag_temp(flag_maxmon1, error = "high value, comparative deviance") # looking at flags, they are all outside c1 chart, mostly +3SD outside normal variation range with others

# check max monthlies for cr23x again
check_monthly_max2 <- check_extreme(working_dat, groupvars = c("logger", "met", "mon")) %>% 
  subset(logger == "cr23x" & met == "airtemp_max")
qa_mon_max2 <- visual_qa(working_dat, check_monthly_max2, sorttime = "mon", add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_mon_max2) # maybe just warn jan and feb.. that's +50F.. could happen maybe, but other values in time ranges track closer to c1 chart and snotel
warn_monmax2 <- subset(check_monthly_max2, mon < 3)
working_dat <- flag_temp(warn_monmax2, error = "warnings: high value")


# move on to monthly mins...
# monthly min temps of tmin and tmax
check_monthly_min <- check_extreme(working_dat, groupvars = c("logger", "met", "mon"), min)
# run through visual qa function
qa_mon_min1 <- visual_qa(working_dat, check_monthly_min, sorttime = "mon", add_fourth = "sdlcr_qatemp")

# visualize logger monthly minimums, by logger and metric
## min of airtemp_max
plot_grid(plotlist = qa_mon_min1[grep("cr21x.*airtemp_max", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr23x.*airtemp_max", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr1000.*airtemp_max", qa_mon_min1)]) # november min tmax pretty low
# look at flagging for cr1000 nov min tmax value
check_monthly_min[check_monthly_min$date == "2014-11-12", c(4,8,9,28:ncol(check_monthly_min))] # flagged in 2/3, see how tmax on that date plots out
# > -20 is -4, which seems possible in november at c1

## min of airtemp_min
plot_grid(plotlist = qa_mon_min1[grep("cr21x.*airtemp_min", qa_mon_min1)]) # june looks a bit low
# plot cr21x wth keith's infilled
plot_grid(plotlist = visual_qa(working_dat, subset(check_monthly_min, met == "airtemp_min" & logger == "cr21x"), 
                               sorttime = "mon", add_fourth = "kj_temp")) # do flag cr21x june, july and 1993-07-03
plot_grid(plotlist = qa_mon_min1[grep("cr23x.*airtemp_min", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr1000.*airtemp_min", qa_mon_min1)]) #okay

# only flag min tmin in june, july and 1993-07-03 for cr21x
flag_monmin1 <- subset(check_monthly_min, mon %in% 6:7 & logger == "cr21x" & met == "airtemp_min") %>%
  data.frame() %>%
  # append 2nd day in aug that looks like a sensor fail
  rbind(subset(working_dat, met == "airtemp_min" & date == "1993-07-03"))

# flag and remove from working copy
working_dat <- flag_temp(flag_monmin1, error = "low value, comparative deviance")


# run through monthly min one more time for logger period that was problematic
# monthly min temps of tmin and tmax
check_monthly_min2 <- check_extreme(working_dat, groupvars = c("logger", "met", "mon"), min) %>%
  subset(logger == "cr21x" & met == "airtemp_min")
# run through visual qa function
qa_mon_min2 <- visual_qa(working_dat, check_monthly_min2, sorttime = "mon")
## min of airtemp_min
plot_grid(plotlist = qa_mon_min2) #okay



# clean up environment and move on..
rm(check_monthly_max1, check_monthly_max2,
   flag_maxmon1,  warn_monmax2, 
   check_monthly_min, check_monthly_min2, flag_monmin1,
   qa_mon_max1, qa_mon_max2, qa_mon_min1, qa_mon_min2, 
   qa_daily_diff, check_daily_diff, flag_diffdaily)




# -- QA LAG SPIKES/FLATLINES WITHIN LOGGER DATASET ----
# look for consecutive day temp spikes/declines that exceed 40C within the same metric
# also look for flatlining in temperature (jen morse said try 5-consec days)

# save copy of working dat in case mess up (so don't need to re-run from top)
working_dat_copy <- as.data.frame(working_dat)

## add lag temp
working_dat <- deltamet(working_dat, groupvars = c("logger", "met")) 
working_dat <- ungroup(working_dat) %>% data.frame()# to avoid annoying warning messages (hopefully)
# check distribution of absolute difference with current day's temp and yesterday's temp
dev.off() #reset plotting
boxplot(working_dat$lag1_diffmain)  
sapply(split(working_dat$lag1_diffmain, working_dat$met), function(x) tail(sort(x), n = 20)) # max swings around 21 degrees for both tmax/tmin
sapply(split(working_dat$lag1_diffmain, working_dat$logger), function(x) tail(sort(x), n = 20)) # swings similar by logger.. mabe slightly  higher for cr21x

# can visualize swings to be sure
swing_check1 <- subset(working_dat, lag1_diffmain >= 16) 
qa_swings1 <- visual_qa(working_dat, swing_check1, add_fourth = "sdlcr_qatemp")
#cr21x
plot_grid(plotlist = qa_swings1[grep("cr21x.*airtemp_min", qa_swings1)]) # okay
plot_grid(plotlist = qa_swings1[grep("cr21x.*airtemp_max", qa_swings1)]) # okay
plot_grid(plotlist = qa_swings1[grep("cr23x.*airtemp_min", qa_swings1)]) # okay
quartz()
plot_grid(plotlist = qa_swings1[grep("cr23x.*airtemp_max", qa_swings1)]) # needs flagging: 2003-12-29, 2006-01-21, 2009-01-14 + 2002-11-23, 2005-12-03
plot_grid(plotlist = qa_swings1[grep("cr1000", qa_swings1)]) # 2014-06-28 looks like sensor fail
# look at flagging for cr1000 in jun 2014 date
subset(swing_check1, met == "airtemp_max" & logger == "cr1000" & mon == 6) # over 3SD on all, can flag as comparative deviance
# values to flag in cr23x data
flag_swings1 <- subset(swing_check1, met == "airtemp_max" & as.character(date) %in% c("2003-12-29", "2006-01-21", "2009-01-14")) %>%
  # append preceding dates that need flagging
  rbind(subset(working_dat, met == "airtemp_max" & as.character(date) %in% c("2002-11-23", "2005-12-03")))
working_dat <- flag_temp(flag_swings1, error = "high value, comparative deviance")
# flag cr1000 tmin in 2014-06-28
working_dat <- flag_temp(subset(swing_check1, met == "airtemp_max" & logger == "cr1000" & mon == 6))



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
qa_lagdiffs1 <- visual_qa(working_dat, check_deltadiff1, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_lagdiffs1[grep("airtemp_min", qa_lagdiffs1)]) #tmin okay.. some drops but leave be until review better info
plot_grid(plotlist = qa_lagdiffs1[grep("airtemp_max", qa_lagdiffs1)]) #
# plot with keith's data
qa_lagdiffs1kj <- visual_qa(working_dat, check_deltadiff1, add_fourth = "kj_temp")
plot_grid(plotlist = qa_lagdiffs1kj[grep("airtemp_min", qa_lagdiffs1kj)]) 
# > flag 1993-01-18, 1993-02-04, 1993-03-24 tmin + append 1993-03-25, 1999-11-12 tmin + low value 1993-03-28
# > also try looking at deltadev flag 1 + flag 2 == 2 (prioritize importance on deviating from c1 chart and snotel, 3rd is saddle chart)
quartz()
plot_grid(plotlist = qa_lagdiffs1kj[grep("airtemp_max", qa_lagdiffs1kj)])
# to flag: 2002-12-24, 2009-01-14, 2010-02-10, 2013-08-22 + append 2002-10-24, 2002-11-03, 2002-12-24, 2006-01-28, 2002-02-17, 2011-10-17 (high values)

# select values to flag
flag_lagdiff <- subset(check_deltadiff1, (met == "airtemp_min" & as.character(date) %in% c("1993-01-18", "1993-02-04", "1993-03-24")) |
                                            (met == "airtemp_max" & as.character(date) %in% c("2002-12-24", "2009-01-14", "2010-02-10", "2013-08-22")))
flag_deviance <- subset(working_dat, (met == "airtemp_min" & as.character(date) %in% c("1993-03-25", "1999-11-12", "1993-03-28")) |
                                        (met == "airtemp_max" & as.character(date) %in% c("2002-10-24", "2002-11-03", "2002-12-24", "2006-01-28", "2009-02-17", "2011-10-17")))
# > all of the deviance values fail deviance checks with c1 chart and snotel, most fail deviance check with sdl but not all

# flag values
working_dat <- flag_temp(flag_lagdiff, error = "daily rate change deviance")
working_dat <- flag_temp(flag_deviance)

# re-run delta diff deviance selecting TRUE for flags 1 and 2 (c1 chart and snotel)
# need to re-lag vals
working_dat <- deltamet(working_dat, groupvars = c("logger", "met")) 
working_dat <- deltadev(working_dat, groupvars = c("met", "logger"))
check_deltadiff2 <- subset(working_dat, flag_deltadiff1 == TRUE & flag_deltadiff2 == T)
qa_lagdiffs2 <- visual_qa(working_dat, check_deltadiff2, add_fourth = "sdlcr_qatemp")
# visualize
## cr21x
plot_grid(plotlist = qa_lagdiffs2[grep("cr21.*airtemp_min", qa_lagdiffs2)]) #look at 1993-06-13 + 1999-11-08
# plot with keith's data
plot_grid(plotlist = visual_qa(working_dat, subset(check_deltadiff2, logger == "cr21x" & met == "airtemp_min"), add_fourth = "kj_temp"))
# > general comments: lots of issues in 1992 and 1993; flagging worst offenders with keith's data (9 values)
plot_grid(plotlist = qa_lagdiffs2[grep("cr21.*airtemp_max", qa_lagdiffs2)]) # leave be
## cr23x
plot_grid(plotlist = qa_lagdiffs2[grep("cr23", qa_lagdiffs2)])
plot_grid(plotlist = visual_qa(working_dat, subset(check_deltadiff2, logger == "cr23x"), add_fourth = "kj_temp")) # flag 2003-08-20 tmin, 2003-12-29 tmax + 2002-01-17, 2006-12-28 tmax (high value)
## cr1000
plot_grid(plotlist = qa_lagdiffs2[grep("cr10", qa_lagdiffs2)]) # leave be
plot_grid(plotlist = visual_qa(working_dat, subset(check_deltadiff2, logger == "cr1000"), add_fourth = "kj_temp")) # leave be

# select values to flag
flag_lagdiff2 <- subset(check_deltadiff2, 
                        (met == "airtemp_min" & as.character(date) %in% c("1992-09-07", "1992-10-16", "1992-11-23", "1992-12-04", "1992-12-28", "1993-01-12", "1993-06-13", "1999-01-29", "1999-11-08", "2003-08-20"))|
                           (met == "airtemp_max" & as.character(date) %in% c("2003-12-29"))) %>%
  # append high value tmax dates
  rbind(subset(working_dat, met == "airtemp_max" & as.character(date) %in% c("2002-01-17", "2006-12-28"))) %>%
  ungroup() %>% data.frame()

# visualize with keith's data to be sure these are the values to flag
plot_grid(plotlist = visual_qa(working_dat, flag_lagdiff2, add_fourth = "kj_temp")) #1992-11-23 actually doesn't look too wild for a min temp (kj temps are only mins of the average hourly temp)
flag_lagdiff2 <- subset(flag_lagdiff2, !date == "1992-11-23")

# flag values in working dataset
working_dat <- flag_temp(subset(flag_lagdiff2, !(met == "airtemp_max" & as.character(date) %in% c("2002-01-17", "2006-12-28"))),
                         error = "daily rate change deviance")
working_dat <- flag_temp(subset(flag_lagdiff2, met == "airtemp_max" & as.character(date) %in% c("2002-01-17", "2006-12-28"))) # these will be marked as comparative deviance, which is true for c1 chart

# clean up environment
rm(qa_diffdaily, qa_lagdiffs1, qa_lagdiffs1kj, qa_lagdiffs2, flag_lagdiff, flag_lagdiff2,
   check_deltadiff1, check_deltadiff2)



# -- RUN THROUGH DIFF DAILY ONE MORE TIME AND REVIEW QA'D DATA  ----
# final check
working_dat <- diff_daily(working_dat, rename = F)
par(mfrow=c(1,3))
boxplot(working_dat$deviance_1, main = "daily dev. from c1 chart")
boxplot(working_dat$deviance_2, main = "daily dev. from snotel")
boxplot(working_dat$deviance_3, main = "daily dev. from sdl chart")
par(mfrow=c(1,1))
# look at more extreme deviance in both sdl and d1
check_diffdaily <- subset(working_dat, deviance_1 >= 3 & deviance_2 >= 4 & deviance_3 >= 4) 
qa_diffdaily <- visual_qa(working_dat, check_diffdaily, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_diffdaily[grepl("cr21", qa_diffdaily)]) # leave be
plot_grid(plotlist = qa_diffdaily[grepl("cr23", qa_diffdaily)]) # needs flagging
# plot with keith's data
plot_grid(plotlist = visual_qa(working_dat, subset(check_diffdaily, logger == "cr23x"), add_fourth = "kj_temp")) # needs flagging
# flag worst offenders with keith's data
plot_grid(plotlist = qa_diffdaily[grepl("cr10", qa_diffdaily)]) # in 2014 so can't plot with keith's data.. not compelling enough to flag


# select values to flag
flag_diffdaily1 <- subset(check_diffdaily, logger == "cr23x" & as.character(date) %in% c("2001-01-27", "2001-02-11", "2001-04-12", "2001-08-22", "2003-03-24", "2006-01-21", "2008-01-25")) %>%
  # append jan 28 date
  rbind(subset(working_dat, met == "airtemp_max" & as.character(date) %in% c("2001-01-28", "2001-01-30")))
# make sure those are values to flag
plot_grid(plotlist = visual_qa(working_dat, flag_diffdaily1, add_fourth = "kj_temp"))

working_dat <- flag_temp(flag_diffdaily1)

# clean up environment
rm(check_diffdaily, qa_diffdaily, flag_diffdaily1)



# -- REVIEW AND CLEAN UP WORKING DAT -----
# plot qa'd data for review
# logger values with c1 chart values plotted behind in grey as comparison for ranges
ggplot(working_dat) + 
  geom_point(aes(date, comp1), alpha = 0.6, col = "grey80") +
  geom_point(aes(date, main, col = logger), alpha = 0.5) +
  #geom_smooth(aes(date, cr_temp, group = logger), method = "glm", col = "black") +
  scale_color_viridis_d() +
  facet_wrap(~met)

# how many points flagged?
with(working_dat, sapply(split(qa_flag, met), function(x) summary(!is.na(x)))) #74 points in tmax, 44 in tmin flagged..
with(working_dat, sapply(split(qa_flag, logger), function(x) summary(!is.na(x)))) # cr23x most problematic (73 flags), only 43 in cr21x and 2 points in cr1000 flagged

# what is relationship between loggers and d1 chart?
dplyr::select(working_dat, met, date, doy, yr, logger, main, comp1) %>%
  mutate(logger = factor(logger, levels = c("cr21x", "cr23x", "cr1000"))) %>%
  ggplot(aes(main, comp1)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0), col = "red") +
  facet_grid(met ~ logger, scales = "free") # poblems with cr23x visible, relationship with cr1000 noticeably different than with other two loggers



# -- COMPILE AND WRITE OUT FLAGGED/QA'D SDL CR DATASET -----
# want to write out old data with qa'd data for comparison
# maybe also write out final working_dat for documentation
c1crall_old_new <- left_join(c1crall_long, working_dat)

# plot what was corrected/modified using visual_qa -- for cr1000 only
qa_results <- subset(c1crall_old_new, !is.na(qa_flag) & logger != "cr21x")
qa_show <- visual_qa(c1crall_old_new, qa_results, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_show[grepl("cr23.*airtemp_min", qa_show)])
plot_grid(plotlist = qa_show[grepl("cr23.*airtemp_max", qa_show)])
plot_grid(plotlist = qa_show[grepl("cr10", qa_show)])


# moving on.. clean up old_new and write out along with final working_dat data frame for reference/documentation
c1crall_old_new <- dplyr::select(c1crall_old_new,
                                 LTER_site:qa_flag) %>%
  rename(qa_temp = main) %>%
  distinct()

# write out qa'd dataset
write_csv(c1crall_old_new, "climate_d1_c1/output_data/prep_data/qa_c1cr_temp.csv")
#if want to write out working dat with all flag columns, uncomment this next line (warning: large file)
#write_csv(working_dat, "climate_d1_c1/output_data/prep_data/qa_sdlcr_temp_workingreference.csv")
