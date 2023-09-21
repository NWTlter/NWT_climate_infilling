# run c1 chart through qa script


# script purpose:
# read in c1 chart from edi
# append tk c1 chart to raw c1 2011-ongoing
# run through qa script to look for usual suspects (e.g. stuck pen [flatline], extreme day to day swings, comparative deviance with nearby stations)
# write out qa'd dataset for next step (infilling missing values)


# notes:
# > only need to focus on 2010-ongoing because using tk infilled daily c1 dataset for self-study figure update
# > c1 hygrotherm changed oct 2014



# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN", "NP")

# set pathway to climate output data folder
datpath <- "climate_d1_c1/output_data/"

# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")
# functions for visual qa (developed while working on extended summer so lives in that folder)
source("extended_summer/analysis/scripts/qa_functions.R")


# -- GET DATA -----
# c1 chart
c1 <- getTabular(411) %>% data.frame()
# keith jennings et al infilled logger dataset
kj <- getTabular(168) %>% data.frame() # note: AVERAGE temp, not hourly infiled max/min logger temp

# ctw prepped niwot snotel temp (not QA'd)
snotel <- read_csv(paste0(datpath, "prep_data/nwt_snotel_temp.csv"), na = na_vals) %>% data.frame()

# ctw qa'd datasets
## sdl logger
sdllog_qa <- read_csv("extended_summer/analysis/output_data/prep_data/qa_sdlcr_temp.csv",
                      na = na_vals, trim_ws = T) %>% data.frame() %>%
  # need to remove overlapping dates in sdl cr23x and cr21x
  subset(!(logger == "cr23x" & date %in% seq.Date(as.Date("2000-06-24"), as.Date("2000-06-27"), 1))) %>% distinct()
## c1 logger
c1log_qa <- read.csv(paste0(datpath, "prep_data/qa_c1cr_temp.csv"), na.strings = na_vals, strip.white = T) %>% 
  data.frame() %>% distinct() %>%
  mutate(date = as.Date(date))
## sdl chart
#sdl <- getTabular(413) %>% data.frame()
# ctw qa'd sdl chart temp data
sdl_qa <- read.csv("extended_summer/analysis/output_data/prep_data/qa_sdlchart_temp.csv",
                   na.strings = na_vals, strip.white = T) %>% data.frame() %>% distinct() %>%
  mutate(date = as.Date(date))
# d1 qa
d1_qa <- read.csv("climate_d1_c1/output_data/prep_data/qa_d1_temp.csv",
                  na.strings = na_vals, strip.white = T) %>%
  mutate(date = as.Date(date))

# tim kittel infilled d1 and c1 chart, 1950s-2010
# read in from NWT renewal dropbox
tkd1 <- read.csv("../../Dropbox/NWT_data/d1_infilled_daily_temp.csv")
tkc1 <- read.csv("../../Dropbox/NWT_data/c1_infilled_daily_temp.csv")


# boulder weather
bldr <- read_delim("https://www.esrl.noaa.gov/psd/boulder/data/boulderdaily.complete", delim = " ", skip = 1,
                   col_names = c("year", "mon", "day", "tmax", "tmin", "precip", "snow", "snowcover"))

# clean up how bldr daily temp read in..
## eliminate text at bottom of file
textpos <- which(grepl("[a-z]",bldr$year))
# keep metadata if interested..
bldrmeta <- collapse(bldr[textpos,])
# remove text lines and convert cols to numeric, -998 and -999 == NA, and keep only yrs in tk dataset to ongoing
bldr <- bldr[-textpos,]
bldr <- data.frame(apply(bldr, 2, as.numeric))
bldr[bldr < -900] <- NA
bldr <- bldr[bldr$year >= min(tkd1$year),]
# convert F to C
bldr$tmaxC <- (bldr$tmax - 32) * (5/9)
bldr$tminC <- (bldr$tmin - 32) * (5/9)



# -- REVIEW/PREP DATA -----
# check data read in as expected
glimpse(c1) # wide form
glimpse(sdl_qa) # long
glimpse(d1_qa) # long, forgot to assign tk's values to qa_temp beginning of record through 2010
glimpse(sdllog_qa) #long
glimpse(d1_qa) # long
glimpse(c1log_qa) # long
glimpse(tkd1)
glimpse(tkc1)
glimpse(snotel)

# tidy chart temp datatsets (qa'd sdl chart and sdl logger already long-form)
c1_long <- tidytemp(c1, datasource = "c1", dropcol = "airtemp_avg") # not keeping avg airtemp col, only QAing max and min (tmean is chart is just the mean of tmax and tmin anyway)

# prep names in ctw qa'd/prepped datasets to join with c1 cr logger datasets (i.e. no redundant colnames across datasets)
names(sdl_qa)
names(sdllog_qa)
names(d1_qa)
names(c1log_qa)
names(snotel)
colnames(sdl_qa)[11:12] <- c("sdl_qaflag", "sdl_daysflat")
colnames(sdllog_qa)[9:ncol(sdllog_qa)]<- c("sdlcr_temp", "sdlcr_qatemp", "sdlcr_qaflag")
colnames(c1log_qa)[11:12]<- c("c1cr_qatemp", "c1cr_qaflag")
colnames(snotel)[ncol(snotel)] <- "sno_temp"
colnames(d1_qa)[10:11] <- c("d1_qatemp", "d1_qaflag")

# fix temp assignment in d1 qa dataset
## verify all temp vals before 2011 are NA, and after 2010 have values
summary(is.na(d1_qa$d1_qatemp[d1_qa$yr<2011])); summary(is.na(d1_qa$d1_qatemp[d1_qa$yr>2010]))
d1_qa$d1_qatemp[d1_qa$yr < 2011] <- d1_qa$d1_temp[d1_qa$yr < 2011]

# are there any duplicate dates in the c1 logger datasets?
dup_dates <- unique(c1log_qa$date[duplicated(c1log_qa$date[c1log_qa$met=="airtemp_max"])])
# probably overlap btwn cr23x and cr21 logger as in sdl logger
dplyr::select(c1log_qa, date, logger, met, c1cr_qatemp) %>%
  subset(date %in% dup_dates) %>%
  arrange(met, date) # yes

# remove cr21x since not using that logger for infilling
c1log_qa <- subset(c1log_qa, !(date %in% dup_dates & logger == "cr21x")) %>% distinct()

# summarize jennings et al dataset to daily min/max
# min/max jennings data as extra "station" for visual qa
kjc1 <- subset(kj, local_site == "c1") %>%
  rename(doy = jday,
         yr = year) %>%
  mutate(mon = month(date)) %>%
  group_by(LTER_site, local_site, date, yr, mon, doy) %>%
  summarise(airtemp_min = min(airtemp_avg),
            airtemp_max = max(airtemp_avg)) %>%
  gather(met, kj_temp, airtemp_min, airtemp_max) %>% distinct()


# replace c1 1950s to 2010 values with tk dataset
c1tknwt <- tkc1[,c(1:5,8)]
colnames(c1tknwt) <- c("mon", "day", "yr", "airtemp_max", "airtemp_min", "c1_flag")
c1tknwt <- mutate(c1tknwt, date = as.Date(paste(yr, mon, day, sep = "-")),
                  LTER_site = "NWT", 
                  local_site = "c1",
                  doy = yday(date)) %>%
  gather(met, c1_temp, airtemp_max:airtemp_min) %>%
  dplyr::select(colnames(c1_long)) %>%
  rbind(subset(c1_long, yr > 2010 & yr < 2019)) # exclude jan 1 date in 2019

# tidy boulder temp
bldr_long <- rename(bldr, airtemp_max = tmaxC,
                    airtemp_min = tminC) %>%
  mutate(date = paste(year, mon, day, sep = "-"),
         date = as.Date(date))
bldr_long <- tidytemp(bldr_long[c("date", "airtemp_max", "airtemp_min")], datasource = "bldr", special = NA)



# join c1 chart and all comparative temp datasets
c1_long_master <- mutate(c1tknwt, qa_flag = NA) %>% # add empty qa flag col first
  # join qa'd d1 logger
  left_join(dplyr::select(c1log_qa, -c(logger, c1cr_temp))) %>%
  # join snotel temp
  left_join(dplyr::select(snotel, -local_site)) %>%
  # join qa'd sdl dataset, dropping raw temp col and days flat column (not needed)
  left_join(dplyr::select(sdl_qa, -c(local_site, sdl_temp, sdl_flag, sdl_daysflat))) %>%
  # join qa'd sdl logger, remove raw sdl logger temp
  left_join(dplyr::select(sdllog_qa, -c(local_site, logger, sdlcr_temp))) %>%
  # join jennings infilled daily summarized data
  left_join(kjc1) %>%
  # join d1 chart
  left_join(dplyr::select(d1_qa, -c(local_site, d1_temp))) %>%
  # join boulder temp
  left_join(bldr_long) %>%
  ungroup() %>%
  distinct() %>%
  data.frame() %>%
  mutate(mon = as.numeric(mon)) %>%
  arrange(met,date)



# -- QA SENSOR/PEN FAILS (OBVIOUS OUTLIERS) -----
# create working copy
working_dat <- c1_long_master %>%
  # exclude flatline sdl chart vals from consideration
  mutate(sdl_qatemp = ifelse(grepl("flat", sdl_qaflag), NA, sdl_qatemp)) %>%
  # exclude flatline d1 chart vals from consideration
  mutate(d1_qatemp = ifelse(grepl("flat", d1_qaflag), NA, d1_qatemp)) %>%
  # subset to years post-2010 for qa
  subset(yr>2010)

# look at tails for any obvious bad values
## chart temp, split by metric
with(working_dat, lapply(split(c1_temp, met), function(x) tail(sort(x), n = 20))) 
with(working_dat, lapply(split(c1_temp, met), function(x) head(sort(x), n = 20))) # -38 and -37 seem like a jump from -33
## chart temp, split by month
with(working_dat, lapply(split(c1_temp, paste(met, mon)), summary))
# > seems like a jump for tmin in may to be -21 but tmin in jun is only -6?
with(working_dat, lapply(split(c1_temp, paste(met, mon)), function(x) head(sort(x)))) # -24 in feb also seems like a jump from -18
with(working_dat, lapply(split(c1_temp, paste(met, mon)), function(x) tail(sort(x))))



# -- TRIAGE ANY SHIFT IN CHART VS. LOGGER VALUES FOR MID-2010s -----
# > note: henry says hygrotherm for c1 chart changed in 2014 (ctw: perhaps in fall gap in 2014 data? relationship looks better after that)
# run diff_daily and delta lag functions to get differences and set up colnames for rest of normal QA treatment
working_dat <- diff_daily(working_dat, main = "c1_temp", comp1 = "c1cr_qatemp", comp2 = "sno_temp", comp3 = "sdl_qatemp", 
                          groupvars = c("met", "mon")) 
# lag and difference temp with reference stations
working_dat <- deltamet(working_dat)

# plot out differences to ID time period where relationship changes, and if changes for both tmax and tmin
# plot c1 chart temp by date by year to look for change in relationship with c1 logger
# > note: c1 logger instrumentataion changed in 2013 tho
ggplot(working_dat, aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("c1 chart (black/grey) and c1 logger (purple), 2011-2018") +
  facet_grid(met~yr, scales = "free")
# beginning of 2014 until fall 2014 looks different.. maybe also 2013?.. hard to tell

# check against snotel trends (does drift occur off of those values too)?
ggplot(working_dat, aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, comp2), col = "purple") +
  geom_point(aes(date, comp2), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp2), col = "orchid") +
  ggtitle("d1 chart (black/grey) and snotel temp (purple), 2011-2018") +
  facet_grid(met~yr, scales = "free")
# > 2014 definitely looks different, beginning of 2015 too? drift maybe starts in 2013?

# how to drift compare to end of tk record?
ggplot(subset(c1_long_master, yr %in% 2005:2010), aes(date, c1_temp)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, c1cr_qatemp), col = "purple") +
  geom_point(aes(date, c1cr_qatemp), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, c1cr_qatemp), col = "orchid") +
  ggtitle("d1 chart (black/grey) and c1 logger temp (purple), 2005-2010") +
  facet_grid(met~yr, scales = "free") # pretty tight relationships..

# look at temp mean value by year to see if there is a clear pattern in drop
# convert to mean temp
c1tknwt %>%
  #subset(yr %in% 2004:2018) %>%
  spread(met, c1_temp) %>%
  mutate(airtemp_avg = (airtemp_min+airtemp_max)*0.5) %>%
  group_by(yr) %>%
  summarize(mean_temp = mean(airtemp_avg, na.rm = T),
            se = sd(airtemp_avg, na.rm = T)/sqrt(length(!is.na(airtemp_avg))),
            nobs = length(airtemp_avg)) %>%
  ggplot(aes(yr, mean_temp)) +
  #geom_line(aes(group = yr)) +
  # demarcate where tk-infilled dataset stops
  geom_vline(aes(xintercept = 2010), col = "chocolate2", lwd =2, alpha = 0.6) + 
  geom_line() +
  geom_errorbar(aes(ymax = mean_temp + se, ymin = mean_temp - se), width = 0) +
  scale_x_continuous(breaks = seq(1950,2015,5)) 
# note: cold spell in 1980s is real. drift definitely starts in 2013 and goes through 2014.. altho 2014 missing data for summer/fall
# re-check with missing 2014 dates removed
missing2014 <- working_dat$doy[is.na(working_dat$main) & working_dat$yr == 2014] %>% unique()

c1tknwt %>%
  subset(!doy %in% missing2014) %>%
  spread(met, c1_temp) %>%
  mutate(airtemp_avg = (airtemp_min+airtemp_max)*0.5) %>%
  group_by(yr) %>%
  summarize(mean_temp = mean(airtemp_avg, na.rm = T),
            se = sd(airtemp_avg, na.rm = T)/sqrt(length(!is.na(airtemp_avg))),
            nobs = length(airtemp_avg)) %>%
  ggplot(aes(yr, mean_temp)) +
  #geom_line(aes(group = yr)) +
  # demarcate where tk-infilled dataset stops
  geom_vline(aes(xintercept = 2010), col = "chocolate2", lwd =2, alpha = 0.6) + 
  geom_line() +
  geom_errorbar(aes(ymax = mean_temp + se, ymin = mean_temp - se), width = 0) +
  scale_x_continuous(breaks = seq(1950,2015,5)) 
# > still artificial drop in 2013 and 2014

# check if drop occurs in other stations
working_dat %>%
  subset(!doy %in% missing2014) %>%
  dplyr::select(date, yr, met, main, bldr_temp, comp1, comp2, d1_qatemp, kj_temp) %>%
  subset(!is.na(main)) %>%
  gather(station, val, main:ncol(.)) %>%
  spread(met, val) %>%
  mutate(airtemp_avg = (airtemp_min+airtemp_max)*0.5) %>%
  group_by(yr, station) %>%
  summarize(mean_temp = mean(airtemp_avg, na.rm = T),
            se = sd(airtemp_avg, na.rm = T)/sqrt(length(!is.na(airtemp_avg))),
            nobs = length(airtemp_avg)) %>%
  ggplot(aes(yr, mean_temp, col = station)) +
  geom_line() +
  geom_errorbar(aes(ymax = mean_temp + se, ymin = mean_temp - se), width = 0)

# compare year to year change
c1_long_master %>%
  #subset(!doy %in% missing2014) %>%
  subset(yr > 2004) %>%
  dplyr::select(date, yr, met, c1_temp, bldr_temp, c1cr_qatemp, sno_temp) %>%
  subset(!is.na(c1_temp)) %>%
  gather(station, val, c1_temp:ncol(.)) %>%
  spread(met, val) %>%
  mutate(airtemp_avg = (airtemp_min+airtemp_max)*0.5) %>%
  group_by(yr, station) %>%
  summarize(mean_temp = mean(airtemp_avg, na.rm = T)) %>%
  arrange(station, yr) %>%
  ungroup() %>%
  group_by(station) %>%
  mutate(lagmean = lag(mean_temp)) %>%
  ungroup() %>%
  mutate(diff = mean_temp - lagmean) %>%
  # plot year to year diffs and compare
  ggplot(aes(yr, diff, col = station)) +
  scale_x_continuous(breaks = 2005:2018) +
  geom_line()

  
# diurnal range
c1tknwt %>%
  subset(!doy %in% missing2014) %>%
  spread(met, c1_temp) %>%
  mutate(DTR = airtemp_max-airtemp_min) %>%
  filter(!is.na(DTR)) %>%
  group_by(yr) %>%
  summarize(mean_DTR = mean(DTR, na.rm = T),
            se = sd(DTR, na.rm = T)/sqrt(length(DTR)),
            nobs = length(DTR)) %>%
  ggplot(aes(yr, mean_DTR)) +
  #geom_line(aes(group = yr)) +
  # demarcate where tk-infilled dataset stops
  geom_vline(aes(xintercept = 2010), col = "chocolate2", lwd =2, alpha = 0.6) + 
  geom_line() +
  geom_errorbar(aes(ymax = mean_DTR + se, ymin = mean_DTR - se), width = 0) +
  geom_point() +
  scale_x_continuous(breaks = seq(1950,2015,5)) 
# DTR I guess looks within normal DTR variability for recent years


# look at variance over time
c1_long_master %>%
  subset(yr %in% 1990:2018) %>%
  filter(!doy %in% missing2014) %>%
  filter(!is.na(c1_temp)) %>%
  group_by(met, yr) %>%
  summarize(sd_temp = sd(c1_temp, na.rm = T)) %>%
  ggplot(aes(yr, sd_temp)) +
  # demarcate where tk-infilled dataset stops
  geom_vline(aes(xintercept = 2010), col = "chocolate2", lwd =2, alpha = 0.6) + 
  # demarcate yrs where chart drops
  geom_vline(aes(xintercept = 2013), col = "dodgerblue", alpha = 0.6) + 
  geom_vline(aes(xintercept = 2014), col = "dodgerblue", alpha = 0.6) + 
  #geom_line(aes(group = yr)) +
  geom_line() +
  #geom_errorbar(aes(ymax = mean_temp + se, ymin = mean_temp - se), width = 0) +
  #geom_point() +
  scale_x_continuous(breaks = seq(1990,2020, 5)) +
  facet_grid(met~., scales = "free")
# variance looks reasonable/within range for years with shift

# > as with D1 chart, preserve DTR as is, just shift both tmin and tmax up same amount
# > figure out more precisely where c1 chart temp starts to drop and when shift ends

# what does daily differences look like?
ggplot(subset(working_dat, yr < 2016), aes(date, main_diff_1)) +
  geom_point() +
  geom_smooth(col = "grey") +
  facet_grid(met~yr, scales = "free") # kind of looks like begins oct 12 - break in 2014, then another (but lesser) shift fall 2014-jun 2015
# check distribution
ggplot(subset(working_dat, yr < 2016), aes(mon, main_diff_1, group = mon)) +
  geom_boxplot() +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(met~yr, scales = "free")

# check against snotel
ggplot(subset(working_dat, yr < 2017), aes(date, main_diff_2)) +
  geom_point() +
  geom_smooth(col = "grey") +
  facet_grid(met~yr, scales = "free")
# distribution
ggplot(subset(working_dat, yr < 2017), aes(mon, main_diff_2, group = mon)) +
  geom_boxplot() +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(met~yr, scales = "free")
# means
subset(working_dat, yr < 2017) %>%
  subset(!is.na(main_diff_2)) %>%
  group_by(met, yr, mon) %>%
  summarise(meandiff = mean(main_diff_2),
            sediff = sd(main_diff_2), sqrt(length(main_diff_2))) %>%
  ggplot(aes(mon, meandiff, group = mon)) +
  geom_errorbar(aes(ymax = meandiff + sediff, ymin = meandiff - sediff), width = 0) +
  geom_point() +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(met~yr, scales = "free")


# look for drift in oct 2012
# look at actual temp
# fall over time
ggplot(subset(working_dat, yr %in% 2011:2015 & mon >8), aes(date, main)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, fall 2011-2015") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  facet_grid(met~yr, scales = "free")

# panel by seasons
working_dat %>%
  mutate(season = ifelse(mon %in% 1:3, "wnt", 
                         ifelse(mon %in% 4:5, "spr",
                                ifelse(mon %in% 6:8, "sum", "fal")))) %>%
  mutate(season = factor(season, levels = c("wnt", "spr", "sum", "fal"))) %>%
  subset(yr == 2011) %>%
  ggplot(aes(date, main)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, 2011") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(season~met, scales = "free", nrow = 4)
# look at 7/4/2011 for 2011 summer shift, re-aligns in fall, then looks like it shifts again around dec 26 2011..
# keith's data gets off by a day sometime in feb


# sumer 2011 shift
ggplot(subset(working_dat, yr == 2011 & mon %in% 4:8), aes(date, main)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  #geom_line(aes(date, comp1), col = "purple") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  # shift keith's temp fwd one day
  geom_line(aes(date+1, kj_temp), col = "goldenrod") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, jun-jul 2011") +
  scale_x_date(date_breaks = "4 day", date_labels = "%d") +
  facet_grid(met~mon, scales = "free")

# > it really does look like tmin needs to be treated differently than tmax. 
# tmax on the whole looks fine except for late july - early aug: july 17 - aug 6 (break in data after)
# tmin looks consistently shifted lower starting may 10 2011 - aug 9 2011

# look for beginning of second shift:
ggplot(subset(working_dat, (yr == 2011 & mon == 12) |(yr == 2012 & mon == 1)), aes(date, main)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  # shift keith's temp fwd one day
  geom_line(aes(date+1, kj_temp), col = "goldenrod") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, dec 2011 - jan 2012") +
  scale_x_date(date_breaks = "2 day", date_labels = "%d") +
  facet_grid(met~., scales = "free") # looks like it starts dec 23 2011
# also keith's data back on track jan 3 2012

# verify off for all of 2013
working_dat %>%
  mutate(season = ifelse(mon %in% 1:3, "wnt", 
                         ifelse(mon %in% 4:5, "spr",
                                ifelse(mon %in% 6:8, "sum", "fal")))) %>%
  mutate(season = factor(season, levels = c("wnt", "spr", "sum", "fal"))) %>%
  subset(yr == 2013) %>%
  ggplot(aes(date, main)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, 2013") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(season~met, scales = "free", nrow = 4) 
# gets back on track in the fall.. maybe gets back off track around nov 18

# look at fall 2013 closer..
ggplot(subset(working_dat, yr == 2013 & mon > 8), aes(date, main)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  # shift keith's temp fwd one day
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, sep-dec 2013") +
  scale_x_date(date_breaks = "2 day", date_labels = "%d") +
  facet_grid(met~mon, scales = "free")
# > maybe it's off in through the fall too

# look at 2014 patterns..
ggplot(subset(working_dat, yr == 2014 & mon < 8), aes(date, main)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, sep-dec 2014") +
  scale_x_date(date_breaks = "2 day", date_labels = "%d") +
  facet_grid(met~mon, scales = "free")


ggplot(subset(working_dat, yr == 2014 & mon > 8), aes(date, main)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  # shift keith's temp fwd one day
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, sep-dec 2014") +
  scale_x_date(date_breaks = "2 day", date_labels = "%d") +
  facet_grid(met~mon, scales = "free")
# shift in fall 2014 does look different than shift before data break, so adjust separately


# look for end of shift in 2015
ggplot(subset(working_dat, yr == 2015 & mon %in% 3:7), aes(date, main)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, 2015") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  facet_grid(met~mon, scales = "free") # shift ends jun 13 2015 (break in data, when it picks up looks better)



# conclusions:
# 1) first break period to adjust in 2011...
# > tmax on the whole looks fine except for late july - early aug: july 17 - aug 6 (break in data after)
# > tmin looks consistently shifted lower starting may 10 2011 - aug 9 2011
# 2) second break period to adjust is 2011-12-23 to july 2014 (mid july, missing data after [c1 missing data mid july - mid oct 2014])
# 3) third period to adjust is oct 14 2014 (missing data before oct 14) - jun 13 2015 (missing data after june 12) 


# -- TROUBLESHOOT JENNINGS ET AL INFILLED DATASET -----
# use keith's data for correcting 2011, but need to correct values in keith's dataset where it's off by a day

# find dates where keith's data off
ggplot(subset(working_dat, yr == 2011 & mon %in% 2:3), aes(date, main)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  # shift keith's temp fwd one day
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, jun-jul 2011") +
  scale_x_date(date_breaks = "4 day", date_labels = "%d") +
  facet_grid(met~mon, scales = "free") # looks like it gets behind by one day mar 1, 2011


ggplot(subset(working_dat, yr == 2011 | (yr == 2012 & mon <3)), aes(date, main)) +
#ggplot(subset(working_dat, date >= as.Date("2011-12-25") & date <= as.Date("2012-01-05")), aes(date, main)) +
  #geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  #geom_line(aes(date, comp2), col = "steelblue2") +
  # shift keith's temp fwd one day
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 logger and kj temp, 2011") +
  #scale_x_date(date_breaks = "4 day", date_labels = "%d") +
  facet_grid(met~., scales = "free") # just off through end of 2011 (dec 30 2011), okay again in 2012

# make sure it's good in 2013
ggplot(subset(working_dat, yr %in% 2012:2013), aes(date, main)) +
  #geom_line() +
  #geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  #geom_line(aes(date, comp2), col = "steelblue2") +
  # shift keith's temp fwd one day
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  #geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  #geom_smooth(col = "grey") +
  #geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 logger and kj temp, 2011") +
  #scale_x_date(date_breaks = "4 day", date_labels = "%d") +
  facet_grid(met~yr, scales = "free") # 2012 and 2013 okay


# > use kj's logger data, adjusting mar 1 - dec 30 2011 values fwd 1 day (mar 1 will be NA I guess (or use raw logger data))

start_date <- as.Date("2011-03-01")
end_date <- as.Date("2011-12-30")
shift_seq <- seq.Date(start_date, end_date, 1)

# extract mar 1-dec 30 temp values for tmax and tmin
shift_kj <- dplyr::select(working_dat, date, met, kj_temp) %>%
  subset(date %in% shift_seq) %>%
  # be sure arranged by met and date
  arrange(met, date)

working_dat$kj_temp[working_dat$date %in% (shift_seq+1) & working_dat$met == "airtemp_max"] <- shift_kj$kj_temp[shift_kj$met == "airtemp_max"]
working_dat$kj_temp[working_dat$date %in% (shift_seq+1) & working_dat$met == "airtemp_min"] <- shift_kj$kj_temp[shift_kj$met == "airtemp_min"]
# NA mar 1 since don't have known value
working_dat$kj_temp[working_dat$date == start_date] <- NA

# plot to be sure adjustment good
ggplot(subset(working_dat, yr == 2011), aes(date, main)) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  ggtitle("d1 logger and shifted fwd kj temp, 2011") +
  facet_grid(met~yr, scales = "free") # yes

rm(shift_kj, start_date, end_date, shift_seq)



# -- ADJUST C1 CHART TEMP SHIFTS -----
# (1) spring/summer 2011
# > tmax on the whole looks fine except for late july - early aug: july 17 - aug 6 (break in data after)
# > tmin looks consistently shifted lower starting may 10 2011 - aug 9 2011
tmin_per1 <- seq.Date(as.Date("2011-05-11"), as.Date("2011-08-09"), 1)
tmax_per1 <- seq.Date(as.Date("2011-07-16"), as.Date("2011-08-06"), 1)

# find mean of good and bad period in c1 chart relative to comparative stations
## (1) tmin: may 10 2011 - aug 9 2011 -----
tmin_shift1 <- working_dat %>%
  dplyr::select(date, yr, met, main, comp1, comp2, kj_temp) %>%
  subset(met == "airtemp_min" & date < as.Date("2011-12-23")) %>%
  mutate(good = ifelse(date %in% tmin_per1, 0, 1))
# add homogenized temp -- if either snotel or logger missing, use whichever value available
tmin_shift1$hmgT <- apply(tmin_shift1[c("comp1", "comp2")], 1, function(x)mean(x, na.rm = T)) 
# add col for diff
tmin_shift1$diff <- tmin_shift1$main - tmin_shift1$hmgT

# double check period  specified is appropriate to adjust
ggplot(tmin_shift1, aes(date, diff, col = good)) +
  geom_point() +
  # first good period
  geom_smooth(data = subset(tmin_shift1, date < tmin_per1[1]), aes(group = good), method = "lm") +
  # second good period
  geom_smooth(data = subset(tmin_shift1, date > max(tmin_per1)), aes(group = good), method = "lm") +
  # overlay tmax adjustment boundaries
  geom_vline(aes(xintercept = as.Date("2011-07-17")), col = "chocolate2") +
  # out of curiosity, overlay smooth from beginning up to 2011-07-17
  geom_smooth(data = subset(tmin_shift1, date < as.Date("2011-07-17")), method = "lm", col = "red") +
  labs(y = "c1 tmin - homgenized temp") +
  geom_hline(aes(yintercept = mean(diff, na.rm = T)), col = "black") # yep
# > it looks like period to adjust needs to be split up.. adjust tmax period separately from beginning of tmin drift

# plot double mass analysis curves (cumulative values over the period)
# demarcate approx where tmax adjustment starts (i.e. tmin drops off much more too)
tmin_shift1 %>%
  filter(!is.na(main) & !is.na(hmgT)) %>%
  mutate(mainsum = cumsum(abs(main)),
         compsum = cumsum(abs(hmgT))) %>%
  ggplot(aes(compsum, mainsum, col = good)) +
  geom_line() +
  geom_vline(aes(xintercept = compsum[date == tmax_per1[1]]), col = "chocolate2")
  
# calculate means -- remove any dates where c1 tmin is NA so comparing the same average
tmin_shift1_means <- tmin_shift1 %>%
  na.omit() %>%
  gather(station, temp, main:comp2, hmgT) %>%
  # split adjustment period by first date of tmax shift
  mutate(good2 = ifelse(date < min(tmin_per1), "1_good",
                        ifelse(date < min(tmax_per1), "2_off",
                               ifelse(date < max(tmin_per1), "3_off", "4_good")))) %>%
  group_by(yr, station, good2) %>%
  summarise(meanT = mean(temp),
            seT = sd(temp)/sqrt(length(temp))) %>%
  ungroup()

# plot out means and se
ggplot(tmin_shift1_means, aes(station, meanT)) +
  geom_errorbar(aes(ymax = meanT + seT, ymin = meanT - seT)) +
  geom_point() +
  labs(y = "avg tmin") +
  facet_grid(.~good2) 
# all other station hold their positions relative to one another across periods, can shift c1 chart up to similar relative position

# what is the difference in means by period?
tmin_shift1_diff <- tmin_shift1_means %>%
  dplyr::select(-seT) %>%
  spread(station, meanT) %>%
  gather(station, meanT, comp1:hmgT) %>%
  mutate(diff = main - meanT)
ggplot(tmin_shift1_diff, aes(station, diff)) +
  geom_point() +
  labs(y = "c1 meanT - station meanT") +
  facet_grid(.~good2) # still hard to tell bc station relationships with one another change as it gets warmer.. look at previous summer

compare2010 <- c1_long_master %>%
  subset(yr %in% 2008:2010)
compare2010$hmgT <- apply(compare2010[c("sno_temp", "c1cr_qatemp")],1, function(x) mean(x, na.rm = T))
# apply periods
compare2010$good2 <- with(compare2010, ifelse(doy < yday(min(tmin_per1)), "1_good",
                                              ifelse(doy < yday(min(tmax_per1)), "2_off",
                                                                ifelse(doy < yday(max(tmin_per1)), "3_off", "4_good"))))
means_2010 <- compare2010 %>%
  rename(main = c1_temp,
         comp1 = c1cr_qatemp,
         comp2 = sno_temp) %>%
  dplyr::select(date, yr, met, main, comp1, comp2, hmgT, good2) %>%
  na.omit() %>%
  gather(station, temp, main:hmgT) %>%
  group_by(yr, met, station, good2) %>%
  summarise(meanT = mean(temp),
            seT = sd(temp)/sqrt(length(temp))) %>%
  ungroup()

# plot out means and se
means_2010[means_2010$met == "airtemp_min",] %>%
  dplyr::select(-met) %>%
  rbind(tmin_shift1_means) %>%
  ggplot(aes(station, meanT)) +
  geom_errorbar(aes(ymax = meanT + seT, ymin = meanT - seT), width = 0.2) +
  geom_point() +
  labs(y = "avg tmin",
       title = "2008-2011 tmin means, split by good2 periods for 2011 trblshoot") +
  facet_grid(good2~yr, scales = "free_y") # closer relationships for 2008-2010, esp in period of july drop (3_off)..

# what is the difference in means by period?
tmin_diff2010 <- subset(means_2010, met == "airtemp_min") %>%
  dplyr::select(-met) %>%
  rbind(tmin_shift1_means) %>%
  dplyr::select(-seT) %>%
  spread(station, meanT) %>%
  gather(station, meanT, comp1:hmgT) %>%
  mutate(diff = main - meanT)
ggplot(tmin_diff2010, aes(station, diff)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  geom_point() +
  labs(y = "c1 meanT - station meanT") +
  facet_grid(good2~yr)

# find difference btwn mean diffs 2008-2010 and 2011 dif, by period
adjust_tmin1 <- subset(tmin_diff2010, yr <2011) %>%
  filter(station == "hmgT") %>%
  group_by(good2) %>%
  summarize(meandiff = mean(diff)) %>%
  left_join(tmin_shift1_diff[tmin_shift1_diff$station == "hmgT", c("good2", "diff")]) %>%
  rename(diff2011 = diff) %>%
  left_join(tmin_diff2010[tmin_diff2010$yr == 2010 & tmin_diff2010$station == "hmgT", c("good2", "diff")]) %>%
  rename(diff2010 = diff)

# before adjusting, look at diff in tmax. is it similar to 3_off diff for tmin?

## (1) tmax: july 17 - aug 6 2011 (break in data) -----
tmax_shift1 <- working_dat %>%
  dplyr::select(date, yr, met, main, comp1, comp2) %>%
  subset(met == "airtemp_max" & date < as.Date("2011-12-23")) %>%
  mutate(good = ifelse(date %in% tmax_per1, 0, 1))
# homogenize snotel and c1 logger
tmax_shift1$hmgT <- apply(tmax_shift1[c("comp1", "comp2")], 1, function(x)mean(x, na.rm = T)) 
# difference c1 from homogenzied temp
tmax_shift1$diff <- tmax_shift1$main - tmax_shift1$hmgT

# double check period  specified is appropriate to adjust
tmax_shift1 %>%
  ggplot(aes(date, diff, col = good)) +
  geom_point() +
  # demarcate when tmin adjust period starts, for comparison
  geom_vline(aes(xintercept = tmin_per1[1]), col = "seagreen", lwd = 1, alpha = 0.6) +
  labs(y = "c1 tmax - homogenized tmax") +
  geom_hline(aes(yintercept = mean(diff, na.rm = T)), col = "black") # yep
# plot time series
tmax_shift1 %>%
  mutate(mon = month(date), 
         season = ifelse(mon %in% 1:3, "1_winter", 
                         ifelse(mon %in% 4:5, "2_spring",
                                ifelse(mon %in% 6:9, "3_summer", "4_fall")))) %>%
  ggplot(aes(date, main, col = good)) +
  geom_vline(aes(xintercept = min(tmin_per1)), col = "chocolate2") +
  geom_vline(aes(xintercept = max(tmin_per1)), col = "chocolate2") +
  geom_line(alpha = 0.5) +
  geom_line(aes(date, hmgT), col = "orchid", alpha = 0.5) +
  ggtitle("tmax in period 1, with tmin shift window demarcated") +
  facet_wrap(~season, scales = "free_x")

# plot time series for tmin to be sure one more time they really do drop in different windows
tmin_shift1 %>%
  mutate(mon = month(date), 
         season = ifelse(mon %in% 1:3, "1_winter", 
                         ifelse(mon %in% 4:5, "2_spring",
                                ifelse(mon %in% 6:9, "3_summer", "4_fall")))) %>%
  ggplot(aes(date, main, col = good)) +
  geom_vline(aes(xintercept = min(tmin_per1)), col = "chocolate2") +
  geom_vline(aes(xintercept = max(tmin_per1)), col = "chocolate2") +
  geom_line(alpha = 0.5) +
  geom_line(aes(date, hmgT), col = "orchid", alpha = 0.5) +
  ggtitle("tmin in period 1, with tmin shift window demarcated") +
  facet_grid(.~season, scales = "free_x")
# yes, different windows.   

# plot double mass analysis curves (cumulative values over the period)
tmax_shift1 %>%
  filter(!is.na(main) & !is.na(hmgT)) %>%
  filter(month(date) %in% 5:8) %>%
  mutate(mainsum = cumsum(abs(main)),
         compsum = cumsum(abs(hmgT))) %>%
  ggplot(aes(compsum, mainsum, col = as.factor(good))) +
  #geom_line() +
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.5) # there's a slight shift.. not as pronounced as with tmin, but slope is different

# what are the slopes?
dblmass2 <- tmax_shift1 %>%
  filter(!is.na(main) & !is.na(hmgT)) %>%
  mutate(mainsum = cumsum(abs(main)),
         compsum = cumsum(abs(hmgT)))
# overall
summary(lm(mainsum ~ compsum, data =dblmass2)) #0.963
# jan to date before first bad date
summary(lm(mainsum ~ compsum, data = subset(dblmass2, good != 0 & date < min(tmax_per1)))) #0.948
# off period
summary(lm(mainsum ~ compsum, data = subset(dblmass2, good == 0))) #0.870
# period after bad period
summary(lm(mainsum ~ compsum, data = subset(dblmass2, good == 1 & date > max(tmax_per1)))) #0.998

# calculate means -- remove any dates where c1 tmax is NA so comparing the same average
tmax_shift1_means <- tmax_shift1 %>%
  na.omit() %>%
  gather(station, temp, main:comp2, hmgT) %>%
  # split means by same 4 periods used for tmin for comparison, and to be able to bind to 2008-2010 means more easily
  mutate(good2 = ifelse(date < min(tmin_per1), "1_good",
                        ifelse(date < min(tmax_per1), "2_off",
                               ifelse(date < max(tmin_per1), "3_off", "4_good")))) %>%
  group_by(yr, station, good2) %>%
  summarise(meanT = mean(temp),
            seT = sd(temp)/sqrt(length(temp))) %>%
  ungroup()

# plot out means and se
ggplot(tmax_shift1_means, aes(station, meanT)) +
  geom_errorbar(aes(ymax = meanT + seT, ymin = meanT - seT)) +
  geom_point() +
  facet_wrap(~good2)  # good, only 3_off looks like period that needs to be adjusted (tmax shift period not the same as tmin shift period)
# other stations hold their positions relative to one another across periods, can shift c1 chart up to similar relative position

# compare to 2008-2010
# plot out means and se
means_2010[means_2010$met == "airtemp_max",] %>%
  dplyr::select(-met) %>%
  rbind(tmax_shift1_means) %>%
  ggplot(aes(station, meanT)) +
  geom_errorbar(aes(ymax = meanT + seT, ymin = meanT - seT), width = 0.2) +
  geom_point() +
  labs(y = "avg tmax",
       title = "2008-2011 tmax means, split by good2 periods for 2011 trblshoot",
       subtitle = "'2_off' period is okay for tmax, only off for tmin") +
  facet_grid(good2~yr, scales = "free_y") # closer relationships for 2008-2010, esp in period of july drop (3_off)..

# what is the difference in means by period?
tmax_diff2010 <- subset(means_2010, met == "airtemp_max") %>%
  dplyr::select(-met) %>%
  rbind(tmax_shift1_means) %>%
  dplyr::select(-seT) %>%
  spread(station, meanT) %>%
  gather(station, meanT, comp1:hmgT) %>%
  mutate(diff = main - meanT)
ggplot(tmax_diff2010, aes(station, diff)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  geom_point() +
  labs(y = "c1 tmax meanT - station meanT") +
  facet_grid(good2~yr)



# find difference btwn mean diffs 2008-2010 and 2011 dif, by period
adjust_tmax1 <- subset(tmax_diff2010, yr <2011) %>%
  filter(station == "hmgT") %>%
  group_by(good2) %>%
  summarize(meandiff = mean(diff)) %>%
  # append 2011 differences
  left_join(tmax_diff2010[tmax_diff2010$station == "hmgT" & tmax_diff2010$yr == 2011, c("good2", "diff")]) %>%
  rename(diff2011 = diff) %>%
  left_join(tmax_diff2010[tmax_diff2010$yr == 2010 & tmax_diff2010$station == "hmgT", c("good2", "diff")]) %>%
  rename(diff2010 = diff)


# combine adjustment values to compare
adjust_temp1 <- rbind(cbind(met = "airtemp_min", adjust_tmin1),
                 cbind(met = "airtemp_max", adjust_tmax1)) %>%
  #diff off periods from good periods
  mutate(adjust_mean = diff2011 - meandiff,
         adjust_2010 = diff2011 - diff2010)

# what are adjustments?
## round to whole numbers bc chart data whole numer
subset(adjust_temp1, good2 == "2_off" & met == "airtemp_min" | good2 == "3_off") %>%
  dplyr::select(met, good2, adjust_mean, adjust_2010) %>%
  mutate_at(c("adjust_mean","adjust_2010"), round)
# bc want to preserve DTR as much as possible and not sure of snotel/logger trustworthiness use 2008-2010 mean reference, 
# adjust tmax shift period (3_off) by +3 for tmin and tmax, and by +1 for earlier period where tmin is off

July11_shift <- abs(unique(round(adjust_temp1$adjust_mean[adjust_temp1$good2 == "3_off"])))
Spr11_tminshift <- abs(round(adjust_temp1$adjust_mean[adjust_temp1$good2 == "2_off" & adjust_temp1$met == "airtemp_min"]))

#look at adjustment before applying
tmax_shift1 %>%
  subset(date %in% tmin_per1) %>%
  mutate(newT = ifelse(date %in% tmax_per1, main+ July11_shift, main)) %>%
  # adjusted temp
  ggplot(aes(date, newT)) +
  # old temp
  geom_line(aes(date, main), col = "grey50", alpha = 0.5) +
  geom_vline(aes(xintercept = min(tmax_per1)), col = "chocolate2") +
  geom_vline(aes(xintercept = max(tmin_per1)), col = "chocolate2") +
  # new temp
  geom_line(alpha = 0.5, col = "steelblue2") +
  # homogenized temp
  geom_line(aes(date, hmgT), col = "orchid", alpha = 0.5) +
  ggtitle("c1 tmax shifted period 1 (blue), with raw tmin (grey) and homogenized temp (purple)") 
# seems like a reasonable compromise (sometimes warmer than homog temp, sometimes cooler)

# look at tmin
tmin_shift1 %>%
  subset(month(date) %in% 4:8) %>%
  mutate(newT = ifelse(date %in% tmin_per1, main + Spr11_tminshift, main),
         # apply july shift
         newT = ifelse(date %in% tmax_per1, main + July11_shift, newT)) %>%
  # adjusted temp
  ggplot(aes(date, newT)) +
  # old temp
  geom_line(aes(date, main), col = "grey50", alpha = 0.5) +
  geom_vline(aes(xintercept = min(tmin_per1)), col = "seagreen") +
  geom_vline(aes(xintercept = min(tmax_per1)), col = "chocolate2") +
  geom_vline(aes(xintercept = max(tmin_per1)), col = "seagreen") +
  # new temp
  geom_line(alpha = 0.5, col = "steelblue2") +
  # homogenized temp
  geom_line(aes(date, hmgT), col = "orchid", alpha = 0.5) +
  ggtitle("c1 tmin shifted period 1 (blue), with raw tmin (grey) and homogenized temp (purple)") 
# adjusted tmin warmer at the end, but those values will get flagged if really off
# otherwise looks okay

# apply shifts to working dat and add flag
# tmin spring shift
working_dat$main[working_dat$date %in% seq.Date(min(tmin_per1), (min(tmax_per1)-1), 1) & working_dat$met == "airtemp_min"] <- working_dat$main[working_dat$date %in% seq.Date(min(tmin_per1), (min(tmax_per1)-1), 1) & working_dat$met == "airtemp_min"] + Spr11_tminshift
working_dat$qa_flag[working_dat$date %in% seq.Date(min(tmin_per1), (min(tmax_per1)-1), 1) & working_dat$met == "airtemp_min"] <- "Artificial drop in c1 tmin only, shifted +1C based on comparative analysis with c1 cr23x logger and c1 snotel"

# tmin and tmax july shift
working_dat$main[working_dat$date %in% seq.Date(min(tmax_per1), max(tmin_per1), 1)] <- working_dat$main[working_dat$date %in% seq.Date(min(tmax_per1), max(tmin_per1), 1)] + July11_shift 
working_dat$qa_flag[working_dat$date %in% seq.Date(min(tmax_per1), max(tmin_per1), 1)] <- "Artificial drop, shifted +3C based on comparative analysis with c1 cr23x logger and c1 snotel"




# (2) Dec 23, 2011 - July 15, 2014 (to break in data/new hygrotherm in fall 2014) -----
stop2 <- min(working_dat$date[is.na(working_dat$main) & working_dat$mon == 7 & working_dat$yr == 2014]) #2014-07-15 = last date with tmax and tmin values, NA values start 2014-07-16 and go through 2014-10-13
temp_per2 <- seq.Date(as.Date("2011-12-23"), stop2, 1)
temp_shift2 <- working_dat %>%
  dplyr::select(date, met, main, comp1, comp2) %>%
  subset(date > max(tmin_per1) & date <= max(temp_per2)) %>%
  mutate(good = ifelse(date %in% temp_per2, 0, 1))
# add homogenzied temp
temp_shift2$hmgT <- apply(temp_shift2[c("comp1", "comp2")], 1, function(x)mean(x, na.rm = T))
# take difference
temp_shift2$diff <- temp_shift2$main - temp_shift2$hmgT

# keep track of date cr logger switched
switch_date <- max(c1log_qa$date[c1log_qa$logger == "cr23x"])

# plot double mass analysis curves (cumulative values over the period)
temp_shift2 %>%
  filter(!is.na(main) & !is.na(hmgT)) %>%
  group_by(met) %>%
  mutate(mainsum = cumsum(abs(main)),
         compsum = cumsum(abs(hmgT))) %>%
  ggplot(aes(compsum, mainsum, col =good, group = good)) +
  #geom_abline(aes(slope = 1, intercept = 0), col = "black") +
  geom_line() +
  # add geom vline for date logger switched
  ## can only add one line at time bc call applied to both tmax[1] and tmin[2], ggplot only wants one value
  #geom_vline(aes(xintercept = compsum[date == (switch_date+1)][2]), col ="chocolate2") +
  #geom_point(alpha = 0.3) +
  #geom_smooth(method = "lm", se = F) + 
  facet_wrap(~met, scales = "free")

# calculate means -- remove any dates where c1 is NA so comparing the same average
na_dates_tempshift2 <- sort(unique(temp_shift2$date[is.na(temp_shift2$main)])) # this condenses tmin and tmax missing dates
temp_shift2_means <- temp_shift2 %>%
  filter(!date %in% na_dates_tempshift2 &
           # also limit dates to anything preceeding logger switch (assuming amt chart off was consistent until end of hygrotherm life [most conservative, even tho might have drifted more])) 
           date < switch_date)%>%
  # remove row if na in other station cols
  na.omit() %>% 
  gather(station, temp, main:comp2, hmgT) %>%
  group_by(met, station, good) %>%
  summarise(meanT = mean(temp),
            seT = sd(temp)/sqrt(length(temp))) %>%
  ungroup()

# plot out means and se
ggplot(temp_shift2_means, aes(station, meanT)) +
  geom_errorbar(aes(ymax = meanT + seT, ymin = meanT - seT)) +
  geom_point() +
  # add eyeballed adjustment
  #geom_errorbar(data = subset(temp_shift2_means, station == "main" & good == 0), aes(x = station, ymax = (meanT+1) + seT, ymin = (meanT+1) - seT), col = "orchid") +
  #geom_point(data = subset(temp_shift2_means, station == "main" & good == 0), aes(station, meanT + 1), col = "orchid") +
  ggtitle(paste0("Off (0) = ", temp_per2[1]," to ", switch_date, "; Good (1) = ", str_flatten(range(temp_shift2$date[temp_shift2$good == 1]), collapse = " to "))) +
  facet_grid(met~good, scales = "free_y") 

# look at differences
# check differences
temp_diff2 <- temp_shift2_means %>%
  dplyr::select(-seT) %>%
  spread(station, meanT) %>%
  gather(station, meanT, comp1:hmgT) %>%
  mutate(diff = main - meanT)

# plot differences
ggplot(temp_diff2, aes(station, diff)) +
  geom_point() +
  labs(y = "c1 mean temp - comparative tmean") +
  ggtitle("2nd shift period, differences btwn c1 means and comparative means") +
  facet_grid(met~good) # go with relationship to c1 logger, more trustworthy.. also 

# what is difference between good period and off period delta for c1 vs. logger?
adjust_temp2 <- subset(temp_diff2, station != "comp2" & good == 1) %>%
  dplyr::select(-c(good,main, meanT)) %>%
  rename(good_diff = diff) %>%
  left_join(temp_diff2[temp_diff2$good == 0 & temp_diff2$station %in% c("comp1", "hmgT"), c("met", "station", "diff")]) %>%
  rename(off_diff = diff) %>%
  mutate(adjust = good_diff - off_diff)

round(adjust_temp2$adjust) # either way, +1 to tmin and tmax
shift_per2 <- unique(round(adjust_temp2$adjust))

# apply shifts to working dat and add flag
# tmin and tmax period 2 shift
working_dat$main[working_dat$date %in% temp_per2] <- working_dat$main[working_dat$date %in% temp_per2] + shift_per2
working_dat$qa_flag[working_dat$date %in% temp_per2] <- "Artificial drop, shifted +1C based on comparative analysis with c1 cr23x logger and c1 snotel"



# (3) (break in data) Oct 14, 2014 - June 11, 2015 (break in data) -----
# values with new hygrotherm start 10-14-2014 (data NA from 2014-07-16 to 2014-10-13, old hygrotherm/chart recorder pooped out)
# june 2015 break in data = 2015-06-12 to 2015-06-14
# by now new cr1000 in use, can use that as reference to adjust bc is most reliable source, as well as relationship to logger in good period after
start3 <- max(working_dat$date[is.na(working_dat$main) & working_dat$mon == 10 & working_dat$yr == 2014]) +1
stop3 <- min(working_dat$date[is.na(working_dat$main) & working_dat$mon == 6 & working_dat$yr == 2015]) -1 #stop3 = 2015-06-11
temp_per3 <- seq.Date(start3, stop3, 1)
temp_shift3 <- working_dat %>%
  dplyr::select(date, mon, met, main, comp1, comp2) %>%
  # only choose dates after hygrotherm switched
  subset(date > max(temp_per2)) %>%
  mutate(good = ifelse(date %in% temp_per3, 0, 1))
# add homogenized c1 logger and snotel
temp_shift3$hmgT <- apply(temp_shift3[c("comp1", "comp2")], 1, function(x) mean(x, na.rm =T))
# add diff
temp_shift3$diff <- temp_shift3$main - temp_shift3$hmgT

# calculate means -- remove any dates where c1 tmin is NA so comparing the same average
temp_shift3_means <- temp_shift3 %>%
  na.omit() %>%
  gather(station, temp, main:comp2, hmgT) %>%
  group_by(station, met, good) %>%
  summarise(meanT = mean(temp),
            seT = sd(temp)/sqrt(length(temp))) %>%
  ungroup()

# plot out means and se
ggplot(data = subset(temp_shift3_means, station != "bldr_temp"), aes(station, meanT, col = good)) +
  geom_errorbar(aes(ymax = meanT + seT, ymin = meanT - seT), width = 0) +
  geom_point() +
  facet_grid(met~good, scales = "free") 
# all other station hold their positions relative to one another across periods, can shift c1 chart up to similar relative position

# i think what makes sense is capture the same period as temp3 shift period, but for subsequent years and compare those means
# since the chart relationship to logger does change seasonally (e.g. pen can get stuck more in winter, clock can freeze, etc.)
# comparing apples to apples in that way
# need to code up period sort of like water year:
## 1_off: 2014-10-13 to 2015-06-12
## 2_good: 2015-10-13 to 2016-06-12
## 3_good: 2016-10-13 to 2017-06-12
# try to be programmatic about it..
shift3_2good <- seq.Date(as.Date(gsub(2014, 2015, min(temp_per3))),
                         as.Date(gsub(2015, 2016, max(temp_per3))), 1)
shift3_3good <- seq.Date(as.Date(gsub(2014, 2016, min(temp_per3))),
                         as.Date(gsub(2015, 2017, max(temp_per3))), 1)
shift3_4good <- seq.Date(as.Date(gsub(2014, 2017, min(temp_per3))),
                         as.Date(gsub(2015, 2018, max(temp_per3))), 1)
# reduced [to same-day-of-year] means
temp_shift3_means_red <- temp_shift3 %>%
  mutate(good = ifelse(date %in% temp_per3, "1_off",
                       ifelse(date %in% shift3_2good, "2_good",
                              ifelse(date %in% shift3_3good, "3_good",
                                     ifelse(date %in% shift3_4good, "4_good", NA))))) %>%
  filter(!is.na(good)) %>%
  #add yday for keeping track of days with missing data
  mutate(doy = yday(date))

# how many NAs by period
with(temp_shift3_means_red, sapply(split(main, good), function(x)summary(is.na(x)))) #29 days in off period, 16 days in 2nd period, none missing after
#dbl check NAs for logger in 2018
with(temp_shift3_means_red, sapply(split(comp1, good), function(x)summary(is.na(x)))) # nada
#dbl check NAs for snotel
with(temp_shift3_means_red, sapply(split(comp2, good), function(x)summary(is.na(x)))) # missing 2 days in 3rd period


# gather days of year that are missing for c1 chart so comparing similar averages across period
na_days_temp3 <- unique(c(temp_shift3_means_red$doy[is.na(temp_shift3_means_red$main)],
                    temp_shift3_means_red$doy[is.na(temp_shift3_means_red$comp2)]))
# remove missing days from all periods and proceed with calculating means
temp_shift3_means_red <- temp_shift3_means_red %>%
  filter(!doy %in% na_days_temp3) %>%
  dplyr::select(-doy) %>%
  ungroup() %>%
  data.frame() %>%
  gather(station, temp, main:comp2, hmgT) %>%
  group_by(station, met, good) %>%
  summarise(meanT = mean(temp),
            seT = sd(temp)/sqrt(length(temp))) %>%
  ungroup()

# plot means with SEs
ggplot(temp_shift3_means_red, aes(station, meanT)) +
  geom_errorbar(aes(ymax = meanT + seT, ymin = meanT - seT), width = 0) +
  geom_point() +
  facet_grid(met~good, scales = "free") 

# check differences
temp_diff3 <- temp_shift3_means_red %>%
  dplyr::select(-seT) %>%
  spread(station, meanT) %>%
  gather(station, meanT, comp1:hmgT) %>%
  mutate(diff = main - meanT)

# plot differences
ggplot(temp_diff3, aes(station, diff)) +
  geom_point() +
  labs(y = "c1 mean temp - comparative tmean") +
  facet_grid(met~good) # go with relationship to c1 logger, more trustworthy.. 

# is there any reason not to use per 3 and 4 as references? check how the time series plots out (e.g. there is a drop in april one year)
# check first part of periods
ggplot(subset(working_dat, date >= temp_per3[1] & date <= max(shift3_4good) & month(date) %in% 10:12), aes(date, main)) +
  #geom_line() +
  geom_point(alpha = 0.5) +
  #geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("c1 chart (black/grey) and c1 logger (purple), 2014-2018: Oct-Dec") +
  facet_grid(met~yr, scales = "free")

# check second part of periods
ggplot(subset(working_dat, date >= temp_per3[1] & date <= max(shift3_4good) & month(date) <6), aes(date, main)) +
  #geom_line() +
  geom_point(alpha = 0.5) +
  #geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("c1 chart (black/grey) and c1 logger (purple), 2014-2018: Jan-Jun") +
  facet_grid(met~yr, scales = "free") # maybe just use 2_good and 3_good in case there is sensor drift in logger by 2018 (J. Morse said logger switched recently I think..)

# average diff in 2_good and 3_good and adjust 1_off to that
# find difference btwn mean diffs 2008-2010 and 2011 dif, by period
adjust_temp3 <- subset(temp_diff3, good != "4_good") %>% # remove 2017-2018 as reference
  filter(station == "comp1" & good != "1_off") %>% #c1 logger
  group_by(met) %>%
  summarize(meandiff = mean(diff)) %>%
  # append 1_off differences
  left_join(temp_diff3[temp_diff3$station == "comp1" & temp_diff3$good == "1_off", c("met", "diff")]) %>%
  rename(diff_1off = diff) %>% # 2014-2015 off period
  # append 2nd (closest in time period, 2015-2016)
  left_join(temp_diff3[temp_diff3$good == "2_good" & temp_diff3$station == "comp1", c("met", "diff")]) %>%
  rename(diff_2good = diff) # 2015-2016 good reference

# logger was replaced in june 2013, c1 chart hygro in oct 2014.. c1 should agree most with first good period (nearest in time), so use that for adjustment
# what is average difference? # round to nearest degree since chart data is whole number
round(adjust_temp3$diff_1off - adjust_temp3$diff_2good) # go with +1 to tmax and tmin, preserves DTR
shift_per3 <- abs(unique(round(adjust_temp3$diff_1off - adjust_temp3$diff_2good)))

# apply shifts to working dat and add flag
# tmin and tmax period 2 shift
working_dat$main[working_dat$date %in% temp_per3] <- working_dat$main[working_dat$date %in% temp_per3] + shift_per3
working_dat$qa_flag[working_dat$date %in% temp_per3] <- "Artificial drop, shifted +1C based on comparative analysis with c1 cr1000 logger"


# yay it's over!
# save a copy of working dat in case make mistake coding in next section
working_dat_copy <- data.frame(working_dat) # ignore warning messages about day of year col.. stupid dplyr/tidyverse junky bug based on tibble

# clean up environment
rm(adjust_temp1, adjust_temp2, adjust_temp3, adjust_tmax1, adjust_tmin1,
   compare2010, means_2010, temp_diff2, temp_diff3, temp_shift2, temp_shift2_means, temp_shift3, temp_shift3_means, temp_shift3_means_red,
   tmax_diff2010, tmax_shift1, tmin_diff2010, tmin_shift1, tmin_shift1_diff, tmin_shift1_means,
   dblmass2, na_dates_temp2, na_dates_tempshift2, na_days_temp3, start3, stop2, stop3)


# how does time series look now 2011 to present?
ggplot(working_dat, aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("c1 chart (black/grey, adjusted) and c1 logger (purple), 2011-2018") +
  facet_grid(met~yr, scales = "free")
# better. c1 temp still low in 2013 and 2014 compared to logger but that could also be due to missing values
# 2011, 2013 and 2015 look more where they should be given trends in 2008-2010




# -- BEGIN NORMAL QA ROUTINE (yay!) -----

# -- FLAG DAILY DEPARTURES FROM COMPARATIVE DATASETS -----
# round 1 diff daily and flagging
working_dat <- diff_daily(working_dat, rename = FALSE, groupvars = c("met", "mon"))  
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff1 <- filter(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff1 <- visual_qa(working_dat, check_daily_diff1, add_fourth = "sdlcr_qatemp") # add jennings infilled logger as fourth comparison
# visualize
plot_grid(plotlist = qa_daily_diff1)
# > 2013 looks like pen got stuck .. will get flagged in flatline check
# 2015 is a low tmin value (-38), flag given snotel and c1logger didn't record it (and c1 logger is cr1000 by then)

# round 1:
flag_dailydiff1 <- subset(check_daily_diff1, met == "airtemp_min" & yr == 2015)
working_dat <- flag_temp(flag_dailydiff1)


# since only 2 values flagged, and one is flatline, run flatline check..
# -- FLATLINE CHECK -----
# maybe run flatline check first before diff daily test...
## add lag temp
working_dat <- deltamet(working_dat)
# look for 3 or more consecutive days of 0 change
working_dat <- check_flatline(working_dat, ctdays = 2)
flat_check <- subset(working_dat, !is.na(flatline)) # 15 instances of flatlining
qa_flatline <- visual_qa(working_dat, flat_check, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_flatline) 
# > flag 2013-05-28 tmin; warn: 2011-06-22, 2017-08-25, and 2017-11-03 as flatline but not enough discrepancy with other station to remove
# > can annotate other dates as multiday flatline in temp but appropriate given trends at other nearby stations
# > also, pay attention to 2017-08-22 and 2017-08-28 tmin in delta diff or day to day delta (e.g. other stations get cooler, but c1 gets warmer or vv.)

# can give warnings or acknowledgment of everything else (i.e. there is a flatline, but better to use than infilling/roughly follows other trends at nearby stations)
flag_flatline <- subset(flat_check, date == "2013-05-28")
warn_flatline <- subset(flat_check, as.character(date) %in% c("2011-06-22", "2017-08-25", "2017-11-03"))
okay_flatline <- subset(flat_check, !as.character(date) %in% c("2013-05-28", "2011-06-22", "2017-08-25", "2017-11-03"))

# flag and remove values
for(i in 1:nrow(flag_flatline)){
  temp_df <- flag_flatline[i, ]
  # build a sequence of dates to flag
  current_date <- temp_df$date
  start_date <- current_date - 1
  end_date <- start_date + temp_df$flatline
  working_dat$qa_flag[working_dat$date %in% seq.Date(start_date, end_date,1) & working_dat$met == temp_df$met] <- "pen stuck"
  working_dat$main[working_dat$date %in% seq.Date(start_date, end_date,1) & working_dat$met == temp_df$met] <- NA
}

# warn but don't remove values
for(i in 1:nrow(warn_flatline)){
  temp_df <- warn_flatline[i, ]
  # build a sequence of dates to flag
  current_date <- temp_df$date
  start_date <- current_date - 1
  end_date <- start_date + temp_df$flatline
  working_dat$qa_flag[working_dat$date %in% seq.Date(start_date, end_date,1) & working_dat$met == temp_df$met] <- "advisory: 4+ day flatline, roughly follows nearby stations"
}

# annotate in flag col that flatline exists but is okay
for(i in 1:nrow(okay_flatline)){
  temp_df <- okay_flatline[i, ]
  # build a sequence of dates to flag
  current_date <- temp_df$date
  start_date <- current_date - 1
  end_date <- start_date + temp_df$flatline
  working_dat$qa_flag[working_dat$date %in% seq.Date(start_date, end_date,1) & working_dat$met == temp_df$met] <- "note: 4+ day flatline but appropriate given trends at nearby station, okay to use"
}


# clean up environment
rm(flat_check, qa_flatline, flag_flatline, warn_flatline, okay_flatline)


# continue with diff daily checks..
# -- DIFF DAILY (by month), ROUND 2 -----
# run diff daily again so flatline errors not plotted
working_dat <- diff_daily(working_dat, rename = F,groupvars = c("met", "mon"))  
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff2 <- filter(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff2 <- visual_qa(working_dat, check_daily_diff2, add_fourth = "sdlcr_qatemp")
# visualize
plot_grid(plotlist = qa_daily_diff2)
# june tmin didn't get picked up in flatline bc only flatlines for 3 days instead of 4
# flag june 10 and jun 9 2013 tmin for flatline
flag_dailydiff2 <- rbind(check_daily_diff2,
                         subset(working_dat, date == "2013-06-09" & met == "airtemp_min"))
# flag and remove values in working dataset
working_dat <- flag_temp(flag_dailydiff2, error = "pen got stuck, flatlined for 2 days, comparative deviance")


# move on to extremes!
# clean up environment
rm()


# clean up environment
rm(check_daily_diff1, check_daily_diff2, flag_dailydiff1, flag_dailydiff2,
   qa_daily_diff1, qa_daily_diff2)



# -- QA EXTREMES (TMIN/TMAX) -----
# visual qa grand max and min
#max temp by yr
check_max1 <- check_extreme(working_dat, groupvars = c("met", "yr"), max)
qa_max1 <- visual_qa(working_dat, check_max1, add_fourth = "kj_temp")
plot_grid(plotlist = qa_max1[grep("airtemp_max", qa_max1)])  # flag aug 2011 and aug 2015, shouldn't exceed c1cr_temp by that much
plot_grid(plotlist = qa_max1[grep("airtemp_min", qa_max1)])  # 2013-07-17 looks like bad data point
# > note 2013 and 2014 values are better than they were (adjustment helped some), but still definitely lower than usual
# > could just be wonky data too (e.g. see 2013-09-13, high point is reasonable, and some other points, but drops some for a few days)

# look at deviance flags for tmax 2011-08-09, 2015-08-30 and tmin 2013-07-17
flag_max1 <- subset(check_max1, as.character(date) %in% c("2011-08-09", "2015-08-30", "2013-07-17"))
# flags except for 1 instance of comparison with sdl chart would round up to 3sd or already exceeds 3 sd from other stations, can flag with comparative deviance
working_dat <- flag_temp(flag_max1)

# move on to check yearly extreme mins..
# min temps by logger
check_min1 <- check_extreme(working_dat, groupvars = c("met", "yr"), metric=min)
qa_min1 <- visual_qa(working_dat, check_min1, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_min1[grep("airtemp_max", qa_min1)])  # look at 2018-12-04.. too bad only snotel for local comparison, but shouldn't colder than sdl in dec
plot_grid(plotlist = qa_min1[grep("airtemp_min", qa_min1)]) # looks good!

# plot tmax in dec 2018 with d1 chart, c1 logger, snotel
ggplot(subset(working_dat, yr == 2018 & mon >10 & met == "airtemp_max"), aes(date, main)) +
  geom_line(aes(date, comp2), col = "steelblue") +
  geom_point(aes(date, comp2), col = "steelblue") +
  geom_line(aes(date, sdlcr_qatemp), col = "goldenrod") +
  geom_point(aes(date, sdlcr_qatemp), col = "goldenrod") +
  geom_line(aes(date, d1_qatemp), col = "seagreen") +
  geom_point(aes(date, d1_qatemp), col = "seagreen") +
  geom_line() +
  geom_point() # dec val is also colder than d1 chart for that day, definitely flag

flag_min1 <- subset(check_min1, date == "2018-12-04" & met == "airtemp_max")
working_dat <- flag_temp(flag_min1)


# move on to monthlies...
#clean up env
rm(check_max1, qa_max1, check_min1, qa_min1, flag_max1, flag_min1)



# -- QA MONTHLY MIN/MAX -----
# monthly max temps of tmin and tmax
check_monthly_max1 <- check_extreme(working_dat, c("met", "mon"))
# run through visual qa function
qa_mon_max1 <- visual_qa(working_dat, check_monthly_max1, sorttime = "mon", add_fourth = "sdlcr_qatemp")

# visualize logger monthly maximums, by metric
## max of airtemp_max
plot_grid(plotlist = qa_mon_max1[grep("airtemp_max", qa_mon_max1)]) # looks fine
## max of airtemp_min
plot_grid(plotlist = qa_mon_max1[grep("airtemp_min", qa_mon_max1)]) #yay! looks fine too


# monthly min temps of tmin and tmax
check_monthly_min <- check_extreme(working_dat, c("met", "mon"), min)
# run through visual qa function
qa_mon_min1 <- visual_qa(working_dat, check_monthly_min, sorttime = "mon", add_fourth = "sdlcr_qatemp")

# visualize logger monthly minimums, by logger and metric
## min of airtemp_max
plot_grid(plotlist = qa_mon_min1[grep("airtemp_max", qa_mon_min1)]) # 2014-11-25 looks like chart bonked, missing data afterwards, otherwise others are fine
## min of airtemp_min
plot_grid(plotlist = qa_mon_min1[grep("airtemp_min", qa_mon_min1)]) # 2018-11-13 looks like a pen fail (looks fine day before and day prior), flag
# 2016 dec tmins looks okay, but plot out separately to look closer

# 2016 dec tmin
ggplot(subset(working_dat, yr == 2016 & mon >11 & met == "airtemp_min"), aes(date, main)) +
  geom_line(aes(date, comp1), col = "steelblue") +
  geom_point(aes(date, comp1), col = "steelblue") +
  geom_line(aes(date, comp2), col = "orchid") +
  geom_point(aes(date, comp2), col = "orchid") +
  geom_line(aes(date, sdlcr_qatemp), col = "goldenrod") +
  geom_point(aes(date, sdlcr_qatemp), col = "goldenrod") +
  geom_line(aes(date, d1_qatemp), col = "seagreen") +
  geom_point(aes(date, d1_qatemp), col = "seagreen") +
  geom_line() +
  geom_point()
# leave 2016 dec be, c1 chart is generally cooler than snotel and c1 logger in that month, and -30 isn't a crazy value (-22F)
# shouldn't be cooler than d1, but c1 logger nad snotel are cooler than d1 chart so could be


# to be sure, what are absolute tmax and tmin
check_grandmax <- check_extreme(working_dat)
qa_grandmax <- visual_qa(working_dat, check_grandmax, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_grandmax) # fine

check_grandmin <- check_extreme(working_dat, metric = min)
qa_grandmin <- visual_qa(working_dat, check_grandmin, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_grandmin) # also fine


# flag 2014-11-25 tmax and 2018-11-13 tmin then move on
# do they have deviance values?
check_monthly_min[as.character(check_monthly_min$date) %in% c("2014-11-25", "2018-11-13"), 30:38]
# deviates from c1 logger and/or snotel so can use comparative deviance flag
flag_mon_min1 <- subset(check_monthly_min, as.character(date) %in% c("2014-11-25", "2018-11-13"))
working_dat <- flag_temp(flag_mon_min1, error = "comparative deviance, low value")

# clean up environment
rm(check_monthly_max1, check_monthly_min, qa_mon_max1, qa_mon_min1, flag_mon_min1,
   check_grandmax, check_grandmin, qa_grandmax, qa_grandmin)



# -- QA LAG SPIKES/FLATLINES WITHIN LOGGER DATASET ----
# look for consecutive day temp spikes/declines that exceed 40C within the same metric
# also look for flatlining in temperature (jen morse said try 5-consec days)

# save copy of working dat in case mess up (so don't need to re-run from top)
working_dat_copy <- data.frame(working_dat)

## add lag temp
working_dat <- deltamet(working_dat, groupvars = c("met"))
# check distribution of absolute difference with current day's temp and yesterday's temp
boxplot(working_dat$lag1_diffmain)  
sapply(split(working_dat$lag1_diffmain, working_dat$met), function(x) tail(sort(x))) # max swings are mostly 15-16 degrees for tmax (except one 20C swing), 17-19 for tmin

# can visualize swings to be sure
swing_check1 <- subset(working_dat, lag1_diffmain >= 15) 
qa_swings1 <- visual_qa(working_dat, swing_check1, add_fourth = "sdlcr_qatemp")
# plot tmax
plot_grid(plotlist = qa_swings1[grep("airtemp_max", qa_swings1)]) # look for 2011-12-06 in delta swings, is only +5 warmer; otherwise swings follow trends at other stations
# plot tmin
plot_grid(plotlist = qa_swings1[grep("airtemp_min", qa_swings1)]) # is fine, follows trends at other stations

# already qa'd flatline, so move on to delta diff


# -- QA DAY-TO-DAY DELTA DEVIANCE -----
# diff current from lag temp in sdl chart, d1 and c1, then compare daily deltas with logger daily deltas
# pull out observations where delta deviates more than 3sd of logger-other source diff on day-to-day fluxes
working_dat_copy <- as.data.frame(working_dat)

working_dat <- deltadev(working_dat, groupvars = c("met"))
check_deltadiff1 <- subset(working_dat, flag_deltadiff1 == TRUE & flag_deltadiff2 == T & flag_deltadiff3 == T)
#View(subset(working_dat, flag_deltadiff_sdl == TRUE))
qa_lagdiffs1 <- visual_qa(working_dat, check_deltadiff1, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_lagdiffs1) #not compelling enough that I want to NA and flag anything

# bc c1 logger stops halfway thru 2018, and sdl chart also stops in 2018, try looking at snotel delta deviations
check_deltadiff2 <- subset(working_dat, flag_deltadiff2 == T)
qa_lagdiffs2 <- visual_qa(working_dat, check_deltadiff2, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_lagdiffs2[grep("airtemp_max", qa_lagdiffs2)]) # flag 2011-01-26, 2011-07-29, 2016-02-29, 2017-08-03, also check nov 2018 to look for c1 chart being off by one day
plot_grid(plotlist = qa_lagdiffs2[grep("airtemp_min", qa_lagdiffs2)]) # flag 2012-11-23 (add in)
# look at 2012-05-01, 2012-12-24, 2015-08-31, 2018-06-26.. and it does seem like something is off by a day nov-dec 2018

qa_lagdiffs2_tmin <- visual_qa(working_dat, 
                               subset(check_deltadiff2, as.character(date) %in% c("2012-05-01", "2012-12-24", "2015-08-31", "2018-06-26")), 
                               add_fourth = "kj_temp")
plot_grid(plotlist = qa_lagdiffs2_tmin)
# check deviance values
check_deltadiff2[as.character(check_deltadiff2$date) %in% c("2012-05-01", "2012-12-24", "2015-08-31", "2018-06-26"), 30:38]
# deviances would round up to 3sd.. bc of swings and deviance, flag

# flag values first, then look at nov-dec 2018
flag_deltadiff1 <- subset(check_deltadiff2, (met == "airtemp_max" & as.character(date) %in% c("2011-01-26", "2011-07-29", "2016-02-29", "2017-08-03")) |
                            (met == "airtemp_min" & as.character(date) %in% c("2012-05-01", "2012-12-24", "2015-08-31", "2018-06-26"))) %>%
  # add in 2012-11-23 tmin
  rbind(subset(working_dat, date == "2012-11-23" & met == "airtemp_min"))
# be sure these are values to NA
plot_grid(plotlist = visual_qa(working_dat, flag_deltadiff1)) #yes
# flag working dat
working_dat <- flag_temp(flag_deltadiff1, error = "day-to-day rate change +3sd outside comparative station trends")

# clean up environment
rm(check_deltadiff1, check_deltadiff2, qa_lagdiffs1, qa_lagdiffs2, qa_lagdiffs2_tmin, 
   swing_check1, qa_swings1, flag_deltadiff1)



# -- VISUAL CHECK NOV/DEC 2018 FOR 1-DAY SHIFT -----
ggplot(subset(working_dat, yr == 2018 & mon > 10), aes(date, main)) +
  geom_line(aes(date, comp2), col = "steelblue", alpha = 0.5) +
  geom_point(aes(date, comp2), col = "steelblue", alpha = 0.5) +
  geom_line(aes(date, sdlcr_qatemp), col = "goldenrod", alpha = 0.5) +
  geom_point(aes(date, sdlcr_qatemp), col = "goldenrod", alpha = 0.5) +
  geom_line(aes(date, d1_qatemp), col = "seagreen", alpha = 0.5) +
  geom_point(aes(date, d1_qatemp), col = "seagreen", alpha = 0.5) +
  geom_line() +
  geom_point() +
  facet_wrap(~met, nrow = 2) 
#idk.. looks off a bit in november for a week, there are some missing points, but not going to adjust
  


# -- RUN THROUGH DIFF DAILY ONE MORE TIME AND REVIEW QA'D DATA  ----
# save working copy
working_dat_copy <- data.frame(working_dat)

# final check
working_dat <- diff_daily(working_dat, rename = F)
par(mfrow=c(1,3))
boxplot(working_dat$deviance_1, main = "daily dev. from c1 logger")
boxplot(working_dat$deviance_2, main = "daily dev. from snotel")
boxplot(working_dat$deviance_3, main = "daily dev. from sdl chart")
par(mfrow=c(1,1))
# look at more extreme deviance in with snotel and c1 logger (since sdl chart off in 2015-2016 and stops after 2017, can ignore [not best reference])
check_diffdaily <- subset(working_dat, deviance_1 >= 5.5 | deviance_2 >= 5.5) 
qa_diffdaily <- visual_qa(working_dat, check_diffdaily, add_fourth = "sdlcr_qatemp")
plot_grid(plotlist = qa_diffdaily[grep("temp_max", qa_diffdaily)]) 
plot_grid(plotlist = qa_diffdaily[grep("temp_min", qa_diffdaily)]) 
# no tmin vals so aggregious i want to flag, some values are called out just bc tmin is still generally lower than snotel and c1 logger in adjust periods

# check these tmax dates
tmax_check <- c("2011-09-22", "2012-01-31", "2012-02-03", "2013-04-09", "2015-08-16", "2017-07-19", "2017-12-04")
plot_grid(plotlist = visual_qa(working_dat, subset(check_diffdaily, met == "airtemp_max" & as.character(date) %in% tmax_check)))
# flag all but 2012-01-31 tmax
flag_diffdaily <- subset(check_diffdaily, met == "airtemp_max" & as.character(date) %in% tmax_check[!tmax_check ==  "2012-01-31"])
# flag
working_dat <- flag_temp(flag_diffdaily)

# clean up environment
rm(qa_diffdaily, check_diffdaily, tmax_check, flag_diffdaily)



# -- REVIEW AND CLEAN UP WORKING DAT -----
# plot qa'd data for review
# start with same plot from the beginning
ggplot(working_dat, aes(date, main)) +
  #geom_line() +
  geom_point(alpha = 0.5) +
  #geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("c1 chart (black/grey) and c1 logger (purple), 2011-2018") +
  facet_grid(met~yr, scales = "free")

# what is difference btwn homogenized temp and c1 chart like in 2013 and 2014
plotdat <- working_dat %>%
  mutate(hmgT = (comp1+comp2)/2,
         diff = main - hmgT,
         # add col for coloring by adjustment period
         adjust = ifelse(date %in% tmin_per1, "adjust1",
                         ifelse(date %in% temp_per2, "adjust2",
                                ifelse(date %in% temp_per3, "adjust3", "unadjusted (raw)"))))
# save plot for robbie (using c1 temp), j. morse, katie, sarah, etc.
adjust_fig <- ggplot(plotdat, aes(date, diff, col = adjust)) +
  geom_point(alpha = 0.5) +
  geom_hline(aes(yintercept = 0), col ="red") +
  labs(y = "c1 chart T - c1 homogenized T", 
       subtitle = "Homogenized temp = mean of c1 snotel and c1 logger temps") +
  ggtitle("Final c1 chart temp relationship to c1 homogenized reference after adjustments and QA") +
  scale_color_viridis_d(name = "Adjust\nperiod") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25)) +
  facet_grid(met~yr, scales = "free") #2011 high points in tmax aren't my adjustments, raw chart was warmer for a bit and didn't touch that

ggsave("climate_d1_c1/figures/c1_adjust_qa_final.pdf", adjust_fig, scale = 1.2)

# how many points flagged?
with(working_dat, sapply(split(qa_flag, met), function(x) summary(!is.na(x)))) #about half the dataset flagged (bc of adjustments)

# what is relationship between loggers and d1 chart?
dplyr::select(working_dat, met, date, doy, yr, main, comp1) %>%
  #mutate(logger = factor(logger, levels = c("cr21x", "cr23x", "cr1000"))) %>%
  ggplot(aes(main, comp1)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0), col = "red") +
  facet_grid(met ~ ., scales = "free")



# -- COMPILE AND WRITE OUT FLAGGED/QA'D SDL CR DATASET -----
# want to write out old data with qa'd data for comparison
# maybe also write out final working_dat for documentation

c1_old_new <- left_join(c1tknwt, dplyr::select(working_dat, -c1_flag)) %>%
  dplyr::select(LTER_site:qa_flag) %>%
  rename(qa_temp = main)


# write out qa'd dataset
write_csv(c1_old_new, "climate_d1_c1/output_data/prep_data/qa_c1_temp.csv")

