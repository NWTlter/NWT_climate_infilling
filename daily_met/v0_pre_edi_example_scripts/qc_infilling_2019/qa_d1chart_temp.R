# run d1 chart through qa script


# script purpose:
# read in d1 chart from edi
# append tk d1 chart to raw d1 2011-ongoing
# run through qa script to look for usual suspects (e.g. stuck pen [flatline], extreme day to day swings, comparative deviance with nearby stations)
# write out qa'd dataset for next step (infilling missing values)


# notes:
# > only need to focus on 2010-ongoing because using tk infilled daily d1 dataset for self-study figure update

# note from H. Brandes re differences in D1 chart data over time:
# D1 hygrotherm was replaced end of 2016/beginning of 2017. I can look a little closer if you need the specific date, but that's when the chart type changed.



# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN", "NP")

# set pathway to climate output data folder (qa'd data written out to output_data/prep_data)
datpath <- "climate_d1_c1/output_data/"

# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")
# source qa functions (developed during extended summer update so lives in that folder for now..)
source("extended_summer/analysis/scripts/qa_functions.R")


# -- GET DATA -----
# d1 chart
d1 <- getTabular(412) %>% data.frame()
# c1 chart
c1 <- getTabular(411) %>% data.frame()
# keith jennings et al infilled logger dataset
kj <- getTabular(168) %>% data.frame()

# ctw qa'd datasets
## sdl logger
sdllog_qa <- read.csv("extended_summer/analysis/output_data/prep_data/qa_sdlcr_temp.csv",
                      na.strings = na_vals, strip.white = T) %>% data.frame() %>%
  mutate(date = as.Date(date)) %>%
  # need to remove overlapping dates in sdl cr23x and cr21x
  subset(!(logger == "cr23x" & date %in% seq.Date(as.Date("2000-06-24"), as.Date("2000-06-27"), 1)))
## d1 logger
d1log_qa <- read.csv(paste0(datpath, "prep_data/qa_d1cr_temp.csv"), na.strings = na_vals, strip.white = T) %>% 
  data.frame() %>% distinct() %>% # make distinct bc dates repeated due to sdl logger having overlap dates in cr23x and cr21x
  mutate(date = as.Date(date))
## c1 logger
c1log_qa <- read.csv(paste0(datpath, "prep_data/qa_c1cr_temp.csv"), na.strings = na_vals, strip.white = T) %>% 
  data.frame() %>% distinct() %>%
  mutate(date = as.Date(date))
## sdl chart
#sdl <- getTabular(413) %>% data.frame()
# ctw qa'd sdl chart temp data
sdl_qa <- read.csv("extended_summer/analysis/output_data/prep_data/qa_sdlchart_temp.csv",
                   na.strings = na_vals, strip.white = T) %>% 
  mutate(date = as.Date(date)) %>% data.frame()

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


# bring in snotel
snotel <- read_csv("climate_d1_c1/output_data/prep_data/nwt_snotel_temp.csv")


# -- REVIEW/PREP DATA -----
# check data read in as expected
glimpse(d1) # wide form
glimpse(c1) # wide form
glimpse(sdl_qa) # long
glimpse(sdllog_qa) #long
glimpse(d1log_qa) # long
glimpse(c1log_qa) # long
glimpse(tkd1)
glimpse(tkc1)

# tidy chart temp datatsets (qa'd sdl chart and sdl logger already long-form)
d1_long <- tidytemp(d1, datasource = "d1", dropcol = "airtemp_avg")
c1_long <- tidytemp(c1, datasource = "c1", dropcol = "airtemp_avg")

# prep names in ctw qa'd/prepped datasets to join with c1 cr logger datasets (i.e. no redundant colnames across datasets)
names(sdl_qa)
names(sdllog_qa)
names(d1log_qa)
names(c1log_qa)

colnames(sdl_qa)[11:12] <- c("sdl_qaflag", "sdl_daysflat")
colnames(sdllog_qa)[9:ncol(sdllog_qa)]<- c("sdlcr_temp", "sdlcr_qatemp", "sdlcr_qaflag")
colnames(d1log_qa)[11:12]<- c("d1cr_qatemp", "d1cr_qaflag")
colnames(c1log_qa)[11:12]<- c("c1cr_qatemp", "c1cr_qaflag")


# summarize jennings et al dataset to daily min/max
# min/max jennings data as extra "station" for visual qa
kjd1 <- subset(kj, local_site == "d1") %>%
  rename(doy = jday,
         yr = year) %>%
  mutate(mon = month(date)) %>%
  group_by(LTER_site, local_site, date, yr, mon, doy) %>%
  summarise(airtemp_min = min(airtemp_avg),
            airtemp_max = max(airtemp_avg)) %>%
  gather(met, kj_temp, airtemp_min, airtemp_max) %>%
  ungroup()


# replace d1 1950s to 2010 values with tk dataset, append d1 2011-ongoing (i.e. as-is dataset on EDI)
d1tknwt <- tkd1[,c(1:5,8)]
colnames(d1tknwt) <- c("mon", "day", "yr", "airtemp_max", "airtemp_min", "d1_flag")
d1tknwt <- mutate(d1tknwt, date = as.Date(paste(yr, mon, day, sep = "-")),
                  LTER_site = "NWT", 
                  local_site = "d1",
                  doy = yday(date)) %>%
  gather(met, d1_temp, airtemp_max:airtemp_min) %>%
  dplyr::select(colnames(d1_long)) %>%
  # append EDI dataset, 2011-ongoing
  rbind(subset(d1_long, yr > 2010 & yr < 2019)) # exclude jan 1 date in 2019
  
# replace c1 1950s to 2010 values with tk dataset, append EDI data 2011-ongoing
c1tknwt <- tkc1[,c(1:5,8)]
colnames(c1tknwt) <- c("mon", "day", "yr", "airtemp_max", "airtemp_min", "c1_flag")
c1tknwt <- mutate(c1tknwt, date = as.Date(paste(yr, mon, day, sep = "-")),
                  LTER_site = "NWT", 
                  local_site = "c1",
                  doy = yday(date)) %>%
  gather(met, c1_temp, airtemp_max:airtemp_min) %>%
  dplyr::select(colnames(c1_long)) %>%
  # append EDI dataset for years untreated by TK
  rbind(subset(c1_long, yr > 2010 & yr < 2019)) # exclude jan 1 date in 2019



# tidy boulder temp
bldr_long <- rename(bldr, airtemp_max = tmaxC,
                    airtemp_min = tminC) %>%
  mutate(date = paste(year, mon, day, sep = "-"),
         date = as.Date(date))
bldr_long <- tidytemp(bldr_long[c("date", "airtemp_max", "airtemp_min")], datasource = "bldr", special = NA)


# join d1 chart and all comparative temp datasets
d1_long_master <- mutate(d1tknwt, qa_flag = NA) %>% # add empty qa flag col first
  # join qa'd d1 logger
  left_join(dplyr::select(d1log_qa, -c(logger, d1cr_temp))) %>%
  # join qa'd sdl dataset, dropping raw temp col and days flat column (not needed)
  left_join(dplyr::select(sdl_qa, -c(local_site, sdl_temp, sdl_flag, sdl_daysflat))) %>%
  # join qa'd sdl logger, remove raw sdl logger temp
  left_join(dplyr::select(sdllog_qa, -c(local_site, logger, sdlcr_temp))) %>%
  # join jennings infilled daily summarized data
  left_join(kjd1) %>%
  # join c1 chart
  left_join(dplyr::select(c1tknwt, -local_site)) %>%
  # join bldr temp for reference
  left_join(bldr_long) %>%
  ungroup() %>%
  distinct() %>%
  data.frame() %>%
  mutate(mon = as.numeric(mon)) %>%
  arrange(met,date)



# -- QA SENSOR/PEN FAILS (OBVIOUS OUTLIERS) -----
# create working copy
working_dat <- d1_long_master %>%
  # exclude flatline sdl chart vals from consideration
  mutate(sdl_qatemp = ifelse(grepl("flat", sdl_qaflag), NA, sdl_qatemp)) %>%
  # subset to years post-2010 for qa
  subset(yr>2010)
# > note: CTW started to QA dataset pre-2011 (i.e. TK's data) and found a handful of values to flag 2000-2010 and there are more if go back further
# > bc purporse of this is to update TK's figure from the NWT renewal, not going to spend time QAing values in his dataset
# > use TK dataset as is, and just append best I could qa and infill in timely manner for 2011-ongoing specifically for a self-study figure update (not as rigorous as publication quality)



# look at tails for any obvious bad values
## chart temp, split by metric
with(working_dat, lapply(split(d1_temp, met), function(x) tail(sort(x), n = 20))) # highest max val is 21C, about 70F, otherwise several 19C (66F)
with(working_dat, lapply(split(d1_temp, met), function(x) head(sort(x), n = 20))) # one tmin value of -40, but could be? -40C = -40F
## chart temp, split by month
with(working_dat, lapply(split(d1_temp, paste(mon, met)), summary)) # seems okay





# -- TRIAGE SHIFT IN CHART VS. LOGGER VALUES FOR MID-2010s -----
# run diff_daily and delta lag functions to get differences and set up colnames for rest of normal QA treatment
working_dat <- diff_daily(working_dat, main = "d1_temp", comp1 = "d1cr_qatemp", comp2 = "sdl_qatemp", comp3 = "sdlcr_qatemp", 
                          groupvars = c("met", "mon"))
# lag and difference temp with reference stations
working_dat <- deltamet(working_dat)

# plot out differences to ID time period where relationship changes, and if changes for both tmax and tmin
# plot d1 chart temp by date by year to look for change in relationship with d1 logger
ggplot(working_dat, aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart (black/grey) and d1 logger (purple), 2011-2018") +
  facet_grid(met~yr, scales = "free")
# > maybe drift starts later in 2011, occurs through 2012. 2013 okay. drift against in 2014-2016. okay in 2017 onwards

# check against boulder trends (does drift occur off of those values too)?
ggplot(working_dat, aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, bldr_temp), col = "purple") +
  geom_point(aes(date, bldr_temp), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, bldr_temp), col = "orchid") +
  ggtitle("d1 chart (black/grey) and bldr temp (purple), 2011-2018") +
  facet_grid(met~yr, scales = "free") # i do see drift from boulder temp, but perhaps that station too far away to use as good reference point (e.g. mcguire paper found different temporal warming trends by elevation class)

# check against keith's data
ggplot(working_dat, aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, kj_temp), col = "purple") +
  geom_point(aes(date, kj_temp), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, kj_temp), col = "orchid") +
  ggtitle("d1 chart (black/grey) and kj d1 logger (purple), 2011-2013") +
  facet_grid(met~yr, scales = "free")

# check against snotel
ggplot(working_dat, aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(data = subset(snotel, yr %in% 2011:2018), aes(date, tempC), col = "purple") +
  geom_point(data = subset(snotel, yr %in% 2011:2018), aes(date, tempC), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(data = subset(snotel, yr %in% 2011:2018), aes(date, tempC), col = "orchid") +
  ggtitle("d1 chart (black/grey) and snotel (purple), 2011-2013") +
  facet_grid(met~yr, scales = "free")


# think best comparison is still d1 logger.. if comparing means can still use jennings dataset up thru 2013 (that it's off by a day in some places won't matter much to a mean value over a large time period)

# what is the relationship 2000-2010? (i.e. use tk treated data--logger data not perfect, and loggers switched in 2000, but gives idea of reasonable expectation)
quartz()
ggplot(subset(d1_long_master, yr %in% 2000:2010), aes(date, d1_temp)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, d1cr_qatemp), col = "purple") +
  geom_point(aes(date, d1cr_qatemp), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, d1cr_qatemp), col = "orchid") +
  ggtitle("d1 chart and d1 logger, 2000-2010") +
  facet_grid(met~yr, scales = "free") # seems to be usually pretty tight (logger dropped relative to chart in 2009)

# what does daily differences look like?
ggplot(working_dat, aes(date, main_diff_1)) +
  geom_point() +
  geom_smooth(col = "grey") +
  facet_grid(met~yr, scales = "free")

# just look at beginning and end of delta shifts
ggplot(subset(working_dat, yr %in% c(2011,2012, 2013,2016, 2017)), aes(date, main_diff_1)) +
  geom_point() +
  geom_smooth(col = "grey") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(met~yr, scales = "free")
# > looks like..
## breaks around oct 1 2011? through jan 2013 (or may 2013?)
## then maybe okay may/june 2013 until break in data (oct 2013)
## then off again oct/nov 2013 through dec 2016 (maybe jan 2017)

# plot distribution
ggplot(subset(working_dat, yr %in% c(2011,2012, 2013,2016, 2017)), aes(mon, main_diff_1, group = mon)) +
  geom_boxplot() +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(met~yr, scales = "free") # apr 2017 also looks weird
#nov 2011-apr 2013 def different, oct 2013-dec 2012

# look at full time series
ggplot(working_dat, aes(mon, main_diff_1, group = mon)) +
  geom_boxplot() +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(met~yr, scales = "free")

#what does it look like for 2000-2010 (i.e. what's normal?)
# diff tk data from kj data (note: sometimes kj daily data is off by a day, so maybe not best reference)
d1_long_master %>%
  subset(yr %in% 2000:2010) %>%
  mutate(d1_diff_kj = d1_temp - kj_temp) %>%
  ggplot(aes(mon,d1_diff_kj, group = mon)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 0), col = "red") +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(met~yr, scales = "free")
# expect d1 to be less than logger data (altho kj temp is based on AVG value (i.e. max of average hourly airtemp))

# what does it look like with d1 logger data as is (but ctw qad so extreme/comparatively deviant values removed)
quartz()
d1_long_master %>%
  subset(yr %in% 2000:2010) %>%
  mutate(d1_diff_d1cr = d1_temp - d1cr_qatemp) %>%
  ggplot(aes(mon,d1_diff_d1cr, group = mon)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 0), col = "red") +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(met~yr, scales = "free")

# what about saddle chart (maybe any error is more consistent? i.e. not sensitive to a logger instrument or its sensor?)
quartz()
d1_long_master %>%
  subset(yr %in% 2000:2010) %>%
  mutate(d1_diff_sdl = d1_temp - sdl_qatemp) %>%
  ggplot(aes(mon,d1_diff_sdl, group = mon)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 0), col = "red") +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(met~yr, scales = "free") # don't expect d1 to be warmer than the sdl.. gets warmer in summer months for tmin in last 4 years..


# take a closer look at windows of temp shift
# fall 2011
ggplot(subset(working_dat, yr == 2011 & mon %in% 9:11), aes(date, main_diff_1)) +
  geom_point() +
  geom_smooth(col = "grey") +
  scale_x_date(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(met~yr, scales = "free")

#monthly averages before change point
d1_long_master %>%
  subset(yr %in% 2004:2018) %>%
  filter(!is.na(d1_temp)) %>%
  group_by(met, yr) %>%
  summarize(mean_temp = mean(d1_temp, na.rm = T),
            se = sd(d1_temp, na.rm = T)/sqrt(length(d1_temp)),
            nobs = length(d1_temp)) %>%
  ggplot(aes(yr, mean_temp)) +
  #geom_line(aes(group = yr)) +
  geom_line() +
  geom_errorbar(aes(ymax = mean_temp + se, ymin = mean_temp - se), width = 0) +
  geom_point() +
  scale_x_continuous(breaks = 2004:2018) +
  facet_grid(met~., scales = "free")

# convert to mean temp
d1tknwt %>%
  #subset(yr %in% 2004:2018) %>%
  spread(met, d1_temp) %>%
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
# note: cold spell in 1980s is real. dips post-2010 due to chart shifts

# diurnal range
d1tknwt %>%
  #subset(yr %in% 1950:2018) %>%
  spread(met, d1_temp) %>%
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
# DTR 2011-ongoing doesn't look affected by shifts in chart relative to the rest of the record
# there are some wild swings in 1980s and final years of TK dataset ...


# look at variance over time
d1_long_master %>%
  subset(yr %in% 1990:2018) %>%
  filter(!is.na(d1_temp)) %>%
  group_by(met, yr) %>%
  summarize(sd_temp = sd(d1_temp, na.rm = T)) %>%
  ggplot(aes(yr, sd_temp)) +
  # demarcate where tk-infilled dataset stops
  geom_vline(aes(xintercept = 2010), col = "chocolate2", lwd =2, alpha = 0.6) + 
  # demarcate yrs where chart drops
  geom_vline(aes(xintercept = 2012), col = "dodgerblue", alpha = 0.6) + 
  geom_vline(aes(xintercept = 2014), col = "dodgerblue", alpha = 0.6) + 
  geom_vline(aes(xintercept = 2017), col = "dodgerblue", alpha = 0.6) + 
  #geom_line(aes(group = yr)) +
  geom_line() +
  #geom_errorbar(aes(ymax = mean_temp + se, ymin = mean_temp - se), width = 0) +
  #geom_point() +
  scale_x_continuous(breaks = seq(1990,2020, 5)) +
  facet_grid(met~., scales = "free")
# 2013 spikes bc its summertime values are good, but other months are low
# looks like temporal mean is probably somewhere around 9-10 degrees in both tmax and tmin


# look at actual temp
ggplot(subset(working_dat, yr == 2011 & mon %in% 9:11), aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, fall 2011") +
  scale_x_date(date_breaks = "1 week") +
  facet_grid(met~yr, scales = "free") # looks like break happens on nov 4, 2011


# spring 2013
ggplot(subset(working_dat, yr == 2013 & mon %in% 3:9), aes(date, main_diff_1)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  scale_x_date(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90)) + # looks like fall shift occurred around sep 13-- it was the flood! sep 11-13 2013. maybe storms affected instrumentation?? could also be coincidence
  facet_grid(met~., scales = "free")

# look at spring 2013 shift, actual temp
ggplot(subset(working_dat, yr == 2013 & mon %in% 3:6), aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  geom_point(aes(date, kj_temp), col = "goldenrod", alpha = 0.5) +
  geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, spring 2013") +
  scale_x_date(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(met~yr, scales = "free") # doesn't look that bad actually, just off a little by a day here and there
# > maybe spring shifts ends around 4/8/2013?

# look at winter 2012
ggplot(subset(working_dat, (yr == 2012 & mon >9) | (yr == 2013 & mon < 4)), aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  # add keith's data since missing in edi dataset
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  geom_point(aes(date, kj_temp), col = "goldenrod", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  scale_x_date(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("d1 chart and d1 logger, winter 2012") +
  facet_grid(met~., scales = "free") # looks like it changes on mar 14 2013.. goes back to more typical relationship

# look at fall 2013 shift, actual temp
ggplot(subset(working_dat, yr == 2013 & mon %in% 9:12), aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  # add keith's data
  geom_line(aes(date, kj_temp), col = "goldenrod") +
  geom_point(aes(date, kj_temp), col = "goldenrod", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  scale_x_date(date_breaks = "1 week") +
  ggtitle("d1 chart and d1 logger, fall 2013") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(met~yr, scales = "free") # maybe in this case d1 chart was getting slowly worse from sep to dec (gaps btwn chart and logger increasingly widen over time, don't see any tidy breaks)
# if had to choose a start date for adjusting.. i guess weirdness starts around oct 4? could also start it on 12/12


# winter 2016-jan 2017
ggplot(subset(working_dat, (yr == 2016 & mon %in% 9:12) | yr == 2017 & mon < 3), aes(date, main_diff_1, col = met)) +
  geom_line() +
  geom_point() +
  #geom_smooth() +
  scale_x_date(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90)) # jumps down on jan 2 2017

# what were the actual temps?
ggplot(subset(working_dat, (yr == 2016 & mon %in% 12) | (yr == 2017 & mon < 3)), aes(date, main)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple", alpha = 0.5) +
  geom_smooth(col = "grey") +
  geom_smooth(aes(date, comp1), col = "orchid") +
  ggtitle("d1 chart and d1 logger, winter 2016") +
  facet_grid(met~yr, scales = "free") # looks more like a qualitative change occurred on jan 4, 2017


# conclusions:
# first break period to adjust is 2011-11-04 to 2013-03-13
# second break period to adjust is 2013-12-12 to 2017-01-03


# one last check on diurnal temp by month
d1tknwt %>%
  subset(yr %in% 2008:2018) %>%
  spread(met, d1_temp) %>%
  mutate(DTR = airtemp_max-airtemp_min) %>%
  filter(!is.na(DTR)) %>%
  group_by(yr, mon) %>%
  summarize(mean_DTR = mean(DTR, na.rm = T),
            se = sd(DTR, na.rm = T)/sqrt(length(DTR)),
            nobs = length(DTR)) %>%
  ggplot(aes(yr, mean_DTR)) +
  # demarcate where tk-infilled dataset stops
  geom_vline(aes(xintercept = 2010), col = "chocolate2", lwd =2, alpha = 0.6) + 
  geom_errorbar(aes(ymax = mean_DTR + se, ymin = mean_DTR - se), width = 0) +
  geom_point() +
  scale_x_continuous(breaks = seq(2008, 2020, 4)) +
  facet_wrap(~mon)


# one last check on mean temp by month
d1tknwt %>%
  subset(yr %in% 2008:2018) %>%
  spread(met, d1_temp) %>%
  mutate(airtemp_avg = (airtemp_min+airtemp_max)*0.5) %>%
  group_by(yr,mon) %>%
  summarize(mean_temp = mean(airtemp_avg, na.rm = T),
            se = sd(airtemp_avg, na.rm = T)/sqrt(length(!is.na(airtemp_avg))),
            nobs = length(airtemp_avg)) %>%
  ggplot(aes(yr, mean_temp)) +
  #geom_line(aes(group = yr)) +
  # demarcate where tk-infilled dataset stops
  geom_vline(aes(xintercept = 2010), col = "chocolate2", lwd =2, alpha = 0.6) + 
  geom_point() +
  geom_errorbar(aes(ymax = mean_temp + se, ymin = mean_temp - se), width = 0) +
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  facet_wrap(~mon, scales = "free")

d1tknwt %>%
  subset(yr %in% 2000:2018) %>%
  group_by(met, yr,mon) %>%
  summarize(mean_temp = mean(d1_temp, na.rm = T),
            se = sd(d1_temp, na.rm = T)/sqrt(length(!is.na(d1_temp))),
            nobs = length(d1_temp)) %>%
  ggplot(aes(yr, mean_temp)) +
  #geom_line(aes(group = yr)) +
  # demarcate where tk-infilled dataset stops
  geom_vline(aes(xintercept = 2010), col = "chocolate2", lwd =2, alpha = 0.6) + 
  geom_point() +
  geom_errorbar(aes(ymax = mean_temp + se, ymin = mean_temp - se), width = 0) +
  ggtitle("affected periods = nov 2011-may 2013, oct 2013-jan 2017") +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  facet_wrap(~mon+met, scales = "free")


# -- ADJUST BREAK PERIODS BY MEAN ADJUSTMENT -----
# only tmin/max values need to be shifted up, preserve the diurnal range (which looks fine) 
# compare average of good periods to affected periods.. temp warms in tk's dataset from 2008-2010 so something to be mindful of

# specify periods that need adjustment
per1 <- seq.Date(as.Date("2011-11-04"), as.Date("2013-03-13"), 1)
per2 <- seq.Date(as.Date("2013-12-12"), as.Date("2017-01-03"), 1)
goodyr <- d1tknwt %>%
  filter(yr > 2010) %>%
  mutate(good = ifelse(!date %in% c(per1, per2), "good", "bad"),
         season = ifelse(mon %in% c(12, 1:2), "wnt",
                         ifelse(mon %in% c(3:5), "spr",
                                ifelse(mon %in% c(6:8), "sum", "fal")))) %>%
  group_by(good, met) %>%
  mutate(mean = mean(d1_temp, na.rm = T)) %>%
  ungroup() %>%
  group_by(good, met, season) %>%
  mutate(seasonal = mean(d1_temp, na.rm =T)) %>%
  dplyr::select(LTER_site, local_site, met, good, season, mean, seasonal) %>%
  ungroup() %>%
  distinct() %>%
  spread(season, seasonal) %>%
  rename(yr = `mean`) %>%
  gather(period, mean_temp, yr:wnt) %>%
  spread(good, mean_temp) %>%
  group_by(met, period) %>%
  mutate(diff = good-bad) %>%
  ungroup() %>%
  mutate(period = factor(period, level = c("wnt", "spr", "sum", "fal", "yr")))
  
ggplot(goodyr, aes(period, diff)) +
  #geom_line(aes(group=paste(met,season))) +
  geom_point(aes(col = met))
# what are the means (overall and by metric)?
mean(goodyr$diff[goodyr$period != "yr"])
mean(goodyr$diff[goodyr$period != "yr" & goodyr$met == "airtemp_max"])
mean(goodyr$diff[goodyr$period != "yr" & goodyr$met == "airtemp_min"])


# compare to qa'd data logger in the same time period
# use kj data for 2010-2013
d1cr_yr <- subset(kjd1, yr > 2010) %>%
  as.data.frame() %>%
  rename(d1cr_qatemp = kj_temp) %>%
  rbind(data.frame(d1log_qa[d1log_qa$yr > 2013, c("LTER_site", "local_site", "date","yr","mon","doy","met", "d1cr_qatemp")])) %>%
  mutate(good = ifelse(!date %in% c(per1, per2), "good", "bad"),
         season = ifelse(mon %in% c(12, 1:2), "wnt",
                         ifelse(mon %in% c(3:5), "spr",
                                ifelse(mon %in% c(6:8), "sum", "fal")))) %>%
  group_by(good, met) %>%
  mutate(mean = mean(d1cr_qatemp, na.rm = T)) %>%
  ungroup() %>%
  group_by(good, met, season) %>%
  mutate(seasonal = mean(d1cr_qatemp, na.rm =T)) %>%
  dplyr::select(LTER_site, local_site, met, good, season, mean, seasonal) %>%
  ungroup() %>%
  distinct() %>%
  spread(season, seasonal) %>%
  rename(yr = `mean`) %>%
  gather(period, mean_temp, yr:wnt) %>%
  spread(good, mean_temp) %>%
  group_by(met, period) %>%
  mutate(diff = good-bad) %>%
  ungroup() %>%
  mutate(period = factor(period, level = c("wnt", "spr", "sum", "fal", "yr")))

ggplot(d1cr_yr, aes(period, diff)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(col = met))
  
# compare monthly means
goodmon <- d1tknwt %>%
  filter(yr > 2010) %>%
  mutate(good = ifelse(!date %in% c(per1, per2), "good", "bad")) %>%
  group_by(good, met) %>%
  mutate(mean = mean(d1_temp, na.rm = T)) %>%
  ungroup() %>%
  group_by(good, met, mon) %>%
  mutate(monmean = mean(d1_temp, na.rm =T)) %>%
  dplyr::select(LTER_site, local_site, met, good, mon, mean, monmean) %>%
  ungroup() %>%
  distinct() %>%
  spread(mon, monmean) %>%
  rename(yr = `mean`) %>%
  gather(period, mean_temp, yr:ncol(.)) %>%
  spread(good, mean_temp) %>%
  group_by(met, period) %>%
  mutate(diff = good-bad) %>%
  ungroup() %>%
  mutate(period = factor(period, levels = c("yr", 1:12)))

chrtmon <- ggplot(goodmon, aes(period, diff, col = met, group = met)) +
  geom_line() +
  geom_point()

# repeat for d1 logger
# be sure only averaging dates with data available in d1 chart so comparing similar means
## grab tmin dates that aren't NA in chart dataset
tmindates <- d1tknwt$date[!is.na(d1tknwt$d1_temp) & d1tknwt$met == "airtemp_min" & d1tknwt$yr > 2010]
tmaxdates <- d1tknwt$date[!is.na(d1tknwt$d1_temp) & d1tknwt$met == "airtemp_max" & d1tknwt$yr > 2010]
  
d1cr_mon <- subset(kjd1, yr > 2010) %>%
  as.data.frame() %>%
  rename(d1cr_qatemp = kj_temp) %>%
  rbind(data.frame(d1log_qa[d1log_qa$yr > 2013, c("LTER_site", "local_site", "date","yr","mon","doy","met", "d1cr_qatemp")])) %>%
  mutate(good = ifelse(!date %in% c(per1, per2), "good", "bad")) %>%
  filter((met == "airtemp_max" & date %in% tmaxdates) | met == "airtemp_min" & date %in% tmindates) %>%
  group_by(good, met) %>%
  mutate(mean = mean(d1cr_qatemp, na.rm = T)) %>%
  ungroup() %>%
  group_by(good, met, mon) %>%
  mutate(meanmon = mean(d1cr_qatemp, na.rm =T)) %>%
  dplyr::select(LTER_site, local_site, met, good, mon, mean, meanmon) %>%
  ungroup() %>%
  distinct() %>%
  spread(mon, meanmon) %>%
  rename(yr = `mean`) %>%
  gather(period, mean_temp, yr:ncol(.)) %>%
  spread(good, mean_temp) %>%
  group_by(met, period) %>%
  mutate(diff = good-bad) %>%
  ungroup() %>%
  mutate(period = factor(period, level = c("yr", 1:12)))

d1mon <- ggplot(d1cr_mon, aes(period, diff, col = met, group = met)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  geom_point() +
  labs(y = "diff (good - off)")

plot_grid(chrtmon, d1mon, nrow = 2)

# test adjusting differences by mean of seasonal differences
adjust <- round(mean(goodyr$diff[goodyr$period != "yr" & goodyr$met == "airtemp_max"])) # mean of tmax diff is smaller than overall mean, so feel better about more conservative adjustment (i.e. don't want a warming trend that's a result of a higher adjustment number) 
test <- ggplot(goodmon, aes(period, diff-adjust, col = met, group = met)) +
  geom_hline(aes(yintercept = 0))+
  geom_line() +
  geom_point()

plot_grid(chrtmon, d1mon, test, nrow = 3) # looks more reasonable than data as they are.. may be way to press fwd for now


# test if adding 4 to all values shifts means as expected
testdf <- d1tknwt %>%
  mutate(test_temp = ifelse(date %in% c(per1, per2), d1_temp + adjust, d1_temp))

testadj <- testdf %>%
  filter(yr > 2010) %>%
  mutate(good = ifelse(!date %in% c(per1, per2), "good", "bad")) %>%
  group_by(good, met) %>%
  mutate(mean = mean(test_temp, na.rm = T)) %>%
  ungroup() %>%
  group_by(good, met, mon) %>%
  mutate(meanmon = mean(test_temp, na.rm =T)) %>%
  dplyr::select(LTER_site, local_site, met, good, mon, mean, meanmon) %>%
  ungroup() %>%
  distinct() %>%
  spread(mon, meanmon) %>%
  rename(yr = `mean`) %>%
  gather(period, mean_temp, yr:ncol(.)) %>%
  spread(good, mean_temp) %>%
  group_by(met, period) %>%
  mutate(diff = good-bad) %>%
  ungroup() %>%
  mutate(period = factor(period, level = c("yr", 1:12))) %>%
  ggplot(aes(period, diff)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(col = met))

plot_grid(chrtmon, d1mon, test, testadj, nrow = 4) #adjustment worked out as expected

#plot temp over time to see how it looks
ggplot(subset(testdf, yr > 1999), aes(date, test_temp)) +
  geom_point(aes(col=date %in% c(per1,per2))) +
  #geom_point() +
  #geom_point(aes(date, d1_temp), col = "orchid", alpha = 0.5) +
  facet_grid(~met, scales = "free")

# plot with keith's infilled data to check adjustment in beginning of first break
ggplot(subset(testdf, yr == 2011 & mon > 9), aes(date, test_temp)) +
  geom_line() +
  geom_point(aes(col=date %in% c(per1,per2))) +
  #geom_point() +
  #geom_point(aes(date, d1_temp), col = "orchid", alpha = 0.5) +
  geom_line(data = subset(kjd1, yr == 2011 & mon > 9), aes(date, kj_temp), col = "goldenrod") +
  geom_point(data = subset(kjd1, yr == 2011 & mon > 9), aes(date, kj_temp), col = "goldenrod") +
  facet_grid(~met, scales = "free")
# looks reasonable

# check end of first break period
ggplot(subset(testdf, yr == 2013 & mon %in% 2:4), aes(date, test_temp)) +
  geom_line() +
  geom_point(aes(col=date %in% c(per1,per2))) +
  #geom_point() +
  #geom_point(aes(date, d1_temp), col = "orchid", alpha = 0.5) +
  geom_line(data = subset(kjd1, yr == 2013 & mon %in% 2:4), aes(date, kj_temp), col = "goldenrod", alpha = 0.5) +
  geom_point(data = subset(kjd1, yr == 2013 & mon %in% 2:4), aes(date, kj_temp), col = "goldenrod", alpha = 0.5) +
  facet_grid(~met, scales = "free")


ggplot(subset(testdf, yr == 2012), aes(date, test_temp)) +
  geom_line() +
  geom_point(aes(col=date %in% c(per1,per2))) +
  geom_smooth(color = "black") +
  #geom_point() +
  #geom_point(aes(date, d1_temp), col = "orchid", alpha = 0.5) +
  geom_line(data = subset(kjd1, yr == 2012), aes(date, kj_temp), col = "goldenrod", alpha = 0.5) +
  geom_point(data = subset(kjd1, yr == 2012), aes(date, kj_temp), col = "goldenrod", alpha = 0.5) +
  geom_smooth(data = subset(kjd1, yr == 2012), aes(date, kj_temp), col = "yellow", alpha = 0.5) +
  facet_grid(~met, scales = "free")

# check beginning of second break period
ggplot(subset(testdf, yr == 2013 & mon >9), aes(date, test_temp)) +
  geom_line() +
  geom_point(aes(col=date %in% c(per1,per2))) +
  #geom_point() +
  #geom_point(aes(date, d1_temp), col = "orchid", alpha = 0.5) +
  geom_line(data = subset(kjd1, yr == 2013 & mon > 9), aes(date, kj_temp), col = "goldenrod", alpha = 0.5) +
  geom_point(data = subset(kjd1, yr == 2013 & mon > 9), aes(date, kj_temp), col = "goldenrod", alpha = 0.5) +
  facet_grid(~met, scales = "free")

# check beginning of second break period
ggplot(subset(testdf, (yr == 2016 & mon > 10) | (yr == 2017 & mon < 3)), aes(date, test_temp)) +
  geom_line() +
  geom_point(aes(col=date %in% c(per1,per2))) +
  #geom_point() +
  #geom_point(aes(date, d1_temp), col = "orchid", alpha = 0.5) +
  geom_line(data = subset(d1log_qa, (yr == 2016 & mon > 10) | (yr == 2017 & mon < 3)), aes(date, d1cr_qatemp), col = "goldenrod", alpha = 0.5) +
  geom_point(data = subset(d1log_qa, (yr == 2016 & mon > 10) | (yr == 2017 & mon < 3)), aes(date, d1cr_qatemp), col = "goldenrod", alpha = 0.5) +
  facet_grid(~met, scales = "free")

# tmax is where it should be, but tmin chart colder than d1 logger (which shouldn't really happen, logger more sensitive to tmin)
# if adjust tmin separately, lose the diurnal temp data, which looked fine visually

# look at 2016
ggplot(subset(testdf, yr == 2016), aes(date, test_temp)) +
  geom_line() +
  geom_point(aes(col=date %in% c(per1,per2))) +
  geom_smooth(col = "black") +
  # add smooth line for chart data
  geom_smooth(aes(date, d1_temp), col = "dodgerblue", alpha = 0.5) +
  #geom_point(aes(date, d1_temp), col = "orchid", alpha = 0.5) +
  geom_line(data = subset(d1log_qa, yr == 2016), aes(date, d1cr_qatemp), col = "goldenrod", alpha = 0.5) +
  geom_point(data = subset(d1log_qa, yr == 2016), aes(date, d1cr_qatemp), col = "goldenrod", alpha = 0.5) +
  geom_smooth(data = subset(d1log_qa, yr == 2016), aes(date, d1cr_qatemp), col = "yellow", alpha = 0.5) +
  facet_grid(~met, scales = "free")

# > it could be that chart affected seasonally.. but for sake of time, not going to get into that. if we want a good dataset to publish tho, should look into seasonal differences more


# plot all yrs to compare
ggplot(subset(testdf, yr > 2010), aes(date, test_temp)) +
  #geom_line() +
  #geom_point(aes(col=date %in% c(per1,per2))) +
  geom_smooth(col = "black") +
  #geom_point() +
  geom_smooth(aes(date, d1_temp), col = "dodgerblue", alpha = 0.5) +
  #geom_line(data = subset(kjd1, yr > 2010), aes(date, kj_temp), col = "goldenrod", alpha = 0.5) +
  #geom_point(data = subset(d1log_qa, yr > 2010), aes(date, d1cr_qatemp), col = "purple", alpha = 0.5) +
  geom_smooth(data = subset(d1log_qa, yr > 2010), aes(date, d1cr_qatemp), col = "orchid", alpha = 0.5) +
  ggtitle("black = adjusted values, blue = raw values, purple = qa'd d1 logger") +
  facet_grid(met~yr, scales = "free")

# > overall looks better.. good enough for a self-study figure update


# apply the correction to the working dataset and add a flag note
working_dat <- working_dat %>%
  mutate(main = ifelse(date %in% c(per1, per2), main + adjust, main),
         qa_flag = ifelse(date %in% c(per1, per2) & !is.na(main), "temp shifted 4C warmer, chart dropped artificially", NA))


# move on to regular qa!



# -- FLAG DAILY DEPARTURES FROM COMPARATIVE DATASETS -----
# round 1 diff daily and flagging
working_dat <- diff_daily(working_dat, rename = FALSE, groupvars = c("met", "mon"))  
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff1 <- filter(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff1 <- visual_qa(working_dat, check_daily_diff1, add_fourth = "kj_temp") # add jennings infilled logger as fourth comparison
# visualize
plot_grid(plotlist = qa_daily_diff1)
# > 2012 looks like pen got stuck on 2012-01-10, dates preceeding off by one day

# look at jan-mar 2012
ggplot(subset(working_dat, (yr == 2012 & mon < 3) | (yr == 2011 & mon > 10)), aes(date, main)) +
  geom_line() +
  geom_point() +
  # plot shifted fwd
  #geom_line(aes(date+1, main), col = "chocolate2") +
  #geom_point(aes(date+1, main), col ="chocolate2") +
  #plot with d1 logger
  geom_line(aes(date, comp1), col = "purple") +
  geom_point(aes(date, comp1), col = "purple") +
  # and sdl chart
  #geom_line(aes(date, comp2), col = "steelblue2") +
  #geom_point(aes(date, comp2), col = "steelblue2") +
  # and jennings data chart
  #geom_line(aes(date, kj_temp), col = "goldenrod") +
  #geom_point(aes(date, kj_temp), col = "goldenrod") +
  scale_x_date(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~met, nrow = 2) # not sure.. leave be, because it could also be fine.. see places where day to shift off by 1 day, but matches on others

# maybe run flatline check first before diff daily test...
## add lag temp
working_dat <- deltamet(working_dat)
# look for 3 or more consecutive days of 0 change
working_dat <- check_flatline(working_dat, ctdays = 2)
flat_check <- subset(working_dat, !is.na(flatline))
qa_flatline <- visual_qa(working_dat, flat_check, add_fourth = "c1_temp")
plot_grid(plotlist = qa_flatline) 
# > flag april 2017 tmin and tmax dates, march 2018 + 3/27/19
# can give warnings or acknowledgment of everything else (i.e. there is a flatline, but better to use than infilling/roughly follows other trends at nearby stations)
flag_flatline <- subset(flat_check, date == "2017-04-08" | date == "2018-03-19")
warn_flatline <- anti_join(flat_check, flag_flatline)

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
  # check is flags already exist (e.g. shifted temp)
  existing_flag <- working_dat$qa_flag[working_dat$date %in% seq.Date(start_date, end_date,1) & working_dat$met == temp_df$met]
  # if no flags, assign message as is
  if(sum(!is.na(existing_flag)) >0){
  working_dat$qa_flag[working_dat$date %in% seq.Date(start_date, end_date,1) & working_dat$met == temp_df$met] <- "advisory: 4+ day flatline, roughly follows nearby stations"
  }else{
    # append flag to existing flag
    working_dat$qa_flag[working_dat$date %in% seq.Date(start_date, end_date,1) & working_dat$met == temp_df$met] <- paste(existing_flag, "advisory: 4+ day flatline, roughly follows nearby stations", sep = "; ")
  }
}

# add note on 3/27/18 that sensor was stuck
# check if flagged for anything..
data.frame(working_dat[working_dat$date == "2018-03-27" & working_dat$met == "airtemp_min",]) #sd dev comp1 rounds to 4, sd deviance for 3rd source rounds to 3 (2nd missing data)
working_dat$main[working_dat$date == "2018-03-27" & working_dat$met == "airtemp_min"] <- NA
working_dat$qa_flag[working_dat$date == "2018-03-27" & working_dat$met == "airtemp_min"] <- "pen stuck in mid-march, high value"


# clean up environment
rm(flat_check, qa_flatline, flag_flatline, warn_flatline)


# continue with diff daily checks..
# run diff daily again so flatline errors not plotted
working_dat <- diff_daily(working_dat, rename = F,groupvars = c("met", "mon"))  
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff1 <- filter(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff1 <- visual_qa(working_dat, check_daily_diff1, add_fourth = "c1_temp")
# visualize
plot_grid(plotlist = qa_daily_diff1)

# look again at jan 2012
ggplot(subset(working_dat, yr == 2012 & mon == 1), aes(date, main)) +
  geom_line() +
  geom_line(aes(date, comp1), col = "orchid") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  geom_line(aes(date, comp3), col = "forestgreen") +
  geom_line(aes(date, c1_temp), col = "goldenrod") +
  scale_x_date(date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~met, nrow= 2) # i would rather shift jan 7 to stopping point 1 day forward than remove values
# i.e. jan 7 for tmin and tmax will have NA value, but next several day will have values that match (plus 1/14 will have value, not be NA)
shift_seq <- seq.Date(as.Date("2012-01-07"), as.Date("2012-01-13"), 1)

# shift temp values for period in between back one day
#iterate by metric
for(m in c("airtemp_max", "airtemp_min")){
  tempvals <- working_dat$main[working_dat$date %in% shift_seq & working_dat$met == m]
  working_dat$main[working_dat$date %in% (shift_seq+1) & working_dat$met == m] <- tempvals
  working_dat$qa_flag[working_dat$date %in% (shift_seq+1) & working_dat$met == m] <- "temp shifted forward 1 day (corrected)"
  # NA first date and add note
  working_dat$main[working_dat$date == shift_seq[1] & working_dat$met ==m] <- NA
  working_dat$qa_flag[working_dat$date == shift_seq[1] & working_dat$met ==m] <- "temp shifted forward 1 day (corrected)"
}

# check that shift applied as expected
ggplot(subset(working_dat, yr == 2012 & mon == 1), aes(date, main)) +
  geom_line() +
  geom_line(aes(date, comp1), col = "orchid") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  geom_line(aes(date, comp3), col = "forestgreen") +
  geom_line(aes(date, c1_temp), col = "goldenrod") +
  scale_x_date(date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~met, nrow= 2) # yes, shift applied correctly

# run diff daily again
working_dat <- 
flag_dailydiff1 <- subset(check_daily_diff1, met == "airtemp_min" & as.character(date) %in% c("2012-02-03", "2016-06-04"))

# flag and remove values in working dataset
working_dat <- diff_daily(working_dat, rename = F,groupvars = c("met", "mon"))  
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff1 <- filter(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff1 <- visual_qa(working_dat, check_daily_diff1, add_fourth = "c1_temp")
# visualize
plot_grid(plotlist = qa_daily_diff1)

# look at tmax sep 2011, looks like data off by a day where error occurs
ggplot(subset(working_dat, yr == 2011 & mon %in% 9:10), aes(date, main)) +
  geom_line() +
  geom_line(aes(date, comp1), col = "orchid") +
  geom_line(aes(date, comp2), col = "steelblue2") +
  geom_line(aes(date, comp3), col = "forestgreen") +
  geom_line(aes(date, c1_temp), col = "goldenrod") +
  scale_x_date(date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~met, nrow= 2) # idk.. leave it be, no clear culprits, but certainly several day stretch where things look off by a day

# flag all values except sep 2011 since it seems like there's a day-shift issue there
# also flag tmin 3/19 and 3/20/2012 since it looks like it flatlined and all of those values are low

flag_dailydiff1 <- subset(check_daily_diff1, date != "2011-09-19") %>%
  # append tmin 3/19 and 3/20
  rbind(subset(working_dat, met == "airtemp_min" & date %in% c(as.Date("2012-03-19"), as.Date("2012-03-20"))))

working_dat <- flag_temp(flag_dailydiff1)

# check second round of diff daily flag
working_dat <- diff_daily(working_dat, rename = FALSE, groupvars = c("met", "mon"))
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff2 <- filter(working_dat, flag_diff1 == T & flag_diff2 == T & flag_diff3 == T)
# run through visual qa
qa_daily_diff2 <- visual_qa(working_dat, check_daily_diff2, add_fourth = "c1_temp")
# visualize
plot_grid(plotlist = qa_daily_diff2) # leave things be

# clean up environment
rm(check_daily_diff1, check_daily_diff2, flag_dailydiff1, qa_daily_diff1, qa_daily_diff2)



# -- QA EXTREMES (TMIN/TMAX) -----
# visual qa grand max and min
#try max temp by yr
check_max1 <- check_extreme(working_dat, groupvars = c("met", "yr"), max)
qa_max1 <- visual_qa(working_dat, check_max1, add_fourth = "kj_temp")
plot_grid(plotlist = qa_max1[grep("airtemp_max", qa_max1)])  # flag june 2012 and aug 2013
plot_grid(plotlist = qa_max1[grep("airtemp_min", qa_max1)]) # flag july 2014

# >round 1: 
# tmax in 1982-06-28 looks like too big of a jump in comparison to d1 and c1 behavior, all else okay
flag_max1 <- filter(check_max1, (met == "airtemp_max" & yr %in% c(2012, 2013)) | (met == "airtemp_min" &  yr ==2014))
working_dat <- flag_temp(flag_max1, error = "high value")

# > round 2:
# run through grand max once more
check_max2 <- check_extreme(working_dat, groupvars = c("met", "yr"))
qa_max2 <- visual_qa(working_dat, check_max2)
plot_grid(plotlist = qa_max2[grep("airtemp_max", qa_max2)]) # looks good
plot_grid(plotlist = qa_max2[grep("airtemp_min", qa_max2)]) # okay. july 2014 looks off by a day, but leaving be. doesn't affect max val

# min temps by year
check_min1 <- check_extreme(working_dat, groupvars = c("met", "yr"), metric=min)
qa_min1 <- visual_qa(working_dat, check_min1)
plot_grid(plotlist = qa_min1[grep("airtemp_max", qa_min1)]) # looks okay
plot_grid(plotlist = qa_min1[grep("airtemp_min", qa_min1)]) # it looks a little low for 2013 and 2016, but leaving be

# move on to monthlies...
#clean up env
rm(check_max1, check_max2, qa_max1, qa_max2,
   check_min1, qa_min1, flag_max1)



# -- QA MONTHLY MIN/MAX -----
# monthly max temps of tmin and tmax
check_monthly_max1 <- check_extreme(working_dat, c("met", "mon"))
# run through visual qa function
qa_mon_max1 <- visual_qa(working_dat, check_monthly_max1, sorttime = "mon", add_fourth = "c1_temp")

# visualize logger monthly maximums, by logger and metric
## max of airtemp_max
plot_grid(plotlist = qa_mon_max1[grep("airtemp_max", qa_mon_max1)]) # looks good
## max of airtemp_min
plot_grid(plotlist = qa_mon_max1[grep("airtemp_min", qa_mon_max1)]) # july 2011 values look high but those aren't adjusted (i.e. period that is okay), flag may 2018 -- looks like spike

# flag values
flag_mon_max1 <- subset(check_monthly_max1, met == "airtemp_min" & mon == 5) # for the two comparative data sources that are available, both throw flags so can use comparative deviance flag
working_dat <- flag_temp(flag_mon_max1)



# monthly min temps of tmin and tmax
check_monthly_min <- check_extreme(working_dat, c("met", "mon"), min)
# run through visual qa function
qa_mon_min1 <- visual_qa(working_dat, check_monthly_min, sorttime = "mon", add_fourth = "c1_temp")

# visualize logger monthly minimums, by logger and metric
## min of airtemp_max
plot_grid(plotlist = qa_mon_min1[grep("airtemp_max", qa_mon_min1)]) #okay
## min of airtemp_min
plot_grid(plotlist = qa_mon_min1[grep("airtemp_min", qa_mon_min1)]) #okay


# clean up environment and move on..
rm(flag_mon_max1, check_monthly_max1, check_monthly_min,
   qa_mon_max1, qa_mon_min1)



# -- QA LAG SPIKES/FLATLINES WITHIN LOGGER DATASET ----
# look for consecutive day temp spikes/declines that exceed 40C within the same metric
# also look for flatlining in temperature (jen morse said try 5-consec days)

# save copy of working dat in case mess up (so don't need to re-run from top)
working_dat_copy <- as.data.frame(working_dat)

## add lag temp
working_dat <- deltamet(working_dat, groupvars = c("met"))
# check distribution of absolute difference with current day's temp and yesterday's temp
boxplot(working_dat$lag1_diffmain)  
sapply(split(working_dat$lag1_diffmain, working_dat$met), function(x) tail(sort(x))) # max swings are mostly 15-16 degrees for tmax (except one 20C swing), 17-19 for tmin

# can visualize swings to be sure
swing_check1 <- subset(working_dat, lag1_diffmain >= 16) 
qa_swings1 <- visual_qa(working_dat, swing_check1, add_fourth = "c1_temp")
plot_grid(plotlist = qa_swings1) # there are some big swings.. see if reappear in delta diff flagging (below)

# already qa'd flatline, so move on to delta diff


# -- QA DAY-TO-DAY DELTA DEVIANCE -----
# diff current from lag temp in sdl chart, d1 and c1, then compare daily deltas with logger daily deltas
# pull out observations where delta deviates more than 3sd of logger-other source diff on day-to-day fluxes
working_dat_copy <- as.data.frame(working_dat)

working_dat <- deltadev(working_dat, groupvars = c("met"))
check_deltadiff1 <- subset(working_dat, flag_deltadiff1 == TRUE & flag_deltadiff2 == T & flag_deltadiff3 == T)
#View(subset(working_dat, flag_deltadiff_sdl == TRUE))
qa_lagdiffs1 <- visual_qa(working_dat, check_deltadiff1, add_fourth = "c1_temp")
plot_grid(plotlist = qa_lagdiffs1) #not compelling enough that I want to NA and flag anything


# clean up environment
rm(check_deltadiff1, qa_lagdiffs1, swing_check1, qa_swings1)


# -- RUN THROUGH DIFF DAILY ONE MORE TIME AND REVIEW QA'D DATA  ----
# final check
working_dat <- diff_daily(working_dat, rename = F)
par(mfrow=c(1,3))
boxplot(working_dat$deviance_1, main = "daily dev. from d1 logger")
boxplot(working_dat$deviance_2, main = "daily dev. from sdl chart")
boxplot(working_dat$deviance_3, main = "daily dev. from sdl logger")
par(mfrow=c(1,1))
# look at more extreme deviance in both sdl and d1 -- not that sdl chart values off mar 2015-aug 2016 (artificially low) so not best comparison source
check_diffdaily <- subset(working_dat, deviance_1 >= 6 | deviance_2 >= 6 | deviance_3 >= 6) 
qa_diffdaily <- visual_qa(working_dat, check_diffdaily, add_fourth = "kj_temp")
plot_grid(plotlist = qa_diffdaily[grep("temp_max", qa_diffdaily)]) # flag 2011-01-10 + 2011-01-11, 2011-08-21 + 2011-08-23
plot_grid(plotlist = qa_diffdaily[grep("temp_min", qa_diffdaily)]) # flag 2011-01-10 + 2011-01-11, all else not compelling enough

flag_diffdaily <- subset(working_dat, as.character(date) %in% c("2011-01-10", "2011-01-11") |
                           (met == "airtemp_max" & as.character(date) %in% c("2011-08-21", "2011-08-23")))

working_dat <- flag_temp(flag_diffdaily)



# -- REVIEW AND CLEAN UP WORKING DAT -----
# plot qa'd data for review
# chart values with d1 logger values plotted behind in purple as comparison for ranges
ggplot(working_dat) + 
  geom_point(aes(date, comp1), alpha = 0.6, col = "purple") +
  geom_point(aes(date, main), alpha = 0.5) +
  #geom_smooth(aes(date, cr_temp, group = logger), method = "glm", col = "black") +
  scale_color_viridis_d() +
  facet_wrap(~met)

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

d1_old_new <- left_join(d1tknwt, dplyr::select(working_dat, -d1_flag)) %>%
  dplyr::select(LTER_site:qa_flag) %>%
  rename(qa_temp = main)


# write out qa'd dataset
write_csv(d1_old_new, "climate_d1_c1/output_data/prep_data/qa_d1_temp.csv")

