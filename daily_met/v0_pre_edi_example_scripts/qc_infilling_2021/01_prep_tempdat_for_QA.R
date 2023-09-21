# prep temp datasets for QA functions

# script purpose
# read in raw (or previously QA'd and infilled) datasets to use with flagging functions
# > these are the datasets you will use as target and/or spatial reference
# make tidy with metrics you'd like to assess (e.g. max T, min T, mean T, diurnal T)
# write out prepped dats to run through QA procedure



# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN", -6999)

# set path to climdat folder
datpath <- "c1_d1_sdl_clim/homogenize_climdat/"
# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")
# functions to qa temp
source(paste0(datpath,"scripts/flagging_functions.R"))



# -- GET DATA -----
# relatively uncorrected datasets
# sdl chart
sdl <- getTabular(413) %>% data.frame()
# d1 chart
d1 <- getTabular(412) %>% data.frame()
# c1 chart
c1 <- getTabular(411) %>% data.frame()

# logger datasets: 2000-present
d1_log_daily <- getTabular(402) %>% data.frame()
c1_log_daily <- getTabular(401) %>% data.frame() # 2000-06-24 to 2013-03-25 = CR23x, 2013-03-27 CR1000
# GCE automate flagging starts in 2014
# sdl (since 2000)
sdl_log_daily <- getTabular(405) %>% data.frame()

# logger datasets: 1986-2000 (cr21x)
sdl21x_daily <- getTabular(78, na_vals = na_vals) %>% data.frame()
c121x_daily <- getTabular(400, na_vals = na_vals) %>% data.frame()
d121x_daily <- getTabular(70, na_vals = na_vals) %>% data.frame()

# green lakes logger dataset (1997-2019)
gl4_daily <- getTabular(148, na_vals = na_vals) %>% data.frame()

# nwt site visit 2019 datasets
# d1 chart, kittel et al. infilled
d1_kittel <- getTabular(187) %>% data.frame()
# c1 chart, kittel et al. infiled
c1_kittel <- getTabular(185) %>% data.frame()
# jennings infilled logger data
jennings <- getTabular(168) %>% data.frame() # 1990 through 2019; thru 2013 was keith jennings 2014-2019 was kehan yang; no flagging 2014 onwards

# hrly logger raw datasets for noting infilling jennings et al. data
sdl_log_hrly <- getTabular(57) %>% data.frame()
# d1_log_hrly <- getTabular(57) %>% data.frame()
# c1_log_hrly <- getTabular(57) %>% data.frame()


# -- TIDY + PARE RAW DATASETS TO TEMP VARS ONLY ----
# want to preserve flagging for each dataset, in particular note which values were infilled
# standardize flags for QA procedure
# temp metrics to include: 
# > max, min, avg (if recorded, e.g., logger)
# to create: DTR, 1 day lag difference

# e.g., logger datasets have more than temperature included

pare_temp <- function(dat, tempstring, flagstring = NA, keepcols, datecol = "date", DTR = T, maxstring = "max", minstring = "min", reps = NA){
  tempcols <- names(dat)[grepl(tempstring, names(dat), ignore.case = T)]
  tempdat <- dat[c(keepcols, tempcols)]
  
  if(!is.na(flagstring)){
    flagcols <- names(tempdat[grepl(flagstring, names(tempdat), ignore.case = T)])
    flagdat <- subset(dat, select = c(keepcols, flagcols))
    # if creating diurnal temp, collapse any flags from maxT and minT
    if(DTR){
      if(all(is.na(reps))){
        maxcol <- names(flagdat)[grepl(maxstring, names(flagdat), ignore.case = T)]
        mincol <- names(flagdat)[grepl(minstring, names(flagdat), ignore.case = T)]
        flagdat <- rowwise(flagdat) %>%
          mutate(flag_DTR = ifelse(is.na(get(maxcol)) & is.na(get(mincol)), NA, 
                                   ifelse(is.na(get(maxcol)), paste0(minstring, ": ", get(mincol)), 
                                          ifelse(is.na(get(mincol)), paste0(maxstring, ": ", get(maxcol)),
                                                 ifelse(get(maxcol) == get(mincol), as.character(get(maxcol)),
                                                        paste0(maxstring, ": ", get(maxcol), ", ", minstring, ": ", get(mincol))))))) %>%
          ungroup()
        #prefix whatever flagstring is being used
        names(flagdat)[names(flagdat) == "flag_DTR"] <- paste0(flagstring, "_DTR")
      }else{
        # for multiple sensors, calculate per sensor DTR
        for(r in reps){
          maxcol <- names(flagdat)[grepl(r, names(flagdat), ignore.case = T) & grepl(maxstring, names(flagdat), ignore.case = T)]
          mincol <- names(flagdat)[grepl(r, names(flagdat), ignore.case = T) & grepl(minstring, names(flagdat), ignore.case = T)]
          flagdat <- rowwise(flagdat) %>%
            mutate(flag_DTR = ifelse(is.na(get(maxcol)) & is.na(get(mincol)), NA, 
                                     ifelse(is.na(get(maxcol)), paste0(minstring, ": ", get(mincol)), 
                                            ifelse(is.na(get(mincol)), paste0(maxstring, ": ", get(maxcol)),
                                                   ifelse(get(maxcol) == get(mincol), as.character(get(maxcol)),
                                                          paste0(maxstring, ": ", get(maxcol), ", ", minstring, ": ", get(mincol))))))) %>%
            ungroup()
          #prefix whatever flagstring is being used and append sensor to newly created DTR flag with sensor
          names(flagdat)[names(flagdat) == "flag_DTR"] <- paste0(flagstring, "_DTR_", r)
        }
      }
      flagcols <- names(flagdat)[grep(flagstring, names(flagdat), ignore.case = T)]
    }
    # make long form
    flagdat <- gather(flagdat, metric, flag, c(flagcols))
    # drop flag cols from tempdat
    tempdat <- tempdat[!names(tempdat) %in% flagcols]
  }
  
  if(DTR){
    if(all(is.na(reps))){
      tempdat$DTR <- tempdat[[grep(maxstring, names(tempdat), ignore.case = T)]] - tempdat[[grep(minstring, names(tempdat), ignore.case = T)]]
    }else{
      # for multiple sensors, calculate per sensor DTR
      for(r in reps){
        maxcol <- names(tempdat)[grepl(r, names(tempdat), ignore.case = T) & grepl(maxstring, names(tempdat), ignore.case = T)]
        mincol <- names(tempdat)[grepl(r, names(tempdat), ignore.case = T) & grepl(minstring, names(tempdat), ignore.case = T)]
        tempdat[paste0("DTR_", r)] <- tempdat[maxcol] - tempdat[mincol]
      }
    }
  }
  
  # make temperature data long form (tidy)
  temppos <- min(which(names(tempdat) %in% tempcols))
  templong <- gather(tempdat, metric, temp, c(temppos:ncol(tempdat)))
  
  #re-join flagging
  if(!is.na(flagstring)){
    # clean up metric in flagdat
    #tempcols <- unique(templong$metric)
    # make regex
    metricvals <- str_flatten(unique(templong$metric), collapse = "|")
    flagdat$metric <- str_extract(flagdat$metric, pattern = metricvals)
    templong <- merge(templong, flagdat, all.x = T)
  }
  
  # subset data so only period of measurement included (instrument active lifetime)
  # > this matters for loggers vs. hmps
  templong_startdate <- min(with(templong, unique(get(datecol)[!is.na(temp)])))
  templong_enddate <- max(with(templong, unique(get(datecol)[!is.na(temp)])))
  templong <- subset(templong, get(datecol) >= templong_startdate)
  templong <- subset(templong, get(datecol) <= templong_enddate)
  
  # return dataset
  return(templong)
}


# 1) raw NWT datasets -----
# raw chart
sdl_tidy <- pare_temp(sdl, tempstring = "airtemp", keepcols = c("local_site", "date"), flagstring = "flag")
d1_tidy <- pare_temp(d1, tempstring = "airtemp", keepcols = c("local_site", "date"), flagstring = "flag")
c1_tidy <- pare_temp(c1,  tempstring = "airtemp", keepcols = c("local_site", "date"), flagstring = "flag")

# loggers
# > note: at sdl, d1 and c1, logger will stop at some point and data collection shifts to hmps
sdlcr_tidy <- pare_temp(sdl_log_daily, tempstring = "airtemp_m|airtemp_a", keepcols = c("local_site", "date", "logger"), flagstring = "flag", reps = NA)
sdlcr_hmps <- pare_temp(sdl_log_daily, tempstring = "airtemp_h", keepcols = c("local_site", "date", "logger"), flagstring = "flag", reps = c("hmp1", "hmp2", "hmp3"))

d1cr_tidy <- pare_temp(d1_log_daily, tempstring = "airtemp_m|airtemp_a", keepcols = c("local_site", "date", "logger"), flagstring = "flag")
d1cr_hmps <- pare_temp(d1_log_daily, tempstring = "airtemp_h", keepcols = c("local_site", "date", "logger"), flagstring = "flag", reps = c("hmp1", "hmp2", "hmp3"))

c1cr_tidy <- pare_temp(c1_log_daily, tempstring = "airtemp_m|airtemp_a", keepcols = c("local_site", "date", "logger"), flagstring = "flag")
c1cr_hmps <- pare_temp(c1_log_daily, tempstring = "airtemp_h", keepcols = c("local_site", "date", "logger"), flagstring = "flag", reps = c("hmp1", "hmp2", "hmp3"))

# loggers: 1986-2000
# > no flagging in these datasets, no local site either
sdl21x_tidy <- pare_temp(data.frame(cbind(local_site = "sdl", sdl21x_daily[!grepl("^time", names(sdl21x_daily))])), 
                                    tempstring = "airtemp", keepcols = c("local_site", "date", "logger"), flagstring = NA, reps = NA)
c121x_tidy <- pare_temp(data.frame(cbind(local_site = "c1", c121x_daily[!grepl("^time", names(c121x_daily))])), 
                         tempstring = "airtemp", keepcols = c("local_site", "date", "logger"), flagstring = NA, reps = NA)
d121x_tidy <- pare_temp(data.frame(cbind(local_site = "d1", d121x_daily[!grepl("^time", names(d121x_daily))])), 
                         tempstring = "airtemp", keepcols = c("local_site", "date", "logger"), flagstring = NA, reps = NA)

# green lakes logger
gl4_tidy <- pare_temp(gl4_daily[!grepl("^time", names(gl4_daily))], tempstring = "airtemp", keepcols = c("local_site", "date", "logger"), flagstring = "flag", reps = NA)

# sdl hourly loggers
sdlcr_hrly_tidy <- pare_temp(sdl_log_hrly, tempstring = "airtemp_m|airtemp_a", datecol = "date_time_start", keepcols = c("local_site", "date_time_start", "logger"), flagstring = "flag")
sdlcr_hrly_hmps <- pare_temp(sdl_log_hrly, tempstring = "airtemp_h", datecol = "date_time_start", keepcols = c("local_site", "date_time_start", "logger"), flagstring = "flag", reps = c("hmp1", "hmp2", "hmp3"))


# 2) clean up hmps in metric ----
# move sensor info into its own column
tidy_sensor <- function(dat, sensorstring = "hmp[0-9]", keepcols = c("local_site", "date", "logger")){
  dat <- mutate(dat, sensor = str_extract(metric, sensorstring),
                metric = gsub(paste0("_", sensorstring), "", metric))
  # rearrange cols
  firstnames <- c(keepcols, "metric", "sensor")
  dat <- subset(dat, select = c(firstnames, names(dat)[!names(dat) %in% firstnames]))
  return(dat)
}


sdlcr_hmps <- tidy_sensor(sdlcr_hmps)
sdlcr_hrly_hmps <- tidy_sensor(sdlcr_hrly_hmps, keepcols = c("local_site", "date_time_start", "logger"))
d1cr_hmps <- tidy_sensor(d1cr_hmps)
c1cr_hmps <- tidy_sensor(c1cr_hmps)

# sdl is missing data in nov 2019; local_site is NA on those dates and no flagging done
sdlcr_hmps$local_site <- "sdl"
sdlcr_hrly_hmps$local_site <- "sdl"


# -- TIDY + PARE INFILLED DAT -----
# 1) Jennings et al. hourly logger data ----
# > note: need to compare with raw data to know which values were infilled starting in 2014

jennings_tidy <- subset(jennings, select= c(local_site:airtemp_flag))
# quick screen 2014-2019 data before moving forward
jennings2014 <- subset(jennings_tidy, year >= 2014)
sapply(split(jennings2014$airtemp_avg, jennings2014$year), summary)
sapply(split(jennings2014$airtemp_avg, month(jennings2014$date)), summary) # extremes seem reasonable
sapply(split(jennings2014$airtemp_avg, jennings2014$local_site), summary)
# compare with global extremes
sapply(split(jennings$airtemp_avg, jennings$local_site), summary) # ok
# visualize
ggplot(jennings2014, aes(month(date), airtemp_avg, group = month(date))) +
  geom_jitter(aes(col = year), alpha = 0.5) +
  geom_boxplot(fill = "transparent") +
  scale_color_viridis_c() +
  facet_wrap(~factor(local_site, levels = c("d1", "sdl", "c1")), nrow = 3) # some yearly clustering of warm temps in fall
# check again with all years, but split at 2014
ggplot(jennings, aes(month(date), airtemp_avg, group = month(date))) +
  geom_jitter(aes(col = year), alpha = 0.5) +
  geom_boxplot(fill = "transparent") +
  scale_color_viridis_c() +
  facet_grid((year<2014)~factor(local_site, levels = c("d1", "sdl", "c1"))) # some stretch of years warmer/cooler seasonally I guess

# proceed with quick qa
jennings2014_devs <- flag_deviations(jennings2014, groupvars = "local_site",  metric = "airtemp_avg")
summary(jennings2014_devs[grepl("flag", names(jennings2014_devs))]) # no flags
jennings2014_devs <- flag_deviations(jennings2014, groupvars = c("local_site", "mon"),  metric = "airtemp_avg", returnworking = T)
# plot distribution
subset(jennings2014_devs, select = c(local_site, std_airtemp_avg, grp_std_airtemp_avg)) %>%
  gather(met, val, std_airtemp_avg, grp_std_airtemp_avg) %>%
  ggplot() +
  geom_histogram(aes(val), fill = "grey80", col = "grey30") +
  facet_grid(local_site ~ met) # wouldn't expect bimodal dist.. unless is a reflect of winter vs. summer months? more pronounced at sdl than d1
# how does it look pre 2014?
jennings2013_devs <- flag_deviations(subset(jennings, year < 2014), groupvars = c("local_site", "mon"),  metric = "airtemp_avg", returnworking = T)
#plot distribution
subset(jennings2013_devs, select = c(local_site, std_airtemp_avg, grp_std_airtemp_avg)) %>%
  gather(met, val, std_airtemp_avg, grp_std_airtemp_avg) %>%
  ggplot() +
  geom_histogram(aes(val), fill = "grey80", col = "grey30") +
  facet_grid(local_site ~ met) # it's there, but less pronounced, and moreso for d1
# move on ..
# not expecting flatline, but to check
jennings2014_flats <- flag_flatlines(jennings2014, metric = "airtemp_avg", groupvars = "local_site", numdays = 6) #if 6 hrs no change in temp
summary(jennings2014_flats$airtemp_avg_flatline) # nothing
jennings2014_spikes <- flag_spikes(jennings2014, metric = "airtemp_avg", groupvars = c("local_site"), abs_limit = 10, returnworking = T) # 5 degree change in an hour 
summary(jennings2014_spikes) # swinging 20 in an hour seems like a lot?
sapply(split(jennings2014_spikes$airtemp_avg_change, jennings2014_spikes$local_site), summary)
ggplot(jennings2014_spikes) +
  geom_vline(data=subset(jennings2014_spikes, abs(airtemp_avg_change) > 10), aes(xintercept = airtemp_avg_change, col= year(date)), lwd = 1, alpha = 0.5) +
  geom_histogram(aes(airtemp_avg_change), bins = 50) +
  scale_color_viridis_c() +
  facet_wrap(~factor(local_site, levels = c("d1", "sdl", "c1")), nrow = 3)
ggplot(jennings2014_spikes, aes(local_site, airtemp_avg_change)) +
  geom_jitter(aes(col = year(date)), alpha = 0.5) +
  geom_violin(fill = "transparent") +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey30") +
  scale_color_viridis_c()
# check out a few cases manually
egspike <- which(abs(jennings2014_spikes$airtemp_avg_change) > 19)
View(jennings2014_spikes[(egspike[1]-5):(egspike[1]+5),])
# what does it look like pre-2014?
jennings2013_spikes <- flag_spikes(subset(jennings, year < 2014), metric = "airtemp_avg", groupvars = c("local_site"), abs_limit = 10, returnworking = T) # 10 degree change in an hour 
# compare
sapply(split(jennings2013_spikes$airtemp_avg_change, jennings2013_spikes$local_site), summary)
# check out a few cases manually
egspike2013 <- which(abs(jennings2013_spikes$airtemp_avg_change) > 19)
View(jennings2013_spikes[(egspike2013[1]-5):(egspike2013[1]+5),])
View(jennings_tidy[jennings_tidy$date == as.Date("2002-05-19"),]) # flagged values here were flagged (for spike change) and infilled in keith's process..
# it warmed at sdl and d1 in the same time period, but not nearly as sharply
ggplot(jennings2013_spikes) +
  geom_vline(data=subset(jennings2013_spikes, abs(airtemp_avg_change) > 10), aes(xintercept = airtemp_avg_change, col= year(date)), lwd = 1, alpha = 0.5) +
  geom_histogram(aes(airtemp_avg_change), bins = 50) +
  scale_color_viridis_c() +
  facet_wrap(~factor(local_site, levels = c("d1", "sdl", "c1")), nrow = 3)
ggplot(jennings2013_spikes, aes(local_site, airtemp_avg_change)) +
  geom_jitter(aes(col = year(date)), alpha = 0.5) +
  geom_violin(fill = "transparent") +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey30") +
  scale_color_viridis_c() # not a many extreme spikes in jennings-years data, 
# retain jenning's et al flag col
jennings2013_spikes <- flag_spikes(subset(jennings_tidy, year < 2014), metric = "airtemp_avg", groupvars = c("local_site"), abs_limit = 15, sd_limit = 7) # 15 degree change in an hour 
# how often were flagged values infilled?
sort(with(subset(jennings2013_spikes, !is.na(airtemp_avg_temporal_spike)), summary(factor(airtemp_flag))))
# of 334 total flagged vals in 1990-2013 data, about half had no flagging in keith's process, next most common was pulled for rate change and infilled via avg temp 24 hrs before and after
# there are 983 flagged vals in 2014-2019 data 
# ultimately for daily record in renewal, will matter (most I think?) if spike temp is selected as min or max T for the day
jennings2013_spikes <- flag_spikes(subset(jennings_tidy, year < 2014), metric = "airtemp_avg", groupvars = c("local_site"), abs_limit = 15, sd_limit = 7, returnworking = T) %>%
  left_join(jennings_tidy[c("date.time", "date", "local_site", "airtemp_flag")])
ggplot(jennings2013_spikes, aes(local_site, airtemp_avg_change)) +
  geom_jitter(data = subset(jennings2013_spikes, !is.na(airtemp_avg_temporal_spike) & airtemp_flag == "qc0in0"), pch = 1, col = "red") +
  geom_jitter(data = subset(jennings2013_spikes, !is.na(airtemp_avg_temporal_spike) & airtemp_flag != "qc0in0"), aes(fill = airtemp_flag), pch = 21,  alpha = 0.5) +
  geom_violin(fill = "transparent") +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey30") +
  scale_fill_viridis_d()

# will proceed with hourly dataset providing tmax + tmin isn't a flagged value
# clean up environment first
rm(jennings2013_devs, jennings2013_spikes, jennings2014, jennings2014_devs, jennings2014_flats, jennings2014_spikes)
# apply spikes flagging to full temp dataset
jennings_spikes <- flag_spikes(jennings_tidy, metric = "airtemp_avg", groupvars = c("local_site"), abs_limit = 11, sd_limit = 10)
jennings_daily <- jennings_spikes %>%
  group_by(local_site, date, year, jday) %>%
  summarise(airtemp_max = max(airtemp_avg),
            flag_airtemp_max = airtemp_flag[which.max(airtemp_avg)],
            spike_airtemp_max = airtemp_avg_temporal_spike[which.max(airtemp_avg)],
            airtemp_min = min(airtemp_avg),
            flag_airtemp_min= airtemp_flag[which.min(airtemp_avg)],
            spike_airtemp_min = airtemp_avg_temporal_spike[which.min(airtemp_avg)],
            airtemp_mean = mean(airtemp_avg),
            flag_airtemp_mean = str_flatten(sort(unique(airtemp_flag)), collapse = ","),
            spike_airtemp_mean = !all(is.na(airtemp_avg_temporal_spike)))
# how many min or max were flagged for spikes?
ggplot(jennings_daily, aes(date, airtemp_max)) +
  geom_line(col = "grey50", alpha = 0.5) +
  geom_point(data= subset(jennings_daily, !is.na(spike_airtemp_max)), aes(col = flag_airtemp_max)) +
  geom_smooth() +
  geom_vline(aes(xintercept = as.Date("2014-01-01")), lty = 2)+
  facet_wrap(~factor(local_site, levels = c("d1", "sdl", "c1")), nrow = 3)
ggplot(jennings_daily, aes(date, airtemp_min)) +
  geom_line(col = "grey50", alpha = 0.5) +
  geom_point(data= subset(jennings_daily, !is.na(spike_airtemp_min)), aes(col = flag_airtemp_min)) +
  geom_smooth() +
  geom_vline(aes(xintercept = as.Date("2014-01-01")), lty = 2) +
  facet_wrap(~factor(local_site, levels = c("d1", "sdl", "c1")), nrow = 3)
ggplot(jennings_daily, aes(date, airtemp_mean)) +
  geom_line(col = "grey50", alpha = 0.5) +
  geom_point(data= subset(jennings_daily, spike_airtemp_mean)) +
  geom_smooth() +
  geom_vline(aes(xintercept = as.Date("2014-01-01")), lty = 2)+
  facet_wrap(~factor(local_site, levels = c("d1", "sdl", "c1")), nrow = 3)


# >resolution: keep spike flags on daily points for awareness in comparative station checks
# make spike_airtemp_mean a character col like other spike flags
# also compare with hourly raw dataset to see if there is data-driven way to ID infilled vals in 2014-2019 period
jennings_daily <- jennings_daily %>%
  mutate(spike_airtemp_mean = ifelse(spike_airtemp_mean, "At least one hourly data point in day spike-flagged", NA))
jennings_daily_allflags <- pare_temp(jennings_daily[!grepl("spike", names(jennings_daily))], tempstring = "airtemp", flagstring = "flag", keepcols = c("local_site", "date"))
jennings_daily_spikeflags <- pare_temp(jennings_daily[!grepl("flag", names(jennings_daily))], tempstring = "airtemp", flagstring = "spike", keepcols = c("local_site", "date")) %>%
  rename(qaflag_hrly_spike = flag)
# join hrly spike flag info
jennings_daily_allflags <- merge(jennings_daily_allflags, jennings_daily_spikeflags)
# look at DTR
ggplot(jennings_daily_allflags, aes(date, temp)) +
  geom_line() +
  geom_point(data = subset(jennings_daily_allflags, !is.na(qaflag_hrly_spike)), col = "red") +
  facet_grid(metric~factor(local_site, levels = c("d1", "sdl", "c1")), scales = "free_y")


# compare with raw logger datasets
comparej <- subset(jennings_daily_allflags, local_site == "sdl") %>%
  mutate(metric = gsub("mean", "avg", metric)) %>%
  rename_at(c("temp", "flag", "qaflag_hrly_spike"), function(x) paste0(x, "_jennings")) %>%
  left_join(sdlcr_tidy)
summary(comparej$temp_jennings == comparej$temp) # overlap is on average values
ggplot(comparej, aes(temp_jennings, temp, col = year(date))) + 
  geom_point(alpha = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_color_viridis_c(option = "A") +
  facet_wrap(~ metric, scales = "free")
group_by(comparej, factor(metric)) %>%
  summarise(sameval = round(temp_jennings,0) == round(temp,0)) %>%
  table()
# even when round to nearest degree, many max, min and DTR are not what you'd get using daily data

# to be sure I didn't mess up anything in dataframe manipulation, repeat with tmax only..
jennings_max <- subset(jennings, local_site== "sdl") %>%
  group_by(local_site, date) %>%
  summarise(tempj = max(airtemp_avg)) %>%
  ungroup() %>%
  mutate(metric = "airtemp_max") %>%
  left_join(subset(sdlcr_tidy, metric == "airtemp_max"))
ggplot(jennings_max, aes(tempj, temp, col = year(date))) + geom_point(alpha = 0.5)
# temps are not close.. daily cr temps trend warmer than tmax from jennings et al hrly dataset..
# metadata for daily logger data says tmin and tmax recorded instantaneously in 5sec intervals.. so daily dat could be both warmer and cooler for that reason, DTR larger

# not sure what to do wrt logger datasets..
# jennings et al. process has more extensive QA built on kittel methods, but values derived from hourly differ from daily values in non-trivial way
# .. maybe can just treat jennings et al. daily-derived dataset as another potential infill source, separate from logger daily dataset (since expect tmin and tmax could be more extreme in daily dataset)
# it will also be tough to use the daily dataset to tell which days may have hourly values infilled 2014-2019 since max and min values can differ so much (hrly avg values mute micro-spikes)

# clean up
rm(jennings_max, jennings_daily_spikeflags, comparej)

# compare daily derived from hrly at saddle to jennings daily derived at sdl for 2014-2019 to see if can ID infilled values there
jennings2014_sdl <- subset(jennings_spikes, year >= 2014 & local_site == "sdl") %>%
  rename(airtemp_avgj = airtemp_avg, airtemp_flagj = airtemp_flag) %>%
  left_join(sdl_log_hrly[c("date_time_start", "airtemp_avg", "flag_airtemp_avg")], by = c("date.time" = "date_time_start")) %>%
  mutate(sametemp = airtemp_avgj == airtemp_avg,
         difftemp = airtemp_avgj - airtemp_avg)
summary(jennings2014_sdl) # some values are very different..
# look at spread of values that differ
boxplot(jennings2014_sdl$difftemp[!jennings2014_sdl$sametemp])
# note dates that differ and can flag those in outgoing dataset
diffdates <- with(jennings2014_sdl, unique(date[!sametemp  & !is.na(sametemp)]))
View(subset(jennings2014_sdl, date %in% diffdates)) # correctly ID'd
# add column to indicate if difference in 2014-2018 values
# > does not consider any hmp adjustment for 2019 since don't know how that was treated
jennings_daily_allflags$hrly2014_adjusted <- with(jennings_daily_allflags, (date %in% diffdates) & local_site =="sdl")
# check as expected
ggplot(jennings_daily_allflags, aes(date, temp, col = hrly2014_adjusted)) +
  geom_point() +
  facet_grid(local_site ~ metric) # yes
# clean up
rm(jennings2014_sdl, diffdates, jennings_spikes)

# standardize airtemp_mean to airtemp_avg in metric
jennings_daily_allflags$metric <- gsub("mean", "avg", jennings_daily_allflags$metric)


# -- ADD DESCRIPTIVE TO ALL -----
#yr, mon, doy, time + data source if desired
describe_time <- function(dat, datecol = "date", dateform = "%Y-%m-%d", datsource = NA){
  # be sure lubridate loaded
  library(lubridate)
  # assign data to new frame with a date column for merging
  newdat <- dat
  if(grepl("%H", dateform)){
    names(newdat)[names(newdat) == datecol] <- "date_time"
    # ensure in posix form
    newdat$date_time <- as.POSIXct(newdat$date_time, format = dateform)
    newdat$date <- as.Date(newdat$date_time)
  }else{
    names(newdat)[names(newdat) == datecol] <- "date"
    # ensure in date form
    newdat$date <- as.Date(newdat$date, format = dateform)
  }
  
  # create data frame of all expected timestamps
  tempdates <- sort(unique(newdat$date))
  mindate <- min(tempdates)
  maxdate <- max(tempdates)
  datestep <- tempdates[2] - tempdates[1]
  dateseq <- seq.Date(mindate, maxdate, by = datestep)
  
  dateframe <- data.frame(date = dateseq,
                          yr = year(dateseq),
                          mon = month(dateseq),
                          doy = yday(dateseq))
  
  
  if(grepl("%H", dateform, ignore.case = T)){
    tempdt <- sort(unique(newdat$date_time))
    mindt <- min(tempdt)
    maxdt <- max(tempdt)
    # calculate interval
    timeinterval <- tempdt[2] - tempdt[1]
    # create date_time sequence
    dtseq <- seq.POSIXt(mindt, maxdt, by = timeinterval)
    # extract time pattern from dateform
    timestep <- str_extract(dateform, "%H.*S+")
    dtframe <- data.frame(date_time = dtseq,
                          date = as.Date(dtseq),
                          timestamp = format(dtseq, format = timestep))
    # merge timestamps with dateframe
    dateframe <- merge(dtframe, dateframe, all = T)
  }
  
  # combine date/time descriptive info to data
  newdat <- merge(dateframe, newdat, all = T)
  # if data source indicated, add to beginning
  if(!is.na(datsource)){
    newdat <- cbind(data_source = datsource, newdat)
  }
  
  return(data.frame(newdat))
}

# chart
c1_tidy <- describe_time(c1_tidy, datsource = "raw chart")
sdl_tidy <- describe_time(sdl_tidy, datsource = "raw chart")
d1_tidy <- describe_time(d1_tidy, datsource = "raw chart")

# loggers
c1cr_tidy <- describe_time(c1cr_tidy, datsource = "raw logger")
sdlcr_tidy <- describe_time(sdlcr_tidy, datsource = "raw logger")
d1cr_tidy <- describe_time(d1cr_tidy, datsource = "raw logger")
gl4_tidy <- describe_time(gl4_tidy, datsource = "raw logger")

# 21x
c121x_tidy <- describe_time(c121x_tidy, datsource = "raw logger")
sdl21x_tidy <- describe_time(sdl21x_tidy, datsource = "raw logger")
d121x_tidy <- describe_time(d121x_tidy, datsource = "raw logger")

# hmps
c1cr_hmps <- describe_time(c1cr_hmps, datsource = "raw hmps")
sdlcr_hmps <- describe_time(sdlcr_hmps, datsource = "raw hmps")
d1cr_hmps <- describe_time(d1cr_hmps, datsource = "raw hmps")

# derived/infilled 
jennings_daily_allflags <- describe_time(jennings_daily_allflags, datsource = "jennings derived")


# -- STACK AND REVIEW -----
# stack cr21x (1986-2000) with later loggers (cr23x, cr1000) 
sdlcr_tidy <- rbind(cbind(sdl21x_tidy, flag = NA), sdlcr_tidy)
c1cr_tidy <- rbind(cbind(c121x_tidy, flag = NA), c1cr_tidy)
d1cr_tidy <- rbind(cbind(d121x_tidy, flag = NA), d1cr_tidy)

# review
# loggers
## sdl
with(sdlcr_tidy, lapply(split(sdlcr_tidy, logger), summary))
lapply(split(sdlcr_tidy[c("local_site", "metric", "flag")],sdlcr_tidy$logger), unique)
## c1
with(c1cr_tidy, lapply(split(c1cr_tidy, logger), summary))
lapply(split(c1cr_tidy[c("local_site", "metric", "flag")],c1cr_tidy$logger), unique)
## d1
with(d1cr_tidy, lapply(split(d1cr_tidy, logger), summary))
lapply(split(d1cr_tidy[c("local_site", "metric", "flag")],d1cr_tidy$logger), unique)
# > there are some unrealistic temp extremes (esp in cr21x period) across sites, watch for those in QA
## green lake 4
with(gl4_tidy, lapply(split(gl4_tidy, logger), summary))
lapply(split(gl4_tidy[c("local_site", "metric", "flag")],gl4_tidy$logger), unique)

# hmps
##sdl
with(sdlcr_hmps, lapply(split(sdlcr_hmps, sensor), summary))
lapply(split(sdlcr_hmps[c("local_site", "metric", "flag")],sdlcr_hmps$sensor), unique)
## c1
with(c1cr_hmps, lapply(split(c1cr_hmps, sensor), summary))
lapply(split(c1cr_hmps[c("local_site", "metric", "flag")],c1cr_hmps$sensor), unique)
## d1
with(d1cr_hmps, lapply(split(d1cr_hmps, sensor), summary))
lapply(split(d1cr_hmps[c("local_site", "metric", "flag")],d1cr_hmps$sensor), unique)

# chart
summary(sdl_tidy)
sapply(split(sdl_tidy$temp, sdl_tidy$metric), function(x) summary(is.na(x)))
unique(sdl_tidy[c("local_site", "metric", "flag")])

summary(c1_tidy)
sapply(split(c1_tidy$temp, c1_tidy$metric), function(x) summary(is.na(x)))
unique(c1_tidy[c("local_site", "metric", "flag")])

summary(d1_tidy)
sapply(split(d1_tidy$temp, d1_tidy$metric), function(x) summary(is.na(x)))
unique(d1_tidy[c("local_site", "metric", "flag")])

# derived/infilled
with(jennings_daily_allflags, lapply(split(jennings_daily_allflags, local_site), summary))
unique(jennings_daily_allflags[c("local_site", "metric")])


# -- FINISHING -----
# write out all prepped datasets for next step of QA
datout <- paste0(datpath, "data/prep_data/ready_for_qa/")
## raw chart datasets
write.csv(sdl_tidy, paste0(datout, "sdl_chart_tidy.csv"), row.names = F)
write.csv(d1_tidy, paste0(datout, "d1_chart_tidy.csv"), row.names = F)
write.csv(c1_tidy, paste0(datout, "c1_chart_tidy.csv"), row.names = F)
## raw daily logger datasets
write.csv(sdlcr_tidy, paste0(datout, "sdlcr_tidy.csv"), row.names = F)
write.csv(d1cr_tidy, paste0(datout, "d1cr_tidy.csv"), row.names = F)
write.csv(c1cr_tidy, paste0(datout, "c1cr_tidy.csv"), row.names = F)
write.csv(gl4_tidy, paste0(datout, "gl4_tidy.csv"), row.names = F)
## raw daily hmps 2018-ongoing
write.csv(sdlcr_hmps, paste0(datout, "sdl_hmps_tidy.csv"), row.names = F)
write.csv(d1cr_hmps, paste0(datout, "d1_hmps_tidy.csv"), row.names = F)
write.csv(c1cr_hmps, paste0(datout, "c1_hmps_tidy.csv"), row.names = F)
## infilled datasets
write.csv(jennings_daily_allflags, paste0(datout, "jennings_hrly2daily_tidy.csv"), row.names = F)
#write.csv(d1kittel_tidy, paste0(datout, "d1_kittel_tidy.csv"), row.names = F)
#write.csv(c1kittel_tidy, paste0(datout, "c1_kittel_tidy.csv"), row.names = F)

