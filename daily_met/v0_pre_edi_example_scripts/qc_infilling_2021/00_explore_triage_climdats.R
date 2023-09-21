# exploratory script to triage existing climdat issues for c1, d1, and sdl
# author(s): ctw
# questions?: caitlin.t.white@colorado.edu

# script purpose:
# preview new raw data quirks since last infill (summer 2019: appended data through 2018/12/31 for NWT site visit)
## usual suspects:
## missing data
## internal value spikes
## flatlines
## max >= min & min =< max
## extreme high or low values beyond j. morse recommendations (not necessarily untrue, but something to examine)
# + new issues (see notes)

# notes:
# SCE says D1 has mean values = max *or* min temp when the other is missing. Add check for that.
# trewin 2012 suggests range checks for each month (flag 10 most extreme vals per month and verify)

# metadata notes (e.g. flagging) from EDI 
# sdl, c1 and d1 logger flags:
# Flag values: n=no flag; m=missing; q=questionable; e=estimated.
## no flagging done 2000-2014 so flag vals set to n for that period


# parking lot for referenes:
# https://link.springer.com/article/10.1007%2Fs10661-012-2831-6
# https://rmets.onlinelibrary.wiley.com/doi/10.1002/gdj3.95 **good reference paper
# qa: https://cawcr.gov.au/technical-reports/CTR_049.pdf

# for breakpoints analysis
# https://www.marinedatascience.co/blog/2019/09/28/comparison-of-change-point-detection-methods/
# https://cran.r-project.org/web/packages/strucchange/vignettes/strucchange-intro.pdf
# mcp: https://lindeloev.github.io/mcp/articles/packages.html



# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN")

# set path to climdat folder
datpath <- "c1_d1_sdl_clim/homogenize_climdat/"
# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")
# functions to qa temp
source(paste0(datpath,"scripts/qa_functions.R"))


# -- GET DATA -----
# relatively uncorrected datasets
# sdl chart
sdl <- getTabular(413) %>% data.frame()
# d1 chart
d1 <- getTabular(412) %>% data.frame()
# c1 chart
c1 <- getTabular(411) %>% data.frame()


# logger datasets
d1_log_daily <- getTabular(402) %>% data.frame()
c1_log_daily <- getTabular(401) %>% data.frame() # 2000-06-24 to 2013-03-25 = CR23x, 2013-03-27 CR1000
# GCE automate flagging starts in 2014

# sdl (since 2000)
sdl_log_daily <- getTabular(405) %>% data.frame()
sdl_log_hrly <- getTabular(57) %>% data.frame()

# nwt site visit 2019 datasets
# d1 chart, kittel et al. infilled
d1_kittel <- getTabular(187) %>% data.frame()
# c1 chart, kittel et al. infiled
c1_kittel <- getTabular(185) %>% data.frame()
# sdl temp frankenbaby
## read in all to list bc don't remember what's what + see what sce added
sdltemp_files <- list.files("c1_d1_sdl_clim/homogenize_climdat/data/prep_data", full.names = T) 
sdltemp_list <- list()
for(i in 1:length(sdltemp_files)){
  sdltemp_list[[i]] <- read.csv(sdltemp_files[i])
  names(sdltemp_list[i])
}
# logger projected sdl chart time series used for 2019 nwt site visit
frankensdl <- sdltemp_list[[3]]


# -- PREP MASTER DATA (for easier plotting) -----
# function for tidying env vars + flags of interest in logger datasets (out of 80ish vars)
gather_logdat <- function(dat, grab = "airtemp", lastbasic = "jday"){
  grabcols <- grep(grab, names(dat))
  endbasic <- grep(lastbasic, names(dat))
  tempdat <- subset(dat, select = c(1:endbasic, grabcols)) %>%
    gather(met, val, (grep(lastbasic, names(.))+1):ncol(.)) %>%
    mutate(type = ifelse(grepl("flag", met), "flag", "temp"),
           met = gsub("flag_", "",met),
           property = str_extract(met, "[:alpha:]{3}$"),
           met = gsub("_[a-z]{3}$", "", met)) %>%
    spread(type, val) %>%
    mutate(temp = as.numeric(temp))
  return(tempdat)
}

all_log <- rbind(gather_logdat(c1_log_daily), gather_logdat(d1_log_daily), gather_logdat(sdl_log_daily))
str(all_log)

# see how it looks (is df structured how you want it?)
ggplot(all_log,aes(date, temp)) +
  geom_point(alpha = 0.5) +
  facet_wrap(local_site~property) # NAs in local_site?

# check raw dat
unique(d1_log_daily$local_site); unique(c1_log_daily$local_site); unique(sdl_log_daily$local_site) # let SCE know..
View(subset(sdl_log_daily, is.na(local_site)))
View(subset(sdl_log_daily, date >= as.Date("2019-11-01") & date <= as.Date("2019-11-30"))) # just blank for middle of month.. looks like logger dat missing in that month
View(subset(all_log, date >= as.Date("2019-11-01") & date <= as.Date("2019-11-30"))) # can assign sdl

# infill missing basic site-date info
all_log <- all_log %>%
  replace_na(., list(LTER_site = "NWT", local_site = "sdl")) %>%
  mutate(year = year(date), jday = ifelse(is.na(jday), yday(date), jday))

# return to plotting
ggplot(all_log,aes(date, temp)) +
  geom_line(aes(col = met), alpha = 0.5) +
  geom_smooth(aes(lty = month(date) %in% c(1:5, 11:12)), method = "lm", col = "black") +
  facet_wrap(local_site~property) #ok for now

# look at period when hmps launched
ggplot(subset(all_log, year > 2016),aes(date, temp)) +
  geom_line(aes(col = met), alpha = 0.5) +
  geom_smooth(method = "lm", col = "black") +
  facet_wrap(local_site~property) #incongruence at d1 in 2018-2019 (over winter?); c1 starts about a year earlier than sdl or d1

# pull quality chart data (not infilled, passed last CTW QA/QC screening)
c1_good <- subset(c1_kittel, raw_Tmax == max_temp & raw_Tmin == min_temp) %>% mutate(dataset = "Kittel")
d1_good <- subset(d1_kittel, raw_Tmax == max_temp & raw_Tmin == min_temp) %>% mutate(dataset = "Kittel")

# tidy raw chart data to compare
str(d1); str(c1)
rawdat <- rbind(d1, c1) %>% mutate(dataset = "raw", DTR = airtemp_max - airtemp_min)
# assign flags to mean temp and DTR if min temp or max temp has flag
rawdat$flag_airtemp_avg <- with(rawdat, ifelse(flag_airtemp_max == flag_airtemp_min, flag_airtemp_max, 
                                                paste0("max: ", flag_airtemp_max, ", min: ", flag_airtemp_min)))
rawdat$flag_DTR <- rawdat$flag_airtemp_avg
rawdat <- rawdat %>% gather(met, val, grep("airtemp|DTR", names(.))) %>%
  mutate(type = ifelse(grepl("flag", met), "flag", "temp"),
         met = gsub("flag_", "", met)) %>%
  spread(type, val) %>%
  # make names similar to infilled dataset
  mutate(met = ifelse(grepl("airtemp", met), paste0(met, "_temp"), met),
         met = gsub("^airtemp_", "", met),
         met = gsub("avg", "mean", met),
         temp = as.numeric(temp))


# stack chart data (raw and cleared)
chart_tidy <- rbind(c1_good, d1_good) %>%
  dplyr::select(LTER_site:flag_1, dataset) %>%
  rename(flag = flag_1) %>%
  gather(met, temp, max_temp:DTR) %>%
  dplyr::select(names(rawdat)) %>%
  rbind(rawdat[names(.)]) %>%
  mutate(yr = year(date), mon = month(date), doy = yday(date), temp = as.numeric(temp), local_site = casefold(local_site))

# plot to see if structured as intended
ggplot(chart_tidy, aes(date, temp, col = dataset)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~local_site+met)

# look at most recent years
ggplot(subset(chart_tidy, yr > 2015), aes(date, temp)) +
  geom_line(aes(lty = dataset, col = mon %in% c(10:12, 1:5)), alpha = 0.5) +
  geom_smooth(method = "lm", col= "black") +
  facet_wrap(~local_site+met, nrow = 2) 

# to look out for in QA/QC:
# are warming trends at D1 chart real or product of data quality issues? (e.g. sensor drifts, instrument changes, check against logger dat)
# similarly, is lack of trend at C1 chart real or product of data quality issue?

# quick check of how loggers at C1 and D1 compare to chart
test <- subset(all_log, year > 1999) %>%
  rename(source = met, met = property, yr = year, doy = jday) %>%
  mutate(met= gsub("avg", "mean", met),
         met= paste0(met, "_temp"),
         mon = month(date),
         dataset = "logger") %>%
  rbind(mutate(chart_tidy, source = "chart", logger = "chart")[names(.)])

# recent years
ggplot(subset(test, yr > 2015 & met != "DTR" & local_site != "sdl"), aes(date, temp)) +
  geom_line(aes(lty = dataset, col = source), alpha = 0.5) +
  geom_smooth(aes(lty = dataset, fill = dataset), col = "black", method = "lm") +
  scale_color_viridis_d() +
  facet_wrap(~local_site+met, nrow = 2)
# takeaway: logger trends do not agree with raw chart trends in recent years

# since 2000
ggplot(subset(test, yr > 1999 & met != "DTR" & local_site != "sdl"), aes(date, temp)) +
  geom_line(aes(lty = dataset, col = source), alpha = 0.5) +
  geom_smooth(aes(lty = dataset, fill = dataset), col = "black", method = "lm") +
  scale_color_viridis_d() +
  facet_wrap(~local_site+met, nrow = 2)
# note 1: Kittel data plotted here are untreated, good vals only (trend line not fit for all datapoints in the Kittel datasets)
## logger and chart trends here are not QA'd. so just trends in the datasets as they are on EDI
# takeaway: untreated, raw chart data would suggest moderate to no trend, when logger suggests otherwise
# note 2: raw data have missing values, where as logger dat have fewer:

propmissing <- group_by(test, local_site, yr, dataset, met, source) %>%
  summarise(prop_NA = sum(is.na(temp)/(length(date)))) %>%
  subset(yr > 1999 & dataset != "Kittel") %>%
  # remove hmp pre 2014
  subset(!(yr<= 2014 & grepl("hmp", source)))

# clean up
rm(test, propmissing)


 # -- TRIAGE NEW DATA (2019-present) -----
# To answer:
## how many missing dats in each dataset?
## how many dats with flags assigned by j. morse et al?
## anything else funny from the metadata?

# 1. missing data ----
# 1a. chart ----
## look at flagging
ggplot(subset(rawdat, year(date) > 2018), aes(date, temp)) +
  geom_point(aes(col = flag), alpha = 0.5) +
  facet_wrap(~local_site + met, nrow = 2) #q = questionable? it's not described in the metadata
## look at missing data
ggplot(subset(rawdat, year(date) > 2018 & grepl("min|max", met)), aes(day(date), is.na(temp))) +
  geom_point(aes(col = local_site), alpha = 0.5, position = position_dodge(width = 0.2, preserve = "total")) +
  facet_wrap(~year(date) + month(date), nrow = 2)
# c1 has a handful missing in jan, june, nov, and dec 2019; moreso missing in jan, july, much of oct and dec 2020
# d1 has a good chunk missing: jan, feb, mar, and apr 2019 and 2020; oct and dec 2019; a little in aug 2019 and sep 2020

# what are the counts?
subset(rawdat, year(date) > 2018 & grepl("min|max", met)) %>%
  group_by(local_site, year(date), month(date), met) %>%
  summarise(present = sum(!is.na(temp)),
            missing = sum(is.na(temp))) %>%
  ungroup() %>%
  filter(missing > 0) %>%
  data.frame()
  
# 1b. loggers -----
# ignore hmps before 2015
ggplot(subset(all_log, year(date) > 2018 & met != "airtemp"), aes(day(date), is.na(temp))) +
  geom_point(aes(col = paste(local_site, met)), alpha = 0.5, position = position_dodge(width = 0.2, preserve = "total")) +
  facet_wrap(~year(date) + month(date), nrow = 2)
# not too bad ITO missing data..
## all D1 hmps out continuously around jan 10- feb 20, 2019; and june 30- july 15, 2020
## all sdl hmps out around nov 7 - nov 23 2019
## otherwise, one sdl hmp out may - dec 2020, but other two usually available (not sure about data quality yet)

# if data quality, good should be able to...
# infill c1 chart with c1 logger dat
# infill d1 chart with d1 logger dat except for jan + feb 2019 (but sdl logger available)
# infill sdl logger nov 2019 with d1 logger
## sdl logger dat for proposal should just be one value per day per metric, ctw does not need to infill each hmp (sce can use keith's script for that)

