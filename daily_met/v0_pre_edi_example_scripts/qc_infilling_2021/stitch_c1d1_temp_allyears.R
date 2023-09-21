# quick stitch D1 and C1 temp 2018-2020

# script purpose:
# until can smooth over full prep, qa, and infill temp workflow, infill 2019-2020 chart with HMP data at each site

# steps:
# read in dats: kittel et al. tempdats, raw chart, raw logger (for hmps)
# review hmp quality at each (quick scan)
# infill with kittel methods
# write out to data/ subfolder as draft dats

# notes from site visit scripts re: kittel et al. methods for infilling temp:
# Flag 1: A=no infilling; B=method 1 p<.05; C=method 1 p>.05; D=method 2 p<.05; E=method 2 p>.05
# Flag 2: A=no infilling; B=unadjusted infilling; C=infilled adjusted for known Tmin; D=infilled adjusted for known Tmax
# Flag 3: A=no infilling; B=DTR p<.05; C=DTR 1 p>.05

#*method 1 = seasonal, same-year regression infill (for temp, 14 days before and after target infill window)
#*method 2 = multi-year (historic), same-day (+-3 days) regression infill

# *when a tmin or tmax is known (re: flag 2), TK used the predicted DTR and re-calculated tmean and calculated missing other temp val based on known val (i.e. he didn't use the predicted tmean when a tmax or tmin was known, even if r2 greater for tmean equation relative to dtr r2)  


# rules ctw used for site visit:
# rules for keeping adjusted temp vs using predicted when chart tmin or tmax exists:
## if adjusted tmin and adjusted tmax deviate more from c1logger than predicted (flag_adjtmax & flag_adjtmin == 1), use predicted 
## if either flag_adjtmax or flag_adjtmin == 1 (but not both) and raw chart value was adjusted by ctw (adjusted for artificial drop in temperature during record), use predicted
## if either flag_adjtmax or flag_adjtmin == 1 (but not both) and raw chart value unadjusted, use adjusted temp over predicted (i.e. keep chart value that exists and calculate missing temp from that and predicted DTR)
# triage temps with flags and add them in to predicted temp dataset .. or recalculate final temps with these rules



# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN", -9999)

# set path to climdat folder
datpath <- "c1_d1_sdl_clim/homogenize_climdat/"
# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")
# functions to flag potentially errant values
source(paste0(datpath,"scripts/flagging_functions.R"))


# -- GET DATA -----
# data prepped for QA (tidy form)
tidy_tempdat <- list.files(paste0(datpath, "data/prep_data/ready_for_qa/"),full.names = T)

# d1
d1chart <- read.csv(tidy_tempdat[grep("d1_chart", tidy_tempdat)], na.strings = na_vals)
d1hmps <- read.csv(tidy_tempdat[grep("d1_hmp", tidy_tempdat)], na.strings = na_vals)
d1k_temp <- getTabular(187) %>% data.frame()

# c1
c1chart <- read.csv(tidy_tempdat[grep("c1_chart", tidy_tempdat)], na.strings = na_vals)
c1hmps <- read.csv(tidy_tempdat[grep("c1_hmp", tidy_tempdat)], na.strings = na_vals)
c1k_temp <- getTabular(185) %>% data.frame()

# sdl
## chart-logger time series (ctw screened hmp dat 2018-2020)
sdlfrank <- read.csv(paste0(datpath, "data/sdl_temp_1981-2020_draft.csv"), na.strings = na_vals)

# other dats
# ameriflux US NR1 (C1 forest) -- on CTW hard drive bc big file
ameriforest <- read.csv("../../Documents/nwt_lter/aux_climdats/AmeriFlux/forest_prepped2compare.csv", na.strings = na_vals)
# nwt snotel
snotel <- read.csv(paste0(datpath, "data/prep_data/nwt_snotel_temp.csv"), na.strings = na_vals)
# gl4
gl4 <- read.csv(paste0(datpath, "data/prep_data/ready_for_qa/gl4_tidy.csv"), na.strings = na_vals)


# -- INFILL FUNCTIONS -----
# > adapt code from infill_and_compile_d1_c1_temp.R in the long-term-trends repo

# function for short-term seasonal tk infill method
tk_movingfill <- function(dat, target_site, missing_dates, site_order, window_days=13, metric = c("tmean", "DTR"), nobs_limit = 14){
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
        if(length(logger)>1){
          next
        }
        logger <- logger[!is.na(logger)] %>% str_flatten(collapse = " ")
      } else{
        logger <- NA
      }
      mod <- lm(formula = paste0(target_site, "_tmean ~ ", ycol), data = temp_df)
      r2_df <- rbind(r2_df, 
                     data.frame(cbind(site = site,
                                      logger = logger, 
                                      nobs = nrow(mod$model),
                                      r2 = summary(mod)$r.squared, # TK used r2 not adjusted r2
                                      pval = summary(mod)$coefficients[8])))
      
    }
    
    # make numeric cols numeric
    r2_df$nobs <- as.numeric(r2_df$nobs)
    r2_df$r2 <- as.numeric(r2_df$r2)
    r2_df$pval <- as.numeric(r2_df$pval)
    
    # remove any row that has an r2 of 1 (unrealistic) and filter out any model nobs less than limit specified (TK has a min of 14)
    if(nrow(r2_df)>0){
      r2_df <- subset(r2_df, r2 != 1 & nobs >= nobs_limit)
    }
    # select best
    best <- subset(r2_df, pval <= 0.05 & r2 == max(r2, na.rm = T))
    # if nothing has signif pval, select best r2
    if(nrow(best) == 0){
      best <- subset(r2_df, r2 == max(r2, na.rm = T))
    }
    
    
    # infill missing values in time_window based on best model
    for(m in metric){
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
    }
    
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

# build multi-year regression
tk_historicfill <- function(dat, target_site, missing_dates, site_order, metric = c("tmean", "DTR"), nobs_limit = 14){
  
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
      # add 366 to be sure
      doyrange <- sort(c(doyrange, 366))
    }
    
    # iterate through infill hiearchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in site_order){
      # subset dat
      temp_df <- subset(dat, doy %in% doyrange)
      if(grepl("cr", site)){
        # id logger col
        logcol <- paste0(gsub("cr", "", site), "_logger") 
        #id logger deployed at time of missing observation
        logger <- dat[[logcol]][dat$date == d]
        #subset to that logger
        temp_df <- filter(temp_df, get(logcol) == logger)
      } else{
        logger <- NA
      }
      ycol <- paste0(site, "_tmean")
      # check there are observations > 0 for explanatory site
      if(all(is.na(temp_df[ycol]))){
        next
      }
      # check that there is a value for x on the missing date
      if(is.na(temp_df[[ycol]][temp_df$date == d])){
        next
      }
      # check that days in temp_df y have value, x has value (i.e., there are data points to relate)
      ydates <- temp_df$date[!is.na(temp_df[[ycol]])]
      xdates <- temp_df$date[!is.na(temp_df[[paste0(target_site,"_tmean")]])]
      if(length(xdates[xdates %in% ydates]) < nobs_limit){
        next
      }
      # run lm model and store results
      mod <- lm(formula = paste0(target_site,"_tmean ~ ", ycol), data = temp_df)
      r2_df <- rbind(r2_df, 
                     data.frame(cbind(site = site,
                                      nobs = nrow(mod$model),
                                      logger = logger, 
                                      r2 = summary(mod)$r.squared,
                                      pval = summary(mod)$coefficients[8])))
      
    }
    
    # make stored r2 and pval numeric
    r2_df$r2 <- as.numeric(r2_df$r2) 
    r2_df$nobs <- as.numeric(r2_df$nobs) 
    r2_df$pval <- as.numeric(r2_df$pval)
    
    # remove any row that has an r2 of 1 (unrealistic) and filter out any model nobs less than limit specified (TK has a min of 14)
    if(nrow(r2_df)>0){
      r2_df <- subset(r2_df, r2 != 1 & nobs >= nobs_limit)
    }
    # if r2_df empty because no models could be run, state that in infill df and move on
    if(nrow(r2_df) == 0){
      tempinfill_df <- data.frame(date = d, infill = NA,
                                  metric = m,
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
      logcol <- paste0(gsub("cr", "", best$site), "_logger") 
      #subset to that logger
      temp_df <- filter(temp_df, get(logcol) == best$logger)
    } else{
      logger <- NA
    }
    # iterate through mean T and DTR, run lm, predict missing values, and store results in data frame
    for(m in metric){
      best_mod <- lm(formula = paste0(target_site, "_", m, " ~ ", best$site, "_", m), data = temp_df)
      tempinfill <- predict(best_mod, newdata = subset(dat, date == d))
      tempinfill_df <- data.frame(date = d, infill = tempinfill,
                                  metric = m,
                                  source.station = best$site,
                                  logger = best$logger, 
                                  pval = summary(best_mod)$coefficients[8],
                                  r2 = summary(best_mod)$r.squared, #TK used r2 not adjusted r2 (CTW likes adjusted r2)
                                  n.obs = nrow(best_mod$model),
                                  equation = paste0("y = ", best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                  infillrun = length(d),
                                  method = "multi-yr")
      
      # append model infill to master infill df
      infill_df_m2 <- rbind(infill_df_m2, tempinfill_df)
    }
    
    # clean up things that shouldn't be recycled
    rm(r2_df, tempinfill_df, best, logger)
    
  }
  # return infilled df
  return(infill_df_m2)
}


# group days by the infill event in shortfill, average pval and r2 and if individuals pvals <= 0.05 and mean pval <= 0.05, select method with highest average r2

# append infill event to longfill data frame for comparative purposes
compare_results <- function(dat_longfill, dat_shortfill){
  # start comparative dataframe
  dat_compare <- left_join(dat_longfill, dat_shortfill[c("date", "metric", "infillevent")]) %>%
    # append method 1 results to method 2
    rbind(dat_shortfill) %>%
    # create col for indicating whether selected or dropped
    ## also flag for models where nobs < # days infilled (compare if those would get dropped through mean approach)
    mutate(flag_mod = ifelse(infillrun > n.obs, TRUE, FALSE),
           flag_pval = ifelse(pval > 0.05, TRUE, FALSE))
  return(dat_compare)
}

# iterate by infill event and compare/select best model according to TK criteria
select_model <- function(dat_compare){
  
  # take overall means of pval and r2 by infill event
  dat_means <- group_by(dat_compare, infillevent, method) %>%
    summarise(mean_pval = mean(pval),
              mean_r2 = mean(r2)) %>%
    mutate(mean_pval_flag = ifelse(mean_pval < 0.05, FALSE, TRUE),
           r2_check1 = ifelse(mean_pval_flag == TRUE, 0, NA)) %>%
    ungroup() %>%
    # append model flag to indicate mods where nobs < days infilled
    left_join(distinct(dat_compare[c("infillevent", "method", "flag_mod")]))
  # specify r2_check1 as 1 if pair has 0
  # which events failed pval check?
  fail_events <- dat_means$infillevent[dat_means$r2_check1 == 0] %>% na.exclude()
  for(f in fail_events){
    dat_means$r2_check1[dat_means$infillevent == f & is.na(dat_means$r2_check1)] <- 1
  }
  # add second r2 check based on max r2
  dat_means <- group_by(dat_means, infillevent) %>%
    mutate(r2_check2 = mean_r2 == max(mean_r2)) %>%
    ungroup() %>%
    # select based on highest r2, as long as mean pval and model not flagged
    mutate(selectmod = ifelse(mean_pval_flag == TRUE | flag_mod == TRUE, FALSE, ## those that are flagged aren't selected outright
                              ifelse(mean_pval_flag == FALSE & flag_mod == FALSE & r2_check2 == TRUE, TRUE, FALSE))) ## those that pass flag checks and have highest r2 are selected outright
  # iterate through infill events that don't have a model selected and choose
  missingselect <- group_by(dat_means, infillevent) %>%
    summarise(sumselect = sum(selectmod)) %>%
    filter(sumselect == 0)
  for(m in missingselect$infillevent){
    temp_dat <- subset(dat_means, infillevent == m)
    temp_method <- temp_dat$method[temp_dat$flag_mod == F & temp_dat$mean_pval_flag == F]
    dat_means$selectmod[dat_means$infillevent == m & dat_means$method == temp_method] <- TRUE
  }
  return(dat_means)
}


# -- REVIEW RAW DATA -----
# how many days are missing at C1 and D1 chart 2019-2020?
subset(c1chart, yr >2018 & grepl("ma|mi", metric)) %>%
  group_by(yr, mon, metric) %>%
  summarise(missing = sum(is.na(temp))) %>%
  filter(missing > 0) %>% data.frame() %>%
  spread(metric, missing)
# 2019: jan, jun, nov, dec
# 2020: jan, jul, oct, dec.. the most missing these last three

# are data available in c1 hmps? (starts in 2017!)
subset(c1hmps, yr > 2018 & grepl("ma|mi", metric)) %>%
  group_by(yr, mon, metric, sensor) %>%
  summarise(missing = sum(is.na(temp))) %>%
  filter(missing > 0) %>% data.frame() # !all data present
# how many flags?
subset(c1hmps, yr > 2018 & grepl("ma|mi", metric)) %>%
  group_by(yr, metric, flag, sensor) %>%
  summarise(nobs = length(temp)) # vals questionable at hmps for 1 day in 2020 -- can check against sdl if an issue

# d1 review
subset(d1chart, yr >2018 & grepl("ma|mi", metric)) %>%
  group_by(yr, mon, metric) %>%
  summarise(missing = sum(is.na(temp))) %>%
  filter(missing > 0) %>% data.frame() %>%
  spread(metric, missing) # more missing vals than at c1..
# 2019: jan-may, aug, oct, dec
# 2020: jan-apr, sep

# are data available in d1 hmps? (starts in 2017!)
subset(d1hmps, yr > 2018 & grepl("ma|mi", metric)) %>%
  group_by(yr, mon, metric, sensor) %>%
  summarise(missing = sum(is.na(temp))) %>%
  filter(missing > 0) %>% data.frame() # yikes.
# 2019: jan + feb largely missing, all else there
# 2020: 1 day in june, almost half of july -- but chart has those covered

# how many flags?
subset(d1hmps, yr > 2018 & grepl("ma|mi", metric)) %>%
  group_by(yr, metric, flag, sensor) %>%
  summarise(nobs = length(temp)) # vals que

# > seems like treating c1 will be easier so start there


# -- QUICK QA HMPS -----
# all expected dates there?
check_datetime(c1hmps)
check_datetime(d1hmps)
check_datetime(subset(c1chart, yr > 2018))
check_datetime(subset(d1chart, yr > 2018)) # all there

# check maxmin
c1hmp_maxmin <- subset(c1hmps, grepl("max|min", metric), select = -flag) %>%
  spread(metric, temp) %>%
  flag_limits(., maxcol = "airtemp_max", mincol = "airtemp_min", minval = -35, maxval = 30) # one high val of 31 in 2017, which maybe
d1hmp_maxmin <- subset(d1hmps, grepl("max|min", metric), select = -flag) %>%
  spread(metric, temp) %>%
  flag_limits(., maxcol = "airtemp_max", mincol = "airtemp_min", minval = -38, maxval = 25) # ok
# check chart
c1chart_maxmin <- subset(c1chart, yr > 2018, select = -flag) %>%
  spread(metric, temp) %>%
  flag_limits(., maxcol = "airtemp_max", mincol = "airtemp_min", minval = -35, maxval = 30) # 1 case of min > max on 2020-01-07
d1chart_maxmin <- subset(d1chart, yr > 2018, select = -flag) %>%
  spread(metric, temp) %>%
  flag_limits(., maxcol = "airtemp_max", mincol = "airtemp_min", minval = -38, maxval = 25) # ok

# don't expect flatlines in hmps but check anyway
c1hmps_flat <-  flag_flatlines(subset(c1hmps, yr > 2018), metric = "temp", groupvars = c("metric", "sensor"))
d1hmps_flat <-  flag_flatlines(subset(d1hmps, yr > 2018), metric = "temp", groupvars = c("metric", "sensor"))
# check for flatlines in chart dats
c1chart_flat <- flag_flatlines(subset(c1chart, yr > 2018), metric = "temp", groupvars = "metric")
d1chart_flat <- flag_flatlines(subset(d1chart, yr > 2018), metric = "temp", groupvars = "metric")

# check vals outside 4-5 sds
## c1 hmps
c1hmps_devs_withinsensor <- rename(c1hmps, met = "metric") %>% flag_deviations(metric = "temp", groupvars = c("sensor", "met"))
c1hmps_devs_xsensor <- rename(c1hmps, met = "metric") %>% flag_deviations(metric = "temp", groupvars = c("met", "mon"))
# > hmp3 on 2017-06-13 is no good for tmin or DTR (scratch all for that day to be sure)
c1hmps_devs_xsensor_withinyr <- rename(c1hmps, met = "metric") %>% flag_deviations(metric = "temp", groupvars = c("met", "yr"))
# > same day gets flagged

## d1 hmps
d1hmps_devs_withinsensor <- rename(d1hmps, met = "metric") %>% flag_deviations(metric = "temp", groupvars = c("sensor", "met"))
d1hmps_devs_xsensor <- rename(d1hmps, met = "metric") %>% flag_deviations(metric = "temp", groupvars = c("met", "mon"))
d1hmps_devs_xsensor_withinyr <- rename(d1hmps, met = "metric") %>% flag_deviations(metric = "temp", groupvars = c("met", "yr"))
# same dates get flagged

## chart -- use all years for more of a dist but only care about flags 2019-2020
c1chart_devs <- rename(c1chart, met = "metric") %>% flag_deviations(metric = "temp", groupvars = c("met", "mon")) %>%
  subset(yr > 2018) # 2 cold vals at end of oct 2020, but looks like it was a short cold spell
d1chart_devs <- rename(d1chart, met = "metric") %>% flag_deviations(metric = "temp", groupvars = c("met", "mon")) %>%
  subset(yr > 2018) # 2019 Aug 7.. min = max (16C).. next day has min of 14C.. check against hmps


# check spikes
c1hmp_spikes <- rename(c1hmps, met = "metric") %>% flag_spikes(metric = "temp", groupvars = c("sensor", "met"))
d1hmp_spikes <- rename(d1hmps, met = "metric") %>% flag_spikes(metric = "temp", groupvars = c("sensor", "met"))
c1chart_spikes <- rename(c1chart, met = "metric") %>% flag_spikes(metric = "temp", groupvars = c("met", "mon")) %>%
  subset(yr > 2018) # one tmax 2020-09-08
d1chart_spikes <- rename(d1chart, met = "metric") %>% flag_spikes(metric = "temp", groupvars = c("met", "mon")) %>%
  subset(yr > 2018) # all clear


# average hmps for regression
c1hmp_means <- c1hmps %>%
  group_by(date, yr, mon, doy, metric) %>%
  summarise(mean_hmp = mean(temp, na.rm = T),
            se_hmp = sd(temp, na.rm = T)/sqrt(length(temp[!is.na(temp)])),
            nobs = length(temp[!is.na(temp)]),
            .groups = "drop_last")
boxplot(c1hmp_means$se_hmp ~ c1hmp_means$metric) # big range on tmin for c1
# does it change over time?
ggplot(c1hmp_means, aes(factor(yr), se_hmp)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~metric) # you bet. looks like issues got worked out by 2019-2020

d1hmp_means <- d1hmps %>%
  group_by(date, yr, mon, doy, metric) %>%
  summarise(mean_hmp = mean(temp, na.rm = T),
            se_hmp = sd(temp, na.rm = T)/sqrt(length(temp[!is.na(temp)])),
            nobs = length(temp[!is.na(temp)]),
            .groups = "drop_last") %>%
  data.frame() %>%
  # replace any NaNs with NA
  mutate_all(.funs = function(x) ifelse(is.nan(x), NA, x))
boxplot(d1hmp_means$se_hmp ~ d1hmp_means$metric) # mostly small
# does it change over time?
ggplot(d1hmp_means, aes(factor(yr), se_hmp)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~metric) 


# -- PREP DATA FOR INFILL ----
# each site/source in its own column
# want tmean and DTR, with "[site/source name]_" prefixed
# start at jan 1 2018 so hmps used across sites consistent temporally (2017 hmp dat c1 seems to vary widely anyway)

## c1 chart
# append kittel dataset for more obs, only vals not infilled or adjusted (i.e., good raw data)
c1k_goodraw <- subset(c1k_temp, flag_1 == "A" & flag_2 == "A" & flag_3 == "A") %>% # decide to keep data adjusted for artificial drops
  rename(c1_tmean = mean_temp, c1_DTR = DTR, yr = year) %>%
  mutate(mon = month(date), doy = yday(date)) %>%
  subset(select = c(date, yr, mon, doy, c1_tmean, c1_DTR))
# chart since 2018
# > note: for some reason dec mean temps missing in 2019 (but data available)
c1_prep <- subset(c1chart, yr > 2018 , select = -c(data_source,flag)) %>% #
  mutate(metric = gsub("airtemp_avg", "tmean", metric)) %>%
  unite(met, local_site, metric) %>%
  spread(met, temp) %>%
  mutate(c1_tmean = (c1_airtemp_max + c1_airtemp_min)/2, # #recrunch c1 mean to be sure
         flagmin =  c1_airtemp_min > c1_airtemp_max)
c1_prep[which(c1_prep$flagmin), grepl("temp|mean|DTR", names(c1_prep))] <- NA
c1_prep <- subset(c1_prep, select = -c(c1_airtemp_max, c1_airtemp_min, flagmin)) %>% arrange(date) %>% data.frame()

# rbind c1k and c1prep
c1_prep <- rbind(c1k_goodraw, c1_prep) %>%
  arrange(date)

## d1 chart
d1k_goodraw <- subset(d1k_temp, flag_1 == "A" & flag_2 == "A" & flag_3 == "A") %>% # decide to keep data adjusted for artificial drops
  rename(d1_tmean = mean_temp, d1_DTR = DTR, yr = year) %>%
  mutate(mon = month(date), doy = yday(date)) %>%
  subset(select = c(date, yr, mon, doy, d1_tmean, d1_DTR))
# d1chart since 2018
d1_prep <- subset(d1chart, yr > 2018, select = -c(data_source,flag)) %>%
  mutate(metric = gsub("airtemp_avg", "tmean", metric)) %>%
  unite(met, local_site, metric) %>%
  spread(met, temp) %>%
  #recrunch mean and DTR to be sure
  mutate(d1_DTR = d1_airtemp_max - d1_airtemp_min,
         d1_tmean = (d1_airtemp_max + d1_airtemp_min)/2,
         flagmin =  d1_airtemp_min > d1_airtemp_max) %>%
  subset(select = -c(d1_airtemp_max, d1_airtemp_min, flagmin)) %>% arrange(date) %>% data.frame()
  
  
# rbind d1k and d1prep
d1_prep <- rbind(d1k_goodraw, d1_prep) %>%
  arrange(date)

## hmps
c1hmp_prep <- subset(c1hmp_means, yr > 2017 & grepl("avg|DTR", metric), select = -c(se_hmp,nobs)) %>%
  mutate(metric = gsub("airtemp_avg", "tmean", metric),
         metric = paste0("c1hmp_", metric),
         date = as.Date(date)) %>%
  spread(metric, mean_hmp) %>%
  data.frame()
d1hmp_prep <- subset(d1hmp_means, yr > 2017 & grepl("avg|DTR", metric), select = -c(se_hmp,nobs)) %>%
  mutate(metric = gsub("airtemp_avg", "tmean", metric),
         metric = paste0("d1hmp_", metric),
         date = as.Date(date)) %>%
  spread(metric, mean_hmp) %>% data.frame()
sdlhmp_prep <- subset(sdlfrank, yr > 2017, select = c("date", "infilled", "metric", "adjusted_airtemp")) %>%
  # NA any vals infilled by other stations
  mutate(adjusted_airtemp = ifelse(infilled, NA, adjusted_airtemp)) %>%
  spread(metric, adjusted_airtemp) %>%
  mutate(sdlhmp_DTR = max - min,
         date = as.Date(date)) %>%
  rename(sdlhmp_tmean = mean) %>%
  subset(select = -c(max, min, infilled))
  
# prep other dats
gl4_prep <- gl4 %>%
  mutate(metric = gsub("airtemp_", "", metric),
         date = as.Date(date)) %>%
  spread(metric, temp) %>%
  rename(gl4cr_DTR = DTR, gl4cr_tmean = avg, gl4_logger = logger) %>%
  subset(select = c(date, gl4_logger, gl4cr_tmean, gl4cr_DTR))
  
sno_prep <- snotel %>%
  mutate(met = gsub("airtemp_", "", met)) %>%
  spread(met, tempC) %>%
  mutate(snotel_tmean = (max+min)/2,
         snotel_DTR = max-min,
         date = as.Date(date)) %>%
  subset(select = c(date, snotel_tmean, snotel_DTR))

nr1_prep <- ameriforest %>%
  spread(met, airtemp) %>%
  mutate(nr1_DTR = max-min,
         date = as.Date(date)) %>%
  rename(nr1_tmean = mean) %>%
  subset(select = c(date, nr1_tmean, nr1_DTR))

# stack all
master_temp <- left_join(c1_prep, d1_prep) %>%
  left_join(c1hmp_prep) %>%
  left_join(d1hmp_prep) %>%
  left_join(sdlhmp_prep) %>%
  left_join(sno_prep) %>%
  left_join(nr1_prep) %>%
  left_join(subset(gl4_prep, select = -gl4_logger)) %>%
  # add logger dat back in (only applies to gl4)
  left_join(gl4_prep[c("date", "gl4_logger")])
  

# -- INFILL ----
# get code functional

# c1 ----
# specify d1 missing dates and hierarchy of source station
c1missing_dates <- with(master_temp, date[is.na(c1_tmean) & yr > 2018])
c1_infill_order <- c("c1hmp", "snotel", "nr1", "sdlhmp", "gl4cr", "d1hmp", "d1")

# infill c1 chart by 2wk moving window
# > update: try 3wk moving window bc fewer years hmp data to compare
c1_shortfill <- tk_movingfill(master_temp, "c1", c1missing_dates, c1_infill_order, window_days = 20)
# infill c1 chart by historic values
c1_longfill <- tk_historicfill(master_temp, "c1", c1missing_dates, c1_infill_order)

c1_compare <- compare_results(c1_longfill, c1_shortfill)
c1_means <- select_model(c1_compare)
# does every event have a model selected?
length(unique(c1_means$infillevent)) == length(c1_means$selectmod[c1_means$selectmod])

# join selection results to c1_compare
c1_compare2 <- left_join(c1_compare, c1_means)
c1_choose <- subset(c1_compare2, selectmod == TRUE) %>%
  arrange(date, metric) %>%
  dplyr::select(-c(infillrun, infillevent:selectmod)) %>%
  unite(source.station, source.station, logger) 
# pull out lm results to work on calculating tmin and tmax, join results later on
c1_lminfo <- dplyr::select(c1_choose, date, metric:method) %>%
  # spread it by metric (DTR or tmean)
  gather(met, val, pval:equation) %>% # all cols coerced to character class
  unite(metric, metric, met) %>%
  spread(metric, val)

# make cols in c1_lminfo that should be numeric, numeric. all cols got coerced to character in gather statement
c1_lminfo[,grep("obs|r2|pval", colnames(c1_lminfo))] <- sapply(c1_lminfo[,grep("obs|r2|pval", colnames(c1_lminfo))], as.numeric)
# check all looks good
str(c1_lminfo) # yes

c1_lmtemps <- dplyr::select(c1_choose, date:source.station) %>%
  spread(metric, infill) %>%
  # date to char to join
  mutate(date = as.character(date)) %>%
  # append cols for tmin, tmax
  left_join(c1chart[c1chart$met == "airtemp_max", c("date", "temp")]) %>%
  rename(tmax = temp) %>%
  left_join(c1chart[c1chart$met == "airtemp_min", c("date", "temp")]) %>%
  # date back to Date
  mutate(date = as.Date(date)) %>%
  rename(tmin = temp) %>%
  ungroup() %>%
  mutate(DTR = as.numeric(DTR),
         tmean = as.numeric(tmean),
         proj.tmax = tmean + (DTR/2),
         proj.tmin = tmean - (DTR/2),
         adj.tmax = ifelse(!is.na(tmin), tmin + DTR, NA),
         adj.tmin = ifelse(!is.na(tmax), tmax - DTR, NA),
         final_tmax = ifelse(!is.na(tmax), tmax, 
                             ifelse(!is.na(adj.tmax), adj.tmax, proj.tmax)),
         final_tmin = ifelse(!is.na(tmin), tmin, 
                             ifelse(!is.na(adj.tmin), adj.tmin, proj.tmin)))

#verify all dates present
summary(c1missing_dates %in% c1_lmtemps$date)

# rbind triaged temps back with other predicted temps
final_c1_pred <-c1_lmtemps %>%
  #add flag 2 (B = unadjusted infilling)
  mutate(flag.2 = "B",
         infill_QA_note = NA) %>%
  # keep date, source.station, mean T , DTR, and final tmin and tmax vals
  dplyr::select(date, source.station, final_tmax, final_tmin, tmean, DTR, flag.2, infill_QA_note) %>%
  rename(tmax = final_tmax,
         tmin = final_tmin) %>%
  # sort by date
  arrange(date) %>%
  # append lm info to each infilled date
  left_join(c1_lminfo, by = c("date", "source.station")) %>%
  # add flag 1 and flag 3
  # > Flag 1: A=no infilling; B=method 1 p<.05; C=method 1 p>.05; D=method 2 p<.05; E=method 2 p>.05
  # > Flag 3: A=no infilling; B=DTR p<.05; C=DTR 1 p>.05
  mutate(flag.1 = ifelse(grepl("window", method) & tmean_pval <= 0.05, "B",
                         ifelse(grepl("window", method) & tmean_pval > 0.05, "C",
                                ifelse(grepl("multi-yr", method) & tmean_pval <= 0.05, "D", "E"))),
         flag.3 = ifelse(DTR_pval <= 0.05, "B", "C"),
         year = year(date),
         month = month(date),
         day = day(date),
         doy = yday(date),
         LTER_site = "NWT",
         local_site = "C1") %>%
  #rearrange cols in order of tk dataset
  dplyr::select(LTER_site, local_site, date, doy,year:day, tmax:DTR, flag.1, flag.2, flag.3, source.station,
                tmean_pval, tmean_r2, tmean_n.obs, tmean_equation, DTR_pval, DTR_r2, DTR_n.obs, DTR_equation, infill_QA_note) %>%
  # rename to match EDI colnames
  rename(max_temp = tmax, min_temp = tmin, mean_temp = tmean, flag_1 = flag.1, flag_2 = flag.2, flag_3 = flag.3,
         source_station = source.station, t_mean_pvalue= tmean_pval, t_mean_rsquared = tmean_r2, 
         num_obs_in_t_mean_regression_equation = tmean_n.obs, t_mean_regression_equation = tmean_equation,
         TDTR_pvalue = DTR_pval, TDTR_rsquared = DTR_r2, num_obs_in_TDTR_regression_equation = DTR_n.obs,
         TDTR_regression_equation = DTR_equation)

# prep d1chart to join
c1chart_join <- subset(c1chart, year(date) > 2018 & grepl("min|max", metric), select = c(date, metric, temp, flag)) %>%
  mutate(metric = gsub("airtemp_", "", metric)) %>%
  gather(thing, val, temp, flag) %>%
  unite(metric, metric, thing) %>%
  spread(metric, val) %>%
  rename(Tmax_QAflag = max_flag, Tmin_QAflag = min_flag, raw_Tmax= max_temp, raw_Tmin = min_temp) %>%
  mutate(date = as.Date(date))

final_c1_pred2 <- left_join(final_c1_pred, d1chart_join) %>%
  arrange(date) %>%
  subset(select = names(c1k_temp)) %>%
  #clean up source_stations
  mutate(source_station = casefold(gsub("_NA", "", source_station), upper = T),
         source_station = gsub("HMP", " HMP", source_station),
         source_station = gsub("NR1", "AmeriFlux US-NR1", source_station),
         source_station = gsub("SNOTEL", "Niwot Snotel (663)", source_station))

# prep chart dat that didn't need infill
c1chart_rawtojoin <- subset(c1chart, yr > 2018) %>%
  # add cols in TK dataset
  mutate(day = day(date),
         metric = gsub("airtemp_", "", metric),
         metric = gsub("avg", "mean", metric),
         local_site = "C1",
         source_station= "C1",
         date = as.Date(date)) %>%
  rename(year = yr) %>%
  gather(thing, val, temp, flag) %>%
  unite(metric, metric, thing) %>%
  mutate(metric = gsub("DTR_temp", "DTR", metric)) %>%
  spread(metric, val) %>%
  subset(select = -c(DTR_flag, mean_flag)) %>%
  rename(Tmax_QAflag = max_flag, Tmin_QAflag = min_flag) %>%
  # reclass temps as numeric
  mutate_at(vars("max_temp", "min_temp", "mean_temp", "DTR"), as.numeric) %>%
  mutate(raw_Tmax = max_temp, raw_Tmin = min_temp,
         # recrunch tmean so all there
         mean_temp = (max_temp + min_temp)/2,
         flag_1 = "A",
         flag_2 = "A",
         flag_3 = "A",
         infill_QA_note = NA,
         LTER_site = "NWT") %>%
  #remove dates that have been infilled
  filter(!date %in% final_c1_pred$date) %>%
  #append cols for lminfo
  cbind(data.frame(matrix(ncol = 8, nrow = nrow(.)))) %>%
  rename_at(vars(matches("^X")), function(x) x <- colnames(final_c1_pred2)[grep("pval|rsq|equ|num_obs", colnames(final_c1_pred2))]) %>%
  #rearrange cols to match tk's
  dplyr::select(colnames(final_c1_pred2))

c1out <- rbind(c1k_temp, final_c1_pred2, c1chart_rawtojoin) %>%
  arrange(date)

# review
c1out %>%
  subset(select = c(date, source_station, max_temp:DTR)) %>%
  gather(met, val, max_temp:DTR) %>%
  ggplot(aes(date, val, col = source_station)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~met)

View(subset(c1out, is.na(DTR) | is.na(mean_temp))) # all there
summary(c1out)

#verify all dates present
check_datetime(c1out)


# d1 ----
# specify d1 missing dates and hierarchy of source station
d1missing_dates <- with(master_temp, date[is.na(d1_tmean) & yr > 2018])
d1_infill_order <- c("d1hmp", "sdlhmp", "gl4cr", "c1hmp", "c1", "snotel", "nr1")

# infill d1 chart by 2wk moving window
d1_shortfill <- tk_movingfill(master_temp, "d1", d1missing_dates, d1_infill_order, window_days = 20)
# infill d1 chart by historic values
d1_longfill <- tk_historicfill(master_temp, "d1", d1missing_dates, d1_infill_order)

d1_compare <- compare_results(d1_longfill, d1_shortfill)
d1_means <- select_model(d1_compare)
# does every event have a model selected?
length(unique(d1_means$infillevent)) == length(d1_means$selectmod[d1_means$selectmod])

# join selection results to c1_compare
d1_compare <- left_join(d1_compare, d1_means)
d1_choose <- subset(d1_compare, selectmod == TRUE) %>%
  arrange(date, metric) %>%
  dplyr::select(-c(infillrun, infillevent:selectmod)) %>%
  unite(source.station, source.station, logger) 
# pull out lm results to work on calculating tmin and tmax, join results later on
d1_lminfo <- dplyr::select(d1_choose, date, metric:method) %>%
  # spread it by metric (DTR or tmean)
  gather(met, val, pval:equation) %>% # all cols coerced to character class
  unite(metric, metric, met) %>%
  spread(metric, val)
# make cols in d1_lminfo that should be numeric, numeric. all cols got coerced to character in gather statement
d1_lminfo[,grep("obs|r2|pval", colnames(d1_lminfo))] <- sapply(d1_lminfo[,grep("obs|r2|pval", colnames(d1_lminfo))], as.numeric)
# check all looks good
str(d1_lminfo) # yes

d1_lmtemps <- dplyr::select(d1_choose, date:source.station) %>%
  spread(metric, infill) %>%
  # date to char to join
  mutate(date = as.character(date)) %>%
  # append cols for tmin, tmax
  left_join(d1chart[d1chart$met == "airtemp_max", c("date", "temp")]) %>%
  rename(tmax = temp) %>%
  left_join(d1chart[d1chart$met == "airtemp_min", c("date", "temp")]) %>%
  # date back to Date
  mutate(date = as.Date(date)) %>%
  rename(tmin = temp) %>%
  ungroup() %>%
  mutate(DTR = as.numeric(DTR),
         tmean = as.numeric(tmean),
         proj.tmax = tmean + (DTR/2),
         proj.tmin = tmean - (DTR/2),
         adj.tmax = ifelse(!is.na(tmin), tmin + DTR, NA),
         adj.tmin = ifelse(!is.na(tmax), tmax - DTR, NA),
         final_tmax = ifelse(!is.na(tmax), tmax, 
                             ifelse(!is.na(adj.tmax), adj.tmax, proj.tmax)),
         final_tmin = ifelse(!is.na(tmin), tmin, 
                             ifelse(!is.na(adj.tmin), adj.tmin, proj.tmin)))


# rbind triaged temps back with other predicted temps
final_d1_pred <-d1_lmtemps %>%
  #add flag 2 (B = unadjusted infilling)
  mutate(flag.2 = "B",
         infill_QA_note = NA) %>%
  # keep date, source.station, mean T , DTR, and final tmin and tmax vals
  dplyr::select(date, source.station, final_tmax, final_tmin, tmean, DTR, flag.2, infill_QA_note) %>%
  rename(tmax = final_tmax,
         tmin = final_tmin) %>%
  # sort by date
  arrange(date) %>%
  # append lm info to each infilled date
  left_join(d1_lminfo, by = c("date", "source.station")) %>%
  # add flag 1 and flag 3
  # > Flag 1: A=no infilling; B=method 1 p<.05; C=method 1 p>.05; D=method 2 p<.05; E=method 2 p>.05
  # > Flag 3: A=no infilling; B=DTR p<.05; C=DTR 1 p>.05
  mutate(flag.1 = ifelse(grepl("window", method) & tmean_pval <= 0.05, "B",
                         ifelse(grepl("window", method) & tmean_pval > 0.05, "C",
                                ifelse(grepl("multi-yr", method) & tmean_pval <= 0.05, "D", "E"))),
         flag.3 = ifelse(DTR_pval <= 0.05, "B", "C"),
         year = year(date),
         month = month(date),
         day = day(date),
         doy = yday(date),
         LTER_site = "NWT",
         local_site = "D1") %>%
  #rearrange cols in order of tk dataset
  dplyr::select(LTER_site, local_site, date, doy,year:day, tmax:DTR, flag.1, flag.2, flag.3, source.station,
                tmean_pval, tmean_r2, tmean_n.obs, tmean_equation, DTR_pval, DTR_r2, DTR_n.obs, DTR_equation, infill_QA_note) %>%
  # rename to match EDI colnames
  rename(max_temp = tmax, min_temp = tmin, mean_temp = tmean, flag_1 = flag.1, flag_2 = flag.2, flag_3 = flag.3,
         source_station = source.station, t_mean_pvalue= tmean_pval, t_mean_rsquared = tmean_r2, 
         num_obs_in_t_mean_regression_equation = tmean_n.obs, t_mean_regression_equation = tmean_equation,
         TDTR_pvalue = DTR_pval, TDTR_rsquared = DTR_r2, num_obs_in_TDTR_regression_equation = DTR_n.obs,
         TDTR_regression_equation = DTR_equation)

# prep d1chart to join
d1chart_join <- subset(d1chart, year(date) > 2018 & grepl("min|max", metric), select = c(date, metric, temp, flag)) %>%
  mutate(metric = gsub("airtemp_", "", metric)) %>%
  gather(thing, val, temp, flag) %>%
  unite(metric, metric, thing) %>%
  spread(metric, val) %>%
  rename(Tmax_QAflag = max_flag, Tmin_QAflag = min_flag, raw_Tmax= max_temp, raw_Tmin = min_temp) %>%
  mutate(date = as.Date(date))

final_d1_pred2 <- left_join(final_d1_pred, d1chart_join) %>%
  arrange(date) %>%
  subset(select = names(d1k_temp)) %>%
  #clean up source_stations
  mutate(source_station = casefold(gsub("_NA", "", source_station), upper = T),
         source_station = gsub("HMP", " HMP", source_station),
         source_station = gsub("NR1", "AmeriFlux US-NR1", source_station))

# prep chart dat that didn't need infill
d1chart_rawtojoin <- subset(d1chart, yr > 2018) %>%
  # add cols in TK dataset
  mutate(day = day(date),
         metric = gsub("airtemp_", "", metric),
         metric = gsub("avg", "mean", metric),
         local_site = "D1",
         source_station= "D1",
         date = as.Date(date)) %>%
  rename(year = yr) %>%
  gather(thing, val, temp, flag) %>%
  unite(metric, metric, thing) %>%
  mutate(metric = gsub("DTR_temp", "DTR", metric)) %>%
  spread(metric, val) %>%
  subset(select = -c(DTR_flag, mean_flag)) %>%
  rename(Tmax_QAflag = max_flag, Tmin_QAflag = min_flag) %>%
  # reclass temps as numeric
  mutate_at(vars("max_temp", "min_temp", "mean_temp", "DTR"), as.numeric) %>%
  mutate(raw_Tmax = max_temp, raw_Tmin = min_temp,
         flag_1 = "A",
         flag_2 = "A",
         flag_3 = "A",
         infill_QA_note = NA,
         LTER_site = "NWT") %>%
  #remove dates that have been infilled
  filter(!date %in% final_d1_pred$date) %>%
  #append cols for lminfo
  cbind(data.frame(matrix(ncol = 8, nrow = nrow(.)))) %>%
  rename_at(vars(matches("^X")), function(x) x <- colnames(final_d1_pred2)[grep("pval|rsq|equ|num_obs", colnames(final_d1_pred2))]) %>%
  #rearrange cols to match tk's
  dplyr::select(colnames(final_d1_pred2))

d1out <- rbind(d1k_temp, final_d1_pred2, d1chart_rawtojoin) %>%
  arrange(date) %>%
  data.frame()

# review
d1out %>%
  subset(select = c(date, source_station, max_temp:DTR)) %>%
  gather(met, val, max_temp:DTR) %>%
  ggplot(aes(date, val, col = source_station)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~met)

View(subset(d1out, is.na(DTR) | is.na(mean_temp)))
summary(d1out)
#verify all dates present
check_datetime(d1out)


# -- WRITE OUT ----
write_csv(d1out, paste0(datpath, "data/d1_temp_1952-2020_draft.csv"))
write_csv(c1out, paste0(datpath, "data/c1_temp_1952-2020_draft.csv"))

