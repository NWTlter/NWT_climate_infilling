# infilling for saddle temp and precip 2015-2017
# ctw sep 2018 (contact caitlin.t.white@colorado.edu with questions)
# prepared for extended summer update for K. Suding/LTER ASM 2018

# hierarchy of infill methods for temp
# 1) 2 week moving window regression using saddle logger data
# 2) monthly regression using saddle logger data
# 3) 2 week moving window regression using D1 chart data
# 4) monthly regression using D1 chart data
# 5) std. deviation ratio method using D1 chart

# some rules:
# adj R^2 > 0.6 (higher better)
# pval =< 0.05



# ------------------
# set working directory to ctw climate infilling subfolder on NWT_climate_infilling github repo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load needed libraries
library(tidyverse)
library(lubridate)

# read in temp datasets
# read in NWT climate dataset used in NSF proposal
# > read in from EDI data portal
sdl_charttemp <- read_csv("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.413.10&entityid=afc48b6fab15649c9f91a9367debd2e0",
                          trim_ws=TRUE, na = c("", "NA", ".", "NaN"))

sdl_loggerdat <- read_csv("http://niwot.colorado.edu/data_csvs/sdlcr23x-cr1000.daily.ml.data.csv",
                          na = c("", "NA", "NaN"))

# keep only temp data from logger (don't need other variables right now)
sdl_loggerdat <- sdl_loggerdat[,1:12] # should keep "LTER_site" through "flag_airtemp_avg 


# read in D1 raw chart
# all chart data, all from NWT data portal
c1_charttemp <- read_csv("http://niwot.colorado.edu/data_csvs/c-1tdayv.ml.data.csv",
                          na = c("", "NA", "NaN")) 

d1_charttemp <- read_csv("http://niwot.colorado.edu/data_csvs/d-1tdayv.ml.data.csv",
                         na = c("", "NA", "NaN"))


# Jennings, Molotch, and Kittel (2018) infilled data for sdl, d1 and c1
# Keith Jennings et al. infilled *hourly* data
Jennings_infill <- read_csv("http://niwot.colorado.edu/data_csvs/infillclimate_c1_d1_sdl.hourly.kj.data.csv")

# summarise Jennings et al. data to daily max, min, or mean (temp)
Jennings_summarized <- Jennings_infill %>%
  group_by(LTER_site, local_site, year, date, jday) %>%
  summarise(J_tmax = max(airtemp_avg),
            J_tmin = min(airtemp_avg),
            J_tmean = mean(airtemp_avg))

# data used in NSF proposal
# Hope Humphries sent the infilled saddle data to Emily Farrer, CTW doesn't know how infilled
# assume infilled data sent to HH from Tim Kittel, and methodology likely similar to that used in Jennings, Molotch, and Kittel (2018)
NSF_temp <- read_csv("./Saddle_precip_temp_formoisturedeficit.csv") %>%
  mutate(`date` = as.Date(paste(Year,Month,Day, sep="-")),
         LTER_site = "NWT",
         local_site = "sdl") %>%
  dplyr::select(LTER_site, local_site, date, TMIN, TMAX)


###########################
## Visual inspection ###
########################

# what's missing in 2015-2017 years?
sdl_charttemp %>%
  mutate(yr = year(date),
         mon = month(date),
         dy = day(date),
         doy = yday(date)) %>%
  dplyr::select(date, yr, mon, dy, doy, airtemp_max, airtemp_min, airtemp_avg) %>%
  gather(metric, value, airtemp_max:airtemp_avg) %>%
  filter(yr > 2014) %>%
  ggplot(aes(doy, value)) +
  geom_line(col="grey50") +
  geom_point(alpha=0.5) +
  labs(y="Daily temperature (°C)") +
  facet_grid(metric~yr, scales = "free_y") +
  theme_bw()
  
sdl_charttemp %>%
  mutate(yr = year(date),
         mon = month(date),
         dy = day(date),
         doy = yday(date)) %>%
  dplyr::select(date, yr, mon, dy, doy, airtemp_max, airtemp_min, airtemp_avg) %>%
  gather(metric, value, airtemp_max:airtemp_avg) %>%
  filter(yr > 2014) %>%
  group_by(yr, mon, metric) %>%
  summarise(missing = sum(is.na(value))) %>%
  ggplot(aes(as.factor(mon), missing)) +
  geom_point() +
  labs(y="Missing days", x = "Month") +
  facet_grid(metric~yr) +
  theme_bw() # 2017 has the most missing days .. overall missing days most often in non-summer months (fewer ppl up there checking equip.)

## > Can I use Keith's D1 or C1 infilled data for infilling??
### >> Answer: No, too different from chart data. Stopped after looking at D1..
# plot Jennings et al. against chart data to see if can use Jennings sdl data for regression
sdl_temp_all <- inner_join(sdl_charttemp, Jennings_summarized) %>%
  left_join(NSF_temp)

# compare saddle chart with Jennings infilled
boxplot(sdl_temp_all$airtemp_max - sdl_temp_all$J_tmax,
        ylab = "chart tmax - Jennings tmax")
boxplot(sdl_temp_all$airtemp_max[sdl_temp_all$flag_airtemp_max==1] - sdl_temp_all$J_tmax[sdl_temp_all$flag_airtemp_max==1],
        ylab = "chart NWT infilled - Jennings infilled")

# compare saddle chart with NSF proposal data (1990 onwards)
boxplot(sdl_temp_all$airtemp_max - sdl_temp_all$TMAX,
        ylab = "chart tmax - NSF tmax") # no difference
boxplot(sdl_temp_all$airtemp_max[sdl_temp_all$flag_airtemp_max==1] - sdl_temp_all$TMAX[sdl_temp_all$flag_airtemp_max==1],
        ylab = "chart NWT infilled - NSF tmax")

# compare D1 chart with Jennings infilled
d1_temp_all <- inner_join(d1_charttemp, Jennings_summarized)

# compare saddle chart with Jennings infilled
boxplot(d1_temp_all$airtemp_max - d1_temp_all$J_tmax,
        ylab = "D1 chart tmax - Jennings tmax")
boxplot(d1_temp_all$airtemp_min - d1_temp_all$J_tmin,
        ylab = "D1 chart tmin - Jennings tmin")
boxplot(D1_temp_all$airtemp_max[sdl_temp_all$flag_airtemp_max==1] - sdl_temp_all$J_tmax[sdl_temp_all$flag_airtemp_max==1],
        ylab = "chart NWT infilled - Jennings infilled")

ggplot(d1_temp_all, aes(jday, airtemp_max)) +
  geom_point(alpha = 0.5) +
  geom_point(aes(jday, J_tmax), col = "dodgerblue", pch=1) +
  facet_wrap(~year)

ggplot(d1_temp_all, aes(jday, airtemp_min)) +
  geom_point(alpha = 0.5) +
  geom_point(aes(jday, J_tmin), col = "dodgerblue", pch=1, alpha=0.5) +
  facet_wrap(~year)

ggplot(d1_temp_all, aes(jday, airtemp_avg)) +
  geom_point(alpha = 0.5) +
  geom_point(aes(jday, J_tmean), col = "dodgerblue", pch=1, alpha=0.5) +
  facet_wrap(~year)

### ---------------------------- > Pressing on with saddle logger and D1 and C1 chart data for infilling..
# NOTE!: logger data is not QAd (according to NWT metadata)

# quick logical check of logger data (day to day variation)
logger_check <- sdl_loggerdat %>%
  mutate(yr = year(date),
         mon = month(date),
         dy = day(date),
         doy = yday(date)) %>%
  dplyr::select(logger, date, yr, mon, dy, doy, airtemp_max, airtemp_min, airtemp_avg) %>%
  mutate(max_lag1 = lag(airtemp_max,1),
         min_lag1 = lag(airtemp_min,1),
         mean_lag1 = lag(airtemp_avg,1),
         diffmax = airtemp_max - max_lag1,
         diffmin = airtemp_min - min_lag1,
         diffmean = airtemp_avg - mean_lag1)

# how much overlap is there between logging equipment? 
group_by(logger_check, logger, yr) %>%
  summarise(nobs = length(airtemp_max)) # answer: none switched in 2012, on dec 5.


# what is sd and distribution of day to day variation in temp in logger data?
## max temp
hist(logger_check$diffmax)
summary(logger_check$diffmax)
sd(logger_check$diffmax, na.rm=T)
## min temp
boxplot(logger_check$diffmin)
summary(logger_check$diffmin)
sd(logger_check$diffmin, na.rm=T)
## mean temp
boxplot(logger_check$diffmean)
summary(logger_check$diffmean)
sd(logger_check$diffmean, na.rm=T)
# bc it's colorado, day to day swings of 20 degrees seem possible .. but check by month (is there bias in winter?)

boxplot(logger_check$diffmax ~ logger_check$mon,
        ylab = "Tmax - Tmax_lag1", xlab ="Month")
boxplot(logger_check$diffmin ~ logger_check$mon,
        ylab = "Tmin - Tmin_lag1", xlab ="Month")
boxplot(logger_check$diffmean ~ logger_check$mon,
        ylab = "Tmean - Tmean_lag1", xlab ="Month")

## most day to day swings in non-summer months .. is this kind of variation in Keith's data?

# day to day variation at saddle using Jennings et al. infilled data
# quick logical check of logger data (day to day variation)
jennings_check <- Jennings_summarized %>%
  ungroup() %>%
  filter(local_site == "sdl",
         `date` %in% logger_check$date) %>% # same time period as logger data
  mutate(yr = year(date),
         mon = month(date),
         dy = day(date),
         doy = yday(date)) %>%
  dplyr::select(date, yr, mon, dy, doy, J_tmax, J_tmin, J_tmean) %>%
  mutate(max_lag1 = lag(J_tmax,1),
         min_lag1 = lag(J_tmin,1),
         mean_lag1 = lag(J_tmean,1),
         diffmax = J_tmax - max_lag1,
         diffmin = J_tmin - min_lag1,
         diffmean = J_tmean - mean_lag1)

# what is sd and distribution of day to day variation in temp in logger data?
# plot with logger variation
par(mfrow=c(1,2))
## max temp
hist(jennings_check$diffmax, main = "Jennings", ylab = "Tmax - Tmax_lag1")
hist(logger_check$diffmax, main = "logger")
summary(jennings_check$diffmax)
sd(jennings_check$diffmax, na.rm=T)
## min temp
boxplot(jennings_check$diffmin, main = "Jennings", ylab = "Tmin - Tmin_lag1")
boxplot(logger_check$diffmin, main = "logger")
summary(jennings_check$diffmin)
sd(jennings_check$diffmin, na.rm=T)
## mean temp
boxplot(jennings_check$diffmean, main = "Jennings", ylab = "Tmean - Tmean_lag1")
boxplot(logger_check$diffmean, main = "logger")
summary(jennings_check$diffmean)
sd(jennings_check$diffmean, na.rm=T)
# bc it's colorado, day to day swings of 20 degrees seem possible .. but check by month (is there bias in winter?)


boxplot(jennings_check$diffmax ~ jennings_check$mon,
        ylab = "Tmax - Tmax_lag1", xlab ="Month", main = "Jennings")
boxplot(logger_check$diffmax ~ logger_check$mon,
        ylab = "Tmax - Tmax_lag1", xlab ="Month", main = "logger")
boxplot(jennings_check$diffmin ~ jennings_check$mon,
        ylab = "Tmin - Tmin_lag1", xlab ="Month", main = "Jennings")
boxplot(logger_check$diffmin ~ logger_check$mon,
        ylab = "Tmin - Tmin_lag1", xlab ="Month", main = "logger")
boxplot(jennings_check$diffmean ~ jennings_check$mon,
        ylab = "Tmean - Tmean_lag1", xlab ="Month", main = "Jennings")
boxplot(logger_check$diffmean ~ logger_check$mon,
        ylab = "Tmean - Tmean_lag1", xlab ="Month", main = "logger")

# close enough .. moving on.. (but someone should QA logger dat more seriously later, mean temp is least consistent with Keith's)

###############################################
### Regression infilling with saddle logger ###
###############################################

# > only in years 2015 and 2016 bc 2017 not available

sdl_chartlogger_1516 <- sdl_charttemp %>%
  filter(`date` > as.Date("2014-12-15")) %>%
  dplyr::rename(chartmax = airtemp_max,
                chartmin = airtemp_min,
                chartmean = airtemp_avg) %>%
  dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) %>% #no flagging done in these years
  inner_join(sdl_loggerdat) %>%
  dplyr::rename(loggermax = airtemp_max,
                loggermin = airtemp_min,
                loggermean = airtemp_avg) %>%
  dplyr::select(-c(flag_airtemp_max, flag_airtemp_min, flag_airtemp_avg)) #no flagging done in these years

# which rows missing?
which(is.na(sdl_chartlogger_1516$chartmax))
which(is.na(sdl_chartlogger_1516$chartmin))
# about 10% of data missing in each tmax and tmin :(

# rule for for loop: if less than 5 days in 2 week span, skip (something with only 5 days probably won't have significant R2 anyway)
tmax_missing_dates <- sdl_chartlogger_1516$date[is.na(sdl_chartlogger_1516$chartmax)]
tmin_missing_dates <- sdl_chartlogger_1516$date[is.na(sdl_chartlogger_1516$chartmin)]


# ***** Method 1: moving window regression *******
## > 2 week before and after date missing


#initialize df for max temp
max_temp_infill <- data.frame(missing_date= NA, complete_nobs = NA, intercept = NA, slope = NA, 
                              tmax_logger = NA, tmax_infill = NA, r2 = NA, pval = NA )

# for loop to execute moving window regressions on max T
for(i in tmax_missing_dates){
  temp_row <- which(tmax_missing_dates == i)
  # subset data to 2 weeks before and 2 weeks after missing date
  begin_date <- i - 14
  end_date <-  i + 14
  temp_df <- subset(sdl_chartlogger_1516,date >= begin_date & date <=end_date)
  # count complete records of chart tmax and logger tmax
  complete_obs <- nrow(temp_df[!is.na(temp_df$chartmax & temp_df$loggermax),])
  # fill in date and count of complete observations
  max_temp_infill[temp_row, "missing_date"] <- i
  max_temp_infill[temp_row, "complete_nobs"] <- complete_obs
  ## logic check: at least 10 complete observations (both sources have tmax data on same day)
  if(complete_obs < 10) {
    next # skip to next date
  }
  
  # if passes logic check, continue with linear regression ..  
  else {
    temp_model <- lm(chartmax ~ loggermax, data=temp_df)
    temp_x <- temp_df$loggermax[temp_df$date == i]
    max_temp_infill[temp_row, "intercept"] <- temp_model$coefficients[[1]]
    max_temp_infill[temp_row, "slope"] <- temp_model$coefficients[[2]]
    max_temp_infill[temp_row, "tmax_logger"] <- temp_x
    max_temp_infill[temp_row, "tmax_infill"] <- temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x)
    max_temp_infill[temp_row, "r2"] <- summary(temp_model)$r.squared 
    max_temp_infill[temp_row, "pval"] <- summary(temp_model)$coefficients[8]
    }
  }

# clean up
max_temp_infill_2wk <-  cbind(tmax_missing_dates, max_temp_infill)
# check paired correctly: should be 0
summary(as.numeric(max_temp_infill_2wk$tmax_missing_dates) - max_temp_infill_2wk$missing_date)
max_temp_infill_2wk <- max_temp_infill_2wk %>%
  dplyr::select(-missing_date) %>% # remove number-form date column
  filter(r2 > 0.6) # keep only infilled values with r2 > 0.6 (per metadata methods)

# asssess:
# how many dates missing?
length(tmax_missing_dates)
# how many infilled?
nrow(max_temp_infill_2wk)


## ****** Repeat above but for minimum temperature
#initialize df for min temp
min_temp_infill <- data.frame(missing_date= NA, complete_nobs = NA, intercept = NA, slope = NA, 
                              tmin_logger = NA, tmin_infill = NA, r2 = NA, pval = NA )

for(i in tmin_missing_dates){
  temp_row <- which(tmin_missing_dates == i)
  # subset data to 2 weeks before and 2 weeks after missing date
  begin_date <- i - 14
  end_date <-  i + 14
  temp_df <- subset(sdl_chartlogger_1516,date >= begin_date & date <=end_date)
  # count complete records of chart tmin and logger tmin
  complete_obs <- nrow(temp_df[!is.na(temp_df$chartmin & temp_df$loggermin),])
  # fill in date and count of complete observations
  min_temp_infill[temp_row, "missing_date"] <- i
  min_temp_infill[temp_row, "complete_nobs"] <- complete_obs
  ## logic check: at least 10 complete observations (both sources have tmin data on same day)
  if(complete_obs < 10) {
    next # skip to next date
  }
  
  # if passes logic check, continue with linear regression ..  
  else {
    temp_model <- lm(chartmin ~ loggermin, data=temp_df)
    temp_x <- temp_df$loggermin[temp_df$date == i]
    min_temp_infill[temp_row, "intercept"] <- temp_model$coefficients[[1]]
    min_temp_infill[temp_row, "slope"] <- temp_model$coefficients[[2]]
    min_temp_infill[temp_row, "tmin_logger"] <- temp_x
    min_temp_infill[temp_row, "tmin_infill"] <- temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x)
    min_temp_infill[temp_row, "r2"] <- summary(temp_model)$r.squared 
    min_temp_infill[temp_row, "pval"] <- summary(temp_model)$coefficients[8]
  }
}

# clean up
min_temp_infill_2wk <-  cbind(tmin_missing_dates, min_temp_infill)
# check paired correctly: should be 0
summary(as.numeric(min_temp_infill_2wk$tmin_missing_dates) - min_temp_infill_2wk$missing_date)
min_temp_infill_2wk <- min_temp_infill_2wk %>%
  dplyr::select(-missing_date) %>% # remove number-form date column
  filter(r2 > 0.6) # keep only infilled values with r2 > 0.6 (per metadata methods)

# asssess:
# how many dates missing?
length(tmin_missing_dates)
# how many infilled?
nrow(min_temp_infill_2wk)


############################################
### Regression infilling using D1 ##
###########################################

# tidy, make long form
sdl_charttemp_long <- sdl_charttemp %>%
  dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) %>% # flagging only indicates infill method used up to 2008
  gather(metric, value, airtemp_max:airtemp_avg)

d1_charttemp_long <- d1_charttemp %>%
  dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) %>% # flagging only indicates infill method used up to 2008
  gather(metric, value, airtemp_max:airtemp_avg)

sdl_d1_chartlong <- rbind(sdl_charttemp_long, d1_charttemp_long) %>%
  filter(year(`date`) > 1981) %>% # sdl chart starts July 9, 1981
  mutate(metric = gsub("airtemp_", "t", metric)) %>% # lazy typing
  spread(local_site, value) %>%
  mutate(yr = year(`date`),
         mon = month(`date`)) # for monthly regressions

# which rows missing? [will show na for tmax, tmin, and tmean]
which(is.na(sdl_d1_chartlong$sdl))

# create vector of dates missing for all temp types (will discard any infilled value don't need later)
temp_missing_1517 <- sdl_d1_chartlong$date[is.na(sdl_d1_chartlong$sdl) & sdl_d1_chartlong$yr > 2014] %>%
  unique()

# ***** Method 1: moving window regression *******
## > 2 week before and after date missing
# rule for for loop: if less than 5 days in 2 week span, skip (something with only 5 days probably won't have significant R2 anyway)

#initialize df for max temp
sdl_temp_infill_d1 <- data.frame(missing_date= NA, complete_nobs = NA, temp_type=NA, intercept = NA, 
                                 slope = NA, d1_value = NA, infill_value = NA, r2 = NA, pval = NA )

# for loop to execute moving window regressions on max T
for(t in c("tmax", "tmin")){
  subset_df <- subset(sdl_d1_chartlong, metric == t)
  missing_dates <- subset_df$date[is.na(subset_df$sdl) & subset_df$yr > 2014]
  for(i in missing_dates){
    # subset data to 2 weeks before and 2 weeks after missing date
    begin_date <- i - 14
    end_date <-  i + 14
    temp_df <- subset(subset_df,date >= begin_date & date <=end_date)
    # count complete records of chart tmax and logger tmax
    complete_obs <- nrow(temp_df[!is.na(temp_df$sdl & temp_df$d1),])
    
    ## logic check: at least 10 complete observations (both sources have tmax data on same day)
    if(complete_obs < 10) {
      null_row <- data.frame(i,complete_obs, t, NA, NA, NA, NA, NA, NA)
      colnames(null_row)[1:9] <- colnames(sdl_temp_infill_d1)
      sdl_temp_infill_d1 <- rbind(sdl_temp_infill_d1, null_row)
    }
    
    # if passes logic check, continue with linear regression ..  
    else {
      temp_model <- lm(sdl ~ d1, data=temp_df)
      temp_x <- temp_df$d1[temp_df$date == i]
      
      infill_row <- data.frame(
        missing_date = i,
        complete_nobs = complete_obs,
        temp_type = t,
        intercept = temp_model$coefficients[[1]],
        slope = temp_model$coefficients[[2]],
        d1_value = temp_x,
        infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
        r2 = summary(temp_model)$r.squared,
        pval = summary(temp_model)$coefficients[8]
      )
     # add to infill data frame
      sdl_temp_infill_d1 <- rbind(sdl_temp_infill_d1,infill_row) 
    }
  }
}

# clean up
# add dates
temp_missing_1517 <- data.frame(`date` = temp_missing_1517)
temp_missing_1517$missing_date <- as.numeric(temp_missing_1517$date)

sdl_temp_infill_d1_max_2wk <- sdl_temp_infill_d1 %>%
  left_join(temp_missing_1517) %>%
  dplyr::select(`date`,complete_nobs:pval) %>% #drop date as number field
  mutate(infill_value = round(infill_value,2)) %>%
  # drop NAs and r2 <=0.6
  filter(!is.na(infill_value) &
         r2 >0.6 &
         temp_type == "tmax")

sdl_temp_infill_d1_min_2wk <- sdl_temp_infill_d1 %>%
  left_join(temp_missing_1517) %>%
  dplyr::select(`date`,complete_nobs:pval) %>% #drop date as number field
  mutate(infill_value = round(infill_value,2)) %>%
  # drop NAs and r2 <=0.6
  filter(!is.na(infill_value) &
           r2 >0.6 &
           temp_type == "tmin")

# compare infill values
max_temp_infill_2wk %>%
  dplyr::rename(`date` = tmax_missing_dates) %>%
  full_join(sdl_temp_infill_d1_2wk[sdl_temp_infill_d1_2wk$temp_type == "tmax",], by = "date") %>%
  ggplot() +
  geom_point(data = sdl_d1_1517, aes(date, sdlmax), col = "grey50") +
  geom_point(aes(date, tmax_infill), col = "orchid", alpha = 0.5) +
  geom_point(aes(date, infill_value), col = "navy", alpha = 0.5) +
  theme_bw() #looks plausible

min_temp_infill_2wk %>%
  dplyr::rename(`date` = tmin_missing_dates) %>%
  full_join(sdl_temp_infill_d1_2wk[sdl_temp_infill_d1_2wk$temp_type == "tmin",], by = "date") %>%
  ggplot() +
  geom_point(data = sdl_d1_1517, aes(date, sdlmin), col = "grey50") +
  geom_point(aes(date, tmin_infill), col = "orchid", alpha = 0.5) +
  geom_point(aes(date, infill_value), col = "navy", alpha = 0.5) +
  theme_bw() # still a lot of missing values..

# merge infill values with chart data, see how many missing left
sdl_temp_1517_final <- sdl_charttemp %>%
  filter(year(`date`) > 2014) %>%
  dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) %>%
  left_join(sdl_temp_infill_d1_max_2wk[c("date", "infill_value", "r2")], by = "date") %>%
  dplyr::rename(d1_tmax_infill = infill_value,
                d1_tmax_r2 = r2) %>%
  left_join(sdl_temp_infill_d1_min_2wk[c("date", "infill_value", "r2")], by = "date") %>%
  dplyr::rename(d1_tmin_infill = infill_value,
                d1_tmin_r2 = r2) %>%
  left_join(max_temp_infill_2wk[c("tmax_missing_dates", "tmax_infill", "r2")], by = c("date" = "tmax_missing_dates")) %>%
  dplyr::rename(log_tmax_infill = tmax_infill,
                log_tmax_r2 = r2) %>%
  left_join(min_temp_infill_2wk[c("tmin_missing_dates", "tmin_infill", "r2")], by = c("date" = "tmin_missing_dates")) %>%
  dplyr::rename(log_tmin_infill = tmin_infill,
                log_tmin_r2 = r2) %>%
  mutate(tmax_infill = ifelse(!is.na(airtemp_max), airtemp_max, log_tmax_infill),
         tmax_source = ifelse(!is.na(airtemp_max), "sdl chart", 
                              ifelse(!is.na(log_tmax_infill), "2wk logger",
                                     ifelse(!is.na(d1_tmax_infill), "2wk d1", NA))),
         tmax_infill = ifelse(!is.na(tmax_infill), tmax_infill, d1_tmax_infill),
         tmin_infill = ifelse(!is.na(airtemp_min), airtemp_min, log_tmin_infill),
         tmin_source = ifelse(!is.na(airtemp_min), "sdl chart", 
                              ifelse(!is.na(log_tmin_infill), "2wk logger",
                                     ifelse(!is.na(d1_tmin_infill), "2wk d1", NA))),
         tmin_infill = ifelse(!is.na(tmin_infill), tmin_infill, d1_tmin_infill),
         tmax_infill = round(tmax_infill, 2),
         tmin_infill = round(tmin_infill, 2)) %>%
  dplyr::select(LTER_site, local_site, date, tmax_infill, tmax_source, tmin_infill, tmin_source)


## ****************
#### clean up global environment before proceed to monthly regression infilling 
rm(d1_temp_all, infill_row, jennings_check, Jennings_infill, logger_check,
   max_temp_infill, max_temp_infill_2wk, min_temp_infill, min_temp_infill_2wk,
   null_row, sdl_chartlogger_1516, sdl_temp_all, sdl_temp_infill_d1, sdl_temp_infill_d1_2wk,
   sdl_temp_infill_d1_max_2wk, sdl_temp_infill_d1_min_2wk, subset_df, temp_df, temp_missing_1517,
   temp_model, test)
rm(tmax_missing_dates, tmin_missing_dates, missing_dates, 
   begin_date, end_date, complete_obs, i, t)


###############################
### ------------------------- > proceed to infill remaining missing through monthly regressions

tmax_missing <- sdl_temp_1517_final$date[is.na(sdl_temp_1517_final$tmax_infill)]
tmin_missing <- sdl_temp_1517_final$date[is.na(sdl_temp_1517_final$tmin_infill)]

# rename colnames in infilled dataset to match NSF_temp names
sdl_temp_1517_final <- sdl_temp_1517_final %>%
  dplyr::rename(TMIN = tmin_infill,
                TMAX = tmax_infill)

## monthly regressions for 2015-2016 using logger data
sdl_logger_all <- NSF_temp %>%
  rbind(sdl_temp_1517_final[c("LTER_site", "local_site", "date", "TMIN", "TMAX")]) %>%
  inner_join(sdl_loggerdat) %>%
  mutate(mon = month(`date`)) %>%
  dplyr::rename(loggermax = airtemp_max,
                loggermin = airtemp_min) %>%
  # remove unneeded columns so easier to look at
  dplyr::select(-c(flag_airtemp_max, flag_airtemp_min, flag_airtemp_avg, jday))
  
# initialize df for tmax
tmax_monthly_regress <- data.frame()

# for loop...
# have to write the first part wonky bc R converts my dates to numbers..
for(missing in as.character(tmax_missing[year(tmax_missing)<2017])){
  i <- as.Date(missing)
  temp_df <- subset(sdl_logger_all,mon == month(i))
  
  # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
  temp_model <- lm(TMAX ~ loggermax, data=temp_df)
  
  temp_x <- temp_df$loggermax[temp_df$date == i]
  
  infill_row <- data.frame(
    missing_date = i,
    complete_nobs = nrow(temp_model$model),
    intercept = temp_model$coefficients[[1]],
    slope = temp_model$coefficients[[2]],
    logger_value = temp_x,
    infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
    r2 = summary(temp_model)$r.squared,
    pval = summary(temp_model)$coefficients[8] 
    )
  tmax_monthly_regress <- rbind(tmax_monthly_regress, infill_row)
}


## **** repeat for tmin

# initialize df for tmin
tmin_monthly_regress <- data.frame()

# for loop...
# have to write the first part wonky bc R converts my dates to numbers..
for(missing in as.character(tmin_missing[year(tmin_missing)<2017])){
  i <- as.Date(missing)
  temp_df <- subset(sdl_logger_all,mon == month(i))
  
  # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
  temp_model <- lm(TMIN ~ loggermin, data=temp_df)
  
  temp_x <- temp_df$loggermin[temp_df$date == i]
  
  infill_row <- data.frame(
    missing_date = i,
    complete_nobs = nrow(temp_model$model),
    intercept = temp_model$coefficients[[1]],
    slope = temp_model$coefficients[[2]],
    logger_value = temp_x,
    infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
    r2 = summary(temp_model)$r.squared,
    pval = summary(temp_model)$coefficients[8] 
  )
  tmin_monthly_regress <- rbind(tmin_monthly_regress, infill_row)
}



###############################
### ****** now infill for 2017 with D1 data ...
## monthly regressions for 2017 using D1 chart data

# merge sdl infilled data and d1 chart data
# Jennings et al. D1 infilled data is cooler in temp than d1 chart so sticking with raw chart so any pre-2015 bias consistent with 2015-2017 
sdl_d1_all <- NSF_temp %>%
  rbind(sdl_temp_1517_final[c("LTER_site", "local_site", "date", "TMIN", "TMAX")]) %>%
  inner_join(d1_charttemp, by = c("LTER_site", "date")) %>%
  mutate(mon = month(`date`)) %>%
  dplyr::rename(d1max = airtemp_max,
                d1min = airtemp_min) %>%
  # remove unneeded columns so easier to look at
  dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) 


# initialize df for tmax
tmax_monthly_regress_d1 <- data.frame()

# for loop...
# have to write the first part wonky bc R converts my dates to numbers..
for(missing in as.character(tmax_missing)){
  i <- as.Date(missing)
  temp_df <- subset(sdl_d1_all,mon == month(i))
  
  # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
  temp_model <- lm(TMAX ~ d1max, data=temp_df)
  
  temp_x <- temp_df$d1max[temp_df$date == i]
  
  infill_row <- data.frame(
    missing_date = i,
    complete_nobs = nrow(temp_model$model),
    intercept = temp_model$coefficients[[1]],
    slope = temp_model$coefficients[[2]],
    d1_value = temp_x,
    infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
    r2 = summary(temp_model)$r.squared,
    pval = summary(temp_model)$coefficients[8] 
  )
  tmax_monthly_regress_d1 <- rbind(tmax_monthly_regress_d1, infill_row)
}


## **** repeat for tmin

# initialize df for tmin
tmin_monthly_regress_d1 <- data.frame()

# for loop...
# have to write the first part wonky bc R converts my dates to numbers..
for(missing in as.character(tmin_missing)){
  i <- as.Date(missing)
  temp_df <- subset(sdl_d1_all,mon == month(i))
  
  # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
  temp_model <- lm(TMIN ~ d1min, data=temp_df)
  
  temp_x <- temp_df$d1min[temp_df$date == i]
  
  infill_row <- data.frame(
    missing_date = i,
    complete_nobs = nrow(temp_model$model),
    intercept = temp_model$coefficients[[1]],
    slope = temp_model$coefficients[[2]],
    d1_value = temp_x,
    infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
    r2 = summary(temp_model)$r.squared,
    pval = summary(temp_model)$coefficients[8] 
  )
  tmin_monthly_regress_d1 <- rbind(tmin_monthly_regress_d1, infill_row)
}


###############################
### ****** now infill for April 2017 with C1 data ...
## D1 chart data not available those dates for tmin or tmax

# merge sdl infilled data and d1 chart data
# Jennings et al. D1 infilled data is cooler in temp than d1 chart so sticking with raw chart so any pre-2015 bias consistent with 2015-2017 
sdl_c1_all <- NSF_temp %>%
  rbind(sdl_temp_1517_final[c("LTER_site", "local_site", "date", "TMIN", "TMAX")]) %>%
  inner_join(c1_charttemp, by = c("LTER_site", "date")) %>%
  mutate(mon = month(`date`)) %>%
  dplyr::rename(c1max = airtemp_max,
                c1min = airtemp_min) %>%
  # remove unneeded columns so easier to look at
  dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) 


# initialize df for tmax
tmax_monthly_regress_c1 <- data.frame()

# for loop...
# have to write the first part wonky bc R converts my dates to numbers..
for(missing in as.character(tmax_missing)){
  i <- as.Date(missing)
  temp_df <- subset(sdl_c1_all,mon == month(i))
  
  # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
  temp_model <- lm(TMAX ~ c1max, data=temp_df)
  
  temp_x <- temp_df$c1max[temp_df$date == i]
  
  infill_row <- data.frame(
    missing_date = i,
    complete_nobs = nrow(temp_model$model),
    intercept = temp_model$coefficients[[1]],
    slope = temp_model$coefficients[[2]],
    c1_value = temp_x,
    infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
    r2 = summary(temp_model)$r.squared,
    pval = summary(temp_model)$coefficients[8] 
  )
  tmax_monthly_regress_c1 <- rbind(tmax_monthly_regress_c1, infill_row)
}


## **** repeat for tmin

# initialize df for tmin
tmin_monthly_regress_c1 <- data.frame()

# for loop...
# have to write the first part wonky bc R converts my dates to numbers..
for(missing in as.character(tmin_missing)){
  i <- as.Date(missing)
  temp_df <- subset(sdl_c1_all,mon == month(i))
  
  # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
  temp_model <- lm(TMIN ~ c1min, data=temp_df)
  
  temp_x <- temp_df$c1min[temp_df$date == i]
  
  infill_row <- data.frame(
    missing_date = i,
    complete_nobs = nrow(temp_model$model),
    intercept = temp_model$coefficients[[1]],
    slope = temp_model$coefficients[[2]],
    c1_value = temp_x,
    infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
    r2 = summary(temp_model)$r.squared,
    pval = summary(temp_model)$coefficients[8] 
  )
  tmin_monthly_regress_c1 <- rbind(tmin_monthly_regress_c1, infill_row)
}


########## compare values (just to see, will always defer to d1 infill if available)
# tmax infill
ggplot() +
  geom_point(data=tmax_monthly_regress_d1, aes(missing_date, infill_value),
             size = tmax_monthly_regress_d1$r2,
             col = "blue", alpha = 0.5) +
  geom_point(data=tmax_monthly_regress_c1, aes(missing_date, infill_value), 
             size = tmax_monthly_regress_c1$r2,
             col = "darkgreen", alpha = 0.5) +
  theme_bw()

ggplot() +
  geom_point(data=tmin_monthly_regress_d1, aes(missing_date, infill_value),
             size = tmin_monthly_regress_d1$r2,
             col = "blue", alpha = 0.5) +
  geom_point(data=tmin_monthly_regress_c1, aes(missing_date, infill_value), 
             size = tmin_monthly_regress_c1$r2,
             col = "darkgreen", alpha = 0.5) +
  theme_bw()


unique(tmax_monthly_regress_d1$r2)
unique(tmax_monthly_regress_c1$r2)

unique(tmin_monthly_regress_d1$r2)
unique(tmin_monthly_regress_c1$r2)

## create final infilled dataset
# NOTES: all d1 monthly infilled data have r2 > 0.6 except for July 2017 tmin infill (r2 = 0.55)
# 

d1_monthly_infill_max <- tmax_monthly_regress_d1 %>%
  dplyr::select(missing_date, infill_value, r2) %>%
  mutate(type = "TMAX",
    source = "d1 monthly lm")

d1_monthly_infill_min <- tmin_monthly_regress_d1 %>%
  dplyr::select(missing_date, infill_value, r2) %>%
  mutate(type = "TMIN",
    source = "d1 monthly lm")

c1_monthly_infill_max <- tmax_monthly_regress_c1 %>%
  filter(missing_date %in% d1_monthly_infill_max$missing_date[is.na(d1_monthly_infill_max$infill_value)]) %>%
  dplyr::select(missing_date, infill_value, r2) %>%
  mutate(type = "TMAX",
    source = "c1 monthly lm")

c1_monthly_infill_min <- tmin_monthly_regress_c1 %>%
  filter(missing_date %in% d1_monthly_infill_min$missing_date[is.na(d1_monthly_infill_min$infill_value)]) %>%
  dplyr::select(missing_date, infill_value, r2) %>%
  mutate(type = "TMIN",
    source = "c1 monthly lm")

monthly_infill_all <- rbind(d1_monthly_infill_max, d1_monthly_infill_min,
                            c1_monthly_infill_max, c1_monthly_infill_min) %>%
  dplyr::arrange(`type`, missing_date)

#write.csv(monthly_infill_all, "/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/sdl_infill_monthlylm_2015-2017.csv")

monthly_tmax <- monthly_infill_all %>%
  filter(type == "TMAX") %>%
  mutate(LTER_site = "NWT",
         local_site = "sdl") %>%  
  dplyr::rename(`date` = missing_date,
                mo_TMAX = infill_value,
                mo_tmax_source = source) %>%
  dplyr::select(LTER_site, local_site, `date`, mo_TMAX, mo_tmax_source) %>%
  na.omit()

monthly_tmin <- monthly_infill_all %>%
  filter(type == "TMIN") %>%
  mutate(LTER_site = "NWT",
         local_site = "sdl") %>%  
  dplyr::rename(`date` = missing_date,
                mo_TMIN = infill_value,
                mo_tmin_source = source) %>%
  dplyr::select(LTER_site, local_site, `date`, mo_TMIN, mo_tmin_source) %>%
  na.omit()

  
########### FINALIZE TEMP DATA (finally!) #######
sdl_infilled_2wk_monthly_1517 <- sdl_temp_1517_final %>%
  left_join(monthly_tmax) %>%
  left_join(monthly_tmin) %>%
  mutate(airtemp_max = ifelse(!is.na(TMAX), TMAX, mo_TMAX),
         tmax_source_cw =ifelse(!is.na(TMAX), tmax_source, mo_tmax_source),
         airtemp_min = ifelse(!is.na(TMIN), TMIN, mo_TMIN),
         tmin_source_cw =ifelse(!is.na(TMIN), tmin_source, mo_tmin_source)) %>%
  dplyr::select(LTER_site, local_site, date, airtemp_max, tmax_source_cw,
                airtemp_min, tmin_source_cw)

sdl_infilled_2wk_monthly_1517$airtemp_mean <- with(sdl_infilled_2wk_monthly_1517, round(((airtemp_max + airtemp_min)/2),2))

#write.csv(sdl_infilled_2wk_monthly_1517, "./sdl_temp_infilled_20152017_ctw.csv")
          

# plot all to see
temp_long <- sdl_infilled_2wk_monthly_1517 %>%
  gather(metric, value, airtemp_max, airtemp_min, airtemp_mean) %>%
  mutate(source = ifelse(metric == "airtemp_max", tmax_source_cw,
                         ifelse(metric == "airtemp_min", tmin_source_cw, 
                                ifelse(tmax_source_cw == tmin_source_cw, tmax_source_cw, "multi"))))

ggplot(temp_long, aes(date, value)) +
  geom_point(aes(fill = source), pch=21, col="grey70", alpha = 0.7) +
  labs(title = "NWT LTER: Saddle daily temperature, 2015-2017, colored by data source",
       subtitle = "Sources: Raw chart data, 2-week moving window regression, monthly regression, or multiple methods",
       y = "Temperature (°C)",
       x = "Date") +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  theme_bw() +
  facet_grid(.~metric)

# ctw notices in some parts of record chart temp has decimal places and in more recent years is rounded (i didn't do this)



######################################################
## Combine ALL years (NSF data + ctw infilled data) ##

sdl_temp_infill_19822017 <- NSF_temp %>%
  dplyr::rename(airtemp_min = TMIN,
                airtemp_max = TMAX) %>%
  mutate(tmax_source_cw = "NSF proposal data",
         tmin_source_cw = "NSF proposal data") %>%
  dplyr::select(LTER_site, local_site, date, airtemp_max, tmax_source_cw,
                airtemp_min, tmin_source_cw)
sdl_temp_infill_19822017$airtemp_mean <- with(sdl_temp_infill_19822017, (round(((airtemp_max + airtemp_min)/2), 2)))

sdl_temp_infill_19822017 <- rbind(sdl_temp_infill_19822017,sdl_infilled_2wk_monthly_1517)

#write.csv(sdl_temp_infill_19822017, "./sdl_temp_infilled_19822017_ctw.csv")


# one last plot of all data..
ggplot(sdl_temp_infill_19822017, aes(date, airtemp_max)) +
  #geom_point(data=sdl_charttemp, aes(date, airtemp_max), size = 1.5) +
  geom_point(aes(col = tmax_source_cw),alpha=0.5) +
  geom_smooth(method = "lm", col = "black") +
  theme_bw()

ggplot(sdl_temp_infill_19822017, aes(date, airtemp_min)) +
  geom_point(aes(col = tmax_source_cw),alpha=0.5) +
  geom_smooth(method = "lm", col = "black")

ggplot(sdl_temp_infill_19822017, aes(date, airtemp_mean, col = tmax_source_cw)) +
  geom_point(alpha=0.5)

