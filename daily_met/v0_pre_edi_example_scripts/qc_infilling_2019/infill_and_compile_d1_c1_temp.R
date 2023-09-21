# infill and compile c1 and d1 temperature through ongoing

# script purpose:
# read in qa'd c1 and d1 chart temp and infill temp following TK methods
## 1) Infill based on mean temp and DTR
## 2) Run seasonal and time series regression, choose best fit (highest r2 if signif p)
## 3) Back-calculate tmin and tmax based on predicted DTR and mean T
# if that doesn't yield infilled values, default to chart metadata infill methods and infill source hierarchies noted
# write out for simple analyses and plotting for 2019 NWT LTER self-study

# > note 1: to follow TK methods, must infill *mean temp* and *diurnal temp*, then calculate min and max from those
# append yrs 2011-ongoing to historic record (1950s-2010) infilled by T. Kittel for 2015 NSF renewal
# > note 2: yrs 2011-2015 in NSF renewal have bad data (e.g. chart values artificially low in some years), so re-do 2011 onwards for better assessment of temporal trends
# >ctw QA'd and adjusted low 2011-2015 temps, but conservative adjustments so some years still likely cooler vals than actually occurred (2013, 2014 in particular)
# > tmin tended to have more of an artificial drop than tmax, but adjusted both vals the same to preserve DTR 
# > note 3: sdl chart values mar 2015-aug 2016 are artificially low and haven't been corrected, don't use to infill

#d1 ppt = 415
#c1 ppt = 414

# notes from metadata:
## c1 + d1 metadata --
# Niwot Data Files used in compilation of summary:
#   Temperature Data:
#   1 Climate Report Data --->
#   2 Chart Recorder Temperature data --->
#   3 DP211 data ---> (ctw: Jen Morse says M. Williams is doubtful about data quality of the DP211 data and better to avoid using)
#   4 Cr21x
# 
# (Method Flag 1) Regression Methodology:
#   A regression was performed using the chart recorder data at the missing site and the adjacent site using a 14 day window prior to and after the missing day, the regression equation was applied to the value at the known site to determine the value for filling the missing site. A r^2 value above 0.6 were considered acceptable. If the r^2 was below 0.6, or missing, a Standard Deviation Method was used to fill the missing value (see Method Flag 2).
# (Method Flag 2) Standard Deviation Methodology:
#   Ratios of known site values and standard deviations were used in order to determine the replacement values. The standard deviation was taken for the day (mm/dd) in question throughout the climate record at both the known site and the unknown site and applied as follows: 

# d1 data adjacency hierachy:
# >> no hierarchies listed in metadata, but assume similar to sdl and c1:
# (1) Cr loggers @ d1
# (2) SDL chart recorder -- *ctw, bad temp data 2015-2016
# (3) SDL Cr loggers
# (4) C1 chart recorder


# c1 data adjacency hierarchy:
# Temperature data adjacency hierarchy (if value missing from charts):
# (1) Dp211 Data Logger [ctw: don't use per jen morse, dubious data quality]
# (2) Cr21x Data Logger
# (3) Saddle (SDL) Chart Recorder -- *ctw, bad temp data 2015-2016
# (4) D1 Chart Recorder
# (5) Allenspark (AP)
# (6) Longmont (LNG)
# (7) Berthoud (BTD)

# if default to metadata infill methods over tk methods, will choose station campsci logger as #1 infill source

# notes from tk infill
# Flag 1: A=no infilling; B=method 1 p<.05; C=method 1 p>.05; D=method 2 p<.05; E=method 2 p>.05
# Flag 2: A=no infilling; B=unadjusted infilling; C=infilled adjusted for known Tmin; D=infilled adjusted for known Tmax
# Flag 3: A=no infilling; B=DTR p<.05; C=DTR 1 p>.05

#*method 1 = seasonal, same-year regression infill (for temp, 14 days before and after target infill window)
#*method 2 = multi-year (historic), same-day (+-3 days) regression infill

# *when a tmin or tmax is known (re: flag 2), TK used the predicted DTR and re-calculated tmean and calculated missing other temp val based on known val (i.e. he didn't use the predicted tmean when a tmax or tmin was known, even if r2 greater for tmean equation relative to dtr r2)  



# -- SETUP ----
rm(list = ls())
library(tidyverse)
library(lubridate)
#library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN", "NP")

# set pathway to climate_d1_c1
datpath <- "climate_d1_c1/output_data/"

# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")


# get data
## d1 chart temp
#raw data (edi) -- incomplete QA and infilling
d1raw <- getTabular(412)
#ctw qa'd
d1qa <- read.csv(paste0(datpath,"prep_data/qa_d1_temp.csv"), na.strings = na_vals) %>%
  mutate(date = as.Date(date)) # date col never reads in as date class with read.csv
## d1 cr loggers 
#d1logs <- getTabular(402) %>% data.frame()
d1logqa <-  read.csv(paste0(datpath, "/prep_data/qa_d1cr_temp.csv"), na.strings = na_vals) %>%
  mutate(date = as.Date(date))
## c1 chart temp
#raw data (edi) -- incomplete QA and infilling
c1raw <- getTabular(411)
#ctw qa'd
c1qa <-  read.csv(paste0(datpath, "/prep_data/qa_c1_temp.csv"), na.strings = na_vals) %>%
  mutate(date = as.Date(date))
## c1 loggers
#c1logs <- getTabular(401, na_vals = na_vals) %>% data.frame()
c1logqa <-  read.csv(paste0(datpath, "/prep_data/qa_c1cr_temp.csv"), na.strings = na_vals) %>%
  mutate(date = as.Date(date))
## sdl chart (discontinued after dec 31 2017)
sdlqa <-  read.csv("extended_summer/analysis/output_data/prep_data/qa_sdlchart_temp.csv", na.strings = na_vals) %>%
  mutate(date = as.Date(date))
## sdl cr loggers
## ctw qa'd sdl logger data (qa = unreasonable/outlier values NA'd but not infilled)
sdllogqa <- read.csv("extended_summer/analysis/output_data/prep_data/qa_sdlcr_temp.csv", na.strings = na_vals) %>%
  mutate(date = as.Date(date))
## snotel data (not QA'd by ctw)
snotel <-  read.csv(paste0(datpath, "/prep_data/nwt_snotel_temp.csv"), na.strings = na_vals) %>%
  mutate(date = as.Date(date))
# tim kittel infilled d1 and c1 temp -- only goes thru dec 31 2010
## from NWT renewal dropbox
tkd1temp <- read.csv("~/Dropbox/NWT_data/d1_infilled_daily_temp.csv", na.strings = na_vals)
tkc1temp <- read.csv("~/Dropbox/NWT_data/c1_infilled_daily_temp.csv", na.strings = na_vals)



# -- CTW + NWT METADATA FUNCTIONS ------
# functions CTW developed for infilling temp data following metadat methods on EDI (used on sdl chart and sdl logger data earlier when working on extended summer)
# did not end up using these functions in the script below to infill d1 and c1, but keeping here just in case needed for future reference
# functions created to follow TK methods are below in the section INFILL c1 TEMP


# function to tidy temp datasets [make long form] (this could be made generic for ppt too..)
tidytemp <- function(dat, datasource = NA, sep = "_", special = "flag", dropcol = NA){
  #if cols to drop, drop
  if(!is.na(dropcol)){
    dat <- dat[!colnames(dat) %in% dropcol] 
  }
  
  # gather temp and any special cols
  # id start of temp cols
  temp_pos <- min(grep("temp", colnames(dat)))
  dat_long <- dat %>%
    gather(met, temp, temp_pos:ncol(.)) %>%
    arrange(met, date)
  
  # if special cols exist, pull out special cols and rejoin wide-form
  if(!is.na(special)){
    tempspecial <- dat_long %>%
      filter(grepl(special, met)) %>%
      mutate(met = gsub(paste0(special,"_"), "", met))
    # rename temp col as special val
    colnames(tempspecial)[which(colnames(tempspecial) == "temp")] <- special
    
    # drop special vals from long-form dat and join wide to temp vals
    dat_long <- subset(dat_long, !grepl(special, met)) %>%
      # add month and year
      mutate(yr = year(date),
             mon = month(date),
             doy = yday(date)) %>%
      left_join(tempspecial) %>%
      dplyr::select(LTER_site:date, yr:doy, met:ncol(.)) 
  }
  
  # if desired, prefix temp and special col colname with datasource
  if(!is.na(datasource)){
    colnames(dat_long)[colnames(dat_long) %in% c("temp", special)] <- paste(datasource, colnames(dat_long)[colnames(dat_long) %in% c("temp", special)], sep = sep)
  }
  
  # return tidy dataset and clean up environment
  return(dat_long)
  rm(tempspecial, temp_pos)
}



# 14-day moving window regression
nwt_movingfill <- function(dataset, metric, ytemp, xtemp, nmin = 10, days = 14, r2 = 0.6, p = 0.05){
  
  require(dplyr)
  
  #initialize df for storing infilled temp values
  infill_df <- data.frame()
  
  for(m in metric){
    dat <- as.data.frame(subset(dataset, met == m))
    # id missing dates
    missing_dates <- sort(dat["date"][is.na(dat[ytemp])]) %>% as.Date()
    
    # for loop to execute moving window regressions on max T
    for(i in 1:length(missing_dates)){
      current_date <- missing_dates[i]
      # subset data to 2 weeks before and 2 weeks after missing date
      begin_date <- current_date - days
      end_date <-  current_date + days
      temp_df <- subset(dat, date >= begin_date & date <=end_date)
      # count complete records of chart tmax and logger tmax
      complete_obs <- sum(complete.cases(temp_df[c(xtemp, ytemp)]))
      # fill in date and count of complete observations
      infill_tempdf <- data.frame(missing_date = current_date, 
                                  met = m,
                                  logger = temp_df$logger[temp_df$date == current_date],
                                  complete_nobs = complete_obs,
                                  mon = month(current_date), yr = year(current_date))
      
      ## logic check: at least 10 complete observations (both sources have tmax data on same day)
      if(complete_obs < nmin) {
        #NAs for all other cols
        infill_tempdf <- cbind(infill_tempdf, 
                               xtemp = temp_df[[xtemp]][temp_df$date == current_date],
                               fit = NA, upr = NA, lwr = NA, se = NA,
                               adjr2 = NA, pval = NA, RSE = NA, method = NA)
        #next # skip to next date
      } else {
        # if passes logic check, continue with linear regression .. 
        temp_model <- lm(as.formula(paste(ytemp, xtemp, sep = "~")), data = temp_df)
        temp_predict <- predict.lm(temp_model, newdata = temp_df[temp_df$date == current_date,], se.fit = T, interval = "prediction")
        infill_tempdf <- cbind(infill_tempdf,
                               xtemp = temp_df[[xtemp]][temp_df$date == current_date],
                               temp_predict$fit,
                               se = temp_predict$se.fit,
                               adjr2 = summary(temp_model)$adj.r.squared,
                               pval = summary(temp_model)$coefficients[8],
                               RSE = summary(temp_model)$sigma,
                               method = paste(xtemp, days, "day lm"))
      }
      infill_df <- rbind(infill_df, infill_tempdf)
    }
  }
  # clean up
  infill_df <- infill_df %>%
    filter(adjr2 > r2 & pval <=p & !is.na(fit)) # keep only infilled values with r2 > 0.6 and pval <= 0.05 (per metadata methods)
  # correct colnames
  names(infill_df)[grepl("xtemp", colnames(infill_df))] <- xtemp
  return(infill_df)
  
}


# function to infill by monthly regression
nwt_monthfill <- function(dat, metric, ytemp, xtemp, mod){
  require(dplyr)
  
  # subset data to no NAs in y or x
  tempdat <- subset(dat, !is.na(dat[ytemp]) & !is.na(dat[xtemp]) & met %in% metric)
  # specify value to predict
  preddat <- subset(dat, met %in% metric & is.na(dat[ytemp]))
  
  # run model
  templm <- lm(as.formula(mod), data = tempdat)
  # null model to calculate p-vals for predicted vals
  nullmod <- lm(as.formula(paste(ytemp, "~ 1")), data = tempdat)
  # predict missing values
  predvals <- predict.lm(templm, newdata = preddat, 
                         se.fit = T, type = "response", interval = "prediction")
  
  # compile predictions with regression stats
  monthly_regress <- preddat %>%
    mutate(complete_nobs = nrow(templm$model)) %>%
    dplyr::select(date, met, logger, complete_nobs, mon, yr, eval(xtemp)) %>%
    rename(missing_date = date) %>%
    cbind(data.frame(predvals$fit,
                     se = predvals$se.fit,
                     adjr2 = summary(templm)$adj.r.squared,
                     pval = anova(nullmod, templm)$'Pr(>F)'[2],
                     RSE = sigma(templm),
                     method = paste(xtemp, "month lm")))
  return(monthly_regress)
  
}




# -- REVIEW DATASETS -----
# to be sure read in as exècted
glimpse(d1qa); summary(d1qa)
glimpse(c1qa); summary(c1qa)
glimpse(d1logqa)
glimpse(c1logqa)
glimpse(sdlqa)
glimpse(sdllogqa)


# tk temps 1950s to 2010 need to be infilled for qa'd c1 and d1 chart temp for same period


# -- REPLICATE TK INILL METHOD TO BE SURE HAVE DOWN -----
# notes:
# tk always used the same time series to infill DTR as to infill mean temp (e.g. their differences == 0)
summary(tkc1temp$X..obs.in.reg..for.TDTR - tkc1temp$X..obs.in.reg..for.Tmean)
# looking at equations and their coefficients, i don't think he applied any math trans before running regressions, just straight OLS lin regress
# looks like most often he used larger windows (historic days?), e.g.:
summary(tkc1temp$X..obs.in.reg..for.TDTR)
summary(as.factor(tkc1temp$Flag.1)) # meanT: A = no infill, B = method 1 (seasonal), D = method 2 (long-term)
summary(as.factor(tkc1temp$Flag.2)) # whether tmin or tmax known pre-adjusting
summary(as.factor(tkc1temp$Flag.3)) # DTR p val only, B < 0.05, C > 0.05
# don't use infilled values as independent (x's) or dependent (y) data in model

# try to rep 12.25.2010 at C1 with c1 cr23x logger dat
## 17 obs
## tmean eq: y=0.99201560039702x+-0.867104845204439, pval = 6.43e-18, r2 = 0.9936904
## dtr eq:y=0.836499256090382x+0.510569974546737, pval = 4.38e-12, r2 = 0.9622462
## > both the same equations used to infill 12/25 and 12/24
# used method 1 (seasonal infill)

# widen c1 logger and calculate mean T and DTR
c1logsub <- subset(c1logqa, yr == 2010 & date >= "2010-12-01") %>%
  dplyr::select(date, mon, yr, doy, met, qa_temp) %>%
  spread(met, qa_temp) %>%
  mutate(c1cr_DTR = airtemp_max - airtemp_min,
         c1cr_meanT = (airtemp_max + airtemp_min)/2)
tkc1sub <- subset(tkc1temp, Year == 2010 & Month > 11) %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  #append c1 logger dat
  left_join(c1logsub, by = c("date" = "date", "Month" = "mon"))

summary(tkc1sub$Flag.1 == "A") # total usable n = 25, but only 17 obs.. think he used 14-day moving window (14d prior to missing stretch & 14d after, but only 3 days after so 14+3 = 17)
# would need to id missing stretch in month..
missing_dates <- tkc1sub$date[tkc1sub$Flag.1 != "A"]
start_date <- min(missing_dates) - 14
end_date <- max(missing_dates) + 14

test <- subset(tkc1sub, Flag.1 == "A" & date >= start_date & date <= end_date)
# DTR regression
summary(lm(DTR ~ c1cr_DTR, data = test)) # perfect match
# Tmean regression
summary(lm(`Mean.temperature..deg.C.` ~ c1cr_meanT, data = test)) # perfect match except for pval (but both p's very very small)

# try to rep multi-yr regression
# D1 infilled C1, Jan 6 1980
# 348 obs, method D for tmean/DTR
# tmean equation: y=0.901877950431429x+4.90253515365412, pval: 5.82e-134, r2: 0.8272077
# dtr equation: y=0.568666708556348x+5.61677367576244, pval: 1.01e-16, r2: 0.1809128

# pair tk c1/d1, remove any infilled values (i.e. Flag == A on both)
meth2_df <- tkc1temp[,1:8]
colnames(meth2_df)[4:8] <- c("c1_tmax", "c1_tmin", "c1_tmean", "c1_dtr", "c1_flag1")
meth2_df <- left_join(meth2_df, tkd1temp[1:8], by = c("Month" = "month", "Day" = "day", "Year" = "year"))
lm2_df <- meth2_df 
# NA any value that's been infilled so not used in regression
lm2_df[lm2_df$c1_flag1 != "A", 4:7] <- NA
lm2_df[lm2_df$flag.1 != "A", 9:12] <- NA
# add on day of year col to subset data frame to days around Jan 6
lm2_df <- mutate(lm2_df, date = as.Date(paste(Year, Month, Day, sep = "-")),
                 doy = yday(date))
# specify target day of year
target <- yday(as.Date("1980-01-06"))
# subset data frame to verify nobs used
lm2_dfsub <- subset(lm2_df, !(is.na(c1_dtr) & is.na(DTR))) %>%
  filter(doy %in% c((target-3):(target+3)))
summary(is.na(lm2_dfsub$c1_dtr)) #394 nobs.. 11 c1 temps are NA in that, which means 383 obs.. looking for 348
summary(is.na(lm2_dfsub$DTR)) #35 NAs in D1 temp..which gets us to 348 obs!

# dtr test
summary(lm(c1_dtr ~ DTR, data = lm2_dfsub)) # equation matches, pval and r2 roughly equal (pval smaller and r2 0.002 less but rounds to 0.18)
# tmean test
summary(lm(c1_tmean ~ Tmean, data = lm2_dfsub)) # it's a match!


# confirmed rules:
# multi year = +/- 3 days from target, all yrs -- if logger used, only use values in lifetime of logger
# moving window: 14 days after and preceeding missing window (missing window could be multiple consec days)


# -- PREP ALL STATION TEMP DATA WITH DTR AND MEAN T COLS -----
# make sure qa_temp is NA where it should be (e.g. don't use flatline advisory vals)
# also make sure logger dats don't have duplicate dates when cr21x swapped for cr23x 

# loggers -----
# c1 logger
# check qa flags
unique(c1logqa$qa_flag)
unique(c1logqa$c1cr_flag)
# check for duplicate dates when loggers switched
c1dup_check <- dplyr::select(c1logqa, date,logger) %>%
  distinct()
summary(duplicated(c1dup_check$date)) #5 duplicated
c1dup_dates <- c1dup_check$date[duplicated(c1dup_check$date)]
subset(c1logqa, date %in% c1dup_dates)
# both have missing data, go with cr23x -- newer

c1log_prep <- c1logqa %>%
  # remove dup dates
  filter(!(date %in% c1dup_dates & logger == "cr21x")) %>%
  # ensure qa_temp NA is 
  mutate(c1cr_qatemp = ifelse(!is.na(qa_flag), NA, qa_temp)) %>%
  dplyr::select(logger:met, c1cr_qatemp) %>%
  spread(met, c1cr_qatemp) %>%
  mutate(c1cr_DTR = airtemp_max - airtemp_min,
         c1cr_tmean = (airtemp_max + airtemp_min)/2) %>%
  # remove cols without any data in the early part of the dataset
  ## e.g. starts 1986 but no data until 1988
  filter(date >= min(date[!is.na(c1cr_DTR)])) %>%
  #take out tmax and tmin
  dplyr::select(-c(airtemp_max, airtemp_min)) %>%
  # rename logger col for master merge
  rename(c1_logger = logger)

# check no dup dates
summary(duplicated(c1log_prep$date)) # nope!



# sdl logger
# check qa flags
unique(sdllogqa$qa_flag) # leave shifted temps
# are nas already taken care of in qa_temp
unique(sdllogqa$qa_temp[!(is.na(sdllogqa$qa_flag)) & !grepl("shifted", sdllogqa$qa_flag)]) #yes

# check for duplicate dates when loggers switched
sdldup_check <- dplyr::select(sdllogqa, date,logger) %>%
  distinct()
summary(duplicated(sdldup_check$date)) #4 duplicated
sdldup_dates <- sdldup_check$date[duplicated(sdldup_check$date)]
subset(sdllogqa, date %in% sdldup_dates)
# go with cr21x bc no missing data

sdllog_prep <- sdllogqa %>%
  filter(!(date %in% sdldup_dates & logger == "cr23x")) %>%
  dplyr::select(logger:met, qa_temp) %>%
  spread(met, qa_temp) %>%
  mutate(sdlcr_DTR = airtemp_max - airtemp_min,
         sdlcr_tmean = (airtemp_max + airtemp_min)/2) %>%
  #take out tmax and tmin
  dplyr::select(-c(airtemp_max, airtemp_min)) %>%
  # rename logger col for master merge
  rename(sdl_logger = logger)


# d1 logger
# check qa flags
unique(d1logqa$qa_flag) # leave shifted temps
unique(d1logqa$d1cr_flag) # n means nothing
# are nas already taken care of in qa_temp
unique(sdllogqa$qa_temp[!(is.na(sdllogqa$qa_flag)) & !grepl("shifted", sdllogqa$qa_flag)]) #yes
# check for duplicate dates when loggers switched
d1dup_check <- dplyr::select(d1logqa, date,logger) %>%
  distinct()
summary(duplicated(d1dup_check$date)) #nothing duplicated


d1log_prep <- d1logqa %>%
  dplyr::select(logger:met, qa_temp) %>%
  distinct() %>%
  spread(met, qa_temp) %>%
  mutate(d1cr_DTR = airtemp_max - airtemp_min,
         d1cr_tmean = (airtemp_max + airtemp_min)/2) %>%
  #take out tmax and tmin
  dplyr::select(-c(airtemp_max, airtemp_min)) %>%
  # rename logger col for master merge
  rename(d1_logger = logger)


# snotel -----
snotel_prep <- spread(snotel, met, tempC) %>%
  mutate(sno_DTR = airtemp_max - airtemp_min,
         sno_tmean = (airtemp_max + airtemp_min)/2) %>%
  # get rid of first three rows bc have 0s or NA
  filter(date > "1989-10-03") %>%
  #take out tmax and tmin
  dplyr::select(-c(airtemp_max, airtemp_min, LTER_site, local_site))


# chart data -----
# c1 chart
unique(c1qa$qa_flag) # keep advisories.. didn't NA bc did not deviate enough and more data points that way
unique(c1qa$c1_flag) #A == C1 as source

c1_prep <- c1qa %>%
  mutate(c1_qatemp = qa_temp,
         # add in tk's dataset
         c1_qatemp = ifelse(yr < 2011 & c1_flag == "A", c1_temp, qa_temp)) %>%
  dplyr::select(date:met, c1_qatemp) %>%
  spread(met, c1_qatemp) %>%
  mutate(c1_DTR = airtemp_max - airtemp_min,
         c1_tmean = (airtemp_max + airtemp_min)/2) %>%
  # beginning of period empty bc infilled, subset out
  filter(date >= min(date[!is.na(c1_DTR)])) %>%
  #take out tmax and tmin
  dplyr::select(-c(airtemp_max, airtemp_min))

# d1 chart
unique(d1qa$qa_flag) # keep advisories.. didn't NA bc did not deviate enough and more data points that way
# do advisories have data?
summary(d1qa$d1_temp[grepl("advisor", d1qa$qa_flag)]) # yes
unique(d1qa$c1_flag)
d1_prep <- d1qa %>%
  mutate(d1_qatemp = qa_temp,
         # add in tk's dataset
         d1_qatemp = ifelse(yr < 2011 & d1_flag == "A", d1_temp, qa_temp)) %>%
  dplyr::select(date:met, d1_qatemp) %>%
  spread(met, d1_qatemp) %>%
  mutate(d1_DTR = airtemp_max - airtemp_min,
         d1_tmean = (airtemp_max + airtemp_min)/2) %>%
  # beginning of period empty bc infilled, subset out
  filter(date >= min(date[!is.na(d1_DTR)])) %>%
  #take out tmax and tmin
  dplyr::select(-c(airtemp_max, airtemp_min))

# sdl chart -- note 2014 to 2016 has artificially low temps
# note from sdl logger qa script: sdl chart values mar 2015-aug 2016 no bueno, noted elsewhere apr 2014-aug 23 2016
# see if see bad data
subset(sdlqa, yr > 2010) %>%
  mutate(suspect = ifelse(date >= as.Date("2015-03-01") & date < as.Date("2016-09-01"), 1, 0)) %>%
  ggplot(aes(date, sdl_qatemp, col = suspect)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  facet_grid(.~yr, scales = "free_x") 
# 2014 looks low too tho.. don't use any data 2014-2016 to infill 

# check out flagging
unique(sdlqa$sdl_flag) # 1 or 2 means infilled, don't use
unique(sdlqa$qa_flag)
#be sure temp vals NAs for flatline
summary(sdlqa$sdl_qatemp[grepl("warning", sdlqa$qa_flag)])
# bc i don't think i was discerning bad flatlines from advisory flatlines when i QAd sdl (was the guinea pig), NA all for infill purposes
sdl_prep <- sdlqa %>%
  mutate(qa_temp = ifelse(!is.na(qa_flag), NA, sdl_qatemp),
         qa_temp = ifelse(!is.na(sdl_flag), NA, qa_temp)) %>%
  dplyr::select(date:met, qa_temp) %>%
  distinct() %>%
  # NA anything 2014-2016
  mutate(qa_temp = ifelse(yr %in% 2014:2016, NA, qa_temp)) %>%
  spread(met, qa_temp) %>%
  mutate(sdl_DTR = airtemp_max - airtemp_min,
         sdl_tmean = (airtemp_max + airtemp_min)/2) %>%
  # beginning of period empty bc infilled, subset out
  filter(date >= min(date[!is.na(sdl_DTR)])) %>%
  #take out tmax and tmin
  dplyr::select(-c(airtemp_max, airtemp_min))


# join all temp dat into master
## d1 and c1 both start at the same time so can start with either
master_temp <- left_join(c1_prep, d1_prep) %>%
  left_join(sdl_prep) %>%
  left_join(c1log_prep) %>%
  left_join(d1log_prep) %>%
  left_join(sdllog_prep) %>%
  left_join(snotel_prep)

# add cols to keep track of consec days missing

# all prepped, clean up
rm(c1dup_dates, c1dup_check, d1dup_check, sdldup_check, sdldup_dates)




# -- INFILL C1 TEMP -----
# can either keep track of consec days (e.g. NA events), or in for loop have look ahead and look behind for first day not NA and count 14 days from that (for inside a for?)

# store dates for DTR and tmeans that are NA (should match) *after* 2010 (TK infilled thru 2010)
c1missing_dates <- master_temp$date[is.na(master_temp$c1_tmean) & master_temp$yr > 2010]
c1missing_dates_dtr <- master_temp$date[is.na(master_temp$c1_DTR) & master_temp$yr > 2010]
summary(c1missing_dates_tmean == c1missing_dates_dtr) # all dates agree, as expected
rm(c1missing_dates_dtr) # clean up

# specify hierarchy of infill stations for c1 (by colname? -- just first part, in loop concat rest of colname since all named similarly)
c1_infill_order <- c("c1cr", "sno", "sdl", "sdlcr", "d1", "d1cr") # c1 logger, snotel, sdl chart, sdl logger, d1 chart, d1 logger

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






# infill c1 chart by 2wk moving window
c1_shortfill <- tk_movingfill(master_temp, "c1", c1missing_dates, c1_infill_order)
# infill c1 chart by historic values
c1_longfill <- tk_historicfill(master_temp, "c1", c1missing_dates, c1_infill_order)





# -- INFILL D1 TEMP -----
# specify d1 missing dates and hierarchy of source station
d1missing_dates <- master_temp$date[is.na(master_temp$d1_tmean) & master_temp$yr > 2010]
d1_infill_order <- c("d1cr", "sdl", "sdlcr", "c1", "c1cr")

# infill d1 chart by 2wk moving window
d1_shortfill <- tk_movingfill(master_temp, "d1", d1missing_dates, d1_infill_order)
# infill d1 chart by historic values
d1_longfill <- tk_historicfill(master_temp, "d1", d1missing_dates, d1_infill_order)



# -- REPLICATE TK MODEL SELECTION -----
# use same examples as above
tk_dat <- subset(master_temp, yr<2011)

# rep 14-day regression example
# dec 23-28 2010, infilled with c1 cr23x logger
## 17 obs
## tmean eq: y=0.99201560039702x+-0.867104845204439, pval = 6.43e-18, r2 = 0.9936904
## dtr eq:y=0.836499256090382x+0.510569974546737, pval = 4.38e-12, r2 = 0.9622462
## > both the same equations used to infill 12/25 and 12/24
# used method 1 (seasonal infill)
ex1_dates <- seq.Date(as.Date("2010-12-23"), as.Date("2010-12-28"), 1)
ex1_short <- tk_movingfill(tk_dat, "c1", missing_dates = ex1_dates, site_order = c("c1cr", "d1"))
ex1_long <- tk_historicfill(tk_dat, "c1", missing_dates = ex1_dates, site_order = c("c1cr", "d1"))

# in this case, both pvals under 0.05 in each method, but r2 much better for tmean and dtr using short fill method (easy comparison)
# can we compare mean pval and r2?
sapply(ex1_short[c("pval", "r2")], mean) 
sapply(ex1_long[c("pval", "r2")], mean)
# > would select method 1 based on mean approach

# try to rep multi-yr regression
# D1 infilled C1, Jan 6 1980
ex2_short <- tk_movingfill(dat = tk_dat, target_site = "c1", missing_dates = as.Date("1980-01-06"), site_order = "d1") 
ex2_long <- tk_historicfill(dat = tk_dat, target_site = "c1", missing_dates = as.Date("1980-01-06"), site_order = "d1") 

# still try mean approach, but in this case tmean estimate not from signif model, whereas both p's signif in long-term although DTR r2 much less than r2 for short-term method)
sapply(ex2_short[c("pval", "r2")], mean)
sapply(ex2_long[c("pval", "r2")], mean) 
# would select method 2 based on mean approach


# other stipulations that make sense to me..
# > if nobs in model less than number days to be infilled, discard that model
# > maximize signif pval on both first?
# > then if both signif, select whichever method yields overall highest mean r2?
# > could have the case where pval signif (bc many observations, but r2 is very low for each..)


# -- INFILL SELECTION -----
# group days by the infill event in shortfill, average pval and r2 and if individuals pvals <= 0.05 and mean pval <= 0.05, select method with highest average r2

# select c1 -----
# some of this below could be generalized into a function (some already done, some left manual)..

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

c1_compare <- compare_results(c1_longfill, c1_shortfill)
c1_means <- select_model(c1_compare)
# does every event have a model selected?
length(unique(c1_means$infillevent)) == length(c1_means$selectmod[c1_means$selectmod])

# join selection results to c1_compare
c1_compare <- left_join(c1_compare, c1_means)
c1_choose <- subset(c1_compare, selectmod == TRUE) %>%
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
  # append cols for tmin, tmax
  left_join(c1qa[c1qa$met == "airtemp_max", c("date", "c1_temp", "qa_temp")]) %>%
  rename(tmax_raw = c1_temp,
         tmax = qa_temp) %>%
  left_join(c1qa[c1qa$met == "airtemp_min", c("date", "c1_temp", "qa_temp")]) %>%
  rename(tmin_raw = c1_temp,
         tmin = qa_temp) %>%
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
                             ifelse(!is.na(adj.tmin), adj.tmin, proj.tmin))) %>%
  #compare with c1 logger
  left_join(c1logqa[c1logqa$met == "airtemp_max", c("date", "qa_temp")]) %>%
  rename(c1cr_tmax = qa_temp) %>%
  left_join(c1logqa[c1logqa$met == "airtemp_min", c("date", "qa_temp")]) %>%
  rename(c1cr_tmin = qa_temp) %>%
  # add in QA checks -- predicted/adjusted deviation from c1 logger
  # generally, tmax is cooler on chart vs logger and tmin is warmer on chart vs logger
  mutate(adjtmax_diff_c1crtmax = final_tmax - c1cr_tmax,
         adjtmin_diff_c1crtmin = final_tmin - c1cr_tmin,
         raw_tmax_adj = ifelse(tmax_raw != tmax, 1, 0),
         raw_tmin_adj = ifelse(tmin_raw != tmin, 1, 0))

ggplot(subset(c1_lmtemps, !is.na(adj.tmax)), aes(proj.tmax, c1cr_tmax)) +
  geom_point() +
  geom_point(aes(adj.tmax, c1cr_tmax), col ="orchid", alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0))

ggplot(subset(c1_lmtemps, !is.na(adj.tmax)), aes(date, c1cr_tmax)) +
  geom_point() +
  geom_point(aes(date, proj.tmax), col ="orchid", alpha = 0.5) +
  geom_point(aes(date, adj.tmax), col ="lightblue", alpha = 0.5) +
  ggtitle("Compare C1 chart predicted (orchid) vs adjusted (blue) vs C1 logger (black)") +
  labs(y = "TMAX")

ggplot(subset(c1_lmtemps, !is.na(adj.tmin)), aes(date, c1cr_tmin)) +
  geom_point() +
  geom_point(aes(date, proj.tmin), col ="orchid", alpha = 0.5) +
  geom_point(aes(date, adj.tmin), col ="lightblue", alpha = 0.5) +
  ggtitle("Compare C1 chart predicted (orchid) vs adjusted (blue) vs C1 logger (black)") +
  labs(y = "TMIN")

triage_c1temps <- subset(c1_lmtemps, !is.na(adj.tmax) | ! is.na(adj.tmin)) %>%
  # diff predicted from c1cr
  mutate(pred_diff_c1crtmax = proj.tmax - c1cr_tmax,
         pred_diff_c1crtmin = proj.tmin - c1cr_tmin,
         # flag anything where adjusted deviates more than predicted
         flag_adjtmax = ifelse(abs(adjtmax_diff_c1crtmax) > abs(pred_diff_c1crtmax), 1, 0),
         flag_adjtmin = ifelse(abs(adjtmin_diff_c1crtmin) > abs(pred_diff_c1crtmin), 1, 0))

# rules for keeping adjusted temp vs using predicted when chart tmin or tmax exists:
## if adjusted tmin and adjusted tmax deviate more from c1logger than predicted (flag_adjtmax & flag_adjtmin == 1), use predicted 
## if either flag_adjtmax or flag_adjtmin == 1 (but not both) and raw chart value was adjusted by ctw (adjusted for artificial drop in temperature during record), use predicted
## if either flag_adjtmax or flag_adjtmin == 1 (but not both) and raw chart value unadjusted, use adjusted temp over predicted (i.e. keep chart value that exists and calculate missing temp from that and predicted DTR)
# triage temps with flags and add them in to predicted temp dataset .. or recalculate final temps with these rules

triage_c1temps2 <- triage_c1temps %>%
  # remove diff cols so easier to look at dataset and compare, have flags for ref
  dplyr::select(-grep("diff", colnames(triage_c1temps))) %>%
  ungroup() %>%
  # if both adjuted tmax and tmin calculated from 1 known chart temp and DTR deviate more from c1 logger than predicted vals, use predicted vals 
  mutate(final_tmax2 = ifelse(flag_adjtmax == 1 & flag_adjtmin == 1, proj.tmax, 
                              # if only one adjusted temp is flagged BUT known chart temp used was adjusted (for artificial drop in chart), use predicted, otherwise use adjusted vals
                              ifelse(flag_adjtmax == 1 & flag_adjtmin == 0 & raw_tmin_adj == 1, proj.tmax, NA)),
         # if another source used so no NA flag, but raw value was adjusted, used the predicted value
         final_tmax2 = ifelse(is.na(flag_adjtmax) & raw_tmin_adj == 1, proj.tmax, final_tmax2),
         # repeat for tmin
         final_tmin2 = ifelse(flag_adjtmax == 1 & flag_adjtmin == 1, proj.tmin, 
                              ifelse(flag_adjtmin ==1 & raw_tmax_adj == 1, proj.tmin, NA)), 
         # if another source used so no NA flag, but raw value was adjusted, used the predicted value
         final_tmin2 = ifelse(is.na(flag_adjtmin) & raw_tmax_adj == 1, proj.tmin, final_tmin2)) %>%
  # finally, if either tmax or tmin selected and matched projected value, infill its pair with projected
  ## the only value that would exist *IS* the projected value
  mutate(final_tmax2 = ifelse(is.na(final_tmax2) & !is.na(final_tmin2), proj.tmax, final_tmax2),
         final_tmin2 = ifelse(is.na(final_tmin2) & !is.na(final_tmax2), proj.tmin, final_tmin2)) %>%
  #add flag to indicate prediction prioritized over adjusted infill
  mutate(infill_QA_note = ifelse(!is.na(final_tmax2), "Predicted infill chosen over known temperature-adjusted infill", NA))

# to be sure pair done correctly, verify all final2 values match projected
## should either be true or NA
summary(triage_c1temps2$final_tmax2 == triage_c1temps2$proj.tmax)
summary(triage_c1temps2$final_tmin2 == triage_c1temps2$proj.tmin) # good
# now the rest can be infilled with the calculated (known chart temp adjusted) values
triage_c1temps2$final_tmax2[is.na(triage_c1temps2$final_tmax2)] <- triage_c1temps2$final_tmax[is.na(triage_c1temps2$final_tmax2)]
triage_c1temps2$final_tmin2[is.na(triage_c1temps2$final_tmin2)] <- triage_c1temps2$final_tmin[is.na(triage_c1temps2$final_tmin2)]
# add a flag to indicate whether final temp was adjusted or predicted (to match TK's Flag.2)
# > # Flag 2: A=no infilling; B=unadjusted infilling; C=infilled adjusted for known Tmin; D=infilled adjusted for known Tmax
triage_c1temps2$Flag.2 <- ifelse(triage_c1temps2$final_tmax2 == triage_c1temps2$final_tmax & !is.na(triage_c1temps2$tmin), "C",
                                 ifelse(triage_c1temps2$final_tmax2 == triage_c1temps2$final_tmax & !is.na(triage_c1temps2$tmax), "D", "B"))
# whittle triaged df down to final cols
triage_c1temps2 <- dplyr::select(triage_c1temps2, date, source.station, final_tmax2, final_tmin2, tmean, DTR, Flag.2, infill_QA_note) %>%
  rename(tmax = final_tmax2, tmin = final_tmin2)

# rbind triaged temps back with other predicted temps
final_c1_pred <- subset(c1_lmtemps, !date %in% triage_c1temps2$date) %>%
  #add flag 2 (B = unadjusted infilling)
  mutate(Flag.2 = "B",
         infill_QA_note = NA) %>%
  # keep date, source.station, mean T , DTR, and final tmin and tmax vals
  dplyr::select(date, source.station, final_tmax, final_tmin, tmean, DTR, Flag.2, infill_QA_note) %>%
  rename(tmax = final_tmax,
         tmin = final_tmin) %>%
  # add on triaged adjusted temps
  rbind(triage_c1temps2) %>%
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
  dplyr::select(LTER_site, local_site, date, doy,year:day, tmax:DTR, flag.1, Flag.2, flag.3, source.station,
                tmean_pval, tmean_r2, tmean_n.obs, tmean_equation, DTR_pval, DTR_r2, DTR_n.obs, DTR_equation, infill_QA_note) %>%
  #cbind QA flagging to infilled temps
  left_join(c1qa[c1qa$met == "airtemp_max", c("date", "qa_flag")]) %>%
  rename(Tmax.QAflag = qa_flag) %>%
  left_join(c1qa[c1qa$met == "airtemp_min", c("date", "qa_flag")]) %>%
  rename(Tmin.QAflag = qa_flag)

# clean up environment
rm(triage_c1temps, triage_c1temps2)



# select d1 -----
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
  # append cols for tmin, tmax
  left_join(d1qa[d1qa$met == "airtemp_max", c("date", "d1_temp", "qa_temp")]) %>%
  rename(tmax_raw = d1_temp,
         tmax = qa_temp) %>%
  left_join(d1qa[d1qa$met == "airtemp_min", c("date", "d1_temp", "qa_temp")]) %>%
  rename(tmin_raw = d1_temp,
         tmin = qa_temp) %>%
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
                             ifelse(!is.na(adj.tmin), adj.tmin, proj.tmin))) %>%
  #compare with d1 logger
  left_join(d1logqa[d1logqa$met == "airtemp_max", c("date", "qa_temp")]) %>%
  rename(d1cr_tmax = qa_temp) %>%
  left_join(d1logqa[d1logqa$met == "airtemp_min", c("date", "qa_temp")]) %>%
  rename(d1cr_tmin = qa_temp) %>%
  # add in QA checks -- predicted/adjusted deviation from d1 logger
  # generally, tmax is cooler on chart vs logger and tmin is warmer on chart vs logger
  mutate(adjtmax_diff_d1crtmax = final_tmax - d1cr_tmax,
         adjtmin_diff_d1crtmin = final_tmin - d1cr_tmin,
         raw_tmax_adj = ifelse(tmax_raw != tmax, 1, 0),
         raw_tmin_adj = ifelse(tmin_raw != tmin, 1, 0))

ggplot(subset(d1_lmtemps, !is.na(adj.tmax)), aes(proj.tmax, d1cr_tmax)) +
  geom_point() +
  geom_point(aes(adj.tmax, d1cr_tmax), col ="orchid", alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0))

ggplot(subset(d1_lmtemps, !is.na(adj.tmax)), aes(date, d1cr_tmax)) +
  geom_point() +
  geom_point(aes(date, proj.tmax), col ="orchid", alpha = 0.5) +
  geom_point(aes(date, adj.tmax), col ="lightblue", alpha = 0.5) +
  ggtitle("Compare D1 chart predicted (orchid) vs adjusted (blue) vs D1 logger (black)") +
  labs(y = "TMAX")

ggplot(subset(d1_lmtemps, !is.na(adj.tmin)), aes(date, d1cr_tmin)) +
  geom_point() +
  geom_point(aes(date, proj.tmin), col ="orchid", alpha = 0.5) +
  geom_point(aes(date, adj.tmin), col ="lightblue", alpha = 0.5) +
  ggtitle("Compare D1 chart predicted (orchid) vs adjusted (blue) vs D1 logger (black)") +
  labs(y = "TMIN")

triage_d1temps <- subset(d1_lmtemps, !is.na(adj.tmax) | ! is.na(adj.tmin)) %>%
  # diff predicted from d1cr
  mutate(pred_diff_d1crtmax = proj.tmax - d1cr_tmax,
         pred_diff_d1crtmin = proj.tmin - d1cr_tmin,
         # flag anything where adjusted deviates more than predicted
         flag_adjtmax = ifelse(abs(adjtmax_diff_d1crtmax) > abs(pred_diff_d1crtmax), 1, 0),
         flag_adjtmin = ifelse(abs(adjtmin_diff_d1crtmin) > abs(pred_diff_d1crtmin), 1, 0))

# rules for keeping adjusted temp vs using predicted when chart tmin or tmax exists:
## if adjusted tmin and adjusted tmax deviate more from d1logger than predicted (flag_adjtmax & flag_adjtmin == 1), use predicted 
## if either flag_adjtmax or flag_adjtmin == 1 (but not both) and raw chart value was adjusted by ctw (adjusted for artificial drop in temperature during record), use predicted
## if either flag_adjtmax or flag_adjtmin == 1 (but not both) and raw chart value unadjusted, use adjusted temp over predicted (i.e. keep chart value that exists and calculate missing temp from that and predicted DTR)
# triage temps with flags and add them in to predicted temp dataset .. or recalculate final temps with these rules

triage_d1temps2 <- triage_d1temps %>%
  # remove diff cols so easier to look at dataset and compare, have flags for ref
  dplyr::select(-grep("diff", colnames(triage_d1temps))) %>%
  ungroup() %>%
  # if both adjuted tmax and tmin calculated from 1 known chart temp and DTR deviate more from d1 logger than predicted vals, use predicted vals 
  mutate(final_tmax2 = ifelse(flag_adjtmax == 1 & flag_adjtmin == 1, proj.tmax, 
                              # if only one adjusted temp is flagged BUT known chart temp used was adjusted (for artificial drop in chart), use predicted, otherwise use adjusted vals
                              ifelse(flag_adjtmax == 1 & flag_adjtmin == 0 & raw_tmin_adj == 1, proj.tmax, NA)),
         # if another source used so no NA flag, but raw value was adjusted, used the predicted value
         final_tmax2 = ifelse(is.na(flag_adjtmax) & raw_tmin_adj == 1, proj.tmax, final_tmax2),
         # repeat for tmin
         final_tmin2 = ifelse(flag_adjtmax == 1 & flag_adjtmin == 1, proj.tmin, 
                              ifelse(flag_adjtmin ==1 & raw_tmax_adj == 1, proj.tmin, NA)), 
         # if another source used so no NA flag, but raw value was adjusted, used the predicted value
         final_tmin2 = ifelse(is.na(flag_adjtmin) & raw_tmax_adj == 1, proj.tmin, final_tmin2)) %>%
  # finally, if either tmax or tmin selected and matched projected value, infill its pair with projected
  ## the only value that would exist *IS* the projected value
  mutate(final_tmax2 = ifelse(is.na(final_tmax2) & !is.na(final_tmin2), proj.tmax, final_tmax2),
         final_tmin2 = ifelse(is.na(final_tmin2) & !is.na(final_tmax2), proj.tmin, final_tmin2),
         # add note where predicted infill chosen over temp-adjusted infill
         infill_QA_note = ifelse(!is.na(final_tmax2), "Predicted infill chosen over known temperature-adjusted infill", NA))
# to be sure pair done correctly, verify all final2 values match projected
## should either be true or NA
summary(triage_d1temps2$final_tmax2 == triage_d1temps2$proj.tmax)
summary(triage_d1temps2$final_tmin2 == triage_d1temps2$proj.tmin) # good
# now the rest can be infilled with the calculated (known chart temp adjusted) values
triage_d1temps2$final_tmax2[is.na(triage_d1temps2$final_tmax2)] <- triage_d1temps2$final_tmax[is.na(triage_d1temps2$final_tmax2)]
triage_d1temps2$final_tmin2[is.na(triage_d1temps2$final_tmin2)] <- triage_d1temps2$final_tmin[is.na(triage_d1temps2$final_tmin2)]
# add a flag to indicate whether final temp was adjusted or predicted (to match TK's Flag.2)
# > # Flag 2: A=no infilling; B=unadjusted infilling; C=infilled adjusted for known Tmin; D=infilled adjusted for known Tmax
triage_d1temps2$Flag.2 <- ifelse(triage_d1temps2$final_tmax2 == triage_d1temps2$final_tmax & !is.na(triage_d1temps2$tmin), "C",
                                 ifelse(triage_d1temps2$final_tmax2 == triage_d1temps2$final_tmax & !is.na(triage_d1temps2$tmax), "D", "B"))
# whittle triaged df down to final cols
triage_d1temps2 <- dplyr::select(triage_d1temps2, date, source.station, final_tmax2, final_tmin2, tmean, DTR, Flag.2, infill_QA_note) %>%
  rename(tmax = final_tmax2, tmin = final_tmin2)

# rbind triaged temps back with other predicted temps
final_d1_pred <- subset(d1_lmtemps, !date %in% triage_d1temps2$date) %>%
  #add flag 2 (B = unadjusted infilling)
  mutate(Flag.2 = "B",
         infill_QA_note = NA) %>%
  # keep date, source.station, mean T , DTR, and final tmin and tmax vals
  dplyr::select(date, source.station, final_tmax, final_tmin, tmean, DTR, Flag.2, infill_QA_note) %>%
  rename(tmax = final_tmax,
         tmin = final_tmin) %>%
  # add on triaged adjusted temps
  rbind(triage_d1temps2) %>%
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
  dplyr::select(LTER_site, local_site, date, doy,year:day, tmax:DTR, flag.1, Flag.2, flag.3, source.station,
                tmean_pval, tmean_r2, tmean_n.obs, tmean_equation, DTR_pval, DTR_r2, DTR_n.obs, DTR_equation, infill_QA_note) %>%
  #cbind QA flagging to infilled temps
  left_join(d1qa[d1qa$met == "airtemp_max", c("date", "qa_flag")]) %>%
  rename(Tmax.QAflag = qa_flag) %>%
  left_join(d1qa[d1qa$met == "airtemp_min", c("date", "qa_flag")]) %>%
  rename(Tmin.QAflag = qa_flag)

# clean up environment
rm(triage_d1temps, triage_d1temps2)



# -- PUT IT ALL TOGETHER! -----
# join ctw-infilled with raw 2011-ongoing chart temp data + TK infilled dataset, with all flagging (TK flags and CTW qa flag)

# make list of functions to clean/standardize station names across both datasets
clean_stations <- list(function(x) gsub(" Cr", "-cr", x),
                       function(x) gsub(" Dp", "-dp", x),
                       function(x) gsub("cr_", "-", x),
                       function(x) gsub("c1", "C1", x),
                       function(x) gsub("slddp", "SDL-dp", x),
                       function(x) gsub("d1", "D1", x),
                       function(x) gsub("sdl", "SDL",x),
                       function(x) gsub("_NA", "", x),
                       function(x) gsub("loch", "Loch", x),
                       function(x) gsub("sno", "Niwot Snotel (663)", x))


# c1 -----
# give tkc1temp same colnames as tkd1temp
colnames(tkc1temp) <- colnames(tkd1temp)
c1_compiled <- tkc1temp %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d"),
         doy = yday(date),
         # add LTER and site cols
         LTER_site = "NWT",
         local_site = "C1") %>%
  #re-arrange cols
  dplyr::select(LTER_site, local_site, date, doy, year, month, day, Tmax:ncol(.)) %>%
  #append C to temp colnames
  rename_at(vars(c("Tmax", "Tmin","Tmean")), function(x) paste0(x, ".C")) %>%
  # clean up X obs colname, and standardize equation cols
  rename_all(function(x) gsub("X..", "X.", x)) %>%
  rename_all(function(x) gsub("eqn", "equation", x)) %>%
  #add infill QA note and tmax and tmin qa flag cols
  mutate(infill_QA_note = NA,
         Tmax.QAflag = NA,
         Tmin.QAflag = NA)

# prep qa'd c1 dat for joining (exclude dates infilled)
c1qa_prep <- c1qa[c1qa$yr > 2010, !(colnames(c1qa)) %in% c("c1_flag", "c1_temp")] %>%
  # add cols in TK dataset
  mutate(day = day(date),
         met = gsub("airtemp_", "T", met),
         # fix NA in advisory warning
         qa_flag = gsub("NA; ", "", qa_flag)) %>%
  rename(year = yr,
         month = mon) %>%
  gather(qacol, val, qa_temp, qa_flag) %>%
  unite(met, met, qacol, sep = ".") %>%
  mutate(met = gsub(".qa_temp", "", met),
         met = gsub("qa_", "QA", met)) %>%
  spread(met, val) %>%
  # reclass temps as numeric
  mutate_at(vars("Tmax", "Tmin"), as.numeric) %>%
  rename_at(vars("Tmax", "Tmin"), function(x) paste0(x, ".C")) %>%
  #calculate Tmean and DTR, add other cols from c1 dataset
  mutate(Tmean.C = (Tmax.C + Tmin.C)/2,
         DTR = Tmax.C - Tmin.C,
         flag.1 = "A",
         flag.2 = "A",
         flag.3 = "A",
         source.station = "C1",
         local_site = "C1",
         infill_QA_note = NA) %>%
  #remove dates that have been infilled
  filter(!date %in% final_c1_pred$date) %>%
  #append cols for lminfo
  cbind(data.frame(matrix(ncol = 8, nrow = nrow(.)))) %>%
  rename_at(vars(matches("^X")), function(x) x <- colnames(c1_compiled)[grep("pval|rsq|equ|X.obs", colnames(c1_compiled))]) %>%
  #rearrange cols to match tk's
  dplyr::select(colnames(c1_compiled))

# rename cols in predicted c1 dat and rbind all (all dates should be accounted for and only 1 row per date)
colnames(final_c1_pred) <- colnames(c1qa_prep)
colnames(final_c1_pred)

c1_compiled <- rbind(c1_compiled, c1qa_prep, final_c1_pred) %>%
  arrange(date)
# check only 1 row per date
summary(duplicated(c1_compiled$date)) # good
# check for missing dates
summary(c1_compiled$date %in% seq.Date(min(c1_compiled$date), max(c1qa$date), 1)) # everything there
# append raw dataset for user comparison
c1_compiled <- left_join(c1_compiled, c1raw[c("date", "airtemp_max", "airtemp_min")]) %>%
  rename(raw_Tmax.C = airtemp_max,
         raw_Tmin.C = airtemp_min)

# clean up station names
for(i in clean_stations){
  c1_compiled$source.station <- sapply(c1_compiled$source.station, i)
}
# check names
unique(c1_compiled$source.station) #good
unique(c1_compiled$local_site) #good

# plot to see how it looks
ggplot(c1_compiled, aes(date, Tmax.C)) +
  geom_point(alpha = 0.5) +
  geom_point(aes(date, Tmin.C), col = "lightblue", alpha = 0.5)

# plot yrs QA'd and infilled by CTW
ggplot(subset(c1_compiled, year > 2010), aes(date, Tmax.C)) +
  geom_point(data = subset(c1logqa, met == "airtemp_max" & yr > 2011), aes(date, qa_temp), col = "salmon", alpha = 0.5) +
  geom_point(aes(col = flag.1 != "A"), alpha = 0.5) +
  scale_color_viridis_d()

ggplot(subset(c1_compiled, year > 2010), aes(date, Tmin.C)) +
  geom_point(data = subset(c1logqa, met == "airtemp_min" & yr > 2011), aes(date, qa_temp), col = "salmon", alpha = 0.5) +
  geom_point(aes(col = flag.1 != "A"), alpha = 0.5) +
  scale_color_viridis_d()

summary(c1_compiled)



# d1 -----
#start with tk d1 results, add in Date and doy
d1_compiled <- tkd1temp %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d"),
         doy = yday(date),
         # add LTER and site cols
         LTER_site = "NWT",
         local_site = "D1") %>%
  #re-arrange cols
  dplyr::select(LTER_site, local_site, date, doy, year, month, day, Tmax:ncol(.)) %>%
  #append C to temp colnames
  rename_at(vars(c("Tmax", "Tmin","Tmean")), function(x) paste0(x, ".C")) %>%
  # clean up X obs colname, and standardize equation cols
  rename_all(function(x) gsub("X..", "X.", x)) %>%
  rename_all(function(x) gsub("eqn", "equation", x)) %>%
  #add infill QA note and tmax and tmin qa flag cols
  mutate(infill_QA_note = NA,
         Tmax.QAflag = NA,
         Tmin.QAflag = NA)

# prep qa'd c1 dat for joining (exclude dates infilled)
d1qa_prep <- d1qa[d1qa$yr > 2010, !(colnames(d1qa)) %in% c("d1_flag", "d1_temp")] %>%
  # add cols in TK dataset
  mutate(day = day(date),
         met = gsub("airtemp_", "T", met),
         # fix NA in advisory warning
         qa_flag = gsub("NA; ", "", qa_flag)) %>%
  rename(year = yr,
         month = mon) %>%
  gather(qacol, val, qa_temp, qa_flag) %>%
  unite(met, met, qacol, sep = ".") %>%
  mutate(met = gsub(".qa_temp", "", met),
         met = gsub("qa_", "QA", met)) %>%
  spread(met, val) %>%
  # reclass temps as numeric
  mutate_at(vars("Tmax", "Tmin"), as.numeric) %>%
  rename_at(vars("Tmax", "Tmin"), function(x) paste0(x, ".C")) %>%
  #calculate Tmean and DTR, add other cols from d1 dataset
  mutate(Tmean.C = (Tmax.C + Tmin.C)/2,
         DTR = Tmax.C - Tmin.C,
         flag.1 = "A",
         flag.2 = "A",
         flag.3 = "A",
         source.station = "D1",
         local_site = "D1",
         infill_QA_note = NA) %>%
  #remove dates that have been infilled
  filter(!date %in% final_d1_pred$date) %>%
  #append cols for lminfo
  cbind(data.frame(matrix(ncol = 8, nrow = nrow(.)))) %>%
  rename_at(vars(matches("^X")), function(x) x <- colnames(d1_compiled)[grep("pval|rsq|equ|X.obs", colnames(d1_compiled))]) %>%
  #rearrange cols to match tk's
  dplyr::select(colnames(d1_compiled))

# rename cols in predicted d1 dat and rbind all (all dates should be accounted for and only 1 row per date)
colnames(final_d1_pred) <- colnames(d1qa_prep)
colnames(final_d1_pred)

d1_compiled <- rbind(d1_compiled, d1qa_prep, final_d1_pred) %>%
  arrange(date)
# check only 1 row per date
summary(duplicated(d1_compiled$date)) # good
# check for missing dates
summary(d1_compiled$date %in% seq.Date(min(d1_compiled$date), max(d1qa$date), 1)) # everything there
# append raw dataset for user comparison
d1_compiled <- left_join(d1_compiled, d1raw[c("date", "airtemp_max", "airtemp_min")]) %>%
  rename(raw_Tmax.C = airtemp_max,
         raw_Tmin.C = airtemp_min)

# clean up station names
for(i in clean_stations){
  d1_compiled$source.station <- sapply(d1_compiled$source.station, i)
}
# check station names
unique(d1_compiled$source.station) #good
unique(d1_compiled$local_site) #good

# plot to see how it looks
ggplot(d1_compiled, aes(date, Tmax.C)) +
  geom_point(alpha = 0.5) +
  geom_point(aes(date, Tmin.C), col = "lightblue", alpha = 0.5)

# plot yrs QA'd and infilled by CTW
ggplot(subset(d1_compiled, year > 2010), aes(date, Tmax.C)) +
  geom_point(data = subset(d1logqa, met == "airtemp_max" & yr > 2011), aes(date, qa_temp), col = "salmon", alpha = 0.5) +
  geom_point(aes(col = flag.1 != "A"), alpha = 0.5) +
  scale_color_viridis_d()

ggplot(subset(d1_compiled, year > 2010), aes(date, Tmin.C)) +
  geom_point(data = subset(d1logqa, met == "airtemp_min" & yr > 2011), aes(date, qa_temp), col = "salmon", alpha = 0.5) +
  geom_point(aes(col = flag.1 != "A"), alpha = 0.5) +
  scale_color_viridis_d()

summary(d1_compiled) # in manual review, none of the warmest years are in period ctw treated.. could be real, or possibly d1 chart not adjusted enough in periods where dropped artificially?
# looking at d1 logger tmax, 2017 and 2012 are in top 20 tmax, albeit not most extreme values
# .. something to think about for further correction to dataset and/or code



# -- WRITE OUT COMPILED DATASETS -----
# c1
write_csv(x = c1_compiled, paste0(datpath, "c1_dailytemp_infilled_1952-2018.csv"))

# d1
write_csv(x = d1_compiled, paste0(datpath, "d1_dailytemp_infilled_1952-2018.csv"))
