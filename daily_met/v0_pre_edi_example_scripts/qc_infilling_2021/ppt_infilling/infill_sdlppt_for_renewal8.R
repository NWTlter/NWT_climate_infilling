# infill sdl ppt using kittel et al. methods

# code borrowed from the infill c1 and d1 ppt script but parking sdl code here for now bc this is new
# bringing in more datasets for earlier years

# based on breakpoints analysis (see 00_explore_nwt_ppt.R), will split saddle infill WY 1981-1995, and WY 1996-present. Mean breakpoint at Oct 1995.


# -- SETUP -----
# clean environment, load needed libraries, modify default settings
rm(list = ls())
library(tidyverse)
library(lubridate)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals = c("NP", "NA", NA, "NaN", NaN, ".", -9999)

#set path to climate data qa'd prep data folder
datpath <- "c1_d1_sdl_clim/homogenize_climdat/data/prep_data/"

# -- UTILITY FUNCTIONS -----
source("utility_functions/utility_functions_all.R")

# -- PRETTY DATA FUNCTIONS ----
# add water year to data
wateryear <- function(dat){
  dat$mon <- month(dat$date)
  dat$wy_mon <- with(dat, ifelse(mon %in% c(10:12), mon - 9, mon+3)) # oct = 1, sep = 12
  dat$yr <- year(dat$date)
  dat$wy <- with(dat, ifelse(mon %in% c(10:12), yr+1, yr)) #wy = oct 1-sep 30
  return(dat)
}

# pare down to ameriflux dat only and add site info
pare_flux <- function(dat, matchcols = c("timestamp|^P_"), ID = "US-NR1", localname = "Forest"){
  tempdat <- subset(dat, select = grepl(matchcols, names(dat), ignore.case = T))
  # separate date from time
  tempdat$date_start <- substr(tempdat$TIMESTAMP_START, start = 1, stop = 8) 
  tempdat$date_end <- substr(tempdat$TIMESTAMP_END, start = 1, stop = 8)
  tempdat$time_start <- substr(tempdat$TIMESTAMP_START, start = 9, stop = 12)
  tempdat$time_end <- substr(tempdat$TIMESTAMP_END, start = 9, stop = 12)
  # add site
  tempdat$fluxID <- ID
  tempdat$fluxname <- localname
  # clean up
  tempdat <- dplyr::select(tempdat, date_start:fluxname, TIMESTAMP_START:ncol(tempdat)) %>%
    mutate_at(.vars = c("date_start", "date_end"), .funs = function(x) as.Date(x, format = "%Y%m%d")) %>%
    mutate_at(.vars = names(.)[grepl("^P_", names(.))], as.numeric)
  
  return(tempdat)
}

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
    # > also check pvals are distinct (if same, choose by spatial hierarchy)
    if(nrow(best) > 1 & !all(is.na(best$pval)) & length(unique(best$pval))>1){
      best <- subset(r2_df, r2 == max(r2, na.rm = T) & pval == max(pval, na.rm = T)) 
    }
    if(nrow(best) > 1 & (all(is.na(best$pval)) | length(unique(best$pval))==1)){
      # choose closest station by order hierarchy
      #temp_order <- sapply(best$site, function(x) grep(x, c1_order))
      temp_order <- sapply(best$site, function(x) which(x == site_order))
      best_site <- names(temp_order)[which.min(temp_order)]
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

# choose "best" based on date to infill I guess?
seasonfill_nonzero <- function(dat, target_site, missing_dates, site_order, nobs_limit = 14){
  
  # initiate data frame for storing results
  infill_df_season <- data.frame()
  
  # iterate through each date with missing temp data
  for(d in as.character(missing_dates)){
    # print out to console to keep track of progress
    print(paste("Predicting",d))
    
    d <- as.Date(d, format = "%Y-%m-%d")
    # pull out qdays to backfill
    q <- dat$qdays[dat$date == d]
    # generate date sequence
    event_dates <- seq.Date(d-(q-1), d, 1)
    # check to see that a nonzero is present somewhere in ref data
    # > if no next
    new_order <- site_order
    for(s in site_order){
      checkref <- subset(dat, date %in% event_dates, select = c("date", paste0(s, "_clean")))
      if(all(is.na(checkref[[paste0(s, "_clean")]]))){
        # remove from site order
        new_order <- new_order[!new_order == s]
        next
      }
      if(sum(checkref[[paste0(s, "_clean")]], na.rm = T)==0){
        # remove from site order
        new_order <- new_order[!new_order == s]
      }
    }
    # iterate through infill hiearchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in new_order){
      
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
      r2_df_best <- subset(r2_df, r2 != 1 & nobs >= nobs_limit)
      # if nothing present, revert to best r2
      if(nrow(r2_df_best) ==0){
        r2_df_best <- subset(r2_df, r2 !=1)
        r2_df_best <- subset(r2_df_best, r2==max(r2))
        # be sure only 1 model or no nodel pulled
        stopifnot(nrow(r2_df_best)<=1)
      }
      # assign best to r2_df
      r2_df <- r2_df_best
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
                                  method = "seasonal",
                                  backfill_date = d,
                                  backfill_qdays = q)
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
    # > also check pvals are distinct (if same, choose by spatial hierarchy)
    if(nrow(best) > 1 & !all(is.na(best$pval)) & length(unique(best$pval))>1){
      best <- subset(r2_df, r2 == max(r2, na.rm = T) & pval == max(pval, na.rm = T)) 
    }
    if(nrow(best) > 1 & (all(is.na(best$pval)) | length(unique(best$pval))==1)){
      # choose closest station by order hierarchy
      #temp_order <- sapply(best$site, function(x) grep(x, c1_order))
      temp_order <- sapply(best$site, function(x) which(x == site_order))
      best_site <- names(temp_order)[which.min(temp_order)]
      best <- subset(r2_df, site == best_site)
    }
    
    # infill missing values for entire backfill period based on best model for date of qdays > 1
    # iterate through each event date to backfill
    for(e in event_dates){
      # make sure e is date
      e <- as.Date(e)
      ## re-subset temp_df based on best model
      temp_df <- subset(dat, date > (e-31) & date < (e+30))
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
      if(dat[[paste0(best$site,"_clean")]][dat$date == e] == 0){
        tempinfill <- -Inf
      }else{
        tempinfill <- predict(best_mod, newdata = subset(dat, date == e))
      }
      tempinfill_df <- data.frame(date = e, infill = tempinfill,
                                  source.station = best$site,
                                  logger = best$logger, 
                                  pval = summary(best_mod)$coefficients[8],
                                  r2 = summary(best_mod)$r.squared,
                                  n.obs = nrow(best_mod$model),
                                  equation = paste0("y = ", best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                  infillrun = length(d),
                                  method = "seasonal",
                                  backfill_date = d,
                                  backfill_qdays = q)
      
      # append model infill to master infill df
      infill_df_season <- rbind(infill_df_season, tempinfill_df)
    }
    
    # after all event dates are infilled, clean up things that shouldn't be recycled
    rm(r2_df, tempinfill_df, best, logger)
    
  }
  # return infilled df
  return(infill_df_season)
}


historicfill_nonzero <- function(dat, target_site, missing_dates, site_order, nobs_limit = 14){
  
  # initiate data frame for storing results
  infill_df_m2 <- data.frame()
  
  # iterate through each date with missing temp data
  for(d in as.character(missing_dates)){
    # print out to console to keep track of progress
    print(paste("Predicting",d))
    
    d <- as.Date(d, format = "%Y-%m-%d")
    # pull out qdays to backfill
    q <- dat$qdays[dat$date == d]
    # generate date sequence
    event_dates <- seq.Date(d-(q-1), d, 1)
    # check to see that a nonzero is present somewhere in ref data
    # > if no next
    new_order <- site_order
    for(s in site_order){
      checkref <- subset(dat, date %in% event_dates, select = c("date", paste0(s, "_clean")))
      if(all(is.na(checkref[[paste0(s, "_clean")]]))){
        # remove from site order
        new_order <- new_order[!new_order == s]
        next
      }
      if(sum(checkref[[paste0(s, "_clean")]], na.rm = T)==0){
        # remove from site order
        new_order <- new_order[!new_order == s]
      }
    }
    
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
    
    
    # iterate through infill hierarchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in new_order){
      
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
      r2_df_best <- subset(r2_df, r2 != 1 & nobs >= nobs_limit)
      # if nothing present, revert to best r2
      if(nrow(r2_df_best) ==0){
        r2_df_best <- subset(r2_df, r2 != 1)
        r2_df_best <- subset(r2_df_best, r2==max(r2))
        # be sure only 1 model pulled
        stopifnot(nrow(r2_df_best)<=1)
      }
      # assign best to r2_df
      r2_df <- r2_df_best
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
                                  method = "multi-yr",
                                  backfill_date = d,
                                  backfill_qdays = q)
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
    # if there is a tie in r2, choose lowest pval (unless tie is r2==0)
    # > also check pvals are distinct (if same, choose by spatial hierarchy)
    if(nrow(best) > 1 & !all(is.na(best$pval)) & length(unique(best$pval))>1){
      best <- subset(r2_df, r2 == max(r2, na.rm = T) & pval == max(pval, na.rm = T)) 
    }
    if(nrow(best) > 1 & (all(is.na(best$pval)) | length(unique(best$pval))==1)){
      # choose closest station by order hierarchy
      #temp_order <- sapply(best$site, function(x) grep(x, c1_order))
      temp_order <- sapply(best$site, function(x) which(x == site_order))
      best_site <- names(temp_order)[which.min(temp_order)]
      best <- subset(r2_df, site == best_site)
    }
    
    # infill missing values for entire backfill period based on best model for date of qdays > 1
    # iterate through each event date to backfill
    for(e in event_dates){
      # make sure e is date
      e <- as.Date(e)
      # recrunch doyrange
      # specify time window
      e_tempdoy <- dat$doy[dat$date == e]
      e_doyrange <- (e_tempdoy-3):(e_tempdoy+3)
      #adjust if at beginning or end of year -- going to ignore extra day in leap years, otherwise have to choose based on month and day, this is easier and close enough
      if(any(e_doyrange<1)){
        e_doyrange[e_doyrange<1] <- e_doyrange[e_doyrange<1] +365
      }
      if(any(e_doyrange>365)){
        e_doyrange[e_doyrange>365] <- e_doyrange[e_doyrange>365] - 365
        # add 366 to be sure
        e_doyrange <- sort(c(e_doyrange, 366))
      }
      
      # infill missing values in entire backfill event based on best model
      ## re-subset temp_df based on best model
      temp_df <- subset(dat, doy %in% e_doyrange)
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
      if(dat[[paste0(best$site,"_clean")]][dat$date == e] == 0){
        tempinfill <- -Inf
      }else{
        tempinfill <- predict(best_mod, newdata = subset(dat, date == e))
      }
      tempinfill_df <- data.frame(date = e, infill = tempinfill,
                                  source.station = best$site,
                                  logger = best$logger, 
                                  pval = summary(best_mod)$coefficients[8],
                                  r2 = summary(best_mod)$r.squared,
                                  n.obs = nrow(best_mod$model),
                                  equation = paste0("y = ", best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                  infillrun = length(d),
                                  method = "multi-yr",
                                  backfill_date = d,
                                  backfill_qdays = q)
      
      # append model infill to master infill df
      infill_df_m2 <- rbind(infill_df_m2, tempinfill_df)
    }
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
  dat$Flag.2[dat$date %in% single_dates] <- "A" # not adjusetd for any period total (accumlated ppt)
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


# -- GET DATA ----
# raw sdl ppt dataset from EDI
sdlppt <- getTabular(416) %>% data.frame()
# infilled sdl ppt dataset from renewal
sdl_infilled <- read_csv(paste0(datpath, "sdl_ppt_infill_19822018_nsfctw.csv"), na = na_vals)
# infilled d1 ppt from EDI
d1ppt <- getTabular(186) %>% data.frame()
# infilled c1 ppt from EDI
c1ppt <- getTabular(184) %>% data.frame() # from chart
c1cr_ppt <- getTabular(401) %>% data.frame() # from logger --only available at C1


# ctw prepped data -- not QA'd
snotel_ppt <- read_csv(paste0(datpath, "nwt_snotel_ppt.csv"))
uc_ppt <- read_csv(paste0(datpath, "nwt_univcamp_ppt.csv"))

# ameriflux
flux_datpath <- "/Users/scarlet/Documents/nwt_lter/aux_climdats/AmeriFlux/"
Forest <- read.csv(paste0(flux_datpath, "AMF_US-NR1_BASE-BADM_18-5/AMF_US-NR1_BASE_HH_18-5.csv"),
                   skip =2, colClasses = "character", na.strings = na_vals)

TvanW <- read.csv(paste0(flux_datpath, "AMF_US-NR3_BASE-BADM_2-5/AMF_US-NR3_BASE_HH_2-5.csv"), 
                  skip =2, colClasses = "character", na.strings = na_vals)

TvanE <- read.csv(paste0(flux_datpath, "AMF_US-NR4_BASE-BADM_2-5/AMF_US-NR4_BASE_HH_2-5.csv"),
                  skip =2, colClasses = "character", na.strings = na_vals)
names(TvanW); names (TvanE) # I don't think there are ppt data in these dats
rm(TvanW, TvanE)
# winnow ameriflux to just ppt
forestppt <- pare_flux(Forest)

# other stations within regions used to infill kittel datasets 1952-2010
regionppt <- read.csv("/Users/scarlet/Documents/nwt_lter/nwt_climate/raw_data/2802258.csv", na.strings = na_vals) #13MB file



# -- PREP DATA -----
# make a master ppt data frame as done with temp data
# calculate a "clean" col for each station where non-zero ppt ln'd, and remove any infilled values
# also specify infill hierarchy (even tho will choose best model in the end)
# can copy/modify temp infill functions

# prep sdl chart -----
# run qa functions
sdlppt <- clean_qdays(sdlppt)
check_qdays(sdlppt)
# check bad dates -- since only using sdl to infill, NA inconsistent vals to be sure not using bad data to infill c1 and d1
# "2013-04-30"
View(subset(sdlppt, date > "2013-04-15")) # 0 ppt, 1 qday
# "2014-07-22"
View(subset(sdlppt, date > "2014-07-01")) # 0 ppt, 1 qday
# "2015-06-30"
View(subset(sdlppt, date > "2015-06-01")) # 0 ppt, 1 qday
# 2016-12-19
View(subset(sdlppt, date > "2016-12-01")) # 0 ppt, NA qday
# for loop clean up
for(d in c("2013-04-30", "2014-07-22", "2015-06-30", "2016-12-19")){
  sdlppt$ppt_tot[sdlppt$date == as.Date(d)] <- NA
  sdlppt$qdays[sdlppt$date == as.Date(d)] <- NA
  
}
# verify sdl chart again
check_qdays(sdlppt) #good
# want master data frame with columns:
## date, doy, yr, mon, day, [site]_clean, [site]_ln
sdl_prep <- sdlppt %>%
  mutate(sdl_clean = ifelse(!is.na(flag_ppt_tot) | qdays > 1, NA, ppt_tot), # NA anything that's been infilled, or has qdays > 1 (e.g., pen stuck)
         # note > looking at metadata on EDI, there are data marked as questionable in the notes that do not have a q flag indicated (namely 2011 onwards)
         sdl_ln = ifelse(sdl_clean > 0, log(sdl_clean), NA),
         doy = yday(date), yr = year(date), mon = month(date), day = day(date)) %>%
  subset(select = c(date, doy, yr, mon, day, ppt_tot, qdays, sdl_clean, sdl_ln))


# prep c1 and d1 chart -----
# start with c1, make master data frame with chart ppt > y2010 rbinded to tk dataset
# want both tk flag 1 == A (C1 is source) and flag2  == A (uninfilled data), G (0 precip) or NA (for yrs >2010)
c1_prep <- c1ppt %>%
  # there are some errant flags for infilled values in 2018! says A and G but is infilled..
  # because infilling more years for sdl, allow backfilled data -- just ignore values entirely filled from another site
  # because allowing backfilled, NA those with pval > 0.05
  mutate(c1_clean = ifelse(flag_2 %in% LETTERS[grep("H", LETTERS):grep("M", LETTERS)] | flag_1 %in% c("C", "E"), NA, precip)) %>%
  # add ln ppt for non-zero ppt
  mutate(c1_ln = ifelse(c1_clean > 0, log(c1_clean), NA)) %>%
  rename(yr = year) %>%
  # start with first date C1 instrument in place
  subset(date >= min(date[grepl("C1", source_station)]), select= c(date, yr, c1_clean, c1_ln))

# repeat for d1
d1_prep <- d1ppt %>%
  mutate(d1_clean = ifelse(flag_2 %in% LETTERS[grep("H", LETTERS):grep("M", LETTERS)] | flag_1 %in% c("C", "E"), NA, precip)) %>%
  # add ln ppt for non-zero ppt
  mutate(d1_ln = ifelse(d1_clean > 0, log(d1_clean), NA)) %>%
  # start with first date d1 instrument in place
  subset(date >= min(date[grepl("D1", source_station)]), select= c(date, d1_clean, d1_ln))


# prep c1 logger -----
c1cr_prep <- subset(c1cr_ppt, select = c(logger, date, ppt_tot, flag_ppt_tot)) %>%
  mutate(c1cr_clean = ifelse(flag_ppt_tot != "n", NA, ppt_tot),
         c1cr_ln = ifelse(c1cr_clean >0, log(c1cr_clean), NA)) %>%
  dplyr::select(date, logger, c1cr_clean, c1cr_ln) %>%
  rename(c1cr_logger = logger)


# prep snotel -----
# NA B, K, X or S in qc flag (means estimated or suspect)
snotel_est <- c("B", "K", "X", "S")
# c1 snotel
sno_prep <- mutate(snotel_ppt,
                   sno_clean = ifelse(ppt_qcflag %in% snotel_est | ppt_qaflag %in% snotel_est, NA, ppt_mm),
                   sno_ln = ifelse(sno_clean > 0, log(sno_clean), NA)) %>%
  dplyr::select(date:doy, sno_clean, sno_ln)

# university camp snotel
uc_prep <- mutate(uc_ppt, 
                  uc_clean = ifelse(ppt_qcflag %in% snotel_est | ppt_qaflag %in% snotel_est,  NA, ppt_mm),
                  uc_ln = ifelse(uc_clean > 0, log(uc_clean), NA)) %>%
  dplyr::select(date:doy, uc_clean, uc_ln)

# prep ameriflux -----
# find the first date prcp starts
forest_prep <- subset(forestppt, date_start >= min(forestppt$date_start[!is.na(forestppt$P_PI_F_1_1_1)])) %>%
  group_by(date_start) %>%
  summarise(forest_ppt = sum(P_PI_F_1_1_1),
            nobs = length(P_PI_F_1_1_1[!is.na(P_PI_F_1_1_1)])) %>%
  ungroup() %>% # pause to look .. days w NAs are 2021 jan 1 forward, which is fine, don't need right now
  subset(nobs == 48) %>% # must have expected 30-min obs
  mutate(forest_ln = ifelse(forest_ppt >0, log(forest_ppt), NA)) %>%
  rename(forest_clean = forest_ppt, date = date_start) %>%
  subset(select = c(date, forest_clean, forest_ln))

# prep regional -----
# flags in GHCND metadata

#Table 1 (Measurement Flag/Attribute) -- in attribute pos 1
# Blank = no measurement information applicable
# A = value in precipitation or snow is a multi-day total, accumulated since last measurement
# (used on Daily Form pdf file)
# B = precipitation total formed from two twelve-hour totals
# D = precipitation total formed from four six-hour totals
# H = represents highest or lowest hourly temperature (TMAX or TMIN)
# or average of hourly values (TAVG)
# K = converted from knots
# L = temperature appears to be lagged with respect to reported
# hour of observation
# O = converted from oktas
# P = identified as "missing presumed zero" in DSI 3200 and 3206 T = trace of precipitation, snowfall, or snow depth
# W = converted from 16-point WBAN code (for wind direction)

# Table 2 (Quality Flag/Attribute) -- will be in 2nd attribute position
# Blank = did not fail any quality assurance check D = failed duplicate check
# G = failed gap check
# I = failed internal consistency check
# K = failed streak/frequent-value check
# L = failed check on length of multiday period M = failed mega-consistency check
# N = failed naught check
# O = failed climatological outlier check
# R = failed lagged range check
# S = failed spatial consistency check
# T = failed temporal consistency check
# W = temperature too warm for snow
# X = failed bounds check
# Z = flagged as a result of an official Datzilla investigation

# Table 3 (Source Flag/Attribute) -- will be in 3rd attribute position
# Blank = No source (i.e., data value missing)
# 0 = U.S. Cooperative Summary of the Day (NCDC DSI-3200)
# 7 = U.S. Cooperative Summary of the Day -- Transmitted via WxCoder3 (NCDC DSI-3207)
# H = High Plains Regional Climate Center real-time data
# Z = Datzilla official additions or replacements
# N = Community Collaborative Rain, Hail,and Snow (CoCoRaHS)

# split out pcp attributes
region_prep <- subset(regionppt, select= c(STATION:DATE, PRCP, PRCP_ATTRIBUTES))
names(region_prep) <- casefold(names(region_prep))
region_prep$att1 <- str_extract(region_prep$prcp_attributes, "^[A-Z]?(?=,)")
region_prep$att2 <- str_extract(region_prep$prcp_attributes, "(?<=^[A-Z]?,)[:alnum:]*(?=,)")
region_prep$att3 <- gsub("^[A-Z]?,[A-Z]?,", "", region_prep$prcp_attributes)
region_prep$att3 <- gsub(",[0-9]+$|,$", "", region_prep$att3)
region_prep$att4 <- str_extract(region_prep$prcp_attributes, "(?<=,)[:digit:]*$")
region_prep <- region_prep %>%
  mutate_at(.vars = names(.)[grep("^att", names(.))], function(x) ifelse(x == "", NA, x))

# how many dats sources by another station?
with(region_prep, sapply(split(att3, name), function(x) summary(as.factor(x))))
# other flags:
with(region_prep, sapply(split(att2, name), function(x) summary(as.factor(x)))) # qa flag
with(region_prep, sapply(split(att1, name), function(x) summary(as.factor(x)))) # measurement flag (T = trace, P = presumed 0)
# > to only keep dats with no flags would be to remove bulk of the data.. so I guess leave all for now and see how often models from these places are chosen
# >expecting closer stations will be selected first

# create abbreviation tbl
regionabbr <- data.frame(name = sort(unique(region_prep$name)))
regionabbr$abbr <- c("bldr14","cc0.4", "cccyn", "ep3", "ep3.3", "fraser", "grand1", "grand6", "ned2.6")

region_prep2 <- mutate(region_prep, 
                       yr = year(date),
                       ln = ifelse(prcp == 0, NA, log(prcp)),
                       date = as.Date(date)) %>%
  rename(clean = prcp) %>%
  left_join(regionabbr) %>%
  dplyr::select(date, abbr, clean, ln) %>%
  gather(met, val, clean, ln) %>%
  unite(met, abbr, met) %>%
  spread(met, val)

# combine all -----
masterppt <- left_join(sdl_prep, d1_prep) %>%
  left_join(c1_prep) %>%
  left_join(c1cr_prep) %>%
  left_join(forest_prep) %>%
  left_join(sno_prep) %>%
  left_join(uc_prep) %>%
  left_join(region_prep2)



# -- INFILL SDL PPT -----
# specify station hierarchies
# regional: c("bldr14","cc0.4", "cccyn", "ep3", "ep3.3", "fraser", "grand1", "grand6", "ned2.6")
sdl_order <- c("d1", "c1cr", "c1", "forest", "sno", "uc", "bldr14", "ned2.6", "ep3", "ep3.3", "grand1", "grand6", "fraser", "cccyn", "cc0.4")

# specify missing dates -- only need to infill 2011 onwards
sdl_missing_dates <- masterppt$date[is.na(masterppt$sdl_clean)]

sdl_seasonfill <- tk_ppt_seasonalfill(masterppt, target_site = "sdl", sdl_missing_dates, sdl_order)
sdl_longfill <- tk_ppt_historicfill(masterppt, "sdl", sdl_missing_dates, sdl_order)

# iterate through each date and choose best model (pval under 0.05 and highest r2)
sdl_choose <- data.frame()
for(d in sdl_missing_dates){
  temp_df <- rbind(sdl_seasonfill[sdl_seasonfill$date == d,],
                   sdl_longfill[sdl_longfill$date == d,])
  best <- subset(temp_df, r2 == max(r2, na.rm = T) & pval <= 0.05)
  if(nrow(best) ==0){
    # choose highest r2 if no pval under 0.05
    best <- subset(temp_df, r2 == max(r2, na.rm = T) & !is.na(pval))
  }
  sdl_choose <- rbind(sdl_choose, best)
}
# check all dates there
summary(sdl_missing_dates %in% sdl_choose$date) # all there
# exponentiate infill values
sdl_choose$exp.ppt <- exp(sdl_choose$infill) 


#infill sdl with backfill function (backfill fxn contains infill_singles fxn to infill missing days)
sdl_prep2 <- sdl_prep %>%
  # pair model fits
  left_join(sdl_choose)
sdl_prep3 <- backfill_ppt(sdl_prep2)

# see how it looks
ggplot(subset(sdl_prep3, qdays == 1), aes(date, ppt_tot)) +
  geom_point( alpha = 0.5) +
  geom_point(data = sdl_prep3, aes(date, backfill), col = "pink", alpha = 0.5)

# view points that have backfill value even tho sdl ppt present and qdays = 1
View(subset(sdl_prep3, Flag.2 == "A" & Flag.1 != "A")) # -- it's bc ppt vals present were infilled using older methods
ggplot(subset(sdl_prep3, qdays == 1), aes(ppt_tot, backfill, col = source.station)) + 
  geom_point()  +
  geom_abline(aes(slope = 1, intercept = 0))

ggplot(subset(sdl_prep3, qdays == 1), aes(source.station, ppt_tot - backfill)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.5)

# bring in original sdl flags to keep track of infilled vals I'm replacing with new methods
sdl_prep3 <- left_join(sdl_prep3, sdlppt[c("date", "flag_ppt_tot")])

#infill flags for dates not infilled (flag.1 = A and flag.2 = A)
View(subset(sdl_prep3, is.na(Flag.1) & is.na(equation)))
sdl_prep3 <- mutate(sdl_prep3,
                    Flag.1 = ifelse(is.na(Flag.1) & is.na(equation), "A", Flag.1),
                    Flag.2 = ifelse(is.na(Flag.2) & is.na(equation), "A", Flag.2),
                    # specify SDL as source
                    source.station = ifelse(Flag.1 == "A" & Flag.2 == "A", "SDL Belfort Shielded", source.station))

# ID dates that still need to be backfilled but best model had 0 accumulated
sdl_outstanding_backfill <- sort(unique(with(sdl_prep3, date[grepl("^0", Flag.2) & !is.na(qdays)])))
# these will be the **I flags in Flag 2:
# > **I - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by Belfort Recording Gauge: Shielded.
View(subset(sdl_prep3, date %in% sdl_outstanding_backfill))
# > note: all of these dates with qdays > 1 have qual notes in the metadata that aren't reflected in the dataset.. something to revisit when have more time

# try seasonal backfill using sites that have non-zero ppt in the qdays range
sdl_nonzero_seasonal <- seasonfill_nonzero(masterppt, target_site = "sdl", missing_dates = sdl_outstanding_backfill, site_order = sdl_order)
sdl_nonzero_historic <- historicfill_nonzero(masterppt, target_site = "sdl", missing_dates = sdl_outstanding_backfill, site_order = sdl_order)

sdl_choose_nonzero <- data.frame()
for(d in sdl_outstanding_backfill){
  d <- as.Date(d)
  temp_df <- rbind(sdl_nonzero_seasonal[sdl_nonzero_seasonal$date == d,],
                   sdl_nonzero_historic[sdl_nonzero_historic$date == d,])
  best <- subset(temp_df, r2 == max(r2, na.rm = T) & pval <= 0.05)
  if(nrow(best) ==0){
    # choose highest r2 if no pval under 0.05
    best <- subset(temp_df, r2 == max(r2, na.rm = T) & !is.na(pval))
  }
  # select the predicted data from the best method
  if(best$method == "seasonal"){
    best_infill <- subset(sdl_nonzero_seasonal, backfill_date == best$backfill_date)
  }else{
    best_infill <- subset(sdl_nonzero_historic, backfill_date == best$backfill_date)
  }
  sdl_choose_nonzero <- rbind(sdl_choose_nonzero, best_infill)
}
# exponentiate infill values
sdl_choose_nonzero$exp.ppt <- exp(sdl_choose_nonzero$infill) 

#infill sdl with backfill function (backfill fxn contains infill_singles fxn to infill missing days)
sdl_prep_nonzero <- subset(sdl_prep, date %in% sdl_choose_nonzero$date) %>%
  # pair model fits
  left_join(sdl_choose_nonzero[names(sdl_choose)])
sdl_prep_nonzero2 <- backfill_ppt(sdl_prep_nonzero)
# change flag2 to I
sdl_prep_nonzero2$Flag.2 <- "I"
# add flag_ppt_tot to match main dataset
sdl_prep_nonzero2 <- left_join(sdl_prep_nonzero2, sdlppt[c("date", "flag_ppt_tot")])

# replace nonzero infilled data in primary infill dataset and proceed with single infilling as needed
sdl_master <- subset(sdl_prep3, !date %in% sdl_prep_nonzero2$date) %>%
  rbind(sdl_prep_nonzero2) %>%
  arrange(date)


# -- CLEAN UP -----
# make dataset that matches format on EDI
names(d1ppt)
names(sdl_master)
sdl_master2 <- sdl_master %>%
  # join regional station abbreviations
  left_join(regionabbr, by = c("source.station"="abbr")) %>%
  # append logger if logger used to infill
  mutate(clean_source = ifelse(!is.na(name), paste0("GHCNd - ", name), source.station),
         clean_source = gsub("d1", "D1 Belfort Shielded", clean_source),
         clean_source = gsub("c1", "C1 Belfort Shielded", clean_source),
         clean_source = gsub("uc", "University Camp Snotel (838)", clean_source),
         clean_source = gsub("sno", "Niwot Snotel (663)", clean_source),
         clean_source = gsub("forest", "AmeriFlux US-NR1", clean_source),
         clean_source = ifelse(!is.na(logger), paste("C1", logger, sep = "-"), clean_source),
         # create final ppt col
         precip = ifelse(!is.na(backfill), backfill, ppt_tot),
         LTER_site = "NWT", local_site = "SDL") %>%
  #rearrange cols to match tk's
  dplyr::select(LTER_site, local_site, yr, date, mon, doy, day, precip, Flag.1, Flag.2, clean_source, pval, r2, n.obs, equation, ppt_tot, qdays, flag_ppt_tot) %>%
  rename(pvalue = pval, rsquared = r2, num_obs_in_regression_equation = n.obs, regression_equation = equation, year = yr,
         raw_ppt_tot = ppt_tot, raw_qdays = qdays, raw_flag_ppt_tot = flag_ppt_tot, flag_1 = Flag.1, flag_2 = Flag.2, source_station = clean_source)

# check all dates there (should be all TRUE)
summary(seq.Date(min(sdl_master2$date), max(sdl_master2$date), 1) %in% unique(sdl_master2$date)) # yes

# plot to examine
ggplot(sdl_master2, aes(date, precip, col = !is.na(regression_equation))) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_d(name = "Infilled?", direction =-1)

# drop cols to match EDI dataset
sdl_master_out <- sdl_master2 %>%
  # add winter adjusted ppt column too
  mutate(winter_adjusted = ifelse(mon %in% c(10:12, 1:5), precip*0.39, precip)) %>%
  subset(select = -c(mon, doy, day)) %>%
  #rearrange and round numbers to 2 decimals
  subset(select = c(LTER_site:precip, winter_adjusted, flag_1:raw_flag_ppt_tot)) %>%
  mutate(precip = round(precip, 2), winter_adjusted = round(winter_adjusted,2))
summary(names(d1ppt) %in% names(sdl_master_out))
summary(names(sdl_master_out) %in% names(d1ppt)) # keeping raw flag is extra column + winter adjusted

# plot with winter adjusted amounts
ggplot(sdl_master_out, aes(date, winter_adjusted, col = !is.na(regression_equation))) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_d(name = "Infilled?", direction =-1)


# -- WRITE OUT -----
write.csv(sdl_master_out, "c1_d1_sdl_clim/homogenize_climdat/data/sdl_ppt_1981-2020_draft.csv", row.names = F)
