# functions to infill temp and precip following Tim Kittel methods

# TK uses two methods:
# 1) a 14-days moving window regression
# 2) +/- 3 days of the day of year through the time series
# run each method, choose the best based on r2 and pval, then use tmean and DTR predicted to calculate tmax and tmin for the day



# prepartory functions:
# id dates in time series that need infilling for any metric
idInfillDates <- function(dat, site, startyr){
  infilldates <- with(dat, unique(date[local_site == site & yr >= startyr & is.na(measurement)]))
  return(infilldates)
}


# everything needs 1) airtemp_avg & 2) DTR + local_site, yr, date, doy, metric + measurement
# make data frame of airtemp_avg and dtr appropriate to run through infill functions
mean_and_diurnalT <- function(dat){
  
  tempdat <- subset(dat, select = c(date, yr, mon, doy, local_site, metric, measurement)) %>%
    tidyr::spread(metric, measurement)
  # if avg temp not present, make
  if(!any(grepl("avg", names(tempdat)))){
    tempdat$airtemp_avg <- (tempdat$airtemp_max + tempdat$airtemp_min)/2
  }
  # if diurnal temp not present, make
  if(!any(grepl("DTR", names(tempdat), ignore.case = T))){
    tempdat$DTR <- tempdat$airtemp_max - tempdat$airtemp_min
  }
  
  # return all mets back in case helpful for crunching tmax and tmin after predictions
  tempdat <- gather(tempdat, metric, measurement, grep("temp|DTR", names(tempdat)))
  return(tempdat)
}



# function for short-term seasonal tk infill method
# choose best model based on r2 and pval for tmean (in tk dat, predicted tmean and dtr come from same source station, can't differ within day)

# updated movingfill based on 2022 replication (still can't replicate perfectly, but it's pretty close: e.g., correct #obs, pval, coefficient and r2 within nearest decimal place)
# to be clear: 2018 attempt wasn't that off either, this is just closer

# 'new' rules (what's different in 2022):
# source station MUST have tmean and DTR vals to be used (e.g., if electronic source has tmean but is missing tmin or tmax that day, cannot either for tmean or DTR infilling on missing date)
# infill one date at a time (not batch of dates in same window)
# moving window is determined by: 
# 1) two week buffer looking backwards and forwards of most recent non-NA dates on both sides of missing dates
# 2) cap backwards and forwards endpoints within a month (-/+30d) of missing date to infill
# > max # obs should be 28 OR fewer (if bc capped within a month)
# same window data predicts missing tmean and missing DTR, tmean and DTR must be predicted by same source station on a given date

tk_temp_movingfill <- function(dat, target_site, missing_dates, site_order, window_days=13, metric = c("airtemp_avg", "DTR"), windowcap = 30, nobs_limit = 14){
  # initiate empty data frame for storing predictions and regression info
  dat_out <- data.frame()
  for(d in as.character(missing_dates)){
    
    # print dates being infilled to keep track of progress
    print(paste("Predicting", d))
    
    # convert to date
    d <- as.Date(d, format = "%Y-%m-%d")
    
    # specify a month out (30d) from infill date in question
    # ? did TK do two weeks entire non-NA, or only two weeks absolute?
    start_date <- d - windowcap
    end_date <- d + windowcap - 1
    
    # subset dat
    temp_df <- subset(dat, date >= start_date & date <= end_date)
    
    # if all of targets dat is NA for the period, next
    datpresent <- sum(!is.na(temp_df$measurement[temp_df$local_site == target_site & temp_df$metric == "DTR"]))
    
    if(datpresent == 0){
      
      # set everything to NA and note insufficient nobs
      tempinfill_df <- data.frame(date = d, 
                                  infill = NA,
                                  metric = NA,
                                  source.station = NA,
                                  pval = NA,
                                  r2 = NA,
                                  n.obs = NA,
                                  equation = NA,
                                  method = paste0((window_days+1),"-d moving window"))
      
      # append model infill to master infill df
      dat_out <- rbind(dat_out, tempinfill_df)
      
      next
    }
    
    # set nobs limit as default or whatever is available for target_site in period
    temp_nobs <- ifelse(datpresent >= nobs_limit, nobs_limit, 7) # make 7 days the min reqd (that is min TK had)
    
    
    # iterate through infill hiearchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in site_order){
      
      # select 2 week window of nonNA dates looking backwards and forwards
      
      #print(site)
      if(!site %in% unique(temp_df$local_site)){
        next
      }
      # widen data
      wide_df <- subset(temp_df, local_site %in% c(target_site, site), select = c(date:measurement))
      wide_df <- tidyr::unite(wide_df, metric, local_site, metric)
      wide_df <- tidyr::spread(wide_df, metric, measurement)
      wide_df$allthere <- apply(wide_df[grep(stringr::str_flatten(metric, collapse = "|"), names(wide_df))], 1, function(x)all(!is.na(x)))
      
      # refine start and end by station availability
      
      # check there are observations > 0 for explanatory site
      if(sum(wide_df$allthere) == 0){
        next
      }
      
      # check that values present for both metrics on date in question
      sitecols <- paste(site, metric, sep = "_")
      if(any(is.na(wide_df[wide_df$date == d, sitecols]))){
          next
        }
      
      
      # choose 14 nonNA dates preceeding and following target infill fate (within monthcap)
      preceed_dates <- with(wide_df, date[date < d & allthere])
      follow_dates <- with(wide_df, date[date > d & allthere])
      
      wide_df <- subset(wide_df, date %in% c(tail(preceed_dates, n = 14),head(follow_dates, n = 14)))
      # # check that target and reference site have days that overlap (and count of overlap > nobs_limit)
      # target_dates <- unique(wide_df$date[!is.na(wide_df[[target_site]])])
      # site_dates <- unique(wide_df$date[!is.na(wide_df[[site]])])
      # 
      # #if the target_dates are under nobs limit, set nobs limit at length of target_dates
      # if(length(target_dates) < nobs_limit){
      #   temp_nobs <- length(target_dates)
      # }
      # if(sum(target_dates %in% site_dates) < temp_nobs){
      #   next
      # }
      # 
      mod <- lm(formula = paste0(target_site, "_airtemp_avg ~ ", site, "_airtemp_avg"), data = wide_df)
      
      r2_df <- rbind(r2_df, 
                     data.frame(cbind(site = site,
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
      r2_df <- subset(r2_df, r2 != 1 & nobs >= temp_nobs)
    }
    
    # if there is nothing bc nothing , next
    if(nrow(r2_df) == 0){
      
      # set everything to NA and note insufficient nobs
      tempinfill_df <- data.frame(date = d, 
                                  infill = NA,
                                  metric = NA,
                                  source.station = NA,
                                  pval = NA,
                                  r2 = NA,
                                  n.obs = NA,
                                  equation = NA,
                                  method = paste0((window_days+1),"-d moving window"))
      
      # append model infill to master infill df
      dat_out <- rbind(dat_out, tempinfill_df)
      
      next
    }
    
    # select best
    best <- subset(r2_df, pval <= 0.05) # subset to those with signif pvals
    best <- subset(best, r2 == max(r2, na.rm = T))
    # if nothing has signif pval, select best r2
    if(nrow(best) == 0){
      best <- subset(r2_df, r2 == max(r2, na.rm = T))
    }
    
    # re-subset data based on best model
    # widen data
    wide_df <- subset(temp_df, local_site %in% c(target_site, best$site), select = c(date:measurement))
    wide_df <- tidyr::unite(wide_df, local_site, local_site, metric)
    wide_df <- tidyr::spread(wide_df, local_site, measurement)
    wide_df$allthere <- apply(wide_df[grep(stringr::str_flatten(metric, collapse = "|"), names(wide_df))], 1, function(x)all(!is.na(x)))
    
    # winnow dates
    # choose 14 nonNA dates preceeding and following target infill fate (within monthcap)
    preceed_dates <- with(wide_df, date[date < d & allthere])
    follow_dates <- with(wide_df, date[date > d & allthere])
    
    wide_df <- subset(wide_df, date %in% c(tail(preceed_dates, n = 14),head(follow_dates, n = 14), d))
    
    # infill missing values in time_window based on best model
    for(m in metric){
      best_mod <- lm(formula = paste0(target_site, "_", m, " ~ ", best$site, "_", m), data = subset(wide_df, allthere))
      tempinfill <- predict(best_mod, newdata = wide_df[wide_df$date == d,])
      tempinfill_df <- data.frame(date = d, 
                                  infill = tempinfill,
                                  metric = m,
                                  source.station = best$site,
                                  pval = summary(best_mod)$coefficients[8],
                                  r2 = summary(best_mod)$r.squared,
                                  n.obs = nrow(best_mod$model),
                                  equation = paste0("y = ",  best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                  method = paste0((window_days+1),"-d moving window"))
      
      
      # append model infill to master infill df
      dat_out <- rbind(dat_out, tempinfill_df)
    }
    
  }
  # return infilled df when finished
  return(dat_out)
}





# build multi-year regression, +- 3 days on each side of target infill date
tk_temp_historicfill <- function(dat, target_site, missing_dates, site_order, metric = c("airtemp_avg", "DTR"), nobs_limit = 14){
  
  # initiate data frame for storing results
  dat_out <- data.frame()
  
  # iterate through each date with missing temp data
  for(d in as.character(missing_dates)){
    # print out to console to keep track of progress
    print(paste("Predicting",d))
    
    d <- as.Date(d, format = "%Y-%m-%d")
    # specify time window
    tempdoy <- unique(dat$doy[dat$date == d])
    stopifnot(length(tempdoy) == 1)
    doyrange <- (tempdoy-3):(tempdoy+3)
    #adjust if at beginning or end of year -- going to ignore extra day in leap years, otherwise have to choose based on month and day, this is easier and close enough
    if(any(doyrange<1)){
      doyrange[doyrange<1] <- doyrange[doyrange<1] +365
    }
    if(any(doyrange>365)){
      doyrange[doyrange>365] <- doyrange[doyrange>365] - 365
    }
    # if doyrange contains 365 and 1, add 366 in as well (error thrown for infilling 12/31 on leap years if don't include 366)
    if(365 %in% doyrange & 1 %in% doyrange){
      doyrange <- c(366, doyrange)
    }
    
    # subset dat to doy range and local_sites that were operating during date in question
    temp_df <- subset(dat, doy %in% doyrange & local_site %in% unique(dat$local_site[dat$date == d]))
    

    # iterate through infill hierarchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in site_order){
      
      # if site not in temp_df, next
      if(!site %in% unique(temp_df$local_site)){
        next
      }
      
      # widen data
      wide_df <- subset(temp_df, local_site %in% c(target_site, site), select = c(date:measurement))
      wide_df <- tidyr::unite(wide_df, local_site, local_site, metric)
      wide_df <- tidyr::spread(wide_df, local_site, measurement)
      wide_df$allthere <- apply(wide_df[grep(stringr::str_flatten(metric, collapse = "|"), names(wide_df))], 1, function(x)all(!is.na(x)))
      
      
      if(sum(wide_df$allthere)<nobs_limit){
        next
      }
      
      # check that explanatory site is present
      if(!any(grepl(site, names(wide_df)))){
        next
      }
      
      # # check that there is overlap for min nobs specified on non-NA dates for both sites
      # target_dates <- unique(wide_df$date[!is.na(wide_df[[target_site]])])
      # site_dates <- unique(wide_df$date[!is.na(wide_df[[site]])])
      # if(sum(target_dates %in% site_dates)<nobs_limit){
      #   next
      # }
      # 
      
      # check that there is a nonNA value for reference site on the missing date
      #stopifnot(length(wide_df[[site]][wide_df$date == d]) == 2) # should be 2 for DTR and airtemp avg
      if(any(is.na(wide_df[wide_df$date == d, grep(site, names(wide_df))]))){
        next
      }
      
      
      # run lm model and store results
      mod <- lm(formula = paste0(target_site,"_airtemp_avg ~ ", site, "_airtemp_avg"), data = subset(wide_df, allthere))
      r2_df <- rbind(r2_df, 
                     data.frame(cbind(site = site,
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
                                  metric = NA,
                                  source.station = NA,
                                  pval = NA,
                                  r2 = NA,
                                  n.obs = NA,
                                  equation = NA,
                                  method = "multi-yr")
      # append model infill to master infill df
      dat_out <- rbind(dat_out, tempinfill_df)
      
      # move on to next date
      next
    }
    
    # select best model
    best <- subset(r2_df, pval <= 0.05)
    best <- subset(best, r2 == max(r2, na.rm = T))
    
    # if nothing has signif pval, select best r2
    if(nrow(best) == 0){
      best <- subset(r2_df, r2 == max(r2, na.rm = T))
    }
    
    # if best is tied [multiple rows], choose first site (should correspond to highest ranking site)
    if(nrow(best) > 1){
      best <- best[1,]
    }
    
    # infill missing values based on best model
    # widen data
    wide_df <- subset(temp_df, local_site %in% c(target_site, best$site), select = c(date:measurement))
    wide_df <- tidyr::unite(wide_df, local_site, local_site, metric)
    wide_df <- tidyr::spread(wide_df, local_site, measurement)
    wide_df$allthere <- apply(wide_df[grep(stringr::str_flatten(metric, collapse = "|"), names(wide_df))], 1, function(x)all(!is.na(x)))
    
    
    # iterate through mean T and DTR, run lm, predict missing values, and store results in data frame
    for(m in metric){
    best_mod <- lm(formula = paste0(target_site, "_", m, " ~ ", best$site, "_", m), data = subset(wide_df, allthere))
    
    #  predict value and store
    tempinfill <- predict(best_mod, newdata = subset(wide_df, date == d))
    
    tempinfill_df <- data.frame(date = d, infill = tempinfill,
                                metric = m,
                                source.station = best$site,
                                pval = summary(best_mod)$coefficients[8],
                                r2 = summary(best_mod)$r.squared,
                                n.obs = nrow(best_mod$model),
                                equation = paste0("y = ", best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                method = "multi-yr")
    
    # append model infill to master infill df
    dat_out <- rbind(dat_out, tempinfill_df)
    
    }
    
  }
  # return infilled df
  return(dat_out)
}






# selection functions
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
select_model <- function(dat_historic, dat_season){
  
  alltog <- rbind(dat_historic, dat_season)
  alldates <- unique(sort(alltog$date))
  
  seasonNAs <-with(dat_season, unique(date[is.na(infill)]))
  historicNAs <-with(dat_historic, unique(date[is.na(infill)]))
  
  # id dates where date not NA in season fill but IS NA in historic fill
  choose_season <- alldates[(!alldates %in% seasonNAs) & (alldates %in% historicNAs)]
  # the opposite: date has infill from historic, but not by seasonal method
  choose_historic <-  alldates[(!alldates %in% historicNAs) & (alldates %in% seasonNAs)]
  
  chosen_dat <- rbind(dat_season[dat_season$date %in% choose_season,],
                  dat_historic[dat_historic$date %in% choose_historic,])
  
  remain_dates <- alldates[!alldates %in% chosen_dat$date]
  # subset regression output for each method for remaining dates in question
  alltog <- subset(alltog, date %in% remain_dates)
  
  # if either method failed for regression (e.g., insufficient observation, by default choose the other method)
  
  # take overall means of pval and r2 by infill event
  dat_means <- dplyr::group_by(alltog, date, method) %>%
    dplyr::mutate(mean_pval = mean(pval),
              mean_r2 = mean(r2),
           flag_mean_pval = ifelse(mean_pval < 0.05, FALSE, TRUE),
           pval_sub10 = ifelse(mean_pval < 0.1, TRUE, FALSE),
           flag_nobs = n.obs < 14) %>%
    ungroup() %>%
    distinct(date, method, n.obs, mean_pval, mean_r2, flag_mean_pval, pval_sub10, flag_nobs) %>%
    arrange(date, method) %>%
    group_by(date) %>%
    mutate(maxr2 = max(mean_r2) == mean_r2,
      maxr2_diff = max(mean_r2) - mean_r2) %>%
    ungroup() %>%
    mutate(selectedmod = ifelse(!flag_mean_pval & !flag_nobs & maxr2, TRUE, FALSE))
  
  # pull out dates that have a chosen mod
  pull_season <- subset(dat_season, date %in% with(dat_means, date[selectedmod & grepl("moving", method)]))
  pull_historic <- subset(dat_historic, date %in% with(dat_means, date[selectedmod & grepl("multi", method)]))
  
  chosen_dat <- distinct(rbind(chosen_dat, pull_season, pull_historic))
  chosen_dat <- arrange(chosen_dat, date)
  
  # remove pulled dates from dat_means
  dat_means <- subset(dat_means, !date %in% chosen_dat$date) %>%
    group_by(date) %>%
    # if anything is under 0.05 and has the max r2 but doesn't meet nobs limit, still go with that model over the other
    mutate(selectedmod = ifelse(!flag_mean_pval & pval_sub10 & maxr2, TRUE, FALSE))
  
  # repeat process
  pull_season <- subset(dat_season, date %in% with(dat_means, date[selectedmod & grepl("moving", method)]))
  pull_historic <- subset(dat_historic, date %in% with(dat_means, date[selectedmod & grepl("multi", method)]))
  
  chosen_dat <- distinct(rbind(chosen_dat, pull_season, pull_historic))
  chosen_dat <- arrange(chosen_dat, date)
  
  # remove pulled dates from dat_means
  dat_means <- subset(dat_means, !date %in% chosen_dat$date) %>%
    group_by(date) %>%
    # if anything is under 0.05 and meetings nobs count and difference from max r2 is not too great (subjective -- screened and most of these are less than 0.01 difference)
    mutate(selectedmod = ifelse(!flag_mean_pval & !flag_nobs & maxr2_diff < 0.2, TRUE, FALSE))
  
  # repeat process
  pull_season <- subset(dat_season, date %in% with(dat_means, date[selectedmod & grepl("moving", method)]))
  pull_historic <- subset(dat_historic, date %in% with(dat_means, date[selectedmod & grepl("multi", method)]))
  
  chosen_dat <- distinct(rbind(chosen_dat, pull_season, pull_historic))
  chosen_dat <- arrange(chosen_dat, date)
  
  # remove pulled dates from dat_means
  dat_means <- subset(dat_means, !date %in% chosen_dat$date) %>%
    # choose whatever has the highest r2 for a given date
    group_by(date) %>%
    mutate(selectedmod = maxr2)
  
  # repeat process
  pull_season <- subset(dat_season, date %in% with(dat_means, date[selectedmod & grepl("moving", method)]))
  pull_historic <- subset(dat_historic, date %in% with(dat_means, date[selectedmod & grepl("multi", method)]))
  
  chosen_dat <- distinct(rbind(chosen_dat, pull_season, pull_historic))
  chosen_dat <- arrange(chosen_dat, date)
  
  # at this point, all dates should have a selection. warn if not:
  if(length(alldates[!alldates %in% chosen_dat$date]) == 0){
    print("All missing dates have best model selected. Huzzah!")
  }else{
    print("Following dates still need model selection:")
    print(alldates[!alldates %in% chosen_dat$date])
  }
  
  return(chosen_dat)
}


# function to calculate, compare, and choose final tmax, tmin, tmean and DTR values
# TK rules are:
# 1) if tmax or tmin present, calculate the other from predicted DTR (don't use predicted tmean)
# 2) if both tmax and tmin missing, use tmean +- (DTR/2) to calculate tmax and tmin
# CTW additions:
# 3) for loggers: if tmean and another extreme present, compare prediction-derived missing value to value calculated from measured variables for the day
# 4) devise some rule for keep raw value over predicted..

calculate_minmax <- function(chosen_dat, target_dat, target_site){
  
  # subset target site from target_dat
  dat_select <- subset(target_dat, local_site == target_site, select = c(date:measurement, local_site))
  add_back <- c(names(target_dat)[!names(target_dat) %in% names(dat_select)], "local_site")
  
  # spread measured data
  dat_select <- tidyr::spread(dat_select, metric, measurement)
  # calculate DTR
  dat_select$dtr <- with(dat_select, airtemp_max - airtemp_min)
  
  # separate predicted DTR from predicted tmean to process each, then merge with measured variables
  pred_dtr <- subset(chosen_dat, metric == "DTR", select = -metric)
  names(pred_dtr)[grep("infil|pval|r2|equ|n.ob", names(pred_dtr))] <- paste("dtr", names(pred_dtr)[grep("infi|pval|r2|equ|obs", names(pred_dtr))], sep = "_")
  
  pred_tmean <- subset(chosen_dat, metric == "airtemp_avg", select = -metric)
  names(pred_tmean)[grep("infil|pval|r2|equ|obs", names(pred_tmean))] <- paste("airtemp_avg", names(pred_tmean)[grep("infil|pval|r2|equ|obs", names(pred_tmean))], sep = "_")
  # merge
  pred_dat <- merge(pred_tmean, pred_dtr)
  # calculate tmax and tmin to have it ready
  pred_dat$tmax_infill <- with(pred_dat, airtemp_avg_infill + (dtr_infill/2))
  pred_dat$tmin_infill <- with(pred_dat, airtemp_avg_infill - (dtr_infill/2))
  
  # attach predicted values to dat_select to start comparing and selecting final
  dat_select <- merge(dat_select, pred_dat[grep("date|infill", names(pred_dat))], all = T)
  # crunch measured dat-adjusted predictions
  # measured tmax or tmin with measured tmean present
  dat_select$adj_tmax_measured <- with(dat_select, ifelse(is.na(airtemp_max) & !is.na(airtemp_min) & !is.na(airtemp_avg), 
                                                    airtemp_min + ((airtemp_avg - airtemp_min) * 2), NA))
  dat_select$adj_tmin_measured <- with(dat_select, ifelse(is.na(airtemp_min) & !is.na(airtemp_max)  & !is.na(airtemp_avg), 
                                                    airtemp_max - ((airtemp_max - airtemp_avg) * 2), NA))
  # prediction adjusted by measured tmin or tmax present (e.g., measured + predicted DTR)
  dat_select$adj_tmax_infill <- with(dat_select, ifelse(is.na(airtemp_max) & !is.na(airtemp_min), airtemp_min + (dtr_infill), NA))
  dat_select$adj_tmin_infill <- with(dat_select, ifelse(is.na(airtemp_min) & !is.na(airtemp_max), airtemp_max - (dtr_infill), NA))
  
  # calculate tmean predicted adjusted vals
  dat_select$meanadj_tmax_infill <- with(dat_select, ifelse(is.na(airtemp_max) & !is.na(airtemp_min), airtemp_min + abs((airtemp_avg_infill- airtemp_min)*2), NA))
  dat_select$meanadj_tmin_infill <- with(dat_select, ifelse(is.na(airtemp_min) & !is.na(airtemp_max), airtemp_max - abs((airtemp_max-airtemp_avg_infill)*2), NA))
  
  # add flags for diurnal adj tmax < observed tmean and diurnal adj tmin > observed mean
  dat_select$flag_airtemp_avg <- with(dat_select, (adj_tmax_infill < airtemp_avg) | (adj_tmin_infill > airtemp_avg))
  
  # select final temps
  # first rename the measures temps with raw_[name]
  names(dat_select)[grep("^airtemp_[a-z]{3}$", names(dat_select))] <- paste0("raw_", names(dat_select)[grep("^airtemp_[a-z]{3}$", names(dat_select))])
  
  dat_select <- dplyr::mutate(dat_select,
                           airtemp_max = ifelse(flag_airtemp_avg, tmax_infill, NA), # use prediction-derived tmax if raw airtemp_avg flagged
                           airtemp_max = ifelse(is.na(raw_airtemp_avg) & !is.na(raw_airtemp_max) & !is.na(raw_airtemp_min), tmax_infill, airtemp_max), # for case where tmin and tmax recorded but mean not
                           airtemp_max = ifelse(is.na(airtemp_max) & !is.na(adj_tmax_infill), adj_tmax_infill, airtemp_max), # otherwise, if dtr adjusted present, use that
                           airtemp_max = ifelse(is.na(airtemp_max) & !is.na(raw_airtemp_max), raw_airtemp_max, airtemp_max),  # otherwise if raw value present, use that
                           airtemp_max = ifelse(is.na(airtemp_max) & !is.na(tmax_infill), tmax_infill, airtemp_max), # otherwise, use prediction-derived tmax
                           # add selection method to make life easier for flagging
                           airtemp_max_method = ifelse(!is.na(adj_tmax_infill) & airtemp_max == adj_tmax_infill, "tmin observed dtr adjusted",
                                                   ifelse(!is.na(raw_airtemp_max) & airtemp_max == raw_airtemp_max, "raw", "predicted")),
                           # apply same treatment to tmin
                           airtemp_min = ifelse(flag_airtemp_avg, tmin_infill, NA), # use predicted tmin if raw airtemp_avg flagged
                           airtemp_min = ifelse(is.na(raw_airtemp_avg) & !is.na(raw_airtemp_max) & !is.na(raw_airtemp_min), tmin_infill, airtemp_min), # for case where tmin and tmax recorded but mean not
                           airtemp_min = ifelse(is.na(airtemp_min) & !is.na(adj_tmin_infill), adj_tmin_infill, airtemp_min), # otherwise, if dtr adjusted present, use that
                           airtemp_min = ifelse(is.na(airtemp_min) & !is.na(raw_airtemp_min), raw_airtemp_min, airtemp_min), # otherwise if raw value present, use that
                           airtemp_min = ifelse(is.na(airtemp_min) & !is.na(tmin_infill), tmin_infill, airtemp_min), # otherwise, use prediction-derived tmin
                           # add selection method to make life easier for flagging
                           airtemp_min_method = ifelse(!is.na(adj_tmin_infill) & airtemp_min == adj_tmin_infill, "tmin observed dtr adjusted",
                                                       ifelse(!is.na(raw_airtemp_min) & airtemp_min == raw_airtemp_min, "raw", "predicted")),
                           # apply same treatment to tmean
                           airtemp_avg = ifelse(flag_airtemp_avg, airtemp_avg_infill, NA), # airtemp avg flagged, use predicted
                           airtemp_avg = ifelse(is.na(airtemp_avg) & !is.na(raw_airtemp_avg), raw_airtemp_avg, airtemp_avg), # otherwise, if raw has value present, use raw
                           airtemp_avg = ifelse(is.na(airtemp_avg) & is.na(raw_airtemp_avg), airtemp_avg_infill, airtemp_avg), # otherwise use predicted (raw is missing)
                           # add selection method to make life easier for flagging
                           airtemp_avg_method = ifelse(!is.na(raw_airtemp_avg) & airtemp_avg == raw_airtemp_avg, "raw", "predicted"),
                           # post selection checks
                           flagmax = airtemp_max <= airtemp_min | airtemp_avg >= airtemp_max,
                           flagmin = airtemp_avg <= airtemp_min)
  
  # if flagmax or flagmin, default to typical infill method (predicted tmean +- DTR*0.5)
  # pull dates, iterate through and replace with predicted tmax, tmin and tmean
  flagdates <- dat_select$date[dat_select$flagmax | dat_select$flagmin]
  dat_select$airtemp_max[dat_select$date %in% flagdates] <- dat_select$tmax_infill[dat_select$date %in% flagdates]
  dat_select$airtemp_min[dat_select$date %in% flagdates] <- dat_select$tmin_infill[dat_select$date %in% flagdates]
  dat_select[dat_select$date %in% flagdates, c("airtemp_max_method", "airtemp_min_method")] <- "predicted (raw extreme dtr adjusted failed)"
  
  # select columns to keep and rejoin regression equations and site id info
  keepcols <- names(dat_select)[grepl("date|yr|mon|doy|logger|local|^raw_air|^airtemp|dtr_in|flag", names(dat_select))]
  dat_out <- subset(dat_select, select = keepcols)
  # join id cols from target_dat
  dat_out <- merge(dat_out, pred_dat[grep("date|^airte|metho|source|dtr", names(pred_dat))], all.x = T)
  
  # start ordering column names
  idcols <- names(dat_out)[grep("dat|yr|mon|doy|local|log", names(dat_out))]
  dtrcols <- names(dat_out)[grep("dtr", names(dat_out))]
  rawcols <- names(dat_out)[grep("^raw", names(dat_out))]
  tempcols <- names(dat_out)[grep("^airtemp", names(dat_out))]
  flagcols <- names(dat_out)[grep("flag", names(dat_out))]
  # reorder
  dat_out <- dat_out[c(idcols, tempcols, "method", "source.station", dtrcols, flagcols, rawcols)] 
  # return selected so user can review
  return(dat_out)
                          
}

# iterate by infill event and compare/select best model according to TK criteria
select_model_old <- function(dat_compare){
  
  # take overall means of pval and r2 by infill event
  dat_means <- group_by(dat_compare, date, method) %>%
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



### OLD METHODS #######

# tk_temp_movingfill_old <- function(dat, target_site, missing_dates, site_order, window_days=13, metric = c("airtemp_avg", "DTR"), windowcap = NA, singlefill = F, nobs_limit = 14){
#   # initiate empty data frame for storing predictions and regression info
#   infill_df_m1 <- data.frame()
#   # initiate counter at position 1
#   i <- 1
#   # initiate infill event counter at 1
#   e <- 1
#   while(i <=length(missing_dates)){
#     
#     # print dates being infilled to keep track of progress
#     print(paste("Predicting missing values for", missing_dates[i]))
#     
#     # specify time window
#     # find first date not NA looking backwards and count days specified back from that
#     # subset df to all dates before missing date  
#     
#     firstnotNA <- with(dat, max(date[local_site == target_site & date < missing_dates[i] & !is.na(measurement)]))
#     lastnotNA <- with(dat, min(date[local_site == target_site & date > missing_dates[i] & !is.na(measurement)]))
#     # if last is NA, then assign max date for target
#     if(is.na(as.character(lastnotNA))){
#       lastnotNA <- with(dat, unique(max(date[local_site == target_site])))
#     }
#     #specify dates this applies to
#     time_window <- missing_dates[(missing_dates > firstnotNA & missing_dates <= lastnotNA)]
#     
#     if(singlefill){
#       time_window <- missing_dates[i]
#     }
#     
#     start_date <- firstnotNA - window_days # subtract 13 bc including the first and last not NA dates, so 14 days total
#     end_date <- lastnotNA + window_days
#     
#     # if it is one-date-at-a-time, try month cap backwards and forwards to see if can replicate tk's infilling for d1 1992 with saddle
#     if(singlefill & !is.na(windowcap)){
#       
#       start_date <- max(start_date, time_window - windowcap)
#       end_date <- min(end_date, time_window + windowcap)
#     }
#     
#     # subset dat
#     temp_df <- subset(dat, date >= start_date & date <= end_date)
#     
#     # set nobs limit
#     temp_nobs <- nobs_limit
#     
#     # iterate through infill hiearchy and pick best r2 with pval < 0.05
#     r2_df <- data.frame()
#     for(site in site_order){
#       #print(site)
#       # widen data
#       wide_df <- subset(temp_df, local_site %in% c(target_site, site), select = c(date:measurement))
#       wide_df <- tidyr::spread(wide_df, local_site, measurement)
#       
#       # refine start and end by station availability
#       
#       # check there are observations > 0 for explanatory site
#       if(all(is.na(wide_df[[site]]))){
#         next
#       }
#       
#       # if single-day fill is on, check that there is are predictor values for both metrics on the date needing infilling
#       if(singlefill){
#         
#         # check that values present for both metrics
#         if(any(is.na(wide_df[[site]][wide_df$date == missing_dates[i]]))){
#           next
#         }
#       }
#       
#       # check that target and reference site have days that overlap (and count of overlap > nobs_limit)
#       target_dates <- unique(wide_df$date[!is.na(wide_df[[target_site]])])
#       site_dates <- unique(wide_df$date[!is.na(wide_df[[site]])])
#       
#       #if the target_dates are under nobs limit, set nobs limit at length of target_dates
#       if(length(target_dates) < nobs_limit){
#         temp_nobs <- length(target_dates)
#       }
#       if(sum(target_dates %in% site_dates) < temp_nobs){
#         next
#       }
#       
#       mod <- lm(formula = paste(target_site, site, sep = "~"), data = subset(wide_df, metric == "airtemp_avg"))
#       
#       r2_df <- rbind(r2_df, 
#                      data.frame(cbind(site = site,
#                                       nobs = nrow(mod$model),
#                                       r2 = summary(mod)$r.squared, # TK used r2 not adjusted r2
#                                       pval = summary(mod)$coefficients[8])))
#       
#     }
#     
#     # make numeric cols numeric
#     r2_df$nobs <- as.numeric(r2_df$nobs)
#     r2_df$r2 <- as.numeric(r2_df$r2)
#     r2_df$pval <- as.numeric(r2_df$pval)
#     
#     # remove any row that has an r2 of 1 (unrealistic) and filter out any model nobs less than limit specified (TK has a min of 14)
#     if(nrow(r2_df)>0){
#       r2_df <- subset(r2_df, r2 != 1 & nobs >= temp_nobs)
#     }
#     # select best
#     best <- subset(r2_df, pval <= 0.05 & r2 == max(r2, na.rm = T))
#     # if nothing has signif pval, select best r2
#     if(nrow(best) == 0){
#       best <- subset(r2_df, r2 == max(r2, na.rm = T))
#     }
#     
#     # re-subset data based on best model
#     # widen data
#     wide_df <- subset(temp_df, local_site %in% c(target_site, best$site), select = c(date:measurement))
#     wide_df <- tidyr::unite(wide_df, local_site, local_site, metric)
#     wide_df <- tidyr::spread(wide_df, local_site, measurement)
#     
#     # infill missing values in time_window based on best model
#     for(m in metric){
#       best_mod <- lm(formula = paste0(target_site, "_", m, " ~ ", best$site, "_", m), data = wide_df)
#       tempinfill <- predict(best_mod, newdata = wide_df[wide_df$date %in% time_window,])
#       tempinfill_df <- cbind(date = as.character(time_window), infill = tempinfill) %>%
#         as.data.frame %>%
#         mutate(metric = m,
#                source.station = best$site,
#                pval = summary(best_mod)$coefficients[8],
#                r2 = summary(best_mod)$r.squared,
#                n.obs = nrow(best_mod$model),
#                equation = paste0("y = ",  best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
#                infillrun = length(time_window),
#                infillevent = e,
#                method = paste0((window_days+1),"-d moving window")) %>%
#         # convert date to Date class
#         mutate(date = as.Date(date, format = "%Y-%m-%d"))
#       
#       # append model infill to master infill df
#       infill_df_m1 <- rbind(infill_df_m1, tempinfill_df)
#     }
#     
#     # indicate how many days infilled to update progress
#     print(paste(length(time_window), "days predicted"))
#     
#     # once infilled, update i (date after last date infilled) and increment infill event
#     last_infilled <- which(missing_dates == max(time_window))
#     i <- last_infilled +1
#     e <- e+1
#     # clean up things that shouldn't be recycled
#     rm(r2_df, tempinfill_df, best, time_window, start_date, end_date)
#   }
#   # return infilled df when finished
#   return(infill_df_m1)
# }
# 



# old nwt infill methods: 14-day moving window regression a la nwt original infill regression method 1
nwt_temp_movingfill <- function(dataset, metric, target_site, ref_site, nmin = 10, days = 14, r2 = 0.6, p = 0.05){
  
  require(dplyr)
  
  #initialize df for storing infilled temp values
  infill_df <- data.frame()
  
  for(m in metric){
    dat <- as.data.frame(subset(dataset, met == m))
    # id missing dates
    missing_dates <- sort(dat["date"][is.na(dat[ytemp])])
    missing_dates <- as.Date(missing_dates)
    
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


# old nwt infill method: function to infill by monthly regression
nwt_temp_monthfill <- function(dat, metric, ytemp, xtemp, mod){
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

