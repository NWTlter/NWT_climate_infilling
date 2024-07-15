#functions to infill precipitation following tim kittel's methods


# precipitation infill functions

# regression function on natural log ppt, returns best model of all sites considered
# > inputs are dat (data subsetted to date range of interest, target, reference, and nobs limit)
best_ln_regression <- function(dat, target_site, site_order, nobs_limit){
  
  # initiate data frame for storing lm results
  r2_df <- data.frame()
  
  # iterate through infill hierarchy and pick best r2 with pval < 0.05
  for(site in site_order){
    
    # widen data
    wide_df <- subset(dat, local_site %in% c(target_site, site))
    wide_df <- tidyr::spread(wide_df, local_site, measurement)
    
    # check there are observations > 0 for reference site
    if(all(is.na(wide_df[[site]])) | !site %in% names(wide_df)){
      next
    }
    # check that there is a value for reference site (unlogged value, 0s okay) on the missing date
    if(is.na(wide_df[[site]][wide_df$date == d])){
      next
    }
    
    # create natural logged vars if both sites present with ref site value present on target's missing date
    wide_df[paste0(target_site, "_ln")] <- ifelse(wide_df[[target_site]]>0, log(wide_df[[target_site]]), NA)
    wide_df[paste0(site, "_ln")] <- ifelse(wide_df[[site]]>0, log(wide_df[[site]]), NA)
    
    # check that there are days where both target site and source station have logged values so can run lm model
    if(all(is.na(wide_df[paste0(target_site, "_ln")][!is.na(wide_df[paste0(site, "_ln")])]))){
      next
    }
    
    # run lm model and store results
    mod <- lm(formula = paste0(target_site,"_ln ~ ", site, "_ln"), data = wide_df)
    r2_df <- rbind(r2_df, 
                   data.frame(cbind(date = d,
                                    target = target_site,
                                    site = site,
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
  
  return(r2_df)
}


# shorter-term "seasonal" infill: -31 and +30 days before and after target infill date
tk_ppt_seasonalfill <- function(dat, target_site, missing_dates, site_order, qdayscheck = F, nobs_limit = 14){
  
  # initiate data frame for storing results
  infill_df_season <- data.frame()
  
  # iterate through each date with missing data
  for(d in as.character(missing_dates)){
    # print out to console to keep track of progress
    print(paste("Predicting",d))
    
    d <- as.Date(d, format = "%Y-%m-%d")
    
    # subset dat to time window of interest, all sites available
    temp_df <- subset(dat, date > (d-31) & date < (d+30))
    
    # initiate data frame for storing lm results
    r2_df <- data.frame()
    
    # iterate through infill hierarchy and pick best r2 with pval < 0.05
    for(site in site_order){
      
      # print(paste("process", site))
      if("qdays" %in% names(temp_df)){
        # check for a positive value during qdays range if that is triggered
        temp_qdays <- with(subset(temp_df, local_site == target_site & date == d), qdays)
        temp_amt <- with(subset(temp_df, local_site == target_site & date == d), raw)
        # specify date seq to backfill
        event_dates <- seq.Date(d-(temp_qdays-1), d, 1)
        check_qdays <- subset(temp_df, local_site %in% c(target_site, site) & date %in% event_dates)
        if(!site %in% check_qdays$local_site){
          next
        }
        if(qdayscheck & temp_amt > 0){
          ref_amt <- with(subset(check_qdays, local_site == site), sum(measurement, na.rm =T))
          if(ref_amt == 0){
            print(paste("eject", site))
            next
          }
        }
      }
      
      # print(paste("widen", site))
      
      # widen data
      wide_df <- subset(temp_df, local_site %in% c(target_site, site), select = c(date:measurement))
      wide_df <- tidyr::spread(wide_df, local_site, measurement)
      
      # check there are observations > 0 for reference site
      if(all(is.na(wide_df[[site]])) | !site %in% names(wide_df)){
        next
      }
      # check that there is a value for reference site (unlogged value, 0s okay) on the missing date
      if(is.na(wide_df[[site]][wide_df$date == d])){
        next
      }
      
      # create natural logged vars if both sites present with ref site value present on target's missing date
      wide_df[paste0(target_site, "_ln")] <- ifelse(wide_df[[target_site]]>0, log(wide_df[[target_site]]), NA)
      wide_df[paste0(site, "_ln")] <- ifelse(wide_df[[site]]>0, log(wide_df[[site]]), NA)
      
      # check that there are days where both target site and source station have logged values so can run lm model
      if(all(is.na(wide_df[paste0(target_site, "_ln")][!is.na(wide_df[paste0(site, "_ln")])]))){
        next
      }
      
      # run lm model and store results
      mod <- lm(formula = paste0(target_site,"_ln ~ ", site, "_ln"), data = wide_df)
      r2_df <- rbind(r2_df, 
                     data.frame(cbind(site = site,
                                      #logger = logger, 
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
                                  #logger = NA, 
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
    if(nrow(best) > 1 & !all(is.na(best$pval))){
      best <- subset(best, r2 == max(r2, na.rm = T) & pval == max(pval, na.rm = T)) 
    }
    #if best is still more than 1 row at this point (non 0 r2 and pval both tied), choose based on site order
    # would apply to tied r2 and either all pvals are NA or pvals are tied
    if(nrow(best) > 1 & (all(is.na(best$pval)) | length(unique(best$pval)) == 1)){
      # choose closest station by order hierarchy
      temp_order <- sapply(best$site, function(x) grep(x, site_order))
      best_site <- names(which.min(temp_order))
      best <- subset(r2_df, site == best_site)
    }
    
    # infill missing values based on best model
    ## re-subset temp_df based on best model
    temp_df <- subset(temp_df, local_site %in% c(target_site, best$site), select = c(date:measurement))
    
    # widen data
    temp_df <- tidyr::spread(temp_df, local_site, measurement)
    # create natural logged vars if both sites present with ref site value present on target's missing date
    temp_df[paste0(target_site, "_ln")] <- ifelse(temp_df[[target_site]]>0, log(temp_df[[target_site]]), NA)
    temp_df[paste0(best$site, "_ln")] <- ifelse(temp_df[[best$site]]>0, log(temp_df[[best$site]]), NA)
    
    # predict date missing value and store results in data frame
    best_mod <- lm(formula = paste0(target_site, "_ln", " ~ ", best$site, "_ln"), data = temp_df)
    
    # if source station had 0 rainfall on missing date, record -Inf for target (exponentiates to 0), else predict value
    if(temp_df[[best$site]][temp_df$date == d] == 0){
      tempinfill <- -Inf
    }else{
      tempinfill <- predict(best_mod, newdata = subset(temp_df, date == d))
    }

    tempinfill_df <- data.frame(date = d, infill = tempinfill,
                                source.station = best$site,
                                pval = summary(best_mod)$coefficients[8],
                                r2 = summary(best_mod)$r.squared,
                                n.obs = nrow(best_mod$model),
                                equation = paste0("y = ", best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                infillrun = length(d),
                                method = "seasonal")
    
    # append model infill to master infill df
    infill_df_season <- rbind(infill_df_season, tempinfill_df)
    
    # clean up things that shouldn't be recycled
    rm(r2_df, tempinfill_df, best)
    
  }
  # return infilled df
  return(infill_df_season)
}


# build multi-year regression, +- 3 days on each side of target infill date
tk_ppt_historicfill <- function(dat, target_site, missing_dates, site_order, qdayscheck = F, nobs_limit = 14){
  
  # initiate data frame for storing results
  infill_df_historic <- data.frame()
  
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
    
    # subset dat
    temp_df <- subset(dat, doy %in% doyrange)
    
    # iterate through infill hierarchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in site_order){
      
      # if site not in temp_df, next
      if(!site %in% unique(temp_df$local_site)){
        next
      }
      
      # print(paste("process", site))
      if("qdays" %in% names(temp_df)){
        # check for a positive value during qdays range if that is triggered
        temp_qdays <- with(subset(temp_df, local_site == target_site & date == d), qdays)
        temp_amt <- with(subset(temp_df, local_site == target_site & date == d), raw)
        # specify date seq to backfill
        event_dates <- seq.Date(d-(temp_qdays-1), d, 1)
        check_qdays <- subset(temp_df, local_site %in% c(target_site, site) & date %in% event_dates)
        if(!site %in% check_qdays$local_site){
          next
        }
        if(qdayscheck & temp_amt > 0){
          ref_amt <- with(subset(check_qdays, local_site == site), sum(measurement, na.rm =T))
          if(ref_amt == 0){
            print(paste("eject", site))
            next
          }
        }
        }
      
      # print(paste("widen", site))
      # widen data
      wide_df <- subset(temp_df, local_site %in% c(target_site, site), select = c(date:measurement))
      wide_df <- tidyr::spread(wide_df, local_site, measurement)
    
      # check there are observations > 0 for explanatory site
      if(all(is.na(wide_df[site]))){
        next
      }
      # check that there is a value for reference site (unlogged val, 0s okay) on the missing date
      stopifnot(length(wide_df[[site]][wide_df$date == d]) == 1)
      if(is.na(wide_df[[site]][wide_df$date == d])){
        next
      }
      
      # create natural logged vars if both sites present with ref site value present on target's missing date
      wide_df[paste0(target_site, "_ln")] <- ifelse(wide_df[[target_site]]>0, log(wide_df[[target_site]]), NA)
      wide_df[paste0(site, "_ln")] <- ifelse(wide_df[[site]]>0, log(wide_df[[site]]), NA)
      
      # check that there are days where both target site and source station have value so can run lm model
      if(all(is.na(wide_df[[paste0(target_site, "_ln")]][!is.na(wide_df[site])]))){
        next
      }
      # run lm model and store results
      mod <- lm(formula = paste0(target_site,"_ln ~ ", site, "_ln"), data = wide_df)
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
                                  source.station = NA,
                                  pval = NA,
                                  r2 = NA,
                                  n.obs = NA,
                                  equation = NA,
                                  infillrun = NA,
                                  method = "multi-yr")
      # append model infill to master infill df
      infill_df_historic <- rbind(infill_df_historic, tempinfill_df)
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
    temp_df <- subset(temp_df, local_site %in% c(target_site, best$site), select = c(date:measurement))
    
    # widen data
    temp_df <- tidyr::spread(temp_df, local_site, measurement)
    # create natural logged vars if both sites present with ref site value present on target's missing date
    temp_df[paste0(target_site, "_ln")] <- ifelse(temp_df[[target_site]]>0, log(temp_df[[target_site]]), NA)
    temp_df[paste0(best$site, "_ln")] <- ifelse(temp_df[[best$site]]>0, log(temp_df[[best$site]]), NA)
    
    # run lm on natural logged positive precip, predict missing values, and store results in data frame
    best_mod <- lm(formula = paste0(target_site, "_ln", " ~ ", best$site, "_ln"), data = temp_df)
    
    # if source station had 0 rainfall on missing date, record -Inf for target (exponentiates to 0), else predict value
    if(temp_df[[best$site]][temp_df$date == d] == 0){
      tempinfill <- -Inf
    }else{
      tempinfill <- predict(best_mod, newdata = subset(temp_df, date == d))
    }
    
    tempinfill_df <- data.frame(date = d, infill = tempinfill,
                                source.station = best$site,
                                pval = summary(best_mod)$coefficients[8],
                                r2 = summary(best_mod)$r.squared,
                                n.obs = nrow(best_mod$model),
                                equation = paste0("y = ", best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                infillrun = length(d),
                                method = "multi-yr")
    
    # append model infill to master infill df
    infill_df_historic <- rbind(infill_df_historic, tempinfill_df)
    
    # clean up things that shouldn't be recycled
    rm(r2_df, tempinfill_df, best)
    
  }
  # return infilled df
  return(infill_df_historic)
}


# choose best model of seasonal vs. historic fill
choose_best <- function(missing_dates, seasonfill, historicfill){
  
  # initiate data frame for storing best of two possible infill values for given missing date
  chosen <- data.frame()
  # iterate through missing dates and choose "best" model
  for(d in as.character(missing_dates)){
    d <- as.Date(d, format = "%Y-%m-%d")
    temp_df <- rbind(seasonfill[seasonfill$date == d,],
                     historicfill[historicfill$date == d,])
    # if no models available, choose first row and change method to NA (all vals except date will be NA)
    if(all(is.na(temp_df$r2))){
      best <- temp_df[1,]
      best$method <- NA
      chosen <- rbind(chosen, best)
      next
    }
    best <- subset(temp_df, r2 == max(r2, na.rm = T) & pval <= 0.05)
    if(nrow(best) ==0){
      # choose highest r2 if no pval under 0.05
      best <- subset(temp_df, r2 == max(r2, na.rm = T) & !is.na(pval))
    }
    chosen <- rbind(chosen, best)
  }
  return(chosen)
}


# function to backfill days with accumulated ppt
backfill_ppt <- function(dat){
  # ID qdays > 1 date
  qdays_dates <- subset(dat, qdays > 1) %>%
    dplyr::select(date, ppt_tot, qdays) %>% na.omit()
  
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
    if(grepl("season", dat$method[dat$date == f])){
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
## needs to be run AFTER backfill days (therefore function tucked inside backfill ppt at the end)
infill_singles <- function(dat){
  single_dates <- dat$date[is.na(dat$Flag.2) & !is.na(dat$exp.ppt)]
  dat$backfill[dat$date %in% single_dates] <- dat$exp.ppt[dat$date %in% single_dates]
  dat$Flag.2[dat$date %in% single_dates] <- "A" # not adjustd for any period total (accumlated ppt)
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

# expand each model by the number of backfill event days and corresponding predicted values
expand_backfill <- function(dat = alldats_qdays, bestmodels = sdl_chosen_qdays, target_site = "sdl"){
  
  # initiate df for storing predicted values for all backfill events based on chosen models with non-0 accumulated ppt
  dat_out <- data.frame()
  # iterate through backfill events to infill
  for(d in as.character(bestmodels$date)){
    print(paste("Predicting all backfill event dates for", d))
    d <- as.Date(d)
    
    # determine event dates
    temp_qdays <- with(subset(dat, local_site == target_site & date == d), qdays)
    # specify date seq to backfill
    event_dates <- seq.Date(d-(temp_qdays-1), d, 1)
    # specify formula from best model
    temp_formula = with(bestmodels, equation[date == d])
    # if the formula is empty that means no model was suitable and accumulated amount should be divided by number of event
    if(is.na(temp_formula)){
      temp_df <- subset(dat, date %in% event_dates & local_site == target_site)
      temp_df$infill <- with(temp_df, raw[!is.na(raw)]/nrow(temp_df))
      temp_out <- cbind(temp_df[c("date", "infill")], subset(bestmodels, date == d, select = c(source.station:method)), row.names = NULL)
      temp_out$method <- "H"
      temp_out$infillrun <- nrow(temp_out)
      # rbind to master and move on to next date
      dat_out <- rbind(dat_out, temp_out)
      next
    }
    
    temp_formula = gsub("y = ", "", temp_formula, fixed = T)
    temp_source = with(bestmodels, source.station[date == d])
    temp_df <- subset(dat, date %in% event_dates & local_site %in% c(target_site, temp_source), select = c(date:measurement))
    
    # widen data
    temp_df <- tidyr::spread(temp_df, local_site, measurement)
    # create natural logged vars if both sites present with ref site value present on target's missing date
    temp_df$x <- ifelse(temp_df[[temp_source]]>0, log(temp_df[[temp_source]]), NA)
    
    # (until I figure out better way to do this) pull out intercept and slope
    intercept <- as.numeric(gsub("x .*$", "", temp_formula))
    slope <- as.numeric(gsub("^[0-9]+.[0-9]+x [+] ", "", temp_formula))
    temp_df$infill <- ifelse(is.na(temp_df$x), -Inf, (slope * temp_df$x) + intercept)
    temp_df$infillrun <- nrow(temp_df)
    
    # reformat dat_out to match how chosen_out is
    temp_out <- cbind(temp_df[c("date", "infill")], subset(bestmodels, date == d, select = c(source.station:method)), row.names = NULL)
    # update infill run to the number of rows predicted
    temp_out$infillrun <- nrow(temp_out)
    temp_out$method <- paste0(temp_out$method, ", I")
    
    # bind to master
    dat_out <- rbind(dat_out, temp_out)
    
  }
  # these flags will be whatever Flag.1 should be as normal, and Flag 2 will be "I" (entire period filled from same source station that has non-zero ppt)
  # EXCEPT when no model was good, and then period infilled by dividing accumulated total by # days in backfill period ("H")
  return(dat_out)
}



