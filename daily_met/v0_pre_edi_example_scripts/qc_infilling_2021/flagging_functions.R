# functions for temperature QC/QC flagging
# author(s): CTW

# goals:
## create recyclable script (i.e. for NWT data manager or others to use in future)
## write generic to apply to any NWT dataset (daily or hourly)
## write internal and external comparative checks
## functions should flag values; up to person to inspect with visual qa

# functions should screen/flag the following:
## logic checks (e.g. max < min, physically implausible values)
## > SCE also says to screen for mean T being set to max or min T when the other is missing
## > for chart, mean T is derived, so could just compare math mean to mean T value
## instrument bonks (e.g. flatlines for stuck sensor/pen)
## extremes (max, min), spikes and step changes (temporal consistency)
## both internal consistency (e.g. compared to other met values at same site) and spatial coherency (agreement with nearby stations that correlate highly)

# notes:


# references:
## these works informed QA/QC functions built
# > Kittel, T. 2009.

# 0. SETUP ----
# load needed libraries
library(lubridate)
library(dplyr)
library(tidyr)


# 1. AT SAME STATION -----
# 1a. Potential data errors/climatological consistency checks -----

# plausibility checks (e.g. outside physically plausible value or range detection of instrument)
## allow data user to enter bounds (max, min) and screen for those

flag_limits <- function(dat, maxcol, mincol = NA, maxval = NA, minval = NA){
  # check to be sure max limit > min limit (if both present)
  stopifnot((maxval>minval | is.na(maxval) | is.na(minval)))
  
  # run through each metric and flag vals that exceed limits (on either end)
  # 1. max col (or single col entered)
  flagmax_hi <- which(dat[maxcol] > maxval)
  flagmax_lo <- which(dat[maxcol] < minval)
  # create flag col
  dat[paste0("qaflag_", maxcol)] <- c()
  # add flagging
  dat[flagmax_hi, paste0("qaflag_", maxcol)] <- "above limit"
  dat[flagmax_lo, paste0("qaflag_", maxcol)] <- "below limit"
  
  # 2. mincol
  if(!is.na(mincol)){
    flagmin_hi <- which(dat[mincol] > maxval)
    flagmin_lo <- which(dat[mincol] < minval)
    # create flag col
    dat[paste0("qaflag_", mincol)] <- NA
    # add flagging
    dat[flagmin_hi, paste0("qaflag_", mincol)] <- "above limit"
    dat[flagmin_lo, paste0("qaflag_", mincol)] <- "below limit"
  }
  
  # 3. if both max and min present, logic check to be sure max > min
  if(all(!is.na(list(maxcol, mincol))) & maxcol != mincol){
    flagmaxmin <- which(dat[maxcol] < dat[mincol])
    # add flagging
    dat[flagmaxmin, paste0("qaflag_", maxcol)] <- paste0("; min exceeds max", dat[flagmaxmin, paste0("qaflag_", maxcol)])
    # clean up flag
    dat[flagmaxmin, paste0("qaflag_", maxcol)] <- gsub("NA; ", "", dat[flagmaxmin, paste0("qaflag_", maxcol)])
  }
  
  # print results
  print(paste(maxcol,"QA flags (blank if none):"))
  print(table(dat[[paste0("qaflag_", maxcol)]]))
  if(!is.na(mincol)){
    print(paste(mincol,"QA flags (blank if none):"))
    print(table(factor(dat[[paste0("qaflag_", mincol)]])))
  }
  return(dat)
}

# flag max/min and plausibility limits for long-form data
flag_maxmin <- function(dat, metric = "temp", maxmet = "airtemp_max", minmet = "airtemp_min", groupvars = NA, maxlim = 40, minlim = -40){
  # check to be sure max limit > min limit (if both present)
  stopifnot((maxval>minval | is.na(maxval) | is.na(minval)))
  # create rowid for tracking
  dat$rowid <- rownames(dat)
  # subset to just max and min temp
  
  # global pass at max and min
  dat$qa_maxmin <- NA
  dat$qa_maxmin <- NA
  return(dat)
}
# check for all expected date-times in series
## POSIX and hours data could be added in to this function, but writing just for normal Date classes to start
check_datetime <- function(dat, datecol = "date", increment = 1, dateform = "%Y-%m-%d"){
  if(class(dat[[datecol]]) != "Date"){
    dat[datecol] <- as.Date(dat[[datecol]], format = dateform)
  }
  expected <- seq.Date(min(dat[[datecol]]), max(dat[[datecol]]), increment) # increment by 1 day by default
  if(all(dat[[datecol]] %in% expected) & all(expected %in% dat[[datecol]])){
    print("All expected date-times present!")
  }else{
    expected_missing <- expected[!expected %in% dat[[datecol]]]
    names(expected_missing) <- "expected dates missing"
    unexpected_dates <- dat[[datecol]][!dat[[datecol]] %in% expected]
    names(unexpected_dates) <- "unexpected dates present"
    return(list(expected_missing, unexpected_dates))
  }
}


# relative extreme values (just to flag, up to user to decide to keep or replace)
## based on frequency distribution.. allow users to specify SD thresholds
flag_deviations <- function(dat, datecol = "date", dateform = "%Y-%m-%d", metric, groupvars = c("mon"), ranks_num = 3, sd_num = 4, returnworking = F, idcols = "date"){
  # ensure date column is in Date class
  if(class(dat[[datecol]]) != "Date"){
  dat[[datecol]] <- as.Date(dat[[datecol]], format = dateform)
  }
  # create working df
  working <- dat
  # create month and year cols -- #can make this use regex if don't want to do depend a package, if R makes datecol yyyy-mm-dd would be easy
  working$mon <- lubridate::month(working[[datecol]])
  working$yr <- lubridate::year(working[[datecol]])
  
  # check for values that fall outside SD num, and flag most ranks_num extreme values for review
  working <- working %>%
    # calculate overall rank and std metric
    mutate(global_rank_metric = rank(get(metric), na.last = "keep"),
           std_metric = c(scale(get(metric))),
           check_globalrank_metric = global_rank_metric %in% c(head(sort(unique(global_rank_metric)), n = ranks_num), tail(sort(unique(global_rank_metric)), n = ranks_num)),
           flag_std_metric = abs(std_metric) > sd_num) %>%
    # grouped by grouping vars
    grouped_df(vars= groupvars) %>%
    arrange(mon, get(metric), get(datecol)) %>%
    mutate(grp_rank_metric = rank(get(metric), na.last = "keep"),
           grp_std_metric = c(scale(get(metric))),
           check_grprank_metric = grp_rank_metric %in% c(head(sort(unique(grp_rank_metric)), n = ranks_num), tail(sort(unique(grp_rank_metric)), n = ranks_num)),
           flag_grpstd_metric = abs(grp_std_metric) > sd_num) %>%
    ungroup() %>%
    data.frame()
  
    if(returnworking){
      # rename generic "metric" to actual metric of interest
      names(working) <- gsub("metric", metric, names(working))
      return(working)
    }else{
      # append flagging only to dat
      ## use actual values for flags so column contains flag info AND reason all in one
      ## > keep: check global, flag global, check grp, flag grp 
      working$check_globalrank_metric <- with(working, ifelse(check_globalrank_metric, global_rank_metric, NA))
      working$flag_std_metric <- with(working, ifelse(flag_std_metric, std_metric, NA))
      working$check_grprank_metric <- with(working, ifelse(check_grprank_metric, grp_rank_metric, NA))
      working$flag_grpstd_metric <- with(working, ifelse(flag_grpstd_metric, grp_std_metric, NA))
      # drop rank and std dev cols calculated
      working <- working[names(working)[!grepl("_rank_metric|^grp_std_metric|^std_metric", names(working))]]
      # rename generic "metric" to actual metric of interest
      names(working) <- gsub("metric", metric, names(working))
      # pare down to metric and flagvars of interest
      working <- working[c(idcols, groupvars, names(working)[grepl(metric, names(working))])]
      # then join with dat to return
      dat <- left_join(dat, working)
      return(dat)
    }
}



# 1b. Temporal consistency checks ----
## allow users to specify thresholds

# multiday flatlines
## create flatline events so easier to pull those dates for visual qa or subset df to flat events of interest
flag_flatlines <- function(dat, metric, groupvars = NA, numdays = 4){
  # create working df
  working <- dat
  # if grouping variables present, group dataset first
  if(!any(is.na(groupvars))){
    working <- grouped_df(working, groupvars)
  }
  # create 1 day lagged var
  working$change <- working[[metric]] - lag(working[[metric]],1)
  # calculate runs of 0-temp change
  count0 <- rle(working$change == 0)
  # convert rle result to data frame format
  count0 <- data.frame(cbind(run = count0$lengths, rle0 = count0$values))
  # look for numdays+ consecutive days of 0 change
  count0$flag <- (count0$run >= numdays & count0$rle0 == 1)
  flag0 <- which(count0$flag)
  # add column with flatline count
  dat[paste0(metric, "_flatline")] <- NA
  if(length(flag0) == 0){
    # return dat
    return(dat)
  }else{
    # assign flags
    count0$rowid_start <- NA
    count0$rowid_end <- NA
    # id rows in target dataset that violate flatline limit
    for(i in flag0){
      temp_start <- sum(count0$run[1:i-1])+1
      count0$rowid_start[i] <- temp_start
      count0$rowid_end[i] <- (temp_start + count0$run[i])-1
    }
    # drop anything not flagged
    count0 <- subset(count0, flag)
    # flag target dataset
    for(i in 1:nrow(count0)){
      dat[[paste0(metric, "_flatline")]][count0$rowid_start[i]:count0$rowid_end[i]] <- paste0("event ", i, " (", count0$run[i], ")")
    }
    # return data with flagged flatlines
    return(dat)
  }
}

# day-to-day spikes
## consider relative vs. absolute spikes
## should be able to apply to metric of interest (e.g. temp, SD)
flag_spikes <- function(dat, metric, groupvars = NA, abs_limit = 25, sd_limit = 5, returnworking = FALSE, id = "date"){
  # create working df
  working <- dat
  # if grouping variables present, group dataset first
  if(!any(is.na(groupvars))){
    working <- grouped_df(working, groupvars)
  }
  # create 1 day lagged var
  #working$change <- working[[metric]] - lag(working[[metric]],1)
  #working$change_std <- scale(working$change)
  working <- working %>%
    mutate(change = get(metric)-lag(get(metric), 1),
           change_std = c(scale(change))) %>%
    ungroup()
  # pull obs that exceed limits
  flag_abslim <- which(abs(working$change) >= abs_limit)
  flag_sdlim <- which(abs(working$change_std) >= sd_limit)
  
  # apply flagging
  working[paste0(metric, "_temporal_spike")] <- NA
  # append absolute-based flagging
  for(i in flag_sdlim){
    working[[paste0(metric, "_temporal_spike")]][i] <- paste0("Absolute change from preceeding exceeds ", abs_limit, " (", round(working$change[i],2),")")
  }
  # append sd-based flagging
  for(i in flag_sdlim){
    tempflag <- paste0("; standardized change from preceeding exceeds ", sd_limit, " SD (", round(working$change_std[i], 4),")")
    if(!i %in% flag_abslim | length(flag_abslim) == 0){
      working[[paste0(metric, "_temporal_spike")]][i] <- gsub("; st", "St", tempflag)
    }else{
      # append
      working[[paste0(metric, "_temporal_spike")]][i] <- paste0(working[[paste0(metric, "_temporal_spike")]][i], tempflag)
    }
  }
  
  # append to target dataset
  dat <- cbind(dat, working[paste0(metric, "_temporal_spike")])
  if(returnworking){
    names(working) <- gsub("^change", paste0(metric, "_change"), names(working))
    return(working[,c(which(names(working) %in% groupvars), grep(paste(id, metric, sep = "|"), names(working), ignore.case = T))]) #c(id, metric, "change", "change_std", paste0(metric, "_temporal_spike"))]
  }else{
    return(dat)
  }
  
}



# step changes -- use a breakpoints analysis instead
# flag_rollingstep <- function(dat, rollingdays = 10){
#   
# }

# 1c. Introduced artificial changepoints from station history -----
# function to check for step changes at known instrument change points




# 2. -- SPATIAL CONSISTENCY (in comparison with nearby stations)
## note: Kittel 2009 notes hourly data will have less spatial correlation with nearby stations, better for daily or weekly data



# 3. -- VISUAL QA ----
# function for plotting double mass analysis