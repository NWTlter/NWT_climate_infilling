# qaqc functions common across ppt and temp

# check all expected timestamps present
#yr, mon, doy, time + data source if desired

standardize_time <- function(newdat, dateform){
  # create data frame of all expected timestamps
  tempdates <- sort(unique(newdat$date))
  mindate <- min(tempdates)
  maxdate <- max(tempdates)
  #datestep <- tempdates[2] - tempdates[1]
  # difference tempdates and take the minimum > 0
  datestep <- diff(tempdates)
  #datestep_tbl <- table(datestep)
  dateseq <- seq.Date(mindate, maxdate, by = median(datestep[datestep > 0]))
  
  dateframe <- data.frame(date = dateseq,
                          yr = lubridate::year(dateseq),
                          mon = lubridate::month(dateseq),
                          doy = lubridate::yday(dateseq))
  
  
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
  # print missing datetimes found (if any)
  return(newdat)
}

check_datetime <- function(dat, datecol = "date", dateform = "%Y-%m-%d", idcols = NA, groupvar = NA, datsource = NA,...){
  
  
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
  master <- data.frame()
  # if no individual sites, proceed with using min and max time of global dataset
  if(is.na(groupvar)){
    master <- standardize_time(newdat, dateform)
    # print date range and # missing timestamps found
    print(paste("date range is:")) 
    print(range(newdat$date))
    print(paste(sum(is.na(newdat[[groupvar]])), "missing time intervals corrected"))
    # fill down any static cols
    if(!is.na(idcols[1])){
      newdat[,idcols] <- newdat[1,idcols]  # min (first) date should have info for all static id cols 
    }
  }else{
    # store all of newdat in its own temp object
    newdat_all <- newdat
    # iterate by site if individual sites present
    for(g in unique(newdat_all[[groupvar]])){
      # subset group of interest
      newdat <- newdat_all[newdat_all[groupvar] == g,]
      newdat <- standardize_time(newdat, dateform)
      # print date range and # missing timestamps found
      print(paste("date range for", g, "is:")) 
      print(range(newdat$date))
      print(paste(sum(is.na(newdat[[groupvar]])), "missing time intervals corrected"))
      # fill down any static cols
      if(!is.na(idcols[1])){
        newdat[,idcols] <- newdat[1,idcols]  # min (first) date should have info for all static id cols 
      }
      # append subset to master df
      master <- rbind(master, newdat)
    }
  }
  # if data source indicated, add to beginning
  if(!is.na(datsource)){
    master <- cbind(data_source = datsource, master)
  }
  
  return(data.frame(master))
}


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

# # check for all expected date-times in series
# ## POSIX and hours data could be added in to this function, but writing just for normal Date classes to start
# check_datetime <- function(dat, datecol = "date", increment = 1, dateform = "%Y-%m-%d"){
#   if(class(dat[[datecol]]) != "Date"){
#     dat[datecol] <- as.Date(dat[[datecol]], format = dateform)
#   }
#   expected <- seq.Date(min(dat[[datecol]]), max(dat[[datecol]]), increment) # increment by 1 day by default
#   if(all(dat[[datecol]] %in% expected) & all(expected %in% dat[[datecol]])){
#     print("All expected date-times present!")
#   }else{
#     expected_missing <- expected[!expected %in% dat[[datecol]]]
#     names(expected_missing) <- "expected dates missing"
#     unexpected_dates <- dat[[datecol]][!dat[[datecol]] %in% expected]
#     names(unexpected_dates) <- "unexpected dates present"
#     return(list(expected_missing, unexpected_dates))
#   }
# }
# 
