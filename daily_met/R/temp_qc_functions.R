# temperature-specific data QC functions


## note from Caitlin ----
# > dropping functions I started to qc hobo temp data for Chris Ray, which are based on qc I've written for climate data (and should be helpful to QC daily met data as well)
# > they are in various states of completion, so adapt code however might be helpful:

# > when developing code, i assign the prepped data to workingdat and test on that (so there is always a clean copy of data and working copy)
# workingdat <- tempdf # this is the stacked temperature data frame you want to qc

# flag if value exceeds max or min specified threshold
flag_rangelimit <- function(dat, targetvar = "TempC", maxval = 40, minval = -50, groupvars = NULL, joinflags = T){
  # group if grouped vars present
  if(!is.null(groupvars)){
    dat <- grouped_df(dat, groupvars)
  }
  
  # flag anything outside limits
  dat$flag_rangelimit <- (dat[[targetvar]] > maxval | dat[[targetvar]] < minval)
  
  # ungroup if needed
  if(!is.null(groupvars)){
    dat <- ungroup(dat)
  }
  
  # return results as part of data frame or as vector
  if(joinflags){
    return(dat)
  }else{
    #just return vector of flags
    return(dat[["flag_rangelimit"]])
  }
}

# test function on workingdat
# workingdat <- flag_rangelimit(workingdat)

# check for anomolous daily [or whatever time resolution] values (>4sd)
flag_daily <- function(dat, targetvar = "TempC", threshold = 4, groupvars = NULL, joinflags = T){
  # group if grouped
  if(!is.null(groupvars)){
    dat <- grouped_df(dat, groupvars)
  }
  # calculate z-score (scale)
  dat$temp_z <- scale(dat[[targetvar]])[,1]
  dat$flag_absolute <- abs(dat$temp_z) > threshold
  dat <- ungroup(dat)
  
  # return results as part of data frame or as vector
  if(joinflags){
    return(dat)
  }else{
    #just return vector of flags
    return(dat[["flag_rangelimit"]])
  }
}

# test function on workingdat
# workingdat <- flag_absolute(workingdat)

# check for rate change (>4 z-score)
flag_ratechange <- function(dat, ordervar = "clean_datetime", targetvar = "TempC", absolute = FALSE, threshold = 4, groupvars = NULL, joinflags = T, idcols = NULL){
  
  # group if grouped
  if(!is.null(groupvars)){
    dat <- grouped_df(dat, groupvars)
  }
  
  # be sure dataset is ordered 
  dat <- dat[order(dat[[ordervar]]),]
  
  # calculate delta value at t and t-1 lag
  dat$ratechange <- dat[[targetvar]] - lag(dat[[targetvar]])
  
  # calculate z-score (scale)
  dat$ratechange_z <- scale(dat[["ratechange"]])[,1]
  # flag any ratechange that exceeds threshold
  if(absolute){
    # flagging based on absolute change
    dat$flag_ratechange <- abs(dat$ratechange) > threshold
  }else{
    # flagging based on scaled change
    dat$flag_ratechange_z <- abs(dat$ratechange_z) > threshold
  }
  
  # ungroup dat before returning
  dat <- ungroup(dat)
  
  # return results as part of data frame or as vector
  if(joinflags){
    return(dat)
  }else{
    #just return vector of flags
    stopifnot("please specify 'idcols' variables to return with flags" = !is.null(idcols))
    return(dat[names(dat) %in% c("ratechange", "ratechange_z", "flag_ratechange", "flag_ratechange_z")])
  }
  
  
}

# test on working dat
# workingdat <- flag_ratechange(workingdat, absolute = T, threshold = 6)
# testflags <- flag_ratechange(workingdat, absolute = F, threshold = 6, joinflags = F)


# check for elevated or depressed values following rate change
# > requirement is that user runs ratechange check first
# > note from ctw: this function is unfinished. i last stopped testing different changepoint functions (from different packages) on datasets
# > what i landed on was flagging a value if multiple changepoint functions flagged a value, but i didn't finish coding that
flag_spikeshift <- function(dat, idcol = "DataFile", ordervar = "clean_datetime", targetvar = "TempC", windowsize = 24, absolute = F, groupvars = NULL){
  
  # group if grouped
  if(!is.null(groupvars)){
    dat <- grouped_df(dat, groupvars)
  }
  
  # be sure dataset is ordered 
  dat <- dat[order(dat[[ordervar]]),]
  
  # grab timestemps that need check for singular stepchange or step-shift (multiday) ratechange
  check_dates <- dat[which(dat$flag_ratechange & dat$flag_ratechange_z), c(idcol, timecol)]
  
  # iterate through each instrument to eval
  for(i in unique(check_dates[[idcol]])){
    # iterate through each date and test for changepoints
    datsub <- dat[dat[idcol] == i,]
    # determine timestep
    timestep <- min(diff(datsub[[ordervar]]))
    for(d in check_dates[[ordervar]][check_dates[idcol] ==i]){
      starttime <- d - (windowsize*timestep)
      stoptime <- d + (windowsize*timestep)
      timeseq <- seq(starttime, stoptime, timestep)
      modeldat <- datsub[dat[[ordervar]] %in% timeseq,] 
      # assign targetvar generic name for modeling
      names(modeldat)[names(modeldat) == targetvar] <- "y"
      names(modeldat)[names(modeldat) == ordervar] <- "t"
      # test for consistent breakpoints using a few methods
      # 1. strucchange breakpoints
      strucmod <- strucchange::breakpoints(y ~ 1, data = modeldat)
      strucmod$breakpoints
      
      # cpm
      cpmmod <- cpm::processStream(modeldat$y, cpmType = "GLR")
      cpmmod$changePoints  
      
      # meancpt
      changemod <- changepoint::cpt.meanvar(modeldat$y, method = "PELT")
      changemod@cpts
      
      # find the common breaks
      common_breaks <- table(c(strucmod$breakpoints, cpmmod$changePoints, changemod@cpts))
      common_breaks <- common_breaks[common_breaks > 1]
      # if there is a timestep reported as a breakpoint more than once, note
      if(length(common_breaks) > 1){
        # pull out timesteps
        names(common_breaks)
      }
      
      
      
    }
    
    
  }
  # check for single day spikes first
  dat$ratechange_lead <- dat[[targetvar]] - lead(dat[[targetvar]])
  dat$ratechange_lead_z <- scale(dat$ratechange_lead)[,1]
  dat$ratechange_rollmax <- zoo::rollapply(dat$ratechange, function(x) max(abs(x), na.rm = T) ,width = windowsize, align = "center", fill = NA)
  
}