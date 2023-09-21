# functions to qc precipitation data

# function to verify all dates within a qdays > 1 period don't have ppt values recorded
check_qdays <- function(dat){
  print(unique(dat$local_site))
  # initiate empty vector for storing dates to check
  check_dates <- NULL
  
  # ID qdays > 1 date
  qdays_dates <- subset(dat, qdays > 1) %>%
    dplyr::select(date, measurement, qdays)
  
  for(i in 1:nrow(qdays_dates)){
    # specify date seq to backfill
    event_dates <- seq.Date(qdays_dates$date[i] - (qdays_dates$qdays[i]-1), qdays_dates$date[i], 1)
    temp_df <- subset(dat, date %in% event_dates)
    
    # verify all ppt_tot values NA in event series (minus the accumulated date)
    ppttally <- sum(temp_df$measurement[temp_df$date != qdays_dates$date[i]], na.rm = T) 
    if(!is.na(ppttally)){
      dates_fail <- as.character(temp_df$date[temp_df$date != qdays_dates$date[i] & !is.na(temp_df$measurement)])
      # add to check_dates
      check_dates <- c(check_dates, dates_fail)
    }
  }
  # note which dates have qdays/inconsistencies for easier review and for tracking
  dat$qdays_qcnote <- NA
  dat$qdays_qcnote[dat$date %in% as.Date(check_dates)] <- "ppt value falls within qdays >1 backfill period"
  
  if(length(check_dates) != 0){
    print("Inconsistencies with qdays found. Returning bad dates.")
    print(check_dates)
  } else{
    print("No inconsistencies with qdays found!")
  }
  return(dat)
}

# NA any nonsensical qdays values and corresponding ppt (e.g. anything other than NA or numeric qdays >= 1)
clean_qdays <- function(dat){
  
  bad_qdays <- unique(c(dat$qdays[dat$qdays < 1 & !is.na(dat$qdays)], dat$qdays[grepl("[a-z][A-Z]", dat$qdays)]))
  if(length(bad_qdays) != 0){
    needs_correct <- which(dat$qdays %in% bad_qdays)
    for(n in needs_correct){
      print(paste("Correcting", dat$qdays[n], "qdays value on", dat$date[n]))
      dat$measurement[n] <- NA
      dat$qdays[n] <- NA
      dat$qdays_qcnote[n] <- ifelse(is.na(dat$qdays_qcnote[n]), "nonsensical qday or ppt value, NA both", 
                                    paste(dat$qdays_qcnote[n], "nonsensical qday or ppt value, NA both", sep = ";"))
    }
    
  } else{
    print("All qdays NA or 1+ days.")
  }
  return(dat)
}

# loop to iterate through sites, assign raw ppt measurement to raw_value column for keeping track
qc_qdays <- function(dat, groupvar){
  
  # initiate master for storing dat out
  dat_out <- data.frame()
  # iterate through each grouping variable
  for(g in unique(dat[[groupvar]])){
    print(paste("processing", g))
    tempdat <- subset(dat, get(groupvar) == g)
    tempdat$raw <- tempdat$measurement
    tempdat$raw_qdays <- tempdat$qdays
    # run check_qdays
    tempdat <- check_qdays(tempdat)
    # run clean_qdays
    tempdat <- clean_qdays(tempdat)
    # bind to master
    dat_out <- rbind(dat_out, tempdat)
  }
  return(dat_out)
}

