# functions to prepare input climate datasets

# set pathway to static input data files
# e.g., if user needs to download a climate dataset and it cannot be read in dynamically
set_path <- function(){
  
}

na_vals <- c(" ", "", ".", NA, NaN, "NA", "NaN", -9999)

#function to convert degrees F to C
F2C <- function(xF){
  xC <- (xF - 32) * (5/9)
  xC <- round(xC, 2)
  return(xC)
}

# function to convert inches to mm
in2mm <- function(xI){
  xmm <- (xI * (1/25.4))
  xmm <- round(xmm, 2) 
  return(xmm)
}

# function to convert feet to meters (e.g., for elevation)
ft2m <- function(xFt){
  xM <- (xFt * 0.3048)
  xM <- round(xM, 2)
  return(xM)
}


# get snotel datasets


tidySnotel <- function(dat, hasFlags = T){
  # all dats from different stations have same structure
  
  mets <- names(dat)[grepl("Precip|Temp", names(dat))]
  dat_out <- tidyr::gather(dat, metric, value, dplyr::all_of(mets))
  # if QA or QC flags present, separate from measurements
  if(hasFlags){
   dat_out$type <- with(dat_out, ifelse(grepl("Flag", metric), stringr::str_extract(metric, "Q.[.]Flag"), "measurement"))
   # clean up metric
   dat_out$metric <- with(dat_out, ifelse(grepl("Snow.adj", metric), stringr::str_extract(metric, "P.*adj"),
                                                 stringr::str_extract(metric,"^Air.*mum|^Air.*age|^Pre.*ment")))
   dat_out <- tidyr::spread(dat_out, type, value)
  }
  # convert measurement to numeric
  dat_out$measurement <- as.numeric(dat_out$measurement)
  
  # clean up colnames
  names(dat_out) <- casefold(names(dat_out))
  names(dat_out)[grepl("elev", names(dat_out))] <- "elevation"
  names(dat_out) <- gsub("[.]", "_", names(dat_out))
  
  # clean up extra periods in metric
  dat_out$metric <- gsub("...", ".", dat_out$metric, fixed = T)
  
  
  return(dat_out)
}


# function to read, stack, and prep all ghcnd datasets
# > all .csvs downloaded should live in same folder
getGHCND <- function(datpath){
  
  datfiles <- list.files(datpath, full.names = T, pattern = "[.]csv$")
  
  # create master df for row-binding all datasets
  master <- data.frame()
  for(i in datfiles){
    dat <- read.csv(i, na.strings = c(" ", "", NA, "NA"), blank.lines.skip = T, stringsAsFactors = F)
    # convert date to date class -- in standard form so should be able to detect
    dat$DATE <- as.Date(dat$DATE)
    # bind_rows efficiently rbinds datasets with mismatched colnames, preserving all colnames 
    # if ever bind_rows bonks, can use setdiff() on names
    master <- dplyr::bind_rows(master, dat)
  }
 return(master) 
}

tidyGHCND <- function(dat, mets = c("TMAX", "TMIN", "PRCP"), hasAttributes = T){
  
  # collapse metric strings to regex search string
  mets <- stringr::str_flatten(mets, collapse = "|")
  
  # pull in observation data and flags (tidy) so metric in one col, value in next, and flags (attributes) are in next
  metcols <- names(dat)[grepl(mets, names(dat), ignore.case = T)]
  # gather measurements and their attributes (if present)
  dat_out <- tidyr::gather(dat, key = METRIC, value = MEASUREMENT, c(all_of(metcols)))
  # if attributes present, split out
  if(hasAttributes){
    dat_out$type <- ifelse(grepl("ATTR", dat_out$METRIC), "zATTRIBUTE", "MEASUREMENT")
    # clean up metric
    dat_out$METRIC <- gsub("_AT.*$", "", dat_out$METRIC)
    # spread attributes and measurements
    dat_out <- tidyr::spread(dat_out, type, MEASUREMENT)
    # split attribute
    dat_out <- tidyr::separate(dat_out, zATTRIBUTE, into = c("M", "Q", "S", "time_observed"), sep = ",", )
    # if any attribute blank, assign NA
    dat_out[c("M", "Q", "S", "time_observed")] <- lapply(dat_out[c("M", "Q", "S", "time_observed")], function(x) ifelse(x=="",NA, x))
  }
  # class measurement as numeric 
  dat_out$MEASUREMENT <- as.numeric(dat_out$MEASUREMENT)
  # lowcase names
  names(dat_out) <- casefold(names(dat_out))

  return(dat_out)
}



  



# prepare CSU climate center data
prepCSU <- function(dat){
  tempdat <- dat
  tempdat$station <- names(dat)[1]
  names(tempdat)[1] <- "date"
  tempdat <- mutate(tempdat, yr = year(date),mon = month(date), doy = yday(date), stationID = NA) %>%
    rename(TMAX = maxt, TMIN = mint, PRCP = pcpn) %>%
    select(station, stationID, date, yr, mon, doy, TMAX, TMIN, PRCP) %>%
    mutate(station = gsub("[.]", " ", station)) %>%
    # remove alpha chars from temp and ppt cols, convert from char to numeric
    mutate_at(vars("TMAX", "TMIN", "PRCP"), .funs = function(x) gsub("T", "0", x)) %>%
    mutate_at(vars("TMAX", "TMIN", "PRCP"), .funs = function(x) as.numeric(gsub("A|S|M", "", x))) %>%
    # convert american to metric
    # F to C
    mutate_at(vars("TMAX", "TMIN"),~F2C(.)) %>%
    # in to mm
    mutate(PRCP = round(PRCP*25.4, 2))
  
  # retrieve station ID from metadata
  ID <- metadat_df$unlist.metadat.[grepl(unique(tempdat$station), metadat_df$station) & grepl("Station ID", metadat_df$unlist.metadat.)]
  ID <- gsub("Station ID: ", "", ID)
  tempdat$stationID  <- ID
  return(tempdat)
}

# == FOR AMERIFLUX =====

# function to read in Ameriflux data for sites at/near NWT (static file read-in)
# > for dynamic read in, use amerifluxr package (CTW slightly prefers more control with static file read in to amerifluxr package)

getAmeriflux <- function(fluxpath){
  
  # id unzipped folders that have ameriflux data
  fluxfolders <- list.dirs(fluxpath, full.names = T)
  # grab AMF folders only
  fluxfolders <- fluxfolders[grep("AMF", fluxfolders)]
  # grab .csv files within each folder
  fluxfiles <- lapply(fluxfolders, function(x) list.files(x, pattern = "csv$", full.names = T))
  fluxfiles <- unlist(fluxfiles)
  # use station ID as name
  names(fluxfiles) <- stringr::str_extract(fluxfiles, pattern = "US-NR[0-9]")
  
  # read in data to list
  datlist <- lapply(fluxfiles, function(x) read.csv(x, skip =2, colClasses = "character", na.strings = na_vals))
  
  # append site info to data (metadata spreadsheet should be in same folder, if not ignore)
  fluxmeta <- lapply(fluxfolders, function(x) list.files(x, pattern = "[.]xl", full.names = T))
  names(fluxmeta) <- names(fluxfiles)
  # drop anything that doesn't have a value
  #fluxmeta <- fluxmeta[sapply(fluxmeta, nchar)>0]
  
  # read in metadata via readxl and trim to site info and location metadata
  metalist <- lapply(fluxmeta, function(x) readxl::read_excel(x, trim_ws = T))
  metalist <- lapply(metalist, function(x) subset(x, grepl("elev$|lat$|long$|site_name", VARIABLE, ignore.case = T), select = c(SITE_ID, VARIABLE, DATAVALUE)))  
  metalist <- lapply(metalist, function(x) tidyr::spread(x, VARIABLE, DATAVALUE))
  
  # column-bind  metadata to datlist data frames
  for(i in names(datlist)){
    datlist[[i]] <- cbind(data.frame(metalist[[i]]), data.frame(datlist[[i]]))
  }
  
  # return Ameriflux data with site metadata as leading columns
  return(datlist)
}



prepAmeriflux <- function(dat, mets = c("TA", "P")){
  
  # prep search string for columns to keep
  keepcols <- paste0("^", mets, "($|_)")
  keepcols <- stringr::str_flatten(keepcols, collapse = "|")
  sitetimecols <-  paste0("^", c("SITE", "LOCATION", "TIMESTAMP"), "_")
  sitetimecols <- stringr::str_flatten(sitetimecols, collapse = "|")
  allcols <- paste(sitetimecols, keepcols, sep = "|")
  
  # subset Ameriflux to timestamp columns, variables, and variable flags of interest
  tempdat <- subset(dat, select = grepl(allcols, names(dat)))
  
  # separate date from time
  tempdat$date_start <- substr(tempdat$TIMESTAMP_START, start = 1, stop = 8) 
  tempdat$date_end <- substr(tempdat$TIMESTAMP_END, start = 1, stop = 8)
  tempdat$time_start <- substr(tempdat$TIMESTAMP_START, start = 9, stop = 12)
  tempdat$time_end <- substr(tempdat$TIMESTAMP_END, start = 9, stop = 12)
  # clean up time formatting
  tempdat[grep("^TIME", names(tempdat))] <- lapply(tempdat[grep("^TIME", names(tempdat))], function(x) as.POSIXct(x, format = "%Y%m%d%H%M%OS", tz = "UTC"))
  tempdat[grep("^date", names(tempdat))] <- lapply(tempdat[grep("^date", names(tempdat))], function(x) as.Date(x, format = "%Y%m%d"))
  
  # rearrange cols
  tempdat <- subset(tempdat, select = c(grep("SITE", names(tempdat)), 
                                         grep("LOC", names(tempdat)),  
                                         grep("TIME", names(tempdat)),
                                         date_start:time_end,
                                         grep(keepcols, names(tempdat))))
  # keep site-time-loc cols where they are, order variables columns by variable-flagging
  tempdat <- tempdat[c(names(tempdat)[grepl("site|loc|date|time", names(tempdat), ignore.case = T)], 
                        # order variables and their flag cols alphabetically
                       sort(names(tempdat)[grepl(keepcols, names(tempdat))]))]
  
  # subset dataset to first date-time where at least one variable measured is not NA
  narows <- apply(tempdat[grep(keepcols, names(tempdat))],1, function(x) all(is.na(x))) # are all measured/flagged obs NA? (T/F)
  # first element that is false will be starting row that has firstnon-NA value, take that to end of data record
  tempdat <- tempdat[names(narows[!narows][1]):nrow(tempdat),]
  
  # make met observation columns, e.g,. ppt, temp, numeric (corresponding flag columns stay as character)
  tempdat[grep(keepcols, names(tempdat))] <- data.frame(apply(tempdat[grep(keepcols, names(tempdat))], 2, as.numeric))

  return(tempdat)
}


# function to tidy ameriflux data

tidyAmeriflux <- function(datlist, checkdate = T, ...){
  dat_out <- stackData(datlist, ...)
  
  # standardize names to match others
  names(dat_out) <- casefold(names(dat_out))
  names(dat_out) <- gsub("site", "station", names(dat_out))
  names(dat_out)[grepl("elev", names(dat_out))] <- "elevation"
  names(dat_out)[grepl("lat", names(dat_out))] <- "latitude"
  names(dat_out)[grepl("lon", names(dat_out))] <- "longitude"
  
  # separate infilled from raw
  dat_out$type <- with(dat_out, ifelse(grepl("_PI_F", metric), "qc", "raw"))
  dat_out$metric <- gsub("_PI_F", "", dat_out$metric)
  dat_out$rep <- stringr::str_extract(dat_out$metric, "_.*$")
  dat_out$metric <- gsub("_.*", "", dat_out$metric)
  
  require(magrittr)
  # tidy data
  dat_out <- dat_out %>%
      tidyr::gather(met2, measurement, max:tot) %>%
      tidyr::unite(metric, metric, met2, sep = "_") %>%
      tidyr::gather(datum, value, nobs, measurement) %>%
      tidyr::unite(type, type, datum, sep = "_") %>%
      tidyr::spread(type, value)
    
    # drop any precip metric that is not daily total precip, or temp metric that is the daily sum
    dat_out <- subset(dat_out, !grepl("P_m|P_a|TA_tot", metric))
    # make metric like nwt naming convention
    dat_out$metric <- gsub("P_", "ppt_", dat_out$metric)
    dat_out$metric <- gsub("TA_", "airtemp_", dat_out$metric)
    
    # note which values were adjusted
    dat_out$raw_adjusted <- with(dat_out, (raw_measurement != qc_measurement) | (is.na(raw_measurement) & !is.na(qc_measurement)))
    dat_out$raw_adjusted[is.na(dat_out$raw_adjusted)] <- FALSE
    
  if(checkdate){
  # give it yr, mon, doy
  dat_out <- check_datetime(dat_out, datecol = "date_start", groupvar = c("station_id"))
  }
  
  return(dat_out)
}



# == GENERIC FUNCTIONS =====
# function unlist datasets and stack them to write out
stackData <- function(datlist, data_source = NA){
  dat_out <- data.frame()
  for(i in 1:length(datlist)){
    dat_out <- dplyr::bind_rows(dat_out, datlist[[i]])
  }
  if(!is.na(data_source)){
    dat_out <- cbind(data_source, dat_out)
  }
  return(dat_out)
}


# function to convert sub-daily Ameriflux data (or other datasets) to daily
# need to provide which col to defer to for 24hr period
sub2daily <- function(dat, intervalcol = "date_start", mets = c("TA", "P"), idcols = c("SITE", "LOC"), missingAllowed = 2){
  
  require(magrittr)
  
  # prep search string for metric columns to collapse
  mets <- paste0("^", mets, "($|_)")
  mets <- stringr::str_flatten(mets, collapse = "|")
  
  # gather mets in one column for counting missing
  dat_out <- tidyr::gather(dat, metric, measurement, names(dat)[grepl(mets, names(dat))])
  dat_out <- dplyr::grouped_df(dat_out, vars = c(intervalcol, "metric")) %>% # I think pipe is required for group_by to apply to later commands?
    mutate(nobs = sum(!is.na(measurement)))
  # note whether there are sufficient observations to calculate daily value
  dat_out$pass <- with(dat_out, nobs >= max(nobs) - missingAllowed)
  # calculate dailies for temp and precip
  dat_out <- dat_out %>%
    mutate(max = ifelse(pass, max(measurement, na.rm = T), NA),
           min = ifelse(pass, min(measurement, na.rm = T), NA),
           avg = ifelse(pass, mean(measurement, na.rm = T), NA), 
           tot = ifelse(pass, sum(measurement, na.rm = T), NA)) %>%
    ungroup()
  
  # prep search string for id columns to keep
  idcols <- stringr::str_flatten(idcols, collapse = "|")
  
  # distill df to daily obs
  keepcols <- names(dat_out)[grepl(idcols, names(dat_out), ignore.case = T)]
  keepcols <- c(keepcols, intervalcol, "metric", "nobs", "max", "min", "avg", "tot")
  dat_out <- distinct(dat_out[keepcols])
  dat_out <- data.frame(dat_out) # no tibbles
  
  return(dat_out)
  
  #dat_out$intervals <- aggregate(dat_out, by = list(dat_out[[intervalcol]], dat_out$metric), FUN = function(x) sum(!is.na(x)))
  
}

# function to format NWT chart ppt for stacking
tidyNWTchart <- function(datlist){
    
}



# function to tidy (long-form) NWT chart temp (this could be made generic for ppt too..)
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
    arrange(met, date) %>%
    #add month and year
    mutate(yr = year(date),
           mon = month(date),
           doy = yday(date)) %>%
    dplyr::select(c(1:date, yr:doy, met:ncol(.)))
  
  
  # if special cols exist, pull out special cols and rejoin wide-form
  if(!is.na(special)){
    tempspecial <- dat_long %>%
      filter(grepl(special, met)) %>%
      mutate(met = gsub(paste0(special,"_"), "", met))
    # rename temp col as special val
    colnames(tempspecial)[which(colnames(tempspecial) == "temp")] <- special
    # make sure special col is character
    tempspecial[[special]] <- as.character(tempspecial[[special]])
    
    # drop special vals from long-form dat and join wide to temp vals
    dat_long <- subset(dat_long, !grepl(special, met)) %>%
      left_join(tempspecial) %>%
      dplyr::select(1:date, yr:doy, met:ncol(.)) 
  }
  
  # make sure temp is numeric (can be coerced to character when gathered with flags)
  dat_long$temp <- as.numeric(dat_long$temp)
  
  # check if hmps present (if this is for NWT dataset.. assuming yes)
  hmps <- any(grepl("hmp", dat_long$met, ignore.case = T))
  # if present, update logger, add column for reps and clean up metric
  if(hmps){
    # grab last column name before adding sensor and reps for reorganizing
    lastcol <- names(dat_long)[ncol(dat_long)]
    # pull sensor name from met value
    dat_long$sensor <- stringr::str_extract(dat_long$met, "_hmp[:digit:]")
    # clean up met
    dat_long$met <- gsub("_hmp[1-9]", "", dat_long$met)
    #pull rep from sensor col
    dat_long$rep <- gsub("_hmp", "", dat_long$sensor)
    # clean up sensor
    dat_long$sensor <- stringr::str_extract(dat_long$sensor, "_hmp")
    dat_long$logger <- with(dat_long, ifelse(!is.na(sensor), paste0(logger, sensor), logger))
    # drop sensor col
    dat_long <- subset(dat_long, select = -sensor)
    # reorg cols
    dat_long <- subset(dat_long, select = c(1:met, rep, temp:get(lastcol)))
    # check there are reps, if all empty drop that too
    if(all(is.na(dat_long$rep))){
      dat_long <- subset(dat_long, select = -rep)
      print("No replicates detected; if error, review output")
    }
    
  }
  
  # if desired, prefix temp and special col colname with datasource
  if(!is.na(datasource)){
    colnames(dat_long)[colnames(dat_long) %in% c("temp", special)] <- paste(datasource, colnames(dat_long)[colnames(dat_long) %in% c("temp", special)], sep = sep)
  }
  
  # clean up colnames to match other dats out
  names(dat_long)[names(dat_long) == "temp"] <- "measurement"
  names(dat_long)[names(dat_long) == "met"] <- "metric"
  
  # if loggers present, iterate through tidy dataset to remove any dates not relevant to logger/instrument/sensors
  if("logger" %in% names(dat_long)){
    for(u in unique(dat_long$logger)){
      print(u)
      mindate <- with(subset(dat_long, logger == u), ifelse(all(is.na(measurement)), as.character(max(date)), as.character(min(date[!is.na(measurement)]))))
      maxdate <- with(subset(dat_long, logger == u), ifelse(all(is.na(measurement)), as.character(min(date)), as.character(max(date[!is.na(measurement)]))))
      print(mindate)
      print(maxdate)
      # remove unrelevant dates
      dat_long <- subset(dat_long, (logger == u & date >= as.Date(mindate) & date <= as.Date(maxdate)) | logger != u)
    }
  }
  
  
  # return tidy dataset and clean up environment
  return(dat_long)
  rm(tempspecial, temp_pos)
}

# add data source name to data frame
addSource <- function(datlist){
  for(i in 1:length(datlist)){
    datlist[[i]] <- cbind(datsource = names(datlist)[i], data.frame(datlist[[i]]))
  }
  return(datlist)
}

# for wide-form
pare_temp <- function(dat, tempstring, flagstring = NA, keepcols, datecol = "date", DTR = T, maxstring = "max", minstring = "min", reps = NA){
  tempcols <- names(dat)[grepl(tempstring, names(dat), ignore.case = T)]
  tempdat <- dat[c(keepcols, tempcols)]
  
  if(!is.na(flagstring)){
    flagcols <- names(tempdat[grepl(flagstring, names(tempdat), ignore.case = T)])
    flagdat <- subset(dat, select = c(keepcols, flagcols))
    # if creating diurnal temp, collapse any flags from maxT and minT
    if(DTR){
      if(all(is.na(reps))){
        maxcol <- names(flagdat)[grepl(maxstring, names(flagdat), ignore.case = T)]
        mincol <- names(flagdat)[grepl(minstring, names(flagdat), ignore.case = T)]
        flagdat <- rowwise(flagdat) %>%
          mutate(flag_DTR = ifelse(is.na(get(maxcol)) & is.na(get(mincol)), NA, 
                                   ifelse(is.na(get(maxcol)), paste0(minstring, ": ", get(mincol)), 
                                          ifelse(is.na(get(mincol)), paste0(maxstring, ": ", get(maxcol)),
                                                 ifelse(get(maxcol) == get(mincol), as.character(get(maxcol)),
                                                        paste0(maxstring, ": ", get(maxcol), ", ", minstring, ": ", get(mincol))))))) %>%
          ungroup()
        #prefix whatever flagstring is being used
        names(flagdat)[names(flagdat) == "flag_DTR"] <- paste0(flagstring, "_DTR")
      }else{
        # for multiple sensors, calculate per sensor DTR
        for(r in reps){
          maxcol <- names(flagdat)[grepl(r, names(flagdat), ignore.case = T) & grepl(maxstring, names(flagdat), ignore.case = T)]
          mincol <- names(flagdat)[grepl(r, names(flagdat), ignore.case = T) & grepl(minstring, names(flagdat), ignore.case = T)]
          flagdat <- rowwise(flagdat) %>%
            mutate(flag_DTR = ifelse(is.na(get(maxcol)) & is.na(get(mincol)), NA, 
                                     ifelse(is.na(get(maxcol)), paste0(minstring, ": ", get(mincol)), 
                                            ifelse(is.na(get(mincol)), paste0(maxstring, ": ", get(maxcol)),
                                                   ifelse(get(maxcol) == get(mincol), as.character(get(maxcol)),
                                                          paste0(maxstring, ": ", get(maxcol), ", ", minstring, ": ", get(mincol))))))) %>%
            ungroup()
          #prefix whatever flagstring is being used and append sensor to newly created DTR flag with sensor
          names(flagdat)[names(flagdat) == "flag_DTR"] <- paste0(flagstring, "_DTR_", r)
        }
      }
      flagcols <- names(flagdat)[grep(flagstring, names(flagdat), ignore.case = T)]
    }
    # make long form
    flagdat <- gather(flagdat, metric, flag, c(flagcols))
    # drop flag cols from tempdat
    tempdat <- tempdat[!names(tempdat) %in% flagcols]
  }
  
  if(DTR){
    if(all(is.na(reps))){
      tempdat$DTR <- tempdat[[grep(maxstring, names(tempdat), ignore.case = T)]] - tempdat[[grep(minstring, names(tempdat), ignore.case = T)]]
    }else{
      # for multiple sensors, calculate per sensor DTR
      for(r in reps){
        maxcol <- names(tempdat)[grepl(r, names(tempdat), ignore.case = T) & grepl(maxstring, names(tempdat), ignore.case = T)]
        mincol <- names(tempdat)[grepl(r, names(tempdat), ignore.case = T) & grepl(minstring, names(tempdat), ignore.case = T)]
        tempdat[paste0("DTR_", r)] <- tempdat[maxcol] - tempdat[mincol]
      }
    }
  }
  
  # make temperature data long form (tidy)
  temppos <- min(which(names(tempdat) %in% tempcols))
  templong <- gather(tempdat, metric, temp, c(temppos:ncol(tempdat)))
  
  #re-join flagging
  if(!is.na(flagstring)){
    # clean up metric in flagdat
    #tempcols <- unique(templong$metric)
    # make regex
    metricvals <- str_flatten(unique(templong$metric), collapse = "|")
    flagdat$metric <- str_extract(flagdat$metric, pattern = metricvals)
    templong <- merge(templong, flagdat, all.x = T)
  }
  
  # subset data so only period of measurement included (instrument active lifetime)
  # > this matters for loggers vs. hmps
  templong_startdate <- min(with(templong, unique(get(datecol)[!is.na(temp)])))
  templong_enddate <- max(with(templong, unique(get(datecol)[!is.na(temp)])))
  templong <- subset(templong, get(datecol) >= templong_startdate)
  templong <- subset(templong, get(datecol) <= templong_enddate)
  
  # return dataset
  return(templong)
}


prep_ppt <- function(dat, pptstring, flagcols){
  
}


