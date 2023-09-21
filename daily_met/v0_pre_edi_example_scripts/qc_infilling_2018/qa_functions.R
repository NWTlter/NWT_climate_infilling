# qa functions for flagging temp (and precip?)


# function to tidy (long-form) temp (this could be made generic for ppt too..)
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
    # add month and year
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
    
    # drop special vals from long-form dat and join wide to temp vals
    dat_long <- subset(dat_long, !grepl(special, met)) %>%
      left_join(tempspecial) %>%
      dplyr::select(LTER_site:date, yr:doy, met:ncol(.)) 
  }
  
  # make sure temp is numeric (can be coerced to character when gathered with flags)
  dat_long$temp <- as.numeric(dat_long$temp)
  
  # if desired, prefix temp and special col colname with datasource
  if(!is.na(datasource)){
    colnames(dat_long)[colnames(dat_long) %in% c("temp", special)] <- paste(datasource, colnames(dat_long)[colnames(dat_long) %in% c("temp", special)], sep = sep)
  }
  
  # return tidy dataset and clean up environment
  return(dat_long)
  rm(tempspecial, temp_pos)
}


# function to panel plot flagged data
visual_qa <- function(dat, qadat, sorttime = "date", add_fourth = NA){
  # initiate list for storing ggplots
  plot_list <- list()
  # id temperature cols in reference data frame
  tempcols <- colnames(dat)[grepl("temp", colnames(dat))]
  
  for(m in c("airtemp_max", "airtemp_min")){
    tempdf <- qadat[qadat$met == m,] %>% as.data.frame()
    # order by preferred time sort (default is date)
    tempdf <- tempdf[order( tempdf[,which(colnames(tempdf) == sorttime)]),]
    
    for(d in as.character(tempdf$date)){
      d <- as.Date(d, format = "%Y-%m-%d")
      tempplot <- ggplot(data = subset(dat, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1))) +
        geom_line(aes(date, main)) +
        geom_point(aes(date, main)) +
        # circle the flagged value in red
        geom_point(data = subset(dat, met == m & date == as.Date(d)),
                   aes(date, main), col = "red", pch  = 1, size = 3) +
        labs(y = gsub("airtemp_", "T", m),
             x = d) +
        # add sdl chart temp for comparison (purple dots)
        geom_line(aes(date, comp1), col = "purple") +
        geom_point(aes(date, comp1), col = "purple", pch = 1) +
        geom_line(aes(date, comp2), col = "steelblue2") +
        geom_point(aes(date, comp2), col = "steelblue4", pch = 1) +
        geom_line(aes(date, comp3), col = "forestgreen") +
        geom_point(aes(date, comp3), col = "darkgreen", pch = 1) +
        theme_bw()
      
      if(!is.na(add_fourth)){
        colnames(dat)[colnames(dat) == add_fourth] <- "comp4"
        tempplot <- tempplot + 
          geom_line(data = subset(dat, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), 
                    aes(date, comp4), col = "goldenrod1") + 
          geom_point(data = subset(dat, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), 
                     aes(date, comp4), col = "goldenrod1", pch = 1)
      }
      
      # store plot in a list
      plot_list[length(plot_list)+1] <- list(tempplot)
    }
  }
  return(plot_list)
}


# function to difference daily logger temp from comparative chart dataset daily temps
diff_daily <- function(dat, rename = TRUE, main = "sdl_temp", groupvars = c("met", "mon"), comp1= "sdlcr_qatemp", comp2 = "d1_temp", comp3 = "c1_temp"){
  
  if(rename){
  # rename cols in dataset
  colnames(dat)[colnames(dat) == main] <- "main"
  colnames(dat)[colnames(dat) == comp1] <- "comp1"
  colnames(dat)[colnames(dat) == comp2] <- "comp2"
  colnames(dat)[colnames(dat) == comp3] <- "comp3"
  }
  dat <- dat %>%
    mutate(main_diff_1 = abs(main-comp1),
           main_diff_2 = abs(main-comp2),
           main_diff_3 = abs(main-comp3)) %>%
    grouped_df(eval(groupvars)) %>%
    #set threshold for sdl logger deviance at 3sd away from the absolute average difference (by logger, metric, and month)
    mutate(thresh_diff_1 = mean(main_diff_1, na.rm = T) + (3*sd(main_diff_1, na.rm = T)),
           thresh_diff_2 = mean(main_diff_2, na.rm = T) + (3*sd(main_diff_2, na.rm = T)),
           thresh_diff_3 = mean(main_diff_3, na.rm = T) + (3*sd(main_diff_3, na.rm = T)),
           sd_diff_1 = sd(main_diff_1, na.rm = T),
           sd_diff_2 = sd(main_diff_2, na.rm = T),
           sd_diff_3 = sd(main_diff_3, na.rm = T)) %>%
    ungroup() %>%
    # flag logger value if exceeds daily diff threshold for chart comparative datasets
    mutate(flag_diff1 = main_diff_1 > thresh_diff_1,
           deviance_1 = main_diff_1/sd_diff_1,
           flag_diff2 = main_diff_2 > thresh_diff_2,
           deviance_2 = main_diff_2/sd_diff_2,
           flag_diff3 = main_diff_3 > thresh_diff_3,
           deviance_3 = main_diff_3/sd_diff_3) %>%
    data.frame()
}


# function for flagging and removing high values in working dataset
flag_temp <- function(flagdat, error = "comparative deviance"){
  tempdat <- flagdat
  for(row in 1:nrow(tempdat)){
    pos <- with(working_dat, which(met == tempdat$met[row] & date == tempdat$date[row]))
    working_dat$qa_flag[pos] <- error
    working_dat$main[pos] <- NA 
  }
  return(working_dat)
}


# function check max/mins
check_extreme <- function(dat, groupvars = c("met"), metric = max){
  dat <- working_dat %>%
    grouped_df(eval(groupvars)) %>%
    filter(main== eval(metric)(main, na.rm = T)) %>%
    ungroup() %>% data.frame()
  return(dat)
}



# to function calculate lag delta (current day temp - yesterday's temp)
deltamet <- function(dat, days = 1, groupvars = c("met"), metric = lag){
  dat <- dat %>%
    arrange(met, date) %>%
    grouped_df(eval(groupvars)) %>%
    mutate(lag1_main = lag(main, n = days)) %>%
    ungroup() %>%
    mutate(lag1_diffmain = abs(main - lag1_main))
  
  return(dat)
}


# function to identify flatline in sensors
check_flatline <- function(dat, val = "lag1_diffmain", ctdays = 3){
  # calcalute runs of 0-temp change
  count0 <- rle(working_dat[[val]] == 0)
  flatdf <- data.frame(cbind(run = count0$lengths, rle0 = count0$values))
  # look for 4+ consecutive days of 0 change
  flatdf$flag <- (flatdf$run > ctdays & flatdf$rle0 ==1)
  flag0 <- which(flatdf$flag)
  flatdf$rowid <- NA
  # id rows in main dataset that have 4 or more days of 0 change
  for(i in flag0){
    temp_rowid <- sum(flatdf$run[1:i-1])+1
    flatdf$rowid[i] <- temp_rowid
  }
  
  # add column with flatline count
  dat$flatline <- NA
  for(i in sort(flatdf$rowid)){
    dat$flatline[i] <- flatdf$run[flatdf$rowid == i & !is.na(flatdf$rowid)]
  }
  # return subset of data with flatlines
  return(dat)
}


# -- QA DAY-TO-DAY DELTA DEVIANCE -----
# diff current from lag temp in sdl chart, d1 and c1, then compare daily deltas with logger daily deltas
# pull out observations where delta deviates more than 3sd of logger-other source diff on day-to-day fluxes

deltadev <- function(dat, groupvars = c("met")){
  dat <- dat %>%
    # lag by metric (already ordered by date above)
    grouped_df(eval(groupvars)) %>%
    # lag sdl, d1, and c1 chart
    mutate(lag1_comp1 = lag(comp1),
           lag1_comp2 = lag(comp2),
           lag1_comp3 = lag(comp3)) %>%
    ungroup() %>%
    # take difference from current day - prior day temp
    mutate(lag1_diffsdl = abs(comp1 - lag1_comp1),
           lag1_diffd1 = abs(comp2 - lag1_comp2),
           lag1_diffc1 = abs(comp3 - lag1_comp3),
           delta_lag_main1 = lag1_diffmain - lag1_diffsdl,
           delta_lag_main2 = lag1_diffmain - lag1_diffd1,
           delta_lag_main3 = lag1_diffmain - lag1_diffc1) %>%
    #diff the lag differences and assess whether data-source differences outside normal range of difference given the month and logger
    grouped_df(eval(groupvars)) %>%
    mutate(mean_lagdiff_main1 = mean(delta_lag_main1, na.rm = T),
           sd_lagdiff_main1 = sd(delta_lag_main1, na.rm = T),
           thresh_lagdiff_main1 = (mean_lagdiff_main1) + (3 * sd_lagdiff_main1),
           mean_lagdiff_main2 = mean(delta_lag_main2, na.rm = T),
           sd_lagdiff_main2 = sd(delta_lag_main2, na.rm = T),
           thresh_lagdiff_main2 = (mean_lagdiff_main2) + (3 * sd_lagdiff_main2),
           mean_lagdiff_main3 = mean(delta_lag_main3, na.rm = T),
           sd_lagdiff_main3 = sd(delta_lag_main3, na.rm = T),
           thresh_lagdiff_main3 = (mean_lagdiff_main3) + (3 * sd_lagdiff_main3)) %>%
    ungroup() %>%
    mutate(flag_deltadiff1 = delta_lag_main1 > thresh_lagdiff_main1,
           flag_deltadiff2 = delta_lag_main2 > thresh_lagdiff_main2,
           flag_deltadiff3 = delta_lag_main3 > thresh_lagdiff_main3)
  
  return(dat)
}
