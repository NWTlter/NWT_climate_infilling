# dataviz functions

require(tidyverse)
require(lubridate)
theme_set(theme_test())

# show available data and missing by time sequence, for as many vars as want
# > could later make it work to show multiple stations at once by plotting list

available_dataviz <- function(dat, timecol, id = NULL, mets, allvars = T, plotNA = T, scales = "free_y", ...){
  
  if(allvars){
    # collapse meteorological columns into searchable string
    keepcols <- paste0("^", mets, "($|_)")
    keepcols <- stringr::str_flatten(keepcols, collapse = "|")
    # update mets
    mets <- names(dat)[grepl(keepcols, names(dat))]
  }
  
  # subset data to plot
  plotdat <- dat[names(dat) %in% c(timecol, mets)]
  # note any column that is completely empty
  excludemets <- names(plotdat)[sapply(plotdat, function(x) all(is.na(x)))]
  # gather all metrics to facet wrap panels
  plotdat <- gather(plotdat, metric, value, all_of(mets))
  # remove mets that are empty
  plotdat <- subset(plotdat, !metric %in% excludemets)
  
  # create df to mark missing data due to date breaks (e.g., gchnd data)
  # determine expected time interval based on most recent data
  timeinterval <- plotdat[[timecol]][nrow(plotdat)] - plotdat[[timecol]][nrow(plotdat)-1]
  # create expected time series based on time class
  if("Date" %in% class(plotdat[[timecol]])){
    missing_times <- seq.Date(from = min(plotdat[[timecol]]), to = max(plotdat[[timecol]]), by = timeinterval)
  }else{
    # assume it's POSIX format if not Date
    missing_times <- seq.POSIXt(from = min(plotdat[[timecol]]), to = max(plotdat[[timecol]]), by = timeinterval)
  }
  # create df of expected times and data values
  expectedtime_df <- data.frame(missing_times)
  expectedtime_df$missing <- !missing_times %in% plotdat[[timecol]]
  expectedtime_df <- cbind(expectedtime_df, 
                           matrix(nrow = length(missing_times), ncol = length(unique(plotdat$metric)),
                                  dimnames = list(NULL, unique(plotdat$metric))))
  expectedtime_df <- gather(expectedtime_df, metric, value, unique(plotdat$metric))
  # drop empty value col then merge data
  #expectedtime_df <- expectedtime_df[c("missing_times", "metric")]
  names(expectedtime_df)[1] <- timecol
  #expectedtime_df <- left_join(expectedtime_df, cbind(plotdat, present = 1), by = c(timecol, "metric"))
  
  # plot
  if(plotNA){
    p <- ggplot(plotdat, aes(get(timecol), is.na(value))) +
    geom_jitter(alpha = 0.5, height = 0.25, width = 0) +
    geom_jitter(data = subset(expectedtime_df, missing), alpha = 0.5,  height = 0.12, width = 0, col = "orchid") +
    labs(x = NULL, y = "Value missing?", subtitle = id) +
    facet_wrap(~metric)
  }
  if(!plotNA){
    p <- ggplot(plotdat, aes(get(timecol), value), ...) +
      geom_line(alpha = 0.8, na.rm = T) +
      labs(x = NULL, subtitle = id) +
      facet_wrap(~metric, scales = scales)
  }
  # print plot
  print(p)
}

plot_all_list <- function(listobject, ...){
  for(i in 1:length(listobject)){
    available_dataviz(listobject[[i]], id = names(listobject)[i], ...)
  }
}

plot_all_groups <- function(dat, groupvars, ...){
  for(i in unique(dat[[groupvars]])){
    print(paste("Processing", i))
    available_dataviz(subset(dat, get(groupvars) == i), id = i, ...)
  }
}


# panel plot user-selected flagged data for review (e.g., qdays violations)
visual_qcreview <- function(reviewdat, alldat, groupvar, ndays = 15){
  # initiate list for storing ggplots
  plot_list <- list()
  
  # sort review data by groupvar then date
  #reviewdat <- dplyr::arrange(reviewdat, eval(groupvar), date)
  
  # iterate through subsetted review data
  for(r in 1:nrow(reviewdat)){
    
    subdat <- subset(alldat, date < reviewdat$date[r] + ndays  & date > reviewdat$date[r] - ndays)
    tempplot <- ggplot(subdat, aes(date, raw,col = get(groupvar)), ) +
      geom_line() +
      geom_label(data = subset(subdat, qdays > 1), aes(label = qdays)) +
      geom_point(data = reviewdat[r,], pch = 1, col = "black", size = 3) +
      # add info to plot
      labs(subtitle = paste(reviewdat[[groupvar]][r], reviewdat$date[r])) +
      theme(legend.title = element_blank())
      
      # store plot in a list
      plot_list[length(plot_list)+1] <- list(tempplot)
      
  }
  return(do.call(gridExtra::grid.arrange, plot_list))
  # print
  #return(plot_list)
  
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

