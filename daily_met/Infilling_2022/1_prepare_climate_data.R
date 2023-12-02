#' ---
#' title: Prepare temperature data 
#' author: CTW  
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#' 
#' ### Prepare temperature data 
#' 
#' Test report to:
#' 1. Read in NWT and neighbor station temperature datasets
#' 2. Show raw data (removing any infilled or flagged data that we wish to ignore for processing)
#' 3. Show datasets prepared for QC procedure
#' 
#' All code will be displayed to show procedure and work out bugs
#TODO Find this in the script... 
setwd("~/NWTLTER/NWT_climate_infilling/")
# -- SETUP -----
# libraries needed
library(tidyverse)
library(lubridate)

# source scripts (this will change for package)
source("daily_met/R/fetch_data_functions.R")
source("daily_met/R/prep_data_functions.R")
source("daily_met/R/dataviz_functions.R")
source("daily_met/R/common_qaqc_functions.R")

# specify path to data directory (can be wherever, but raw input files should live in data/raw)
datpath <- "~/OneDrive - UCB-O365/nwt_climate/data/"
fluxpath <- "~/OneDrive - UCB-O365/nwt_climate/data/raw/AmeriFlux/"
ghcndpath <- "~/OneDrive - UCB-O365/nwt_climate/data/raw/GHCNd/"

view_plots <- FALSE  # Should the script automatically print all plots?

# create subfolders for data out if they don't exist
for(i in c("prep", "qc", "infill", "homogenize")){
  if(!i %in% list.files(datpath)){
    dir.create(paste0(datpath, i))
  }
  rm(i) # clean up environment
}

# other way to write
sapply(list("qc", "infill", "homogenize"), function(x) 
  if(!x %in% list.files(datpath)){
    dir.create(paste0(datpath, x))
  }
)

# read in dynamic datasets
snotel <- getSnotelNeighbors()
nwtchart <- getNWTcharts()
nwtchart_infilled <- getNWTchartsinfilled()
nwtlogger <- getNWTdailyloggers()
tvan <- getTabular(2) # J Knowles infilled tvan tower data, does not note which values were infilled but data complete for all timesteps, can perhaps determine infilled values using ameriflux tvan data


# read in static/downloaded datasets 
ameriflux <- getAmeriflux(fluxpath)
ghcnd <- getGHCND(ghcndpath)

#' This workflow assumes the data user gave thought to choosing which data stations to use for comparative data QC and infilling
#' For example, stations that have precedent (were used for previously infilling NWT climate data in peer-reviewed publications)
#' Or stations that may have emerged or increased in quality since past infilling efforts
#' This workflow also assumes the data user is reading in as-is data and will need to review each dataset to both get a basic understanding of what is in the data and potential issues in each

#' Every potential data source (e.g., NWT, Snotel, Ameriflex, NOAA GHCNd, etc.) has their own data structure, units, naming conventions, data QAQC (flagging may exist fully, partially, or not at all), and metadata guidance
#' It is up to the data user to read documentation for each data source before starting the workflow for responsible data use and to understand ramifications and possible biases of using different datasets 

#' Because of the number of sites involved and time/memory use constraints, it may make more sense to separate data prep for each source into its own script/markdown
#' We should prep for all stations here for illustration.




# -- SCREEN AND SELECT DATA -----


#' # 1. NWT stations -----
#' #' Screen chart data (target infilling for basic issues, especially for newer data to QC and infill), but ultimately will be appending newly treated data to existing infilled dataset
#' #' Also want to screen electronic logger and other NWT climate data source (e.g., sensor array may become an option with more time and data collected)
#' #' While logger data are available hourly, here we are only interested in daily logger data
#' 
#' # 1.1. NWT climate daily chart -----
#' # if(view_plots){plot_all_list will plot all stations/data frames in list object
#' if(view_plots){plot_all_list(listobject = nwtchart, timecol = "date", mets = c("airtemp", "ppt"))} # default is to plot which timesteps have missing data
#' # > note: temp datasets were infilled for earlier periods by older methods, which is why missing data only occur for most recent decade+
#' 
#' # setting plotNA = F will plot the numeric data
#' if(view_plots){plot_all_list(listobject = nwtchart, timecol = "date", mets = c("airtemp", "ppt"), plotNA = F)}
#' 
#' #' Potential drift in SDL tmin data (noticeable upward trend not present in max -- avg is calculated from min/max)
#' #' Increase in daily ppt overtime for Saddle not present at C1 or D1 also noticeable
#' 
#' 
#' # for illustration, the infilled chart data
#' # > show actual values because no missing data present in infilled dataset
#' if(view_plots){plot_all_list(listobject = nwtchart_infilled, timecol = "date", mets = c("m.*_temp", "precip", "DTR"), plotNA = F)}
#' 

# nothing to do here for prep but to stack all datasets
chartTemp_out <-lapply(nwtchart[1:3], tidytemp)
chartTemp_out <- data.table::rbindlist(chartTemp_out) %>%
  data.frame()
chartTemp_out <- check_datetime(chartTemp_out, groupvar = c("local_site"))

# prep NWT chart ppt -- need to make sure flag cols are character
chartPPT_out <- nwtchart[grepl("ppt", names(nwtchart))]
chartPPT_out <- data.table::rbindlist(chartPPT_out) %>%
  data.frame() %>%
  # reformat to output similar to other dats
  rename(measurement = ppt_tot) %>%
  mutate(metric = "ppt_tot") %>%
  # rearrange cols
  dplyr::select(1:date, metric, measurement:ncol(.))
chartPPT_out <- check_datetime(chartPPT_out, groupvar = c("local_site"))



# # 1.2. NWT climate daily logger -----
# if(view_plots){plot_all_list(listobject = nwtlogger, timecol = "date", mets = c("airtemp", "ppt"))}
# if(view_plots){plot_all_list(listobject = nwtlogger, timecol = "date", mets = c("airtemp", "ppt"), plotNA = F)}

#' Here we see unrealistic values with D1 21x logger era for airtemp min, max and avg, otherwise temps across loggers are generally in the range of plausible but with noticeable spikes
lapply(nwtlogger, names)
loggerTemp_out <- lapply(nwtlogger, function(x) subset.data.frame(x, select = grep("site|logger|date|^airtemp|^flag_airtemp", names(x), ignore.case = T)))
loggerTemp_out <- lapply(loggerTemp_out, function(x) subset.data.frame(x, select = grep("_as_", names(x), invert = TRUE, ignore.case = T))) #TODO MILES DELETE THIS!!
loggerTemp_out <- lapply(loggerTemp_out, tidytemp)
# cr21x logger datasets don't have local_site so append dat name to each dataset so know which data corresponds to what
loggerTemp_out <- addSource(loggerTemp_out)
# for(i in 1:length(loggerTemp_out)){
#   loggerTemp_out[[i]] <- cbind(datsource = names(loggerTemp_out)[i], data.frame(loggerTemp_out[[i]]))
# }

loggerTemp_out <- lapply(loggerTemp_out, check_datetime)
loggerTemp_out <- data.table::rbindlist(loggerTemp_out, fill = T) %>% data.frame()
# infill LTER site and local_site for any where missing
loggerTemp_out$LTER_site <- unique(loggerTemp_out$LTER_site[!is.na(loggerTemp_out$LTER_site)])
loggerTemp_out$local_site <- with(loggerTemp_out, ifelse(is.na(local_site), casefold(str_extract(datsource, "C1|D1|SDL")), local_site))
# drop datsource
loggerTemp_out <- subset(loggerTemp_out, select = -datsource)

c1loggerPPT_out <- nwtlogger[["C123x"]] %>%
  subset(select = grepl("site|logger|date|ppt", names(.))) %>%
  # reformat to output similar to other dats
  rename(measurement = ppt_tot) %>%
  mutate(metric = "ppt_tot") %>%
  # rearrange cols
  dplyr::select(1:date, metric, measurement:ncol(.)) %>%
  check_datetime()





# 1.3 Tvan infilled ----
# only 1 site present, choose all columns present for metrics to plot except date, year and jday
available_dataviz(tvan, timecol = "date", mets = names(tvan)[!grepl("date|ye|jda", names(tvan))], plotNA = F)

# closer look at temp metrics
available_dataviz(tvan, timecol = "date", mets = "^[a-z]+_temp", allvars = T, plotNA = F) # seems reasonable



# 2. Other sources -----
#' Other sources of data include: Ameriflux, Snotel, and GHCNd. As NEON towers acquire more data that may be an option.



# 2.1. Ameriflux -----
#' There are 3 possible Ameriflux sites to draw from: US-NR1 (near C1), US-NR3 (Tvan East), US-NR4 (Tvan west)
#' US-NR1 has the longest record and is the best curated (e.g., QAQC'd and gap-filled), but because Tvan near Saddle worth reviewing

# prep function selects meteorological variables of interest, classes timestamps as POSIX, and pulls out dates and time separately from the timestamps
# because data read in to a list, apply function to each element in list
ameriflux_prepped <- lapply(ameriflux, prepAmeriflux)

#' # visualize data available (missing/present) and data as read in (look for oddities and outliers)
#' # because ameriflux data are sub-daily, plotting can take some time for stations that have fairly complete records for multiple variables
#' if(view_plots){plot_all_list(listobject = ameriflux_prepped, timecol = "TIMESTAMP_START", mets = c("TA", "P"))}
#' 
#' #' For US-NR1 P_PI_F... and TA_PI_F.. will be most reliable. Reading the data documentation you'd guess this anyway, but it's good to visualize to confirm.
#' #' NR3 or NR4 could maybe be helpful for short window temperature infilling. Let's inspect the actual data.
#' 
#' if(view_plots){plot_all_list(listobject = ameriflux_prepped, timecol = "TIMESTAMP_START", mets = c("TA", "P"), plotNA = F)}

#' perhaps keep each of three qc'd and gap-filled sensors for temp at US-NR1 in case.. will probably be able to use sensor 1 fully (1,2,3 are at different vertical heights)
#' or could choose to homogenize the three

#' For sub-hourly data, the next step in prepping data for this workflow's QC is to aggregate sub-hourly to daily data, just for columns and sites of interest

ameriflux_daily <- lapply(ameriflux_prepped, sub2daily)

#' From here, review daily data to see what you'd like to keep for QC and possible use in infilling
#' 
 

# what is the breakdown of data coverage by site by metric?
for(i in 1:length(ameriflux_daily)){
  print(with(ameriflux_daily[[i]], lapply(split(tot, metric), function(x) table(is.na(x))/length(x))))
}
# Tvan sites don't have too much available data
# if(view_plots){
#   plot_all_list(listobject = ameriflux_daily[2:3], timecol = "date_start", mets = c("max", "min", "avg"), plotNA = F)
#   plot_all_groups(ameriflux_daily[[1]], groupvars = "metric", timecol = "date_start", mets = c("max", "min", "avg", "tot"), plotNA = F) # sensor 1 seems the most stable for temp
#   
#   # what is the difference between sensor 1 and 2 for infilled temp?
#   ggplot(subset(ameriflux_daily[[1]], grepl("TA_PI_F_1", metric)), aes(date_start, max)) +
#     geom_hline(aes(yintercept = 0), lty = 2) +
#     geom_line(aes(col = metric), alpha = 0.5) +
#     facet_wrap(~(date(date_start) > as.Date("2010-01-01")) + metric, scales = "free") # sensor 1 tends to be warmer than other 2, but other two have more range 
#   
#   # what is the difference between sensor 1 raw vs. infilled temp?
#   ggplot(subset(ameriflux_daily[[1]], grepl("TA_PI_F_1_2|TA_1_2", metric)), aes(date_start, max)) +
#     geom_point(aes(col = metric), alpha = 0.5)
#     #facet_wrap(~metric, nrow = 3)
#     
#   # what is the difference between sensor 1 raw vs. infilled ppt?
#   ggplot(subset(ameriflux_daily[[1]], grepl("P_", metric)), aes(date_start, tot)) +
#     geom_point(aes(col = metric), alpha = 0.5) +
#     facet_wrap(~metric, nrow = 2)
# # not much choice here.. if decide to ppt, know most of it is infilled
# }
# put all in one dataset then subset to what you want to keep
ameriflux_out <- tidyAmeriflux(ameriflux_daily)

# check how many values were adjusted from raw
lapply(split(ameriflux_out$raw_adjusted, paste(ameriflux_out$station_name, ameriflux_out$metric)), summary)
# NA's for Niwot Ridge Forest are from 2021 not being infilled yet; Tvan sites don't have a QC'd value

# for US-NR1, just keep sensor 1 (infilled and raw); at other stations only temp (TA) is available for metric
ameriflux_out <- subset(ameriflux_out, rep == "_1_1_1" | grepl("3|4", station_id))



# 2.3. Snotel -----
snotel$Date <- as.Date(snotel$Date) 
plot_all_groups(snotel[,!grepl("Flag", names(snotel))], timecol = "Date", mets = c("Pre.*", "Air.*"), 
                groupvars = "Station.Name", plotNA = F)

#' All of these sites seem helpful (keep), variable names need to be standardized
#' Snotel data include the precip increment, and the snow-adjusted precip increment. We want to use snow-adjusted.
#' Snow-adjusted is "daily positive increment in precipitation accum. or SWE, whichever is larger, as observed by SNOTEL instrumentation. This is done to address known scenarios in underatch" (Kirk and Schmidilin 2016, Defining Large Precipitation Events in the Upper Colorado River Basin and Contributions to Lake Powell Inflow)
# 
# # show diff between precip increment and snow adjusted
# if(view_plots){
#   ggplot(snotel, aes(Date, Precipitation.Increment..mm. - Precipitation.Increment...Snow.adj..mm.)) +
#     geom_point() +
#   #geom_abline(aes(slope = 1, intercept = 0), col = "red") +
#     facet_wrap(~Station.Name, scales = "free")
# }


#  standardize col names and write out as tidy RDA file
snotel_out <- check_datetime(dat = snotel, datecol = "Date",groupvar = "Station.Name", idcols = names(snotel)[grepl("Stat|Elev|Lat|Long", names(snotel), ignore.case = T)])
# convert snotel station elevation from ft to meters
snotel_out$Elevation..ft. <- ft2m(snotel_out$Elevation..ft.)
snotel_out <- tidySnotel(snotel_out)



# 2.3 GHCNd -----
ghcndattr <- names(ghcnd)[grepl("ATTR", names(ghcnd))]
ghcndmets <- gsub("_A.*$", "", ghcndattr)
# pink points are dates not present in the dataset (missing data = date present, no value; date missing = no row for the date [date and value missing])
# plot_all_groups(ghcnd, timecol = "DATE", mets = ghcndmets, groupvars = "NAME", allvars = F)
# # because some of these datasets are missing dates, you will see lines connecting gaps (because in the dataset row x and row y are sequential, even though they are not sequential dates)
# plot_all_groups(ghcnd, timecol = "DATE", mets = ghcndmets, groupvars = "NAME", allvars = F, plotNA = F)

# checking for missing dates in the dataset is part of the within-site QC process, and so we will table that correction for later

#' Of all sites shown, the following that seem worthwhile (coverage overlapping target datasets) for considering are:
#' 
#' * Allenspark 2 SE (some gaps in 1980s and 90s but could be otherwise helpful for infilling early Saddle)
#' * Allenspark 3 NW (good coverage for precip and temp mid 1990s to mid 2000s -- if station didn't change, would be interesting to eval Saddle precip relationship vs. here)
#' * Boulder 14 W (good ppt and temp coverage mid 2000s to present -- this is also one of NOAA's higher quality climate stations)
#' * Coal Creek (decent precip and temp coverage 1980s to early 2020)
#' * Fraser (gap 1970s to 1980s, but otherwise goes through present day and pre-dates 1920 -- this may mean site shift though)
#' * Estes Park (coverage)
#' * Estes Park 3 SSE (ppt and temp coverage 2000 - present)
#' * Grand Lake 1 NW (ppt and temp coverage 1940s - present)
#' * Grand Lake 6 SSW
#' * Berthoud Pass (for late 1960s-1980s)
#' * Nederland 2 NEE (temp and ppt coverage 1970s to late 1980s)
#' * Winter Park (won't be too helpful for temp infilling, but precip coverage is 1940s-present)
#' 
#' 
#' Sites to drop from further use:
#' 
#' * Caribou Ranch (only goes to early 1970s -- but if C1 and D1 ever needed to be re-evaluated during that time, good candidate)
#' * Nederland 5 NEE (too short of a time-series)
#' * Silver Lake (only helpful for infilling through 1950s, but one of the few comparative stations for early C1 and D1 infilling)
#' 
#' 
#' Of the measurement variables present, we'll only keep TMAX, TMIN, PRCP, and TOBS (time of observation, which can influence temp systematically if it changes)

ghcnd_idcols <- names(ghcnd)[grepl("stat|name|lat|long|elev", names(ghcnd), ignore.case = T)]
ghcnd_out <- subset(ghcnd, !grepl("carib|nederland 5|silver", NAME, ignore.case = T), 
                    select = c(ghcnd_idcols, names(ghcnd)[grepl("tmin|tmax|prcp|tobs|date", names(ghcnd), ignore.case = T)]))
# be sure to run check_datetime by the unique ID key for the dataset (e.g., in ghcnd, NAME value 'FRASER, CO US' has two different STATION values)
ghcnd_out <- check_datetime(ghcnd_out, datecol = "DATE", idcols = ghcnd_idcols, groupvar = "STATION")

# collapse variable columns in so tidy (metrics in one column, attributes in another, then break into 4 cols of flag types )
ghcnd_out <- tidyGHCND(ghcnd_out, mets = c("TMAX", "TMIN", "PRCP", "TOBS")) # warning message is fine

# check for NAs
summary(ghcnd_out) # there shouldn't be NAs for yr, mon or doy



# -- WRITE OUT ------
#' Now that we've reviewed datasets available, we want to separate temp from ppt in each data source, and assign common names across all datasets for simplicity
#' We also want to make sure the data source is present in each dataset
#' Once prepped, write out each set separately for QC. Some stations may have too many QC flags for infilling use, and so that's why we keep sources separate when writing out.
#' Once QC is done and comparative sets are prepared for infilling, then we can combine all stations in one dataset. 


#' Names can be whatever the user prefers, as long as they are consistent across datasets
#' I prefer lower camelCase or snake_case format, so that's how I'll prep dats out here:
#' 

# standardize stations cols in ghcnd, and make metric convention similar to NWT
names(ghcnd_out)[names(ghcnd_out) %in% c("station", "name")] <- c("station_id", "station_name")
ghcnd_out$metric[ghcnd_out$metric == "TMIN"] <- "airtemp_min"
ghcnd_out$metric[ghcnd_out$metric == "TMAX"] <- "airtemp_max"
ghcnd_out$metric[ghcnd_out$metric == "PRCP"] <- "ppt_tot"
# time of observation still present as TOBS in metric


# snotel colnames already taken care of by tidySnotel function, but metrics need standardization
snotel_out$metric <- gsub("Air.Temperature", "airtemp", snotel_out$metric)
snotel_out$metric <- gsub("Precipitation.Increment", "ppt_tot", snotel_out$metric)
snotel_out$metric <- gsub(".", "_", snotel_out$metric, fixed = T)
snotel_out$metric <- casefold(snotel_out$metric)
snotel_out$metric[grepl("airtemp", snotel_out$metric)] <- substr(snotel_out$metric[grepl("airtemp", snotel_out$metric)],1,11)
snotel_out$metric <- gsub("_ave", "_avg", snotel_out$metric)

# ameriflux colnames and metric conventions already taken care of by tidyAmeriflux function


# can write out as .csv or rdata because they are large
# NWT data
write.csv(chartTemp_out, paste0(datpath, "/prep/nwtchartTemp_prep.csv"), row.names = F) #15 MB
write.csv(chartPPT_out, paste0(datpath, "/prep/nwtchartPPT_prep.csv"), row.names = F) #15 MB
write.csv(loggerTemp_out, paste0(datpath, "/prep/nwtloggerTemp_prep.csv"), row.names = F) #15 MB
write.csv(c1loggerPPT_out, paste0(datpath, "/prep/c1loggerPPT_prep.csv"), row.names = F) #15 MB
saveRDS(chartTemp_out, paste0(datpath, "/prep/nwtchartTemp_prep.rds")) #15 MB
saveRDS(chartPPT_out, paste0(datpath, "/prep/nwtchartPPT_prep.rds")) #15 MB
saveRDS(loggerTemp_out, paste0(datpath, "/prep/nwtloggerTemp_prep.rds")) #15 MB
saveRDS(c1loggerPPT_out, paste0(datpath, "/prep/c1loggerPPT_prep.rds")) #15 MB

# other data
write.csv(ameriflux_out, paste0(datpath, "/prep/ameriflux_prep.csv"), row.names = F) #15 MB
write.csv(snotel_out, paste0(datpath, "/prep/snotel_prep.csv"), row.names = F) #28 MB
write.csv(ghcnd_out, paste0(datpath, "/prep/ghcnd_prep.csv"), row.names = F) #103 MB
saveRDS(ameriflux_out, paste0(datpath, "/prep/ameriflux_prep.rds"))
saveRDS(snotel_out, paste0(datpath, "/prep/snotel_prep.rds"))
saveRDS(ghcnd_out, paste0(datpath, "/prep/ghcnd_prep.rds"))
