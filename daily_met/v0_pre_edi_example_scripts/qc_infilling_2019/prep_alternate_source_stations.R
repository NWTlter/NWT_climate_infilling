# get non-NWT infill stations + prep for infilling

# script to grab alternate infill datasets for Saddle infilling, based on stations TK used to infill C1 and D1 (below in NOTES)
# specifically, read in all datasets and:
# convert to metric if not
# screen out any extreme hi or low unrealistic values for Saddle area
# store all wide form for easy infill reference dataset?
# write out to output_data/prep_data


# NOTES:
# National Weather COOP or Remote Automatic Weather Stations:
# sources: 
# CSU Climate Center: https://climate.colostate.edu/data_access.html
# NOAA Coop weather stations: https://www.ncdc.noaa.gov/cdo-web/search

# Estes Park *2 estes park stations on CSU Climate Center (many on NOAA).. together they provide continuous coverage through present, so grabbed both
# Allespark Lodge *very patchy data (not much available for period)
# Caribou Ranch *only 1962-1970
# Nederland 2NNE * from CSU Climate Center website, 1981-May 1988 .. more recent data on NOAA but there are 9 Ned stations (none match NNE, and all have patchy data)
# Coal Creek Canyon *available, ordered from NOAA
# Grand Lake 1 NW *available 1981-2019, ordered from NOAA
# Grand Lake 6 SSW * avialable 1981-2019, ordered from NOAA
# Fraser 3 *one Fraser stations available and ordered from NOAA .. not sure whether this is F3 or F6
# Fraser 6 * on CSU Climate Center website, one of the 2 Fraser stations only goes through 1974 anyway; station ID 053116 is the one with data from 1989-2019
# Berthoud Pass (*only non-NWT alpine alternate station) *Jan 1981 - May 1985 available on CSU Climate Center webpage, not on NOAA

# NWT datasets (prepped and QA'd in different script):
# D1 chart (temp + ppt)
# D1 logger (temp only)
# C1 chart (temp + ppt)
# C1 logger (temp + ppt)
# SDL logger (temp only)

# NRCS datasets (prepped in different script):
# Snotel at C1
# Snotel at University Camp (near Lake Albion)

# note: Saddle data only goes back to 1981, so alternate datasets should have data from then or later


# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(readxl)
library(lubridate)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN", "NP")

# set data pathway to raw weather files
rawdat <- "/Users/serahsierra/Documents/nwt_lter/nwt_climate/raw_data"

# list raw data files
datfiles <- list.files(rawdat, full.names = T)
xlfiles <- excel_sheets(datfiles[grep("xls", datfiles)])
datlist <- list()
# read in csvs
for(i in 1:length(datfiles[grep("csv", datfiles)])){
  datlist[[i]] <- data.frame(read.csv(datfiles[i]))
}
# read in excel spreadsheets
for(i in 1:length(xlfiles)){
  p <- i + length(datfiles[grep("csv", datfiles)])
  datlist[[p]] <- data.frame(read_xlsx(datfiles[grep("xls", datfiles)], sheet = xlfiles[i], skip = 8, col_names = TRUE))
}
# confirm everything read in as expected..
str(datlist) #looks okay
# get station info for excel spreadsheets
metadat <- list()
for(i in 1:length(xlfiles)){
  metadat[[i]] <- read_xlsx(datfiles[grep("xls", datfiles)], sheet = xlfiles[i], n_max = 8)
}



# -- PREP DATA ----
# break out data frame separately to clean up, convert (if needed), and screen for outlier values
fraser <- datlist[[1]] # metric, no dates if data missing
coalcrk <- datlist[[2]] # metric
grandlk1 <- datlist[[3]] # metric
grandlk6 <- datlist[[4]] # metric
estes1 <- datlist[[5]] # metric
boulder14 <- datlist[[6]] # metric
fraser2 <- datlist[[7]] # same as fraser, but from CSU climate center, american units
berthoud <- datlist[[8]] # needs metric con
ned2NNE <- datlist[[9]] # needs metric con
estes2 <- datlist[[10]] # needs metric con
estes3 <- datlist[[11]] # needs metric con

# consistent names/info across all dats are: station, name, date, tmax, tmin, and prcp.. sometimes snow
# requested data flags from NOAA but didn't arrive with data..
# in a quick manual scan, there are dates with non-zero snow and 0 ppt.. conserve snow col to check for this



# -- CLEAN UP NOAA DATA -----
clean_dat <- function(dat){
  dat$DATE <- as.Date(dat$DATE)
  tempdat <- data.frame(station = unique(dat$NAME),
                        stationID = unique(dat$STATION),
                        date = seq.Date(min(dat$DATE), max(dat$DATE), 1))
  tempdat <- tempdat %>%
    mutate(yr = year(date),
           mon = month(date), 
           doy = yday(date)) %>%
    left_join(dat, by = c("date" = "DATE"))
  keepnames <- which(colnames(tempdat) %in% c("TMAX", "TMIN", "PRCP", "SNOW"))
  tempdat <- tempdat[,c(1:6, keepnames)]
  return(tempdat)
}

fraser_clean <- clean_dat(fraser)
coalcrk_clean <- clean_dat(coalcrk)
grandlk1_clean <- clean_dat(grandlk1)
grandlk6_clean <- clean_dat(grandlk6)
estes_clean <- clean_dat(estes1)
boulder14_clean <- clean_dat(boulder14)



# -- CLEAN UP CSU CLIMATE DATA -----
#fxn to convert F to C
F2C <- function(xF){
  xC <- (xF - 32) * (5/9)
  xC <- round(xC, 2)
  return(xC)
}

# store metadata in searchable df
metadat_df <- data.frame(unlist(metadat))
metadat_df$station <- row.names(metadat_df)

clean_csu <- function(dat){
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

fraser2_clean <- clean_csu(fraser2)
berthoud_clean <- clean_csu(berthoud)
ned2NNE_clean <- clean_csu(ned2NNE)
estes2_clean <- clean_csu(estes2)
estes3_clean <- clean_csu(estes3)

# stack CSU climate dfs
csu_dats <- rbind(fraser2_clean, berthoud_clean, ned2NNE_clean, estes2_clean, estes3_clean)


# -- QUALITY CONTROL ----
# look for extreme high or low values
gather(csu_dats, met, val, TMAX:PRCP) %>%
ggplot(aes(station, val)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~met, scales = "free_y")

gather(csu_dats, met, val, TMAX:PRCP) %>%
ggplot(aes(date, val, col = station)) +
  geom_point(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~met, scales = "free_y")

# vals seem reasonable
