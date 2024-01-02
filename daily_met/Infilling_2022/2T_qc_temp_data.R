################################################################################
#' ---
#' title: QC temperature data 
#' author: CTW  
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#' 
#' ### QC checks on temperature pre gap-filling
#' 
#' Test report to:
#' 1. Read in prepared NWT and neighbor station temperature datasets
#' 2. Flag within station QC checks
#' 3. Flag comparative station QC checks
#' 4. Remove/NA values based on flag count or type of flag violation
#' 5. Prepare temp data for gap-filling and write out as RDS filkes
#' 6. Make allsites key for site order (may be different than ppt allsites order if temp infilling involves different sites, including loggers within sites)
#' 
#' All code will be displayed to show procedure and work out bugs
################################################################################

rm(list=ls())

# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

generate_plots <- FALSE

source("daily_met/R/temp_qc_functions.R")
source("daily_met/R/fetch_data_functions.R") # to read in prepped tidy datasets
source("daily_met/R/dataviz_functions.R")
source("daily_met/R/prep_data_functions.R")

# set path to prepared data
datpath <- "~/OneDrive - UCB-O365/nwt_climate/data/"

# list all rds files in prepped data folder (full path directory)
rdsfiles <- list.files(paste0(datpath, "prep"), pattern = "rds", full.names = T)

################################################################################
# Reading in Data
################################################################################

# read in prepared temp data
charttemp <- get_tidydat("chartTemp", rdsfiles, c("*")) # use asterisk if want all metrics available
nwtlog <- get_tidydat("nwtlog", rdsfiles, "*")
nwtchart <- get_tidydat("nwtchartTemp_prep", rdsfiles, "*")
ameriflux <- get_tidydat("ameri", rdsfiles, "airtemp")
snotel <- get_tidydat("sno", rdsfiles, "airtemp")
ghcnd <- get_tidydat("ghc", rdsfiles, c("temp", "TOBS"))

# read in TK d1 and c1 temp
tktemp <- getNWTchartsinfilled(mets = c("temp"))
tktemp <- stackData(tktemp)

# Pull out the C1 chart from the full nwt chart file for easy visualization
c1_chart_tmp <- charttemp %>% dplyr::filter(local_site == 'c1')


# QAQCing C1 for appending 2022 infilled chart data 
# for now, just focusing on c1 dat
# she needs 1989 - 2020.. but try just for loggers if possible

# what are data availability for charts and loggers
if(generate_plots){
  ggplot(c1_chart_tmp, aes(date, measurement))+
    geom_point(alpha = 0.5)+
    facet_wrap(~metric, nrow = 3)+
    geom_vline(aes(xintercept = lubridate::date("2019-01-01")), color = 'red')+
    geom_vline(aes(xintercept = lubridate::date("2023-01-01")), color = 'red')
  
  ggplot(nwtlog %>% subset(local_site == 'c1'), aes(date, measurement))+
    geom_point(alpha = 0.5)+
    facet_grid(logger~metric)+
    geom_vline(aes(xintercept = lubridate::date("2019-01-01")), color = 'red')+
    geom_vline(aes(xintercept = lubridate::date("2023-01-01")), color = 'red')+
    ggtitle('C1 temp data with 2022 marked')
}

#at c1, only hmps were running from 2020 ish to 2022. 
# NOTE!! From Oct 2022 on, only HMP1 is running in the stevenson screen,  the other HMPs were
# moved to different positions in the canopy and are now aspirated temperatures.
# Can these even be used to fill? For now aspirateds were removed from dataset
# in the prep data step (script 1). Only HMP1 will be able to infill
# c1 chart from Oct 2022 - Dec 2022


################################################################################
# Cleaning data
################################################################################

# goals:
# want each dataset with its list of flags (source flags + additional qc flags), 
# the raw val, and the qc_measurement col that has violators NA'd
# i.e., each dat out is ready to run through infilling in next script
# for triplicate hmps, think it makes sense to infill each sensor (3 tot) 
# and then average those. bc otherwise data points averaged can vary by date

# also be sure to screen for timestamp changes at ghcnd stations, and remove C1 
# and D1 pre-1960s respective dates (what it was in the ppt workflow)


# -- 1. NWT chart dats ----- 

# if was flagged and infilled by TK remove
tktemp_tidy <- subset(tktemp, select = c(LTER_site:flag_3)) %>%
  gather(metric, tk_measurement, max_temp:DTR) %>%
  mutate(metric = gsub("_temp", "", metric),
         metric = gsub("mean", "avg", metric),
         metric = ifelse(metric != "DTR", paste0("airtemp_", metric), metric),
         # denote which measurements are ok/raw based on their flagging
         tk_predicted = ifelse(metric == "airtemp_max" & flag_2 %in% c("A", "D"), FALSE,
                               ifelse(metric == "airtemp_min" & flag_2 %in% c("A", "C"), FALSE,
                                      ifelse(flag_1 == "A", FALSE, TRUE))),
         # lowcase to bind with chart
         local_site = casefold(local_site)) #%>%

charttemp_out <- charttemp %>%
  mutate(raw = measurement,
         # if it's been previously infilled, NA (leave questionable flagged vals
         # and will NA if fail other checks)
         measurement = ifelse(is.na(flag) | flag == "q", measurement, NA)) %>%
  # join to charttemp_out and NA anything that was infilled or adjusted
  left_join(tktemp_tidy[c("date", "local_site", "metric", 
                          "tk_measurement", "tk_predicted")]) %>%
  mutate(ctw_adjusted = measurement != tk_measurement &
           !tk_predicted & yr > 2010) %>%
  # drop DTR and tmean bc realize the average in raw is rounded 
  # (e.g., in TK dataset it's 13.5 but in raw it's 14, even when math mean is 13.5)
  # recalculate DTR and tmean if it's not a value that's been infilled or adjusted in some way
  subset(!grepl("avg|DTR", metric))

# NA infilled vals
charttemp_out <- mutate(charttemp_out, measurement = ifelse(tk_predicted, NA, measurement))

# assign raw if 2019 onwards for d1 or c1, and for sdl [except NA previously infilled values]
charttemp_out$measurement[is.na(charttemp_out$tk_predicted) & 
                            is.na(charttemp_out$flag)] <- with(charttemp_out, raw[is.na(tk_predicted) & is.na(flag)])
charttemp_out$station_id <- paste0(charttemp_out$local_site, "_chart")

# Sanity Cleaning
charttemp_out <- distinct(charttemp_out) #doesn't look like it changes anything.


# -- 2. NWT logger dats -----
nwtlog_out <- nwtlog %>%
  mutate(local_site = casefold(local_site), #lowcase all sites
         station_id = ifelse(!is.na(rep), paste(local_site, logger, rep, sep= "_"), 
                             paste(local_site, logger, sep = "_")),
         raw = measurement, 
         measurement = ifelse(measurement >= -50 & measurement <= 40 & flag == 'n',
                              measurement, NA)
  )

# # Taking a closer look at the sdl HMPs in July 2020, downspike?
if(generate_plots){
  nwtlog_out %>% dplyr::filter(grepl("sdl_cr1000_hmp*", station_id))%>%
  dplyr::filter(lubridate::year(date) == 2022) %>%
  ggplot()+
    geom_point(aes(date, measurement, color = flag))+
    facet_grid(station_id~metric)}

# Clean out flagged
nwtlog_out <- nwtlog_out %>% mutate(
  measurement = ifelse(flag != 'n', NA, measurement)
)


# I am not sure why the CS500 temp sensor (station_id == cr_1000) has min obs 
# after the reported removal date in the methods of the .401 package.
# nwtlog_out |> dplyr::filter(grepl("c1_cr1000", station_id)) |>
#   ggplot()+
#   geom_point(aes(date, measurement, shape = metric, color = flag))+
#   facet_wrap(~station_id)+
#   geom_vline(aes(xintercept=lubridate::date("2018-08-27")))

#I think it is not trust worthy, so removing.
nwtlog_out <- nwtlog_out |> 
  mutate(
    #If cs500, but measurement is after reported removal date (in knb-nwt-lter.401 methods),
    # then NA the data.
    measurement = ifelse(station_id == "c1_cr1000" &
                           date > lubridate::date("2018-08-27"), NA, measurement)
  ) #confirmed this working as expected


# -- 3. GHCNd -----
# limit other dat by starting date of nwt chart data
ghcnd_out <- subset(ghcnd, yr >= 1980)
# in ghcnd want to check if time of observation influences measurement trend at all (shouldn't for ppt)
sort(unique(paste(ghcnd_out$station_name, ghcnd_out$station_id))) # 12 stations

# check for time change at stations
ghcnd_tobs <- group_by(ghcnd_out, station_name, station_id)   %>% 
  mutate(global_last = max(date)) %>%
  #ungroup() %>%
  group_by(station_name, station_id, global_last, time_observed) %>% 
  summarise(nobs = length(measurement),
            switchdate = min(date),
            lastdate = max(date)) %>%
  arrange(station_name, switchdate)
# like ppt, fraser is only station that will need to be split by time change 
# (7 am through jan 12 1995, and then switched to 1600 then to present)

# review vals with q_flags to see how many data points involved
# NA anything that has a qc flag (but keep raw_measurement as in nwt_chart just 
# in case want to run infilling with more values)
# what are the flags again?
sort(unique(ghcnd_out$q))
# "G" = failed gap check (ctw: ?)
# "I" = failed internal consistency
# "N" = failed naught check (ctw: ?)
# "O" = climatological outlier
# "R" = failed lagged range
# "S" = failed spatial consistency
# "T" = failed temporal consistency

if(generate_plots){
  ggplot(subset(ghcnd_out, metric == "airtemp_min"), aes(date, measurement)) +
    geom_line(alpha = 0.2) +
    geom_point(data = subset(ghcnd_out, !is.na(q) & metric == "airtemp_min"), aes(col = q)) +
    facet_wrap(~paste(station_id, station_name), scales = "free_x")+
    geom_vline(aes(xintercept = lubridate::date("2019-01-01")), color = 'red')+
    geom_vline(aes(xintercept = lubridate::date("2023-01-01")), color = 'red')}

# ggplot(subset(ghcnd_out, metric == "airtemp_max"), aes(date, measurement)) +
#   geom_line(alpha = 0.2) +
#   geom_point(data = subset(ghcnd_out, !is.na(q) & grepl("max", metric)), aes(col = q)) +
#   facet_wrap(~paste(station_id, station_name), scales = "free_x")

# what does flagged internal look like up close?
# ggplot(subset(ghcnd_out, grepl("BER", station_name)), aes(date, measurement)) +
#   geom_line(alpha = 0.2) +
#   geom_point(data = subset(ghcnd_out, !is.na(q) & grepl("BER", station_name)), aes(col = q)) +
#   facet_wrap(~metric, nrow = 3)
# 
# # what would data availability be if remove all flagged dats?
# ggplot(subset(ghcnd_out, metric == "airtemp_max" & is.na(q) & grepl("FRA", station_name)), aes(date, measurement)) +
#   geom_line(alpha = 0.2) +
#   geom_point(data = subset(ghcnd_out, !is.na(q) & grepl("max", metric) & grepl("FRA", station_name)), aes(col = q)) +
#   facet_wrap(~paste(station_id, station_name), scales = "free_x") # ok with removing q-flagged dat

#Save raw data
ghcnd_out$raw <- ghcnd_out$measurement

# Create a new column that nukes flagged data
ghcnd_out$measurement[!is.na(ghcnd_out$q)] <- NA

# Create a new column that indicates if this row was nuked in measurement
ghcnd_out$qc_note <- with(ghcnd_out, ifelse(!is.na(q), "value NAd for q flag", NA))

# note when obs time switched by more than 6 hours (that should cover shifts of the day?)
# > note to self: build run check function later

switchdate <- with(ghcnd_tobs, max(switchdate[grepl("116$", station_id)]))
# note time switch at fraser
ghcnd_out$qc_note[grepl("116$", ghcnd_out$station_id) & ghcnd_out$date == switchdate] <- paste("Time switch from 0700 to", with(ghcnd_out, time_observed[grepl("116$", station_id) & date == switchdate & !is.na(time_observed)]))
ghcnd_out$local_site <- ghcnd_out$station_id
ghcnd_out$local_site[grepl("116$", ghcnd_out$station_id) & ghcnd_out$date < switchdate] <- with(ghcnd_out, paste(unique(station_id[grepl("116$", ghcnd_out$station_id)]), "0700", sep = "_"))
ghcnd_out$local_site[grepl("116$", ghcnd_out$station_id) & ghcnd_out$date >= switchdate] <- with(ghcnd_out, paste(unique(station_id[grepl("116$", ghcnd_out$station_id)]), "1600", sep = "_"))

# be sure each station starts at it minimum start time
for(s in unique(ghcnd_out$local_site)){
  mindate <- min(ghcnd_out$date[ghcnd_out$local_site == s & !is.na(ghcnd_out$raw)])
  ghcnd_out <- ghcnd_out[!(ghcnd_out$local_site == s & ghcnd_out$date < mindate),]
}


# -- 4. SNOTEL ------
# plot data
# ggplot(snotel, aes(date, measurement)) +
#   geom_line() +
#   facet_grid(metric ~ station_name)


snotel_out <- snotel
snotel_out$local_site <- gsub(" ", "", snotel$station_name)

# temp record for some of these stations began after precip (but still have those starting dates). start each station at when data consistently starts.
for(s in unique(snotel_out$local_site)){
  mindate <- with(snotel_out, min(date[local_site == s & !is.na(measurement)]))
  snotel_out <- snotel_out[!(snotel_out$local_site == s & snotel_out$date < mindate),]
}


# -- 5. AmeriFlux -----
# ggplot(ameriflux, aes(date, raw_measurement)) +
#   geom_line() +
#   facet_grid(metric ~ paste(station_name, rep))


ameriflux_out <- ameriflux
ameriflux_out$local_site <- with(ameriflux_out, ifelse(is.na(rep), station_id, paste0(station_id, rep)))
unique(ameriflux_out$local_site)
ameriflux_out$local_site <- gsub("-", "_", ameriflux_out$local_site)
# assign qc value for measurement at forest, raw for measurement at tvan
ameriflux_out$measurement <- with(ameriflux_out, ifelse(grepl("NR1", local_site), qc_measurement, raw_measurement))

# start each site at its first nonNA date
for(s in unique(ameriflux_out$local_site)){
  mindate <- with(ameriflux_out, min(date[local_site == s & !is.na(measurement)]))
  ameriflux_out <- ameriflux_out[!(ameriflux_out$local_site == s & ameriflux_out$date < mindate),]
}

ameriflux_out <- distinct(ameriflux_out)



################################################################################
# Creating spatial relationships between sites
################################################################################

# niwot info: lat long pulled from ppt datasets on edi, elev pulled from niwot 
# website (different from what's listed on edi dataset? but only by several m); 
# except c1 coords from b curtis dataset bc is a box on c1 climate
# lat, lon, elev (masl)
nwt_info <- data.frame(rbind(
  c("sdl", 40.05498494, -105.5907045, 3528),
  # make c1 the same as snotel 40.03596085439916, -105.54415060302942 (taken from google maps)
  c("c1", 40.03596085439916, -105.54415060302942, 3022),  
  c("d1", 40.05953123, -105.616957, 3734),
  # set gl4 to gl4 inlet (coords listed in edi gl4 climate dataset are off.. closer to caribou ranch)
  c("gl4",40.055639, -105.617102, 3560) 
))

# assign col names and correct col class 
names(nwt_info) <- c("local_site", "latitude", "longitude", "elevation")
nwt_info[,2:4] <- apply(nwt_info[,2:4], 2, as.numeric)
str(nwt_info)

# join chart and logger dats
nwt_info <- left_join(nwt_info,
                      distinct(charttemp_out[c("local_site", "station_id")]) %>%
                        rbind(distinct(nwtlog_out[names(.)]))) 

nwt_info$station_name <- with(nwt_info, ifelse(grepl("chart", station_id), 
                                               paste(local_site, "chart"), 
                                               paste(local_site, "logger")))

# change nwt local_site to station_id so consistent use with other stations
nwt_info$local_site <- nwt_info$station_id
# add data source
nwt_info$source <- "nwt lter"

ghcnd_sites <- distinct(ghcnd_out, local_site, station_id, station_name, latitude, longitude, elevation)
ghcnd_sites$source <- "ghcnd"

ameriflux_out$source <- "ameriflux"
ameriflux_sites <- distinct(subset(ameriflux_out, select = names(ghcnd_sites)))


snotel_sites <- distinct(subset(snotel_out, select = c(station_id, station_name, latitude, longitude, elevation))) %>%
  mutate(local_site = gsub(" ", "", station_name))
snotel_sites$source <- "snotel"

# bind all
allsites <- rbind(ghcnd_sites, ameriflux_sites, snotel_sites) %>%
  rbind(nwt_info[names(.)])

allsites_dist <- data.frame(geodist::geodist(allsites[c("longitude", "latitude")]))
colnames(allsites_dist) <- allsites$local_site
allsites_dist <- cbind(local_site = allsites$local_site, allsites_dist)
allsites_dist <- allsites_dist %>%
  gather(paired_site, xy_distance, 2:ncol(.))

allsites_elev <- as.matrix(dist(allsites$elevation)) %>%
  data.frame()
colnames(allsites_elev) <- allsites$local_site
allsites_elev <- cbind(local_site = allsites$local_site, allsites_elev) %>%
  gather(paired_site, z_distance, 2:ncol(.))

# join coord dist and elev dist back to all_sites
allsites <- left_join(allsites, allsites_dist) %>%
  left_join(allsites_elev) %>%
  group_by(local_site) %>%
  mutate(xy_rank = ifelse(xy_distance > 0, rank(xy_distance), NA),
         z_rank = ifelse(z_distance > 0, rank(z_distance), NA),
         # sum_rank = xy_rank + z_rank,
         # mean_rank = (xy_rank + z_rank)/2,
         combo_rank = rank(xy_distance + z_distance)) %>%
  ungroup()
allsites <- mutate_at(allsites, grep("lat|lon|elev", names(allsites)), as.numeric)

# prioritize chart sources over loggers for chart, and logger sources over chart for loggers
allsites$combo_rank_adj <- allsites$combo_rank
allsites$combo_rank_adj <- with(allsites, ifelse(source == "nwt lter" & grepl("logger", station_name) & grepl("_cr", paired_site), combo_rank_adj-1, combo_rank_adj)) 
allsites$combo_rank_adj <- with(allsites, ifelse(source == "nwt lter" & grepl("chart", station_name) & grepl("chart", paired_site), combo_rank_adj-1, combo_rank_adj)) 
# make sure rank is 1 for site with itself
allsites$combo_rank_adj <- with(allsites, ifelse(local_site == paired_site, 1, combo_rank_adj))

# put c1 ahead of niwot snotel and forest ameriflux for sdl and d1
#ctw looked manually to adj but this can be made programmatic (e.g., what rank is university camp then niwot snotel/forest flux v. c1 stations, flip flop)
allsites$final_rank <- allsites$combo_rank_adj
allsites$final_rank <- with(allsites, ifelse(grepl("sdl|d1", local_site) & grepl("c1", paired_site), combo_rank_adj-4, 
                                       ifelse(grepl("sdl|d1", local_site) & grepl("niwot|us_nr1", paired_site, ignore.case = T), combo_rank_adj +4, combo_rank_adj)))



# map out
# ggplot(distinct(allsites, local_site, elevation, latitude, longitude), aes(longitude, latitude, fill = elevation)) +
#   #geom_point(alpha = 0.5) +
#   geom_label(aes(label = local_site), alpha = 0.5) +
#   scale_fill_viridis_c()
# 
# ggplot(distinct(allsites, local_site, elevation, latitude, longitude), aes(longitude, elevation, fill = elevation)) +
#   #geom_point(alpha = 0.5) +
#   geom_label(aes(label = local_site), alpha = 0.5, ) +
#   scale_fill_viridis_c()

################################################################################
# Standardizing datasets a little more
################################################################################

# switch local_site to allsites$local site for nwt; preserve orig 
# local site in different col i guess?
charttemp_out$local_site <- charttemp_out$station_id
charttemp_out$station_id <- stringr::str_extract(charttemp_out$station_id, "d1|c1|sdl|gl4")
charttemp_out$station_name <- charttemp_out$local_site


nwtlog_out$local_site <- nwtlog_out$station_id
nwtlog_out$station_id <- str_extract(nwtlog_out$station_id, "d1|c1|sdl|gl4")
nwtlog_out$station_name <- paste(nwtlog_out$station_id, "logger")

names(ameriflux_out) <- gsub("raw_measurement", "raw", names(ameriflux_out))


# -- WRITE OUT -----
# write out qc'd temp for infilling next
saveRDS(charttemp_out, paste0(datpath, "/qc/nwtchartTEMP_qc.rds"))
saveRDS(nwtlog_out, paste0(datpath, "/qc/nwtloggerTEMP_qc.rds"))
saveRDS(ghcnd_out, paste0(datpath, "/qc/ghcndTEMP_qc.rds"))
saveRDS(snotel_out, paste0(datpath, "/qc/snotelTEMP_qc.rds"))
saveRDS(ameriflux_out, paste0(datpath, "/qc/amerifluxTEMP_qc.rds"))
saveRDS(allsites, paste0(datpath, "/qc/siteinfoTEMP_qc.rds"))

rm(list=ls()) #clean env.

################################################################################
# Read back in and join with long term trends qc'd data & prep for regressions
################################################################################

generate_plots <- FALSE

source("daily_met/R/temp_qc_functions.R")
source("daily_met/R/fetch_data_functions.R") # to read in prepped tidy datasets
source("daily_met/R/dataviz_functions.R")
source("daily_met/R/prep_data_functions.R")

# set path to prepared data
datpath <- "~/OneDrive - UCB-O365/nwt_climate/data/"


# list all rds files in qc'd data folder (full path directory)
rdsfiles_qc <- list.files(paste0(datpath, "qc"), pattern = "rds", full.names = T)


charttemp <- get_tidydat("chartTEMP_qc.", rdsfiles_qc, "*")
logtemp <- get_tidydat("loggerTEMP_qc", rdsfiles_qc, "*")
ameriflux <- get_tidydat("fluxTEMP_qc", rdsfiles_qc, "*")
snotel <- get_tidydat("telTEMP_qc", rdsfiles_qc, "*")
ghcnd <- get_tidydat("ghcndTEMP_qc", rdsfiles_qc, "*")
allsites <- readRDS(rdsfiles_qc[grep("infoTEM", rdsfiles_qc)])

# read in previously QC'd dats (from 2018 renewal)
# set pathway to long-term-trends repo (here previously qc'd data live)
lttrepo <- "~/NWTLTER/long-term-trends/"
qcdat <- "output_data/prep_data"
qcc1d1 <- list.files(paste0(lttrepo, "climate_d1_c1/", qcdat), full.names = T)
qcsdl <- list.files(paste0(lttrepo, "extended_summer/analysis/", qcdat), full.names = T)

# loggers qc'd
qc_d1cr <- read_csv(qcc1d1[grep("d1cr", qcc1d1)]) %>% data.frame()
qc_c1cr <- read_csv(qcc1d1[grep("c1cr", qcc1d1)]) %>% data.frame()
qc_sdlcr <- read_csv(qcsdl[grep("sdlcr_temp[.]", qcsdl)]) %>% data.frame()

# charts qc'd
qc_d1 <- read_csv(qcc1d1[grep("d1_te", qcc1d1)]) %>% data.frame()
qc_c1 <- read_csv(qcc1d1[grep("c1_te", qcc1d1)]) %>% data.frame()
qc_sdl <- read_csv(qcsdl[grep("sdlchart", qcsdl)]) %>% data.frame()


# Caitlin was using some of the processed data from the long-term-trends repository
# I think maybe this isn't necessary, but she included it and one of the two tk
# infilling methods using long term (across year) data, so I think it is best
# to keep this included for continuity of future infilling.

# plot qc'd data first to review what got flagged
# ggplot(subset(qc_c1, yr > 2010 & grepl("+3C", qa_flag)), aes(date, qa_temp)) +
#   geom_point(aes(date, c1_temp), alpha = 0.5) +
#   geom_point(aes(date, qa_temp), col = "red", alpha = 0.2) +
#   facet_wrap(~met, scales = "free_x")
# 
# ggplot(subset(qc_d1, yr > 2010 & grepl("drop", qa_flag)), aes(date, qa_temp)) +
#   geom_point(aes(date, d1_temp), alpha = 0.5) +
#   geom_point(aes(date, qa_temp), col = "red", alpha = 0.2) +
#   facet_wrap(~met, scales = "free_x")

# plot c1, d1 and sdl to screen for drop in 15-16 at sdl
# ggplot() +
#   geom_point(data = subset(qc_c1, yr > 2010 & grepl("artificial", qa_flag, ignore.case = T)), aes(date, c1_temp), color = "white", alpha = 0.8) +
#   geom_line(data = subset(qc_c1, yr > 2010), aes(date, qa_temp), color = "green", alpha = 0.5) +
#   geom_point(data = subset(qc_d1, yr > 2010 & grepl("artificial", qa_flag, ignore.case = T)), aes(date, d1_temp), color = "white", alpha = 0.8) +
#   geom_line(data = subset(qc_d1, yr > 2010), aes(date, qa_temp), color = "blue", alpha = 0.5) +
#   geom_point(data = subset(qc_sdl, yr > 2010), aes(date, sdl_temp), color = "white", alpha = 0.8) +
#   geom_line(data = subset(qc_sdl, yr > 2010), aes(date, sdl_qatemp), color = "orchid", alpha = 0.5) +
#   facet_grid(met~local_site) +
#   theme_dark()

# ggplot() +
#   geom_point(data = subset(qc_c1cr, !is.na(qa_flag) & c1cr_temp > -100), aes(date, c1cr_temp), color = "white", alpha = 0.8) +
#   geom_line(data = subset(qc_c1cr), aes(date, qa_temp), color = "green", alpha = 0.5) +
#   geom_point(data = subset(qc_d1cr, !is.na(qa_flag) & d1cr_temp > -100), aes(date, d1cr_temp), color = "white", alpha = 0.8) +
#   geom_line(data = subset(qc_d1cr), aes(date, qa_temp), color = "blue", alpha = 0.5) +
#   geom_point(data = subset(qc_sdlcr, !is.na(qa_flag) & cr_temp > -100), aes(date, cr_temp), color = "white", alpha = 0.8) +
#   geom_line(data = subset(qc_sdlcr), aes(date, qa_temp), color = "orchid", alpha = 0.5) +
#   facet_grid(met~local_site) +
#   theme_dark()
# i'm ok with these points being dropped (using qa_temp in these cases)
# okay to use adjusted chart data also (for artificial drops)

# use ctw qc'd sdl from renewal, and be sure to NA anything where !is.na(flag) [previously infilled]
# use 2011 onwards c1 and d1 from ctw, otherwise what's in charttemp
charttemp_c1d1 <- subset(charttemp, grepl("c1|d1", local_site) & yr < 2011 | yr > 2018) %>%
  # assign raw to measurement if tk predicted is NA (applies to 2019 onwards)
  mutate(measurement = ifelse(is.na(tk_predicted), raw, measurement))


# prep qc'd 2011-2018 data
c1d1_qcd_1118 <- rbind(subset(qc_c1, yr > 2010, select = c(LTER_site:met, qa_temp)),
                       subset(qc_d1, yr > 2010, select = c(LTER_site:met, qa_temp))) %>%
  rename(metric = met, measurement = qa_temp) %>%
  # append chart to local_site
  mutate(local_site = paste0(local_site, "_chart"))

#qc_sdl has some weird things in june 2000 20s with date overlaps + qa flags (flatline), 
# didn't join correctly
# remake with rawtemp but add qa flags in
qcsdl_flags <- subset(qc_sdl, !is.na(qa_flag), select = c(date, met, qa_flag)) %>%
  distinct()
sdlchart_prep <- subset(charttemp, local_site == "sdl_chart") %>%
  left_join(qcsdl_flags, by = c("date", "metric" = "met"))
sdlchart_prep$measurement[!is.na(sdlchart_prep$qa_flag)] <- NA

charttemp_prep <- c1d1_qcd_1118 %>%
  rbind(sdlchart_prep[names(.)]) %>%
  rbind(charttemp_c1d1[names(.)]) %>%
  arrange(local_site, date, metric) %>% 
  distinct()

# prep logger dat
logger_prep <- rbind(subset(qc_c1cr, select = c(LTER_site:met, qa_temp, qa_flag)),
                     subset(qc_d1cr, select = c(LTER_site:met, qa_temp, qa_flag)),
                     subset(qc_sdlcr, select = c(LTER_site:met, qa_temp, qa_flag))) %>%
  rename(metric = met) %>%
  unite(local_site, local_site, logger, sep = "_")
# hmps at d1, c1 and sdl after these

# pull gl4 and hmps from logtemp (needs qc) -- and also d1_cr1000 for the year 2019 (still ran)
hmp_prep <- subset(logtemp, grepl("hmp", local_site) | (yr > 2018 & grepl("d1_cr1000$", local_site))) 
gl4_prep <- subset(logtemp, grepl("gl4", station_name))

# -- QUICK QC ALL DATA ----
# remove min/max outliers in all

# in hmps and recent chart data (2019-2021): 
# 1) check sd deparature for daily measurements and daily delta (t-t1) WITHIN SITE
# 2) check sd departures for comparative sites

# ggplot(hmp_prep, aes(mon, measurement, group = mon)) +
#   geom_boxplot() +
#   geom_jitter(aes(col = factor(yr)), alpha = 0.5) +
#   facet_wrap(~local_site) # d1 hmp2 in march looks suss; sdl hmp3 stops june 2021
# 
# ggplot(gl4_prep, aes(mon, measurement, group = mon)) +
#   geom_boxplot() +
#   geom_jitter(aes(col = factor(yr)), alpha = 0.5)


hmp_dailysds <- arrange(hmp_prep, date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(date, station_name) %>%
  # Normalizing the Temp Values and their Lagged differences within sites
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(date, metric) %>%
  # Normalizing the Temp Values and their Lagged differences across sites
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  # Computing Ranks for all (MM unsure why)
  mutate(ws_deltarank = rank(-abs(ws_scaledelta)),
         ws_temprank = rank(-abs(ws_scaletemp)),
         xs_deltarank = rank(-abs(xs_scaledelta)),
         xs_temprank = rank(-abs(xs_scaletemp))
  ) %>%
  # see if can check max/min here
  group_by(date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] <=
           measurement[metric == "airtemp_min"],
         flag_avg = measurement[metric == "airtemp_avg"] <=
           measurement[metric == "airtemp_min"] | measurement[metric == "airtemp_avg"]
         >= measurement[metric == "airtemp_max"])
# Flagging values with a ws or xs scaled temp or lagged diffs greater than +/-2SD
hmp_dailysds$flag_ws <- apply(hmp_dailysds[grep("ws_sc", names(hmp_dailysds))],
                              MARGIN = 1, function(x) sum(abs(x) >= 2) == 2)
hmp_dailysds$flag_xs <- apply(hmp_dailysds[grep("xs_sc", names(hmp_dailysds))],
                              MARGIN = 1, function(x) sum(abs(x) >= 2) == 2)


# ggplot(hmp_dailysds, aes(date, measurement, col = metric)) +
#   geom_line(alpha = 0.3) +
#   geom_point(data = subset(hmp_dailysds, xs_deltarank <= 15), alpha = 0.9) +
#   geom_point(data = subset(hmp_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
#   geom_point(data = subset(hmp_dailysds, ws_temprank <= 10), pch = 4, alpha = 0.9) +
#   geom_point(data = subset(hmp_dailysds, xs_temprank <= 10), pch = 2, alpha = 0.9) +
#   geom_point(data = subset(hmp_dailysds, flag_xs), pch = 1,alpha = 0.9) +
#   geom_point(data = subset(hmp_dailysds, flag_max), pch = 4,alpha = 0.9) +
#   geom_point(data = subset(hmp_dailysds, flag_avg), pch = 4,alpha = 0.9) +
#   facet_wrap(~local_site)

hmp_dailysds$qcflag <- FALSE
hmp_dailysds$qcflag[hmp_dailysds$xs_deltarank <= 15] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$ws_deltarank <= 15] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$xs_temprank <= 10] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$ws_temprank <= 10] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$flag_xs] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$flag_ws] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$flag_max] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$flag_avg] <- TRUE


# -- screen chart data 2019-2021 same way -----
charttemp_dailysds <- subset(charttemp_prep, yr >= 1980) %>%
  arrange(date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(mon, local_site) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(mon, metric) %>%
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  mutate(ws_deltarank = rank(-abs(ws_scaledelta), ties.method = "first"),
         ws_temprank = rank(-abs(ws_scaletemp), ties.method = "first"),
         xs_deltarank = rank(-abs(xs_scaledelta), ties.method = "first"),
         xs_temprank = rank(-abs(xs_scaletemp), ties.method = "first")
  ) %>%
  # see if can check max/min here
  group_by(date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] <= measurement[metric == "airtemp_min"])
# make same flags
charttemp_dailysds$flag_ws <- apply(charttemp_dailysds[grep("ws_sc", names(charttemp_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 3) == 2)
charttemp_dailysds$flag_xs <- apply(charttemp_dailysds[grep("xs_sc", names(charttemp_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 3) == 2)


# ggplot(charttemp_dailysds, aes(date, measurement, col = local_site)) +
#   geom_line(alpha = 0.3) +
#   geom_point(data = subset(charttemp_dailysds, xs_deltarank <= 15), alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, flag_ws), pch = 1, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, flag_xs), pch = 6, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, flag_max), pch = 6, alpha = 0.9) +
#   facet_wrap(~metric, nrow = 2)

# just for logger regressions, remove these points to be more cautious (some are likely real, e.g., 1980s cold snap)
charttemp_dailysds$qcflag <- FALSE
charttemp_dailysds$qcflag[charttemp_dailysds$xs_deltarank <= 15] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$ws_deltarank <= 15] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$xs_temprank <= 15] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$ws_temprank <= 15] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$flag_xs] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$flag_ws] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$flag_max] <- TRUE
# do postcheck to see
charttemp_dailysds <- group_by(charttemp_dailysds, date) %>%
  mutate(postcheck = sum(qcflag),
         stations = length(unique(local_site[qcflag])),
         obs = length(qcflag)) %>%
  ungroup()
# rules: 
# if postcheck == 2 but stations == 1 (only 1 station bonked and both metrics bonked, NA -- these are usually tmin == tmax days)
# if postcheck == 1 & abs(xsdelta) >= 4
charttemp_dailysds$qcflag2 <- FALSE
charttemp_dailysds$qcflag2[charttemp_dailysds$qcflag & charttemp_dailysds$postcheck == 2 & charttemp_dailysds$stations == 1] <- TRUE
charttemp_dailysds$qcflag2[charttemp_dailysds$qcflag & charttemp_dailysds$postcheck == 1 & abs(charttemp_dailysds$xs_scaledelta) >= 4] <- TRUE

# review what gets flagged
# ggplot(charttemp_dailysds, aes(date, measurement, col = local_site)) +
#   geom_line(alpha = 0.3) +
#   geom_point(data = subset(charttemp_dailysds, xs_deltarank <= 15), alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, flag_ws), pch = 1, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, flag_xs), pch = 6, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, flag_max), pch = 6, alpha = 0.9) +
#   geom_point(data = subset(charttemp_dailysds, qcflag), col = "red") +
#   geom_point(data = subset(charttemp_dailysds, qcflag2), col = "black") +
#   facet_wrap(~metric, nrow = 2)

# except leave 1984 and 1985 at sdl and d1 and 2011 at sdl and c1 alone (manual review)
charttemp_dailysds$qcflag2[charttemp_dailysds$qcflag & charttemp_dailysds$mon %in% c(1,2) & charttemp_dailysds$yr %in% c(1984, 1985, 2011)] <- FALSE


# -- quick qc snotel -----
snotel_dailysds <- subset(snotel) %>%
  # add absolute lim flags
  mutate(limit_flag = measurement < -50 | measurement > 40)
# manually remove tmax for niwot and snotel (manual review via iteration)
snotel_dailysds$sensor_fail <- FALSE
snotel_dailysds$sensor_fail[grepl("Niw", snotel_dailysds$local_site) & snotel_dailysds$date %in% seq.Date(as.Date("2005-06-01"), as.Date("2007-02-01"), 1) & snotel_dailysds$metric == "airtemp_max"] <- TRUE 
snotel_dailysds$sensor_fail[grepl("Univer", snotel_dailysds$local_site) & snotel_dailysds$date %in% seq.Date(as.Date("2010-05-01"), as.Date("2011-09-01"), 1) & snotel_dailysds$metric == "airtemp_max"] <- TRUE 
# resume quick qc
snotel_dailysds <- mutate(snotel_dailysds, measurement = ifelse(limit_flag | sensor_fail, NA, measurement)) %>%
  arrange(date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(mon, local_site) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(mon, metric) %>%
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  mutate(ws_deltarank = rank(-abs(ws_scaledelta)),
         ws_temprank = rank(-abs(ws_scaletemp)),
         xs_deltarank = rank(-abs(xs_scaledelta)),
         xs_temprank = rank(-abs(xs_scaletemp))
  ) %>%
  # see if can check max/min here
  group_by(date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] <= measurement[metric == "airtemp_min"],
         flag_avg = measurement[metric == "airtemp_avg"] <= measurement[metric == "airtemp_min"] | measurement[metric == "airtemp_avg"] >= measurement[metric == "airtemp_max"])
# make same flags
snotel_dailysds$flag_ws <- apply(snotel_dailysds[grep("ws_sc", names(snotel_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 3.5) == 2)
snotel_dailysds$flag_xs <- apply(snotel_dailysds[grep("xs_sc", names(snotel_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 3.5) == 2)
snotel_dailysds$flag_temp <- apply(snotel_dailysds[grep("scaletemp", names(snotel_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 3.5) == 2)

# 
# ggplot(snotel_dailysds, aes(date, measurement, col = local_site)) +
#   geom_line(alpha = 0.3) +
#   geom_point(data = subset(snotel_dailysds, xs_deltarank <= 15), alpha = 0.9) +
#   geom_point(data = subset(snotel_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
#   geom_point(data = subset(snotel_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
#   geom_point(data = subset(snotel_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
#   geom_point(data = subset(snotel_dailysds, flag_ws), pch = 1, alpha = 0.9) +
#   geom_point(data = subset(snotel_dailysds, flag_xs), pch = 6, alpha = 0.9) +
#   geom_point(data = subset(snotel_dailysds, flag_temp), pch = 13, alpha = 0.9) +
#   geom_point(data = subset(snotel_dailysds, flag_max), pch = 4, alpha = 0.9) +
#   geom_point(data = subset(snotel_dailysds, flag_avg), pch = 4, alpha = 0.9) +
#   facet_wrap(~metric, nrow = length(unique(snotel_dailysds$metric)))

# seems reasonable for a quick flagging
# just for logger regressions, remove these points to be more cautious (some are likely real, e.g., 1980s cold snap)
snotel_dailysds$qcflag <- FALSE
snotel_dailysds$qcflag[snotel_dailysds$xs_deltarank <= 15] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$ws_deltarank <= 15] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$xs_temprank <= 15] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$ws_temprank <= 15] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$flag_xs] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$flag_ws] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$flag_temp] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$flag_max] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$flag_avg] <- TRUE


# see if it takes care of high periods for niwot and univ camp
# ggplot(subset(snotel_dailysds, !qcflag), aes(date, measurement, col = local_site)) +
#   geom_point(alpha = 0.3) +
#   facet_grid(local_site~metric)
# 
# ggplot(subset(snotel_dailysds, !qcflag & grepl("Univ",local_site)), aes(date, measurement, col = measurement > 27)) +
#   geom_point(alpha = 0.3) +
#   facet_grid(metric~local_site, scales = "free_x")
# remove niwot tmax july 2005 through all of feb 2007 (manual review)
# uc: tmax errors may 2010 through sep 1 2011
snotel_dailysds$qcflag[grepl("Uni", snotel_dailysds$local_site) & snotel_dailysds$measurement > 27 & !is.na(snotel_dailysds$measurement)] <- TRUE


# -- quick qc ghcnd -----
ghcnd_dailysds <- subset(ghcnd, metric != "TOBS") %>%
  # add absolute lim flags
  mutate(limit_flag = measurement < -60 | measurement > 50,
         measurement = ifelse(limit_flag, NA, measurement)) %>%
  arrange(date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(mon, local_site) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(mon, metric) %>%
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  mutate(ws_deltarank = rank(-abs(ws_scaledelta)),
         ws_temprank = rank(-abs(ws_scaletemp)),
         xs_deltarank = rank(-abs(xs_scaledelta)),
         xs_temprank = rank(-abs(xs_scaletemp))
  ) %>%
  # see if can check max/min here
  group_by(date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] <= measurement[metric == "airtemp_min"])
# make same flags
ghcnd_dailysds$flag_ws <- apply(ghcnd_dailysds[grep("ws_sc", names(ghcnd_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 5) == 2)
ghcnd_dailysds$flag_xs <- apply(ghcnd_dailysds[grep("xs_sc", names(ghcnd_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 5) == 2)
ghcnd_dailysds$flag_temp <- apply(ghcnd_dailysds[grep("scaletemp", names(ghcnd_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 5) == 2)


# ggplot(ghcnd_dailysds, aes(date, measurement, col = metric)) +
#   geom_line(alpha = 0.3) +
#   geom_point(data = subset(ghcnd_dailysds, xs_deltarank <= 15), alpha = 0.9) +
#   geom_point(data = subset(ghcnd_dailysds, ws_deltarank <= 10), pch = 8, alpha = 0.9) +
#   #geom_point(data = subset(ghcnd_dailysds, ws_temprank <= 10), pch = 4, alpha = 0.9) +
#   geom_point(data = subset(ghcnd_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
#   geom_point(data = subset(ghcnd_dailysds, flag_ws), pch = 1, alpha = 0.9) +
#   geom_point(data = subset(ghcnd_dailysds, flag_xs), pch = 6, alpha = 0.9) +
#   geom_point(data = subset(ghcnd_dailysds, flag_temp), pch = 13, alpha = 0.9) +
#   facet_wrap(~local_site)
# let 681, 183, 759 keep their cold temps (no flag by ws_rank)

ghcnd_dailysds$qcflag <- FALSE
ghcnd_dailysds$qcflag[ghcnd_dailysds$xs_deltarank <= 15] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$ws_deltarank <= 10] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$xs_temprank <= 15] <- TRUE
#ghcnd_dailysds$qcflag[ghcnd_dailysds$ws_temprank <= 10] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$flag_xs] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$flag_ws] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$flag_temp] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$flag_max] <- TRUE


# -- quick qc ameriflux + gl4 logger -----
amerigl4 <- ameriflux[names(ameriflux) %in% names(gl4_prep)] %>%
  rbind(gl4_prep[names(.)])
amerigl4_dailysds <- subset(amerigl4) %>%
  arrange(date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(mon, local_site) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(mon, metric) %>%
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  mutate(ws_deltarank = rank(-abs(ws_scaledelta)),
         ws_temprank = rank(-abs(ws_scaletemp)),
         xs_deltarank = rank(-abs(xs_scaledelta)),
         xs_temprank = rank(-abs(xs_scaletemp))
  ) %>%
  # see if can check max/min here
  group_by(date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] <= measurement[metric == "airtemp_min"],
         flag_avg = measurement[metric == "airtemp_avg"] <= measurement[metric == "airtemp_min"] | measurement[metric == "airtemp_avg"] >= measurement[metric == "airtemp_max"])

# make same flags
amerigl4_dailysds$flag_ws <- apply(amerigl4_dailysds[grep("ws_sc", names(amerigl4_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 4) == 2)
amerigl4_dailysds$flag_xs <- apply(amerigl4_dailysds[grep("xs_sc", names(amerigl4_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 4) == 2)


# ggplot(amerigl4_dailysds, aes(date, measurement, col = local_site)) +
#   geom_line(alpha = 0.3) +
#   geom_point(data = subset(amerigl4_dailysds, xs_deltarank <= 15), alpha = 0.9) +
#   geom_point(data = subset(amerigl4_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
#   geom_point(data = subset(amerigl4_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
#   geom_point(data = subset(amerigl4_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
#   geom_point(data = subset(amerigl4_dailysds, flag_ws), pch = 1, alpha = 0.9) +
#   geom_point(data = subset(amerigl4_dailysds, flag_xs), pch = 6, alpha = 0.9) +
#   geom_point(data = subset(amerigl4_dailysds, flag_max), pch = 3, alpha = 0.9) +
#   geom_point(data = subset(amerigl4_dailysds, flag_avg), pch = 5, alpha = 0.9) +
#   facet_wrap(~metric, nrow = length(unique(amerigl4_dailysds$metric)))

# flag first, and then undo flag by # of flags per date (e.g., to allow cold period in winter 2011)
amerigl4_dailysds$qcflag <- FALSE
amerigl4_dailysds$qcflag[amerigl4_dailysds$xs_deltarank <= 15] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$ws_deltarank <= 15] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$xs_temprank <= 15] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$ws_temprank <= 15] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$flag_xs] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$flag_ws] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$flag_max] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$flag_avg] <- TRUE

# group_by date and count flags
amerigl4_dailysds <- group_by(amerigl4_dailysds, date) %>%
  mutate(postcheck = sum(qcflag)) %>%
  ungroup()

# ggplot(amerigl4_dailysds, aes(date, measurement, col = local_site)) +
#   geom_line(alpha = 0.3) +
#   geom_point(data = subset(amerigl4_dailysds, qcflag), alpha = 0.9) +
#   geom_point(data = subset(amerigl4_dailysds, postcheck > 1), col = "black", alpha = 0.9) +
#   facet_wrap(~metric, nrow = 2)

# looks okay
amerigl4_dailysds$qcflag[amerigl4_dailysds$postcheck > 1] <- FALSE
# manual review -- forest site in oct 2019 was cold, don't flag.. leave all usnr1 be since qaqc'd and neg temp at tvan ok
amerigl4_dailysds$qcflag[amerigl4_dailysds$measurement < -20 & amerigl4_dailysds$qcflag] <- FALSE


# -- apply QC flags ----
charttemp_prep <- left_join(charttemp_prep, charttemp_dailysds[c("date", "local_site", "metric", "qcflag2")])
charttemp_prep$measurement[charttemp_prep$qcflag2 & !is.na(charttemp_prep$qcflag2)] <- NA
charttemp_prep <- rename(charttemp_prep, qcflag = qcflag2)

hmp_prep <- left_join(hmp_prep, hmp_dailysds[c("date", "local_site", "metric", "qcflag")])
hmp_prep$measurement[hmp_prep$qcflag & !is.na(hmp_prep$qcflag)] <- NA

snotel <- left_join(snotel, snotel_dailysds[c("date", "local_site", "metric", "qcflag")])
snotel$measurement[snotel$qcflag & !is.na(snotel$qcflag)] <- NA

ghcnd <- left_join(ghcnd, ghcnd_dailysds[c("date", "local_site", "metric", "qcflag")])
ghcnd$measurement[ghcnd$qcflag & !is.na(ghcnd$qcflag)] <- NA

ameriflux <- left_join(ameriflux, amerigl4_dailysds[c("date", "local_site", "metric", "qcflag")])
amerigl4$measurement[amerigl4$qcflag & !is.na(amerigl4$qcflag)] <- NA

gl4_prep <- left_join(gl4_prep, amerigl4_dailysds[c("date", "local_site", "metric", "qcflag")])
gl4_prep$measurement[gl4_prep$qcflag & !is.na(gl4_prep$qcflag)] <- NA

# add quick-qc'd gl4 and hmps to previously qc'd sdl, c1, and d1
hmpgl4d1cr_qc <- rbind(hmp_prep, gl4_prep)
logtemp_qc <- subset(logtemp, !local_site %in% unique(logtemp$local_site[grepl("hmp|gl4", logtemp$local_site)])) %>%
  # but remove d1_cr1000 2019 since that is in nwtlog_qc
  subset(!(local_site == "d1_cr1000" & yr == 2019)) %>%
  left_join(logger_prep) %>% # i only qc'd max and min :( .. NA the day's avg if one of those is off
  rename(qcflag = qa_flag) %>%
  # need to set measurement to qa_temp if there is a note for min/max
  # and pull out avg temp and NA if either of max or min was changed that day OR shifted (shift avg temp too)
  # note which dates have qcflags from ctw (will either adjust or NA those measurements for tmean)
  group_by(local_site, date) %>%
  mutate(dailyflags = str_flatten(sort(unique(qcflag[!is.na(qcflag)])), collapse = ";"),
         # check to be sure tmax == tmin flag on shift day
         flagconstant = qcflag[metric == "airtemp_max"] == qcflag[metric == "airtemp_min"])

logtemp_qc_avg <- subset(logtemp_qc, metric == "airtemp_avg") %>%
  mutate(shifted = grepl("shifted", dailyflags),
         # make dailyflag NA if blank
         dailyflags = ifelse(dailyflags == "", NA, dailyflags),
         # start with qa_temp == measurement
         qa_temp = measurement,
         # and then if it shifted is F it means it has a flag but it's not for shifting backwards
         qa_temp = ifelse(!shifted & !is.na(dailyflags), NA, measurement),
         # NA the value that's shifted where there was a sensor break
         qa_temp = ifelse(shifted & !flagconstant, NA , measurement))

# id which records to shift and assign 1 day prior as date
shifttemps <- subset(logtemp_qc_avg, shifted, select = c(date, local_site, metric, qa_temp)) %>%
  mutate(new_date = date-1) %>%
  subset(select = -date) %>%
  rename(shifttemp = qa_temp, date = new_date)
# join back shifted temps
logtemp_qc_avg <- left_join(logtemp_qc_avg, shifttemps)
logtemp_qc_avg$qa_temp[logtemp_qc_avg$shifted] <- logtemp_qc_avg$shifttemp[logtemp_qc_avg$shifted]
# assign qcflag
logtemp_qc_avg$qcflag <- with(logtemp_qc_avg, ifelse(shifted | grepl("null data", dailyflags, ignore.case = T), dailyflags, NA))
logtemp_qc_avg$qcflag[is.na(logtemp_qc_avg$qcflag) & !is.na(logtemp_qc_avg$dailyflags)] <- "daily average temp removed for tmin or tmax failing qc check"
# clean up flag for shifted data points that also failed qc checks
unique(logtemp_qc_avg$qcflag)
# sub out high value (sensor break) with tmean removed
logtemp_qc_avg$qcflag[grepl("high value", logtemp_qc_avg$qcflag)] <- gsub("high value.*;", "daily average temp removed for tmin or tmax failing qc check;", logtemp_qc_avg$qcflag[grepl("high value", logtemp_qc_avg$qcflag)])
# NA tmean that has non shift flag
logtemp_qc_avg$qa_temp[grepl("removed", logtemp_qc_avg$qcflag) & !is.na(logtemp_qc_avg$qcflag)] <- NA


# join tmeans to tmins and maxes
logtemp_qc <- subset(logtemp_qc, metric != "airtemp_avg", select = -c(flagconstant, dailyflags)) %>%
  rbind(logtemp_qc_avg[names(.)]) %>%
  arrange(local_site, date)

# plot to be sure all temps shifted as expected
# ggplot(subset(logtemp_qc, grepl("shift", qcflag)), aes(date, qa_temp)) +
#   geom_line() +
#   geom_line(data = subset(logtemp_qc, grepl("shift", qcflag)), aes(date, raw), col= "blue", lwd = 1, alpha = 0.8) +
#   geom_line(data = subset(logtemp_qc, grepl("shift", qcflag)), aes(date, measurement), col= "red", alpha = 0.8) +
#   facet_grid(metric~local_site, scale = "free_x") # looks good

# be sure (excluding shifted temps) # non-NA data points == qa_temp
summary(with(logtemp_qc, measurement[!grepl("shift", qcflag)]) == with(logtemp_qc, qa_temp[!grepl("shift", qcflag)])) # good (NAs ok)
# assign qa_temp to measurement col
logtemp_qc$old_measurement <- logtemp_qc$measurement
logtemp_qc$measurement <- logtemp_qc$qa_temp

# --- Stacking Data ------
# put it all together:
# previously qc'd data, d1_cr1000 2019 + hmps, gl4
allnwtlog_qc <- rbind(hmpgl4d1cr_qc, logtemp_qc[names(hmpgl4d1cr_qc)]) %>%
  arrange(local_site, date, metric) %>%
  distinct() %>%
  data.frame()
str(allnwtlog_qc)

# be sure all data matches initial read-in data (same nrows)
nrow(allnwtlog_qc) == nrow(logtemp) # ok

# plot to see how it looks
# ggplot(allnwtlog_qc, aes(date, measurement, col = metric)) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(~local_site, scales = "free") #d1_cr21x has issues w avg
# add check for flagmax and flagmin
allnwtlog_qc <- group_by(allnwtlog_qc, date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] <= measurement[metric == "airtemp_min"],
         flag_avg = measurement[metric == "airtemp_avg"] <= measurement[metric == "airtemp_min"] | measurement[metric == "airtemp_avg"] >= measurement[metric == "airtemp_max"]) %>%
  ungroup()

# ggplot(allnwtlog_qc, aes(date, measurement, col = metric)) +
#   geom_line(alpha = 0.5) +
#   geom_point(data = subset(allnwtlog_qc, flag_max | flag_avg)) +
#   facet_wrap(~local_site, scales = "free")
# 
# # focus on d1_cr21x and c1_cr23x
# ggplot(subset(allnwtlog_qc, grepl("d1_cr21x|c1_cr23x", local_site)), aes(date, measurement, col = metric)) +
#   geom_line(alpha = 0.5) +
#   #geom_smooth(alpha = 0.5, method = "loess") +
#   geom_point(data = subset(allnwtlog_qc, grepl("d1_cr21x|c1_cr23x", local_site) & (flag_max | flag_avg))) +
#   facet_wrap(~yr +local_site, scales = "free") +
#   theme(legend.position = "none") # 1992-1999 are weird years for tmean
# 
# # look at all loggers in that period
# ggplot(subset(allnwtlog_qc, grepl("cr21x", local_site) & yr %in% c(1992:1999)), aes(date, measurement, col = metric)) +
#   geom_line(alpha = 0.5) +
#   #geom_smooth(alpha = 0.5, method = "loess") +
#   #geom_point(data = subset(allnwtlog_qc, grepl("d1_cr21x|c1_cr23x", local_site) & (flag_max | flag_avg))) +
#   facet_wrap(~paste(yr,local_site), scales = "free_x", nrow = 8) +
#   theme(legend.position = "none") # 1992-1999 are weird years for tmean
# re-run quick-qc checks. wonky tmean values seem to be on days where one of the extremes is missing


#TODO write a test to make sure lag date is always one. If lag date not 1, lag test is bunk...
allnwtlog_qc_dailysds <- allnwtlog_qc %>%
  group_by(metric, local_site) %>% #Group by everything and then arrange(date)?
  # arrange(date) %>% #TODO add these anywhere lag delta testing (grep lag in script)
  #want to arrange by every possible category (that is not in groupby and then date.)
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(mon, station_name) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(mon, metric) %>%
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  mutate(ws_deltarank = rank(-abs(ws_scaledelta), ties.method = "first"),
         ws_temprank = rank(-abs(ws_scaletemp)),
         xs_deltarank = rank(-abs(xs_scaledelta)),
         xs_temprank = rank(-abs(xs_scaletemp))
  ) %>%
  # add check for both extremes missing but avg present
  group_by(date, local_site) %>%
  mutate(flag_avgpresent = all(is.na(measurement[metric != "airtemp_avg"])) &
           !is.na(measurement[metric == "airtemp_avg"])) %>%
  ungroup()

# plot rankings
# ggplot(allnwtlog_qc_dailysds, aes(date, measurement, col = metric)) +
#   geom_line(alpha = 0.3) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, xs_deltarank <= 15), alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, flag_avgpresent), pch = 1, alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, flag_avg), pch = 6, alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, flag_max), pch = 6, alpha = 0.9) +
#   facet_wrap(~local_site, scales = "free") +
#   theme(legend.position = "none")


# just 21x and 23x periods
# ggplot(subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site)), aes(date, measurement, col = metric)) +
#   geom_line(alpha = 0.3) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & xs_deltarank <= 20), alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & ws_deltarank <= 20), pch = 8, alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & ws_temprank <= 20), pch = 4, alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & xs_temprank <= 20), pch = 2, alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & flag_avgpresent), pch = 1, alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & flag_avg), pch = 6, alpha = 0.9) +
#   geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & flag_max), pch = 6, alpha = 0.9) +
#   facet_wrap(~local_site, scales = "free_x", nrow = 3) +
#   theme(legend.position = "none")

# flag first, and then undo flag by # of flags per date (e.g., to allow cold period in winter 2011)
allnwtlog_qc_dailysds$qcflag2 <- FALSE
allnwtlog_qc_dailysds$qcflag2[allnwtlog_qc_dailysds$xs_deltarank <= 15] <- TRUE
allnwtlog_qc_dailysds$qcflag2[allnwtlog_qc_dailysds$ws_deltarank <= 15] <- TRUE
allnwtlog_qc_dailysds$qcflag2[allnwtlog_qc_dailysds$xs_temprank <= 15] <- TRUE
allnwtlog_qc_dailysds$qcflag2[allnwtlog_qc_dailysds$ws_temprank <= 15] <- TRUE
allnwtlog_qc_dailysds$qcflag2[allnwtlog_qc_dailysds$flag_xs] <- TRUE
allnwtlog_qc_dailysds$qcflag2[allnwtlog_qc_dailysds$flag_ws] <- TRUE
allnwtlog_qc_dailysds$qcflag2[allnwtlog_qc_dailysds$flag_max] <- TRUE
allnwtlog_qc_dailysds$qcflag2[allnwtlog_qc_dailysds$flag_avg] <- TRUE

# group_by date and count flags
allnwtlog_qc_dailysds <- group_by(allnwtlog_qc_dailysds, date) %>%
  mutate(postcheck = sum(qcflag2)) %>%
  ungroup()

# # plot all sensors again
ggplot(allnwtlog_qc_dailysds, aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(allnwtlog_qc_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, flag_avgpresent), pch = 1, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, flag_avg), pch = 6, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, flag_max), pch = 6, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, postcheck > 1), col = "black", alpha = 0.9) +
  facet_wrap(~local_site, scales = "free") +
  theme(legend.position = "none")

# if postcheck > 1 ignore, otherwise just apply flags to 21x and 23x era
allnwtlog_qc_dailysds$qcflag2[allnwtlog_qc_dailysds$postcheck > 1] <- FALSE
allnwtlog_qc_dailysds$qcflag3 <- with(allnwtlog_qc_dailysds, ifelse(grepl("21x|23x", local_site), qcflag2, FALSE))

# also NA any temps for flag_avgpresent
allnwtlog_qc_dailysds$measurement[allnwtlog_qc_dailysds$flag_avgpresent]

# join qcflag3 and flag_avgpresent to dat
allnwtlog_qc <- left_join(allnwtlog_qc, allnwtlog_qc_dailysds[c("date", "metric", "local_site", "qcflag3", "flag_avgpresent")])

# NA mean temps when tmin and tmax not present
allnwtlog_qc$measurement[allnwtlog_qc$flag_avgpresent & allnwtlog_qc$metric == "airtemp_avg"] <- NA
allnwtlog_qc$qcflag[allnwtlog_qc$flag_avgpresent & allnwtlog_qc$metric == "airtemp_avg"] <- "daily averge temp removed: max and min temp not recorded"

# NA anything that has a qcflag3
View(subset(allnwtlog_qc, qcflag3))
allnwtlog_qc$measurement[allnwtlog_qc$qcflag3] <- NA
allnwtlog_qc$qcflag[allnwtlog_qc$qcflag3] <- "value removed for within-site or spatial coherency deviation"
allnwtlog_qc$qcflag[grepl("TRUE", allnwtlog_qc$qcflag)] <- "value removed for within-site or spatial coherency deviation"
allnwtlog_qc$qcflag[grepl("FALSE", allnwtlog_qc$qcflag)] <- NA

# one more plot to be sure all good now
# ggplot(allnwtlog_qc, aes(date, measurement, col = metric)) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(~local_site, scales = "free")
# 
# ggplot(allnwtlog_qc, aes(date, measurement, col = logger, lty = metric)) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(~station_name, nrow = 4)
# # remove first gl4 sensor (cr10) from regression consideration bc range looks truncated
# 
# # just c1, d1, and sdl
# ggplot(subset(allnwtlog_qc, !grepl("gl4", station_name)), aes(date, measurement, group = local_site, col = station_name, lty = logger)) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(~metric, scales = "free_y", nrow = 3) +
#   theme(legend.position = "top") # good enough. early d1 hmps still look wonky for tmean

allnwtlog_qc <- subset(allnwtlog_qc, local_site != "gl4_cr10", select = -c(qcflag3, flag_avgpresent))

#Ready for regressions
################################################################################
# Write out stacked and cleaned data!
################################################################################


saveRDS(charttemp_prep, paste0(datpath, "/qc/nwtchartTEMP_ready.rds"))
saveRDS(allnwtlog_qc, paste0(datpath, "/qc/nwtloggerTEMP_ready.rds"))
saveRDS(ghcnd, paste0(datpath, "/qc/ghcndTEMP_ready.rds"))
saveRDS(snotel, paste0(datpath, "/qc/snotelTEMP_ready.rds"))
saveRDS(ameriflux, paste0(datpath, "/qc/amerifluxTEMP_ready.rds"))

