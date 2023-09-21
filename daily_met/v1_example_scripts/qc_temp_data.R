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


# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

source("nwt_climate/R/temp_qc_functions.R")
source("nwt_climate/R/fetch_data_functions.R") # to read in prepped tidy datasets
source("nwt_climate/R/dataviz_functions.R")
source("nwt_climate/R/prep_data_functions.R")

# set path to prepared data
datpath <- "~/Documents/nwt_lter/nwt_climate/data/"
#csvfiles <- list.files(paste0(datpath, "prep"), pattern = "csv", full.names = T)
# list all rds files in prepped data folder (full path directory)
rdsfiles <- list.files(paste0(datpath, "prep"), pattern = "rds", full.names = T)


# read in prepared temp data
charttemp <- get_tidydat("chartTemp", rdsfiles, c("*")) # use asterisk if want all metrics available
nwtlog <- get_tidydat("nwtlog", rdsfiles, "*")
ameriflux <- get_tidydat("ameri", rdsfiles, "airtemp")
snotel <- get_tidydat("sno", rdsfiles, "airtemp")
ghcnd <- get_tidydat("ghc", rdsfiles, c("temp", "TOBS"))

# read in TK d1 and c1 temp
tktemp <- getNWTchartsinfilled(mets = c("temp"))
tktemp <- stackData(tktemp)



# -- SDL FOR MO PNAS ----
# for now, just focusing on sdl dat for Meagan's ms
# she needs 1989 - 2020.. but try just for loggers if possible

# what are data availablity for loggers 1989 onwards?
ggplot(subset(nwtlog, local_site == "sdl"), aes(date, measurement, group = paste(logger, rep))) +
  geom_line() +
  facet_wrap(~metric, nrow = 3)

# look at hmps
ggplot(subset(nwtlog, local_site == "sdl" & yr > 2016), aes(date, measurement, group = paste(logger, rep), col = paste(logger, rep))) +
  geom_line(alpha = 0.5) +
  facet_grid(metric ~ paste(logger, rep))



# goals:
# want each dataset with its list of flags (source flags + additional qc flags), the raw val, and the qc_measurement col that has violators NA'd
# i.e., each dat out is ready to run through infilling in next script
# for hmps, think it makes sense to infill each sensor (3 tot) and then average those. bc otherwise # data points averaged can vary by date

# also be sure to screen for timestamp changes at ghcnd stations, and remove C1 and D1 pre-1960s respective dates (what it was in the ppt workflow)


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
# only keep d1 and c1 1964 onwards (based on previous review during ppt)
#subset(year > 1963)


charttemp_out <- charttemp %>%
  mutate(raw = measurement,
         # if it's been previously infilled, NA (leave questionable flagged vals and will NA if fail other checks)
         measurement = ifelse(is.na(flag) | flag == "q", measurement, NA)) %>%
  # join to charttemp_out and NA anything that was infilled or adjusted
  left_join(tktemp_tidy[c("date", "local_site", "metric", "tk_measurement", "tk_predicted")]) %>%
  mutate(ctw_adjusted = measurement != tk_measurement & !tk_predicted & yr > 2010) %>%
  # drop DTR and tmean bc realize the average in raw is rounded (e.g., in TK dataset it's 13.5 but in raw it's 14, even when math mean is 13.5)
  # recalculate DTR and tmean if it's not a value that's been infilled or adjusted in some way
  subset(!grepl("avg|DTR", metric))
  
# look at values I adjusted to suss whether/how they should be removed from infilling (e.g., may be okay to keep in seasonal infill since drift would equally affect all predictor vals)
ggplot(subset(charttemp_out, yr > 2011), aes(date, measurement, group = local_site)) +
  geom_line() +
  geom_point(data = subset(charttemp_out, yr > 2011 & ctw_adjusted), aes(date, measurement), col = "purple", alpha = 0.5) +
  geom_point(data = subset(charttemp_out, yr > 2011 & ctw_adjusted), aes(date, tk_measurement), col = "chocolate", alpha = 0.15) +
  geom_point(data = subset(charttemp_out, yr > 2011 & tk_predicted), aes(date, tk_measurement), col = "dodgerblue", alpha = 0.15) +
  facet_grid(metric~local_site)
# leave points be for predictions -- if it was an infilled value, remove. if it was adjusted by ctw bc of drift, leave for both types of predictions

# NA infilled vals
charttemp_out <- mutate(charttemp_out, measurement = ifelse(tk_predicted, NA, measurement))
# assign raw if 2019 onwards for d1 or c1, and for sdl [except NA previously infilled values]
charttemp_out$measurement[is.na(charttemp_out$tk_predicted) & is.na(charttemp_out$flag)] <- with(charttemp_out, raw[is.na(tk_predicted) & is.na(flag)])
charttemp_out$station_id <- paste0(charttemp_out$local_site, "_chart")
# to be sure
charttemp_out <- distinct(charttemp_out)


# -- 2. NWT logger dats -----

nwtlog_out <- nwtlog %>%
  mutate(local_site = casefold(local_site), #lowcase all sites
         station_id = ifelse(!is.na(rep), paste(local_site, logger, rep, sep= "_"), paste(local_site, logger, sep = "_")),
         raw = measurement, measurement = ifelse(measurement >= -50 & measurement <= 40, measurement, NA))

ggplot(nwtlog_out, aes(date, measurement, group = station_id)) +
  geom_line() +
  geom_point(data = subset(nwtlog_out, flag == "q"), aes(col = flag)) +
  theme(legend.position = "none") +
  facet_wrap(~station_id, scales = "free_x", nrow = 3) # leave q flags as they

# closer look at flagged period
ggplot(subset(nwtlog_out, yr > 2017), aes(date, measurement, group = station_id)) +
  geom_line() +
  geom_point(data = subset(nwtlog_out, flag == "q" & yr > 2017), aes(col = flag)) +
  theme(legend.position = "none") +
  facet_wrap(~station_id, scales = "free_x") # they look alright for now



# -- REVIEW CANDIDATE REF DATS -----
# want to screen/choose which stations to use, for which periods
# look for statistical breaks and drifts.. but this can be tricky when ppt values are missing

# -- 1. GHCNd -----
# limit other dat by starting date of nwt chart data
ghcnd_out <- subset(ghcnd, yr >= 1980)
# in ghcnd want to check if time of observation influences measurement trend at all (shouldn't for ppt)
sort(unique(paste(ghcnd_out$station_name, ghcnd_out$station_id))) # 12 stations

# check for time change at stations
ghcnd_tobs <- group_by(ghcnd_out, station_name, station_id)  %>%
  mutate(global_last = max(date)) %>%
  #ungroup() %>%
  group_by(station_name, station_id, global_last, time_observed) %>% 
  summarise(nobs = length(measurement),
            switchdate = min(date),
            lastdate = max(date)) %>%
  arrange(station_name, switchdate)
# like ppt, fraser is only station that will need to be split by time change (7 am through jan 12 1995, and then switched to 1600 then to present)


# review vals with q_flags to see how many data points involved
# NA anything that has a qc flag (but keep raw_measurement as in nwt_chart just in case want to run infilling with more values)
# what are the flags again?
sort(unique(ghcnd_out$q))
# "G" = failed gap check (ctw: ?)
# "I" = failed internal consistency
# "N" = failed naught check (ctw: ?)
# "O" = climatological outlier
# "R" = failed lagged range
# "S" = failed spatial consistency
# "T" = failed temporal consistency

ggplot(subset(ghcnd_out, metric == "airtemp_min"), aes(date, measurement)) +
  geom_line(alpha = 0.2) +
  geom_point(data = subset(ghcnd_out, !is.na(q) & metric == "airtemp_min"), aes(col = q)) +
  facet_wrap(~paste(station_id, station_name), scales = "free_x")

ggplot(subset(ghcnd_out, metric == "airtemp_max"), aes(date, measurement)) +
  geom_line(alpha = 0.2) +
  geom_point(data = subset(ghcnd_out, !is.na(q) & grepl("max", metric)), aes(col = q)) +
  facet_wrap(~paste(station_id, station_name), scales = "free_x")

# what does flagged internal look like up close?
ggplot(subset(ghcnd_out, grepl("BER", station_name)), aes(date, measurement)) +
  geom_line(alpha = 0.2) +
  geom_point(data = subset(ghcnd_out, !is.na(q) & grepl("BER", station_name)), aes(col = q)) +
  facet_wrap(~metric, nrow = 3)

# what would data availability be if remove all flagged dats?
ggplot(subset(ghcnd_out, metric == "airtemp_max" & is.na(q) & grepl("FRA", station_name)), aes(date, measurement)) +
  geom_line(alpha = 0.2) +
  geom_point(data = subset(ghcnd_out, !is.na(q) & grepl("max", metric) & grepl("FRA", station_name)), aes(col = q)) +
  facet_wrap(~paste(station_id, station_name), scales = "free_x") # ok with removing q-flagged dat

ghcnd_out$raw <- ghcnd_out$measurement
ghcnd_out$measurement[!is.na(ghcnd_out$q)] <- NA
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


# -- 2. SNOTEL ------
# plot data
ggplot(snotel, aes(date, measurement)) +
  geom_line() +
  facet_grid(metric ~ station_name)


snotel_out <- snotel
snotel_out$local_site <- gsub(" ", "", snotel$station_name)

# temp record for some of these stations began after precip (but still have those starting dates). start each station at when data consistently starts.
for(s in unique(snotel_out$local_site)){
  mindate <- with(snotel_out, min(date[local_site == s & !is.na(measurement)]))
  snotel_out <- snotel_out[!(snotel_out$local_site == s & snotel_out$date < mindate),]
}


# -- 3. AmeriFlux -----
ggplot(ameriflux, aes(date, raw_measurement)) +
  geom_line() +
  facet_grid(metric ~ paste(station_name, rep))


ameriflux_out <- ameriflux
ameriflux_out$local_site <- with(ameriflux_out, ifelse(is.na(rep), station_id, paste0(station_id, rep)))
unique(ameriflux_out$local_site)
ameriflux_out$local_site <- gsub("-", "_", ameriflux_out$local_site)
# assign qc value for measurement at forest, raw for measurement at tvan
ameriflux_out$measurement <- with(ameriflux_out, ifelse(grepl("NR1", local_site), qc_measurement, raw_measurement))

# start each site at its first nonNA date
for(s in unique(ameriflux_out$local_site)){
  mindate <- with(ameriflux_out, min(date[local_site == s & !is.na(measurement)]))
  ameriflux_out <- ameriflux_out[!(ameriflux_out$local_site == s & ameriflux_out$date < mindate)]
}

ameriflux_out <- distinct(ameriflux_out)



# -- GRAB SITE INFO (for site order) -----
# niwot info: lat long pulled from ppt datasets on edi, elev pulled from niwot website (different from what's listed on edi dataset? but only by several m); except c1 coords from b curtis dataset bc is a box on c1 climate
# lat, lon, elev (masl)
nwt_info <- data.frame(rbind(
  c("sdl", 40.05498494, -105.5907045, 3528),
  c("c1", 40.03596085439916, -105.54415060302942, 3022),  # make same as snotel 40.03596085439916, -105.54415060302942 (took from google maps)
  c("d1", 40.05953123, -105.616957, 3734),
  c("gl4",40.055639, -105.617102, 3560) # set it to gl4 inlet (coords listed in edi gl4 climate dataset are off.. closer to caribou ranch)
))

# assign col names and correct col class 
names(nwt_info) <- c("local_site", "latitude", "longitude", "elevation")
nwt_info[,2:4] <- apply(nwt_info[,2:4], 2, as.numeric)
str(nwt_info)

# join chart and logger dats
distinct(charttemp_out[c("local_site", "station_id")]) %>%
  rbind(distinct(nwtlog_out[names(.)]))
nwt_info <- left_join(nwt_info,
                      distinct(charttemp_out[c("local_site", "station_id")]) %>%
                        rbind(distinct(nwtlog_out[names(.)]))) 

nwt_info$station_name <- with(nwt_info, ifelse(grepl("chart", station_id), paste(local_site, "chart"), paste(local_site, "logger")))
# change nwt local_site to station_id so consistent use with other stations
nwt_info$local_site <- nwt_info$station_id
# add data source
nwt_info$source <- "nwt lter"

ghcnd_sites <- distinct(ghcnd_out, local_site, station_id, station_name, latitude, longitude, elevation)
ghcnd_sites$source <- "ghcnd"

ameriflux_sites <- distinct(subset(ameriflux_out, select = names(ghcnd_sites)))
ameriflux_sites$source <- "ameriflux"

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
ggplot(distinct(allsites, local_site, elevation, latitude, longitude), aes(longitude, latitude, fill = elevation)) +
  #geom_point(alpha = 0.5) +
  geom_label(aes(label = local_site), alpha = 0.5) +
  scale_fill_viridis_c()

ggplot(distinct(allsites, local_site, elevation, latitude, longitude), aes(longitude, elevation, fill = elevation)) +
  #geom_point(alpha = 0.5) +
  geom_label(aes(label = local_site), alpha = 0.5, ) +
  scale_fill_viridis_c()




# -- STANDARDIZE COLNAMES -----
# switch local_site to allsites$local site for nwt; preserve orig local site in different col i guess?
charttemp_out$local_site <- charttemp_out$station_id
charttemp_out$station_id <- str_extract(charttemp_out$station_id, "d1|c1|sdl|gl4")
charttemp_out$station_name <- charttemp_out$local_site


nwtlog_out$local_site <- nwtlog_out$station_id
nwtlog_out$station_id <- str_extract(nwtlog_out$station_id, "d1|c1|sdl|gl4")
nwtlog_out$station_name <- paste(nwtlog_out$station_id, "logger")

names(ameriflux_out) <- gsub("raw_measurement", "raw", names(ameriflux_out))


# -- WRITE OUT -----
# write out qc'd ppt for infilling next
saveRDS(charttemp_out, paste0(datpath, "/qc/nwtchartTEMP_qc.rds"))
saveRDS(nwtlog_out, paste0(datpath, "/qc/nwtloggerTEMP_qc.rds"))
saveRDS(ghcnd_out, paste0(datpath, "/qc/ghcndTEMP_qc.rds"))
saveRDS(snotel_out, paste0(datpath, "/qc/snotelTEMP_qc.rds"))
saveRDS(ameriflux_out, paste0(datpath, "/qc/amerifluxTEMP_qc.rds"))
saveRDS(allsites, paste0(datpath, "/qc/siteinfoTEMP_qc.rds"))


