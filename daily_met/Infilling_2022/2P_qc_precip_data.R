#' ---
#' title: Prepare temperature data 
#' author: CTW  
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#' 
#' ### QC checks on precip pre gap-filling
#' 
#' Test report to:
#' 1. Read in NWT and neighbor station temperature datasets
#' 2. Check qualifying days don't overlap a non-NA day for NWT precip
#' 3. Check for breaks in time series
#' 4. Wide-format data for gap filling
#' 
#' All code will be displayed to show procedure and work out bugs


# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

source("daily_met/R/ppt_qaqc_functions.R")
source("daily_met/R/fetch_data_functions.R") # to read in prepped tidy datasets
source("daily_met/R/dataviz_functions.R")
source("daily_met/R/prep_data_functions.R")

# set path to prepared data
datpath <- "~/OneDrive - UCB-O365/nwt_climate/data/"
#csvfiles <- list.files(paste0(datpath, "prep"), pattern = "csv", full.names = T)
# list all rds files in prepped data folder (full path directory)
rdsfiles <- list.files(paste0(datpath, "prep"), pattern = "rds", full.names = T)


# read in prepared precip data
chartppt <- get_tidydat("chartPPT", rdsfiles, "ppt")
c1logppt <- get_tidydat("c1log", rdsfiles, "ppt")
ameriflux <- get_tidydat("ameri", rdsfiles, "ppt")
snotel <- get_tidydat("sno", rdsfiles, "ppt")
ghcnd <- get_tidydat("ghc", rdsfiles, "ppt")

# read in TK d1 and c1 ppt
tkppt <- getNWTchartsinfilled(mets = c("ppt"))
tkppt <- stackData(tkppt)




# -- REVIEW NWT -----
# run chart ppt through initial qc loop
# checks for 1) dates with non-NA values [in ppt or qdays] that fall in a qdays > 1 backfill period
# and 2) NAs non-sensical qdays values (will report what the values were)
# both of these checks add a qc note so can pull up just the records that were flagged
chartppt_out <- qc_qdays(chartppt, "local_site")
# subset which flagged values to visual review
reviewdat <- subset(chartppt_out, grepl("ppt value falls", qdays_qcnote))
# > note to SCE: tried thinking through way to make specifying values to flag in the function, but couldn't figure out how to do it
# > feeding a flagged (subsetted) dataset was how I did visual QA before 

# call visual qc review (function in progress .. ugly plot colors and want flexibility to store subsets of plots in list in case it's too many to review in one panel)
# > also need to remove warning messages? and move legend elsewhere (only 1 common legend)
visual_qcreview(reviewdat, alldat = chartppt_out, groupvar = "local_site")

#' What we're looking for here is if any of the circled values look ok to keep or rather we want to NA them
#' I recommend NAing, because the raw values for ppt and qdays are kept throughout the process. We can then compare infilled value to raw value at the end and choose (e.g., if infilled value not very different from raw, keep raw and adjust qdays value that caused inconsistency)
#' The boxed numbers show the number of qdays for the next available non-NA data point in the time series

# haven't made function to NA qdays violation records.. but this is also in part because the user should review them?
# > note to self: chat with SCE about this

# NA qdays violation records
chartppt_out[grepl("ppt value falls", chartppt_out$qdays_qcnote), c("measurement", "qdays")] <- NA

# also want to NA previously infilled data (methods pre-TK methods) because not as robust
ggplot(subset(chartppt_out, yr > 1979), aes(date, measurement)) +
  geom_line() +
  geom_point(data = subset(chartppt_out, yr > 1979 & !is.na(flag_ppt_tot)), aes(col = factor(flag_ppt_tot))) +
  facet_wrap(~local_site, nrow = 3)
# does qdays need to be NA'd if ppt is NA?
summary(chartppt_out$qdays[is.na(chartppt_out$measurement)])
# what is qdays == 2 when ppt is NA?
View(subset(chartppt_out, is.na(measurement) & !is.na(qdays))) # maybe it's fine. 2-day sequence in mar 2019 that is missing data.. but keep an eye on it when infilling
# NA anything that has been infilled or has a questionable flag
chartppt_out$measurement[!is.na(chartppt_out$flag_ppt_tot)] <- NA

# also remove any raw value adjusted by TK so not considered in infilling regressions
# > also chat w SCE about whether to write function for this
tkadjusted <- subset(tkppt, raw_ppt_tot != precip & raw_qdays == 1)
# what are the years adjusted by station?
with(tkadjusted, lapply(split(year, local_site), unique))
# flag 2 is th qc flag -- check 1970 onwards [when unshielded/shielded should be a non-issue]
with(subset(tkadjusted, year > 1969), lapply(split(flag_2, local_site), unique))
# attach original flag col to see if qdays == 1  raw_values were previously infilled by old methods
tkadjusted <- left_join(mutate(tkadjusted, local_site = casefold(local_site)), subset(chartppt_out, select = c(date, local_site, flag_ppt_tot)))
with(subset(tkadjusted, year > 1965), lapply(split(flag_ppt_tot, local_site), unique)) # all of these values were infilled previously by old methods


# notes from TK draft ppt manuscript/nwt renewal:
# Flag 1:
# A - Data recorded at Target Station (see Source Station field for specific gauge information)
# B - Data infilled using Method 1 with a p value <.05
# C - Data infilled using Method 1 with a p value >.05
# D - Data infilled using Method 2 with a p value <.05
# E - Data infilled using Method 2 with a p value >.05

# Flag 2:
# A - Daily value, not adjusted for any period total
# B - Daily value, adjusted for period total as recorded by Belfort Recording Gauge: Shielded.
# C - Daily value, adjusted for period total as recorded by US Weather Bureau Totalizing Gauge: Unshielded
# D - Daily value, adjusted for period total as recorded by US Weather Bureau Totalizing Gauge: Shield-corrected
# E - Daily value, adjusted for period total as recorded by Other Belfort Recording Gauge dataset - 1960: Different instrument & shield than in 'B'
# F - Daily value, adjusted for period total as recorded by Other Belfort Recording Gauge dataset - 1964: Unshielded-period for gauge in 'B'
# G - Period Total of zero recorded at Target Station, so all days in period given zero value
# *H - Period Total recorded at Target Station divided by number of days in period 
# **I - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by Belfort Recording Gauge: Shielded.
# **J - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by US Weather Bureau Totalizing Gauge: Unshielded
# **K - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by US Weather Bureau Totalizing Gauge: Shield-corrected
# **L - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by Other Belfort Recording Gauge dataset - 1960: Different instrument & shield than in 'B'
# **M - Daily value, entire period filled from same source station with non-zero PPT value during the period that has best pvalue/rsquared, and adjusted for period total as recorded by Other Belfort Recording Gauge dataset - 1964: Unshielded-period for gauge in 'B'
# 
# * occurs only when there are no valid, non-zero data from any of the source stations with which to infill and match the period total
# ** these occur when the "best" infilled values for each day of a period are all zeros even though there is a non-zero total recorded for the period.

# note dates where precip is adjusted in tk dataset, whether regression eq present or not (e.g., values adjusted for instrument change don't have regression eq.)
tkflagged <- mutate(tkppt, adjusted = (precip != raw_ppt_tot) | !is.na(regression_equation) | is.na(raw_ppt_tot), local_site = casefold(local_site)) #%>%
# from looking, D1 shielded station begins consistently in 1968, and at C1 in 1965. Keep data from then on for infill equations.
# what are the different instruments noted at c1 and d1?
with(tkflagged, sort(unique(source_station[grepl("^c1|^d1", source_station, ignore.case = T)])))
# note times that had totalizing or belfort 1960
tkflagged <- subset(tkflagged, select = c(date, year, local_site, precip, raw_ppt_tot, source_station, flag_1, flag_2, adjusted)) %>%
  # note when c1 or d1 had instrument before current belfort shielded
  mutate(unshield = grepl("Totalizing|Belfort 1960", source_station))
# RULES
# > if unshield == TRUE & adjusted == F & precip == 0, allow; otherwise if unshield == T, discard (remove value from infilling)
# > if adjusted in any way, ignore (remove value from infilling)
tkflagged <- tkflagged %>%
    mutate(flag_unshielded = (unshield & adjusted) | (unshield & precip >0),
           # except if value is 0 and flag_2 == G or A & raw is NA allow
           flag_unshielded = ifelse(unshield & adjusted & precip == 0 & is.na(raw_ppt_tot) & flag_2 %in% c("A", "G"), FALSE, flag_unshielded),
           tk_qcnote = ifelse(flag_unshielded, "unshielded instrument period and ppt non-zero or adjusted",
                              ifelse(adjusted, "value adjusted by Kittel et al. method", NA))) %>%
  rename(yr = year)
  

chartppt_out_qc <- left_join(chartppt_out, tkflagged[c("date", "local_site", "precip", "source_station", "tk_qcnote")]) %>%
  data.frame() %>%
  mutate(mismatch = measurement != precip,
    infill_qcnote = ifelse(flag_ppt_tot == "q" & !is.na(flag_ppt_tot), "JM flagged as questionable",
                                ifelse(!is.na(flag_ppt_tot) & local_site == "sdl", "infilled by outdated method", 
                                       ifelse(!is.na(flag_ppt_tot) & !is.na(tk_qcnote), paste("infilled by outdated method,", tk_qcnote), 
                                              ifelse(!is.na(flag_ppt_tot), "flagged in raw, tk did not flag", tk_qcnote))))) %>%
  # if tk did not flag and is c1 or d1, return measurement value (these are all 0 ppt values in 1960s after instrument change)
  mutate(measurement = ifelse(grepl("tk did not flag", infill_qcnote), raw, measurement)) %>%
  # NA anything that is noted as infilled by TK method/has an infill note and qdays == 1 EXCEPT if (measurement == 0 & raw == 0)
  # leave multiday values alone (will have a qa note present) for backfilling (those values don't get used as ref values for infilled) 
  mutate(measurement = ifelse(!is.na(infill_qcnote) &  !grepl("tk did not flag", infill_qcnote) & yr < 2011, NA, measurement)) %>%
  # start data at 1964-10-01 (since that is when d1 starts; takes care of unshielded data at c1)
  # and select only columns of interest for writing out
  subset(date >= min(chartppt$date[chartppt$local_site == "d1"]), select = -c(LTER_site, flag_ppt_tot, precip:mismatch)) %>%
  rename(station_name = local_site)
  
  





# -- REVIEW CANDIDATE REF DATS -----
# want to screen/choose which stations to use, for which periods
# look for statistical breaks and drifts.. but this can be tricky when ppt values are missing

# -- 1. GHCNd -----
# limit other dat by starting date of nwt chart data
ghcnd_out <- subset(ghcnd, date >= min(chartppt_out_qc$date))
# in ghcnd want to check if time of observation influences measurement trend at all (shouldn't for ppt)


# check for time change at stations
ghcnd_tobs <- group_by(ghcnd_out, station_name, station_id)  %>%
  mutate(global_last = max(date)) %>%
  #ungroup() %>%
  group_by(station_name, station_id, global_last, time_observed) %>% 
  summarise(nobs = length(measurement),
            switchdate = min(date),
            lastdate = max(date)) %>%
  arrange(station_name, switchdate)
# fraser and allenspark 2 are the only two stations (in their full time series) where tobs changed in way that might be influential:
# allenspark 7am then afternoon (16:30 and 1700)
# fraser 7am, 16:00, and 23:00

# for oct 1964 onwards, fraser 116 is the only station that meaningfully changed (switched from 7am to 16:30 obs in 1995)
# group station by its observation time for that one. all others vary but typically within 1hr window (some within 2 hr)

# ggplot(subset(ghcnd, grepl("allenspark 2|fraser", station_name, ignore.case = T)), aes(date, log(measurement), col = time_observed, fill = time_observed)) +
ggplot(subset(ghcnd_out, measurement > 0), aes(date, log(measurement), col = time_observed, fill = time_observed)) +  
  geom_point(alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~paste(station_id, station_name), scales = "free")
# take a closer look at fraser 116 to see if the time change is that much of an issue (data values look within same range)
# also allenspark 2SE looks problematic before mid 1970s (few values below log(ppt) == 0, exp -> 1 mm)

#plot just fraser 116
ggplot(subset(ghcnd_out, grepl("3116$", station_id)), aes(date, measurement, col = time_observed, fill = time_observed)) +  
  geom_line(alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~paste(station_id, station_name), scales = "free")

# fraser 116 has a low mean in 1994 (but that may have been a droughtier year? think it was in boulder)
ggplot(subset(ghcnd_out, grepl("116|759$", station_id) & yr %in% 1990:2000), aes(yr, measurement)) +  
  stat_summary(aes(group = yr)) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(1986, 2022, 4)) +
  facet_wrap(~paste(station_id, station_name), scales = "free_x") #idk

# check for potential differences by general period of instrument change
ggplot(subset(ghcnd, measurement > 0 & lubridate::year(date)> 1940), aes(date, log(measurement))) +
  geom_point(aes(col = lubridate::year(date)<1966), alpha = 0.5) +
  geom_smooth(col = "red", fill = "pink") +
  geom_smooth(aes(group = lubridate::year(date)<1966), method = "lm") +
  scale_color_viridis_d(name = "pre 1966") +
  #theme(legend.title = element_blank()) +
  facet_wrap(~station_name, scales = "free")


# NA anything that has a qc flag (but keep raw_measurement as in nwt_chart just in case want to run infilling with more values)
# what are the flags again?
unique(ghcnd_out$q)
# "I" = failed internal consistency
# "L" = failed check on length of multiday period
# "O" = climatological outlier

ggplot(ghcnd_out, aes(date, measurement)) +
  geom_line(alpha = 0.2) +
  geom_point(data = subset(ghcnd_out, !is.na(q)), aes(col = q)) +
  facet_wrap(~paste(station_id, station_name))

# look at winter park during error period
ggplot(subset(ghcnd_out, grepl("winter", station_name, ignore.case = T)), aes(date, measurement)) +
  geom_line(alpha = 0.2) +
  geom_point(data = subset(ghcnd_out, grepl("winter", station_name, ignore.case = T) & !is.na(q)), aes(col = q)) +
  facet_wrap(~paste(station_id, station_name))

ggplot(subset(ghcnd_out, grepl("winter", station_name, ignore.case = T) & yr %in% 2010:2013), aes(date, measurement)) +
  geom_col(alpha = 0.2) +
  geom_point(data = subset(ghcnd_out, grepl("winter", station_name, ignore.case = T) & !is.na(q) & yr %in% 2010:2013), aes(col = q)) +
  facet_wrap(~paste(station_id, station_name)) # seems fine?

ghcnd_out$raw <- ghcnd_out$measurement
ghcnd_out$measurement[!is.na(ghcnd_out$q)] <- NA
ghcnd_out$qc_note <- with(ghcnd_out, ifelse(!is.na(q), "value NAd for q flag", NA))
ghcnd_out$qc_note

# note when obs time switched by more than 6 hours (that should cover shifts of the day?)
# > note to self: build run check function later

switchdate <- with(ghcnd_tobs, max(switchdate[grepl("116$", station_id)]))
# note time switch at fraser
ghcnd_out$qc_note[grepl("116$", ghcnd_out$station_id) & ghcnd_out$date == switchdate] <- paste("Time switch from 0700 to", ghcnd_out$time_observed[grepl("116$", ghcnd_out$station_id) & ghcnd_out$date == switchdate])
ghcnd_out$local_site <- ghcnd_out$station_id
ghcnd_out$local_site[grepl("116$", ghcnd_out$station_id) & ghcnd_out$date < switchdate] <- with(ghcnd_out, paste(unique(station_id[grepl("116$", ghcnd_out$station_id)]), "0700", sep = "_"))
ghcnd_out$local_site[grepl("116$", ghcnd_out$station_id) & ghcnd_out$date >= switchdate] <- with(ghcnd_out, paste(unique(station_id[grepl("116$", ghcnd_out$station_id)]), time_observed[grepl("116$", station_id) & date == switchdate], sep = "_"))


# -- 2. SNOTEL ------
# what are the flags?
with(snotel, lapply(split(qa_flag, station_name), unique))
# u = unknown; r = raw (no human review) .. so use at your own risk

with(snotel, lapply(split(qc_flag, station_name), unique))
# v = valid (huzzah!); e = minor edit for sensor noise (ok)
# only values we'd want to NA are if there were B (back estimates), K (estimate), X (external est), or S (suss) 

# plot data to screen flagged values that are u or r
ggplot(snotel, aes(date, measurement)) +
  #geom_col(aes(fill = metric, group = metric), position = position_dodge(width = 0.1), alpha = 0.5) +
  geom_line(aes(col = metric), alpha = 0.5) +
  geom_point(data = subset(snotel, qa_flag == "R")) +
  facet_wrap(~station_name, scales = "free", nrow = 5)

# high point at lake eldo is the snow adjusted value; ppt (unadjusted is 0..). snow adjusted seems better to use but check diffs
# generally, snow adjusted seems inflated.. sometimes snow adj is 2 or 3x the ppt value
check_snowadj <- subset(snotel, select = c(date:yr, station_name, metric, measurement)) %>%
  spread(metric, measurement) %>%
  mutate(diff_adj_ppt = ppt_tot_snow_adj - ppt_tot,
         snow_adj_pres = (ppt_tot == 0) & ppt_tot_snow_adj > 0)

ggplot(check_snowadj, aes(date, diff_adj_ppt)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(alpha = 0.5) +
  geom_point(data = subset(check_snowadj, snow_adj_pres), col = "slateblue", alpha = 0.5) +
  facet_wrap(~station_name, scales = "free")

# compare niwot and c1: is freq of snow_adj ppt at niwot but not at c1 comparable to within site relationship at niwot?
niwot_c1 <- left_join(subset(check_snowadj, station_name == "Niwot"), subset(chartppt_out_qc, station_name == "c1")) %>%
  mutate(snw_adj_c1 = (measurement == 0 & ppt_tot_snow_adj>0) | (measurement > 0 & ppt_tot_snow_adj == 0),
         snw_c1 = (measurement == 0 & ppt_tot>0) | (measurement > 0 & ppt_tot == 0)) # snow adjusted agrees with c1 more often in non-zero ppt than unadjusted ppt

summary(niwot_c1[c("snow_adj_pres", "snw_adj_c1", "snw_c1")])

# standardize measurements
ggplot(snotel, aes(date, scale(measurement))) +
  #geom_col(aes(fill = metric, group = metric), position = position_dodge(width = 0.1), alpha = 0.5) +
  geom_line(aes(col = metric), alpha = 0.5) +
  geom_point(data = subset(snotel, qa_flag == "R")) +
  facet_wrap(~station_name, scales = "free", nrow = 5) #40 deviations if a big jump, even for ppt

# plot month in question with nwt data to see if there is spike at those places
subset(snotel, metric == "ppt_tot_snow_adj", select = c(date:doy, station_name, metric, measurement)) %>%
  rbind(ghcnd_out[names(.)]) %>%
  rename(local_site = station_name) %>%
  dplyr::bind_rows(chartppt_out_qc) %>%
  subset(yr == 1992 & mon == 3) %>%
  ggplot(aes(date, measurement)) +
  geom_point() +
  facet_wrap(~local_site) # outlier, all other sites in the area have 0 or close to 0 ppt. internet search says a blizzard happened mar 4 not mar 2

# pull snow adjusted ppt, but NA the high value at Eldora
snotel_out <- subset(snotel, grepl("adj", metric)) %>%
  mutate(raw = measurement,
         qc_note = ifelse(grepl("Eldora", station_name) & date == "1992-03-02", "value NAd: outlier, no or little precip recorded all other regional sites that day, 0 ppt recorded for unadjusted snotel value", NA),
         measurement = ifelse(!is.na(qc_note), NA, measurement))



# -- 3. Ameriflux -----
# okay as is since only 1 sensor being considered for ppt
ameriflux_out <- rename(ameriflux, measurement = qc_measurement, raw = raw_measurement) %>%
  # pair rep id with station id
  mutate(local_site = paste0(station_id, rep))


# -- GRAB SITE INFO (for site order) -----
# niwot info: lat long pulled from ppt datasets on edi, elev pulled from niwot website (different from what's listed on edi dataset? but only by several m); except c1 coords from b curtis dataset bc is a box on c1 climate
# lat, lon, elev (masl)
nwt_info <- data.frame(rbind(
  c("sdl", 40.05498494, -105.5907045, 3528),
  c("c1", 40.03596085439916, -105.54415060302942, 3022),  # make same as snotel 40.03596085439916, -105.54415060302942 (took from google maps)
  c("d1", 40.05953123, -105.616957, 3734)
))
# assign col names and correct col class 
names(nwt_info) <- c("local_site", "latitude", "longitude", "elevation")
nwt_info[,2:4] <- apply(nwt_info[,2:4], 2, as.numeric)
str(nwt_info)
nwt_info$station_id <- paste(nwt_info$local_site, "chart")
nwt_info$station_name <- nwt_info$local_site

ghcnd_sites <- distinct(ghcnd_out, local_site, station_id, station_name, latitude, longitude, elevation)
ameriflux_sites <- distinct(subset(ameriflux_out, select = names(ghcnd_sites)))
snotel_sites <- distinct(subset(snotel_out, select = c(station_id, station_name, latitude, longitude, elevation))) %>%
  mutate(local_site = gsub(" ", "", station_name))

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

# map out
ggplot(distinct(allsites, local_site, elevation, latitude, longitude), aes(longitude, latitude, fill = elevation)) +
  #geom_point(alpha = 0.5) +
  geom_label(aes(label = local_site), alpha = 0.5) +
  scale_fill_viridis_c()

ggplot(distinct(allsites, local_site, elevation, latitude, longitude), aes(longitude, elevation, fill = elevation)) +
  #geom_point(alpha = 0.5) +
  geom_label(aes(label = local_site), alpha = 0.5, ) +
  scale_fill_viridis_c()



# -- CHECK SOLO STATIONS WITH PPT ----
# review anything where ppt > 0 recorded for only 1 station out of regional dataset (may happen when only a few stations are operational)
# for what gets flagged, review amount (some ppt reasonable, but relatively large amounts more likely to be suspect)
chartppt_out_compare <- chartppt_out_qc
chartppt_out_compare$measurement[chartppt_out_qc$qdays > 1 & chartppt_out_compare$measurement > 0] <- NA
chartppt_out_compare$local_site <- chartppt_out_compare$station_name

compare <- subset(snotel_out, metric == "ppt_tot_snow_adj", select = c(date:doy, station_name, metric, measurement)) %>%
  rename(local_site = station_name) %>%
  rbind(ghcnd_out[names(.)]) %>%
  rbind(chartppt_out_compare[names(.)]) %>%
  rbind(ameriflux_out[names(.)]) %>%
  group_by(date) %>%
  mutate(pos_ppt = length(local_site[measurement > 0 & !is.na(measurement)]),
         nonNA_nobs = length(local_site[!is.na(measurement)]),
         sdl_pos = ifelse(yr > 1980, measurement[local_site == "sdl"] > 0, NA),
         d1_pos = ifelse(yr > 1964, measurement[local_site == "d1"] > 0, NA),
         uc_pos = ifelse(yr > 1978, measurement[local_site == "University Camp"] > 0, NA),
         c1_pos = ifelse(yr > 1964, measurement[local_site == "c1"] > 0, NA),
         sno_pos = ifelse(yr > 1980, measurement[local_site == "Niwot"] > 0, NA),
         ameri_pos = ifelse(yr > 1998, measurement[grepl("NR1", local_site)] > 0, NA),
         allnwt_0sdl = d1_pos %in% c(NA, T) & uc_pos %in% c(NA, T) & c1_pos %in% c(NA, T) & sno_pos %in% c(NA, T) & ameri_pos %in% c(NA, T) & !sdl_pos) %>%
  ungroup() %>%
  mutate(prop_pos = pos_ppt/nonNA_nobs) %>%
  # standardize diffs when it's the only site
  #group_by(local_site) %>%
  mutate(std_diff = ifelse(measurement > 0 & pos_ppt %in% c(1:3), scale(measurement), NA),
         std_diff_0sdl = ifelse(measurement > 0 & allnwt_0sdl & grepl("d1|c1|Univer", local_site), scale(measurement), NA))

# freq table
table(compare[c("pos_ppt", "nonNA_nobs")])

# note to self: tell SCE to note for d1 and sdl that beginning in 2018 it looks like for first time in time series sub 1mm values were recorded (change in practice, everything else prior was either 0 or 1mm)

ggplot(subset(compare, measurement > 10 & pos_ppt %in% c(1) & prop_pos < 0.15), aes(mon, std_diff, group = mon)) +
  geom_hline(aes(yintercept = 2.5)) +
  geom_boxplot(fill = "transparent", outlier.shape = NA) +
  geom_label(aes(col = yr, label = measurement, size = nonNA_nobs), alpha = 0.6) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_viridis_c(option = "B") +
  facet_grid(pos_ppt~local_site) +  #scales = "free_y"
  theme_bw() # using a 2.5 std thresholds corresponds to about 15mm absolute cutoff, which seems reasonable

ggplot(subset(compare, local_site == "sdl" & measurement > 6 &  pos_ppt %in% c(1:3) & prop_pos < 0.15 & !d1_pos & !c1_pos & !uc_pos & !sno_pos & !ameri_pos), aes(mon, measurement, group = mon)) + #prop_pos < 0.15 
  geom_boxplot(fill = "transparent", outlier.shape = NA) +
  geom_jitter(aes(fill = yr, size = nonNA_nobs), pch = 21, height = 0, alpha = 0.8, width = 0.25) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(breaks = seq(10,100, 10)) +
  scale_fill_viridis_c(option = "B") +
  facet_wrap(~local_site, scales = "free_y") +
  theme_minimal()

ggplot(subset(compare, local_site == "sdl" & measurement > 6 &  pos_ppt %in% c(1:3) & prop_pos < 0.15 & d1_pos %in% c(F, NA) & c1_pos  %in% c(F, NA) & uc_pos %in% c(F, NA) & sno_pos %in% c(F, NA) & ameri_pos %in% c(F, NA)), aes(mon, measurement, group = mon)) + #prop_pos < 0.15 
  geom_boxplot(fill = "transparent", outlier.shape = NA) +
  geom_jitter(aes(fill = yr, size = nonNA_nobs), pch = 21, height = 0, alpha = 0.8, width = 0.25) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(breaks = seq(10,100, 10)) +
  scale_fill_viridis_c(option = "B") +
  facet_wrap(~local_site, scales = "free_y") +
  theme_minimal()

flagsdl1 <- subset(compare, local_site == "sdl" & measurement >= 6.35 &  pos_ppt %in% c(1:3)  & !d1_pos & !c1_pos & !uc_pos & !sno_pos & !ameri_pos)
flagsdl2 <- subset(compare, local_site == "sdl" & measurement >= 10 &  pos_ppt %in% c(1:3) & prop_pos < 0.15 & d1_pos %in% c(F, NA) & c1_pos  %in% c(F, NA) & uc_pos %in% c(F, NA) & sno_pos %in% c(F, NA) & ameri_pos %in% c(F, NA))
test <- anti_join(flagsdl2, flagsdl1)


# what does precip at NWT look like on days where sdl is 0 and all other nwt is + or NA?
# > how much precip should fall at d1 to flag sdl on those days (or if all other nwt sites have precip >>> sdl and d1 is relatively small or missing, should those dates be flagged or no?)

sdl0dates <- with(compare, unique(date[allnwt_0sdl & std_diff_0sdl > 2]))
sdl0dates <- sort(sdl0dates[!is.na(sdl0dates)])
ggplot(subset(compare, date %in% sdl0dates & measurement > 0), aes(local_site, measurement)) +
  geom_hline(aes(yintercept = 15)) +
  geom_col(aes(fill = grepl("c1|d1|Univ|Niw|NR1", local_site))) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  facet_wrap(~date)

# reviewed and looks good:
# rule is: sdl == 0, all nwt sites pos precip or NA & at least one nwt site > 2 zscore AND d1 is > 10ppt or missing (catches 2006-01-19)
flag_sdl0 <- subset(compare, date %in% sdl0dates) %>%
  group_by(date) %>%
  mutate(d1ppt = measurement[local_site == "d1"],
         d1trigger = d1ppt >= 10 | is.na(d1ppt)) %>%
  ungroup() %>%
  subset(d1trigger)

# how often are relatively high ppt values at one site correlated with relatively high ppt at another?
compare <- group_by(compare, local_site) %>%
  mutate(ln_measurement = ifelse(!is.na(measurement), scale(measurement), NA))

test <- subset(compare, select = c(local_site, date, ln_measurement)) %>%
  spread(local_site, ln_measurement)
pairs(test[grepl("c1|d1|sdl|Univ|Niw|NR1|Saw|High", names(test))], lower.panel = panel.smooth)
cor.test( ~ d1 + sdl, data = test)
cor.test( ~ d1 + c1, data = test)
cor.test( ~ d1 + test$`High Lonesome`, data = test)
cor.test( ~ d1 + test$`Lake Eldora`, data = test)
cor.test(~ d1 + test$`US-NR1_1_1_1`, data = test)
cor.test(~ d1 + test$`University Camp`, data = test)
cor.test(~ d1 + USW00094075, data = test) # boulder at caribou
cor.test(~ d1 + USC00055878, dat = test) # ned, only 12% cor
cor.test(~ d1 + USC00050183, dat = test) # estes
cor.test(~ d1 + Sawtooth, data = test) # d1 has the best relationship with Sawtooth, sdl doesn't have a good relationship with anyone.. (in the 30s)
cor.test(~ sdl + Sawtooth, data = test)

# rules:
# for any site:
# 1) std_diff is >= 2.5 (but this can also be 15mm abs so doesn't change for future infilling) & is only station that has pos precip and that constitutes sub 15% threshold

# for sdl specifically: 
# 1) equal to or greater than 0.25" (6.35mm) and no other sites at niwot had precip and sdl was 1 max 1:3 sites that had precip in region
# 2) equal to or greater than 10mm, under 15% of sites had rain and all other niwot sites are either NA or did not have rain
# 3) no precip at saddle, and pos precip at all other nwt sites

compare <- compare %>%
  mutate(compare_qcflag = ifelse(std_diff > 2.5 & pos_ppt == 1 & prop_pos < 0.15, "only site in region with precip, exceeds 2.5 scaled limit", NA),
         compare_qcflag = ifelse(local_site == "sdl" & measurement >= 6.35 &  pos_ppt %in% c(1:3)  & !d1_pos & !c1_pos & !uc_pos & !sno_pos & !ameri_pos & is.na(compare_qcflag),
                                 "sdl >= 0.25in precip, only site at nwt with precip, only 1 of 1 to 3 sites in region with precip", compare_qcflag),
         compare_qcflag = ifelse(local_site == "sdl" & measurement >= 10 &  pos_ppt %in% c(1:3) & prop_pos < 0.15 & d1_pos %in% c(F, NA) & c1_pos  %in% c(F, NA) & uc_pos %in% c(F, NA) & sno_pos %in% c(F, NA) & ameri_pos %in% c(F, NA) & is.na(compare_qcflag),
                                 "sdl >= 10mm precip, all other nwt sites no precip or NA, only 1 to 3 regional sites with precip, 15% threshold", compare_qcflag)) %>%
  mutate(qc_measurement = ifelse(!is.na(compare_qcflag), NA, measurement))


# join flags to respective datasets out...

# -- 1. apply qc flags to NWT datasets -----
chartppt_out_qc2 <- rename(chartppt_out_qc, local_site = station_name)
chartppt_out_qc2 <- left_join(chartppt_out_qc2, compare[c("date", "local_site", "compare_qcflag")])
chartppt_out_qc2$compare_qcflag[chartppt_out_qc2$local_site == "sdl" & chartppt_out_qc2$date %in% flag_sdl0$date] <- "sdl 0, all nwt positive precip, at least one > 2 z-score and d1 > 10mm or NA"

# looking at flag results, this doesn't capture days like mar 21 2012 where 10 stations (including d1 and c1) recorded pos precip, but max besides sdl was 10.75 and sdl recorded 91mm..

# diff sdl precip from all other sites, std diff and create a high bar threshold when other nwt stations record precip
sdlppt <- subset(compare, local_site == "sdl", select = c(date:doy, qc_measurement, measurement, compare_qcflag)) %>%
  rename(sdl_qc = qc_measurement, sdl = measurement, sdl_compareflag = compare_qcflag) %>%
  mutate(winter_sdl = ifelse(!mon %in% c(6:9), sdl *.39, sdl)) %>%
  left_join(subset(compare, local_site != "sdl", select = c(date:doy, local_site, qc_measurement, measurement, compare_qcflag))) %>%
  mutate(diff_raw = sdl-measurement,
         diff_winter = winter_sdl - measurement,
         std_diff = ifelse(!is.na(sdl) & !is.na(measurement), scale(diff_raw), NA),
         std_diff2 = ifelse(sdl > measurement, scale(diff_raw), NA),
         winter_diff2 = ifelse(sdl > measurement, scale(diff_winter), NA),
         log_diff2 = ifelse(sdl > measurement, log(diff_raw), NA)) %>%
  group_by(date) %>%
  subset(grepl("d1|c1|versity|NR1|iwot|094075|Saw", local_site)) %>%
  mutate(reg_mean = ifelse(sdl > measurement, mean(diff_raw, na.rm = T), NA),
         reg_mean_winter = ifelse(sdl > measurement, mean(diff_winter, na.rm = T), NA),
         d1_nonNA = !is.na(measurement[local_site == "d1"]))
sdlppt$std_mean_winter <- with(sdlppt, ifelse(!is.na(reg_mean_winter), scale(reg_mean_winter), NA))

# look at dates the standardized difference off winter adjusted ppt is > 5 (sdl ppt >> all others)

sdlover_dates <- with(sdlppt, unique(date[std_mean_winter > 5]))
sdlover_dates <- sdlover_dates[!is.na(sdlover_dates)]

ggplot(subset(compare, date %in% sdlover_dates), aes(local_site, measurement)) +
  #geom_hline(aes(yintercept = 15)) +
  geom_point(aes(col = grepl("c1|d1|Univ|Niw|NR1|sdl", local_site))) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  facet_wrap(~date)
# this seems okay.. but make rule d1 can't be NA
# e.g., may 12 1999 was a snowy period in sdl time series; tk used sdl to infill d1 then; high value on aug 2 2011 seems reasonable is UC is that high and d1 out (d1 was out the whole week)
# also check for diff btween d1 and sdl. e.g., 2013-09-23 gets flagged but sdl relative to d1 looks reasonable even if it is relatively higher precip than the other nwt stations 
# .. actually sep 23 2013 event was a snow storm.. rule is std diff > 5, sdl ppt >> all others, d1 present, and if it's in months 6-9 apply snow overcatch correction factor
sdlover_dates <- with(sdlppt, unique(date[std_mean_winter > 5 & d1_nonNA]))
sdlover_dates <- sort(sdlover_dates[!is.na(sdlover_dates)])

# mark in chart_ppt_out_qc2
chartppt_out_qc2$compare_qcflag[chartppt_out_qc2$date %in% sdlover_dates & chartppt_out_qc2$local_site == "sdl"]
chartppt_out_qc2$comparenwt_qcflag <- with(chartppt_out_qc2, ifelse(date %in% sdlover_dates & local_site == "sdl", "sdl ppt exceeds all nwt sites, mean regional difference > 5 scaled limit", NA))
chartppt_out_qc2$comparenwt_qcflag <- with(chartppt_out_qc2, ifelse(!is.na(comparenwt_qcflag) & mon %in% 6:9, paste("summer snow event, apply overcatch correction;", comparenwt_qcflag), comparenwt_qcflag))

# sarah says could compare summer flagged with mean temp (if <0C, apply correction) but not sure. infill as usual and compare fit val to overcatch corrected

chartppt_out_qc2$compare_qcflag <- with(chartppt_out_qc2, ifelse(is.na(compare_qcflag), comparenwt_qcflag, compare_qcflag))
# NA values with qc flags
chartppt_out_qc2$measurement <- with(chartppt_out_qc2, ifelse(!is.na(compare_qcflag), NA, measurement))
# drop comparenwt flag
chartppt_out_qc2 <- subset(chartppt_out_qc2, select = -comparenwt_qcflag)


# -- 2. apply qc within site flags to non-nwt station for infilling -----
# NA for infilling purposes only
ghcnd_out2 <- left_join(ghcnd_out, compare[c("date", "local_site", "compare_qcflag")])
ghcnd_out2$measurement <- with(ghcnd_out2, ifelse(!is.na(compare_qcflag), NA, measurement))

snotel_out2 <- left_join(snotel_out, compare[c("date", "local_site", "compare_qcflag")], by = c("date", "station_name" = "local_site"))
snotel_out2$measurement <- with(snotel_out2, ifelse(!is.na(compare_qcflag) & qc_flag!= "V", NA, measurement)) # only of these is validated data.. allow if qc flag is V



# -- STANDARDIZE COLNAMES OUT -----
# make data wide-format for infilling (in regression models predictor vars need to be columns)

names(chartppt_out_qc2)
names(snotel_out2)
names(ghcnd_out2)
names(ameriflux_out)
names(allsites)

# add local_site to snotel_out2, removing space in name
snotel_out2$local_site <- gsub(" ", "", snotel_out2$station_name)


# -- WRITE OUT -----
# write out qc'd ppt for infilling next
saveRDS(chartppt_out_qc2, paste0(datpath, "/qc/nwtchartPPT_qc.rds")) #15 MB
saveRDS(ghcnd_out2, paste0(datpath, "/qc/ghcndPPT_qc.rds")) #15 MB
saveRDS(snotel_out2, paste0(datpath, "/qc/snotelPPT_qc.rds")) #15 MB
saveRDS(ameriflux_out, paste0(datpath, "/qc/amerifluxPPT_qc.rds")) #15 MB
saveRDS(allsites, paste0(datpath, "/qc/siteinfoPPT_qc.rds")) #15 MB


