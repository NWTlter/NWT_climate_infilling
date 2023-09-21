#' ---
#' title: Gap-fill temperature data with prev QC'd tempdat
#' author: CTW  
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#' 
#' ### Temperature gap-filling and post-infill review
#' 
#' Test report to:
#' 1. Read in prepared, qc'd NWT and neighbor station temperature datasets
#' 2. Stack all for cycling through infilling
#' 3. Run infill methods, choose best per TK methods
#' 4. Quick review to be sure no infill values violate qc flags
#' 5. Write out for homogenization
#' 
#' All code will be displayed to show procedure and work out bugs



# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

source("nwt_climate/R/fetch_data_functions.R") # to read in prepped tidy datasets
source("nwt_climate/R/dataviz_functions.R")
source("nwt_climate/R/temp_infill_functions.R")

# set path to prepared data
datpath <- "~/Documents/nwt_lter/nwt_climate/data/"
# list all rds files in qc'd data folder (full path directory)
rdsfiles_qc <- list.files(paste0(datpath, "qc"), pattern = "rds", full.names = T)

# read in prepared precip data
charttemp <- get_tidydat("chartTEMP", rdsfiles_qc, "*")
logtemp <- get_tidydat("loggerTEMP", rdsfiles_qc, "*")
ameriflux <- get_tidydat("fluxT", rdsfiles_qc, "*")
snotel <- get_tidydat("telT", rdsfiles_qc, "*")
ghcnd <- get_tidydat("ghcndT", rdsfiles_qc, "*")
allsites <- readRDS(rdsfiles_qc[grep("infoTEM", rdsfiles_qc)])


# read in previously QC'd dats (from 2018 renewal)
# set pathway to long-term-trends repo (qhere previously qc'd data live)
lttrepo <- "~/github/long-term-trends/"
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



# -- STACK DAT FOR 2018-2020 QUICK QC ----
# want qc'd temp data thru 2018, then more recent (non-QC'd data)

# plot qc'd data first to review what got flagged
ggplot(subset(qc_c1, yr > 2010 & grepl("+3C", qa_flag)), aes(date, qa_temp)) +
  geom_point(aes(date, c1_temp), alpha = 0.5) +
  geom_point(aes(date, qa_temp), col = "red", alpha = 0.2) +
  facet_wrap(~met, scales = "free_x")

ggplot(subset(qc_d1, yr > 2010 & grepl("drop", qa_flag)), aes(date, qa_temp)) +
  geom_point(aes(date, d1_temp), alpha = 0.5) +
  geom_point(aes(date, qa_temp), col = "red", alpha = 0.2) +
  facet_wrap(~met, scales = "free_x")

# plot c1, d1 and sdl to screen for drop in 15-16 at sdl
ggplot() +
  geom_point(data = subset(qc_c1, yr > 2010 & grepl("artificial", qa_flag, ignore.case = T)), aes(date, c1_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_c1, yr > 2010), aes(date, qa_temp), color = "green", alpha = 0.5) +
  geom_point(data = subset(qc_d1, yr > 2010 & grepl("artificial", qa_flag, ignore.case = T)), aes(date, d1_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_d1, yr > 2010), aes(date, qa_temp), color = "blue", alpha = 0.5) +
  geom_point(data = subset(qc_sdl, yr > 2010), aes(date, sdl_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_sdl, yr > 2010), aes(date, sdl_qatemp), color = "orchid", alpha = 0.5) +
  facet_grid(met~local_site) +
  theme_dark()

ggplot() +
  geom_point(data = subset(qc_c1cr, !is.na(qa_flag) & c1cr_temp > -100), aes(date, c1cr_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_c1cr), aes(date, qa_temp), color = "green", alpha = 0.5) +
  geom_point(data = subset(qc_d1cr, !is.na(qa_flag) & d1cr_temp > -100), aes(date, d1cr_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_d1cr), aes(date, qa_temp), color = "blue", alpha = 0.5) +
  geom_point(data = subset(qc_sdlcr, !is.na(qa_flag) & cr_temp > -100), aes(date, cr_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_sdlcr), aes(date, qa_temp), color = "orchid", alpha = 0.5) +
  facet_grid(met~local_site) +
  theme_dark()
# i'm ok with these points being dropped (using qa_temp in these cases)
# okay to use adjusted chart data also (for artificial drops)

# use ctw qc'd sdl from renewal, and be sure to NA anything where !is.na(flag) [previously infilled]
# use 2011 onwards c1 and d1 from ctw, otherwise what's in charttemp
charttemp_c1d1 <- subset(charttemp, grepl("c1|d1", local_site) & yr < 2011 | yr > 2018) %>%
  # assign raw to measurement if tk predicted is NA (applies to 2019 onwards)
  mutate(measurement = ifelse(is.na(tk_predicted), raw, measurement))
unique(charttemp_c1d1$yr[is.na(charttemp_c1d1$tk_predicted)])

# prep qc'd 2011-2018 data
c1d1_qcd_1118 <- rbind(subset(qc_c1, yr > 2010, select = c(LTER_site:met, qa_temp)),
                       subset(qc_d1, yr > 2010, select = c(LTER_site:met, qa_temp))) %>%
  rename(metric = met, measurement = qa_temp) %>%
  # append chart to local_site
  mutate(local_site = paste0(local_site, "_chart"))

#qc_sdl has some weird things in june 2000 20s with date overlaps + qa flags (flatline), didn't join correctly
# remake with rawtemp but add qa flags in
qcsdl_flags <- subset(qc_sdl, !is.na(qa_flag), select = c(date, met, qa_flag)) %>%
  distinct()
sdlchart_prep <- subset(charttemp, local_site == "sdl_chart") %>%
  left_join(qcsdl_flags, by = c("date", "metric" = "met"))
sdlchart_prep$measurement[!is.na(sdlchart_prep$qa_flag)] <- NA

charttemp_prep <- c1d1_qcd_1118 %>%
  rbind(sdlchart_prep[names(.)]) %>%
  rbind(charttemp_c1d1[names(.)]) %>%
  arrange(local_site, date, metric) %>% ## 2019
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

ggplot(hmp_prep, aes(mon, measurement, group = mon)) +
  geom_boxplot() +
  geom_jitter(aes(col = factor(yr)), alpha = 0.5) +
  facet_wrap(~local_site) # d1 hmp2 in march looks suss; sdl hmp3 stops june 2021

ggplot(gl4_prep, aes(mon, measurement, group = mon)) +
  geom_boxplot() +
  geom_jitter(aes(col = factor(yr)), alpha = 0.5)


hmp_dailysds <- arrange(hmp_prep, date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(date, station_name) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(date, metric) %>%
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
hmp_dailysds$flag_ws <- apply(hmp_dailysds[grep("ws_sc", names(hmp_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 2) == 2)
hmp_dailysds$flag_xs <- apply(hmp_dailysds[grep("xs_sc", names(hmp_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 2) == 2)


ggplot(hmp_dailysds, aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(hmp_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(hmp_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(hmp_dailysds, ws_temprank <= 10), pch = 4, alpha = 0.9) +
  geom_point(data = subset(hmp_dailysds, xs_temprank <= 10), pch = 2, alpha = 0.9) +
  geom_point(data = subset(hmp_dailysds, flag_xs), pch = 1,alpha = 0.9) +
  geom_point(data = subset(hmp_dailysds, flag_max), pch = 4,alpha = 0.9) +
  geom_point(data = subset(hmp_dailysds, flag_avg), pch = 4,alpha = 0.9) +
  facet_wrap(~local_site)

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


ggplot(charttemp_dailysds, aes(date, measurement, col = local_site)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(charttemp_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, flag_ws), pch = 1, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, flag_xs), pch = 6, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, flag_max), pch = 6, alpha = 0.9) +
  facet_wrap(~metric, nrow = 2)

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
ggplot(charttemp_dailysds, aes(date, measurement, col = local_site)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(charttemp_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, flag_ws), pch = 1, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, flag_xs), pch = 6, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, flag_max), pch = 6, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, qcflag), col = "red") +
  geom_point(data = subset(charttemp_dailysds, qcflag2), col = "black") +
  facet_wrap(~metric, nrow = 2)

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


ggplot(snotel_dailysds, aes(date, measurement, col = local_site)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(snotel_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, flag_ws), pch = 1, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, flag_xs), pch = 6, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, flag_temp), pch = 13, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, flag_max), pch = 4, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, flag_avg), pch = 4, alpha = 0.9) +
  facet_wrap(~metric, nrow = length(unique(snotel_dailysds$metric)))

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
ggplot(subset(snotel_dailysds, !qcflag), aes(date, measurement, col = local_site)) +
  geom_point(alpha = 0.3) +
  facet_grid(local_site~metric)

ggplot(subset(snotel_dailysds, !qcflag & grepl("Univ",local_site)), aes(date, measurement, col = measurement > 27)) +
  geom_point(alpha = 0.3) +
  facet_grid(metric~local_site, scales = "free_x")
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


ggplot(ghcnd_dailysds, aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(ghcnd_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(ghcnd_dailysds, ws_deltarank <= 10), pch = 8, alpha = 0.9) +
  #geom_point(data = subset(ghcnd_dailysds, ws_temprank <= 10), pch = 4, alpha = 0.9) +
  geom_point(data = subset(ghcnd_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(ghcnd_dailysds, flag_ws), pch = 1, alpha = 0.9) +
  geom_point(data = subset(ghcnd_dailysds, flag_xs), pch = 6, alpha = 0.9) +
  geom_point(data = subset(ghcnd_dailysds, flag_temp), pch = 13, alpha = 0.9) +
  facet_wrap(~local_site)
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


ggplot(amerigl4_dailysds, aes(date, measurement, col = local_site)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(amerigl4_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, flag_ws), pch = 1, alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, flag_xs), pch = 6, alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, flag_max), pch = 3, alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, flag_avg), pch = 5, alpha = 0.9) +
  facet_wrap(~metric, nrow = length(unique(amerigl4_dailysds$metric)))

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

ggplot(amerigl4_dailysds, aes(date, measurement, col = local_site)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(amerigl4_dailysds, qcflag), alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, postcheck > 1), col = "black", alpha = 0.9) +
  facet_wrap(~metric, nrow = 2)

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
logtemp_qc <- subset(logtemp, !local_site %in% unique(nwtlog_qc$local_site[grepl("hmp|gl4", nwtlog_qc$local_site)])) %>%
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
ggplot(subset(logtemp_qc, grepl("shift", qcflag)), aes(date, qa_temp)) +
  geom_line() +
  geom_line(data = subset(logtemp_qc, grepl("shift", qcflag)), aes(date, raw), col= "blue", lwd = 1, alpha = 0.8) +
  geom_line(data = subset(logtemp_qc, grepl("shift", qcflag)), aes(date, measurement), col= "red", alpha = 0.8) +
  facet_grid(metric~local_site, scale = "free_x") # looks good

# be sure (excluding shifted temps) # non-NA data points == qa_temp
summary(with(logtemp_qc, measurement[!grepl("shift", qcflag)]) == with(logtemp_qc, qa_temp[!grepl("shift", qcflag)])) # good (NAs ok)
# assign qa_temp to measurement col
logtemp_qc$old_measurement <- logtemp_qc$measurement
logtemp_qc$measurement <- logtemp_qc$qa_temp

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
ggplot(allnwtlog_qc, aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~local_site, scales = "free") #d1_cr21x has issues w avg
# add check for flagmax and flagmin
allnwtlog_qc <- group_by(allnwtlog_qc, date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] <= measurement[metric == "airtemp_min"],
         flag_avg = measurement[metric == "airtemp_avg"] <= measurement[metric == "airtemp_min"] | measurement[metric == "airtemp_avg"] >= measurement[metric == "airtemp_max"]) %>%
  ungroup()
ggplot(allnwtlog_qc, aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.5) +
  geom_point(data = subset(allnwtlog_qc, flag_max | flag_avg)) +
  facet_wrap(~local_site, scales = "free")

# focus on d1_cr21x and c1_cr23x
ggplot(subset(allnwtlog_qc, grepl("d1_cr21x|c1_cr23x", local_site)), aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.5) +
  #geom_smooth(alpha = 0.5, method = "loess") +
  geom_point(data = subset(allnwtlog_qc, grepl("d1_cr21x|c1_cr23x", local_site) & (flag_max | flag_avg))) +
  facet_wrap(~yr +local_site, scales = "free") +
  theme(legend.position = "none") # 1992-1999 are weird years for tmean

# look at all loggers in that period
ggplot(subset(allnwtlog_qc, grepl("cr21x", local_site) & yr %in% c(1992:1999)), aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.5) +
  #geom_smooth(alpha = 0.5, method = "loess") +
  #geom_point(data = subset(allnwtlog_qc, grepl("d1_cr21x|c1_cr23x", local_site) & (flag_max | flag_avg))) +
  facet_wrap(~paste(yr,local_site), scales = "free_x", nrow = 8) +
  theme(legend.position = "none") # 1992-1999 are weird years for tmean
# re-run quick-qc checks. wonky tmean values seem to be on days where one of the extremes is missing

allnwtlog_qc_dailysds <- allnwtlog_qc %>%
  group_by(metric, local_site) %>%
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
  mutate(flag_avgpresent = all(is.na(measurement[metric != "airtemp_avg"])) & !is.na(measurement[metric == "airtemp_avg"])) %>%
  ungroup()

# plot rankings
ggplot(allnwtlog_qc_dailysds, aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(allnwtlog_qc_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, flag_avgpresent), pch = 1, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, flag_avg), pch = 6, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, flag_max), pch = 6, alpha = 0.9) +
  facet_wrap(~local_site, scales = "free") +
  theme(legend.position = "none")


# just 21x and 23x periods
ggplot(subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site)), aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & xs_deltarank <= 20), alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & ws_deltarank <= 20), pch = 8, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & ws_temprank <= 20), pch = 4, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & xs_temprank <= 20), pch = 2, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & flag_avgpresent), pch = 1, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & flag_avg), pch = 6, alpha = 0.9) +
  geom_point(data = subset(allnwtlog_qc_dailysds, grepl("21x|23x", local_site) & flag_max), pch = 6, alpha = 0.9) +
  facet_wrap(~local_site, scales = "free_x", nrow = 3) +
  theme(legend.position = "none")

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

# plot all sensors again
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
ggplot(allnwtlog_qc, aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~local_site, scales = "free")

ggplot(allnwtlog_qc, aes(date, measurement, col = logger, lty = metric)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~station_name, nrow = 4)
# remove first gl4 sensor (cr10) from regression consideration bc range looks truncated

# just c1, d1, and sdl
ggplot(subset(allnwtlog_qc, !grepl("gl4", station_name)), aes(date, measurement, group = local_site, col = station_name, lty = logger)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~metric, scales = "free_y", nrow = 3) +
  theme(legend.position = "top") # good enough. early d1 hmps still look wonky for tmean

allnwtlog_qc <- subset(allnwtlog_qc, local_site != "gl4_cr10", select = -c(qcflag3, flag_avgpresent))

# ready for regressions!



# -- PREP DATA FOR REGRESSIONS -----

# need data to have date, yr, doy, local_site (unique ID), metric, and measurement
charttemp_lm <- mean_and_diurnalT(charttemp_prep)
nwtlog_lm <- mean_and_diurnalT(allnwtlog_qc)
snotel_lm <- mean_and_diurnalT(snotel)
ameriflux_lm <- mean_and_diurnalT(ameriflux) 
ghcnd_lm <- mean_and_diurnalT(subset(ghcnd, metric != "TOBS"))

# need date, yr, mon, doy, local_site, and measurement (qc'd)
alldats <- rbind(charttemp_lm, nwtlog_lm, snotel_lm, ameriflux_lm, ghcnd_lm) %>%
  subset(grepl("avg|DTR", metric)) %>%
  arrange(local_site, date, metric)

# make sure allsites ordered by pair rank
allsites <- arrange(allsites, local_site, final_rank, paired_site)



# -- INFILL SDL LOGGERS -----



# -- 21x 1986-2000 -----
unique(allsites$local_site[grepl("sdl", allsites$local_site)])

sdl21x_missing <- idInfillDates(alldats, "sdl_cr21x", 1980)
sdl21x_order <- with(allsites, paired_site[grepl("sdl_cr21x", local_site) & final_rank!= 1])
# moving window infill
sdl21x_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr21x", missing_dates = sdl21x_missing, site_order =  sdl21x_order)
# historic infill
sdl21x_historic <- tk_temp_historicfill(alldats, "sdl_cr21x", sdl21x_missing, sdl21x_order)
# model selection for sdl21x
select_sdl121x <- select_model(sdl21x_historic, sdl21x_season)
# calculate regression-derived tmin and tmax
sdl21x_predicted <- calculate_minmax(select_sdl121x, allnwtlog_qc, "sdl_cr21x")
# sdl21x_predicted$flagmax <- with(sdl21x_predicted, airtemp_max <= airtemp_min | airtemp_avg >= airtemp_max)
# sdl21x_predicted$flagmin <- with(sdl21x_predicted, airtemp_avg <= airtemp_min)

# how does it look?
ggplot(sdl21x_predicted) +
  geom_vline(aes(xintercept = as.Date("1989-01-01"))) +
  geom_line(aes(date, airtemp_avg), col = "green", alpha = 0.3) +
  geom_line(aes(date, airtemp_max), col = "purple", alpha = 0.3) +
  geom_point(data = subset(sdl21x_predicted, airtemp_max_method != "raw"), aes(date, airtemp_max), col = "red", alpha = 0.5) +
  geom_line(aes(date, airtemp_min), col = "blue", alpha = 0.3) +
  geom_point(data = subset(sdl21x_predicted, airtemp_min_method != "raw"), aes(date, airtemp_min), col = "dodgerblue", alpha = 0.5) +
  geom_point(data = subset(sdl21x_predicted, flagmin), aes(date, airtemp_min), size = 2, col = "blue", alpha = 0.8) +
  geom_point(data = subset(sdl21x_predicted, flagmin), aes(date, airtemp_avg), size = 2, col = "forestgreen", alpha = 0.8) +
  geom_point(data = subset(sdl21x_predicted, flagmin), aes(date, airtemp_max), size = 2, col = "chocolate", alpha = 0.8) +
  geom_smooth(aes(date, airtemp_avg), col = "black", alpha = 0.3)


# -- sdl 23x 2000-2012 -----
sdl23x_missing <- idInfillDates(alldats, "sdl_cr23x", 1980)
sdl23x_order <- with(allsites, paired_site[grepl("sdl_cr23x", local_site) & final_rank!= 1])
# moving window infill
sdl23x_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr23x", missing_dates = sdl23x_missing, site_order =  sdl23x_order)
# historic infill
sdl23x_historic <- tk_temp_historicfill(alldats, "sdl_cr23x", sdl23x_missing, sdl23x_order)
# selection for sdl23x
select_sdl123x <- select_model(sdl23x_historic, sdl23x_season)
# calculate regression derived tmin and max
sdl23x_predicted <- calculate_minmax(select_sdl123x, allnwtlog_qc, "sdl_cr23x")

# how does it look?
ggplot(sdl23x_predicted) +
  #geom_vline(aes(xintercept = as.Date("1989-01-01"))) +
  geom_line(aes(date, airtemp_avg), col = "green", alpha = 0.3) +
  geom_line(aes(date, airtemp_max), col = "purple", alpha = 0.3) +
  geom_point(data = subset(sdl23x_predicted, airtemp_max_method != "raw"), aes(date, airtemp_max), col = "red", alpha = 0.5) +
  geom_line(aes(date, airtemp_min), col = "blue", alpha = 0.3) +
  geom_point(data = subset(sdl23x_predicted, airtemp_min_method != "raw"), aes(date, airtemp_min), col = "dodgerblue", alpha = 0.5) +
  geom_point(data = subset(sdl23x_predicted, flagmin), aes(date, airtemp_min), size = 2, col = "blue", alpha = 0.8) +
  geom_point(data = subset(sdl23x_predicted, flagmin), aes(date, airtemp_avg), size = 2, col = "forestgreen", alpha = 0.8) +
  geom_point(data = subset(sdl23x_predicted, flagmin), aes(date, airtemp_max), size = 2, col = "chocolate", alpha = 0.8)
  



# -- sdl 1000 (pre hmp) 2012-2018 ----
sdl1000_missing <- idInfillDates(alldats, "sdl_cr1000", 1980)
sdl1000_order <- with(allsites, paired_site[grepl("sdl_cr1000", local_site) & final_rank!= 1])
# moving window infill
sdl1000_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr1000", missing_dates = sdl1000_missing, site_order =  sdl1000_order)
# historic infill
sdl1000_historic <- tk_temp_historicfill(alldats, "sdl_cr1000", sdl1000_missing, sdl1000_order)

# selection for sdl1000
select_sdl1000 <- select_model(sdl1000_historic, sdl1000_season)
sdl1000_predicted <- calculate_minmax(select_sdl1000, allnwtlog_qc, "sdl_cr1000")

# how does it look?
ggplot(sdl1000_predicted) +
  #geom_vline(aes(xintercept = as.Date("1989-01-01"))) +
  geom_line(aes(date, airtemp_avg), col = "green", alpha = 0.3) +
  geom_line(aes(date, airtemp_max), col = "purple", alpha = 0.3) +
  geom_point(data = subset(sdl1000_predicted, airtemp_max_method != "raw"), aes(date, airtemp_max), col = "red", alpha = 0.5) +
  geom_line(aes(date, airtemp_min), col = "blue", alpha = 0.3) +
  geom_point(data = subset(sdl1000_predicted, airtemp_min_method != "raw"), aes(date, airtemp_min), col = "dodgerblue", alpha = 0.5) +
  geom_point(data = subset(sdl1000_predicted, flagmin), aes(date, airtemp_min), size = 2, col = "blue", alpha = 0.8) +
  geom_point(data = subset(sdl1000_predicted, flagmin), aes(date, airtemp_avg), size = 2, col = "forestgreen", alpha = 0.8) +
  geom_point(data = subset(sdl1000_predicted, flagmin), aes(date, airtemp_max), size = 2, col = "chocolate", alpha = 0.8) # only 1 dat
# what was predicted?
select_sdl1000$date
View(subset(sdl1000_predicted, date %in% unique(select_sdl1000$date)))



# -- sdl hmp 1 ----
sdl1000_hmp_1_missing <- idInfillDates(alldats, "sdl_cr1000_hmp_1", 1980)
sdl1000_hmp_1_order <- with(allsites, paired_site[grepl("sdl_cr1000_hmp_1", local_site) & final_rank!= 1])
# moving window infill
sdl1000_hmp_1_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr1000_hmp_1", missing_dates = sdl1000_hmp_1_missing, site_order =  sdl1000_hmp_1_order)
# historic infill
sdl1000_hmp_1_historic <- tk_temp_historicfill(alldats, "sdl_cr1000_hmp_1", sdl1000_hmp_1_missing, sdl1000_hmp_1_order)
# selection for hmp1
select_sdl1000_hmp_1 <- select_model(sdl1000_hmp_1_historic, sdl1000_hmp_1_season)
# calculate derived tmin and tmax
sdl1000_hmp_1_predicted <- calculate_minmax(select_sdl1000_hmp_1, allnwtlog_qc, "sdl_cr1000_hmp_1")

# how does it look?
ggplot(sdl1000_hmp_1_predicted) +
  #geom_vline(aes(xintercept = as.Date("1989-01-01"))) +
  geom_line(aes(date, airtemp_avg), col = "green", alpha = 0.3) +
  geom_line(aes(date, airtemp_max), col = "purple", alpha = 0.3) +
  geom_point(data = subset(sdl1000_hmp_1_predicted, airtemp_max_method != "raw"), aes(date, airtemp_max), col = "red", alpha = 0.5) +
  geom_line(aes(date, airtemp_min), col = "blue", alpha = 0.3) +
  geom_point(data = subset(sdl1000_hmp_1_predicted, airtemp_min_method != "raw"), aes(date, airtemp_min), col = "dodgerblue", alpha = 0.5) +
  geom_point(data = subset(sdl1000_hmp_1_predicted, flagmin), aes(date, airtemp_min), size = 2, col = "blue", alpha = 0.8) +
  geom_point(data = subset(sdl1000_hmp_1_predicted, flagmin), aes(date, airtemp_avg), size = 2, col = "forestgreen", alpha = 0.8) +
  geom_point(data = subset(sdl1000_hmp_1_predicted, flagmin), aes(date, airtemp_max), size = 2, col = "chocolate", alpha = 0.8) # only 1 dat

# compare low predicted points period with other hmps when all done



# -- sdl hmp 2 ----
sdl1000_hmp_2_missing <- idInfillDates(alldats, "sdl_cr1000_hmp_2", 1980)
sdl1000_hmp_2_order <- with(allsites, paired_site[grepl("sdl_cr1000_hmp_2", local_site) & final_rank!= 1])
# moving window infill
sdl1000_hmp_2_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr1000_hmp_2", missing_dates = sdl1000_hmp_2_missing, site_order =  sdl1000_hmp_2_order)
# historic infill
sdl1000_hmp_2_historic <- tk_temp_historicfill(alldats, "sdl_cr1000_hmp_2", sdl1000_hmp_2_missing, sdl1000_hmp_2_order)
# selection for hmp2
select_sdl1000_hmp_2 <- select_model(sdl1000_hmp_2_historic, sdl1000_hmp_2_season)
# calculate regression derived tmax and min
sdl1000_hmp_2_predicted <- calculate_minmax(select_sdl1000_hmp_2, allnwtlog_qc, "sdl_cr1000_hmp_2")


# how does it look?
ggplot(sdl1000_hmp_2_predicted) +
  #geom_vline(aes(xintercept = as.Date("1989-01-01"))) +
  geom_line(aes(date, airtemp_avg), col = "green", alpha = 0.3) +
  geom_line(aes(date, airtemp_max), col = "purple", alpha = 0.3) +
  geom_point(data = subset(sdl1000_hmp_2_predicted, airtemp_max_method != "raw"), aes(date, airtemp_max), col = "red", alpha = 0.5) +
  geom_line(aes(date, airtemp_min), col = "blue", alpha = 0.3) +
  geom_point(data = subset(sdl1000_hmp_2_predicted, airtemp_min_method != "raw"), aes(date, airtemp_min), col = "dodgerblue", alpha = 0.5) +
  geom_point(data = subset(sdl1000_hmp_2_predicted, flagmin), aes(date, airtemp_min), size = 2, col = "blue", alpha = 0.8) +
  geom_point(data = subset(sdl1000_hmp_2_predicted, flagmin), aes(date, airtemp_avg), size = 2, col = "forestgreen", alpha = 0.8) +
  geom_point(data = subset(sdl1000_hmp_2_predicted, flagmin), aes(date, airtemp_max), size = 2, col = "chocolate", alpha = 0.8) # only 1 dat



# -- sdl hmp 3 ----
sdl1000_hmp_3_missing <- idInfillDates(alldats, "sdl_cr1000_hmp_3", 1980)
sdl1000_hmp_3_order <- with(allsites, paired_site[grepl("sdl_cr1000_hmp_3", local_site) & final_rank!= 1])
# moving window infill
sdl1000_hmp_3_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr1000_hmp_3", missing_dates = sdl1000_hmp_3_missing, site_order =  sdl1000_hmp_3_order)
# historic infill
sdl1000_hmp_3_historic <- tk_temp_historicfill(alldats, "sdl_cr1000_hmp_3", sdl1000_hmp_3_missing, sdl1000_hmp_3_order)
# selection for hmp3
select_sdl1000_hmp_3 <- select_model(sdl1000_hmp_3_historic, sdl1000_hmp_3_season)
# calculate regression derived tmin and tmax
sdl1000_hmp_3_predicted <- calculate_minmax(select_sdl1000_hmp_3, allnwtlog_qc, "sdl_cr1000_hmp_3")
# hmp 3 seems to have stopped at june 2021?


# how does it look?
ggplot(sdl1000_hmp_3_predicted) +
  #geom_vline(aes(xintercept = as.Date("1989-01-01"))) +
  geom_line(aes(date, airtemp_avg), col = "green", alpha = 0.3) +
  geom_line(aes(date, airtemp_max), col = "purple", alpha = 0.3) +
  geom_point(data = subset(sdl1000_hmp_3_predicted, airtemp_max_method != "raw"), aes(date, airtemp_max), col = "red", alpha = 0.5) +
  geom_line(aes(date, airtemp_min), col = "blue", alpha = 0.3) +
  geom_point(data = subset(sdl1000_hmp_3_predicted, airtemp_min_method != "raw"), aes(date, airtemp_min), col = "dodgerblue", alpha = 0.5) +
  geom_point(data = subset(sdl1000_hmp_3_predicted, flagmin), aes(date, airtemp_min), size = 2, col = "blue", alpha = 0.8) +
  geom_point(data = subset(sdl1000_hmp_3_predicted, flagmin), aes(date, airtemp_avg), size = 2, col = "forestgreen", alpha = 0.8) +
  geom_point(data = subset(sdl1000_hmp_3_predicted, flagmin), aes(date, airtemp_max), size = 2, col = "chocolate", alpha = 0.8) # only 1 date


# -- stack all together ----
allsdllog_predicted <- rbind(sdl21x_predicted, sdl23x_predicted, sdl1000_predicted, 
                             sdl1000_hmp_1_predicted, sdl1000_hmp_2_predicted, sdl1000_hmp_3_predicted) %>%
  arrange(date, local_site) %>%
  subset(select = -c(flagmax, flagmin, flag_airtemp_avg, airtemp_avg_infill)) %>%
  distinct()



# -- INFILL SDL CHART -----
# infill sdl chart for homogenizing logger record
# be sure have sdl chart qc dataset ready to pair with predicted
# add dbl flags to sdlchart_prep and NA those measurements -- should match what's in alldats

# calculate mean
qcsdl_out <- left_join(sdlchart_prep, charttemp_dailysds[c("date", "metric", "local_site", "qcflag2")])
qcsdl_out$measurement[qcsdl_out$qcflag2] <- NA
# update qa_flag
qcsdl_out$qa_flag[qcsdl_out$qcflag2] <- "failed within-site or spatial coherency deviance check"
qcsdl_out <- qcsdl_out[!grepl("tk_|ctw_adj|qcflag2", names(qcsdl_out))]
sdltmean <- subset(charttemp_lm, grepl("avg|DTR", metric) & local_site == "sdl_chart") %>%
  mutate(qa_flag = NA, raw = measurement, flag = NA) %>%
  left_join(distinct(dplyr::select(qcsdl_out, date:local_site, station_id:station_name)))
# add avg and dtr to other dats
qcsdl_out <- rbind(qcsdl_out, sdltmean[names(qcsdl_out)]) %>%
  arrange(date, metric) %>%
  distinct()



sdl_chart_missing <- idInfillDates(alldats, "sdl_chart", 1980)
sdl_chart_order <- with(allsites, paired_site[grepl("sdl_chart", local_site) & final_rank!= 1])
# moving window infill
sdl_chart_season <- tk_temp_movingfill(alldats, target_site = "sdl_chart", missing_dates = sdl_chart_missing, site_order =  sdl_chart_order)
# historic infill
sdl_chart_historic <- tk_temp_historicfill(alldats, "sdl_chart", sdl_chart_missing, sdl_chart_order)
# model selection for sdl_chart
select_sdl_chart <- select_model(sdl_chart_historic, sdl_chart_season)
# calculate regression-derived tmin and tmax
sdl_chart_predicted <- calculate_minmax(select_sdl_chart, subset(qcsdl_out), "sdl_chart")
# local_site is in there 2x
sdl_chart_predicted <- subset(sdl_chart_predicted, select = -local_site.1)

# how does it look?
ggplot(sdl_chart_predicted) +
  geom_vline(aes(xintercept = as.Date("1989-01-01"))) +
  geom_line(aes(date, airtemp_avg), col = "green", alpha = 0.3) +
  geom_line(aes(date, airtemp_max), col = "purple", alpha = 0.3) +
  geom_point(data = subset(sdl_chart_predicted, airtemp_max_method != "raw"), aes(date, airtemp_max), col = "red", alpha = 0.5) +
  geom_line(aes(date, airtemp_min), col = "blue", alpha = 0.3) +
  geom_point(data = subset(sdl_chart_predicted, airtemp_min_method != "raw"), aes(date, airtemp_min), col = "dodgerblue", alpha = 0.5) +
  geom_point(data = subset(sdl_chart_predicted, flagmin), aes(date, airtemp_min), size = 2, col = "blue", alpha = 0.8) +
  geom_point(data = subset(sdl_chart_predicted, flagmin), aes(date, airtemp_avg), size = 2, col = "forestgreen", alpha = 0.8) +
  geom_point(data = subset(sdl_chart_predicted, flagmin), aes(date, airtemp_max), size = 2, col = "chocolate", alpha = 0.8) +
  geom_smooth(aes(date, airtemp_avg), col = "black", alpha = 0.3)

# plot DTR bc end of record looks a little funky
ggplot(sdl_chart_predicted, aes(date, airtemp_max - airtemp_min, col = yr)) +
  geom_line() +
  geom_point(data = subset(sdl_chart_predicted, !is.na(dtr_infill)), col = "black") +
  geom_smooth(method = "lm") +
  scale_color_viridis_c()
# sdl predicted is still missing 3 dates in feb 2011. i don't know why. not important right now since not MO not using sdl chart record for figure
# they are three dates in feb where max, avg, and min are all the same value (three spaced out dates, let be for now)


# -- WRITE OUT FOR HOMOGENIZATION ----
# to data/infill subfolder:

# predicted sdl logger
saveRDS(allsdllog_predicted, paste0(datpath,"infill/sdllogger_infilled_MOdraft.rds"))
# predicted sdl chart
saveRDS(sdl_chart_predicted, paste0(datpath,"infill/sdlchart_infilled_MOdraft.rds"))
# logger prep dat with qc flagging
saveRDS(allnwtlog_qc, paste0(datpath,"infill/sdllogger_dblqc_MOinfill.rds"))
# sdl prep with qc flagging
saveRDS(qcsdl_out, paste0(datpath,"infill/sdlchart_dblqc_infill.rds"))

# Boulder 14 for comparison
boulder14 <- subset(ghcnd, grepl("94075", local_site))
saveRDS(boulder14, paste0(datpath,"infill/boulder14_qwikqc_infill.rds"))
