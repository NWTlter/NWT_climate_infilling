#' ---
#' title: Homogenize sdl logger temperature with infilled tempdat *for MO ms*
#' author: CTW  
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#' 
#' ### Temperature gap-filling and post-infill review
#' 
#' Test report to:
#' # homogenize sdl logger dat for MO
#' # ctw hates all things data rn
#' 

rm(list=ls())
# -- SETUP ----
library(tidyverse)
library(data.table)
options(stringsAsFactors = F)

# set path to prepared data
datpath <- "~/OneDrive - UCB-O365/NWT_Infilling_2023/data/"
# list all rds files in qc'd data folder (full path directory)
rdsfiles <- list.files(paste0(datpath), pattern = "rds", full.names = T, recursive = TRUE)

# read in infilled sdl temp data
# sdlchart_pred <- readRDS(rdsfiles[grepl("sdl_chart_infill", rdsfiles)])
sdlhmp_pred <- readRDS(rdsfiles[grepl("sdlhmp_infilled_2023", rdsfiles)])
nwtlog_qc <- readRDS(rdsfiles[grepl("nwtloggerTEMP_ready", rdsfiles)])

# -- AVERAGE HMP PERIOD ----
sdlhmps <- subset(sdlhmp_pred, grepl("hmp", local_site), 
                  select = c(date:airtemp_max, airtemp_min, airtemp_avg, dtr_infill)) %>%
  mutate(predicted = !is.na(dtr_infill),
         DTR = airtemp_max - airtemp_min) %>%
  group_by(date) %>%
  mutate(sensors_infilled = sum(predicted)) %>%
  ungroup() %>%
  subset(select = -c(dtr_infill, predicted)) %>%
  gather(metric, measurement, airtemp_max:DTR) %>%
  group_by(date, metric) %>%
  mutate(hmp_avg = mean(measurement),
         nobs = length(measurement)) %>%
  ungroup() %>%
  subset(select = c(date:logger, metric, hmp_avg, nobs, sensors_infilled)) %>%
  distinct() %>%
  spread(metric, hmp_avg) %>%
  mutate()
summary(sdlhmps)


# # look at how predicted differs from raw
# View(subset(sdlloggers, predicted & round(airtemp_max,5) != round(raw_airtemp_max,5)))
# View(subset(sdlloggers, predicted & round(airtemp_min,5) != round(raw_airtemp_min,5))) # not worth fussing over rn

sdllog_all <- sdlhmps |> select(-nobs) %>%
  arrange(date) %>%
  # I realize I may not want to spread this yet
  gather(metric, measurement, airtemp_avg:DTR)


# -- MAKE HOMOGENIZED DAT -----
# add in 2012-12-04
# missing_sdldate <- subset(sdlnwt8, date == as.Date("2012-12-04")) %>%
#   rename(logger = source_instrument, measurement = qa_airtemp) %>%
#   mutate(sensors_infilled = 1,
#          metric = paste0("airtemp_", metric),
#          metric = gsub("mean", "avg", metric))

dat_out <- sdllog_all %>%
  # rbind(missing_sdldate[names(.)]) %>%
  arrange(date) %>%
  spread(metric, measurement) %>%
  mutate(DTR = ifelse(is.na(DTR), airtemp_max - airtemp_min, DTR)) %>%
  gather(metric, measurement, airtemp_avg:DTR) %>%
  subset(!metric == "DTR") %>%
  # left_join(adjustment[c("logger", "metric", "adjustnr1")]) %>%
  # mutate(adjustnr1 = ifelse(grepl("hmp", logger), 0, adjustnr1),
  #        adjustment = ifelse(grepl("hmp|21x", logger), 0, adjustnr1),
  #        adjusted_airtemp = round(measurement +adjustment,4)) %>%
  # rename(airtemp = measurement,
  #        # diff_from_hmpdiffnr1 = adjustnr1,
  #        adjust_to_hmp = adjustment) %>%
  mutate(LTER_site = "NWT",
         local_site = "sdl") %>%
  subset(select = c(LTER_site, local_site, logger, date:doy, metric, measurement, sensors_infilled))

# check that there is one value per day, every day in time series
group_by(dat_out, metric, date) %>%
  summarise(nobs = length(measurement)) %>%
  ungroup() %>%
  summary()


summary(unique(dat_out$date) %in% seq.Date(min(dat_out$date), max(dat_out$date), 1))
summary(seq.Date(min(dat_out$date), max(dat_out$date), 1) %in% unique(dat_out$date))
# and be sure 3 metrics per date
group_by(dat_out, date) %>%
  summarise(nobs = length(unique(metric))) %>%
  summary() # looks good

# what do data look like?
ggplot(dat_out, aes(date, measurement)) +
  geom_line(col = "grey30") +
  geom_point(data = subset(dat_out, sensors_infilled > 0), col ="goldenrod", alpha = 0.5) +
  geom_smooth(col = "black", method = "lm") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  facet_wrap(~factor(metric, levels = c("airtemp_max", "airtemp_avg", "airtemp_min")), nrow = 3)

group_by(dat_out, yr, metric) %>%
  summarise(T_xbar = mean(measurement),
            T_se = sd(measurement)/sqrt(length(date)),
            nobs = length(date)) %>%
  subset(!nobs < 360) %>%
  ggplot(aes(yr, T_xbar)) +
  geom_point(col = "grey30", size = 2) +
  geom_errorbar(aes(ymax = T_se + T_xbar, ymin = T_xbar - T_se), width = 0.1) +
  #geom_point(data = subset(dat_out, sensors_infilled > 0), col ="goldenrod", alpha = 0.5) +
  geom_smooth(col = "goldenrod4", fill = "goldenrod1") +
  #scale_x_date(date_breaks = "4 years") +
  facet_wrap(~factor(metric, levels = c("airtemp_max", "airtemp_avg", "airtemp_min")), scales = "free_y", nrow = 3)

annualmeans <- group_by(dat_out, yr, metric) %>%
  summarise(T_xbar = mean(measurement),
            T_se = sd(measurement)/sqrt(length(date)),
            nobs = length(date)) %>%
  ungroup() #%>%
  


# -- CLEAN UP DATASET FOR SCE/EDI -----
# want wide dataset..
# id cols, then date cols
# adjusted tmax, tmean, tmin, and dt
# adjustment cols
# predicted tmax, tmean and dtr (hmp 1,2 and 3 will be separate)
# flag info
# regression info (hmp 1,2 and 3 will be separate)
# qc notes (hmp 1,2 and 3 will be separate??)
# raw values

glimpse(dat_out)

# for ease, make prediction columns then join to adjusted tempdat 
# (make each part separately since this will be wide)
adjusted_dat_out <- subset(dat_out) %>% # drop diffval and just keep adjustment applied
  rename(homogenized = measurement) %>%
  gather(column, value, homogenized:ncol(.)) %>%
  unite(metric, metric, column, sep =  "_") %>%
  spread(metric, value) %>%
  #calculate adjusted_DTR
  mutate(dtr_homogenized= airtemp_max_homogenized - airtemp_min_homogenized) %>%
  # organize columns
  subset(select = c(LTER_site:doy, 
                    airtemp_max_homogenized, airtemp_avg_homogenized, airtemp_min_homogenized, dtr_homogenized,
                    airtemp_max_sensors_infilled, airtemp_avg_sensors_infilled, airtemp_min_sensors_infilled))

# do follow Tim's method for flags for infilling
# note qc flagging in its own column, with short description
# looks like keith's checks were same station only (but that makes sense bc they were hourly so it's reasonable to check for spikes)
# create infill flagging for temp (method 1 = moving window fill, method 2 = historic)

# notice airtemp_avg_method is raw sometimes when an infill method is present, fix:
sdlhmp_pred$airtemp_avg_method[grepl("m", sdlhmp_pred$method)] <- "predicted"
unique(sdlhmp_pred$airtemp_avg_method[is.na(sdlhmp_pred$method)]) # all 'raw', good

sdlhmp_pred$flag_1 <- NA
sdlhmp_pred$flag_1[is.na(sdlhmp_pred$method)] <- "A"
# flagging based on tmean pval
sdlhmp_pred$flag_1[grepl("multi", sdlhmp_pred$method) & sdlhmp_pred$airtemp_avg_pval <= 0.05] <- "D"
sdlhmp_pred$flag_1[grepl("multi", sdlhmp_pred$method) & sdlhmp_pred$airtemp_avg_pval > 0.05] <- "E"
sdlhmp_pred$flag_1[grepl("moving", sdlhmp_pred$method) & sdlhmp_pred$airtemp_avg_pval <= 0.05] <- "B"
sdlhmp_pred$flag_1[grepl("moving", sdlhmp_pred$method) & sdlhmp_pred$airtemp_avg_pval > 0.05] <- "C"

# flagging for tmax/tmin derivation
sdlhmp_pred$flag_2 <- NA
sdlhmp_pred$flag_2[is.na(sdlhmp_pred$method)] <- "A"
sdlhmp_pred$flag_2[sdlhmp_pred$airtemp_max_method == "predicted" & sdlhmp_pred$airtemp_min_method == "predicted"] <- "B"
# if raw adjustment failed, assign B*
sdlhmp_pred$flag_2[grepl("failed", sdlhmp_pred$airtemp_max_method)] <- "B*"
sdlhmp_pred$flag_2[grepl("adjusted$", sdlhmp_pred$airtemp_max_method) & sdlhmp_pred$airtemp_min_method == "raw"] <- "C"
sdlhmp_pred$flag_2[sdlhmp_pred$airtemp_max_method == "raw" & grepl("dtr adjusted$", sdlhmp_pred$airtemp_min_method)] <- "D"

# flagging for dtr pval
sdlhmp_pred$flag_3 <- NA
sdlhmp_pred$flag_3[is.na(sdlhmp_pred$method)] <- "A"
sdlhmp_pred$flag_3[sdlhmp_pred$dtr_pval <= 0.05] <- "B"
sdlhmp_pred$flag_3[sdlhmp_pred$dtr_pval > 0.05] <- "C"

# be sure for prediction only keep hmps for 2018 and cr23x for overlap dates btwn 21 and 23
# preds_out_crs <- subset(sdlhmp_pred, !grepl("hmp", local_site)) %>%
#   # drop method columns since infill flags present, drop local_site as well (not needed)
#   dplyr::select(-(names(sdlhmp_pred)[grepl("method|local_si", names(sdlhmp_pred))])) %>%
#   rename_at(names(.)[grepl("_avg$|_min$|_max$", names(.)) & !grepl("^raw", names(.))], function(x) paste0(x, "_gapfilled")) %>%
#   mutate(dtr_gapfilled = airtemp_max_gapfilled - airtemp_min_gapfilled,
#          # to be sure, check dtr_gapfilled == dtr_infill when dtr_infill is present
#          dtr_check = ifelse(!is.na(dtr_infill), round(dtr_infill,5) == round(dtr_gapfilled,5), NA)) %>% # in manual check, yes agrees
#   # organize columns
#   subset(select = c(date, yr, mon, doy, logger,
#                     airtemp_max_gapfilled, airtemp_min_gapfilled, airtemp_avg_gapfilled, dtr_gapfilled,
#                     flag_1, flag_2, flag_3, source.station,
#                     airtemp_avg_pval, airtemp_avg_r2, airtemp_avg_n.obs, airtemp_avg_equation, 
#                     dtr_pval, dtr_r2, dtr_n.obs, dtr_equation, 
#                     raw_airtemp_max, raw_airtemp_min, raw_airtemp_avg)) %>%
#   # clean up names
#   rename(num_obs_in_airtemp_avg_regression = airtemp_avg_n.obs,
#          num_obs_in_dtr_regression = dtr_n.obs,
#          source_station = source.station)

# need to make yr 2018 where gapfilled cols are means of hmps and everything else NA (since each hmp has its own regreesion)
# and dtr_gapfilled is the diff btwn the means
# preds_out_2018



preds_out_hmps <- subset(sdlhmp_pred, grepl("hmp", local_site)) %>%
  # drop method columns since infill flags present
  dplyr::select(-(names(sdlhmp_pred)[grepl("method", names(sdlhmp_pred))])) %>%
  rename_at(names(.)[grepl("_avg$|_min$|_max$", names(.)) & !grepl("^raw", names(.))], function(x) paste0(x, "_gapfilled")) %>%
  gather(column, value, airtemp_max_gapfilled:ncol(.)) %>%
  mutate(local_site = str_extract(local_site, "hmp.*"),
         # clean up hmp_[1-3] to keep consistent with edi logger dataset
         local_site = gsub("_", "", local_site)) %>%
  unite(column, column, local_site, sep = "_") %>%
  # clean up hmp[1-3]
  # mutate(column = gsub("hmp_(?=[123])", "hmp", column, perl = T)) %>%
  spread(column, value) %>%
  # during gather, numeric cols got converted to character
  mutate_at(.vars = names(.)[!grepl("date|equat|flag|source|logg", names(.))], as.numeric)

# in data pkg 405 all the hmp1 cols come before any hmp2 cols 
format_hmps <- function(dat, n){
  
  dat_out <- dat[grepl(paste0("date|hmp", n), names(dat))] %>%
    # strip hmp# so can recycle this code for other hmps
    rename_all(function(x) gsub("_hmp[1-3]", "", x)) %>%
    mutate(dtr_gapfilled = airtemp_max_gapfilled - airtemp_min_gapfilled,
           # to be sure, check dtr_gapfilled == dtr_infill when dtr_infill is present
           dtr_check = ifelse(!is.na(dtr_infill), round(dtr_infill,5) == round(dtr_gapfilled,5), NA)) %>% # in manual check, yes agrees
    # organize columns
    subset(select = c(date, airtemp_max_gapfilled, airtemp_min_gapfilled, airtemp_avg_gapfilled, dtr_gapfilled,
                      flag_1, flag_2, flag_3, source.station,
                      airtemp_avg_pval, airtemp_avg_r2, airtemp_avg_n.obs, airtemp_avg_equation, 
                      dtr_pval, dtr_r2, dtr_n.obs, dtr_equation, 
                      raw_airtemp_max, raw_airtemp_min, raw_airtemp_avg)) %>%
    # clean up names
    rename(num_obs_in_airtemp_avg_regression = airtemp_avg_n.obs,
           num_obs_in_dtr_regression = dtr_n.obs,
           source_station = source.station) %>%
    # reappend hmp# to all cols but date
    rename_at(.vars = names(.)[!grepl("date", names(.))], function(x) paste0(x, "_hmp",n))
  
  return(dat_out)
}


# prep hmp1, 2 and 3
preds_out_hmp1 <- format_hmps(preds_out_hmps, 1)
preds_out_hmp2 <- format_hmps(preds_out_hmps, 2) 
preds_out_hmp3 <- format_hmps(preds_out_hmps, 3)
preds_out_hmp_all <- left_join(preds_out_hmps[c("date", "yr", "mon", "doy", "logger")], preds_out_hmp1) %>%
  left_join(preds_out_hmp2) %>%
  left_join(preds_out_hmp3)


# -- add in qc flagging -----
# get raw and qc flagging
sdllog_flags <- subset(nwtlog_qc, station_id == "sdl", select = -c(rep, station_id, station_name, LTER_site))

sdllog_flags_crs <- subset(sdllog_flags, !grepl("hmp", local_site), select= -local_site) %>%
  gather(column, value, measurement:ncol(.)) %>%
  unite(metric, metric, column) %>%
  # clean up colnames to spread()
  mutate(metric = gsub("_mea.*$", "", metric),
         metric = ifelse(grepl("raw|_flag$", metric), paste0("raw_", metric), metric),
         metric = gsub("_raw$", "", metric)) %>%
  spread(metric, value) %>%
  # organize cols to keep
  subset(select = c(date,logger, airtemp_max_qcflag, raw_airtemp_max, raw_airtemp_max_flag,
                     airtemp_min_qcflag, raw_airtemp_min, raw_airtemp_min_flag,
                     airtemp_avg_qcflag, raw_airtemp_avg, raw_airtemp_avg_flag)) %>%
  arrange(date) %>%
  # convert raw measurements back to numeric
  mutate_at(names(.)[grepl("^raw", names(.)) & !grepl("_flag$", names(.))], as.numeric)
glimpse(sdllog_flags_crs) # seems ok

sdllog_flags_hmps <- subset(sdllog_flags, grepl("hmp", local_site)) %>%
  mutate(local_site = str_extract(local_site, "hmp.*"),
         # clean up hmp_[1-3] to keep consistent with edi logger dataset
         local_site = gsub("_", "", local_site),
         flag_max = ifelse(is.na(flag_max), FALSE, flag_max),
         flag_avg = ifelse(is.na(flag_avg), FALSE, flag_avg),
         ) %>%
  gather(column, value, measurement, flag, raw, qcflag) %>%
  unite(metric, metric, column) %>%
  # clean up 
  # clean up colnames to spread()
  mutate(metric = gsub("_mea.*$", "", metric),
         metric = ifelse(grepl("raw|_flag$", metric), paste0("raw_", metric), metric),
         metric = gsub("_raw$", "", metric)) %>%
  # join hmp info to metric
  unite(metric, metric, local_site) %>%
  spread(metric, value) %>%
  # change numeric cols back to numeric
  mutate_at(names(.)[grepl("min_h|avg_h|max_h", names(.))], as.numeric) %>%
  # remove cols used for regression
  subset(select = c(names(.)[grepl("date|log|flag|raw", names(.))]))
glimpse(sdllog_flags_hmps)

format_hmp_flags <- function(dat, n){
  #format
  flags_out <- dat[names(dat)[grepl(paste0("date|log|hmp", n), names(dat))]] %>%
    # remove # from hmp
    rename_all(function(x) gsub("_hmp[1-3]", "", x)) %>%
    # organize cols
    subset(select = names(sdllog_flags_crs)) %>%
    # add hmp# back in
    rename_at(names(.)[grepl("air", names(.))], function(x) paste0(x, "_hmp", n))
  # return
  return(flags_out)
}

flags_hmp1 <- format_hmp_flags(sdllog_flags_hmps, 1)
flags_hmp2 <- format_hmp_flags(sdllog_flags_hmps, 2)
flags_hmp3 <- format_hmp_flags(sdllog_flags_hmps, 3)
flags_hmps_all <- left_join(flags_hmp1, flags_hmp2) %>%
  left_join(flags_hmp3)


# # join flagging to regression info
# # drop raw column in predictions because that is what was used for measurement
# preds_out_crs_qflags <- left_join(preds_out_crs[!grepl("raw", names(preds_out_crs))], sdllog_flags_crs)
# glimpse(preds_out_crs_qflags) # seems ok

# need to pull raw col out of hmp_pred and add in raw from qcdat
join_hmp_flags <- function(preddat, flagdat, n){
  preddat_out <- subset(preddat, select = grepl(paste0("date|yr|mon|doy|log|hmp", n), names(preddat))) %>%
    # drop raw
    subset(select = -(grep("^raw", names(.)))) %>%
    # join actual rawdat and flags
    left_join(flagdat[grepl(paste0("date|log|hmp",n), names(flagdat))])
  return(preddat_out)
}

preds_out_hmp1_qflag <- join_hmp_flags(preds_out_hmp_all, flags_hmps_all, 1) 
preds_out_hmp2_qflag <- join_hmp_flags(preds_out_hmp_all, flags_hmps_all, 2) 
preds_out_hmp3_qflag <- join_hmp_flags(preds_out_hmp_all, flags_hmps_all, 3) 

preds_out_hmp_all_qflags <- merge(preds_out_hmp1_qflag, preds_out_hmp2_qflag) |> 
  merge(preds_out_hmp3_qflag) |> arrange(date) 
glimpse(preds_out_hmp_all_qflags) # 80 columns ... ugh. but looks okay


# -- put all together ----
edi_dat <- adjusted_dat_out[!grepl("sensors_", names(adjusted_dat_out))] %>%
  # left_join(preds_out_crs_qflags) %>%
  left_join(preds_out_hmp_all_qflags) %>%
  data.frame()
glimpse(edi_dat) #meh 114 columns

#  -- WRITE OUT -----
# dataset for mo and jhux
# write_csv(dat_out, paste0(datpath, "homogenize/sdllogger_temp_draft2022.csv"))
# saveRDS(dat_out, paste0(datpath, "homogenize/sdllogger_temp_draft2022.rds"))
# more complete dataset for sarah/edi
write_csv(edi_dat, paste0(datpath, "homogenize/nwt_sdl_homogenized_temperature_draft_2024.csv"))
saveRDS(edi_dat, paste0(datpath, "homogenize/nwt_sdl_homogenized_temperature_draft_2024.rds"))

