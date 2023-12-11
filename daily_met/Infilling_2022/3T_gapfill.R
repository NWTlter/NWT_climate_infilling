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
#' 

#TODO
# A lot of this QCing should be moved to the "qc" script.


rm(list=ls())

# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

setwd("~/NWTLTER/NWT_climate_infilling/")

source("daily_met/R/fetch_data_functions.R") # to read in prepped tidy datasets
source("daily_met/R/dataviz_functions.R")
source("daily_met/R/temp_infill_functions.R")

# set path to prepared data
datpath <- "~/OneDrive - UCB-O365/nwt_climate/data/"
# list all rds files in qc'd data folder (full path directory)
rdsfiles_ready <- list.files(paste0(datpath, "qc"), pattern = "ready", full.names = T)
# list all rds files in qc'd data folder (full path directory)
rdsfiles_qc <- list.files(paste0(datpath, "qc"), pattern = "rds", full.names = T)

################################################################################
# Read in Data
################################################################################

charttemp_ready <- get_tidydat("chartTEMP", rdsfiles_ready, "*")
allnwtlog_ready <- get_tidydat("loggerTEMP", rdsfiles_ready, "*")
ameriflux_ready <- get_tidydat("fluxT", rdsfiles_ready, "*")
snotel_ready <- get_tidydat("telT", rdsfiles_ready, "*")
ghcnd_ready <- get_tidydat("ghcndT", rdsfiles_ready, "*")
allsites <- readRDS(rdsfiles_qc[grep("infoTEM", rdsfiles_qc)])

################################################################################
# Prep for regressions
################################################################################

# need data to have date, yr, doy, local_site (unique ID), metric, and measurement
charttemp_lm <- mean_and_diurnalT(charttemp_ready)
nwtlog_lm <- mean_and_diurnalT(allnwtlog_ready)
snotel_lm <- mean_and_diurnalT(snotel_ready)
ameriflux_lm <- mean_and_diurnalT(ameriflux_ready) 
ghcnd_lm <- mean_and_diurnalT(subset(ghcnd_ready, metric != "TOBS"))

# need date, yr, mon, doy, local_site, and measurement (qc'd)
alldats <- rbind(charttemp_lm, nwtlog_lm, snotel_lm, ameriflux_lm, ghcnd_lm) %>%
  subset(grepl("avg|DTR", metric)) %>%
  arrange(local_site, date, metric)

# make sure allsites ordered by pair rank
allsites <- arrange(allsites, local_site, final_rank, paired_site)


################################################################################
# Infilling!
################################################################################
# -- C1 Chart 2019 - 2022

c1_chart_missing <- idInfillDates(alldats, "c1_chart", 2019) 
c1_chart_order <- with(allsites, paired_site[grepl("c1_chart", local_site) & final_rank!= 1])
# moving window infill
c1_chart_season <- tk_temp_movingfill(alldats, target_site = "c1_chart", 
                                      missing_dates = c1_chart_missing, site_order =  c1_chart_order)
# historic infill
c1_chart_historic <- tk_temp_historicfill(alldats, "c1_chart", c1_chart_missing, c1_chart_order)
# model selection for sdl21x
select_c1_chart <- select_model(c1_chart_historic, c1_chart_season)

# calculate regression-derived tmin and tmax
c1_chart_predicted <- calculate_minmax(select_c1_chart, charttemp_lm, "c1_chart")


c1_chart_predicted$flagmax <- with(c1_chart_predicted, airtemp_max <= airtemp_min | airtemp_avg >= airtemp_max)
c1_chart_predicted$flagmin <- with(c1_chart_predicted, airtemp_avg <= airtemp_min)

ggplot(c1_chart_predicted %>% dplyr::filter(yr > 2018)) +
  geom_line(aes(date, airtemp_avg))+
  geom_point(data = c1_chart_predicted %>% dplyr::filter(yr > 2018 & airtemp_avg_method != 'raw'), 
             aes(date, airtemp_avg, color = 'predicted'))+
  geom_line(aes(date, airtemp_max, color = 'max'), alpha = 0.5)+
  geom_point(data = c1_chart_predicted %>% dplyr::filter(yr > 2018 & airtemp_max_method != 'raw'), 
             aes(date, airtemp_max, color = 'predicted'), alpha = 0.5)+
  geom_line(aes(date, airtemp_min, color = 'min'), alpha = 0.5)+
  geom_point(data = c1_chart_predicted %>% dplyr::filter(yr > 2018 & airtemp_min_method != 'raw'), 
             aes(date, airtemp_min, color = 'predicted'), alpha = 0.5)

# predicted c1 chart avg with sdl and d1 plotted beneath to viz check infilled
# pred weird data points

ggplot(c1_chart_predicted |> dplyr::filter(yr > 2018))+
  geom_line(data = alldats|>dplyr::filter(local_site == 'd1_cr1000_hmp_1' & metric == 'airtemp_avg'),
            aes(date, measurement, color = 'd1_cr1000_hmp_1'), alpha = 0.5)+
  geom_line(data = alldats|>dplyr::filter(local_site == 'sdl_cr1000_hmp_2' & metric == 'airtemp_avg'),
            aes(date, measurement, color = 'sdl_cr1000_hmp_2'), alpha = 0.8)+
  geom_line(aes(date, airtemp_avg, color = 'c1_chart'))+
  scale_color_manual(values = c('firebrick', 'black', 'black'))+
  xlim(c(lubridate::date("2020-01-01"), lubridate::date("2020-12-31")))+
  theme(legend.position = 'bottom')+
  labs(y='TempC', x = 'Date')+
  ggtitle('Gapfilled C1 Hygrothermagraph with D1 and Sdl Logger Raw')

# -- d1 Chart 2019 - 2022

d1_chart_missing <- idInfillDates(alldats, "d1_chart", 2019) 
d1_chart_order <- with(allsites, paired_site[grepl("d1_chart", local_site) & final_rank!= 1])
# moving window infill
d1_chart_season <- tk_temp_movingfill(alldats, target_site = "d1_chart", 
                                      missing_dates = d1_chart_missing, site_order =  d1_chart_order)
# historic infill
d1_chart_historic <- tk_temp_historicfill(alldats, "d1_chart", d1_chart_missing, d1_chart_order)
# model selection for sdl21x
select_d1_chart <- select_model(d1_chart_historic, d1_chart_season)

# calculate regression-derived tmin and tmax
d1_chart_predicted <- calculate_minmax(select_d1_chart, charttemp_lm, "d1_chart")


d1_chart_predicted$flagmax <- with(d1_chart_predicted, airtemp_max <= airtemp_min | airtemp_avg >= airtemp_max)
d1_chart_predicted$flagmin <- with(d1_chart_predicted, airtemp_avg <= airtemp_min)

ggplot(d1_chart_predicted %>% dplyr::filter(yr > 2018)) +
  geom_line(aes(date, airtemp_avg))+
  geom_point(data = d1_chart_predicted %>% dplyr::filter(yr > 2018 & airtemp_avg_method != 'raw'), 
             aes(date, airtemp_avg, color = 'predicted'))+
  geom_line(aes(date, airtemp_max, color = 'max'), alpha = 0.5)+
  # geom_point(data = d1_chart_predicted %>% dplyr::filter(yr > 2018 & airtemp_max_method != 'raw'), 
  #            aes(date, airtemp_max, color = 'predicted'), alpha = 0.5)+
  geom_line(aes(date, airtemp_min, color = 'min'), alpha = 0.5)
  # geom_point(data = d1_chart_predicted %>% dplyr::filter(yr > 2018 & airtemp_min_method != 'raw'), 
  #            aes(date, airtemp_min, color = 'predicted'), alpha = 0.5)

ggplot(d1_chart_predicted |> dplyr::filter(yr > 2018))+
  geom_line(data = alldats|>dplyr::filter(local_site == 'c1_cr1000_hmp_1' & metric == 'airtemp_avg'),
            aes(date, measurement, color = 'c1_cr1000_hmp_1'), alpha = 0.5)+
  geom_line(data = alldats|>dplyr::filter(local_site == 'sdl_cr1000_hmp_2' & metric == 'airtemp_avg'),
            aes(date, measurement, color = 'sdl_cr1000_hmp_2'), alpha = 0.8)+
  geom_line(aes(date, airtemp_avg, color = 'd1_chart'))+
  scale_color_manual(values = c('firebrick', 'black', 'black'))+
  xlim(c(lubridate::date("2022-01-01"), lubridate::date("2022-12-31")))+
  theme(legend.position = 'bottom')+
  labs(y='TempC', x = 'Date')+
  ggtitle('Gapfilled d1 Hygrothermagraph with D1 and Sdl Logger Raw')


# -- INFILL SDL HMPs -----

# -- sdl hmp 1 ----
sdl1000_hmp_1_missing <- idInfillDates(alldats, "sdl_cr1000_hmp_1", 2019)
sdl1000_hmp_1_order <- with(allsites, paired_site[grepl("sdl_cr1000_hmp_1", local_site) & final_rank!= 1])
# moving window infill
sdl1000_hmp_1_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr1000_hmp_1", missing_dates = sdl1000_hmp_1_missing, site_order =  sdl1000_hmp_1_order)
# historic infill
sdl1000_hmp_1_historic <- tk_temp_historicfill(alldats, "sdl_cr1000_hmp_1", sdl1000_hmp_1_missing, sdl1000_hmp_1_order)
# selection for hmp1
select_sdl1000_hmp_1 <- select_model(sdl1000_hmp_1_historic, sdl1000_hmp_1_season)
# calculate derived tmin and tmax
sdl1000_hmp_1_predicted <- calculate_minmax(select_sdl1000_hmp_1, allnwtlog_ready, "sdl_cr1000_hmp_1")

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
sdl1000_hmp_2_missing <- idInfillDates(alldats, "sdl_cr1000_hmp_2", 2022)
sdl1000_hmp_2_order <- with(allsites, paired_site[grepl("sdl_cr1000_hmp_2", local_site) & final_rank!= 1])
# moving window infill
sdl1000_hmp_2_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr1000_hmp_2", missing_dates = sdl1000_hmp_2_missing, site_order =  sdl1000_hmp_2_order)
# historic infill
sdl1000_hmp_2_historic <- tk_temp_historicfill(alldats, "sdl_cr1000_hmp_2", sdl1000_hmp_2_missing, sdl1000_hmp_2_order)
# selection for hmp2
select_sdl1000_hmp_2 <- select_model(sdl1000_hmp_2_historic, sdl1000_hmp_2_season)
# calculate regression derived tmax and min
sdl1000_hmp_2_predicted <- calculate_minmax(select_sdl1000_hmp_2, allnwtlog_ready, "sdl_cr1000_hmp_2")


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
sdl1000_hmp_3_missing <- idInfillDates(alldats, "sdl_cr1000_hmp_3", 2022)
sdl1000_hmp_3_order <- with(allsites, paired_site[grepl("sdl_cr1000_hmp_3", local_site) & final_rank!= 1])
# moving window infill
sdl1000_hmp_3_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr1000_hmp_3", missing_dates = sdl1000_hmp_3_missing, site_order =  sdl1000_hmp_3_order)
# historic infill
sdl1000_hmp_3_historic <- tk_temp_historicfill(alldats, "sdl_cr1000_hmp_3", sdl1000_hmp_3_missing, sdl1000_hmp_3_order)
# selection for hmp3
select_sdl1000_hmp_3 <- select_model(sdl1000_hmp_3_historic, sdl1000_hmp_3_season)
# calculate regression derived tmin and tmax
sdl1000_hmp_3_predicted <- calculate_minmax(select_sdl1000_hmp_3, allnwtlog_ready, "sdl_cr1000_hmp_3")
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
sdl_hmps_predicted <- rbind(sdl1000_hmp_1_predicted, sdl1000_hmp_2_predicted, sdl1000_hmp_3_predicted) %>%
  arrange(date, local_site) %>%
  subset(select = -c(flagmax, flagmin, flag_airtemp_avg, airtemp_avg_infill)) %>%
  distinct()

# -- WRITE OUT FOR HOMOGENIZATION ----

# -- WRITE OUT
write.csv(alldats, file = "daily_met/Infilling_2022/other_dats/alldats.csv",
          row.names = FALSE)
write.csv(c1_chart_predicted, file = "daily_met/Infilling_2022/c1/c1_chart_infilled_v1.csv",
          row.names = FALSE)
write.csv(d1_chart_predicted, file = "daily_met/Infilling_2022/d1/d1_chart_infilled_v1.csv",
          row.names = FALSE)
write.csv(sdl_hmps_predicted, file = "daily_met/Infilling_2022/sdl/sdl_hmps_infilled_v1.csv",
          row.names = FALSE)
# saveRDS(allsdllog_predicted, paste0(datpath,"infill/sdlhmp_infilled_2023.rds"))



