################################################################################
#' ---
#' title: Saddle homogenized logger series for re-infilling (1986-2017)
#' ---
#'
#' The Saddle logger record before the HMPs (CR21x 1986-2000, CR23x 2000-2012,
#' CR1000 2012-2017) is CTW's homogenized series on EDI (knb-lter-nwt.314): each
#' logger's raw values plus a constant adjustment per logger and variable (CR21x 0),
#' with CTW's manual fixes (e.g. 1996-97 values shifted -1 day).
#' This script:
#' 1. Reads 314 and keeps only values that were measured (raw + adjustment); values
#'    CTW infilled are set to NA so 3T can re-infill them with the 2025 methods.
#' 2. Re-screens the series with the 2025 all-logger QC pass (2T, "QUICK QC ALL DATA",
#'    the pass applied to the 21x/23x era): it takes the place of the raw sdl_cr21x,
#'    sdl_cr23x and sdl_cr1000 in that pass.
#' 3. Plots the flagged candidates for review. They are not removed until reviewed
#'    (list approved dates in `reviewed_remove`).
#' Run after 2T, from the repo root. Writes data/qc/sdlhomogTEMP_qc.rds.
################################################################################

rm(list=ls())
library(tidyverse)
source("daily_met/R/fetch_data_functions.R")

datpath <- "daily_met/Infilling_2025/data/"
plotpath <- "daily_met/Infilling_2025/data/plots/qc_review/"
# last day of the pre-HMP loggers (3T infills the HMPs from 2018)
sdl_homog_end <- as.Date("2017-12-31")

# QC candidates approved for removal after review (date + metric). Reviewed 2026-10-26 (plot in plotpath):
# kept 1990-07-03 max (cold front at all sites), 1993-07-05 min and 1996-09-27 min (neighbors as cold);
# removed the two CR21x minima below, 4-8 C colder than the saddle chart and d1
reviewed_remove <- data.frame(date = as.Date(c("1992-08-30", "1992-08-31")),
                              metric = c("airtemp_min", "airtemp_min"))


# -- 1. CTW HOMOGENIZED SERIES, MEASURED VALUES ONLY -----
ctw <- getTabular(314) %>%
  mutate(date = as.Date(date)) %>%
  subset(date <= sdl_homog_end)

sdl_homog <- lapply(c("max", "min", "avg"), function(v){
  raw <- ctw[[paste0("raw_airtemp_", v)]]
  gapfilled <- ctw[[paste0("airtemp_", v, "_gapfilled")]]
  qcflag <- ctw[[paste0("airtemp_", v, "_qcflag")]]
  # measured = CTW kept the raw value, or shifted it a day (manual fix); anything else was infilled
  measured <- (!is.na(raw) & abs(gapfilled - raw) < 1e-3) | grepl("shifted", qcflag)
  data.frame(date = ctw$date, ctw_logger = ctw$logger, metric = paste0("airtemp_", v),
             ctw_homogenized = ctw[[paste0("airtemp_", v, "_homogenized")]],
             ctw_adjustment = ctw[[paste0("airtemp_", v, "_adjustment")]],
             ctw_qcflag = qcflag, ctw_infilled = !measured,
             # CTW's raw (unadjusted) logger value and its EDI flag, carried to the published columns
             ctw_raw = raw, ctw_raw_flag = ctw[[paste0("raw_airtemp_", v, "_flag")]])
}) %>%
  bind_rows() %>%
  mutate(measurement = ifelse(ctw_infilled, NA, ctw_homogenized),
         yr = as.numeric(format(date, "%Y")), mon = as.numeric(format(date, "%m")),
         doy = as.numeric(format(date, "%j")),
         LTER_site = "NWT", local_site = "sdl_homog", logger = "homogenized",
         station_id = "sdl", station_name = "sdl logger") %>%
  arrange(date, metric)

# how much is left to infill?
with(sdl_homog, table(metric, ctw_infilled))
with(subset(sdl_homog, ctw_infilled), tapply(date, yr, function(x) length(unique(x))))


# -- 2. 2025 ALL-LOGGER QC PASS -----
# same pass as 2T (QUICK QC ALL DATA), on the QC'd loggers with sdl_homog in place of the raw sdl loggers
nwtlog_ready <- readRDS(paste0(datpath, "qc/nwtloggerTEMP_ready.rds"))
qc_pool <- subset(nwtlog_ready, !local_site %in% c("sdl_cr21x", "sdl_cr23x", "sdl_cr1000"),
                  select = c(date, yr, mon, doy, metric, measurement, local_site, station_name)) %>%
  rbind(sdl_homog[names(.)]) %>%
  arrange(local_site, metric, date)

qc_pool <- group_by(qc_pool, date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] <= measurement[metric == "airtemp_min"],
         flag_avg = measurement[metric == "airtemp_avg"] <= measurement[metric == "airtemp_min"] | measurement[metric == "airtemp_avg"] >= measurement[metric == "airtemp_max"]) %>%
  ungroup()

qc_dailysds <- qc_pool %>%
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
  mutate(flag_avgpresent = all(is.na(measurement[metric != "airtemp_avg"])) &
           !is.na(measurement[metric == "airtemp_avg"])) %>%
  ungroup()
qc_dailysds$flag_ws <- apply(qc_dailysds[grep("ws_sc", names(qc_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 4) == 2)
qc_dailysds$flag_xs <- apply(qc_dailysds[grep("xs_sc", names(qc_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 4) == 2)

# flag first, and then undo flag by # of flags per date (as in 2T)
qc_dailysds <- qc_dailysds %>%
  mutate(qcflag2 = xs_deltarank <= 15 | ws_deltarank <= 15 | xs_temprank <= 15 | ws_temprank <= 15 |
           flag_xs %in% TRUE | flag_ws %in% TRUE | flag_max %in% TRUE | flag_avg %in% TRUE) %>%
  group_by(date) %>%
  mutate(postcheck = sum(qcflag2)) %>%
  ungroup() %>%
  mutate(qcflag2 = qcflag2 & postcheck <= 1)

# candidates on the saddle series
qc_candidates <- subset(qc_dailysds, local_site == "sdl_homog" & (qcflag2 | flag_avgpresent)) %>%
  mutate(reason = paste0(ifelse(flag_avgpresent, "avg present without max/min; ", ""),
                         ifelse(flag_max %in% TRUE, "max <= min; ", ""),
                         ifelse(flag_avg %in% TRUE, "avg outside min-max; ", ""),
                         ifelse(xs_deltarank <= 15 | flag_xs %in% TRUE, "day-to-day change vs other loggers; ", ""),
                         ifelse(ws_deltarank <= 15 | flag_ws %in% TRUE, "day-to-day change vs saddle loggers; ", ""),
                         ifelse(xs_temprank <= 15 | ws_temprank <= 15, "extreme value; ", ""))) %>%
  left_join(distinct(sdl_homog[c("date", "metric", "ctw_logger")]), by = c("date", "metric")) %>%
  dplyr::select(date, metric, ctw_logger, measurement, reason) %>%
  arrange(date, metric)
print(qc_candidates, n = Inf)


# -- 3. PLOT CANDIDATES FOR REVIEW -----
if(nrow(qc_candidates) > 0){
  # +/- 10 days around each candidate: saddle series, saddle chart, and d1/c1 loggers
  nwtchart_ready <- readRDS(paste0(datpath, "qc/nwtchartTEMP_ready.rds"))
  windows <- distinct(qc_candidates["date"]) %>% mutate(event = format(date))
  compdat <- rbind(subset(qc_pool, grepl("sdl_homog|d1_cr|c1_cr", local_site) & !grepl("hmp|_as|_hv", local_site),
                          select = c(date, metric, measurement, local_site)),
                   subset(nwtchart_ready, local_site %in% c("sdl_chart", "d1_chart"),
                          select = c(date, metric, measurement, local_site)))
  plotdat <- lapply(seq_len(nrow(windows)), function(i){
    subset(compdat, abs(as.numeric(date - windows$date[i])) <= 10) %>% mutate(event = windows$event[i])
  }) %>% bind_rows()
  ggplot(plotdat, aes(date, measurement, col = local_site)) +
    geom_line(aes(lwd = local_site == "sdl_homog")) +
    geom_point(data = mutate(qc_candidates, event = format(date), local_site = "sdl_homog"), col = "red", size = 2.5) +
    scale_linewidth_manual(values = c(0.4, 1.1), guide = "none") +
    facet_grid(metric ~ event, scales = "free") +
    labs(title = "Saddle homogenized series: 2025 QC candidates (red), with neighbors",
         y = "deg C", x = NULL, col = NULL) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, size = 6))
  ggsave(paste0(plotpath, "sdl_homog_qc_candidates.png"), width = max(8, 2.2 * nrow(windows)), height = 7, limitsize = FALSE)
  write.csv(qc_candidates, paste0(plotpath, "sdl_homog_qc_candidates.csv"), row.names = FALSE)
}


# -- WRITE OUT -----
# NA reviewed removals; avg goes with a removed max or min (as in 2T)
removed <- paste(sdl_homog$date, sdl_homog$metric) %in% paste(reviewed_remove$date, reviewed_remove$metric)
removed_avg <- sdl_homog$metric == "airtemp_avg" & sdl_homog$date %in% reviewed_remove$date
sdl_homog$qcflag <- ifelse(removed | removed_avg, "value removed for within-site or spatial coherency deviation (2025 QC)", NA)
sdl_homog$measurement[removed | removed_avg] <- NA

saveRDS(sdl_homog, paste0(datpath, "qc/sdlhomogTEMP_qc.rds"))
