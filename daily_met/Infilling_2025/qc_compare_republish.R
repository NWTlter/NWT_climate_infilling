# Review plots: 2025 republished series vs the currently posted EDI series, for all six datasets.
# Annual and seasonal (DJF, MAM, JJA, SON) means of Tmax/Tmean/Tmin and totals of precip, old and
# new overlaid, from 1980 (SDL from its record start). Dotted line = start of the 2025 re-infill.
# Run after 6_build_full_record.R, from the repo root. Not part of the pipeline.
# Writes data/plots/qc_review/republish_vs_posted_<station>.png

rm(list=ls())
library(tidyverse)
source("daily_met/R/fetch_data_functions.R")

datpath <- "daily_met/Infilling_2025/data/"
plotpath <- paste0(datpath, "plots/qc_review/")
fullpath <- paste0(datpath, "publish/full_record/")

# edi id, full-record file, station, start of 2025 re-infill, and the columns to compare
datasets <- tribble(
  ~edi_id, ~file,                                      ~station, ~new_start,
  185,     "c1_infilled_temp_daily.tk.data.csv",       "C1",     "1980-01-01",
  184,     "c1_infilled_precip_daily.tk.data.csv",     "C1",     "1980-01-01",
  187,     "d1_infilled_temp_daily.tk.data.csv",       "D1",     "1980-01-01",
  186,     "d1_infilled_precip_daily.tk.data.csv",     "D1",     "1980-01-01",
  314,     "sdl_daily_airtemp_gapfilled.cw.data.csv",  "Saddle", "1986-12-06",
  315,     "sdl_daily_precip_gapfilled.cw.data.csv",   "Saddle", "1987-06-01"
)
vars <- c(max_temp = "Tmax", mean_temp = "Tmean", min_temp = "Tmin", precip = "Precip",
          airtemp_max_homogenized = "Tmax", airtemp_avg_homogenized = "Tmean",
          airtemp_min_homogenized = "Tmin")
first_yr <- 1980

# daily -> long (date, variable, value)
tidy_daily <- function(dat) {
  dat %>%
    mutate(date = as.Date(date)) %>%
    select(date, any_of(names(vars))) %>%
    pivot_longer(-date, names_to = "variable") %>%
    mutate(variable = vars[variable])
}

daily <- lapply(seq_len(nrow(datasets)), function(i) {
  d <- datasets[i, ]
  bind_rows(tidy_daily(getTabular(d$edi_id)) %>% mutate(version = "posted (old infill)"),
            tidy_daily(read.csv(paste0(fullpath, d$file))) %>% mutate(version = "2025 re-infill")) %>%
    mutate(station = d$station, new_start = as.Date(d$new_start))
}) %>% bind_rows()

# annual and seasonal (Dec counts toward the next year's DJF); keep complete periods only
summarise_period <- function(dat, period_fn) {
  dat %>%
    mutate(period = period_fn(date),
           yr = as.numeric(format(date, "%Y")) + (period == "DJF" & format(date, "%m") == "12")) %>%
    filter(yr >= first_yr) %>%
    group_by(station, variable, version, period, yr) %>%
    summarise(n = sum(!is.na(value)),
              ndays = n(),
              value = ifelse(variable[1] == "Precip", sum(value, na.rm = TRUE), mean(value, na.rm = TRUE)),
              .groups = "drop") %>%
    # complete: every day present (annual >= 365, season >= 90)
    filter(n == ndays, ndays >= ifelse(period == "Annual", 365, 90))
}
season_of <- function(date) {
  c("DJF", "DJF", "MAM", "MAM", "MAM", "JJA", "JJA", "JJA", "SON", "SON", "SON", "DJF")[as.numeric(format(date, "%m"))]
}
periods <- bind_rows(summarise_period(daily, function(date) rep("Annual", length(date))),
                     summarise_period(daily, season_of)) %>%
  mutate(period = factor(period, levels = c("Annual", "DJF", "MAM", "JJA", "SON")),
         variable = factor(variable, levels = c("Tmax", "Tmean", "Tmin", "Precip")))

# biggest differences, for the console
diffs <- periods %>%
  pivot_wider(id_cols = c(station, variable, period, yr), names_from = version, values_from = value) %>%
  mutate(diff = `2025 re-infill` - `posted (old infill)`)
diffs %>% group_by(station, variable, period) %>%
  summarise(mean_diff = round(mean(diff, na.rm = TRUE), 2), max_abs_diff = round(max(abs(diff), na.rm = TRUE), 2),
            yr_max = yr[which.max(abs(diff))], .groups = "drop") %>%
  print(n = Inf)

starts <- distinct(daily, station, variable, new_start)
for (s in unique(datasets$station)) {
  pd <- subset(periods, station == s)
  ggplot(pd, aes(yr, value, col = version)) +
    geom_vline(data = subset(starts, station == s), aes(xintercept = as.numeric(format(new_start, "%Y")) + as.numeric(format(new_start, "%j")) / 366),
               lty = 3) +
    geom_line(alpha = 0.8) +
    geom_point(size = 0.7) +
    scale_color_manual(values = c("posted (old infill)" = "grey45", "2025 re-infill" = "#d95f02")) +
    facet_wrap(variable ~ period, ncol = 5, scales = "free_y") +
    labs(title = paste(s, "- 2025 re-infill vs posted EDI series (annual and seasonal; temp = mean of daily values in deg C, precip = total in mm)"),
         subtitle = "Dotted line: start of the 2025 re-infill. Only complete periods are shown.",
         x = NULL, y = NULL, col = NULL) +
    theme_bw() +
    theme(legend.position = "top", strip.text = element_text(size = 8))
  ggsave(paste0(plotpath, "republish_vs_posted_", casefold(s), ".png"), width = 16, height = 11)
}
