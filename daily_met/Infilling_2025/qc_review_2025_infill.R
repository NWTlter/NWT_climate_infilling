# Scientific review of the 2025 infilled values (C1, D1, SDL temp and precip).
# Not part of the pipeline: run after 5_prep_v1_forEDI*.R, from the repo root.
#
# Temperature: each site's daily anomaly (vs its own 2010-2024 seasonal cycle; SDL's
# homogenized record starts 2018, so 2018-2024 there) is
# compared with the mean anomaly of independent neighbors (SNOTEL, Boulder 14 W,
# US-NR1). Infilled days should track the region about as well as raw days do.
# Precip: 2025 monthly/annual totals vs the site's own record and vs neighbors, and
# each infilled day vs what the neighbors caught that day.
# Writes plots + csvs to data/plots/qc_review_2025_infill/ and prints a summary.

rm(list=ls())
library(tidyverse)

datpath <- "daily_met/Infilling_2025/data/"
out_dir <- paste0(datpath, "plots/qc_review_2025_infill/")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

review_yr <- 2025
clim_yrs <- 2010:2024
nb_temp <- c("Niwot", "UniversityCamp", "LakeEldora", "HighLonesome", "Sawtooth",
             "USW00094075", "US_NR1_1_1_1")
nb_ppt <- c("Niwot", "UniversityCamp", "LakeEldora", "HighLonesome", "Sawtooth",
            "USW00094075", "NPN")

# smooth seasonal cycle: 2-harmonic fit on clim_yrs, returns anomaly
add_anom <- function(df) {
  df %>%
    group_by(site, metric) %>%
    group_modify(function(d, k) {
      d <- mutate(d, s1 = sin(2*pi*doy/365.25), c1 = cos(2*pi*doy/365.25),
                  s2 = sin(4*pi*doy/365.25), c2 = cos(4*pi*doy/365.25))
      fit <- lm(value ~ s1 + c1 + s2 + c2, data = subset(d, yr %in% clim_yrs))
      mutate(d, anom = value - predict(fit, newdata = d)) %>% select(-s1, -c1, -s2, -c2)
    }) %>% ungroup()
}

# -- 1. temperature: full records -----
# c1/d1: full infilled chart records (3T), 2025 flags from the publish files
chart_full <- map_dfr(c("c1", "d1"), function(s) {
  readRDS(paste0(datpath, "infill/", s, "_chart_infilled_v1.rds")) %>%
    transmute(date = as.Date(date), site = toupper(s), airtemp_max, airtemp_min)
})
pub_cd <- map_dfr(c("c1", "d1"), function(s) {
  read.csv(paste0(datpath, "publish/", s, "_daily_airtemp_gapfilled_ongoing.csv")) %>%
    transmute(date = as.Date(date), site = toupper(s), max_pub = max_temp, min_pub = min_temp,
              flag_2, source_station, raw_Tmax, raw_Tmin,
              r2_mean = t_mean_rsquared, r2_dtr = TDTR_rsquared,
              # flag_2: B both predicted, B* tmax failed, C tmax adjusted, D tmin adjusted
              infill_max = flag_2 %in% c("B", "B*", "C"),
              infill_min = flag_2 %in% c("B", "B*", "D"))
})
sdl_pub <- read.csv(paste0(datpath, "publish/sdl_daily_airtemp_gapfilled_ongoing.csv")) %>%
  mutate(date = as.Date(date))
sdl_full <- readRDS(paste0(datpath, "homogenize/nwt_sdl_homogenized_temperature_draft.rds")) %>%
  transmute(date = as.Date(date), site = "SDL",
            airtemp_max = airtemp_max_homogenized, airtemp_min = airtemp_min_homogenized)
# sdl: a day counts as infilled if any of the 3 HMPs was infilled that day
sdl_flags <- sdl_pub %>%
  transmute(date, site = "SDL", max_pub = airtemp_max_homogenized, min_pub = airtemp_min_homogenized,
            flag_2, source_station = paste(source_station_hmp1, source_station_hmp2, source_station_hmp3, sep = "|"),
            raw_Tmax = NA_real_, raw_Tmin = NA_real_,
            r2_mean = pmin(airtemp_avg_rsquared_hmp1, airtemp_avg_rsquared_hmp2, airtemp_avg_rsquared_hmp3, na.rm = TRUE),
            r2_dtr = pmin(dtr_rsquared_hmp1, dtr_rsquared_hmp2, dtr_rsquared_hmp3, na.rm = TRUE),
            infill_max = flag_2 != "A", infill_min = flag_2 != "A")

target <- bind_rows(chart_full, sdl_full) %>%
  pivot_longer(c(airtemp_max, airtemp_min), names_to = "metric", values_to = "value") %>%
  mutate(yr = year(date), doy = yday(date)) %>%
  filter(!is.na(value))

# alldats holds airtemp_avg and DTR only; rebuild max/min from them
alldats_maxmin <- read.csv(paste0(datpath, "infill/alldats.csv")) %>%
  filter(metric %in% c("airtemp_avg", "DTR")) %>%
  pivot_wider(names_from = metric, values_from = measurement) %>%
  mutate(airtemp_max = airtemp_avg + DTR/2, airtemp_min = airtemp_avg - DTR/2) %>%
  select(-airtemp_avg, -DTR) %>%
  pivot_longer(c(airtemp_max, airtemp_min), names_to = "metric", values_to = "measurement") %>%
  filter(!is.na(measurement))
nbrs <- alldats_maxmin %>%
  filter(local_site %in% nb_temp) %>%
  transmute(date = as.Date(date), yr, doy, site = local_site, metric, value = measurement)

anoms <- add_anom(bind_rows(target, nbrs))
regional <- anoms %>%
  filter(site %in% nb_temp) %>%
  group_by(date, metric) %>%
  summarise(reg_anom = mean(anom), n_nb = n(), .groups = "drop") %>%
  filter(n_nb >= 3)

trk <- anoms %>%
  filter(site %in% c("C1", "D1", "SDL"), yr >= 2018) %>%
  inner_join(regional, by = c("date", "metric")) %>%
  mutate(resid = anom - reg_anom)

flags_all <- bind_rows(pub_cd, sdl_flags)
trk25 <- trk %>%
  filter(yr == review_yr) %>%
  left_join(flags_all, by = c("date", "site")) %>%
  mutate(infilled = ifelse(metric == "airtemp_max", infill_max, infill_min),
         status = ifelse(infilled, "infilled", "raw"),
         mon = month(date))

# baseline: how well each site tracks the region on all 2018-2024 days
base <- trk %>% filter(yr < review_yr) %>%
  group_by(site, metric) %>%
  summarise(base_mean = mean(resid), base_sd = sd(resid), .groups = "drop")

trk25 <- left_join(trk25, base, by = c("site", "metric")) %>%
  mutate(z = (resid - base_mean) / base_sd)

temp_summary <- trk25 %>%
  group_by(site, metric, status) %>%
  summarise(n = n(), mean_resid = mean(resid), sd_resid = sd(resid),
            n_out3sd = sum(abs(z) > 3), .groups = "drop") %>%
  left_join(base, by = c("site", "metric"))
temp_monthly <- trk25 %>%
  group_by(site, metric, mon, status) %>%
  summarise(n = n(), mean_resid = mean(resid), .groups = "drop") %>%
  left_join(base, by = c("site", "metric"))
temp_outliers <- trk25 %>% filter(infilled, abs(z) > 3) %>%
  select(site, metric, date, value, anom, reg_anom, resid, z, flag_2, source_station) %>%
  arrange(site, metric, date)
write.csv(temp_summary, paste0(out_dir, "temp_tracking_summary.csv"), row.names = FALSE)
write.csv(temp_monthly, paste0(out_dir, "temp_tracking_monthly.csv"), row.names = FALSE)
write.csv(temp_outliers, paste0(out_dir, "temp_infilled_outliers.csv"), row.names = FALSE)

# daily: site anomaly vs regional anomaly, colored by raw/infilled
p <- ggplot(trk25, aes(date)) +
  geom_line(aes(y = reg_anom), color = "grey50") +
  geom_point(aes(y = anom, color = status), size = 0.8) +
  scale_color_manual(values = c(raw = "black", infilled = "#d95f02")) +
  facet_grid(site ~ metric) +
  labs(y = "anomaly vs 2010-2024 seasonal cycle (C)",
       title = paste(review_yr, "site anomaly (points) vs mean of independent neighbors (grey)"))
ggsave(paste0(out_dir, "temp_anomaly_vs_region.png"), p, width = 12, height = 8)

# monthly mean residual, raw vs infilled, with the site's usual +/- 1 sd band
p <- ggplot(temp_monthly, aes(mon, mean_resid, fill = status)) +
  geom_rect(aes(xmin = 0.5, xmax = 12.5, ymin = base_mean - base_sd, ymax = base_mean + base_sd),
            fill = "grey90", inherit.aes = FALSE) +
  geom_col(position = position_dodge(preserve = "single")) +
  geom_hline(aes(yintercept = base_mean), linetype = 2) +
  scale_fill_manual(values = c(raw = "grey30", infilled = "#d95f02")) +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(site ~ metric) +
  labs(y = "mean (site - regional) anomaly (C)",
       title = paste(review_yr, "monthly offset from region; grey = usual 2018-2024 mean +/- 1 sd"))
ggsave(paste0(out_dir, "temp_monthly_offset.png"), p, width = 12, height = 8)

# D1 summer: published (infilled) vs the q-flagged raw chart and the D1 HMPs
d1hmp <- alldats_maxmin %>%
  filter(grepl("^d1_cr1000_(hmp|hv)", local_site), yr == review_yr) %>%
  transmute(date = as.Date(date), metric, series = local_site, value = measurement)
d1sum <- pub_cd %>% filter(site == "D1", year(date) == review_yr) %>%
  transmute(date, airtemp_max = max_pub, airtemp_min = min_pub, raw_max = raw_Tmax, raw_min = raw_Tmin) %>%
  pivot_longer(-date) %>%
  mutate(metric = ifelse(grepl("max", name), "airtemp_max", "airtemp_min"),
         series = ifelse(grepl("raw", name), "d1_chart raw", "d1_chart published")) %>%
  select(date, metric, series, value) %>%
  bind_rows(d1hmp) %>%
  filter(month(date) %in% 4:9)
p <- ggplot(mapping = aes(date, value, color = series)) +
  geom_line(data = filter(d1sum, !grepl("chart", series)), alpha = 0.5) +
  geom_point(data = filter(d1sum, grepl("chart", series)), size = 0.9) +
  scale_color_manual(values = c("d1_chart published" = "#d95f02", "d1_chart raw" = "black",
                                d1_cr1000_hmp_1 = "#1b9e77", d1_cr1000_hmp_2 = "#7570b3",
                                d1_cr1000_hmp_3 = "#e7298a", d1_cr1000_hv_1 = "#66a61e",
                                d1_cr1000_hv_2 = "#a6761d")) +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  labs(y = "C", title = paste("D1 Apr-Sep", review_yr, ": published vs raw chart vs D1 loggers"))
ggsave(paste0(out_dir, "d1_summer_published_vs_raw.png"), p, width = 12, height = 7)

# internal consistency of published temp
consistency <- bind_rows(
  pub_cd %>% filter(year(date) == review_yr) %>% transmute(site, date, max_pub, min_pub, flag_2),
  sdl_flags %>% filter(year(date) == review_yr) %>% transmute(site, date, max_pub, min_pub, flag_2)) %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(jump_max = abs(max_pub - lag(max_pub)), jump_min = abs(min_pub - lag(min_pub)),
         edge = flag_2 != lag(flag_2)) %>%
  ungroup()
cons_summary <- consistency %>%
  group_by(site) %>%
  summarise(n_days = n(), n_missing = sum(is.na(max_pub) | is.na(min_pub)),
            n_max_lt_min = sum(max_pub < min_pub, na.rm = TRUE),
            p95_jump_max_raw = quantile(jump_max[flag_2 == "A" & !edge], 0.95, na.rm = TRUE),
            max_jump_at_raw_infill_edge = max(c(jump_max[edge], jump_min[edge]), na.rm = TRUE),
            .groups = "drop")
write.csv(cons_summary, paste0(out_dir, "temp_consistency.csv"), row.names = FALSE)

# regression strength used for infilled days
reg_summary <- flags_all %>%
  filter(year(date) == review_yr, flag_2 != "A") %>%
  group_by(site) %>%
  summarise(n_infilled = n(), median_r2_mean = median(r2_mean, na.rm = TRUE),
            min_r2_mean = min(r2_mean, na.rm = TRUE),
            n_r2_mean_lt_0.6 = sum(r2_mean < 0.6, na.rm = TRUE),
            median_r2_dtr = median(r2_dtr, na.rm = TRUE),
            n_r2_dtr_lt_0.3 = sum(r2_dtr < 0.3, na.rm = TRUE), .groups = "drop")
write.csv(reg_summary, paste0(out_dir, "temp_regression_strength.csv"), row.names = FALSE)

# -- 2. precipitation -----
ppt_full <- map_dfr(c("c1", "d1", "sdl"), function(s) {
  readRDS(paste0(datpath, "infill/", s, "PPT_infilled_draft.rds")) %>%
    transmute(date = as.Date(date), site = toupper(s), precip, flag_1)
})
ppt_pub <- map_dfr(c("c1", "d1", "sdl"), function(s) {
  read.csv(paste0(datpath, "publish/", s, "_daily_precip_gapfilled_ongoing.csv")) %>%
    transmute(date = as.Date(date), site = toupper(s), precip, flag_1, source_station,
              rsquared, raw_ppt_tot)
}) %>% filter(year(date) == review_yr)
pptnb <- readRDS(paste0(datpath, "infill/allPPTdats_wNWTqdays.rds")) %>%
  filter(local_site %in% nb_ppt, metric == "ppt_tot") %>%
  transmute(date = as.Date(date), nb = local_site, nb_precip = measurement)

# annual totals: 2025 vs the site's own record
ann <- ppt_full %>% filter(year(date) < review_yr) %>%
  group_by(site, yr = year(date)) %>%
  summarise(total = sum(precip), n = sum(!is.na(precip)), .groups = "drop") %>%
  filter(n >= 360)
ann25 <- ppt_pub %>% group_by(site) %>%
  summarise(total_2025 = sum(precip, na.rm = TRUE), n_infilled = sum(flag_1 != "A"),
            mm_infilled = sum(precip[flag_1 != "A"], na.rm = TRUE), .groups = "drop")
ppt_annual <- ann %>% filter(yr >= 1990) %>% group_by(site) %>%
  summarise(median_1990_2024 = median(total), p10 = quantile(total, 0.1),
            p90 = quantile(total, 0.9), .groups = "drop") %>%
  right_join(ann25, by = "site") %>%
  mutate(rank_in_1990_2025 = map2_int(site, total_2025, ~ sum(ann$total[ann$site == .x & ann$yr >= 1990] < .y) + 1))
write.csv(ppt_annual, paste0(out_dir, "precip_annual.csv"), row.names = FALSE)

# ratio of site to neighbor monthly totals: 2025 vs usual
mon_site <- bind_rows(ppt_full %>% filter(year(date) < review_yr, year(date) >= 2010),
                      ppt_pub %>% select(date, site, precip, flag_1)) %>%
  group_by(site, yr = year(date), mon = month(date)) %>%
  summarise(site_mm = sum(precip), infilled_mm = sum(precip[flag_1 != "A"]), .groups = "drop") %>%
  filter(!is.na(site_mm))
mon_nb <- pptnb %>% filter(year(date) >= 2010, year(date) <= review_yr) %>%
  group_by(nb, yr = year(date), mon = month(date)) %>%
  summarise(mm = sum(nb_precip, na.rm = TRUE) * n() / sum(!is.na(nb_precip)),
            n = sum(!is.na(nb_precip)), .groups = "drop") %>%
  filter(n >= 25) %>%
  group_by(yr, mon) %>% summarise(nb_mm = mean(mm), .groups = "drop")
ratio <- inner_join(mon_site, mon_nb, by = c("yr", "mon")) %>%
  mutate(ratio = (site_mm + 1) / (nb_mm + 1))
ratio_band <- ratio %>% filter(yr < review_yr) %>% group_by(site, mon) %>%
  summarise(lo = quantile(ratio, 0.1), med = median(ratio), hi = quantile(ratio, 0.9), .groups = "drop")
ratio25 <- ratio %>% filter(yr == review_yr) %>% left_join(ratio_band, by = c("site", "mon"))
write.csv(ratio25, paste0(out_dir, "precip_monthly_ratio.csv"), row.names = FALSE)
p <- ggplot(ratio25, aes(mon)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey85") +
  geom_line(aes(y = med), color = "grey40", linetype = 2) +
  geom_line(aes(y = ratio)) +
  geom_point(aes(y = ratio, size = infilled_mm, color = infilled_mm > 0)) +
  scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "#d95f02"), name = "has infilled mm") +
  scale_x_continuous(breaks = 1:12) + scale_y_log10() +
  facet_wrap(~site, ncol = 1) +
  labs(y = "(site + 1) / (neighbor mean + 1), monthly mm",
       title = paste(review_yr, "monthly site:neighbor precip ratio; grey = 2010-2024 10-90%"))
ggsave(paste0(out_dir, "precip_monthly_ratio.png"), p, width = 9, height = 9)

# each infilled day vs what the neighbors caught
inf_days <- ppt_pub %>% filter(flag_1 != "A") %>%
  left_join(pptnb %>% group_by(date) %>%
              summarise(nb_median = median(nb_precip, na.rm = TRUE), nb_max = max(nb_precip, na.rm = TRUE),
                        n_nb = sum(!is.na(nb_precip)), .groups = "drop"), by = "date") %>%
  mutate(check = case_when(precip > 2 & nb_max == 0 ~ "infilled precip, all neighbors dry",
                           precip == 0 & nb_median >= 5 ~ "infilled 0, neighbors wet",
                           precip > 3 * nb_max + 5 ~ "infilled far above every neighbor",
                           TRUE ~ "ok"))
write.csv(inf_days, paste0(out_dir, "precip_infilled_days.csv"), row.names = FALSE)

# -- 3. print summary -----
options(width = 160)
cat("\n== Temperature: (site - regional) anomaly, 2025 raw vs infilled, vs 2018-2024 baseline ==\n")
print(as.data.frame(temp_summary), digits = 2, row.names = FALSE)
cat("\n== Months where infilled days are > 1 baseline sd off the usual offset (n >= 5) ==\n")
print(as.data.frame(filter(temp_monthly, status == "infilled", n >= 5,
                           abs(mean_resid - base_mean) > base_sd)), digits = 2, row.names = FALSE)
cat("\n== Infilled temp days > 3 sd from the usual offset:", nrow(temp_outliers), "==\n")
print(as.data.frame(temp_outliers), digits = 2, row.names = FALSE)
cat("\n== Temp consistency ==\n"); print(as.data.frame(cons_summary), digits = 2, row.names = FALSE)
cat("\n== Regression strength on infilled days ==\n"); print(as.data.frame(reg_summary), digits = 2, row.names = FALSE)
cat("\n== Precip annual ==\n"); print(as.data.frame(ppt_annual), digits = 3, row.names = FALSE)
cat("\n== Precip months outside the usual site:neighbor ratio (10-90%) ==\n")
print(as.data.frame(filter(ratio25, ratio < lo | ratio > hi) %>%
                      select(site, mon, site_mm, infilled_mm, nb_mm, ratio, lo, med, hi)), digits = 2, row.names = FALSE)
cat("\n== Precip infilled days that disagree with neighbors ==\n")
print(as.data.frame(filter(inf_days, check != "ok") %>%
                      select(site, date, precip, source_station, rsquared, nb_median, nb_max, n_nb, check)),
      digits = 3, row.names = FALSE)
cat("\nWrote plots/csvs to", out_dir, "\n")
