# Compare the 2025 re-infill (C1/D1 temp and precip, 1980 on) with the previously
# published infilled datasets on EDI (knb-lter-nwt.184-187).
# Not part of the pipeline: run after 3T/3P, from the repo root.
#
# Each day/metric is classed as raw or infilled in the old and new versions:
#   both raw            -> values should match; differences mean the raw data or QC changed
#   old raw, new infill -> new QC removed the raw value (e.g. q flags, CTW QC)
#   old infill, new raw -> a raw value is now available/kept
#   both infilled       -> compare values and source stations
# Writes csvs + plots to data/plots/qc_compare_old_infill/ and prints a summary.

rm(list=ls())
library(tidyverse)
source("daily_met/R/fetch_data_functions.R")

datpath <- "daily_met/Infilling_2025/data/"
out_dir <- paste0(datpath, "plots/qc_compare_old_infill/")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
yrs <- 1980:2024 # new infill starts 1980; old datasets end 2024

# old published infilled data, cached so reruns don't re-download
old_rds <- paste0(datpath, "raw/old_infilled_edi.rds")
if (!file.exists(old_rds)) saveRDS(getNWTchartsinfilled(), old_rds)
old <- readRDS(old_rds)

# the posted tables and the drafts name the same source differently (Niwot Snotel (663) / Niwot (663) /
# Niwot, C1-cr1000 / c1_cr1000, 053496 / USC00053496, US-NR1 / US_NR1_1_1_1, SDL / sdl_chart), so
# compare sources on a common key; the csvs keep the names as published
src_key <- function(x) {
  x <- gsub(" snotel| \\([0-9]+\\)| logger", "", tolower(x))
  x <- sub("^0([0-9]{5})$", "\\1", sub("^us[cw]00", "", x)) # GHCNd: last 5 digits of the ID
  x <- gsub("[ _-]", "", x)
  x <- sub("^(usnr[0-9])[0-9]+$", "\\1", x)                  # AmeriFlux tower, not sensor position
  sub("^(c1|d1|sdl)$", "\\1chart", x)                       # bare site = its chart
}
src_changed <- function(old_src, new_src) src_key(old_src) != src_key(new_src)

# -- temperature -----
# flag_2: A raw, B both predicted, B* tmax failed, C tmax adjusted, D tmin adjusted
old_t <- map_dfr(c("C1", "D1"), function(s) {
  old[[paste0(s, "temp_infilled")]] %>%
    transmute(site = s, date = as.Date(date), airtemp_max = max_temp, airtemp_min = min_temp,
              old_src = as.character(source_station),
              old_inf_max = flag_2 %in% c("B", "B*", "C"), old_inf_min = flag_2 %in% c("B", "B*", "D"))
}) %>%
  pivot_longer(c(airtemp_max, airtemp_min), names_to = "metric", values_to = "old") %>%
  mutate(old_infilled = ifelse(metric == "airtemp_max", old_inf_max, old_inf_min)) %>%
  select(-old_inf_max, -old_inf_min)

new_t <- map_dfr(c("c1", "d1"), function(s) {
  readRDS(paste0(datpath, "infill/", s, "_chart_infilled_v1.rds")) %>%
    transmute(site = toupper(s), date = as.Date(date), airtemp_max, airtemp_min,
              new_src = as.character(source.station),
              new_inf_max = airtemp_max_method != "raw", new_inf_min = airtemp_min_method != "raw")
}) %>%
  pivot_longer(c(airtemp_max, airtemp_min), names_to = "metric", values_to = "new") %>%
  mutate(new_infilled = ifelse(metric == "airtemp_max", new_inf_max, new_inf_min)) %>%
  select(-new_inf_max, -new_inf_min)

classify <- function(df) {
  df %>% mutate(
    class = case_when(!old_infilled & !new_infilled ~ "both raw",
                      !old_infilled & new_infilled ~ "old raw, new infill",
                      old_infilled & !new_infilled ~ "old infill, new raw",
                      TRUE ~ "both infilled"),
    diff = new - old,
    period = cut(year(date), c(1979, 1989, 2010, 2018, 2024), labels = c("1980-89", "1990-2010", "2011-18", "2019-24")))
}

cmp_t <- inner_join(old_t, new_t, by = c("site", "date", "metric")) %>%
  filter(year(date) %in% yrs) %>%
  classify()

temp_summary <- cmp_t %>%
  group_by(site, metric, period, class) %>%
  summarise(n = n(), mean_diff = mean(diff, na.rm = TRUE), sd_diff = sd(diff, na.rm = TRUE),
            n_absdiff_gt_2 = sum(abs(diff) > 2, na.rm = TRUE),
            n_source_changed = sum(class == "both infilled" & src_changed(old_src, new_src), na.rm = TRUE),
            .groups = "drop")
write.csv(temp_summary, paste0(out_dir, "temp_summary.csv"), row.names = FALSE)
write.csv(filter(cmp_t, class != "both raw" | abs(diff) > 0.05), paste0(out_dir, "temp_changed_days.csv"), row.names = FALSE)

# source stations used, old vs new, on days infilled in both
src_t <- cmp_t %>% filter(class == "both infilled", metric == "airtemp_max") %>%
  count(site, period, old_src, new_src, sort = TRUE)
write.csv(src_t, paste0(out_dir, "temp_source_changes.csv"), row.names = FALSE)

p <- ggplot(filter(cmp_t, class != "both raw"), aes(date, diff, color = class)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_point(size = 0.6, alpha = 0.6) +
  facet_grid(site ~ metric) +
  labs(y = "new - old (C)", title = "Temperature: new 2025 infill minus previously published, days not raw in both")
ggsave(paste0(out_dir, "temp_diff_timeseries.png"), p, width = 12, height = 7)
p <- ggplot(filter(cmp_t, class == "both infilled"), aes(old, new)) +
  geom_abline(color = "grey50") +
  geom_point(size = 0.6, alpha = 0.5) +
  facet_grid(site ~ metric) + coord_equal() +
  labs(x = "previously published infill (C)", y = "new infill (C)", title = "Temperature: days infilled in both versions")
ggsave(paste0(out_dir, "temp_old_vs_new.png"), p, width = 9, height = 9)

# -- precipitation -----
old_p <- map_dfr(c("C1", "D1"), function(s) {
  old[[paste0(s, "ppt_infilled")]] %>%
    transmute(site = s, date = as.Date(date), old = precip, old_src = as.character(source_station),
              old_infilled = flag_1 != "A")
})
new_p <- map_dfr(c("c1", "d1"), function(s) {
  readRDS(paste0(datpath, "infill/", s, "PPT_infilled_draft.rds")) %>%
    transmute(site = toupper(s), date = as.Date(date), new = precip, new_src = as.character(source_station),
              new_infilled = flag_1 != "A")
})
cmp_p <- inner_join(old_p, new_p, by = c("site", "date")) %>%
  filter(year(date) %in% yrs) %>%
  mutate(metric = "precip") %>%
  classify()
ppt_summary <- cmp_p %>%
  group_by(site, period, class) %>%
  summarise(n = n(), old_mm = sum(old, na.rm = TRUE), new_mm = sum(new, na.rm = TRUE),
            n_absdiff_gt_5mm = sum(abs(diff) > 5, na.rm = TRUE),
            n_source_changed = sum(class == "both infilled" & src_changed(old_src, new_src), na.rm = TRUE),
            .groups = "drop")
write.csv(ppt_summary, paste0(out_dir, "precip_summary.csv"), row.names = FALSE)
write.csv(filter(cmp_p, class != "both raw" | abs(diff) > 0.05), paste0(out_dir, "precip_changed_days.csv"), row.names = FALSE)
ppt_annual <- cmp_p %>% group_by(site, yr = year(date)) %>%
  summarise(old_mm = sum(old), new_mm = sum(new), .groups = "drop") %>%
  mutate(diff_mm = new_mm - old_mm, pct = 100 * diff_mm / old_mm)
write.csv(ppt_annual, paste0(out_dir, "precip_annual.csv"), row.names = FALSE)
p <- ggplot(ppt_annual, aes(yr, diff_mm)) +
  geom_col() + facet_wrap(~site, ncol = 1) +
  labs(y = "new - old annual total (mm)", title = "Precipitation: annual total, new minus previously published")
ggsave(paste0(out_dir, "precip_annual_diff.png"), p, width = 10, height = 6)

# -- print summary -----
options(width = 160)
cat("\n== Temperature, 1980-2024: days by class ==\n")
print(as.data.frame(cmp_t %>% count(site, metric, class) %>% pivot_wider(names_from = class, values_from = n, values_fill = 0)), row.names = FALSE)
cat("\n== Temperature: both raw but values differ (> 0.05 C) ==\n")
print(as.data.frame(cmp_t %>% filter(class == "both raw") %>% group_by(site, metric, period) %>%
                      summarise(n_differ = sum(abs(diff) > 0.05, na.rm = TRUE), n_new_na = sum(is.na(new)), .groups = "drop")), row.names = FALSE)
cat("\n== Temperature: days infilled in both versions ==\n")
print(as.data.frame(filter(temp_summary, class == "both infilled")), digits = 2, row.names = FALSE)
cat("\n== Temperature: most common source changes (days infilled in both, tmax) ==\n")
print(as.data.frame(head(filter(src_t, src_changed(old_src, new_src)), 20)), row.names = FALSE)
cat("\n== Precipitation, 1980-2024 ==\n")
print(as.data.frame(ppt_summary), digits = 3, row.names = FALSE)
cat("\n== Precipitation: years where the annual total changes by > 5% ==\n")
print(as.data.frame(filter(ppt_annual, abs(pct) > 5)), digits = 3, row.names = FALSE)
cat("\nWrote plots/csvs to", out_dir, "\n")
