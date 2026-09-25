################################################################################
# QC of the 2025 source data (what the infilling draws FROM), after 2T/2P.
#
# For each sensor, daily differences vs several references, so a sensor that has
# gone bad shows up as drift, a step, or spikes against references that stay flat:
#   - mean of the OTHER sensors at its own site (C1, D1, Saddle; regional stations
#     are compared with the mean of the other regional stations)
#   - C1 mean, D1 mean (leaving the sensor itself out)
#   - Boulder 14 W (USW00094075, the USCRN station on Niwot Ridge)
#   - Daymet v4 (1 km gridded) at the sensor's own location. Daymet interpolates
#     GHCN/SNOTEL stations, so it is not independent for those; it is for NWT sensors.
# Offsets between sites change with the seasons (e.g. D1 min runs much colder than C1 in
# winter), so each sensor's 2025 offset is compared with ITS OWN usual offset for the same
# calendar month in earlier years (hist_start..2024). Sensors with no earlier history
# (e.g. the new D1 hv sensors) fall back to their 2025 median offset, marked "*".
#
# Outputs (data/plots/qc_2025/):
#   temp_<group>_<metric>.png  small multiples: sensor rows x reference columns
#   temp_monthly_departure_heatmap.png  |monthly offset - annual median offset|
#   temp_monthly_departure.csv
#   precip_cumulative_ratio.png  cumulative 2025 precip / cumulative reference
# Run from the repo root after 2T and 2P (needs internet for Daymet on first run;
# Daymet downloads are cached in data/raw/Daymet/).
################################################################################

library(tidyverse)

yr_qc <- 2025
hist_start <- 2018   # earlier years used to define each sensor's usual monthly offset
datpath <- "daily_met/Infilling_2025/data/"
out_dir <- paste0(datpath, "plots/qc_", yr_qc, "/")
daymet_dir <- paste0(datpath, "raw/Daymet/")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(daymet_dir, recursive = TRUE, showWarnings = FALSE)

rd <- function(f) readRDS(paste0(datpath, "qc/", f)) |> ungroup() |> as_tibble()
in_year <- function(d) d |> filter(format(date, "%Y") == as.character(yr_qc))
in_span <- function(d) d |> filter(date >= as.Date(paste0(hist_start, "-01-01")), date <= as.Date(paste0(yr_qc, "-12-31")))

# reference palette: categorical slots in fixed order; diverging blue <-> red, gray midpoint
pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4")
div_lo <- "#2a78d6"; div_mid <- "#f0efec"; div_hi <- "#e34948"

# ---------------- site groups & coordinates ----------------
group_of <- function(site) case_when(
  grepl("^c1", site) ~ "C1",
  grepl("^d1", site) ~ "D1",
  grepl("^sdl", site) ~ "Saddle",
  TRUE ~ "Regional")

siteinfo <- bind_rows(
  rd("siteinfoTEMP_qc.rds") |> distinct(local_site, latitude, longitude),
  rd("siteinfoPPT_qc.rds") |> distinct(local_site, latitude, longitude) |> mutate(local_site = gsub("-", "_", local_site))
) |> distinct(local_site, .keep_all = TRUE) |> mutate(across(c(latitude, longitude), as.numeric))

# ---------------- Daymet (cached per location) ----------------
get_daymet <- function(lat, lon) {
  f <- sprintf("%sdaymet_%d-%d_%.4f_%.4f.csv", daymet_dir, hist_start, yr_qc, lat, lon)
  if (!file.exists(f)) {
    url <- sprintf("https://daymet.ornl.gov/single-pixel/api/data?lat=%.4f&lon=%.4f&vars=tmax,tmin,prcp&years=%s",
                   lat, lon, paste(hist_start:yr_qc, collapse = ","))
    download.file(url, f, quiet = TRUE)
  }
  read_csv(f, skip = 6, show_col_types = FALSE) |>
    transmute(date = as.Date(paste(year, yday), "%Y %j"),
              airtemp_max = `tmax (deg c)`, airtemp_min = `tmin (deg c)`, ppt = `prcp (mm/day)`)
}
locs <- siteinfo |> distinct(latitude, longitude) |> filter(!is.na(latitude))
daymet <- locs |> mutate(dm = map2(latitude, longitude, get_daymet)) |> unnest(dm)
daymet_site <- siteinfo |> inner_join(daymet, by = c("latitude", "longitude"), relationship = "many-to-many") |> select(-latitude, -longitude)

# ---------------- TEMPERATURE ----------------
temp <- bind_rows(lapply(c("nwtchartTEMP_ready.rds", "nwtloggerTEMP_ready.rds", "snotelTEMP_ready.rds",
                           "ghcndTEMP_ready.rds", "amerifluxTEMP_ready.rds"),
                         function(f) rd(f) |> select(local_site, date, metric, measurement))) |>
  filter(metric %in% c("airtemp_max", "airtemp_min"), !is.na(measurement)) |> in_span() |>
  mutate(group = group_of(local_site))
sensors <- temp |> in_year() |> distinct(local_site, group)

# leave-one-out mean of a set of sensors, for each sensor
loo_mean <- function(pool_sites, label) {
  pool <- temp |> filter(local_site %in% pool_sites)
  tot <- pool |> group_by(date, metric) |> summarise(s = sum(measurement), n = n(), .groups = "drop")
  sensors |> select(local_site) |> cross_join(tot) |>
    left_join(pool |> select(local_site, date, metric, own = measurement), by = c("local_site", "date", "metric")) |>
    mutate(s = s - coalesce(own, 0), n = n - !is.na(own)) |>
    filter(n > 0) |> transmute(local_site, date, metric, ref = label, ref_val = s / n)
}
ref_own <- bind_rows(lapply(unique(sensors$group), function(g) {
  loo_mean(sensors$local_site[sensors$group == g], "Own-site others") |>
    filter(local_site %in% sensors$local_site[sensors$group == g])
}))
refs <- bind_rows(
  ref_own,
  loo_mean(sensors$local_site[sensors$group == "C1"], "C1 mean"),
  loo_mean(sensors$local_site[sensors$group == "D1"], "D1 mean"),
  temp |> filter(local_site == "USW00094075") |> select(date, metric, ref_val = measurement) |>
    cross_join(sensors |> select(local_site)) |> filter(local_site != "USW00094075") |> mutate(ref = "Boulder 14 W"),
  daymet_site |> in_span() |> pivot_longer(c(airtemp_max, airtemp_min), names_to = "metric", values_to = "ref_val") |>
    select(local_site, date, metric, ref_val) |> mutate(ref = "Daymet (own cell)")
)
ref_levels <- c("Own-site others", "C1 mean", "D1 mean", "Boulder 14 W", "Daymet (own cell)")

dall <- temp |> filter(local_site %in% sensors$local_site) |>
  inner_join(refs, by = c("local_site", "date", "metric")) |>
  mutate(diff = measurement - ref_val, ref = factor(ref, levels = ref_levels),
         yr = as.numeric(format(date, "%Y")), month = factor(format(date, "%b"), levels = month.abb))

# usual offset: median over earlier years of each month's mean offset (need >= 10 days in a month)
usual_hist <- dall |> filter(yr < yr_qc) |>
  group_by(local_site, metric, ref, month, yr) |> summarise(n = n(), m = mean(diff), .groups = "drop") |>
  filter(n >= 10) |> group_by(local_site, metric, ref, month) |>
  summarise(usual = median(m), n_years = n(), .groups = "drop")

dd <- dall |> filter(yr == yr_qc) |>
  left_join(usual_hist, by = c("local_site", "metric", "ref", "month")) |>
  group_by(local_site, metric, ref) |> arrange(date) |>
  mutate(has_hist = !is.na(usual), usual = ifelse(has_hist, usual, median(diff)),
         run15 = as.numeric(stats::filter(diff, rep(1 / 15, 15), sides = 2))) |> ungroup()

metric_lab <- c(airtemp_max = "daily max", airtemp_min = "daily min")
for (g in c("C1", "D1", "Saddle", "Regional")) for (m in c("airtemp_max", "airtemp_min")) {
  d <- dd |> filter(group == g, metric == m)
  if (nrow(d) == 0) next
  nrow_s <- n_distinct(d$local_site)
  p <- ggplot(d, aes(date, diff)) +
    geom_step(aes(y = usual), linetype = "dashed", colour = "grey35", linewidth = 0.45) +
    geom_point(size = 0.4, alpha = 0.35, colour = pal[1]) +
    geom_line(aes(y = run15), colour = pal[1], linewidth = 0.7, na.rm = TRUE) +
    facet_grid(local_site ~ ref, scales = "free_y") +
    scale_x_date(date_breaks = "3 months", date_labels = "%b") +
    labs(title = sprintf("%s sensors, %s %s: difference from each reference", g, yr_qc, metric_lab[[m]]),
         subtitle = sprintf("Points = daily difference (sensor - reference); line = 15-day running mean; dashed = sensor's usual offset for that month in %d-%d\n(or its %d median if it has no history). A healthy sensor tracks its dashed line; drift, steps or clusters of spikes suggest a problem.", hist_start, yr_qc - 1, yr_qc),
         x = NULL, y = "Sensor - reference (C)") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), strip.text.y = element_text(angle = 0, hjust = 0),
          panel.border = element_rect(fill = NA, colour = "grey85"))
  ggsave(sprintf("%stemp_%s_%s.png", out_dir, tolower(g), m), p,
         width = 14, height = 1.6 + 1.5 * nrow_s, dpi = 120, bg = "white", limitsize = FALSE)
}

# monthly departure of each sensor's offset from its usual offset
mon <- dd |> filter(ref %in% c("Boulder 14 W", "Daymet (own cell)", "Own-site others")) |>
  mutate(month = factor(format(date, "%b"), levels = month.abb)) |>
  group_by(group, local_site, metric, ref, month) |>
  summarise(n = n(), departure = mean(diff) - first(usual), has_hist = first(has_hist), .groups = "drop") |> filter(n >= 10)
write_csv(mon |> mutate(departure = round(departure, 2)), paste0(out_dir, "temp_monthly_departure.csv"))

hm <- mon |> mutate(metric = metric_lab[metric],
                    local_site = factor(local_site, levels = rev(sort(unique(local_site)))),
                    lab = paste0(ifelse(abs(departure) >= 2, sprintf("%.0f", departure), ""), ifelse(has_hist, "", "*")))
p <- ggplot(hm, aes(month, local_site, fill = pmax(pmin(departure, 5), -5))) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = lab), size = 2.6, colour = "grey15") +
  facet_grid(metric ~ ref) +
  scale_fill_gradient2(low = div_lo, mid = div_mid, high = div_hi, midpoint = 0, limits = c(-5, 5),
                       name = "Departure (C)\n(clipped at +/-5)") +
  labs(title = sprintf("%s: each sensor's monthly offset vs each reference, minus its usual offset for that month (%d-%d)", yr_qc, hist_start, yr_qc - 1),
       subtitle = sprintf("Gray = behaving as usual. Numbers where the month departs by >= 2 C. * = no earlier history, compared with its own %s median instead. Blank = < 10 days of data.", yr_qc),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) + theme(panel.grid = element_blank())
ggsave(paste0(out_dir, "temp_monthly_departure_heatmap.png"), p, width = 15, height = 11, dpi = 120, bg = "white")

# ---------------- PRECIP ----------------
ppt <- readRDS(paste0(datpath, "infill/allPPTdats_wNWTqdays.rds")) |> ungroup() |> as_tibble() |>
  select(local_site, date, measurement) |> filter(!is.na(measurement)) |> in_year()
psites <- unique(ppt$local_site)
# regional leave-one-out mean on days where both are present, then cumulative ratio
tot <- ppt |> group_by(date) |> summarise(s = sum(measurement), n = n(), .groups = "drop")
pr <- ppt |> left_join(tot, by = "date") |>
  mutate(loo = (s - measurement) / (n - 1)) |>
  left_join(daymet_site |> select(local_site, date, dm = ppt), by = c("local_site", "date")) |>
  group_by(local_site) |> arrange(date) |>
  mutate(`All other gauges (mean)` = cumsum(measurement) / cumsum(loo),
         `Daymet (own cell)` = ifelse(!is.na(dm), cumsum(measurement * !is.na(dm)) / cumsum(coalesce(dm, 0)), NA),
         cum = cumsum(measurement)) |> ungroup() |>
  pivot_longer(c(`All other gauges (mean)`, `Daymet (own cell)`), names_to = "ref", values_to = "ratio") |>
  filter(is.finite(ratio), date >= as.Date(paste0(yr_qc, "-01-15")))   # ratios are noisy in the first days

p <- ggplot(pr, aes(date, ratio, colour = ref)) +
  geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~local_site, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = setNames(pal[1:2], c("All other gauges (mean)", "Daymet (own cell)")), name = NULL) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b") +
  labs(title = sprintf("%s precip: cumulative total so far / cumulative reference total", yr_qc),
       subtitle = "A healthy gauge settles to a roughly flat line (its level reflects local climate). Bends or steps suggest under-catch,\nclogging, a stuck gauge, or a burst of bad values. Only days with data at both the gauge and the reference are counted.",
       x = NULL, y = "Cumulative ratio") +
  theme_minimal(base_size = 10) + theme(legend.position = "top", panel.grid.minor = element_blank(),
                                        panel.border = element_rect(fill = NA, colour = "grey85"))
ggsave(paste0(out_dir, "precip_cumulative_ratio.png"), p, width = 13, height = 12, dpi = 120, bg = "white")

cat("Wrote:", paste(list.files(out_dir), collapse = ", "), "\n")
