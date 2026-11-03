################################################################################
# QC of every 2025 source series (charts, loggers, SNOTEL, GHCN, AmeriFlux) against
# ALL the other series, with each series' usual behaviour shown behind it.
# Run after 2T and 2P, before infilling, to spot bad data by eye.
#
# For each target series and each other series ("neighbor"):
#   temp:   neighbor-based estimate = neighbor + usual monthly offset (target - neighbor)
#   precip: neighbor-based estimate = neighbor x usual monthly ratio (target / neighbor)
# The usual offsets/ratios come from ref_range (minus the review window). The k_temp / k_ppt neighbors that tracked
# the target best over the history are kept (lowest residual sd for temp, highest daily
# correlation for precip; only neighbors with data in the review window are considered), and the
# target's residual on a day = target - median of those neighbor estimates.
# The same residual is computed for the history, so the gray bands show how far the
# target USUALLY strays from its neighbors in that calendar month:
#   light band = daily residual, 1st-99th percentile (precip 0.5th-99.5th)
#   dark band  = running-mean residual (temp 15-day), 5th-95th percentile
# Review-window days outside the light band (and, for precip, off by >= flag_mm) are flagged.
# About 2% (temp) or 1% (precip) of days fall outside the light band by chance, so look
# for clusters, runs, and flagged days on which a gauge caught nothing.
#
# Sensors with no history borrow a predecessor's history (proxy_hist below);
# their panels say "history from <proxy>". Known-bad periods (exclude_hist) are left out of
# the history, and US-NR1 precip is never a neighbor (same gauge as Boulder 14 W since 2011).
# Chart precip days with qdays > 1 (multi-day accumulations) are left out.
#
# Outputs (data/plots/qc_2025_vs_neighbors/):
#   temp_airtemp_max.png, temp_airtemp_min.png   daily residual + 15-day running mean
#   precip_daily.png                              daily residual (mm)
#   precip_30day_ratio.png                        30-day total / neighbor estimate
#   flagged_days_temp.csv, flagged_days_precip.csv
#   neighbors_used.csv                            neighbors chosen for each target
# Run from the repo root (about 15 s).
# To review another period or a subset of series, set qc_start / qc_end / target_pattern /
# out_dir / out_prefix / do_precip / n_col before source()-ing this script
# (see qc_review_d1_2018.R). The review window is always left out of the history.
################################################################################

library(tidyverse)

datpath <- "daily_met/Infilling_2025/data/"
if (!exists("qc_start")) qc_start <- as.Date("2025-01-01")
if (!exists("qc_end")) qc_end <- as.Date("2025-12-31")
if (!exists("ref_range")) ref_range <- as.Date(c("2010-01-01", "2024-12-31"))
if (!exists("target_pattern")) target_pattern <- "."   # regex on series names
if (!exists("do_precip")) do_precip <- TRUE
if (!exists("out_dir")) out_dir <- paste0(datpath, "plots/qc_2025_vs_neighbors/")
if (!exists("out_prefix")) out_prefix <- ""
if (!exists("n_col")) n_col <- 3
one_year <- format(qc_start, "%Y") == format(qc_end, "%Y") && format(qc_start, "%m-%d") == "01-01" &&
  format(qc_end, "%m-%d") == "12-31"
qc_lab <- if (one_year) format(qc_start, "%Y") else paste(format(qc_start, "%b %Y"), "-", format(qc_end, "%b %Y"))
overlap <- qc_start <= ref_range[2] & qc_end >= ref_range[1]
ref_lab <- paste0(format(ref_range[1], "%Y"), "-", format(ref_range[2], "%Y"), if (overlap) paste0(" excluding ", qc_lab) else "")
date_lab <- if (one_year) "%b" else "%b %Y"
k_temp <- 8        # neighbors per target, temperature
k_ppt <- 6         # neighbors per target, precip
min_overlap <- 300 # ref-year days a neighbor must share with the target
flag_mm <- 5       # precip: a flagged day must also be off by at least this many mm
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# new sensors with no ref-year history: use the sensor they replaced for the usual offsets
proxy_hist <- c(c1_cr1000x_asp_1 = "c1_cr1000_asp_1",   # aspirated, 2024 only
                d1_cr1000_hv_1 = "d1_cr1000_hmp_1",
                d1_cr1000_hv_2 = "d1_cr1000_hmp_2")
# known-bad periods left out of the HISTORY only (the usual offsets and gray bands), so they
# do not widen the bands; the source data are not changed. D1 HMPs stopped tracking temp here.
exclude_hist <- tribble(
  ~site,             ~start,       ~end,
  "d1_cr1000_hmp_1", "2018-07-25", "2019-03-26",
  "d1_cr1000_hmp_2", "2018-07-25", "2019-03-26",
  "d1_cr1000_hmp_3", "2018-07-25", "2019-03-26") |> mutate(across(c(start, end), as.Date))
# series that are the same instrument as another series: never used as a neighbor, so one
# gauge is not counted twice. US-NR1 precip = Boulder 14 W (USW00094075) since 2011.
same_instrument_ppt <- c("US_NR1_1_1_1")

rd <- function(f) readRDS(paste0(datpath, f)) |> ungroup() |> as_tibble()
in_qc <- function(dt) dt >= qc_start & dt <= qc_end
in_ref <- function(dt) dt >= ref_range[1] & dt <= ref_range[2] & !in_qc(dt)
keep_dates <- function(d) d |> filter(in_qc(date) | in_ref(date))

# reference palette (as in qc_2025_sources.R): categorical blue; flags in the red pole
col_series <- "#2a78d6"; col_flag <- "#e34948"
band_light <- "grey88"; band_dark <- "grey72"

site_order <- function(s) {
  grp <- case_when(grepl("^c1", s) ~ 1, grepl("^d1", s) ~ 2, grepl("^sdl", s) ~ 3,
                   grepl("^US", s) & !grepl("^USC", s) ~ 4, grepl("^USC", s) ~ 6, TRUE ~ 5)
  s[order(grp, s)]
}

# ---------------- core: residual of one target vs its neighbors ----------------
# W: date x site matrix with known-bad periods removed (used for neighbors and history);
# W_qc: same without that removal (the target's own values in the review window).
neighbor_resid <- function(W, dates, target, type, k, not_neighbors = character(), W_qc = W) {
  is_ref <- in_ref(dates)
  is_qc <- in_qc(dates)
  mon <- as.integer(format(dates, "%m"))
  hist_site <- if (target %in% names(proxy_hist)) proxy_hist[[target]] else target
  # target series: its own values in the review window, its (or its proxy's) history otherwise
  x <- ifelse(is_qc, W_qc[, target], ifelse(is_ref, W[, hist_site], NA))
  cands <- setdiff(colnames(W), c(target, hist_site, not_neighbors))
  if (target %in% not_neighbors) cands <- setdiff(cands, "USW00094075")   # its twin
  cands <- cands[colSums(!is.na(W[is_qc, cands, drop = FALSE])) >= 60]

  per_nb <- lapply(cands, function(o) {
    y <- W[, o]
    both <- is_ref & !is.na(x) & !is.na(y)
    if (sum(both) < min_overlap) return(NULL)
    if (type == "temp") {
      adj <- tapply(x[both] - y[both], factor(mon[both], levels = 1:12), median)
      cnt <- tabulate(mon[both], 12); adj[cnt < 15] <- NA
      est <- y + adj[mon]
      score <- -sd(x[both] - est[both], na.rm = TRUE)          # higher = better
    } else {
      sx <- tapply(x[both], factor(mon[both], levels = 1:12), sum)
      so <- tapply(y[both], factor(mon[both], levels = 1:12), sum)
      adj <- ifelse(so >= 10, sx / so, NA)
      est <- y * adj[mon]
      score <- suppressWarnings(cor(x[both], y[both]))
    }
    list(site = o, est = as.numeric(est), score = score)
  })
  per_nb <- Filter(Negate(is.null), per_nb)
  if (length(per_nb) < 2) return(NULL)
  scores <- sapply(per_nb, `[[`, "score")
  best <- per_nb[order(-scores)][seq_len(min(k, length(per_nb)))]
  E <- sapply(best, `[[`, "est")
  n_nb <- rowSums(!is.na(E))
  est <- apply(E, 1, median, na.rm = TRUE)
  est[n_nb < 2] <- NA
  tibble(site = target, date = dates, is_ref = is_ref, is_qc = is_qc, month = mon,
         value = x, est = est, n_nb = n_nb, resid = x - est,
         hist_from = ifelse(hist_site == target, NA, hist_site),
         neighbors = paste(sapply(best, `[[`, "site"), collapse = ", "))
}

run_mean <- function(v, w, min_n) {
  s <- stats::filter(replace_na(v, 0), rep(1, w), sides = 2)
  n <- stats::filter(as.numeric(!is.na(v)), rep(1, w), sides = 2)
  as.numeric(ifelse(n >= min_n, s / n, NA))
}
run_sum_pair <- function(x, e, w) {   # 30-day sums over days with both present
  ok <- !is.na(x) & !is.na(e)
  sx <- stats::filter(ifelse(ok, x, 0), rep(1, w), sides = 2)
  se <- stats::filter(ifelse(ok, e, 0), rep(1, w), sides = 2)
  n <- stats::filter(as.numeric(ok), rep(1, w), sides = 2)
  list(sx = as.numeric(sx), se = as.numeric(se), n = as.numeric(n))
}

panel_label <- function(d) d |> group_by(site) |>
  mutate(lab = paste0(site, "\n", ifelse(is.na(first(hist_from)), "",
                                         paste0("history from ", first(hist_from), "; ")),
                      round(median(n_nb[is_qc & !is.na(resid)]), 0), " neighbors/day")) |> ungroup()

theme_qc <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "grey85"),
        strip.text = element_text(hjust = 0, size = 8.5))

# ---------------- TEMPERATURE ----------------
temp <- bind_rows(lapply(paste0("qc/", c("nwtchartTEMP_ready.rds", "nwtloggerTEMP_ready.rds", "snotelTEMP_ready.rds",
                                         "ghcndTEMP_ready.rds", "amerifluxTEMP_ready.rds")),
                         function(f) rd(f) |> select(local_site, date, metric, measurement))) |>
  filter(metric %in% c("airtemp_max", "airtemp_min"), !is.na(measurement)) |> keep_dates()

temp_res <- list(); temp_flags <- list(); nb_used <- list()
metric_lab <- c(airtemp_max = "daily max", airtemp_min = "daily min")
for (m in c("airtemp_max", "airtemp_min")) {
  wide <- temp |> filter(metric == m) |>
    pivot_wider(id_cols = date, names_from = local_site, values_from = measurement, values_fn = mean) |>
    complete(date = seq(min(date), max(date), by = "day")) |> arrange(date)
  W_qc <- as.matrix(wide[, -1]); dates <- wide$date
  W <- W_qc   # known-bad periods removed: never a neighbor, never part of anyone's history
  for (i in seq_len(nrow(exclude_hist))) if (exclude_hist$site[i] %in% colnames(W))
    W[dates >= exclude_hist$start[i] & dates <= exclude_hist$end[i], exclude_hist$site[i]] <- NA
  targets <- colnames(W_qc)[colSums(!is.na(W_qc[in_qc(dates), , drop = FALSE])) >= 60]
  targets <- targets[grepl(target_pattern, targets)]

  r <- bind_rows(lapply(targets, function(s) neighbor_resid(W, dates, s, "temp", k_temp, W_qc = W_qc))) |>
    group_by(site) |> arrange(date) |> mutate(run15 = run_mean(resid, 15, 10)) |> ungroup()
  bands <- r |> filter(is_ref) |> group_by(site, month) |>
    summarise(n_hist = sum(!is.na(resid)),
              d_lo = quantile(resid, 0.01, na.rm = TRUE), d_hi = quantile(resid, 0.99, na.rm = TRUE),
              r_lo = quantile(run15, 0.05, na.rm = TRUE), r_hi = quantile(run15, 0.95, na.rm = TRUE),
              .groups = "drop") |>
    mutate(across(d_lo:r_hi, ~ ifelse(n_hist >= 20, .x, NA)))
  d <- r |> filter(is_qc) |> left_join(bands, by = c("site", "month")) |>
    mutate(flag = !is.na(resid) & !is.na(d_lo) & (resid < d_lo | resid > d_hi),
           site = factor(site, levels = site_order(unique(site)))) |> panel_label()
  d$lab <- factor(d$lab, levels = unique(d$lab[order(d$site)]))
  # mark known-bad periods that fall in the review window
  ex <- exclude_hist |> inner_join(distinct(d, site = as.character(site), lab), by = "site") |>
    filter(end >= qc_start, start <= qc_end) |>
    mutate(start = pmax(start, qc_start), end = pmin(end, qc_end)) |>
    pivot_longer(c(start, end), values_to = "x")

  nb_used[[m]] <- r |> distinct(site, hist_from, neighbors) |> mutate(variable = m)
  temp_flags[[m]] <- d |> filter(flag) |>
    transmute(site = as.character(site), metric = m, date, value, neighbor_est = round(est, 1),
              resid = round(resid, 1), usual_lo = round(d_lo, 1), usual_hi = round(d_hi, 1), n_nb)

  clamp <- function(v) pmax(pmin(v, 12), -12)
  p <- ggplot(d, aes(date)) +
    geom_rect(aes(xmin = date - 0.5, xmax = date + 0.5, ymin = clamp(d_lo), ymax = clamp(d_hi)), fill = band_light, na.rm = TRUE) +
    geom_rect(aes(xmin = date - 0.5, xmax = date + 0.5, ymin = clamp(r_lo), ymax = clamp(r_hi)), fill = band_dark, na.rm = TRUE) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_vline(aes(xintercept = x), data = ex, linetype = "dashed", colour = "grey30", linewidth = 0.4) +
    geom_point(aes(y = clamp(resid)), data = ~ filter(.x, !flag), size = 0.5, alpha = 0.45, colour = col_series, na.rm = TRUE) +
    geom_point(aes(y = clamp(resid)), data = ~ filter(.x, flag), size = 1.3, colour = col_flag, na.rm = TRUE) +
    geom_line(aes(y = clamp(run15)), colour = col_series, linewidth = 0.7, na.rm = TRUE) +
    facet_wrap(~lab, ncol = n_col, scales = "free_y") +
    scale_x_date(date_breaks = "2 months", date_labels = date_lab, expand = expansion(0.01)) +
    labs(title = sprintf("%s %s temperature: each series minus what its best-matching neighbors predict", qc_lab, metric_lab[[m]]),
         subtitle = sprintf(paste0("Points = daily residual (red = outside the usual daily range); line = 15-day running mean. ",
                                   "Gray bands = usual range for that month in %s:\nlight = daily residual 1st-99th percentile, ",
                                   "dark = 15-day running mean 5th-95th percentile. A healthy series stays inside the dark band. ",
                                   "Values clipped at +/-12 C.%s"), ref_lab,
                            if (nrow(ex) > 0) "\nDashed lines = known-bad period (exclude_hist): not used as a neighbor or as history." else ""),
         x = NULL, y = "Series - neighbor estimate (C)") +
    theme_qc
  n_pan <- n_distinct(d$lab)
  ggsave(sprintf("%s%stemp_%s.png", out_dir, out_prefix, m), p, width = 15, height = 1.6 + 2.1 * ceiling(n_pan / n_col),
         dpi = 120, bg = "white", limitsize = FALSE)
}
write_csv(bind_rows(temp_flags) |> arrange(site, metric, date), paste0(out_dir, out_prefix, "flagged_days_temp.csv"))
pflags <- tibble()
if (do_precip) {

# ---------------- PRECIP ----------------
ppt <- rd("infill/allPPTdats_wNWTqdays.rds") |>
  filter(is.na(qdays) | qdays <= 1) |>          # drop chart multi-day accumulations
  select(local_site, date, measurement) |> filter(!is.na(measurement)) |> keep_dates()
wide <- ppt |> pivot_wider(id_cols = date, names_from = local_site, values_from = measurement, values_fn = mean) |>
  complete(date = seq(min(date), max(date), by = "day")) |> arrange(date)
W <- as.matrix(wide[, -1]); dates <- wide$date
targets <- colnames(W)[colSums(!is.na(W[in_qc(dates), , drop = FALSE])) >= 60]
targets <- targets[grepl(target_pattern, targets)]

pr <- bind_rows(lapply(targets, function(s) neighbor_resid(W, dates, s, "ppt", k_ppt, same_instrument_ppt))) |>
  group_by(site) |> arrange(date) |>
  mutate(rs = list(run_sum_pair(value, est, 30))) |>
  mutate(sx = rs[[1]]$sx, se = rs[[1]]$se, n30 = rs[[1]]$n) |> select(-rs) |>
  mutate(ratio30 = ifelse(n30 >= 20 & se >= 5, sx / se, NA)) |> ungroup()
pbands <- pr |> filter(is_ref) |> group_by(site, month) |>
  summarise(n_hist = sum(!is.na(resid)),
            d_lo = quantile(resid, 0.005, na.rm = TRUE), d_hi = quantile(resid, 0.995, na.rm = TRUE),
            r_lo = quantile(log2(ratio30), 0.05, na.rm = TRUE), r_hi = quantile(log2(ratio30), 0.95, na.rm = TRUE),
            .groups = "drop") |>
  mutate(across(d_lo:r_hi, ~ ifelse(n_hist >= 20 & is.finite(.x), .x, NA)))
pd <- pr |> filter(is_qc) |> left_join(pbands, by = c("site", "month")) |>
  mutate(flag = !is.na(resid) & !is.na(d_lo) & (resid < d_lo | resid > d_hi) & abs(resid) >= flag_mm,
         zero_catch = flag & value <= 0.3,
         site = factor(site, levels = site_order(unique(site)))) |> panel_label()
pd$lab <- factor(pd$lab, levels = unique(pd$lab[order(pd$site)]))

# neighbor values on flagged days, so each row can be checked without opening the data
nb_vals <- function(s, dt) {
  nbs <- strsplit(pr$neighbors[match(s, pr$site)], ", ")[[1]]
  v <- W[match(dt, dates), nbs, drop = FALSE]
  paste(sprintf("%s=%s", nbs, ifelse(is.na(v), "NA", format(round(v, 1)))), collapse = "; ")
}
pflags <- pd |> filter(flag) |>
  transmute(site = as.character(site), date, value, neighbor_est = round(est, 1), resid = round(resid, 1),
            usual_lo = round(d_lo, 1), usual_hi = round(d_hi, 1), zero_catch, n_nb) |>
  mutate(neighbor_values = map2_chr(site, date, nb_vals)) |> arrange(site, date)
write_csv(pflags, paste0(out_dir, out_prefix, "flagged_days_precip.csv"))
nb_used[["ppt"]] <- pr |> distinct(site, hist_from, neighbors) |> mutate(variable = "ppt_tot")

n_pan <- n_distinct(pd$lab)
p <- ggplot(pd, aes(date)) +
  geom_rect(aes(xmin = date - 0.5, xmax = date + 0.5, ymin = d_lo, ymax = d_hi), fill = band_light, na.rm = TRUE) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_point(aes(y = resid), data = ~ filter(.x, !flag), size = 0.6, alpha = 0.5, colour = col_series, na.rm = TRUE) +
  geom_point(aes(y = resid, shape = zero_catch), data = ~ filter(.x, flag), size = 1.8, colour = col_flag, na.rm = TRUE) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4), labels = c("Flagged", "Flagged, gauge caught ~0"), name = NULL) +
  facet_wrap(~lab, ncol = n_col, scales = "free_y") +
  scale_x_date(date_breaks = "2 months", date_labels = date_lab, expand = expansion(0.01)) +
  labs(title = sprintf("%s daily precip: each gauge minus what its best-matching neighbors predict", qc_lab),
       subtitle = sprintf(paste0("Gray band = usual daily residual for that month in %s (0.5th-99.5th percentile). ",
                                 "Red = outside it and off by >= %d mm (listed in flagged_days_precip.csv with neighbor values).\n",
                                 "Negative = gauge caught less than its neighbors. Chart days that are multi-day accumulations are left out."),
                         ref_lab, flag_mm),
       x = NULL, y = "Gauge - neighbor estimate (mm)") +
  theme_qc + theme(legend.position = "top")
ggsave(paste0(out_dir, out_prefix, "precip_daily.png"), p, width = 15, height = 1.6 + 2.1 * ceiling(n_pan / n_col), dpi = 120, bg = "white")

p <- ggplot(pd, aes(date)) +
  geom_rect(aes(xmin = date - 0.5, xmax = date + 0.5, ymin = 2^r_lo, ymax = 2^r_hi), fill = band_dark, alpha = 0.7, na.rm = TRUE) +
  geom_hline(yintercept = 1, colour = "grey45", linewidth = 0.3) +
  geom_line(aes(y = ratio30), colour = col_series, linewidth = 0.7, na.rm = TRUE) +
  facet_wrap(~lab, ncol = n_col) +
  scale_y_continuous(trans = "log2", breaks = c(0.25, 0.5, 1, 2, 4), labels = c("1/4", "1/2", "1", "2", "4")) +
  coord_cartesian(ylim = c(0.2, 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = date_lab, expand = expansion(0.01)) +
  labs(title = sprintf("%s precip: centered 30-day total / 30-day neighbor estimate", qc_lab),
       subtitle = sprintf(paste0("Gray band = usual range of that ratio for the month in %s (5th-95th percentile). ",
                                 "1 = caught what its neighbors predict. Runs below the band suggest under-catch or a stuck/clogged gauge;\n",
                                 "above it, over-catch or misplaced values. Only days with both gauge and estimate count; ",
                                 "shown when the 30-day estimate is >= 5 mm."), ref_lab),
       x = NULL, y = "30-day ratio (log scale)") +
  theme_qc
ggsave(paste0(out_dir, out_prefix, "precip_30day_ratio.png"), p, width = 15, height = 1.6 + 2.1 * ceiling(n_pan / n_col), dpi = 120, bg = "white")
}
write_csv(bind_rows(nb_used) |> select(variable, site, hist_from, neighbors), paste0(out_dir, out_prefix, "neighbors_used.csv"))

cat("Flagged temp days:", nrow(bind_rows(temp_flags)), " flagged precip days:", nrow(pflags),
    if (do_precip) paste0("(", sum(pflags$zero_catch), " with ~0 catch)") else "(precip skipped)", "\n")
cat("Wrote:", paste(list.files(out_dir), collapse = ", "), "\n")
