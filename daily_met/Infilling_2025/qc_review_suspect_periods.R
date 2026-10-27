################################################################################
# Plots of source-data periods that look wrong in the 2T "ready" temperature data,
# for deciding whether to NA them before infilling:
#   - Niwot SNOTEL, ~2005-2006 (max reads 7-9 C warm vs Boulder 14 W)
#   - University Camp SNOTEL, ~2010-2011 (max reads 7-8 C warm)
#   - D1 HMPs 1-3, ~Aug 2018 - Mar 2019 (stop tracking temperature)
# Reference: Boulder 14 W (USW00094075), the USCRN station on Niwot Ridge.
# Shading marks where >= 20% of days in a 30-day window differ from Boulder 14 W by
# > 5 C more than that sensor's usual (median) difference -- a guide, not a decision.
# Run from the repo root after 2T. Writes to data/plots/qc_review/.
################################################################################

library(tidyverse)

datpath <- "daily_met/Infilling_2025/data/"
out_dir <- paste0(datpath, "plots/qc_review/")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

rd <- function(f) readRDS(paste0(datpath, "qc/", f)) |> ungroup() |> as_tibble()
temps <- bind_rows(
  rd("nwtchartTEMP_ready.rds") |> select(local_site, date, metric, measurement),
  rd("nwtloggerTEMP_ready.rds") |> select(local_site, date, metric, measurement),
  rd("snotelTEMP_ready.rds") |> select(local_site, date, metric, measurement),
  rd("ghcndTEMP_ready.rds") |> filter(metric != "TOBS") |> select(local_site, date, metric, measurement)
) |> filter(metric %in% c("airtemp_max", "airtemp_min"))

ref_site <- "USW00094075"
pretty <- c(USW00094075 = "Boulder 14 W (USCRN)", Niwot = "Niwot SNOTEL",
            UniversityCamp = "University Camp SNOTEL", LakeEldora = "Lake Eldora SNOTEL",
            d1_chart = "D1 chart", d1_cr1000_hmp_1 = "D1 HMP 1",
            d1_cr1000_hmp_2 = "D1 HMP 2", d1_cr1000_hmp_3 = "D1 HMP 3",
            sdl_cr1000_hmp_1 = "Saddle HMP 1")
metric_lab <- c(airtemp_max = "Daily max", airtemp_min = "Daily min")

# reference categorical palette, in its fixed (CVD-validated) slot order
pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300")
# styles by role: suspect series get the first palette slots AND distinct line types (so
# series that read nearly the same, e.g. the three D1 HMPs, stay visible when overlapping);
# references are neutral: Boulder 14 W always black, others dark then light gray.
series_style <- function(suspects, refs) {
  ref_other <- setdiff(refs, ref_site)
  sites <- c(suspects, refs)
  col <- c(setNames(pal[seq_along(suspects)], suspects), setNames("black", ref_site),
           setNames(c("grey45", "grey70")[seq_along(ref_other)], ref_other))[sites]
  lty <- c(setNames(c("solid", "dashed", "dotted")[seq_along(suspects)], suspects),
           setNames("solid", ref_site), setNames(c("solid", "longdash")[seq_along(ref_other)], ref_other))[sites]
  lwd <- ifelse(sites %in% suspects, 0.75, 0.5)
  list(col = setNames(unname(col), pretty[sites]), lty = setNames(unname(lty), pretty[sites]),
       lwd = setNames(lwd, pretty[sites]), levels = pretty[sites])
}
# drawing order: references first, then suspects 1, 2, 3 -- so dashed/dotted suspects sit on
# top of the solid one and it shows through their gaps
draw_order <- function(st, suspects) c(setdiff(st$levels, pretty[suspects]), pretty[suspects])
style_scales <- function(st) list(
  scale_colour_manual(values = st$col, name = NULL, drop = TRUE),
  scale_linetype_manual(values = st$lty, name = NULL, drop = TRUE),
  scale_linewidth_manual(values = st$lwd, guide = "none"),
  guides(colour = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 1.1)),
         linetype = guide_legend(nrow = 1, byrow = TRUE)))

roll <- function(x, k) as.numeric(stats::filter(x, rep(1 / k, k), sides = 2))
# centred running mean that skips NAs; NA where fewer than min_n values in the window
roll_na <- function(x, k, min_n) {
  ok <- !is.na(x)
  n <- as.numeric(stats::filter(as.numeric(ok), rep(1, k), sides = 2))
  tot <- as.numeric(stats::filter(ifelse(ok, x, 0), rep(1, k), sides = 2))
  ifelse(!is.na(n) & n >= min_n, tot / n, NA)
}

# flag windows where >= 20% of days depart > 5 C from the usual offset vs reference
# (catches intermittent spikes as well as steady bias)
flag_periods <- function(site, met) {
  d <- temps |> filter(local_site %in% c(site, ref_site), metric == met) |>
    select(local_site, date, measurement) |>
    pivot_wider(names_from = local_site, values_from = measurement) |>
    complete(date = seq(min(date), max(date), by = "day")) |> arrange(date)
  diff <- d[[site]] - d[[ref_site]]
  usual <- median(diff, na.rm = TRUE)
  bad <- abs(diff - usual) > 5
  frac <- roll_na(as.numeric(bad), 30, min_n = 10)
  tibble(date = d$date, diff = diff, diff30 = roll_na(diff, 30, min_n = 10),
         usual = usual, off = frac >= 0.2)
}

runs <- function(fl) {
  fl |> filter(!is.na(off)) |> mutate(r = cumsum(off != lag(off, default = FALSE))) |>
    filter(off) |> group_by(r) |> summarise(start = min(date), end = max(date), days = n(), .groups = "drop") |>
    filter(days >= 15) |> select(-r)
}

case_plot <- function(title, suspects, refs, from, to, file) {
  from <- as.Date(from); to <- as.Date(to)
  sites <- c(suspects, refs)
  st <- series_style(suspects, refs)

  top <- temps |> filter(local_site %in% sites, date >= from - 30, date <= to + 30) |>
    group_by(local_site, metric) |> arrange(date) |>
    complete(date = seq(min(date), max(date), by = "day")) |>
    mutate(smooth = roll(measurement, 7)) |> ungroup() |>
    filter(date >= from, date <= to) |>
    mutate(series = factor(pretty[local_site], levels = pretty[sites]),
           metric = metric_lab[metric])

  fl <- map_dfr(suspects, function(s) map_dfr(c("airtemp_max", "airtemp_min"), function(m)
    flag_periods(s, m) |> mutate(local_site = s, metric = m))) |>
    filter(date >= from, date <= to) |>
    mutate(series = factor(pretty[local_site], levels = pretty[sites]), metric = metric_lab[metric])
  shade <- fl |> group_by(series, metric) |> group_modify(~runs(.x)) |> ungroup()
  shade_any <- shade |> distinct(metric, start, end)

  p1 <- ggplot(top, aes(date, smooth, colour = series, linetype = series, linewidth = series,
                        group = factor(series, levels = draw_order(st, suspects)))) +
    geom_rect(data = shade_any, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey90", alpha = 0.6) +
    geom_line(na.rm = TRUE) +
    facet_wrap(~metric, ncol = 1) +
    style_scales(st) +
    labs(title = title, subtitle = paste0("7-day running mean. Colored = suspect series (overlapping ones show through as dashed/dotted); ",
                                          "black/gray = references.\nGray shading = flagged: >= 20% of days in a 30-day window are > 5 C off the usual difference from Boulder 14 W"),
         x = NULL, y = "Air temperature (C)") +
    theme_minimal(base_size = 11) + theme(legend.position = "top", panel.grid.minor = element_blank(),
                                          legend.key.width = unit(1.4, "cm"), legend.spacing.x = unit(0.4, "cm"))

  p2 <- ggplot(fl, aes(date, diff, colour = series)) +
    geom_rect(data = shade_any, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey90", alpha = 0.6) +
    geom_hline(aes(yintercept = usual, colour = series), linetype = "dashed", linewidth = 0.4) +
    geom_point(size = 0.5, alpha = 0.35) +
    geom_line(aes(y = diff30, linetype = series, linewidth = series), na.rm = TRUE) +
    facet_wrap(~metric, ncol = 1) +
    style_scales(st) +
    labs(subtitle = "Daily difference from Boulder 14 W (points), 30-day running mean (line), usual offset (dashed)",
         x = NULL, y = "Suspect - Boulder 14 W (C)") +
    theme_minimal(base_size = 11) + theme(legend.position = "none", panel.grid.minor = element_blank())

  g <- gridExtra::arrangeGrob(p1, p2, ncol = 1, heights = c(1.1, 1))
  ggsave(paste0(out_dir, file), g, width = 11, height = 11, dpi = 130, bg = "white")
  shade |> mutate(case = title)
}

# zoom on the start/end of a flagged period with daily values, to pick exact dates
zoom_plot <- function(title, suspects, refs, centre, file, halfwidth = 30) {
  centre <- as.Date(centre)
  sites <- c(suspects, refs)
  st <- series_style(suspects, refs)
  d <- temps |> filter(local_site %in% sites, date >= centre - halfwidth, date <= centre + halfwidth) |>
    mutate(series = factor(pretty[local_site], levels = pretty[sites]), metric = metric_lab[metric])
  p <- ggplot(d, aes(date, measurement, colour = series, linetype = series, linewidth = series,
                     group = factor(series, levels = draw_order(st, suspects)))) +
    geom_vline(xintercept = centre, linetype = "dotted", colour = "grey40") +
    geom_line() + geom_point(aes(shape = series), size = 1.3) +
    scale_shape_manual(values = setNames(c(16, 17, 15, 1, 2, 0)[seq_along(sites)], pretty[sites]), name = NULL) +
    facet_wrap(~metric, ncol = 1) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d\n%Y") +
    style_scales(st) +
    labs(title = title, subtitle = paste("Daily values; dotted vertical line =", format(centre),
                                         "\nColored = suspect series (filled markers); black/gray = references (open markers)"),
         x = NULL, y = "Air temperature (C)") +
    theme_minimal(base_size = 11) + theme(legend.position = "top", panel.grid.minor = element_blank(),
                                          legend.key.width = unit(1.4, "cm"), legend.spacing.x = unit(0.4, "cm"))
  ggsave(paste0(out_dir, file), p, width = 11, height = 7, dpi = 130, bg = "white")
}

flags <- bind_rows(
  case_plot("Niwot SNOTEL vs neighbours, 2004-2007", "Niwot",
            c("USW00094075", "UniversityCamp", "LakeEldora"), "2004-01-01", "2007-12-31", "1_niwot_snotel_2004-2007.png"),
  case_plot("University Camp SNOTEL vs neighbours, 2009-2012", "UniversityCamp",
            c("USW00094075", "Niwot", "LakeEldora"), "2009-01-01", "2012-12-31", "2_univcamp_snotel_2009-2012.png"),
  case_plot("D1 HMPs vs D1 chart and Boulder 14 W, 2018-2019",
            c("d1_cr1000_hmp_1", "d1_cr1000_hmp_2", "d1_cr1000_hmp_3"),
            c("d1_chart", "USW00094075", "sdl_cr1000_hmp_1"), "2018-01-01", "2019-12-31", "3_d1_hmps_2018-2019.png")
)

# zooms on the apparent start/end of each bad period, read off the overview plots
# (the automatic flag is fooled by D1's seasonal inversion offset in daily min).
# Edit these dates to re-zoom.
zooms <- tribble(
  ~name,      ~title,                   ~suspects,                                                      ~refs,                                    ~start,       ~end,
  "niwot",    "Niwot SNOTEL",           list("Niwot"),                                                  list(c("USW00094075", "UniversityCamp")), "2005-07-19", "2007-03-10",
  "univcamp", "University Camp SNOTEL", list("UniversityCamp"),                                         list(c("USW00094075", "Niwot")),          "2010-05-01", "2011-07-20",
  "d1_hmps",  "D1 HMPs vs D1 chart",    list(c("d1_cr1000_hmp_1", "d1_cr1000_hmp_2", "d1_cr1000_hmp_3")), list(c("d1_chart", "USW00094075")),     "2018-07-25", "2019-04-05"
)
for (i in seq_len(nrow(zooms))) {
  sus <- unlist(zooms$suspects[[i]]); rf <- unlist(zooms$refs[[i]])
  zoom_plot(paste(zooms$title[i], "- start of suspect period"), sus, rf, zooms$start[i], paste0("zoom_", zooms$name[i], "_start.png"))
  zoom_plot(paste(zooms$title[i], "- end of suspect period"), sus, rf, zooms$end[i], paste0("zoom_", zooms$name[i], "_end.png"))
}

write_csv(flags |> select(case, series, metric, start, end, days), paste0(out_dir, "flagged_periods.csv"))
print(as.data.frame(flags |> select(case, series, metric, start, end, days)))
