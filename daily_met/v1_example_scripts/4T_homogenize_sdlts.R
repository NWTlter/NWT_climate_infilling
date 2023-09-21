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




# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

# set path to prepared data
datpath <- "~/Documents/nwt_lter/nwt_climate/data/"
# list all rds files in qc'd data folder (full path directory)
rdsfiles <- list.files(paste0(datpath, "infill"), pattern = "rds", full.names = T)

# read in infilled sdl temp data
sdlchart_pred <- readRDS(rdsfiles[grepl("chart_infill", rdsfiles)])
sdllog_pred <- readRDS(rdsfiles[grepl("logger_infill", rdsfiles)])
nwtlog_qc <- readRDS(rdsfiles[grepl("logger_dblqc", rdsfiles)])

# supporting dats
boulder14 <- readRDS(rdsfiles[grepl("boulder", rdsfiles)]) #2003 to present 
usnr1 <- readRDS("/Users/scarlet/Documents/nwt_lter/nwt_climate/data/qc/amerifluxTEMP_qc.rds")

# be sure all read in as expected
str(sdlchart_pred)
str(sdllog_pred)
str(nwtlog_qc)
str(boulder14)

# also read in d1 tempdat used for recent renewal so columns formatted like MO expects
d1nwt8 <- read_csv("../nwt8-renewal/c1_d1_sdl_clim/homogenize_climdat/data/d1_temp_1952-2020_draft.csv") %>%
  data.frame()

# bring in sdl dat used bc i'm not re-infilling 2012-12-04 (no time)
sdlnwt8 <- read_csv("../nwt8-renewal/c1_d1_sdl_clim/homogenize_climdat/data/sdl_temp_1981-2020_draft.csv") %>%
  data.frame()


# -- PREP SUPPORTING DATS ----
# all need tmean and to be subset to thir qc'd periods -- add low points back to boulder 14, keep 0 days NA'd as they are
boulder14comp <- subset(boulder14, metric != "TOBS", select = c(station_id, station_name, date, yr, mon, doy, metric, raw)) %>% # actually just use raw so there is something to compare for all dates (the raw all seems plausible or slightly off if so)
  rename(boulder14 = raw) %>%
  group_by(date) %>%
  mutate(tmean = boulder14,
         dtr = boulder14[metric == "airtemp_max"] - boulder14[metric == "airtemp_min"]) %>%
  ungroup()

usnr1comp <- subset(usnr1, station_id == "US-NR1", select = c(station_id, station_name, date, yr, mon, doy, metric, qc_measurement)) %>% # actually just use raw so there is something to compare for all dates (the raw all seems plausible or slightly off if so)
  rename(usnr1 = qc_measurement) %>%
  group_by(date) %>%
  mutate(dtr = usnr1[metric == "airtemp_max"] - usnr1[metric == "airtemp_min"]) %>%
  ungroup()



# -- AVERAGE HMP PERIOD ----
sdlhmps <- subset(sdllog_pred, grepl("hmp", local_site), select = c(date:airtemp_max, airtemp_min, airtemp_avg, dtr_infill)) %>%
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

sdlloggers <- subset(sdllog_pred, !grepl("hmp", local_site), select = c(date:airtemp_max, airtemp_min, airtemp_avg, dtr_infill, raw_airtemp_avg:ncol(sdllog_pred))) %>%
  mutate(predicted = !is.na(dtr_infill),
         sensors_infilled = ifelse(!is.na(dtr_infill), 1, 0),
         DTR = airtemp_max - airtemp_min) 
# look at how predicted differs from raw
View(subset(sdlloggers, predicted & round(airtemp_max,5) != round(raw_airtemp_max,5)))
View(subset(sdlloggers, predicted & round(airtemp_min,5) != round(raw_airtemp_min,5))) # not worth fussing over rn

sdllog_all <- sdlhmps[!grepl("nobs|ct_", names(sdlhmps))] %>%
  rbind(sdlloggers[names(.)]) %>%
  arrange(date) %>%
  # I realize I may not want to spread this yet
  gather(metric, measurement, airtemp_avg:DTR)

ggplot(sdllog_all, aes(date, measurement, col = logger)) +
  geom_line() +
  geom_point(data = subset(sdllog_all, sensors_infilled > 0), aes(fill = factor(sensors_infilled)), pch = 21) +
  facet_wrap(~metric, nrow = length(unique(sdllog_all$metric)), scales = "free_y")


# -- REVIEW OVERLAP BTWN SDL SOURCES ----

# look at 2018 overlap with cr1000
ggplot(subset(sdllog_all, yr %in% 2017:2019), aes(date, measurement, col = logger)) +
  geom_line(alpha= 0.5) +
  #geom_point(data = subset(sdllog_all, sensors_infilled > 0), aes(fill = factor(sensors_infilled)), pch = 21) +
  facet_wrap(~metric, nrow = length(unique(sdllog_all$metric)), scales = "free_y")
# bc tmean is not math mean of tmax and tmin for loggers, ok to make separate adjustment if biggest difference between instruments is for tmax but tmean and tmax are closer

# bc chart has issues in winter, probably use summer sdl to adjust other loggers, but do look at one year overlap to get a sense.. (2 yr period of saddle chart shouldn't change too much)
# lot pre-hmp with sdlchart
sdlchart_NAdates <- sdlchart_pred$date[is.na(sdlchart_pred$airtemp_min)]
sdlchart_tidy <- sdlchart_pred
sdlchart_tidy$airtemp_max[sdlchart_tidy$date %in% sdlchart_NAdates] <- sdlchart_tidy$raw_airtemp_max[sdlchart_tidy$date %in% sdlchart_NAdates]
sdlchart_tidy$airtemp_min[sdlchart_tidy$date %in% sdlchart_NAdates] <- sdlchart_tidy$raw_airtemp_min[sdlchart_tidy$date %in% sdlchart_NAdates]
sdlchart_tidy[sdlchart_tidy$date %in% sdlchart_NAdates, c("airtemp_max_method", "airtemp_min_method")] <- "raw"
sdlchart_tidy <- subset(sdlchart_tidy, select = c(date:doy, airtemp_max, airtemp_min, airtemp_avg, dtr_infill)) %>%
  mutate(sensors_infilled = ifelse(!is.na(dtr_infill), 1, 0),
         DTR = airtemp_max - airtemp_min) %>%
  subset(select = -dtr_infill) %>%
  gather(metric, measurement, airtemp_max:airtemp_avg, DTR)
# append logger to sdlchart so it plots -- note will duplicate dates overlap in 2000 for cr21x and cr23x
sdlchart_tidy_plot <- left_join(sdlchart_tidy, distinct(sdlloggers[c("date", "logger")])) %>%
  subset(!is.na(logger))

ggplot(subset(sdllog_all, !grepl("hmp", logger)), aes(date, measurement)) +
  geom_line() +
  geom_line(data = sdlchart_tidy_plot, aes(date, measurement), col = "orchid", lwd = 1, alpha = 0.5) +
  facet_grid(metric~logger, scales = "free", shrink = T)


# -- CALCULATE ADJUSTMENTS -----
# > first smooth cr21x, cr23x, and cr1000 to sdl chart (common denom)
# make joined logger and sdl dat df 
chartlog_join <- subset(sdllog_all, !grepl("hmp", logger), -sensors_infilled) %>%
  rename(logtemp = measurement) %>%
  left_join(distinct(subset(sdlchart_tidy, select = -sensors_infilled))) %>%
  rename(sdltemp = measurement) %>%
  mutate(logsdl_diff = logtemp - sdltemp) %>%
  # drop any records where no overlap w saddle
  subset(!is.na(logsdl_diff)) %>%
  #calculate start and end yr for each logger
  group_by(logger) %>%
  mutate(lastoverlap = max(date),
         beginoverlap = min(date)) %>%
  ungroup() %>%
  distinct()

#plot diff by logger period
ggplot(chartlog_join, aes(factor(mon), logsdl_diff)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  geom_boxplot(fill = "transparent") +
  facet_grid(metric~logger)

# pull start and end dates w sdl chart/loggers
overlap_dates <- data.frame(distinct(chartlog_join[c("logger", "lastoverlap", "beginoverlap")])) # lol, missing 2012-12-04 btwn cr1000 and cr23x. YAY..
# change cr1000 to to start jan 1 2017, and cr21x to date when logger starts recording mostly its own data
ggplot(subset(sdlloggers, logger == "cr21x"), aes(date, airtemp_max, col = is.na(dtr_infill))) +
  geom_point() + 
  facet_wrap(~yr, scales = "free_x") # can go july 1992 thru june 1993

cr21x_yr1 <- seq.Date(from = as.Date("1992-07-01"), by = 1, length.out = 365)
cr21x_final <- seq.Date(from = overlap_dates$lastoverlap[overlap_dates$logger == "cr21x"], by = -1, length.out = 365)

cr23x_yr1 <- seq.Date(from = overlap_dates$beginoverlap[overlap_dates$logger == "cr23x"], by = 1, length.out = 365)
cr23x_final <- seq.Date(from = overlap_dates$lastoverlap[overlap_dates$logger == "cr23x"], by = -1, length.out = 365)

cr1000_yr1 <- seq.Date(from = overlap_dates$beginoverlap[overlap_dates$logger == "cr1000"], by = 1, length.out = 365)
cr1000_final <- seq.Date(from = overlap_dates$lastoverlap[overlap_dates$logger == "cr1000"], by = -1, length.out = 365)

overlapdat_final <- subset(chartlog_join, date %in% unique(c(cr21x_final, cr23x_final, cr1000_final))) %>% distinct() %>% mutate(period = "final")
overlapdat_start <- subset(chartlog_join, date %in% unique(c(cr21x_yr1, cr23x_yr1, cr1000_yr1))) %>% mutate(period = "start")
overlapdat <- rbind(overlapdat_final, overlapdat_start) %>% distinct()

# consider monthly avg diffs
monthly_sdlog_diffs <- subset(overlapdat, metric != "DTR") %>%
  group_by(logger, period, mon, metric) %>%
  mutate(mon_diff = mean(logsdl_diff),
        mon_sd = sd(logsdl_diff),
        mon_nobs = length(unique(date))) %>%
  ungroup() %>%
  # just calculate diff by period and met now
  group_by(period, logger, metric) %>%
  mutate(ann_diff = mean(logsdl_diff),
         ann_sd_ann = sd(logsdl_diff),
         ann_nobs = length(unique(date)),
         JJA_diff = mean(logsdl_diff[mon %in% c(6,7,8)]),
         JJA_sd = sd(logsdl_diff[mon %in% c(6,7,8)]),
         JJA_nobs = length(unique(logsdl_diff[mon %in% c(6,7,8)]))) %>%
  ungroup() %>%
  subset(select = c(mon, logger:metric, period:ncol(.))) %>%
  distinct()
  #idk why 1999 and 2011 are present for 

annual_sdllog_diff <- subset(monthly_sdlog_diffs, select = -c(mon_diff, mon_sd, mon_nobs, mon)) %>%
  distinct()

# what is it by the entire logger period? (but remove infill period for 21x)
alldates_diff <- subset(chartlog_join, yr >=1993) %>%
  group_by(logger, metric) %>%
  summarise(avgdiff = mean(logsdl_diff),
          sddiff = sd(logsdl_diff))


# if sdl chart is drifting at all, then these comparisons aren't helpful..
# revisit overlap btwn 21x and 23x
overlap_2123 <- subset(chartlog_join, yr == 2000 & mon == 6) %>%
  subset(date >= as.Date("2000-06-24") & date <= as.Date("2000-06-27"))


# -- COMPARE HMPS WITH CR1000 ----
hmpcr1000 <- subset(sdllog_all, yr == 2018, -sensors_infilled) %>%
  spread(logger, measurement) %>%
  mutate(loghmp_diff = cr1000 - cr1000_hmp)

ggplot(hmpcr1000, aes(factor(mon), loghmp_diff, group = mon)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  geom_boxplot(fill = "transparent") +
  geom_jitter(col = "orchid", alpha = 0.5) +
  stat_summary(col = "pink") +
  facet_wrap(~metric)

hmpcr1000_avg <- group_by(hmpcr1000, metric) %>%
  mutate(yr_avg = mean(loghmp_diff, na.rm = T)) %>%
  group_by(mon, yr_avg, metric) %>%
  summarise(mon_avg = mean(loghmp_diff, na.rm = T)) %>%
  ungroup()

ggplot(hmpcr1000_avg, aes(mon, mon_avg)) +
  geom_line() +
  facet_wrap(~metric)

# -- COMPARE CR PERIOD WITH D1, US NR1, + BOULDER 14 -----
lognr1 <- subset(sdllog_all, date >= min(usnr1comp$date) & metric != "DTR", -sensors_infilled) %>%
  left_join(usnr1comp[c("date", "metric", "usnr1")]) %>%
  mutate(lognr1_diff = measurement - usnr1)

# plot
ggplot(lognr1, aes(factor(mon), lognr1_diff, group = mon)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  geom_boxplot(fill = "transparent") +
  geom_jitter(col = "orchid", alpha = 0.5) +
  stat_summary(col = "pink") +
  facet_grid(logger~metric)

# nr1 is the only constant
lognr1_avgs <- group_by(lognr1, logger, metric) %>%
  mutate(yr_avg = mean(lognr1_diff, na.rm = T)) %>%
  group_by(logger, yr_avg, metric, mon) %>%
  summarise(mon_avg = mean(lognr1_diff, na.rm = T)) %>%
  ungroup() 

lognr1_avg_wide <- lognr1_avgs %>%
  mutate(mon = paste0("m", mon),
         mon = factor(mon, levels = paste0("m", 1:12))) %>%
  spread(mon, mon_avg)

ggplot(lognr1_avgs, aes(mon, mon_avg, col = logger)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~metric)

# for now diff other loggers from cr1000_hmps with nr1
pinghmps <- subset(lognr1_avgs, !grepl("hmp", logger)) %>%
  rename(log_yr_avg = yr_avg, log_mon_avg = mon_avg) %>%
  left_join(subset(lognr1_avgs, grepl("hmp", logger), select = -logger)) %>%
  mutate(diff_mon = mon_avg - log_mon_avg,
         diff_yr = yr_avg - log_yr_avg)

ggplot(pinghmps, aes(mon, diff_mon, col = logger)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  geom_line(aes(mon, diff_yr), lty = 2) +
  facet_wrap(~metric)


adjustment <- subset(pinghmps, select = c(logger, diff_yr, metric)) %>%
  left_join(lognr1_avgs[c("logger", "metric", "yr_avg")]) %>%
  distinct() %>%
  mutate(adjustnr1 = round(diff_yr, 4))
adjustment_2018 <- subset(hmpcr1000_avg, select = c(metric, yr_avg)) %>%
  distinct() %>%
  mutate(adjcr2018 = round(yr_avg, 4))
adjustment <- left_join(adjustment, adjustment_2018[c("metric", "adjcr2018")]) # yr_avg = logger - nr1



# -- MAKE HOMOGENIZED DAT -----
# add in 2012-12-04
missing_sdldate <- subset(sdlnwt8, date == as.Date("2012-12-04")) %>%
  rename(logger = source_instrument, measurement = qa_airtemp) %>%
  mutate(sensors_infilled = 1,
         metric = paste0("airtemp_", metric),
         metric = gsub("mean", "avg", metric))

dat_out <- sdllog_all %>%
  rbind(missing_sdldate[names(.)]) %>%
  arrange(date) %>%
  spread(metric, measurement) %>%
  mutate(DTR = ifelse(is.na(DTR), airtemp_max - airtemp_min, DTR)) %>%
  gather(metric, measurement, airtemp_avg:DTR) %>%
  subset(!metric == "DTR") %>%
  left_join(adjustment[c("logger", "metric", "adjustnr1")]) %>%
  mutate(adjustnr1 = ifelse(grepl("hmp", logger), 0, adjustnr1),
         adjustment = ifelse(grepl("hmp|21x", logger), 0, adjustnr1),
         adjusted_airtemp = round(measurement +adjustment,4)) %>%
  rename(airtemp = measurement,
         diff_from_hmpdiffnr1 = adjustnr1,
         adjust_to_hmp = adjustment) %>%
  mutate(LTER_site = "NWT",
         local_site = "sdl") %>%
  subset(select = c(LTER_site, local_site, logger, date:doy, metric:adjusted_airtemp, sensors_infilled)) %>%
  #pick cr23x for overlap dates in june 2000
  subset(!((date %in% unique(overlap_2123$date))  & grepl("21x", logger))) %>%
  # pick hmps for 2018 onwards (drop cr1000)
  subset(!(logger == "cr1000" & yr == 2018))

# check that there is one value per day, every day in time series
group_by(dat_out, metric, date) %>%
  summarise(nobs = length(adjusted_airtemp)) %>%
  ungroup() %>%
  summary()
summary(unique(dat_out$date) %in% seq.Date(min(dat_out$date), max(dat_out$date), 1))
summary(seq.Date(min(dat_out$date), max(dat_out$date), 1) %in% unique(dat_out$date))
# and be sure 3 metrics per date
group_by(dat_out, date) %>%
  summarise(nobs = length(unique(metric))) %>%
  summary() # looks good

# what do data look like?
ggplot(dat_out, aes(date, adjusted_airtemp)) +
  geom_line(col = "grey30") +
  geom_point(data = subset(dat_out, sensors_infilled > 0), col ="goldenrod", alpha = 0.5) +
  geom_smooth(col = "black", method = "lm") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  facet_wrap(~factor(metric, levels = c("airtemp_max", "airtemp_avg", "airtemp_min")), nrow = 3)

summary(lm(adjusted_airtemp~yr * factor(mon), data = subset(dat_out, metric == "airtemp_min")))
summary(lm(adjusted_airtemp~yr * factor(mon), data = subset(dat_out, metric == "airtemp_max")))
summary(lm(adjusted_airtemp~yr * factor(mon), data = subset(dat_out, metric == "airtemp_avg"))) #0.04C per yr increase in avg daily temp

group_by(dat_out, yr, metric) %>%
  summarise(T_xbar = mean(adjusted_airtemp),
            T_se = sd(adjusted_airtemp)/sqrt(length(date)),
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
  summarise(T_xbar = mean(adjusted_airtemp),
            T_se = sd(adjusted_airtemp)/sqrt(length(date)),
            nobs = length(date)) %>%
  ungroup() #%>%

summary(lm(T_xbar~yr * metric, data = annualmeans)) # all by about 0.07C per yr.. so in this period, avg temps have increase 2C already
cor.test(~T_xbar + yr, data = subset(annualmeans, metric == "airtemp_avg"))
summary(lm(T_xbar~yr, data = subset(annualmeans, metric == "airtemp_min"))) #+0.09 (9% CI btween 0.05 and 0.14)
summary(lm(T_xbar~yr, data = subset(annualmeans, metric == "airtemp_max"))) #+0.05 (95% CI btwn .009 and .11)
summary(lm(T_xbar~yr, data = subset(annualmeans, metric == "airtemp_avg"))) #+0.07 (but 95% CI btwn 0.03 and .12)

# see if can re-create MO's GDD fig to be sure
ggplot(dat_out, aes(date, adjusted_airtemp, col = logger)) +
  geom_line(alpha = 0.5, lwd = 1) +
  # plot unadjusted airtemp to see how cr1000 plots
  geom_line(data = subset(dat_out, logger == "cr1000" & yr == 2018), aes(date, airtemp), col = "white", alpha = 0.75) +
  facet_wrap(~metric)

# make MOs' plot (sum of tmean where daily T > 32C)
GDD_sdl <- subset(dat_out, metric == "airtemp_avg" & adjusted_airtemp > 0) %>%
  group_by(yr) %>%
  summarise(GDD1 = sum(airtemp + diff_from_hmpdiffnr1),
            GDD = sum(adjusted_airtemp))
ggplot(GDD_sdl, aes(yr, GDD)) +
  geom_line() + 
  geom_line(aes(yr, GDD1), col = "seagreen") + 
  geom_smooth(method = "lm", fill = "blue") +
  geom_smooth(aes(yr, GDD1), method = "lm", col = "seagreen", fill = "seagreen1") +
  geom_vline(aes(xintercept = 1994), lty = 2) +
  geom_vline(aes(xintercept = 2018), lty = 2) +
  scale_x_continuous(breaks = seq(1986, 2022, 4)) +
  scale_y_continuous(limits = c(600,1400))

# to be sure, look at same for D1 (verify 1993 is cold there too)
GDD_d1 <- subset(d1nwt8, year > 1986 & mean_temp > 0) %>%
  group_by(year) %>%
  summarise(GDD = sum(mean_temp))

ggplot(GDD_d1, aes(year, GDD)) +
  geom_line() + 
  geom_smooth(fill = "blue", method = "lm") +
  geom_vline(aes(xintercept = 1994), lty = 2) +
  geom_vline(aes(xintercept = 2018), lty = 2) +
  scale_x_continuous(breaks = seq(1986, 2022, 4)) +
  scale_y_continuous(limits = c(600,1400))

# plot both together for more direct comparison
ggplot(GDD_sdl, aes(yr, GDD)) +
  geom_line() + 
  #geom_line(aes(yr, GDD1), col = "seagreen") + 
  geom_smooth(method = "lm", fill = "blue") +
  #geom_smooth(aes(yr, GDD1), method = "lm", col = "seagreen", fill = "seagreen1") +
  geom_line(data = GDD_d1, aes(year, GDD), col = "pink") +
  geom_smooth(data = GDD_d1, aes(year, GDD), col = "pink", fill = "pink", method = "lm") +
  geom_vline(aes(xintercept = 1994), lty = 2) +
  geom_vline(aes(xintercept = 2018), lty = 2) +
  scale_x_continuous(breaks = seq(1986, 2022, 4)) +
  scale_y_continuous(limits = c(600,1400))

# zoom in on 1993 temps
ggplot(subset(dat_out, yr == 1993), aes(date, adjusted_airtemp, col = factor(sensors_infilled))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point() +
  geom_point(aes(date, airtemp + diff_from_hmpdiffnr1), pch = 1, size = 2) +
  facet_wrap(~metric)

# plot june 1990
ggplot(subset(nwtlog_qc, yr == 1990 & mon == 6), aes(date, measurement, col = local_site)) +
  geom_line() +
  facet_wrap(~metric)

# if use adjustment for cr21x, what do you get?
ggplot(dat_out, aes(date, adjusted_airtemp)) +
  geom_line(aes(date, airtemp + diff_from_hmpdiffnr1), col = "orchid", lwd = 2, alpha = 0.5) +
  geom_line() +
  facet_wrap(~metric, nrow = 3)

ggplot(subset(nwtlog_qc, yr == 1990 & mon == 6), aes(date, measurement, col = station_name)) +
  #geom_line(aes(date, airtemp + diff_from_hmpdiffnr1), col = "orchid", lwd = 2, alpha = 0.5) +
  geom_line() +
  facet_wrap(~metric, nrow = 3)
  


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

# for ease, make prediction columns then join to adjusted tempdat (make each part separately since this will be wide)
adjusted_dat_out <- subset(dat_out, select = -diff_from_hmpdiffnr1) %>% # drop diffval and just keep adjustment applied
  rename(gapfilled = airtemp, homogenized = adjusted_airtemp, adjustment = adjust_to_hmp) %>%
  gather(column, value, gapfilled:ncol(.)) %>%
  unite(metric, metric, column, sep =  "_") %>%
  spread(metric, value) %>%
  #calculate adjusted_DTR
  mutate(dtr_homogenized= airtemp_max_homogenized - airtemp_min_homogenized,
         dtr_gapfilled = airtemp_max_gapfilled - airtemp_min_gapfilled) %>%
  # organize columns
  subset(select = c(LTER_site:doy, 
                    airtemp_max_homogenized, airtemp_avg_homogenized, airtemp_min_homogenized, dtr_homogenized,
                    airtemp_max_adjustment, airtemp_avg_adjustment, airtemp_min_adjustment,
                    airtemp_max_gapfilled, airtemp_avg_gapfilled, airtemp_min_gapfilled, dtr_gapfilled,
                    airtemp_max_sensors_infilled, airtemp_avg_sensors_infilled, airtemp_min_sensors_infilled))

# do follow Tim's method for flags for infilling
# note qc flagging in its own column, with short description
# looks like keith's checks were same station only (but that makes sense bc they were hourly so it's reasonable to check for spikes)
# create infill flagging for temp (method 1 = moving window fill, method 2 = historic)

# notice airtemp_avg_method is raw sometimes when an infill method is present, fix:
sdllog_pred$airtemp_avg_method[grepl("m", sdllog_pred$method)] <- "predicted"
unique(sdllog_pred$airtemp_avg_method[is.na(sdllog_pred$method)]) # all 'raw', good

sdllog_pred$flag_1 <- NA
sdllog_pred$flag_1[is.na(sdllog_pred$method)] <- "A"
# flagging based on tmean pval
sdllog_pred$flag_1[grepl("multi", sdllog_pred$method) & sdllog_pred$airtemp_avg_pval <= 0.05] <- "B"
sdllog_pred$flag_1[grepl("multi", sdllog_pred$method) & sdllog_pred$airtemp_avg_pval > 0.05] <- "C"
sdllog_pred$flag_1[grepl("moving", sdllog_pred$method) & sdllog_pred$airtemp_avg_pval <= 0.05] <- "D"
sdllog_pred$flag_1[grepl("moving", sdllog_pred$method) & sdllog_pred$airtemp_avg_pval > 0.05] <- "E"

# flagging for tmax/tmin derivation
sdllog_pred$flag_2 <- NA
sdllog_pred$flag_2[is.na(sdllog_pred$method)] <- "A"
sdllog_pred$flag_2[sdllog_pred$airtemp_max_method == "predicted" & sdllog_pred$airtemp_min_method == "predicted"] <- "B"
# if raw adjustment failed, assign B*
sdllog_pred$flag_2[grepl("failed", sdllog_pred$airtemp_max_method)] <- "B*"
sdllog_pred$flag_2[grepl("adjusted$", sdllog_pred$airtemp_max_method) & sdllog_pred$airtemp_min_method == "raw"] <- "C"
sdllog_pred$flag_2[sdllog_pred$airtemp_max_method == "raw" & grepl("dtr adjusted$", sdllog_pred$airtemp_min_method)] <- "D"

# flagging for dtr pval
sdllog_pred$flag_3 <- NA
sdllog_pred$flag_3[is.na(sdllog_pred$method)] <- "A"
sdllog_pred$flag_3[sdllog_pred$dtr_pval <= 0.05] <- "B"
sdllog_pred$flag_3[sdllog_pred$dtr_pval > 0.05] <- "C"

# be sure for prediction only keep hmps for 2018 and cr23x for overlap dates btwn 21 and 23
preds_out_crs <- subset(sdllog_pred, !grepl("hmp", local_site)) %>%
  # drop method columns since infill flags present, drop local_site as well (not needed)
  dplyr::select(-(names(sdllog_pred)[grepl("method|local_si", names(sdllog_pred))])) %>%
  rename_at(names(.)[grepl("_avg$|_min$|_max$", names(.)) & !grepl("^raw", names(.))], function(x) paste0(x, "_gapfilled")) %>%
  mutate(dtr_gapfilled = airtemp_max_gapfilled - airtemp_min_gapfilled,
         # to be sure, check dtr_gapfilled == dtr_infill when dtr_infill is present
         dtr_check = ifelse(!is.na(dtr_infill), round(dtr_infill,5) == round(dtr_gapfilled,5), NA)) %>% # in manual check, yes agrees
  # organize columns
  subset(select = c(date, yr, mon, doy, logger,
                    airtemp_max_gapfilled, airtemp_min_gapfilled, airtemp_avg_gapfilled, dtr_gapfilled,
                    flag_1, flag_2, flag_3, source.station,
                    airtemp_avg_pval, airtemp_avg_r2, airtemp_avg_n.obs, airtemp_avg_equation, 
                    dtr_pval, dtr_r2, dtr_n.obs, dtr_equation, 
                    raw_airtemp_max, raw_airtemp_min, raw_airtemp_avg)) %>%
  # clean up names
  rename(num_obs_in_airtemp_avg_regression = airtemp_avg_n.obs,
         num_obs_in_dtr_regression = dtr_n.obs,
         source_station = source.station)

# need to make yr 2018 where gapfilled cols are means of hmps and everything else NA (since each hmp has its own regreesion)
# and dtr_gapfilled is the diff btwn the means
preds_out_2018



preds_out_hmps <- subset(sdllog_pred, grepl("hmp", local_site)) %>%
  # drop method columns since infill flags present
  dplyr::select(-(names(sdllog_pred)[grepl("method", names(sdllog_pred))])) %>%
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
         local_site = gsub("_", "", local_site)) %>%
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


# join flagging to regression info
# drop raw column in predictions because that is what was used for measurement
preds_out_crs_qflags <- left_join(preds_out_crs[!grepl("raw", names(preds_out_crs))], sdllog_flags_crs)
glimpse(preds_out_crs_qflags) # seems ok

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
preds_out_hmp_all_qflags <- left_join(preds_out_hmp1_qflag, preds_out_hmp2_qflag) %>%
  left_join(preds_out_hmp3_qflag)
glimpse(preds_out_hmp_all_qflags) # 80 columns ... ugh. but looks okay


# -- put all together ----
edi_dat <- adjusted_dat_out[!grepl("sensors_", names(adjusted_dat_out))] %>%
  left_join(preds_out_crs_qflags) %>%
  left_join(preds_out_hmp_all_qflags) %>%
  data.frame()
glimpse(edi_dat) #meh 114 columns

# still need to clean up source stations and logger col


#  -- WRITE OUT -----
# dataset for mo and jhux
write_csv(dat_out, paste0(datpath, "homogenize/sdllogger_temp_draft2022.csv"))
saveRDS(dat_out, paste0(datpath, "homogenize/sdllogger_temp_draft2022.rds"))
# more complete dataset for sarah/edi
write_csv(edi_dat, paste0(datpath, "homogenize/nwt_sdl_homogenized_temperature_draft.csv"))
saveRDS(edi_dat, paste0(datpath, "homogenize/nwt_sdl_homogenized_temperature_draft.rds"))

