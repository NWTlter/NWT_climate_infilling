#notes:
# tvan west elev: 3504
# tvan east elev: 3502
# forest elev: 3050
# c1 elev = 3022; sdl = 3528; d1 = 3739

# set path to climdat folder
datpath <- "c1_d1_sdl_clim/homogenize_climdat/"
flux_datpath <- "/Users/scarlet/Documents/nwt_lter/aux_climdats/AmeriFlux/"
na_vals <- c(" ", "", ".", NA, NaN, "NA", "NaN", -9999)
source(paste0(datpath,"scripts/flagging_functions.R"))

Forest <- read.csv(paste0(flux_datpath, "AMF_US-NR1_BASE-BADM_18-5/AMF_US-NR1_BASE_HH_18-5.csv"),
                  skip =2, colClasses = "character", na.strings = na_vals)

TvanW <- read.csv(paste0(flux_datpath, "AMF_US-NR3_BASE-BADM_2-5/AMF_US-NR3_BASE_HH_2-5.csv"), 
                  skip =2, colClasses = "character", na.strings = na_vals)

TvanE <- read.csv(paste0(flux_datpath, "AMF_US-NR4_BASE-BADM_2-5/AMF_US-NR4_BASE_HH_2-5.csv"),
                   skip =2, colClasses = "character", na.strings = na_vals)

# pare down to temp dat only and add site info
pare_flux <- function(dat, matchcols = c("timestamp|^TA"), ID = "US-NR1", localname = "Forest"){
  tempdat <- subset(dat, select = grepl(matchcols, names(dat), ignore.case = T))
  # separate date from time
  tempdat$date_start <- substr(tempdat$TIMESTAMP_START, start = 1, stop = 8) 
  tempdat$date_end <- substr(tempdat$TIMESTAMP_END, start = 1, stop = 8)
  tempdat$time_start <- substr(tempdat$TIMESTAMP_START, start = 9, stop = 12)
  tempdat$time_end <- substr(tempdat$TIMESTAMP_END, start = 9, stop = 12)
  # add site
  tempdat$fluxID <- ID
  tempdat$fluxname <- localname
  # clean up
  tempdat <- dplyr::select(tempdat, date_start:fluxname, TIMESTAMP_START:ncol(tempdat)) %>%
    mutate_at(.vars = c("date_start", "date_end"), .funs = function(x) as.Date(x, format = "%Y%m%d")) %>%
    mutate_at(.vars = names(.)[grepl("^TA_", names(.))], as.numeric)
  
  return(tempdat)
}

Forest_temp <- pare_flux(Forest)
TvanW_temp <- pare_flux(TvanW, ID = "US-NR3", localname = "TvanWest")
TvanE_temp <- pare_flux(TvanE, ID = "US-NR4", localname = "TvanEast")

# -- C1 FOREST -----
# check all timestamps
Forest_temp$timestamp_posix <- as.POSIXct(Forest_temp$TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC")
# drop TAU cols
Forest_temp <- Forest_temp[!grepl("TAU", names(Forest_temp))]
# check all timestamps present
foreststamps <- seq.POSIXt(from = min(Forest_temp$timestamp_posix), to = max(Forest_temp$timestamp_posix), by = "30 min")
summary(!foreststamps %in% Forest_temp$timestamp_posix) # all there

# find the first obs not NA, and start dat there
# note: PI_F are the gap-filled vals, numbers in colnames are rel spatial position + rep of sensor (h, v, rep)
# start with the row that is lowest for tempcols
startrow <- min(sapply(Forest_temp[grepl("^TA", names(Forest_temp))], function(x) min(which(!is.na(x)))))
View(Forest_temp[startrow:nrow(Forest_temp), ])
Forest_temp <- Forest_temp[startrow:nrow(Forest_temp),]

# visualize
Forest_temp %>%
  gather(sensor, temp, grep("^TA", names(.))) %>%
  mutate(location = str_extract(sensor, "_[:digit:].*$")) %>%
  ggplot(aes(timestamp_posix, temp, col = grepl("PI", sensor))) +
  geom_line(alpha = 0.5) +
  facet_wrap(~location)


# missing vals
Forest_temp %>%
  gather(sensor, temp, grep("^TA", names(.))) %>%
  mutate(location = str_extract(sensor, "_[:digit:].*$"),
         present = ifelse(is.na(temp), 0, 1)) %>%
  subset(present == 0) %>%
  ggplot(aes(timestamp_posix, factor(present), col = sensor, group = sensor)) +
  geom_point(alpha = 0.5, position = position_dodge(width = 1)) +
  coord_flip()

# look like at least sensor 1 (TA_PI_F_1_1_1) is infilled for full time

# Assess dailies from infilled sensors
Forest_dailies <- Forest_temp %>%
  gather(sensor, temp, grep("^TA", names(.))) %>%
  mutate(location = str_extract(sensor, "_[:digit:].*$")) %>%
  group_by(date_start, sensor, location) %>%
  summarize(minT = min(temp),
            maxT = max(temp),
            meanT = mean(temp),
            nobs = sum(!is.na(temp))) %>%
  ungroup()

Forest_dailies_tidy <- Forest_dailies %>%
  gather(met, airtemp, minT:meanT) %>%
  mutate(met = gsub("T", "", met))

ggplot(subset(check_allyrs, yr > 2015), aes(date, airtemp)) +
  geom_line(data = subset(Forest_dailies_tidy, grepl("PI_F_1_1", sensor) & met != "mean" & year(date_start) > 2015), aes(date_start, airtemp), col = "pink", alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_x_date(breaks = "6 month") +
  facet_wrap(~met, nrow = 2)


# -- TVAN WEST -----
TvanW_dailies <- TvanW_temp %>%
  mutate(TA = as.numeric(TA)) %>%
  group_by(date_start) %>%
  summarise(maxT = max(TA),
            minT = min(TA),
            meanT = mean(TA),
            nobs = sum(!is.na(TA))) %>%
  ungroup() %>%
  gather(met, temp, maxT:meanT) %>%
  mutate(met = gsub("T", "", met))

min(TvanW_dailies$date_start[!is.na(TvanW_dailies$temp)])

ggplot(subset(check_allyrs, yr > 2015), aes(date, airtemp)) +
  geom_line(data = subset(Forest_dailies_tidy, grepl("PI_F_1_1", sensor) & met != "mean" & year(date_start) > 2015), aes(date_start, airtemp), col = "pink", alpha = 0.5) +
  geom_line(data = subset(TvanW_dailies, met != "mean" & year(date_start) > 2015), aes(date_start, temp), col = "blue", alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_x_date(breaks = "6 month") +
  facet_wrap(~met, nrow = 2)

# -- TVAN EAST -----
TvanE_dailies <- TvanE_temp %>%
  mutate(TA = as.numeric(TA)) %>%
  group_by(date_start) %>%
  summarise(maxT = max(TA),
            minT = min(TA),
            meanT = mean(TA),
            nobs = sum(!is.na(TA))) %>%
  ungroup() %>%
  gather(met, temp, maxT:meanT) %>%
  mutate(met = gsub("T", "", met))

ggplot(subset(check_allyrs, yr > 2015), aes(date, airtemp)) +
#ggplot() +
  geom_point(data = subset(Forest_dailies_tidy, grepl("PI_F_1_1", sensor) & met != "mean" & year(date_start) > 2015), aes(date_start, airtemp), col = "pink", alpha = 0.5) +
  geom_point(data = subset(TvanW_dailies, met != "mean" & year(date_start) > 2015), aes(date_start, temp), col = "blue", alpha = 0.5) +
  geom_point(data = subset(TvanE_dailies, met != "mean" & year(date_start) > 2015), aes(date_start, temp), col = "orange", alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_date(breaks = "6 month") +
  facet_wrap(~met, nrow = 2)


# -- check diffs -----
ameristacked <- subset(Forest_dailies_tidy, grepl("PI_F_1_1", sensor), select = c("date_start", "nobs", "met", "airtemp")) %>%
  rename(temp = airtemp) %>%
  mutate(location = "forest") %>%
  rbind(mutate(TvanE_dailies, location = "tvan east")) %>%
  rbind(mutate(TvanW_dailies, location = "tvan west")) %>%
  rename(date = date_start) %>%
  left_join(check_allyrs[c("date", "met", "airtemp")]) %>%
  mutate(diffsdl = airtemp-temp)

ggplot(subset(ameristacked, met != "mean" & year(date) > 2002), aes(date, diffsdl)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(col = location)) +
  geom_vline(aes(xintercept = as.Date("2019-01-01"))) +
  geom_smooth(aes(lty = date >= as.Date("2019-01-01")), method = "lm") +
  facet_grid(location~met, scale = "free_y")


# forest site is the consistent data source, so adjust sdl by mean diff between sdl-forest for hmp period vs. sdl-forest (logger period)
forest_out <- data.frame(cbind(local_site = "C1 Ameriflux", subset(Forest_dailies_tidy, grepl("PI_F_1_1", sensor), select = c(date_start, sensor, met, airtemp)))) %>%
  rename(date = date_start) %>%
  filter(date <= max(date[!is.na(airtemp)]))
# check length of all mets
with(forest_out, sapply(split(airtemp, met), summary))
with(forest_out, sapply(split(airtemp, met), length)) # all same
with(forest_out, sapply(split(date, met), summary)) # same date distribution

# -- FINISHING ----
# write out forest for sdl quickstitch
write.csv(forest_out, paste0(flux_datpath, "forest_prepped2compare.csv"), row.names = F)

