# explore sdl ppt

# script purpose:
# read in sdl ppt (infilled) dataset, corr with sdl snowdepth to answer: is saddle ppt reliable in winter?
# check corr with d1 ppt as well. if d1 more correlated, use d1 for renewal
# > per discussion with SCE, JK recs to SCE re: sdl ppt, jump in sdl ppt in mid 90s nobody seems to know anything about


# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
library(rgdal)
library(changepoint)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN")

# set path to climdat folder
datpath <- "c1_d1_sdl_clim/homogenize_climdat/"
# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")

# -- GET DATA -----
# raw sdl ppt dataset from EDI
sdlppt <- getTabular(416) %>% data.frame()
# infilled sdl ppt dataset from renewal
sdl_infilled <- read_csv(paste0(datpath, "data/prep_data/sdl_ppt_infill_19822018_nsfctw.csv"), na = na_vals)
# infilled d1 ppt from EDI
d1ppt <- getTabular(186) %>% data.frame()
# infilled c1 ppt from EDI
c1ppt <- getTabular(184) %>% data.frame() # from chart
c1cr_ppt <- getTabular(401) %>% data.frame() # from logger --only available at C1
# jennings hourly dataset (has ppt at c1, sdl, d1)
jppt <- getTabular(168) %>% data.frame()
# sdl snowdepth
sdl_snowfence <- getTabular(34) %>% data.frame() # snowfence subset
sdl_snow <- getTabular(31) %>% data.frame() # larger grid

# other snowdats
swe <- getTabular(96) %>% data.frame()
snow_cover <- getTabular(98) %>% data.frame()

# other dat
## sdl climate data just for 1981-1982?
sdlclim81 <- getTabular(81) %>% data.frame()
nwt_snotel <- read_csv(paste0(datpath, "data/prep_data/nwt_snotel_ppt.csv"), na = na_vals)
nwt_univcamp <- read.csv(paste0(datpath, "data/prep_data/nwt_univcamp_ppt.csv"), na = na_vals)

# saddle gridpoints
gridpoints_shp <- readOGR("/Users/scarlet/Downloads/saddle_grdpts_zip/saddle_grdpts")
gridpoints <- data.frame(gridpoints_shp)
gridpoints$LTER_POINT <- as.numeric(gridpoints$LTER_POINT)

# -- PREP PPT DATS FOR COMPARISON -----
# snowfence data
sdl_snowfence <- sdl_snowfence %>%
  # split coords
  mutate(x = as.numeric(gsub(",.*$", "", coordinate)),
         y = gsub("^.*,", "", coordinate),
         y = as.numeric(gsub("'", "", y)))
# able to plot spatially?
ggplot(subset(sdl_snowfence, !is.na(depth) & depth > 0), aes(x,y, size = month(date), col = depth, shape = type)) +
  geom_point(alpha = 0.5) +
  scale_colour_viridis_c() +
  facet_wrap(~year(date))
# does the jump in winter ppt correspend with the snow fence??
# maybe check corr vs. control plot

# plot full grid depth
sdl_snow <- left_join(sdl_snow, gridpoints[c("LTER_POINT", "coords.x1", "coords.x2")], by = c("point_ID" = "LTER_POINT"))
# create simple numeric measurement col
sdl_snow$mean_depth_num <- parse_number(sdl_snow$mean_depth)
# create water year order
sdl_snow$wy_mon <- month(sdl_snow$date)
sdl_snow$wy_mon <- with(sdl_snow, ifelse(wy_mon %in% 1:9, wy_mon +3, wy_mon - 9))

subset(sdl_snow, !is.na(mean_depth_num) & mean_depth_num > 0, select = c(date, coords.x1, coords.x2, mean_depth_num, wy_mon)) %>%
  distinct() %>%
  ggplot( aes(coords.x1, coords.x2, size= mean_depth_num)) +
  geom_point(aes(color = wy_mon), alpha = 0.5) +
  scale_colour_viridis_c() +
  facet_wrap(~year(date))

# add water year to data
wateryear <- function(dat){
  dat$mon <- month(dat$date)
  dat$wy_mon <- with(dat, ifelse(mon %in% c(10:12), mon - 9, mon+3)) # oct = 1, sep = 12
  dat$yr <- year(dat$date)
  dat$wy <- with(dat, ifelse(mon %in% c(10:12), yr+1, yr)) #wy = oct 1-sep 30
  return(dat)
}

sdl_snow <- wateryear(sdl_snow)
# track accumulated snow per year per point (use mean depth)
sdl_snow <- sdl_snow %>%
  subset(!is.na(mean_depth_num)) %>%
  arrange(date) %>%
  group_by(point_ID, wy) %>%
  # check that all yrs starts at oct (or which month if not)
  mutate(delta = mean_depth_num - lag(mean_depth_num),
         startmo = min(wy_mon),
         stopmo = max(wy_mon),
         startsno = mean_depth_num[date == min(date)],
         sample = 1:length(mean_depth_num)) %>%
  ungroup() %>%
  mutate(delta = ifelse(is.na(delta) & sample==1, startsno, delta))
    
sdlsnow_accum <- subset(sdl_snow, delta > 0) %>%
  group_by(point_ID, wy) %>%
  mutate(accum = cumsum(delta)) %>%
  filter(sample == max(sample)) %>%
  ungroup()

sdlsnow_accum %>%
  mutate(mon_name = month(date, label = T,abbr = T)) %>%
  ggplot(aes(coords.x1, coords.x2, size = accum, color = mon_name)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_d(option = "A") +
  labs(subtitle = "SDL grid, max accumulated snow within water year, colored by month of max accumulation") +
  facet_wrap(~wy)

# look at distribution across yrs
sdlsnow_accum %>%
  mutate(mon_name = month(date, label = T,abbr = T)) %>%
  ggplot(aes(wy, accum, group = wy)) +
  geom_jitter(aes(col = mon_name), width = 0.2, height =0, alpha = 0.85) +
  geom_boxplot(notch = T, fill = "transparent") +
  scale_x_continuous(breaks = seq(1990, 2021, 4)) +
  stat_summary(fill = "grey80", pch=21, size = 1) +
  scale_color_viridis_d(option = "A")


# keep max accumulated depth per wy and sum sdl ppt to that date per wy
sdlsnow_maxacc <- group_by(sdlsnow_accum, wy) %>%
  filter(accum == max(accum)) %>%
  ungroup() %>%
  # drop double site in 2012
  filter(!(wy == 2012 & sample == 9))

# is max accumulated in the same spot year to year?
ggplot(sdlsnow_maxacc, aes(coords.x1, coords.x2, color = accum, size = wy_mon)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(subtitle = "SDL gridpoint of max accumulated snow over winter, annually") +
  facet_wrap(~wy) # nope


# ppt trends
sdl_infilled <- wateryear(sdl_infilled)
sdl_wyppt <- group_by(sdl_infilled, wy, wy_mon) %>%
  summarise(ppt = sum(ppt_tot), .groups = "drop_last") %>%
  group_by(wy) %>%
  mutate(wyppt = sum(ppt))

d1ppt <- wateryear(d1ppt)
d1_wyppt <- group_by(d1ppt, wy, wy_mon) %>%
  summarise(ppt = sum(precip), .groups = "drop_last") %>%
  group_by(wy) %>%
  mutate(wyppt = sum(ppt))

c1ppt <- wateryear(c1ppt)
c1_wyppt <- group_by(c1ppt, wy, wy_mon) %>%
  summarise(ppt = sum(precip), .groups = "drop_last") %>%
  group_by(wy) %>%
  mutate(wyppt = sum(ppt))

ggplot(distinct(sdl_wyppt[c("wy", "wyppt")]), aes(wy, wyppt)) + geom_point() + geom_line()+
  geom_point(data = distinct(d1_wyppt[c("wy", "wyppt")]), aes(wy, wyppt), col = "blue") +
  geom_line(data = distinct(d1_wyppt[c("wy", "wyppt")]), aes(wy, wyppt), col = "blue") +
  geom_point(data = distinct(c1_wyppt[c("wy", "wyppt")]), aes(wy, wyppt), col = "forestgreen") +
  geom_line(data = distinct(c1_wyppt[c("wy", "wyppt")]), aes(wy, wyppt), col = "forestgreen")


# -- CORR TEST SDL PPT ~ SDL MAX SNOW -----
# SCE suggests checking relationship btw sdl ppt and max sdl snowdepth
# expect positive corr (except wind may factor); if corr reasonable, proceed using sdl ppt for climate trends at NWT in renewal
# > if corr weak, defer to D1 ppt record

# since already infilled, check with sdl ppt through 2018 used for 2019 site visit
# Noc - May windows
# > check ppt vs. abs max depth (watch timing)
# also look at monthly relationship, and possibly cumulative change (does it match?)

# join by wy
sdlppt_maxsno <-  rename(sdl_infilled, sdldate = date) %>%
  # join d1 for comparison
  left_join(d1ppt[c("date", "precip")], by = c("sdldate" = "date")) %>%
  left_join(sdlsnow_maxacc[c("date", "wy", "coords.x1", "coords.x2", "accum", "sample")]) %>%
  subset(wy >= 1993) %>%
  group_by(wy) %>%
  filter(sdldate <= date) %>%
  group_by(wy, accum, date, coords.x1, coords.x2) %>%
  summarise(cumppt_sdl = sum(ppt_tot),
            cumppt_d1 = sum(precip)) %>%
  ungroup()

with(subset(sdlppt_maxsno, wy < 2019), cor.test(cumppt_sdl,accum)) # about a 75% correlation
with(subset(sdlppt_maxsno, wy < 2019), cor.test(cumppt_d1,accum)) #hm.. similar

subset(sdlppt_maxsno, wy < 2019) %>%
  gather(site, cumppt, cumppt_sdl, cumppt_d1) %>%
  ggplot(aes(cumppt, accum, col = wy)) +
  geom_point(aes(shape = wy < 1995), size = 2) +
  geom_smooth(method = "lm") +
  geom_abline(aes(intercept = 0, slope = 1), lty = 2) +
  labs(subtitle = "D1 vs. SDL ppt relationship to SDL accumulated snow, annually",
       x = "Cumulative ppt (mm) Oct 1 to max snow accumulation date",
       y= "Max accumulated snow at Saddle grid") +
  scale_color_viridis_c() +
  
  facet_wrap(~site)

# SCE also wants to see the relationship between sdl ppt and the mean of max depth (or max accum)
# try regressions vs. all points (bc should be same as max depth?).. my way matches cumulative ppt up to the date of max accumulated

# pull max accumulated then iterate through a sum ppt to the max sample date per point
# use sdlsnow_accum first
sdlsnow_accum2 <- sdlsnow_accum
sdlsnow_accum2$ppt_to_date <- NA
sdlsnow_accum2$nobs_ppt <- NA
sdlsnow_accum2$d1ppt_to_date <- NA
sdlsnow_accum2$d1nobs_ppt <- NA

for(i in 1:nrow(sdlsnow_accum2)){
  temprow <- sdlsnow_accum2[i,]
  tempppt <- subset(sdl_infilled, wy == temprow$wy & date <= temprow$date)
  d1ppt_temp <- subset(d1ppt, wy == temprow$wy & date <= temprow$date)
  # crunch sdl ppt
  sdlsnow_accum2$ppt_to_date[i] <- sum(tempppt$ppt_tot)
  sdlsnow_accum2$nobs_ppt[i] <- nrow(tempppt)
  # crunch d1 ppt
  sdlsnow_accum2$d1ppt_to_date[i] <- sum(d1ppt_temp$precip)
  sdlsnow_accum2$d1nobs_ppt[i] <- nrow(d1ppt_temp)
}


plot_grid(
ggplot(subset(sdlsnow_accum2, wy < 2019), aes(ppt_to_date, accum, col = wy)) +
  geom_point() +
  geom_smooth(col = "black", method = "lm") +
  labs(subtitle = "sdl relationship") +
  scale_color_viridis_c(),

ggplot(sdlsnow_accum2, aes(d1ppt_to_date, accum, col = wy)) +
  geom_point() +
  geom_smooth(col = "black", method = "lm") +
  #geom_abline(aes(slope = 1, intercept = 0), col = "red", lty = 2) +
  labs(subtitle = "d1 relationship") +
  scale_color_viridis_c()
)

# sdl v d1
with(subset(sdlsnow_accum2, wy < 2019 & wy > 1995), cor.test(accum,ppt_to_date))
with(subset(sdlsnow_accum2, wy < 2019 & wy > 1995), cor.test(accum,d1ppt_to_date))

summary(lm(accum ~ ppt_to_date, data = subset(sdlsnow_accum2, wy < 2019)))
summary(lm(accum ~ d1ppt_to_date, data = subset(sdlsnow_accum2, wy < 2019 & wy > 1995)))

# look at average across whole grid.. except then what do you sum ppt to? do you avg summed ppt then?
# try SCE way -- mean max depth v tot ppt ()
sdl_snowmax <- wateryear(sdl_snow) %>%
  # want complete water years only
  group_by(wy) %>%
  mutate(nobs = length(unique(wy_mon))) %>% # manual exam: looks like sampled to spring in all yrs (more recent yrs no fall surveys)
  group_by(wy, point_ID) %>%
  filter(mean_depth_num == max(mean_depth_num)) %>%
  mutate(nobs = length(unique(date))) %>%
  dplyr::select(coords.x1, coords.x2, point_ID, wy, mean_depth_num, nobs) %>%
  distinct()
boxplot(sdl_snowmax$nobs) # some max depths per WY per grid point were same on multiple dates..

# I still think it makes sense to crunch cumulative ppt to the max depth date per point, and then avg both of those
# unless it's just a hard oct-may window.. but snow sampling events are uneven
sdl_snowmax <- wateryear(sdl_snow) %>%
  # want complete water years only
  group_by(wy) %>%
  mutate(nobs = length(unique(wy_mon))) %>% # manual exam: looks like sampled to spring in all yrs (more recent yrs no fall surveys)
  group_by(wy, point_ID) %>%
  filter(mean_depth_num == max(mean_depth_num)) %>%
  # take the first date of the max depth measurement per point
  filter(date == min(date)) %>%
  dplyr::select(coords.x1, coords.x2, point_ID, wy, mean_depth_num, date, wy_mon, mon, nobs) %>%
  distinct() %>% ungroup()

# sum ppt to date per point
sdl_snowmax$ppt_to_date <- NA
sdl_snowmax$nobs_ppt <- NA 
sdl_snowmax$d1ppt_to_date <- NA
sdl_snowmax$d1nobs_ppt <- NA

for(i in 1:nrow(sdl_snowmax)){
  temprow <- sdl_snowmax[i,]
  tempppt <- subset(sdl_infilled, wy == temprow$wy & date <= temprow$date)
  d1ppt_temp <- subset(d1ppt, wy == temprow$wy & date <= temprow$date)
  # crunch sdl ppt
  sdl_snowmax$ppt_to_date[i] <- sum(tempppt$ppt_tot)
  sdl_snowmax$nobs_ppt[i] <- nrow(tempppt)
  # crunch d1 ppt
  sdl_snowmax$d1ppt_to_date[i] <- sum(d1ppt_temp$precip)
  sdl_snowmax$d1nobs_ppt[i] <- nrow(d1ppt_temp)
}


plot_grid(
  ggplot(subset(sdl_snowmax, wy < 2019 & wy > 1995), aes(ppt_to_date, mean_depth_num, col = wy)) +
    geom_point(alpha = 0.6) +
    geom_smooth(col = "black", method = "lm") +
    labs(subtitle = "sdl relationship", y = "max snow depth") +
    scale_color_viridis_c(),
  
  ggplot(subset(sdl_snowmax, wy < 2019 & wy > 1995), aes(d1ppt_to_date, mean_depth_num, col = wy)) +
    geom_point(alpha = 0.6) +
    geom_smooth(col = "black", method = "lm") +
    #geom_abline(aes(slope = 1, intercept = 0), col = "red", lty = 2) +
    labs(subtitle = "d1 relationship", y = "max snow depth") +
    scale_color_viridis_c()
)

with(subset(sdl_snowmax, wy < 2019 & wy > 1995), cor.test(mean_depth_num,ppt_to_date)) #0.49 sdl
with(subset(sdl_snowmax, wy < 2019 & wy > 1995), cor.test(mean_depth_num,d1ppt_to_date)) #0.47 d1

# take average of wys
sdl_snowmax_mean <- subset(sdl_snowmax, wy < 2019 & wy > 1995) %>%
  group_by(wy) %>%
  summarise(mean_sdlmax = mean(mean_depth_num),
         se_sdlmax = sd(mean_depth_num)/sqrt(length(mean_depth_num)),
         mean_sdlppt = mean(ppt_to_date),
         se_sdlppt = sd(ppt_to_date)/sqrt(length(ppt_to_date)),
         mean_d1ppt = mean(d1ppt_to_date),
         se_d1ppt = sd(d1ppt_to_date)/sqrt(length(d1ppt_to_date)),
  ) %>% ungroup()

plot_grid(
  ggplot(sdl_snowmax_mean, aes(mean_sdlppt, mean_sdlmax, col = wy)) +
    geom_point() +
    geom_errorbar(aes(ymax = se_sdlmax + mean_sdlmax, ymin = mean_sdlmax - se_sdlmax)) +
    geom_errorbar(aes(xmax = se_sdlppt + mean_sdlppt, xmin = mean_sdlppt - se_sdlppt)) +
    geom_smooth(col = "black", method = "lm") +
    labs(subtitle = "sdl relationship", y = "mean of max snowdepth on sdl grid",
         x = "mean sdl ppt to max depth date") +
    scale_color_viridis_c(),
  
  ggplot(sdl_snowmax_mean, aes(mean_d1ppt, mean_sdlmax, col = wy)) +
    geom_point() +
    geom_errorbar(aes(ymax = se_sdlmax + mean_sdlmax, ymin = mean_sdlmax - se_sdlmax)) +
    geom_errorbar(aes(xmax = se_d1ppt + mean_d1ppt, xmin = mean_d1ppt - se_d1ppt)) +
    geom_smooth(col = "black", method = "lm") +
    #geom_abline(aes(slope = 1, intercept = 0), col = "red", lty = 2) +
    labs(subtitle = "d1 relationship", y = "mean of max snowdepth on sdl grid",
         x = "mean d1 ppt to max depth date") +
    scale_color_viridis_c()
)

summary(lm(mean_sdlmax ~ mean_sdlppt, data = sdl_snowmax_mean))
summary(lm(mean_sdlmax ~ mean_d1ppt, data = sdl_snowmax_mean))
with(sdl_snowmax_mean, cor.test(mean_sdlmax, mean_sdlppt))
with(sdl_snowmax_mean, cor.test(mean_sdlmax, mean_d1ppt))

# out of curiosity, what is the sdl ppt relationship to d1?
with(sdl_snowmax_mean, cor.test(mean_d1ppt, mean_sdlppt)) #0.8
plot(mean_d1ppt ~ mean_sdlppt, data = sdl_snowmax_mean)

# to see, look at mean of accumulated snow metric

sdlsnow_accum_mean <-  subset(sdlsnow_accum2, wy < 2019 & wy > 1995) %>%
  group_by(wy) %>%
  summarise(mean_sdlaccum = mean(accum),
            se_sdlaccum = sd(accum)/sqrt(length(accum)),
            mean_sdlppt = mean(ppt_to_date),
            se_sdlppt = sd(ppt_to_date)/sqrt(length(ppt_to_date)),
            mean_d1ppt = mean(d1ppt_to_date),
            se_d1ppt = sd(d1ppt_to_date)/sqrt(length(d1ppt_to_date)),
  ) %>% ungroup()

plot_grid(
  ggplot(sdlsnow_accum_mean, aes(mean_sdlppt, mean_sdlaccum, col = wy)) +
    geom_point() +
    geom_errorbar(aes(ymax = se_sdlaccum + mean_sdlaccum, ymin = mean_sdlaccum - se_sdlaccum)) +
    geom_errorbar(aes(xmax = se_sdlppt + mean_sdlppt, xmin = mean_sdlppt - se_sdlppt)) +
    geom_smooth(col = "black", method = "lm") +
    labs(subtitle = "sdl relationship", y = "mean of accum. snow on sdl grid",
         x = "mean sdl ppt to max accum. date") +
    scale_color_viridis_c(),
  
  ggplot(sdlsnow_accum_mean, aes(mean_d1ppt, mean_sdlaccum, col = wy)) +
    geom_point() +
    geom_errorbar(aes(ymax = se_sdlaccum + mean_sdlaccum, ymin = mean_sdlaccum - se_sdlaccum)) +
    geom_errorbar(aes(xmax = se_d1ppt + mean_d1ppt, xmin = mean_d1ppt - se_d1ppt)) +
    geom_smooth(col = "black", method = "lm") +
    #geom_abline(aes(slope = 1, intercept = 0), col = "red", lty = 2) +
    labs(subtitle = "d1 relationship", y = "mean of accum. snow on sdl grid",
         x = "mean d1 ppt to max accum. date") +
    scale_color_viridis_c()
)

summary(lm(mean_sdlaccum ~ mean_sdlppt, data = sdlsnow_accum_mean))
summary(lm(mean_sdlaccum ~ mean_d1ppt, data = sdlsnow_accum_mean)) #d1 better here too
with(sdlsnow_accum_mean, cor.test(mean_sdlaccum, mean_sdlppt)) #0.69 sdl
with(sdlsnow_accum_mean, cor.test(mean_sdlaccum, mean_d1ppt)) #0.79 d1

# to be extra certain, compare mean max depth to oct-may total ppt (just to see what consequence of averaging cumulative ppt per max date might be)

snowmax_wyppt <- left_join(sdl_snowmax_mean, distinct(sdl_wyppt[c("wy", "wyppt")])) %>%
  rename(sdl_wyppt = wyppt)%>%
  left_join(distinct(d1_wyppt[c("wy", "wyppt")])) %>%
  rename(d1_wyppt = wyppt) %>%
  dplyr::select(wy, mean_sdlmax, se_sdlmax, sdl_wyppt, d1_wyppt) %>%
  gather(site, wy_ppt, sdl_wyppt, d1_wyppt)

ggplot(snowmax_wyppt, aes(wy_ppt, mean_sdlmax)) +
  geom_text(aes(label = wy)) +
  geom_smooth(method = "lm") +
  facet_wrap(~site)
with(subset(snowmax_wyppt, grepl("sdl", site)), cor.test(wy_ppt, mean_sdlmax)) #.61. p = 0.002
with(subset(snowmax_wyppt, grepl("d1", site)), cor.test(wy_ppt, mean_sdlmax)) #.78. p = 1.292 e-05

# -- LOOK FOR BREAK AT SADDLE ----
# if create monthly time series, will change show up as a breakpoint?
# keep complete water years only
sdlppt_ts <- ts(sdl_wyppt$ppt[sdl_wyppt$wy %in% c(1982:2018)], frequency = 12) #start = c(1981,10), end = c(2018,9),
sdlppt_components <- decompose(sdlppt_ts)
plot(sdlppt_components)
sdldf <- cbind(subset(sdl_wyppt, wy %in% c(1982:2018)), list2DF(sdlppt_components))

# subset: d1 1952 oct - 2020 sep 
d1ppt_ts <- ts(d1_wyppt$ppt[d1_wyppt$wy %in% c(1953:2020)], frequency = 12) #start = c(1952,10), end = c(2020,9), 
d1ppt_components <- decompose(d1ppt_ts)
plot(d1ppt_components)
d1df <- cbind(subset(d1_wyppt, wy%in% c(1953:2020)), list2DF(d1ppt_components))

# subset: c1 1952 oct - 2020 sep 
c1ppt_ts <- ts(c1_wyppt$ppt[c1_wyppt$wy %in% c(1953:2020)], frequency = 12) # start = c(1952,10), end = c(2020,9),
c1ppt_components <- decompose(c1ppt_ts)
plot(c1ppt_components)
c1df <- cbind(subset(c1_wyppt, wy%in% c(1953:2020)), list2DF(c1ppt_components))

# stack all
stackppt <- rbind(cbind(site = "sdl", sdldf),
                  cbind(site = "d1", d1df),
                  cbind(site = "c1", c1df))

# look at trends across all sites
subset(stackppt, select = -c(x, type, figure, wyppt)) %>%
  gather(met, val, ppt:ncol(.)) %>%
  # create date for plotting purposes
  mutate(date = as.Date(paste(wy, wy_mon, 1, sep = "-"))) %>%
  ggplot(aes(date, val, col= site)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~met, scales = "free")

# look at seasonal only
stackppt %>%
  # create date for plotting purposes
  mutate(date = as.Date(paste(wy, wy_mon, 1, sep = "-"))) %>%
  subset(wy %in% c(1982:1984)) %>%
  ggplot(aes(date, seasonal)) +
  geom_line() +
  labs(x = "water year (month 1 = october, 12 = sep)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m") +
  facet_wrap(~site, nrow = 3)

# look at trend
# look at seasonal only
stackppt %>%
  # create date for plotting purposes
  mutate(date = as.Date(paste(wy, wy_mon, 1, sep = "-"))) %>%
  ggplot(aes(date, trend, col = site)) +
  geom_line() +
  labs(x = "water year") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  geom_smooth()
  #scale_x_date(date_breaks = "1 month", date_labels = "%y-%m") +
  #facet_wrap(~site, nrow = 3)

# where is breakpoint?
cpt.mean(ts(sdldf$trend[!is.na(sdldf$trend)])) #151
sdltrend <- sdldf[!is.na(sdldf$trend), ]
sdltrend[151,] # break in mean is at oct 1995
# when does variance change?
cpt.var(ts(sdldf$trend[!is.na(sdldf$trend)])) #358
sdltrend[358,] #2012 jan

cpt.mean(ts(d1df$trend[!is.na(d1df$trend)])) #347
d1trend <- d1df[!is.na(d1df$trend),]
d1trend[347,] # feb 1982..
cpt.mean(ts(c1df$trend[!is.na(c1df$trend)])) #42
c1trend <- c1df[!is.na(c1df$trend),]
c1trend[42,] # sep 1956..

# how correlated are the trends between sites?
trendwide <- dplyr::select(stackppt, c(site, wy, wy_mon, trend)) %>%
  gather(trend, val, trend) %>%
  unite(trend, site, trend) %>%
  spread(trend, val)
cor.test(trendwide$c1_trend, trendwide$d1_trend) #.40
plot(trendwide$d1_trend ~ trendwide$c1_trend)
with(subset(trendwide, !is.na(sdl_trend)), cor.test(d1_trend, sdl_trend)) #.30
with(subset(trendwide, !is.na(sdl_trend)), cor.test(c1_trend, sdl_trend)) #.19
# c1 and d1 are more correlated than sdl to either..

# what do data deseosoned look like?
stackppt %>%
  # create date for plotting purposes
  mutate(date = as.Date(paste(wy, wy_mon, 1, sep = "-"))) %>%
  subset(wy >1981) %>%
  ggplot(aes(date, ppt-seasonal, col = site)) +
  geom_line(alpha = 0.5, lwd= 1) +
  labs(x = "water year (month 1 = october, 12 = sep)")
  #scale_x_date(date_breaks = "1 month", date_labels = "%y-%m") +
  #facet_wrap(~site, nrow = 3)


# -- COMPARE W JENNINGS ET Al. DATASET----
# I've done this before (for extended summer work) and there is a reason it's not helpful.. but can't remember, so checking again
# > I think it's something about how sdl ppt was disaggregated to hourly in the jennings et al. dataset, it won't match exactly when sum daily
jppt_daily <- subset(jppt) %>%
  group_by(date, local_site) %>%
  summarise(jppt = sum(ppt_tot)) %>%
  wateryear(.)

jsdl_ppt <- subset(jppt_daily, local_site == "sdl")
# how does it plot?
jsdl_ppt %>%
  left_join(sdl_infilled) %>%
  ggplot(aes(ppt_tot, jppt, col = wy)) +
  geom_point(alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept= 0), lty = 2) +
  scale_color_viridis_c()

# check water yr cumulative ppt
jsdl_ppt %>%
  left_join(sdl_infilled[c("date", "ppt_tot", "day")]) %>%
  rename(chart_ppt = ppt_tot) %>%
  gather(dataset, val, jppt, chart_ppt) %>%
  arrange(wy, wy_mon, date) %>%
  group_by(dataset, wy) %>%
  mutate(cumppt = cumsum(val),
         wy_doy = 1:length(val),
         maxdoy = max(wy_doy)) %>%
  #filter out any wy that aren't complete
  subset(maxdoy>300) %>%
  ungroup() %>%
  ggplot(aes(wy_doy, cumppt, col = dataset, group = dataset)) +
  geom_line(alpha = 0.5, lwd = 2) +
  #scale_color_viridis_d() +
  facet_wrap(~wy)

# cumsum j daily ppt
jppt_daily <- jppt_daily %>%
  # left_join(sdl_infilled[c("date", "ppt_tot", "day")]) %>%
  # rename(chart_ppt = ppt_tot) %>%
  #gather(dataset, val, jppt, chart_ppt) %>%
  arrange(wy, wy_mon, date) %>%
  group_by(wy, local_site) %>%
  mutate(cumppt = cumsum(jppt),
         wy_doy = 1:length(jppt),
         maxdoy = max(wy_doy)) %>%
  #filter out any wy that aren't complete
  subset(maxdoy>300) %>%
  ungroup()

ggplot(jppt_daily, aes(wy_doy, cumppt, col = local_site, group = local_site)) +
  geom_line(alpha = 0.5, lwd = 2) +
  #scale_color_viridis_d() +
  facet_wrap(~wy)

