# infilling for saddle precip 2015-current date on EDI


# hierarchy of infill methods for precip
# 1) method 1 (ratio method) using D1
# 2) method 1 (ratio method) using C1

# some rules:
# use same precip data used in NSF proposal for years 1982 - 2014
# add in chart data for 2015-current, NAs infilled by CTW using ratio method from NWT metadata
# for 2015-current, only keep raw chart data where qdays = 1; qdays > 1 --> NA (infill)
# ratios calculated based on days both sdl and infill source site had observations

## from methods section in NWT saddle chart precip metadata:
# (Method Flag 1) Daily Ratio Methodology:
# Total precipitation for the date in question was calculated for the the period 1981-2008, for both the 'Known Site' and the 'Unknown Site', only for days where daily values existed for both sites (missing values and QD's > 1 were removed). 
# A ratio was determined [Unknown Site : Known Site] based on these totals.  
# This ratio was then multiplied by the value for that date at the 'Known Site' in order to determine the 'Unknown Site' value.  

# Precipitation data adjacency hierarchy:
# SDL:
# 1) D1
# 2) C1

# > CTW calculated ratios by month instead of by day, didn't trust variation day to day enough, wanted to generate more conservative infilling estimates 

# -- SETUP ------
# clean environment, load needed libraries, modify default settings
rm(list = ls())
library(tidyverse)
library(lubridate)
options(stringsAsFactors = F)
theme_set(theme_bw())

#set path to extended summer analysis (wherever want to write out prepped ppt csv)
datpath <- "extended_summer/analysis/"

# -- FUNCTIONS -----
source("utility_functions/utility_functions_all.R")


# -- GET DATA -----
# data used in NSF proposal
# Hope Humphries sent the infilled saddle data to Emily Farrer, CTW doesn't know how infilled
# assume infilled data sent to HH from Tim Kittel, and methodology likely similar to that used in Jennings, Molotch, and Kittel 2018
NSF_precip <- read_csv("~/Dropbox/NWT_data/Saddle_precip_temp_formoisturedeficit.csv") %>%
  mutate(`date` = as.Date(paste(Year,Month,Day, sep="-")),
         LTER_site = "NWT",
         local_site = "sdl") %>%
  dplyr::select(LTER_site, local_site, date, PCP) %>%
  dplyr::rename(ppt_tot = PCP) 
## NOTE: Emily's data has already been corrected for snow blowing Sep-May (multiply chart value * 0.39)
## NOTE2: If revise this analysis, correction should only be applied Oct-May from what CTW has read, but will follow what Emily did


# read in  data from EDI data portal
# sdl chart precip data
sdl_chartpcp <- getTabular(416)
# c1 chart precip data
c1_chartpcp <- getTabular(414)
# d1 chart precip data
d1_chartpcp <- getTabular(415)
# Jennings, Molotch, and Kittel (2018) infilled data for sdl, d1 and c1
# Keith Jennings et al. infilled *hourly* data
Jennings_infill <- getTabular(168) 


# read in tim kittel d1 and c1 daily infilled to compare with sdl 1982-1994 (look for jump in precip values)
# from NWT renewal dropbox
tkd1 <- read.csv("../../Dropbox/NWT_data/d1_infilled_daily_ppt.csv")
tkc1 <- read.csv("../../Dropbox/NWT_data/c1_infilled_daily_ppt.csv")


# -- PREP DATA -----
glimpse(sdl_chartpcp)
glimpse(d1_chartpcp)
glimpse(c1_chartpcp)
glimpse(Jennings_infill)
glimpse(tkd1)
glimpse(tkc1)

# summarise Jennings et al. data to daily totals (ppt), or max, min, or mean (temp)
Jennings_summarized <- Jennings_infill %>%
  group_by(LTER_site, local_site, year, date, jday) %>%
  summarise(ppt_tot = sum(ppt_tot),
            airtemp_max = mean(airtemp_avg),
            airtemp_min = min(airtemp_avg),
            airtemp_avg = mean(airtemp_avg))

# subset T. Kittel infilled daily ppt to dates that overlap with sdl
tkd1 <- tkd1 %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  subset(date %in% sdl_chartpcp$date)
tkc1 <- tkc1 %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  subset(date %in% sdl_chartpcp$date)
# stack tk ppt datasets
## colnames in d1 dat same as c1
colnames(tkd1)[1:11] <- colnames(tkc1)[1:11] 
tkdat <- dplyr::select(tkd1, colnames(tkc1)) %>%
  mutate(local_site = "tkd1") %>%
  rbind(cbind(tkc1, local_site = "tkc1")) %>%
  dplyr::select(local_site, date, Month:Source.Station) %>%
  rename(ppt = `Precipitation..mm.`) %>%
  # make wide form for comparing with sdl chart pcp
  gather(met, val, ppt:ncol(.)) %>%
  unite(var, local_site, met) %>%
  spread(var, val) %>%
  # need to make ppt numeric since was gathered with char cols and coerced to char
  mutate(tkc1_ppt = as.numeric(tkc1_ppt),
         tkd1_ppt = as.numeric(tkd1_ppt))



##############################
## Assess 1982-1994 SDL ppt ##
##############################
# looking for step change somewhere in mid 90s; J. Morse said wind shield changes on sdl chart then, might have influenced ppt values prior (artificially low values compared to later mid-90s onwards)
# take difference between sdl and c1 ppt, and sdl and d1 ppt and look for change in that relationship by month by year

chartppt_all <- sdl_chartpcp %>%
  rename(sdl_ppt = ppt_tot) %>%
  # join nsf ppt for comparison
  left_join(NSF_precip) %>%
  # rename to avoid confusion about what's what
  rename(nsf_ppt = ppt_tot) %>%
  # add un-winter corrected NSF precip to compare with raw chart (multiply sep-may nsf ppt by 1/0.39)
  mutate(raw_nsfppt = ifelse(month(date) %in% c(9:12, 1:5), nsf_ppt * (1/0.39), nsf_ppt)) %>%
  # take difference
  mutate(sdl_rawnsf_diff = round(sdl_ppt - raw_nsfppt, 4)) # get rid of scientific number from multiplying by 1/0.39 abovw
# look at difference by qualifying days == 1 vs qdays > 1
boxplot(sdl_rawnsf_diff ~ (qdays>1), data = chartppt_all, main = "SDL chart ppt - NSF ppt, by qdays")
# this means qdays >1 ppt corrected for in the NSF ppt data (phew!)
# what about any flagging in dataset?
boxplot(sdl_rawnsf_diff ~ is.na(flag_ppt_tot), data = subset(chartppt_all, qdays == 1), main = "SDL chart ppt - NSF ppt, by flag")
# flagged values were used in nsf renewal (flagging just indicates infill method, not a data quality issue)
# > move on with comparing nsf data to d1 and c1 tk data

chartppt_all2 <- chartppt_all %>%
  # drop raw sdl chart since only looking for time step in nsf renewal years
  dplyr::select(LTER_site:date, nsf_ppt:raw_nsfppt) %>%
  # join tkdat
  left_join(tkdat) %>%
  # also join jennings data for comparison
  left_join(Jennings_summarized[Jennings_summarized$local_site == "sdl", c("date", "ppt_tot")]) %>%
  rename(jk_sdlppt = ppt_tot) %>%
  # create diff cols based on *winter overcatch uncorrected* [raw] data
  mutate(nsf_diff_c1 = round(nsf_ppt - tkc1_ppt, 2), # remove sci not
         nsf_diff_d1 = round(nsf_ppt - tkd1_ppt, 2),
         nsf_diff_jksdl = round(nsf_ppt - jk_sdlppt, 2))

# quick check on if TK used sdl chart as they were to infill d1 and c1
# e.g. tk used sdl to infill d1 ppt on 1981-09-27, with 14 x observations (assume 2 week window used, droped qdays > 1, see if get similar regression and output)

# notes from tk paper on 2wk infill method for ppt:
# Least-squares linear regression infilling models developed from daily records (upgraded from month-based regression and non-statistical approaches; Greenland 1987, 1989), with logarithmic transformation of daily precipitation values

test <- subset(tkd1, date %in% seq.Date(as.Date("1981-09-27")-13, as.Date("1981-09-27")+13, 1)) %>%
  # extract ppt vals where d1 was source (i.e. not an infilled value)
  mutate(d1chart = ifelse(grepl("D1", Source.Station), `Precipitation..mm.`, NA)) %>%
  left_join(sdl_chartpcp) %>%
  # log transform daily ppt?
  mutate(log_sdl = ifelse(ppt_tot == 0, ppt_tot, log(ppt_tot)),
         log_d1 = ifelse(d1chart == 0, d1chart, log(d1chart)))
# regression using full 2-wk window before and after date
summary(glm(log_sdl ~ log_sdl, data = subset(test, !is.na(d1chart)))) # doesn't match tk regression 
# > .. idk? i've tried abs values and varied windows around and can't match what tk did.. looking at regression of same day, there are more than 14 Sep 27 with data available at d1 and sdl chart 1982-2010.. move on for now

# compare differences over time to look for break point
chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_diff_c1:ncol(.)) %>%
  gather(compare, diff, nsf_diff_c1:ncol(.)) %>%
  # i forgot keith used sdl data as they are, so don't expect to see a changepoint in deltas
  filter(compare != "nsf_diff_jksdl" & !is.na(diff) & diff != 0) %>%
  ggplot(aes(date, diff)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0), col = "red") +
  geom_vline(aes(xintercept = as.Date("1995-01-01")), col = "red") +
  facet_grid(~compare)


chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_diff_c1:ncol(.)) %>%
  gather(compare, diff, nsf_diff_c1:ncol(.)) %>%
  mutate(doy = yday(date),
         yrgroup = ifelse(Year < 1990, "80s",
                          ifelse(Year < 1995, "early 90s",
                                 ifelse(Year < 2000, "late 90s", "2000s")))) %>%
  mutate(yrgroup = factor(yrgroup, levels = c("80s", "early 90s", "late 90s", "2000s"))) %>%
  mutate(doy = yday(date),
         yrgroup2 = ifelse(Year < 1995, Year,
                                 ifelse(Year < 2000, "1995-1999", "2000s"))) %>%
  # i forgot keith used sdl data as they are, so don't expect to see a changepoint in deltas
  filter(compare != "nsf_diff_jksdl" & !is.na(diff)) %>%
  ggplot(aes(doy, diff, col = Year, group = Year)) + 
  #geom_point(alpha = 0.5) +
  geom_smooth(se = F) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_c() +
  #geom_vline(aes(xintercept = as.Date("1995-01-01")), col = "red") +
  facet_grid(compare ~yrgroup2, scales = "free_y")

# try differences by year
chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_diff_c1:nsf_diff_d1) %>%
  gather(compare, diff, nsf_diff_c1:ncol(.)) %>%
  # i forgot keith used sdl data as they are, so don't expect to see a changepoint in deltas
  filter(compare != "nsf_diff_jksdl" & !is.na(diff)) %>%
  #filter(compare == "nsf_diff_c1" & !is.na(diff)) %>%
  ggplot(aes(Year, diff, group = Year)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0), col = "red") +
  #geom_vline(aes(xintercept = as.Date("1995-01-01")), col = "red") +
  facet_wrap(compare~Month, scales = "free_y")

# try mean differences by year?
chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_diff_c1:nsf_diff_d1) %>%
  gather(compare, diff, nsf_diff_c1:ncol(.)) %>%
  # i forgot keith used sdl data as they are, so don't expect to see a changepoint in deltas
  filter(compare != "nsf_diff_jksdl" & !is.na(diff) & ! diff == 0) %>%
  group_by(Year, Month, compare) %>%
  summarise(meandiff = mean(diff),
            sediff = sd(diff)/sqrt(length(diff))) %>%
  ungroup() %>%
  ggplot(aes(Year, meandiff, col = compare)) + 
  geom_errorbar(aes(ymax = meandiff + sediff, ymin = meandiff-sediff), width = 0) +
  geom_point() +
  geom_smooth(aes(fill = compare)) +
  scale_x_continuous(breaks = seq(1980, 2010, 5)) +
  facet_wrap(~Month, scales = "free_y")


# difference against difference?
chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_diff_c1:nsf_diff_d1) %>%
  gather(compare, diff, nsf_diff_c1:ncol(.)) %>%
  # i forgot keith used sdl data as they are, so don't expect to see a changepoint in deltas
  filter(compare != "nsf_diff_jksdl" & !is.na(diff) & ! diff == 0) %>%
  group_by(Year, Month, compare) %>%
  summarise(meandiff = mean(diff)) %>%
  ungroup() %>%
  spread(compare, meandiff) %>%
  ggplot(aes(nsf_diff_c1, nsf_diff_d1, col = Year> 1994)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point() +
  facet_wrap(~Month)
  

# too hard to see.. there could be a pattern in early record winter months, but maybe not. try compare differences in monthly totals
chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_ppt, tkc1_ppt, tkd1_ppt) %>%
  filter(Year < 2001) %>%
  group_by(Year, Month) %>%
  summarise(sdl = sum(nsf_ppt),
            d1 = sum(tkd1_ppt),
            c1 = sum(tkc1_ppt)) %>%
  ungroup() %>%
  gather(met, val, sdl:ncol(.)) %>%
  mutate(met = factor(met, levels = c("c1", "sdl", "d1"))) %>%
  ggplot() +
  geom_line(aes(Month, val, col = met)) +
  scale_color_manual(name = "site", values = c("c1" = "forestgreen", "sdl" = "orchid", "d1" = "lightblue")) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~Year, scales = "free_y")


chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_ppt, tkc1_ppt, tkd1_ppt) %>%
  filter(Year < 2011) %>%
  group_by(Year, Month) %>%
  summarise(sdl = sum(nsf_ppt),
            d1 = sum(tkd1_ppt),
            c1 = sum(tkc1_ppt)) %>%
  ungroup() %>%
  gather(met, val, sdl:ncol(.)) %>%
  mutate(met = factor(met, levels = c("c1", "sdl", "d1"))) %>%
  ggplot(aes(Year, val, col = met)) +
  geom_point() +
  geom_smooth(aes(fill = met)) +
  scale_color_manual(name = "site", values = c("c1" = "forestgreen", "sdl" = "orchid", "d1" = "lightblue")) +
  scale_fill_manual(name = "site", values = c("c1" = "forestgreen", "sdl" = "orchid", "d1" = "lightblue")) +
  scale_x_continuous(breaks = seq(1980,2010,5)) +
  facet_wrap(~Month, scales = "free_y")

# take monthly differences between stations
chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_ppt, tkc1_ppt, tkd1_ppt) %>%
  filter(Year < 20011) %>%
  group_by(Year, Month) %>%
  summarise(sdl = sum(nsf_ppt),
            d1 = sum(tkd1_ppt),
            c1 = sum(tkc1_ppt)) %>%
  ungroup() %>%
  mutate(sdl_diff_d1 = sdl - d1,
         sdl_diff_c1 = sdl- c1) %>%
  dplyr::select(Year, Month, sdl_diff_d1, sdl_diff_c1) %>%
  gather(compare, diff, sdl_diff_d1:sdl_diff_c1) %>%
  group_by(Month, compare) %>%
  mutate(meandiff = mean(diff)) %>%
  ungroup() %>%
  ggplot(aes(Year, diff, col = compare)) +
  geom_hline(aes(yintercept = meandiff)) +
  geom_point() +
  geom_smooth(aes(fill = compare), method = "lm") +
  #scale_color_manual(name = "site", values = c("sdl_diff_c1" = "forestgreen", "sdl_diff_d1" = "lightblue")) +
  labs(y = "SDL - other station [monthly ppt]") +
  scale_x_continuous(breaks = seq(1980, 2010, 5)) +
  facet_wrap(~Month, scales = "free_y")

# check again by water year
chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_ppt, tkc1_ppt, tkd1_ppt) %>%
  mutate(WY = ifelse(Month %in% 10:12, Year+1, Year)) %>%
  filter(Year < 2012) %>%
  group_by(WY) %>%
  summarise(sdl = sum(nsf_ppt),
            d1 = sum(tkd1_ppt),
            c1 = sum(tkc1_ppt),
            nobs = length(unique(Month))) %>%
  ungroup() %>%
  filter(nobs == 12) %>%
  gather(met, val, sdl:c1) %>%
  mutate(met = factor(met, levels = c("c1", "sdl", "d1"))) %>%
  ggplot(aes(WY, val, col = met)) +
  geom_line() +
  geom_smooth(method = "lm") +
  scale_color_manual(name = "site", values = c("c1" = "forestgreen", "sdl" = "orchid", "d1" = "lightblue"))

# try diffing monthly ppt from lagged 1Yr monthly ppt to see if stations are changing interannually by month in similar ways?
chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_ppt, tkc1_ppt, tkd1_ppt) %>%
  filter(Year < 2011) %>%
  group_by(Year, Month) %>%
  summarise(sdl = sum(nsf_ppt),
            d1 = sum(tkd1_ppt),
            c1 = sum(tkc1_ppt)) %>%
  ungroup() %>%
  gather(met, val, sdl:ncol(.)) %>%
  mutate(met = factor(met, levels = c("c1", "sdl", "d1"))) %>%
  arrange(met,Month, Year) %>%
  group_by(met) %>%
  mutate(lag1 = lag(val),
         difflag = val-lag1) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(Year, difflag, col = met), alpha = 0.75) +
  scale_color_manual(name = "site", values = c("c1" = "forestgreen", "sdl" = "orchid", "d1" = "lightblue")) +
  scale_x_continuous(breaks = seq(1980, 2010, 5)) +
  facet_wrap(~Month, scales = "free_y")


# only consider values with normal diff range (i.e. remove high discrepancies to look for )
flagppt <- chartppt_all2 %>%
  dplyr::select(date, Month, Year, nsf_ppt, tkc1_ppt, tkd1_ppt) %>%
  filter(Year < 2011) %>%
  # flag high values
  mutate(sdl_diff_d1 = nsf_ppt - tkd1_ppt,
         sdl_diff_c1 = nsf_ppt - tkc1_ppt) %>%
  group_by(Year, Month) %>%
  mutate(mean_diffd1 = mean(sdl_diff_d1),
         mean_diffc1 = mean(sdl_diff_c1),
         sd_diffd1 = sd(sdl_diff_d1),
         sd_diffc1 = sd(sdl_diff_c1)) %>%
  ungroup() %>%
  #flag diffs outside of 3sd above or below mean
  mutate(devd1 = abs((mean_diffd1 - sdl_diff_d1)/sd_diffd1),
         devc1 = abs((mean_diffc1 - sdl_diff_c1)/sd_diffc1),
         # add logical flag for if sdl ppt in between d1 and c1, even if deviance is flagged
         logippt = nsf_ppt < tkd1_ppt & nsf_ppt > tkc1_ppt)
  # filter out ppt values that are flagged in deviance and not logical in comparison to d1 and c1
flagppt %>%  
  filter(!(devd1 > 3 & devc1 > 3 & logippt == FALSE)) %>%
  # continue with summarizing monthly values and compare change
  group_by(Year, Month) %>%
  summarise(sdl = sum(nsf_ppt),
            d1 = sum(tkd1_ppt),
            c1 = sum(tkc1_ppt)) %>%
  ungroup() %>%
  gather(met, val, sdl:ncol(.)) %>%
  mutate(met = factor(met, levels = c("c1", "sdl", "d1"))) %>%
  arrange(met,Month, Year) %>%
  group_by(met) %>%
  mutate(lag1 = lag(val),
         difflag = val-lag1) %>%
  ungroup() %>%
  ggplot(aes(Year, difflag, col = met, group = met)) +
  geom_point(alpha = 0.75) +
  geom_smooth(aes(fill = met)) +
  scale_fill_manual(name = "site", values = c("c1" = "forestgreen", "sdl" = "orchid", "d1" = "lightblue")) +
  scale_color_manual(name = "site", values = c("c1" = "forestgreen", "sdl" = "orchid", "d1" = "lightblue")) +
  scale_x_continuous(breaks = seq(1980, 2010, 5)) +
  facet_wrap(~Month, scales = "free_y")


# see if can match m williams fig with data as they are
chartppt_all2 %>%
  filter(Month %in% 6:9 & Year %in% 1987:1996) %>%
  group_by(Year, Month) %>%
  summarise(sdl_ppt = sum(raw_nsfppt),
            d1_ppt = sum(tkd1_ppt)) %>%
  ungroup() %>%
  gather(met, val, sdl_ppt:d1_ppt) %>%
  mutate(yrmon = paste0(Year, Month)) %>%
  filter(yrmon != 19956) %>%
  ggplot(aes(yrmon, val, col = met, group = met)) +
  geom_line() +
  geom_point() +
  scale_color_manual(name = "site", values = c("d1_ppt" = "pink", "sdl_ppt" = "red"))

######################
## Precip infilling ##
######################

# ID missing dates in sdl precip dataset
pcp_missing_date <- sdl_chartpcp$date[is.na(sdl_chartpcp$ppt_tot)] #& year(sdl_chartpcp$date)>2014]
# count of missing ppt dates by year
sapply(split(pcp_missing_date, year(pcp_missing_date)), length) # there are missing ppt vals in every year..

# quick check that can merge Jennings et al. precip total with chart data 
# NWT stopped infilling chart precip data after 2008
# Keith and others infilled C1, D1 and Saddle values for 1990 - 2013

Jennings_summarized[c("LTER_site", "local_site", "date", "ppt_tot")] %>%
  dplyr::rename(Jennings_ppt = ppt_tot) %>%
  full_join(d1_chartpcp[c("LTER_site", "local_site", "date", "ppt_tot")]) %>%
  dplyr::rename(d1_ppt = ppt_tot) %>%
  ggplot() +
  geom_point(aes(date, d1_ppt), col = "blue", alpha=0.5) +
  geom_point(aes(date, Jennings_ppt), col = "chocolate1", alpha = 0.5) +
  theme_bw()

Jennings_summarized[c("LTER_site", "local_site", "date", "ppt_tot")] %>%
  dplyr::rename(Jennings_ppt = ppt_tot) %>%
  full_join(d1_chartpcp[c("LTER_site", "local_site", "date", "ppt_tot")]) %>%
  dplyr::rename(d1_ppt = ppt_tot) %>%
  ggplot() +
  geom_point(aes(Jennings_ppt, d1_ppt), col = "blue", alpha=0.5) +
  theme_bw()

## > going to use Jennings et al. 1990-2013 (last date available) then add in D1 chart data for more recent years

# make clean (complete) d1 dataset to calculate standard deviations
d1_Jchart_pcp <- Jennings_summarized[c("LTER_site", "local_site", "date", "ppt_tot")] %>%
  filter(local_site == "d1") %>%
  mutate(qdays = 1) %>%
  rbind(d1_chartpcp[year(d1_chartpcp$date)>2013, c("LTER_site", "local_site", "date", "ppt_tot", "qdays")]) %>%
  #dplyr::select(d1_chartpcp, -flag_ppt_tot) %>%
  mutate(ppt_tot_clean = ifelse(qdays == 1, ppt_tot, NA))

plot(d1_Jchart_pcp$date, d1_Jchart_pcp$ppt_tot_clean) # 2014 and on is raw chart data, qdays == 1

# make clean (complete) sdl dataset to calculate standard deviations
sdl_Jchart_pcp <- Jennings_summarized[c("LTER_site", "local_site", "date", "ppt_tot")] %>%
  filter(local_site == "sdl") %>%
  rbind(NSF_precip[year(NSF_precip$date)==2014,]) %>%
  mutate(qdays = 1) %>%
  rbind(sdl_chartpcp[year(sdl_chartpcp$date)>2014, c("LTER_site", "local_site", "date", "ppt_tot", "qdays")]) %>%
  #dplyr::select(sdl_chartpcp, -flag_ppt_tot) %>%
  mutate(ppt_tot_clean = ifelse(qdays == 1, ppt_tot, NA))

# correct sdl_chartpcp values for blowing snow (multiply months Sep - May by 0.39) [Sep to May following Emily's method]
sdl_Jchart_pcp <- sdl_Jchart_pcp %>%
  mutate(ppt_tot_clean_crt = ifelse(year(`date`)>2014 & month(`date`) %in% c(1:5, 9:12),
           #ifelse(month(`date`) %in% c(1:5, 9:12),
                                 ppt_tot_clean * 0.39, ppt_tot_clean))

plot(sdl_Jchart_pcp$date, sdl_Jchart_pcp$ppt_tot_clean) # 2015 and on is raw chart data, qdays == 1
plot(sdl_Jchart_pcp$date, sdl_Jchart_pcp$ppt_tot_clean_crt) # snow-corrected looks better

ggplot(sdl_Jchart_pcp) +
  geom_point(aes(date, ppt_tot_clean_crt, 
                 col = month(`date`) %in% c(6:8)), # is data from summer or not
             alpha = 0.5) +
  labs(title = paste0("Daily precip at saddle, 1990-", max(year(sdl_Jchart_pcp$date)), ", where qdays = 1 (corrected for blowing snow in non-summer months)"),
       subtitle = "Precip data 1990-2013 summarized to daily values from Jennings et al. 2018 daily infilled saddle data",
       y = "ppt (mm)") +
  theme_bw()
  

## > daily values must exist for BOTH sites in ratio calculations

d1_pcp_infill_dat <- left_join(d1_Jchart_pcp, sdl_Jchart_pcp, by = c("LTER_site", "date")) %>%
  mutate(yr = year(date),
         mon = month(date),
         doy = yday(date),
         day = day(date)) %>%
  filter(date >= min(sdl_chartpcp$date)) %>% # limit range to range of sdl chart data
  dplyr::select(LTER_site, date, yr, mon, day, doy, ppt_tot_clean.x, ppt_tot_clean_crt) %>%
  dplyr::rename(d1_ppt = ppt_tot_clean.x,
                sdl_ppt = ppt_tot_clean_crt) %>%
  na.omit() %>% # only keep dates where values for d1 and sdl both present
  # group_by(LTER_site, mon, day) %>%
  # summarise(d1_ppt_tot = sum(d1_ppt),
  #           sdl_ppt_tot = sum(sdl_ppt),
  #           nobs = length(sdl_ppt),
  #           sdl_d1_ratio = sdl_ppt_tot / d1_ppt_tot) %>%
  # ungroup() %>%
  group_by(LTER_site, mon) %>% 
  summarise(d1_ppt_monthly = sum(d1_ppt),
         sdl_ppt_monthly = sum(sdl_ppt),
         sdl_d1_monthly_ratio = sdl_ppt_monthly/d1_ppt_monthly)

# plot to inspect
# d1_pcp_infill_dat %>%
#   ungroup() %>%
#   mutate(doy = seq(from=1,to=length(sdl_d1_ratio), by=1)) %>%
#   ggplot(aes(doy, sdl_d1_ratio)) +
#   geom_line() +
#   geom_point() +
#   geom_smooth()
# 
# d1_pcp_infill_dat %>%
#   ungroup() %>%
#   mutate(doy = seq(from=1,to=length(sdl_d1_ratio), by=1)) %>%
#   gather(metric, value, d1_ppt_tot:sdl_d1_ratio) %>%
#   ggplot(aes(doy, value)) +
#   #geom_hline(aes(yintercept = 100), col ="red") +
#   geom_point() +
#   geom_line() +
#   geom_smooth() +
#   facet_grid(metric~., scales = "free_y")
  
plot(d1_pcp_infill_dat$mon, d1_pcp_infill_dat$sdl_d1_monthly_ratio) # not sure if some artifact effect going on..

###########################################
### Infill: method 1 using sdl:d1 ratios ##
###########################################

# start with joined, cleaned d1 and sdl chart precip (missing values included)

sdl_infilled_byd1 <- left_join(d1_Jchart_pcp, sdl_Jchart_pcp, by = c("LTER_site", "date")) %>%
  mutate(yr = year(date),
         mon = month(date),
         doy = yday(date),
         day = day(date)) %>%
  #filter(yr < 2014) %>% # to QA adding in raw data, doesn't change values in major way
  dplyr::select(LTER_site, date, yr, mon, day, doy, ppt_tot_clean.x, ppt_tot_clean_crt) %>%
  dplyr::rename(d1_ppt = ppt_tot_clean.x,
                sdl_ppt = ppt_tot_clean_crt) %>%
  left_join(d1_pcp_infill_dat[c("LTER_site", "mon", "sdl_d1_monthly_ratio")], by = c("LTER_site", "mon")) %>%
  mutate(sdl_infill = ifelse(is.na(sdl_ppt), round(d1_ppt * sdl_d1_monthly_ratio, 2), sdl_ppt))

plot(sdl_infilled_byd1$date, sdl_infilled_byd1$sdl_infill)


############################### 
## Precip infilling using C1 ##


# make clean (complete) c1 dataset for calculating ratios
c1_Jchart_pcp <- Jennings_summarized[c("LTER_site", "local_site", "date", "ppt_tot")] %>%
  filter(local_site == "c1") %>%
  mutate(qdays = 1) %>%
  rbind(c1_chartpcp[year(c1_chartpcp$date)>2013, c("LTER_site", "local_site", "date", "ppt_tot", "qdays")]) %>%
  # dplyr::select(c1_chartpcp, -flag_ppt_tot) %>%
  mutate(ppt_tot_clean = ifelse(qdays == 1, ppt_tot, NA))

plot(c1_Jchart_pcp$date, c1_Jchart_pcp$ppt_tot_clean) # all raw chart data, qdays == 1
# later years look a little higher in precip.. (artifact?)
# boxplot by year to check: do added years have greater precip?
boxplot(c1_Jchart_pcp$ppt_tot_clean~year(c1_Jchart_pcp$date)) # doesn't look like it
# note the 2013 flood outlier! 


## > daily values must exist for BOTH sites in ratio calculations

c1_pcp_infill_dat <- left_join(c1_Jchart_pcp, sdl_Jchart_pcp, by = c("LTER_site", "date")) %>%
  mutate(yr = year(date),
         mon = month(date),
         doy = yday(date),
         day = day(date)) %>%
  filter(date >= min(sdl_chartpcp$date)) %>% # earlier decade data look funny, so not including in ratio calculation
  dplyr::select(LTER_site, date, yr, mon, day, doy, ppt_tot_clean.x, ppt_tot_clean_crt) %>%
  dplyr::rename(c1_ppt = ppt_tot_clean.x,
                sdl_ppt = ppt_tot_clean_crt) %>%
  na.omit() %>% # only keep dates where values for c1 and sdl both present
  # group_by(LTER_site, mon, day) %>%
  # summarise(c1_ppt_tot = sum(c1_ppt),
  #           sdl_ppt_tot = sum(sdl_ppt),
  #           nobs = length(sdl_ppt),
  #           sdl_c1_ratio = sdl_ppt_tot / c1_ppt_tot) %>%
  # ungroup() %>%
  group_by(LTER_site, mon) %>% 
  summarise(c1_ppt_monthly = sum(c1_ppt),
            sdl_ppt_monthly = sum(sdl_ppt),
            sdl_c1_monthly_ratio = sdl_ppt_monthly/c1_ppt_monthly)

# plot to inspect
# c1_pcp_infill_dat %>%
#   ungroup() %>%
#   mutate(doy = seq(from=1,to=length(sdl_c1_ratio), by=1)) %>%
#   ggplot(aes(doy, sdl_c1_ratio)) +
#   geom_line() +
#   geom_point() +
#   geom_smooth()
# 
# c1_pcp_infill_dat %>%
#   ungroup() %>%
#   mutate(doy = seq(from=1,to=length(sdl_c1_ratio), by=1)) %>%
#   gather(metric, value, c1_ppt_tot:sdl_c1_ratio) %>%
#   ggplot(aes(doy, value)) +
#   #geom_hline(aes(yintercept = 100), col ="red") +
#   geom_point() +
#   geom_line() +
#   geom_smooth() +
#   facet_grid(metric~., scales = "free_y")

plot(c1_pcp_infill_dat$mon, c1_pcp_infill_dat$sdl_c1_monthly_ratio) # same seasonal patterns but in reverse..

###########################################
### Infill: method 1 using sdl:c1 ratios ##
###########################################

# start with joined, cleaned c1 and sdl chart precip (missing values included)

sdl_infilled_byc1 <- left_join(c1_Jchart_pcp, sdl_Jchart_pcp, by = c("LTER_site", "date")) %>%
  mutate(yr = year(date),
         mon = month(date),
         doy = yday(date),
         day = day(date)) %>%
  #filter(yr < 2014) %>% # to QA adding in raw data, doesn't change values in major way
  dplyr::select(LTER_site, date, yr, mon, day, doy, ppt_tot_clean.x, ppt_tot_clean_crt) %>%
  dplyr::rename(c1_ppt = ppt_tot_clean.x,
                sdl_ppt = ppt_tot_clean_crt) %>%
  left_join(c1_pcp_infill_dat[c("LTER_site", "mon", "sdl_c1_monthly_ratio")], by = c("LTER_site", "mon")) %>%
  mutate(sdl_infill = ifelse(is.na(sdl_ppt), round(c1_ppt * sdl_c1_monthly_ratio, 2), sdl_ppt))

plot(sdl_infilled_byc1$date, sdl_infilled_byc1$sdl_infill)

#################
### Quick QA ##

# compare c1 and d1 infilled
par(mfrow=c(2,1))
plot(sdl_infilled_byd1$date[sdl_infilled_byd1$yr>2014], sdl_infilled_byd1$sdl_infill[sdl_infilled_byd1$yr>2014])
plot(sdl_infilled_byc1$date[sdl_infilled_byd1$yr>2014], sdl_infilled_byc1$sdl_infill[sdl_infilled_byd1$yr>2014])
par(mfrow = c(1,1))

# merge infilled_by_d1 and infilled_by_c1 for 2015-2017 to compare
compare_ppt_infill <- sdl_infilled_byd1 %>%
  dplyr::rename(d1_sdl_infill = sdl_infill) %>%
  left_join(sdl_infilled_byc1) %>%
  dplyr::rename(c1_sdl_infill = sdl_infill) %>%
  filter(yr > 2008) %>%
  mutate(delta_d1_c1 = d1_sdl_infill - c1_sdl_infill)

range(compare_ppt_infill$delta_d1_c1, na.rm=T)
boxplot(compare_ppt_infill$delta_d1_c1, ylab = "D1 infill - C1 infill") #C1 overestimates more than D1, not surprising  
plot(compare_ppt_infill$delta_d1_c1 ~ compare_ppt_infill$mon, 
        ylab = "D1 ppt infill - C1 ppt infill (mm)",
        xlab = "month")
abline(a=0, b=0, col = "grey")

sdl_ppt_infill_20152017 <- compare_ppt_infill %>%
  filter(date >= min(sdl_chartpcp$date)) %>%
  mutate(ppt_tot = ifelse(!is.na(d1_sdl_infill), d1_sdl_infill, c1_sdl_infill),
         source = ifelse(!is.na(sdl_ppt), "sdl chart", 
                         ifelse(!is.na(d1_sdl_infill), "d1 infill", "c1 infill")),
         local_site = "sdl") %>%
  dplyr::select(LTER_site, local_site, date, yr, mon,day,ppt_tot, source)

summary(sdl_ppt_infill_20152017) # no NAs in precip!! >>lucky<<

# see how it looks
sdl_ppt_infill_20152017 %>%
  mutate(doy = yday(date)) %>%
ggplot(aes(doy, ppt_tot)) +
  geom_line(col = "grey50") +
  geom_point(aes(col = source), alpha=0.5) +
  labs(title = paste0("NWT LTER: Saddle daily precipitation",substr(min(sdl_ppt_infill_20152017$date),1,4), "-", substr(max(sdl_ppt_infill_20152017$date),1,4), ", colored by data source"),
       subtitle = "Sources: CTW QA'd Saddle chart data (excluded qdays > 1), or infilling via ratio method from D1 or C1",
       y = "Total daily ppt (mm)",
       x = "Day of year") +
  theme_bw() +
  facet_grid(yr~.) # seems reasonable .. nothing funny sticks out .. lots of infill for 2017 ..


## ---------------------------------------  
## combine all years, all data for final infilled data set
# NSF proposal data + ctw infilled 2015-current data

sdl_ppt_infill_1982current <- NSF_precip %>%
  mutate(yr = year(date),
         mon = month(date),
         `day` = day(date),
         source = "NSF proposal data") %>%
  dplyr::select(LTER_site, local_site, `date`, yr, mon, `day`, ppt_tot, source) %>%
  rbind(subset(sdl_ppt_infill_20152017, year(date)>=2015)) %>%
  filter(!year(date) == 2019) #2019 not complete yet

summary(sdl_ppt_infill_1982current) # no NAs, complete
write_csv(sdl_ppt_infill_1982current, 
          paste0(datpath, "output_data/prep_data/sdl_ppt_infill_19822018_nsfctw.csv"))

# plot to see how it looks all together
ggplot(sdl_ppt_infill_1982current, aes(date, ppt_tot, col =source)) +
  geom_point(alpha = 0.5) +
  labs(title = paste0("NWT LTER: Saddle daily precipition, 1982-", substr(max(sdl_ppt_infill_20152017$date),1,4), ", colored by data source"),
       subtitle = "Sources: Infilling from C1 or D1 chart done using the ratio method in the saddle ppt chart metadata",
       y = "Daily precipitation (mm)",
       x = "Date") +
  theme_bw()
