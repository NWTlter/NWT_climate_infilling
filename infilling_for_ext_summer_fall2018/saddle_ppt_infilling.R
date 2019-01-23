# infilling for saddle precip 2015-2017
# ctw sep 2018 (contact caitlin.t.white@colorado.edu with questions)
# prepared for extended summer update for K. Suding/LTER ASM 2018


# hierarchy of infill methods for precip
# 1) method 1 (ratio method) using D1
# 2) method 1 (ratio method) using C1

# some rules:
# use same precip data used in NSF proposal for years 1982 - 2014
# add in chart data for 2015-2017, NAs infilled by CTW using ratio method from NWT metadata
# for 2015-2017, only keep raw chart data where qdays = 1; qdays > 1 --> NA (infill)
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



# ------------------
# set working directory to ctw climate infilling subfolder on NWT_climate_infilling github repo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load needed libraries
library(tidyverse)
library(lubridate)

# data used in NSF proposal
# Hope Humphries sent the infilled saddle data to Emily Farrer, CTW doesn't know how infilled
# assume infilled data sent to HH from Tim Kittel, and methodology likely similar to that used in Jennings, Molotch, and Kittel 2018
NSF_precip <- read_csv("./Saddle_precip_temp_formoisturedeficit.csv") %>%
  mutate(`date` = as.Date(paste(Year,Month,Day, sep="-")),
         LTER_site = "NWT",
         local_site = "sdl") %>%
  dplyr::select(LTER_site, local_site, date, PCP) %>%
  dplyr::rename(ppt_tot = PCP) 
## NOTE: Emily's data has already been corrected for snow blowing Sep-May (multiply chart value * 0.39)
## NOTE2: If revise this analysis, correction should only be applied Oct-May from what CTW has read, but will follow what Emily did

# Jennings, Molotch, and Kittel (2018) infilled data for sdl, d1 and c1
# Keith Jennings et al. infilled *hourly* data
Jennings_infill <- read_csv("http://niwot.colorado.edu/data_csvs/infillclimate_c1_d1_sdl.hourly.kj.data.csv")

# summarise Jennings et al. data to daily totals (ppt), or max, min, or mean (temp)
Jennings_summarized <- Jennings_infill %>%
  group_by(LTER_site, local_site, year, date, jday) %>%
  summarise(ppt_tot = sum(ppt_tot),
            airtemp_max = mean(airtemp_avg),
            airtemp_min = min(airtemp_avg),
            airtemp_avg = mean(airtemp_avg))

######################
## Precip infilling ##
######################

# read in datasets
# read in precip chart data from EDI
# > read in from EDI data portal
sdl_chartpcp <- read_csv("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.416.8&entityid=c1c73287f2e10039c0d89dba8fd44993",
                         trim_ws = TRUE,
                         na = c("", "NA", "NaN"))

# chart data from NWT data portal
c1_chartpcp <- read_csv("http://niwot.colorado.edu/data_csvs/c-1pdayv.ml.data.csv",
                        na = c("", "NA", "NaN"))

d1_chartpcp <- read_csv("http://niwot.colorado.edu/data_csvs/d-1pdayv.ml.data.csv",
                        na = c("", "NA", "NaN"))
# correct d1 chart precip ppt_tot name to match others
names(d1_chartpcp)[4] <- "ppt_tot"

# ID missing dates in 2015-2017
pcp_missing_date <- sdl_chartpcp$date[is.na(sdl_chartpcp$ppt_tot) & year(sdl_chartpcp$date)>2014]


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
  #geom_point(aes(date, Jennings_ppt), col = "chocolate1", alpha = 0.5) +
  theme_bw()

## > going to use Jennings et al. 1990-2013 (last date available) then add in D1 chart data for more recent years

# make clean (complete) d1 dataset to calculate standard deviations
d1_Jchart_pcp <- Jennings_summarized[c("LTER_site", "local_site", "date", "ppt_tot")] %>%
  filter(local_site == "d1") %>%
  mutate(qdays = 1) %>%
  rbind(d1_chartpcp[year(d1_chartpcp$date)>2013, c("LTER_site", "local_site", "date", "ppt_tot", "qdays")]) %>%
  mutate(ppt_tot_clean = ifelse(qdays == 1, ppt_tot, NA))

plot(d1_Jchart_pcp$date, d1_Jchart_pcp$ppt_tot_clean) # 2014 and on is raw chart data, qdays == 1

# make clean (complete) sdl dataset to calculate standard deviations
sdl_Jchart_pcp <- Jennings_summarized[c("LTER_site", "local_site", "date", "ppt_tot")] %>%
  filter(local_site == "sdl") %>%
  rbind(NSF_precip[year(NSF_precip$date)==2014,]) %>%
  mutate(qdays = 1) %>%
  rbind(sdl_chartpcp[year(sdl_chartpcp$date)>2014, c("LTER_site", "local_site", "date", "ppt_tot", "qdays")]) %>%
  mutate(ppt_tot_clean = ifelse(qdays == 1, ppt_tot, NA))

# correct sdl_chartpcp values for blowing snow (multiply months Sep - May by 0.39) [Sep to May following Emily's method]
sdl_Jchart_pcp <- sdl_Jchart_pcp %>%
  mutate(ppt_tot_clean_crt = ifelse(year(`date`)>2014 & month(`date`) %in% c(1:5, 9:12),
                                 ppt_tot_clean * 0.39, ppt_tot_clean))

plot(sdl_Jchart_pcp$date, sdl_Jchart_pcp$ppt_tot_clean) # 2015 and on is raw chart data, qdays == 1
plot(sdl_Jchart_pcp$date, sdl_Jchart_pcp$ppt_tot_clean_crt) # snow-corrected looks better

ggplot(sdl_Jchart_pcp) +
  geom_point(aes(date, ppt_tot_clean_crt, 
                 col = month(`date`) %in% c(6:8)), # is data from summer or not
             alpha = 0.5) +
  labs(title = "Daily precip at saddle, 1990-2017 where qdays = 1 (corrected for blowing snow in non-summer months)",
       subtitle = "Precip data 1990-2013 summarized to daily values from Jennings et al. 2018 daily infilled saddle data",
       y = "ppt (mm)") +
  theme_bw()
  

## > daily values must exist for BOTH sites in ratio calculations

d1_pcp_infill_dat <- left_join(d1_Jchart_pcp, sdl_Jchart_pcp, by = c("LTER_site", "date")) %>%
  mutate(yr = year(date),
         mon = month(date),
         doy = yday(date),
         day = day(date)) %>%
  #filter(yr < 2014) %>% # to QA adding in raw data, doesn't change values in major way
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
  mutate(ppt_tot_clean = ifelse(qdays == 1, ppt_tot, NA))

plot(c1_Jchart_pcp$date, c1_Jchart_pcp$ppt_tot_clean) # 2014 and on is raw chart data, qdays == 1
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
  #filter(yr < 2014) %>% # to QA adding in NSF and raw data, doesn't change values in major way
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
  filter(yr > 2014) %>%
  mutate(delta_d1_c1 = d1_sdl_infill - c1_sdl_infill)

range(compare_ppt_infill$delta_d1_c1, na.rm=T)
boxplot(compare_ppt_infill$delta_d1_c1, ylab = "D1 infill - C1 infill") #C1 overestimates more than D1, not surprising  
plot(compare_ppt_infill$delta_d1_c1 ~ compare_ppt_infill$mon, 
        ylab = "D1 ppt infill - C1 ppt infill (mm)",
        xlab = "month")
abline(a=0, b=0, col = "grey")

sdl_ppt_infill_20152017 <- compare_ppt_infill %>%
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
  labs(title = "NWT LTER: Saddle daily precipitation 2015-2017, colored by data source",
       subtitle = "Sources: CTW QA'd Saddle chart data (excluded qdays > 1), or infilling via ratio method from D1 or C1",
       y = "Total daily ppt (mm)",
       x = "Day of year") +
  theme_bw() +
  facet_grid(yr~.) # seems reasonable .. nothing funny sticks out .. lots of infill for 2017 ..

#write.csv(sdl_ppt_infill_20152017, "./sdl_ppt_infill_20152017_ctw.csv")

## ---------------------------------------  
## combine all years, all data for final infilled data set
# NSF proposal data + ctw infilled 2015-2017 data

sdl_ppt_infill_19822017 <- NSF_precip %>%
  mutate(yr = year(date),
         mon = month(date),
         `day` = day(date),
         source = "NSF proposal data") %>%
  dplyr::select(LTER_site, local_site, `date`, yr, mon, `day`, ppt_tot, source) %>%
  rbind(sdl_ppt_infill_20152017)

summary(sdl_ppt_infill_19822017) # no NAs, complete
#write.csv(sdl_ppt_infill_19822017, "./sdl_ppt_infill_19822017_ctw.csv")

# plot to see how it looks all together
ggplot(sdl_ppt_infill_19822017, aes(date, ppt_tot, col =source)) +
  geom_point(alpha = 0.5) +
  labs(title = "NWT LTER: Saddle daily precipition, 1982-2017, colored by data source",
       subtitle = "Sources: Infilling from C1 or D1 chart done using the ratio method in the saddle ppt chart metadata",
       y = "Daily precipitation (mm)",
       x = "Date") +
  theme_bw()
