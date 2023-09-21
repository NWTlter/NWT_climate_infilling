#stitch saddle temp datasets (patch until full workflow done)
# author(s): CTW, caitlin.t.white@colorado.edu

# script purpose: make all years temp dataset for saddle
# 1. read in relevant temp datasets:
# > qa'd, infilled saddle temp dataset from nwt site visit 2018
# > currently saddle logger dataset on EDI
# 2. QA check most recent years of saddle logger data
# > also run same-site QA functions on data through 2018 for QA comparison (same-site functions not written then)
# 3. infill any 2019 and 2020 values as needed
# > will need to read in other temp datasets and QA those first if so (CTW does not anticipate needing)
# 4. (optional) backward project logger data to chart-only period (if find better method)
# 5. append recent yrs temp data to ongoing dataset, write out to data/output for renewal use


# notes:


# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN")

# set path to climdat folder
datpath <- "c1_d1_sdl_clim/homogenize_climdat/"
# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")
# functions to flag potentially errant values
source(paste0(datpath,"scripts/flagging_functions.R"))


# -- GET DATA -----
# current EDI logger data
sdl_logger_edi <- getTabular(405)
d1_logger_edi <- getTabular(402)

# frankenstein'd sdl logger-chart dataset for 2019 site review (data through 12/31/2018)
# note: used cr21x for extended summer analysis bc data collection nearer in time to 1980s chart data, projected values more similar
sdl_frankentemp_2018 <- read.csv("../long-term-trends/extended_summer/analysis/output_data/prep_data/crall21xhcn_ctw.csv", na.strings = na_vals, strip.white = T)
# just in case -- chart predicted from cr21x (nearest to 1980s chart data) and cr1000 (most recent)
# > this was used to make sdl_frankentemp (which went into extended summer)
sdl_predicted_2018 <- read.csv("../long-term-trends/extended_summer/analysis/output_data/prep_data/predict_sdl_cralltemp_1982-ongoing.csv", na.strings = na_vals, strip.white = T)
# sdl chart data qa'd (but not infilled) for 2018 site review
# > think we decided not to infill because katie wanted to use the logger data over the chart data, so no need
sdl_chart_qa2018 <- read.csv("../long-term-trends/extended_summer/analysis/output_data/prep_data/qa_sdlchart_temp.csv", na.strings = na_vals, strip.white = T)

# temp sdl frankenstein dataset sarah made adjusted for hmp through 2019 (missing 2020 data)
sdl_frankentemp_sce <- read.csv(paste0(datpath, "data/prep_data/crall21xhcn_ctw_2020.csv"), na.strings = na_vals, strip.white = T)

# d1 +c1 chart for last few years
d1 <- getTabular(412) %>% data.frame()
c1 <- getTabular(411) %>% data.frame()

# check how dats read in
str(sdl_frankentemp_2018) # data from 1981-01-01 to 1981-07-08 only there so AET function ran for the extended summer analysis
str(sdl_predicted_2018)
str(sdl_chart_qa2018)
str(sdl_frankentemp_sce)
str(d1)
str(c1)
# all date cols are character

# also read in C1 ameriflux for quick, reliable reference
# > for now, this is coming from CTW's hard drive
ameriforest <- read_csv("../../Documents/nwt_lter/aux_climdats/AmeriFlux/forest_prepped2compare.csv", na = na_vals)

# jennings et al. qa'd and infilled hrly dataset
# > ctw looked at this in script 01.. not so sure about QA in final years, but 1990-2014 can be used as reference
# > also bringing in to quickly grab and add mean temp for sdl
jennings <- read_csv(paste0(datpath, "data/prep_data/ready_for_qa/jennings_hrly2daily_tidy.csv"))


# -- QA RECENT SDL LOGGER DAT -----
# also test qa functions for same-site flags to see how they compare to comparative site flag functions from site visit

# winnow logger dat to airtemp only
sdl_logger_temp <- sdl_logger_edi[grep("^LTER|^local|logger|date|year|jday|airtemp", names(sdl_logger_edi))] %>%
  gather(met, val, min(grep("airtemp", names(.))):ncol(.)) %>%
  mutate(type = ifelse(grepl("^flag", met), "flag", "airtemp"),
         met = gsub("flag_|airtemp_", "", met)) %>%
  spread(type, val) %>%
  mutate(instrument = ifelse(grepl("hmp", met), str_extract(met, "hmp[0-9]"), "logger"),
         met = gsub("hmp[0-9]_", "", met),
         airtemp = as.numeric(airtemp),
         met = factor(met, levels = c("max", "avg", "min"))) %>%
  # drop rows that are hmp + cr23x because hmps only active since cr1000
  subset(!(logger == "cr23x" & grepl("hmp", instrument))) %>%
  # drop hmps for cr1000 pre-2018
  subset(!(year < 2018 & grepl("hmp", instrument))) %>%
  #check that there is at least 1 obs per met per day
  group_by(date, met) %>%
  mutate(nobs = sum(!is.na(airtemp))) %>%
  ungroup()
str(sdl_logger_temp)

# same site flagging

# all dates expected present?
check_datetime(sdl_logger_temp) 
# temp plausibility checks at Saddle
checklimits <- flag_limits(sdl_logger_temp, maxcol = "airtemp", maxval = 30, minval = -35)
# flag deviations for review
# group by logger, instrument so only comparing vals to itself

# set grouping vars
grp1 <- c("logger", "instrument", "met") # for global
grp2 <- c(grp1, "mon") # by month
# flag values outside absolute or SD threshold deviations
devs_all <- flag_deviations(sdl_logger_temp, metric = "airtemp", groupvars = grp1, ranks_num = 3, sd_num = 4)
devs_monthly <- flag_deviations(sdl_logger_temp, metric = "airtemp", groupvars = grp2, ranks_num = 3, sd_num = 4)
# check day to day spikes
spikecheck <- flag_spikes(sdl_logger_temp, metric = "airtemp", groupvars = grp1, abs_limit = 30)
spikecheck_monthly <- flag_spikes(mutate(sdl_logger_temp, mon = month(date)), metric = "airtemp", groupvars = grp2, abs_limit = 30)
spikecheck_monthly_all <- flag_spikes(mutate(sdl_logger_temp, mon = month(date)), metric = "airtemp", groupvars = c("mon", "met"), abs_limit = 30)
spikecheck_monthly_working <- flag_spikes(mutate(sdl_logger_temp, mon = month(date)), metric = "airtemp", groupvars = grp2, abs_limit = 30, returnworking = T)
# check flatlines
flatcheck <- flag_flatlines(sdl_logger_temp, metric = "airtemp", groupvars = grp1, numdays = 4)

# plot for review
baseplot <- ggplot(sdl_logger_temp, aes(date, airtemp, col = met)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~instrument+logger, scales = "free_x")

# checklimits
baseplot + 
  geom_point(data = subset(checklimits, !is.na(qaflag_airtemp)), aes(date, airtemp), pch = 1, size = 3)
# deviations
## global
baseplot + 
  geom_point(data = subset(devs_all, !is.na(flag_grpstd_airtemp)), aes(date, airtemp), pch = 2, size = 3, col = "black")
## monthly
baseplot + 
  geom_label(data = subset(devs_monthly, !is.na(flag_grpstd_airtemp)), aes(date, airtemp, label = round(flag_grpstd_airtemp,2)), pch = 16, size = 3)
# spikes
baseplot + geom_point(data = subset(spikecheck, !is.na(airtemp_temporal_spike)), aes(date, airtemp), pch = 16, size = 3)
baseplot + geom_point(data = subset(spikecheck_monthly, !is.na(airtemp_temporal_spike)), aes(date, airtemp), pch = 16, size = 3)
baseplot + geom_point(data = subset(spikecheck_monthly_all, !is.na(airtemp_temporal_spike)), aes(date, airtemp), pch = 16, size = 3)

# try binding all QA checks, then tally total flags per met per date
#allchecks <- cbind()



# -- hmp only -----
hmptemp <- subset(sdl_logger_temp, grepl("hmp", instrument) & year > 2017)

ggplot(hmptemp, aes(date, airtemp, group = instrument)) +
  geom_line(aes(lty = instrument), alpha = 0.5) +
  geom_point(aes(col = flag), alpha= 0.5) +
  scale_color_manual(values = c("grey80", "goldenrod1", "dodgerblue")) +
  facet_wrap(~met+instrument)

q_dates <- unique(hmptemp$date[hmptemp$flag == "q"])
ggplot(subset(hmptemp, date %in% q_dates), aes(date, airtemp, group = instrument)) +
  #geom_line(aes(lty = instrument), alpha = 0.5) +
  geom_point(aes(col = flag, shape = instrument), size = 3, alpha= 0.5) +
  scale_color_manual(values = c("grey50", "goldenrod1", "dodgerblue")) +
  scale_y_continuous(breaks = seq(-10,20,2)) +
  facet_wrap(~met, scales = "free_y", nrow = 3)
# on most questionable days, q value is still relatively close
# consider (because for purposes of prosposal, high precision to decimal degree not needed), if within 0.5 keep, else take out of daily hmp average
# but within 0.5 of what? of each.. it should be based on the average difference/typical relationship between hmps

# make wide to diff hmps then run qa check
hmpwide <- subset(hmptemp, !is.na(airtemp), select = -c(nobs)) %>%
  gather(thing, val, airtemp, flag) %>%
  unite(what, instrument, thing) %>%
  spread(what, val) %>%
  mutate_at(.vars = grep("hmp[0-9]_airtemp$", names(.)), as.numeric) %>%
  mutate(diff1_2 = hmp1_airtemp - hmp2_airtemp,
         diff1_3 = hmp1_airtemp - hmp3_airtemp,
         diff2_3 = hmp2_airtemp - hmp3_airtemp,
         nobs = apply(.[grep("hmp[0-9]_airtemp$", names(.))], 1, function(x) sum(!is.na(x))))

with(hmpwide[grepl("met|diff", names(hmpwide))], lapply(split(hmpwide[grepl("diff", names(hmpwide))], met), summary))
par(mfrow = c(3,1))
hist(hmpwide$diff1_2)
hist(hmpwide$diff1_3)
hist(hmpwide$diff2_3)
par(mfrow = c(1,1))

dplyr::select(hmpwide, date, met, grep("diff", names(hmpwide))) %>%
  gather(diff, val, diff1_2:ncol(.)) %>%
  ggplot(aes(date, val, col = met)) +
  geom_hline(aes(yintercept = 0)) +
  #geom_point(alpha = 0.25) +
  geom_smooth(aes(fill = met), method = "lm") +
  scale_x_date(breaks = "2 month", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~diff, nrow = 3, scales = "free")

# is relationship steadier if exclude q dates?
dplyr::select(hmpwide, date, met, grep("diff", names(hmpwide))) %>%
  subset(!date %in% q_dates) %>%
  gather(diff, val, diff1_2:ncol(.)) %>%
  ggplot(aes(date, val, col = met)) +
  geom_hline(aes(yintercept = 0)) +
  #geom_point(alpha = 0.25) +
  #geom_smooth(aes(fill = met)) +
  geom_smooth(aes(fill = met), method = "lm") +
  scale_x_date(breaks = "2 month", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~diff, nrow = 3, scales = "free") # no, there is still drift overtime..

# run deviation flag on hmp diffs
hmpdevs <- dplyr::select(hmpwide, date, met, grep("diff", names(hmpwide))) %>%
  gather(diff, val, diff1_2:ncol(.)) %>%
  flag_deviations(., groupvars = c("diff"),metric = "val", sd_num = 4)

ggplot(hmpdevs, aes(date, val, col = met)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(alpha = 0.5) +
  geom_point(data = subset(hmpdevs, !is.na(flag_grpstd_val)), aes(date, val), size = 3, pch = 1) +
  facet_wrap(~diff)

# incorporate dataset flagging info
hmpdevs$edi_qflag <- NA
for(i in c(1:3)){
  q_dates_temp <- unique(hmpwide$date[hmpwide[[paste0("hmp", i, "_flag")]]=="q"])
  # remove NA
  q_dates_temp <- q_dates_temp[!is.na(q_dates_temp)]
  for(t in q_dates_temp){
    tag <- which(hmpdevs$date == t & grepl(i, hmpdevs$diff))
    hmpdevs$edi_qflag[tag] <- paste0("hmp", i)
  }
}
hmpdevs$flag_0.5 <- abs(hmpdevs$val) >= 0.5
# consideration.. with drift in differences over time.. in future, NWT data mgmt should think through SD or absval threshold triggers
# assume differences will grow.. and they differ seasonally.

ggplot(hmpdevs, aes(date, val)) +
  geom_point(data = subset(hmpdevs, !is.na(edi_qflag)), aes(date, val, shape = edi_qflag), col = "grey30", size = 3) +
  #geom_point(aes(col = !is.na(flag_grpstd_val)), alpha = 0.5) +
  geom_point(aes(col = !is.na(flag_std_val)), alpha = 0.5) +
  facet_wrap(~diff)

hmpspikes <- flag_spikes(hmptemp, metric = "airtemp", groupvars = c("met", "instrument"), returnworking = T)
View(subset(hmpspikes, date %in% with(hmpdevs, date[!is.na(flag_std_val)])))
# > for patch, just ignore the q'd values that also were flagged in the global deviations

# pull dates and hmps to ignore
ignore_hmps <- subset(hmpdevs, !is.na(flag_std_val) & !is.na(edi_qflag), select = c(date, edi_qflag)) %>%
  mutate(ignore = T) %>%
  distinct()

hmptemp_daily <- left_join(hmptemp, ignore_hmps, by = c("date", "instrument" = "edi_qflag")) %>%
  # crunch daily mean, se, and obs involved
  # drop any NAs in val and drop rows to igno
  filter(is.na(ignore) & !is.na(airtemp)) %>%
  #group_by() %>%
  grouped_df(names(.)[grep("LTER", names(.)):grep("met", names(.))]) %>%
  #grouped_df(names(.)[1:7]) %>%
  summarise(daily = mean(airtemp),
            se = sd(airtemp)/sqrt(length(airtemp)),
            nobs = length(airtemp),
            hmps = str_flatten(instrument, collapse = ","),
            .groups = "drop")
alldates <- data.frame(LTER_site = "NWT", local_site = hmptemp$local_site[1], logger = hmptemp$logger[1],
                       date = seq.Date(min(hmptemp_daily$date), max(hmptemp_daily$date), 1)) %>%
  mutate(year = year(date),
         jday = yday(date),
         max = NA, avg = NA, min = NA) %>%
  gather(met, val, max:min) %>%
  dplyr::select(-val)
alldates <- left_join(alldates, hmptemp_daily)                      

missing_dates <- unique(alldates$date[!alldates$date %in% hmptemp_daily$date])
min(missing_dates)-30

# see results
ggplot(hmptemp_daily, aes(date, daily)) +
  geom_point(aes(col = se, size = nobs), alpha = .4) +
  #geom_errorbar(aes(date, ymax = daily + se, ymin = daily - se)) +
  #geom_line(aes(date, daily - se), lty = 2) +
  #geom_path() +
  geom_smooth(col= "black", fill = "grey") +
  scale_color_viridis_c() +
  facet_wrap(~met, scales = "free")


# infill from d1 logger
d1_logger_temp <- dplyr::select(d1_logger_edi, LTER_site:jday, grep("airtemp_hmp", names(d1_logger_edi))) %>%
  subset(year == 2019 & date >= (min(missing_dates) - 30) & date <= (max(missing_dates)+30)) %>%
  gather(met, val, airtemp_hmp1_max:ncol(.)) %>%
  mutate(what = ifelse(grepl("^airtemp", met), "airtemp", "flag"),
         instrument = str_extract(met, "hmp[0-9]"),
         met = str_extract(met, "[a-z]{3}$")) %>%
  spread(what, val) %>%
  mutate(airtemp = as.numeric(airtemp))
# be sure no flags
unique(d1_logger_temp$flag) #none
str(d1_logger_temp)
d1_temp_daily <- d1_logger_temp %>%
  grouped_df(names(.)[grep("LTER", names(.)):grep("met", names(.))]) %>%
  summarise(d1_daily = mean(airtemp),
            d1_se = sd(airtemp)/sqrt(length(airtemp)),
            d1_nobs = length(airtemp),
            d1_hmps = str_flatten(instrument, collapse = ","),
            .groups = "drop") %>% rename(d1_local = local_site)
comparetemp <- merge(d1_temp_daily, alldates, all.x = T)  
cor.test(comparetemp$d1_daily, comparetemp$daily, na.rm = T)
infilllm <- lm(daily ~ d1_daily, data = subset(comparetemp, !is.na(daily)))
comparetemp <- cbind(comparetemp, data.frame(predict(infilllm, newdata = comparetemp, interval = "predict")))

plot_grid(ggplot(comparetemp, aes(date, daily - fit)) + 
            geom_hline(aes(yintercept = 0), col = "grey30") +
            geom_path() +
            scale_x_date(breaks = "1 week", date_labels = "%m-%d") +
            facet_wrap(~met, nrow = 1),
          ggplot(comparetemp, aes(date, d1_daily)) + 
            geom_hline(aes(yintercept = 0), col = "grey30") +
            geom_point() +
            geom_point(aes(date, daily), col ="pink") +
            geom_point(data = subset(comparetemp, is.na(daily)), aes(date, fit), col ="orchid") +
            scale_x_date(breaks = "1 week", date_labels = "%m-%d") +
            facet_wrap(~met, nrow = 1),
          ggplot(comparetemp, aes(date, daily-d1_daily)) + 
            geom_hline(aes(yintercept = 0), col = "grey30") +
            geom_point(col = "grey60") +
            scale_x_date(breaks = "1 week", date_labels = "%m-%d") +
            facet_wrap(~met, nrow = 1),
          nrow = 3, align = "vh"
)

alldates2 <- merge(alldates, comparetemp[c("date", "met", "fit", "lwr", "upr")], all.x = T) %>%
  mutate(airtemp = ifelse(!is.na(daily), daily, fit))
str(alldates2)
allyears_sdl <- sdl_frankentemp_2018[c("date", "TMIN", "TMAX")] %>%
  mutate(date = as.Date(date)) %>%
  subset(date >= as.Date("1981-07-09")) %>% # first date of saddle chart data 
  rename(min = TMIN, max = TMAX) %>%
  gather(met, airtemp, min, max) %>%
  rbind(subset(alldates2, met != "avg", select = names(.))) %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, met) %>%
  mutate(nobs = length(airtemp))
str(allyears_sdl)
ggplot(allyears_sdl, aes(date, airtemp, col = met)) + 
  geom_point(alpha = 0.2) +
  geom_line() +
  geom_smooth(method = "lm")

spread(allyears_sdl, met, airtemp) %>%
  ggplot(aes(date, max.min)) + 
  geom_point(alpha = 0.2) +
  geom_line() +
  geom_smooth(method = "lm")

## note for tomorrow: 2018 has double data.. i have no idea why. one is cr1000 logger dat, not sure what the other is (doesn't correspond to chart or mean of hmps..)


# -- QUICKSTITCH SDL DATA -----
# check to see if other qa flagged vals from site visit are still in SDL chart
checksdl <- subset(sdl_predicted_2018, select = c("logger", "date", "met", "sdl_temp" , "fit", "method", "qa_temp", "qa_flag")) %>%
  rename(cr_qa_temp = qa_temp, cr_qa_flag = qa_flag, crdat_sdl_temp = sdl_temp) %>%
  #mutate(date = as.Date(date)) %>%
  full_join(sdl_chart_qa2018) %>%
  # did I use raw sdl to project cr?
  mutate(checksdl = sdl_temp == crdat_sdl_temp) # yeeep. bleh.
# how many bad vals are in there?
nrow(subset(checksdl, !is.na(fit) & !is.na(qa_flag) & !grepl("cr1000", method))) # 27 vals.. 5 = max; 22 = min
View(subset(checksdl, !is.na(fit) & !is.na(qa_flag)  & !grepl("cr1000", method))) # 4 are flagged for removal, the rest are warnings for flatlines
# > also I don't know how 2nd highest 24C degree (1990-06-09) is in frankenstein data because there was a value for the logger and sdl chart that day around 21C.. unless it was an adjustment for logger differences over time?

# pull what should be the fitted values
checksdl_sitevisit <- subset(checksdl, !grepl("cr1000", method)) %>%
  mutate(finaltemp = ifelse(!is.na(cr_qa_temp), cr_qa_temp, fit),
         infilled = !is.na(fit),
         met = gsub("airtemp_", "", met),
         date = as.Date(date)) %>%
  dplyr::select(date, met, finaltemp, infilled, qa_flag, method, logger) %>%
  rename(sdl_qa_flag = qa_flag)

check_allyrs <- ungroup(allyears_sdl) %>%
  left_join(checksdl_sitevisit) %>%
  mutate(expected_val = round(airtemp,5) == round(finaltemp,5),
         yr = year(date), mon = month(date), doy = yday(date),
         final_method = ifelse(!infilled, logger, method))

#which are the values that shouldn't be in there, how do they influence the record?
subset(check_allyrs, !(yr==2018 & !expected_val)) %>%
  ggplot(aes(date, airtemp)) +
  geom_point(aes(col = final_method), alpha = 0.5) +
  geom_point(data = subset(check_allyrs, !(yr==2018 & !expected_val) & !is.na(sdl_qa_flag)), aes(date, airtemp, shape = sdl_qa_flag)) +
  #geom_smooth(aes(col = final_method, fill = final_method), method = "lm") +
  geom_smooth(method = "lm") +
  facet_wrap(~met, nrow = 2)

# remove 2018 vals that were not the expected val
check_allyrs <- subset(check_allyrs, !(yr==2018 & !expected_val))


# check diff between hmps and loggers in 2018
subset(sdl_logger_temp, year == 2018) %>%
  ggplot(aes(date, airtemp, col= instrument)) +
  geom_line() +
  facet_wrap(~met) # logger warmer than hmps across all mets..

# check against saddle (loggers are typically warmer in tmax for chart, and colder in tmin since loggers can capture microspikes.. maybe math mean would be good to compare against logger tmean)
# > there's no overlap between hmps and saddle chart :{ so look at diff between logger and chart, then logger and hmps
# > missing sdl data in 2017 so look at 2016 too
sdl_stack <-  subset(sdl_chart_qa2018, yr > 2010, select = c(date, sdl_qatemp, met)) %>%
  spread(met, sdl_qatemp) %>%
  mutate(avg = apply(.[c("airtemp_max", "airtemp_min")],1, mean)) %>%
  gather(met, airtemp, airtemp_max:ncol(.)) %>%
  mutate(instrument = "sdl_chart", met = gsub("airtemp_", "", met), date = as.Date(date)) %>% 
  rbind(subset(sdl_logger_temp, year %in% c(2011:2019), select = names(.)))

ggplot(sdl_stack, aes(date, airtemp, col= instrument)) +
  geom_vline(aes(xintercept = as.Date("2012-03-01")), lty = 2) + # new logger roughly starts here
  geom_line(alpha = 0.5) +
  facet_wrap(~met, nrow = 3) 

# look at diff between logger (continual source) vs. others
spread(sdl_stack, instrument, airtemp) %>%
  gather(nonlogger, val, hmp1:hmp3, sdl_chart) %>%
  mutate(diff = logger - val) %>%
  ggplot(aes(date, diff, col= nonlogger)) +
  geom_vline(aes(xintercept = as.Date("2012-12-05")), lty = 2) + # new logger roughly starts here
  geom_hline(aes(yintercept = 0)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~met) 

# closer look at hmp diffs
spread(sdl_stack, instrument, airtemp) %>%
  gather(nonlogger, val, hmp1:hmp3, sdl_chart) %>%
  mutate(diff = logger - val) %>%
  subset(!(grepl("hmp", nonlogger) & year(date)<2018)) %>%
  # remove sdl 2018 nwards too
  subset(!(grepl("sdl", nonlogger) & year(date)>= 2018)) %>%
  # remove logger 2019
  subset(year(date)< 2019) %>%
  mutate(nonlogger = factor(nonlogger, levels = c("sdl_chart", "hmp1", "hmp2", "hmp3"))) %>%
  ggplot(aes(date, diff, col= nonlogger)) +
  #geom_vline(aes(xintercept = as.Date("2012-12-05")), lty = 2) + # new logger roughly starts here
  geom_hline(aes(yintercept = 0)) +
  geom_line(alpha = 0.5) +
  labs(y = "logger - nonlogger") +
  facet_wrap(~met + nonlogger, scales = "free") 

# for reference, what is the diff in 2018 btwn cr1000 and hmps?
sdl2018 <- subset(sdl_stack, year(date)==2018) %>%
  spread(instrument, airtemp) %>%
  gather(hmp, val, hmp1:hmp3) %>%
  mutate(diff_cr1000hmp = logger - val) %>%
  group_by(met, hmp) %>%
  mutate(meandiff = mean(diff_cr1000hmp))

distinct(sdl2018[c("hmp", "met", "meandiff")])

# hmp   met   meandiff
# <chr> <chr>    <dbl>
# 1 hmp1  avg      1.14 
# 2 hmp1  max      1.84 
# 3 hmp1  min      0.933
# 4 hmp2  avg      1.05 
# 5 hmp2  max      1.76 
# 6 hmp2  min      0.826
# 7 hmp3  avg      1.22 
# 8 hmp3  max      1.89 
# 9 hmp3  min      1.02 

# what is the difference between cr23x and cr21x on 4 days of overlap in june 2000?
cr23transition <- subset(check_allyrs, !expected_val, select = c(date, logger, met, finaltemp)) %>%
  distinct() %>%
  spread(logger, finaltemp) %>%
  mutate(diff_21x23x = cr21x - cr23x) %>%
  group_by(met) %>%
  mutate(meandiff_21x23x = mean(diff_21x23x))

distinct(cr23transition[c("met", "meandiff_21x23x")])
# met   meandiff_21x23x
# <chr>           <dbl>
# 1 max            -1.12 
# 2 min            -0.951

# -- BREAKPOINTS ANALYSES ----- 
# test for breakpoints to see if any are artifact of stitching different sources overtime
# also testing which method seems best given different packages and algorithms available and requirements in each (e.g. apriori specified breakpoints)
# > following this as an example:
# > https://www.marinedatascience.co/blog/2019/09/28/comparison-of-change-point-detection-methods/

library(changepoint)
library(bcp)
library(strucchange)
library(tree)

# make temp time series objects
tmax_ts <- ts(check_allyrs$airtemp[check_allyrs$met == "max"], )
tmin_ts <- ts(check_allyrs$airtemp[check_allyrs$met == "min"])
plot(tmax_ts)

# 1. changepoint approach -----
# this method assumes only 1 breakpoint in mean
cpt.mean(tmax_ts) #1755
cpt.mean(tmin_ts) #4664
# what dates are the breakpoints? 
check_allyrs$date[check_allyrs$met == "max"][1755] # shoot. 1986 is the year switch from chart to logger.. but not until december
check_allyrs$date[check_allyrs$met == "max"][4644] # 1994 is interesting.. don't think connected with an instrument change

# does change in variance match up?
cpt.var(tmax_ts) # none
cpt.meanvar(tmax_ts) #1716.. close
cpt.var(tmin_ts) # none
cpt.meanvar(tmin_ts) #4683, also close

# test for multiple changepoints (up to a max)
cpt.mean(tmax_ts, method = "PELT", Q = 10, penalty = "CROPS", pen.value = c(1,25)) 
# > suggests a lot of changepoints, which is what the web example also concluded with their data


# 2. bcp approach -----
bcp_tmax <- bcp(tmax_ts, return.mcmc = T) 
plot(bcp_tmax)
bcpdat <- data.frame(airtemp = bcp_tmax$data[,2], postmean = bcp_tmax$posterior.mean[,1], date = check_allyrs$date[check_allyrs$met == "max"])
plot(bcpdat$postmean ~ bcpdat$date)
bcp_tmin <- bcp(tmin_ts, return.mcmc = T)
plot(bcp_tmin) # similar result.. seems for period of last logger (cr1000 through hmps) is fine..
# could try removing seasonal trend.. but will try other packages

# 3. strucchange approach -----
# can test for structural changes in means and variance; input must be class ts
bp_tmax <- breakpoints(tmax_ts[1:2000] ~ 1)
bp_tmax
plot(bp_tmax)
strucdat <- subset(check_allyrs, met == "max", select = c(date, airtemp))
strucdat$date[bp_tmax$breakpoints]
# > seems to be more of a seasonal difference when only 1000 data points (may, and either oct or nov)
# > with 2000 points, optimal breaks are at may, nov, apr, so still seasonal a bit..

# try removing seasonality
# https://semba-blog.netlify.app/02/20/2019/time-series-analysis-in-r/
# > start tmax at 1982-01-01 so units work out..
tmax_1982 <- subset(check_allyrs, date >= as.Date("1982-01-01") & met == "max") %>%
  replace_na(list(final_method = "hmps"))
tmax_ts1982 <- ts(tmax_1982$airtemp, start = c(1982,1), frequency = 365.24)
tmax_components <- decompose(tmax_ts1982)
plot(tmax_components) # hoo boy
tmax_1982$deseasoned <- tmax_1982$airtemp - tmax_components$seasonal
tmax_1982$trend <- tmax_components$trend

#find min dates of methods employed
mindates <- lapply(split(tmax_1982$date, tmax_1982$final_method), min)
mindates <- data.frame(as.Date(unlist(mindates)))
names(mindates)[1] <- "date" 
mindates$method <- rownames(mindates)
methodrle <- rle(tmax_1982$final_method)
ggplot(tmax_1982, aes(date, trend)) +
  geom_vline(data = mindates, aes(xintercept = date, col = method), lty = 2, lwd = 2, alpha = 0.5) +
  geom_line()
ggplot(tmax_1982, aes(date, deseasoned)) +
  geom_line(alpha = 0.5) +
  geom_vline(data = mindates, aes(xintercept = date, col = method), lty = 2, alpha = 0.7, lwd = 2)

# try bcp and strucchange again
bp_trend <- breakpoints(tmax_components$trend[1:5000] ~ 1)
plot(bp_trend)
tmax_1982$date[bp_trend$breakpoints] #why are there 2-yr trends? seems non-coincidental.. but none so far overlap with instrument changes
# "ENSO exhibits oscillatory behavior in the tropical Pacific ocean–atmosphere system. It is widely accepted that the occurrence frequency of ENSO peaks in approximately 3- to 5-year periods. 
# In addition, a secondary peak at approximately 2-year periodicity was noted by Rasmusson et al. (1990). The 2-year peak is referred to as the biennial oscillation component"
# https://link.springer.com/article/10.1007/s00382-013-1862-1

# in strucchange breakpoints, can specify min number observations per segment.. if set to 5 years to avoid ENSO cycles (?) and I am looking for 6 or so breakpoints if it's instrument related..
bp_trend_restric <- breakpoints(tmax_components$trend[1:5000] ~ 1, h = 1095, breaks = 2)
tmax_1982$date[bp_trend_restric$breakpoints]

bp_trend_restric2 <- breakpoints(tmax_components$trend[1:10000] ~ 1, h = (3*365), breaks = 3)
tmax_1982$date[bp_trend_restric$breakpoints]


# out of curiosity, what does kittel d1 look like for similar period?
d1_kittel <- getTabular(187) %>% data.frame()
d1k_tmax_1952 <- ts(d1_kittel$max_temp, start = c(1952,1), frequency = 365.24)
d1k_tmax_components <- decompose(d1k_tmax_1952)
plot(d1k_tmax_components)
# there is some periodicity, but it looks better.
# pull out comparable time period to saddle
d1_kittel_1982 <- subset(d1_kittel, year >= 1982)
d1k_tmax_1982 <- ts(d1_kittel_1982$max_temp, start = c(1982,1), frequency = 365.24)
d1k_tmax_components_1982 <- decompose(d1k_tmax_1982)
plot(d1k_tmax_components_1982)
d1_kittel_1982$d1_trend <- d1k_tmax_components_1982$trend
d1_kittel_1982$d1_deseasoned <- d1_kittel_1982$max_temp - d1k_tmax_components_1982$seasonal
d1_kittel_1982$date <- as.Date(d1_kittel_1982$date)

# same at c1?
c1_kittel <- getTabular(185) %>% data.frame()
# pull out comparable time period to saddle
c1_kittel_1982 <- subset(c1_kittel, year >= 1982)
c1k_tmax_1982 <- ts(c1_kittel_1982$max_temp, start = c(1982,1), frequency = 365.24)
c1k_tmax_components_1982 <- decompose(c1k_tmax_1982)
plot(c1k_tmax_components_1982)
c1_kittel_1982$c1_trend <- c1k_tmax_components_1982$trend
c1_kittel_1982$c1_deseasoned <- c1_kittel_1982$max_temp - c1k_tmax_components_1982$seasonal
c1_kittel_1982$date <- as.Date(c1_kittel_1982$date)

tmax_trends <- left_join(tmax_1982, d1_kittel_1982[c("date", "d1_trend", "d1_deseasoned")]) %>%
  rename(sdl_trend = trend, sdl_deseasoned = deseasoned) %>%
  left_join(c1_kittel_1982[c("date", "c1_trend", "c1_deseasoned")])

plot_grid(
  tmax_trends[c("date", "sdl_trend", "d1_trend", "c1_trend")] %>%
    gather(met, val, sdl_trend:c1_trend) %>%
    ggplot(aes(date, val, col = met)) +
    geom_vline(data = mindates, aes(xintercept = date), lty = 2) +
    geom_line() +
    scale_color_discrete(name = NULL) +
    scale_x_date(breaks = "4 years", date_labels = "%Y") +
    labs(y = "trend") +
    theme(legend.position = c(0.3, 0.1),
          legend.background = element_blank()),
  
  tmax_trends[c("date", "sdl_deseasoned", "d1_deseasoned", "c1_deseasoned")] %>%
    gather(met, val, sdl_deseasoned:c1_deseasoned) %>%
    #subset(year(date) < 2019) %>%
    ggplot(aes(date, val, col = met)) +
    geom_vline(data = mindates, aes(xintercept = date), lty = 2) +
    geom_line(alpha = 0.5) +
    geom_smooth(aes(fill = met), method = "lm") +
    scale_color_discrete(name = NULL) +
    scale_x_date(breaks = "4 years", date_labels = "%Y") +
    theme(legend.position = "none") +
    facet_wrap(~met, nrow = 3)
)

# 4. tree approach -----
# for curiosity
tree_tmax <- tree(as.numeric(deseasoned) ~ c(1:nrow(tmax_1982)), data = tmax_1982) #doesn't like the date variable, so go by data order
summary(tree_tmax)
plot(tree_tmax)
text(tree_tmax, pretty = 0)
tmax_1982$date[c(1781, 6502, 3954)] #"1986-11-16" "1999-10-20" "1992-10-28"
# 1986 is consistently a shift year, and comes before the instrument change

# how do d1 and c1 compare in same period compare?
tree_tmax_d1 <- tree(as.numeric(d1_deseasoned) ~ c(1:nrow(d1_kittel_1982)), data = d1_kittel_1982)
summary(tree_tmax_d1)
plot(tree_tmax_d1)
text(tree_tmax_d1, pretty = 0)
tree_tmax_d1
d1_kittel_1982$date[1443] #"1985-12-13" .. close to 1986

tree_tmax_c1 <- tree(as.numeric(c1_deseasoned) ~ c(1:nrow(c1_kittel_1982)), data = c1_kittel_1982)
summary(tree_tmax_c1)
plot(tree_tmax_c1)
text(tree_tmax_c1, pretty = 0)
tree_tmax_c1
c1_kittel_1982$date[1777] #"1986-11-12" .. almost same date as at Saddle

# what about full d1 record?
tree_tmax_d1_allyrs <- tree(as.numeric(d1k_tmax_components$x - d1k_tmax_components$seasonal) ~ c(1:nrow(d1_kittel)))
summary(tree_tmax_d1_allyrs)
plot(tree_tmax_d1_allyrs); text(tree_tmax_d1_allyrs, pretty = 0)
d1_kittel$date[c(12706, 10715)] # just the cold spell of the 80s
# 1981 = st. helens; 1982 = chichón; 1985 = US cold wave to polar vortex; 1991 = pinatubo 
# chichón volcano eruption in 1982 responsible for shifts found globally in latter half 80s globally: https://www.sciencedaily.com/releases/2015/11/151124081517.htm


# QUICKCHECK D1/C1 RECENT YEAR TRENDS VS. SADDLE -----
# 1. D1 Kittel + D1 hmps ----
# check d1 logger to look for same pattern in trend last few years of record
d1_log_daily <- getTabular(402) %>% data.frame()
d1_log_daily_temp <- dplyr::select(d1_log_daily, LTER_site:jday, grep("airtemp", names(d1_log_daily))) %>%
  gather(met, val, min(grep("airtemp", names(.))):ncol(.)) %>%
  mutate(type = ifelse(grepl("^flag", met), "flag", "airtemp"),
         met = gsub("flag_|airtemp_", "", met)) %>%
  spread(type, val) %>%
  mutate(instrument = ifelse(grepl("hmp", met), str_extract(met, "hmp[0-9]"), "logger"),
         met = gsub("hmp[0-9]_", "", met),
         airtemp = as.numeric(airtemp),
         met = factor(met, levels = c("max", "avg", "min"))) %>%
  # drop rows that are hmp + cr23x because hmps only active since cr1000
  subset(!(logger == "cr23x" & grepl("hmp", instrument))) %>%
  # drop hmps for cr1000 pre-2018
  subset(!(year < 2018 & grepl("hmp", instrument))) %>%
  #check that there is at least 1 obs per met per day
  group_by(date, met) %>%
  mutate(nobs = sum(!is.na(airtemp))) %>%
  ungroup()
str(d1_log_daily_temp) #d1 loggers are out for a stretch in summer of 2020, vals aren't crazy so going to average then stitch to d1 kittel just to check for trend
d1_log_daily_1820 <- subset(d1_log_daily_temp, year > 2017) %>%
  group_by(LTER_site, local_site, logger, date, year, jday, met) %>%
  summarise(daily = mean(airtemp, na.rm = T),
            se = sd(airtemp, na.rm = T)/sqrt(length(airtemp[!is.na(airtemp)])),
            nobs = length(airtemp[!is.na(airtemp)]),
            who = str_flatten(instrument[!is.na(airtemp)], collapse = ","),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(daily = ifelse(nobs == 0, NA, daily)) # there are some wild differences in daily temp between hmps.. (big se values)

d1_tmax_stitch <- subset(d1_log_daily_1820, met == "max", select = c(date, daily)) %>%
  left_join(d1_kittel_1982[c("date", "max_temp")])
#how related is chart and logger tmax?
with(d1_tmax_stitch[complete.cases(d1_tmax_stitch),], cor.test(daily, max_temp)) # okay enough
ggplot(d1_tmax_stitch) + geom_line(aes(date, daily), col = "pink") + geom_line(aes(date, max_temp), col = "orchid")
# big difference in end of 2018 for logger vs. chart -- i didn't make any adjustments to chart at end of 2018
# > stay with chart for quick assessment, logger temps look warm for that time of year
d1$date <- as.Date(d1$date)
d1_1920 <- subset(d1, year(date)>2017)
d1_tmax_stitch <- left_join(d1_tmax_stitch, d1_1920[c("date", "airtemp_max")])
ggplot(d1_tmax_stitch) +
  geom_line(aes(date, airtemp_max), col = "purple", lwd = 1.5, alpha = 0.4) +
  geom_line(aes(date, daily), col = "pink") + 
  geom_line(aes(date, max_temp), col = "orchid")
d1_tmax_stitch <- d1_tmax_stitch %>%
  mutate(final_tmax = ifelse(year(date)== 2018, max_temp, ifelse(is.na(daily), airtemp_max, daily)))

ggplot(d1_tmax_stitch) +
  geom_line(aes(date, airtemp_max), col = "purple") +
  geom_line(aes(date, daily), col = "pink") + 
  geom_line(aes(date, max_temp), col = "orchid") +
  geom_line(aes(date, final_tmax), lwd = 1, col = "orange", alpha = 0.3)

d1_tmax_allyrs <- d1_kittel[c("date", "max_temp")] %>%
  rename(final_tmax = max_temp) %>%
  rbind(subset(d1_tmax_stitch, year(date) > 2018, select = c("date", "final_tmax")))
d1_tmax_decomp <- decompose(ts(d1_tmax_allyrs$final_tmax,
                               start = c(1952, 1), freq = 365.24)) 
plot(d1_tmax_decomp)
# how does 1982 compare?
d1_tmax_decomp_8220 <- decompose(ts(d1_tmax_allyrs$final_tmax[year(d1_tmax_allyrs$date) > 1981],
                                    start = c(1982, 1), freq = 365.24)) 
plot(d1_tmax_decomp_8220)

d1decompdat <- subset(d1_tmax_allyrs, year(date) > 1981)
d1decompdat$d1_trendall <- as.numeric(d1_tmax_decomp_8220$trend)
d1decompdat$d1_deseasoned_all <- d1decompdat$final_tmax - as.numeric(d1_tmax_decomp_8220$seasonal)
tmax_trends <- left_join(tmax_trends, d1decompdat[c("date", "d1_trendall", "d1_deseasoned_all")]) 

plot_grid(
  tmax_trends[c("date", "sdl_trend", "d1_trendall", "c1_trend")] %>%
    rename(d1_trend = d1_trendall) %>%
    gather(met, val, sdl_trend:c1_trend) %>%
    ggplot(aes(date, val, col = met)) +
    geom_vline(data = mindates, aes(xintercept = date), lty = 2) +
    geom_line() +
    scale_color_discrete(name = NULL) +
    scale_x_date(breaks = "4 years", date_labels = "%Y") +
    labs(y = "trend") +
    theme(legend.position = c(0.3, 0.1),
          legend.background = element_blank()),
  tmax_trends[c("date", "sdl_deseasoned", "d1_deseasoned_all", "c1_deseasoned")] %>%
    rename(d1_deseasoned = d1_deseasoned_all) %>%
    gather(met, val, sdl_deseasoned:c1_deseasoned) %>%
    #subset(year(date) < 2019) %>%
    ggplot(aes(date, val, col = met)) +
    geom_vline(data = mindates, aes(xintercept = date), lty = 2) +
    geom_line(alpha = 0.5) +
    geom_smooth(aes(fill = met), method = "lm") +
    scale_color_discrete(name = NULL) +
    scale_x_date(breaks = "4 years", date_labels = "%Y") +
    theme(legend.position = "none") +
    facet_wrap(~met, nrow = 3)
)


# 2. C1 Kittel + C1 hmps ----
# since D1 logger data doesn't agree with D1 chart at end of 2018, check C1 Kittel + C1 hmps
c1_log_daily <- getTabular(401) %>% data.frame()
c1_log_daily_temp <- dplyr::select(c1_log_daily, LTER_site:jday, grep("airtemp", names(c1_log_daily))) %>%
  gather(met, val, min(grep("airtemp", names(.))):ncol(.)) %>%
  mutate(type = ifelse(grepl("^flag", met), "flag", "airtemp"),
         met = gsub("flag_|airtemp_", "", met)) %>%
  spread(type, val) %>%
  mutate(instrument = ifelse(grepl("hmp", met), str_extract(met, "hmp[0-9]"), "logger"),
         met = gsub("hmp[0-9]_", "", met),
         airtemp = as.numeric(airtemp),
         met = factor(met, levels = c("max", "avg", "min"))) %>%
  # drop rows that are hmp + cr23x because hmps only active since cr1000
  subset(!(logger == "cr23x" & grepl("hmp", instrument))) %>%
  # drop hmps for cr1000 pre-2018
  subset(!(year < 2018 & grepl("hmp", instrument))) %>%
  #check that there is at least 1 obs per met per day
  group_by(date, met) %>%
  mutate(nobs = sum(!is.na(airtemp))) %>%
  ungroup()
str(c1_log_daily_temp)
summary(c1_log_daily_temp[c1_log_daily_temp$year > 2019,])

c1_log_daily_1820 <- subset(c1_log_daily_temp, year > 2017) %>%
  group_by(LTER_site, local_site, logger, date, year, jday, met) %>%
  summarise(daily = mean(airtemp, na.rm = T),
            se = sd(airtemp, na.rm = T)/sqrt(length(airtemp[!is.na(airtemp)])),
            nobs = length(airtemp[!is.na(airtemp)]),
            who = str_flatten(instrument[!is.na(airtemp)], collapse = ","),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(daily = ifelse(nobs == 0, NA, daily)) #
# count days per yr per met
group_by(c1_log_daily_1820, met, year) %>%
  summarise(nobs = length(date[!is.na(date)])) # all there

c1_tmax_stitch <- subset(c1_log_daily_1820, met == "max", select = c(date, daily, se)) %>%
  left_join(c1_kittel_1982[c("date", "max_temp")])
#how related is chart and logger tmax?
with(c1_tmax_stitch[complete.cases(c1_tmax_stitch),], cor.test(daily, max_temp)) # better than d1
ggplot(c1_tmax_stitch) + geom_line(aes(date, daily), col = "pink") + geom_line(aes(date, max_temp), col = "orchid")
# summer logger a bit warmer than chart in 2018, but winter 18-19 looks better aligned than d1
c1_tmax_stitch$diff <- with(c1_tmax_stitch, daily - max_temp)
# where are the biggest differences?
ggplot(subset(c1_tmax_stitch, year(date) == 2018), aes(date, diff)) + geom_point() # in winter.. makes sense

# look at c1 temps in beginning of 2019
c1$date <- as.Date(c1$date)
c1_1920 <- subset(c1, year(date)>2017)
c1_tmax_stitch <- left_join(c1_tmax_stitch, c1_1920[c("date", "airtemp_max")])
ggplot(c1_tmax_stitch) +
  geom_line(aes(date, airtemp_max), col = "purple", lwd = 1.5, alpha = 0.4) +
  geom_line(aes(date, daily), col = "pink") + 
  geom_line(aes(date, max_temp), col = "orchid")
# check rawchart diff for recent yrs
c1_tmax_stitch$rawdiff <- c1_tmax_stitch$daily - c1_tmax_stitch$airtemp_max
ggplot(c1_tmax_stitch, aes(date, rawdiff)) + 
  geom_point() 
# biggest diffs at the end of 2018.. looking at it manually, both sources have big temp drops here and there. not sure which is more reliable just from a quick scan
# stick with kittel data through 2018 and hmp averages after for quick assessment

c1_tmax_stitch  <- c1_tmax_stitch %>%
  mutate(final_tmax = ifelse(year(date)== 2018, max_temp, ifelse(is.na(daily), airtemp_max, daily)))

ggplot(c1_tmax_stitch) +
  geom_line(aes(date, airtemp_max), col = "purple") +
  geom_line(aes(date, daily), col = "pink") + 
  geom_line(aes(date, max_temp), col = "orchid") +
  geom_line(aes(date, final_tmax), lwd = 1, col = "orange", alpha = 0.3)

c1_tmax_allyrs <- c1_kittel[c("date", "max_temp")] %>%
  rename(final_tmax = max_temp) %>%
  rbind(subset(c1_tmax_stitch, year(date) > 2018, select = c("date", "final_tmax")))
c1_tmax_decomp <- decompose(ts(c1_tmax_allyrs$final_tmax,
                               start = c(1952, 1), freq = 365.24)) 
plot(c1_tmax_decomp)
# how does 1982 compare?
c1_tmax_decomp_8220 <- decompose(ts(c1_tmax_allyrs$final_tmax[year(c1_tmax_allyrs$date) > 1981],
                                    start = c(1982, 1), freq = 365.24)) 
plot(c1_tmax_decomp_8220)

c1decompdat <- subset(c1_tmax_allyrs, year(date) > 1981)
c1decompdat$c1_trendall <- as.numeric(c1_tmax_decomp_8220$trend)
c1decompdat$c1_deseasoned_all <- c1decompdat$final_tmax - as.numeric(c1_tmax_decomp_8220$seasonal)
tmax_trends <- left_join(tmax_trends, c1decompdat[c("date", "c1_trendall", "c1_deseasoned_all")]) 

plot_grid(
  tmax_trends[c("date", "sdl_trend", "c1_trendall", "c1_trend")] %>%
    #rename(c1_trend = c1_trendall) %>%
    gather(met, val, sdl_trend:c1_trend) %>%
    ggplot(aes(date, val, col = met)) +
    geom_vline(data = mindates, aes(xintercept = date), lty = 2) +
    geom_line() +
    scale_color_discrete(name = NULL) +
    scale_x_date(breaks = "4 years", date_labels = "%Y") +
    labs(y = "trend") +
    theme(legend.position = c(0.3, 0.1),
          legend.background = element_blank()),
  tmax_trends[c("date", "sdl_deseasoned", "c1_deseasoned_all", "c1_deseasoned")] %>%
    #rename(c1_deseasoned = c1_deseasoned_all) %>%
    gather(met, val, sdl_deseasoned:c1_deseasoned) %>%
    #subset(year(date) < 2019) %>%
    ggplot(aes(date, val, col = met)) +
    geom_vline(data = mindates, aes(xintercept = date), lty = 2) +
    geom_line(alpha = 0.5) +
    geom_smooth(aes(fill = met), method = "lm") +
    scale_color_discrete(name = NULL) +
    scale_x_date(breaks = "4 years", date_labels = "%Y") +
    theme(legend.position = "none") +
    facet_wrap(~met, nrow = 3)
)

# plot all
plot_grid(
  tmax_trends[c("date", "sdl_trend", "d1_trendall", "c1_trendall")] %>%
    rename(c1_trend = c1_trendall,
           d1_trend = d1_trendall) %>%
    gather(met, val, sdl_trend:c1_trend) %>%
    mutate(met = factor(met, levels = c("c1_trend", "sdl_trend", "d1_trend"))) %>%
    ggplot(aes(date, val, col = met)) +
    geom_vline(data = mindates, aes(xintercept = date), lty = 2) +
    geom_line() +
    scale_color_discrete(name = NULL) +
    scale_x_date(breaks = "4 years", date_labels = "%Y") +
    labs(y = "trend") +
    theme(legend.position = c(0.3, 0.1),
          legend.background = element_blank()),
  tmax_trends[c("date", "sdl_deseasoned", "d1_deseasoned_all", "c1_deseasoned_all")] %>%
    rename(c1_deseasoned = c1_deseasoned_all, d1_deseasoned = d1_deseasoned_all) %>%
    gather(met, val, sdl_deseasoned:c1_deseasoned) %>%
    mutate(met = factor(met, levels = c("c1_deseasoned", "sdl_deseasoned", "d1_deseasoned"))) %>%
    #subset(year(date) < 2019) %>%
    ggplot(aes(date, val, col = met)) +
    geom_vline(data = mindates, aes(xintercept = date), lty = 2) +
    geom_line(alpha = 0.5) +
    geom_smooth(aes(fill = met), method = "lm") +
    scale_color_discrete(name = NULL) +
    scale_x_date(breaks = "4 years", date_labels = "%Y") +
    theme(legend.position = "none") +
    facet_wrap(~met, nrow = 3)
)


# -- FINISH STITCH ----
# to do:
# > adjust pre-hmp period based on relationship to c1 ameriflux (most consistent, qa'd record)
# > remove and infill high Tmax in 1982 (use D1 raw if all dats there)
# > compare adjustment to sarah's adjustment (just to know how different it might be)
# > if there are logger vals in the raw record present now that were infilled back in 2019, use those instead

check_allyrs <- distinct(check_allyrs)
sdl_out <- subset(check_allyrs, expected_val, select = c(date, yr, mon, doy, logger, infilled, final_method, met, finaltemp)) %>%
  rename(infill_method = final_method)
# check all dates present
check_dates <- seq.Date(min(sdl_out$date), max(sdl_out$date), 1)
summary(check_dates %in% unique(sdl_out$date))
# who is missing..
check_dates[!check_dates %in% unique(sdl_out$date)] # these are the 4 days cr23x and cr21x overlapped transitioning to cr23x
# apparently I used the saddle temp because it was in the middle.. of both
# come back to this, use cr23 or cr21, and adjust backwards (or forwards depending on ameriflux relationship)

june2000 <- subset(check_allyrs, date %in% check_dates[!check_dates %in% unique(sdl_out$date)]) %>%
  dplyr::select(-c(final_method, infilled, method)) %>% # note: cr23x was infilled from sdl on 6/27 for tmax, all other vals are raw logger vals
  # spread loggers to diff them from one another and saddle
  spread(logger, finaltemp) %>%
  mutate(diff21_23 = cr21x - cr23x,
         diffsdl_21 = airtemp - cr21x,
         diffsdl_23 = airtemp - cr23x) # I avg'd them for those 4 days for final temp used..
sapply(split(june2000$diff21_23, june2000$met), mean) #cr23x warmer by 1.1 for tmax, by 0.95 for tmin
# keep cr21x for those 4 days but adjust all of cr21x by +1 for tmax and tmin
june2000_keep <- subset(check_allyrs, date %in% check_dates[!check_dates %in% unique(sdl_out$date)]) %>%
  subset(logger == "cr21x") %>%
  rename(infill_method = final_method) %>%
  dplyr::select(names(sdl_out))

sdl_out <- rbind(sdl_out, june2000_keep) %>%
  arrange(date, met)
# check dates again
check_dates[!check_dates %in% unique(sdl_out$date)] # good


# 1. infill tmax in 1982 -----
tmax82 <- with(sdl_out, date[yr == 1982 & finaltemp == max(finaltemp)])
# check this is the correct date
sdl_chart_qa2018[sdl_chart_qa2018$date == tmax82 & sdl_chart_qa2018$met == "airtemp_max", ] # correct
tmax82_window <- seq.Date(tmax82 - 30, tmax82 + 30, 1)
# see if D1 kittel will work
View(subset(d1_kittel, date %in% tmax82_window)) # all A's, good to go
sdl_d1k82 <- subset(sdl_out, date %in% tmax82_window & grepl("max", met)) %>%
  left_join(subset(d1_kittel, date %in% tmax82_window, select = c(date, max_temp)))
# check diff por curiosidad
plot(sdl_d1k82$finaltemp ~ sdl_d1k82$max_temp)
summary(sdl_d1k82$finaltemp - sdl_d1k82$max_temp)
ggplot(sdl_d1k82) + geom_line(aes(date, finaltemp), col = "black") + 
  geom_line(aes(date, max_temp), col = "blue")
# join C1 to see if it has some spike jul 13ish
View(subset(c1_kittel, date %in% tmax82_window)) # c1 infilled start jul 21
sdl_d1k82 <- rename(sdl_d1k82, d1_tmax = max_temp) %>%
  left_join(subset(c1_kittel, date %in% tmax82_window, select = c(date, max_temp, flag_1))) %>%
  rename(c1_tmax = max_temp, c1flag1 = flag_1)
# visualize
ggplot(sdl_d1k82) + geom_line(aes(date, finaltemp), col = "black") + 
  geom_line(aes(date, d1_tmax), col = "blue") +
  geom_line(aes(date, c1_tmax), col = "forestgreen") +
  geom_point(data = subset(sdl_d1k82, c1flag1 != "A"), aes(date, c1_tmax, col = c1flag1))
# seems mostly fine.. remove high D1 val from consideration
d1high <- with(sdl_d1k82, date[d1_tmax == max(d1_tmax)]) # jul 12
c1_low <- with(sdl_d1k82, date[c1_tmax == min(c1_tmax)]) # may 31

sdld1c1_lm82 <- lm(finaltemp ~ d1_tmax + c1_tmax, data = subset(sdl_d1k82, !date %in% c(tmax82, d1high, c1_low) | c1flag1 != "A"))
summary(sdld1c1_lm82)
# compare w d1 only
sdld1_lm82 <- lm(finaltemp ~ d1_tmax, data = subset(sdl_d1k82, !date %in% c(tmax82, d1high, c1_low) | c1flag1 != "A"))
summary(sdld1_lm82)
anova(sdld1_lm82, sdld1c1_lm82) # says model 2 is better.. will go w that for now

fits82 <- predict(sdld1c1_lm82, newdata = sdl_d1k82, se.fit = T, interval = "prediction")
sdl_d1k82 <- cbind(sdl_d1k82, fits82$fit, se = fits82$se.fit)

# how close are the predictions?
ggplot(sdl_d1k82) + geom_line(aes(date, finaltemp), col = "grey50") + 
  geom_line(aes(date, d1_tmax), col = "blue", alpha = 0.5) +
  geom_line(aes(date, c1_tmax), col = "forestgreen", alpha = 0.5) +
  geom_line(aes(date, fit), col = "purple", lwd = 1) + # ok
  geom_line(aes(date, upr), col = "orchid", lty = 3) +
  geom_line(aes(date, lwr), col = "orchid", lty = 3) # prediction captures actual value

tmaxrow <- which(sdl_out$date == tmax82 & sdl_out$met == "max")
sdl_out$finaltemp[tmaxrow] <- sdl_d1k82$fit[sdl_d1k82$ date == tmax82]
sdl_out$infill_method[tmaxrow] <- "cr21x-sdl chart ~ d1 + c1 chart, 60d window"

# 2. replace infilled with missing vals updated since site visit ---- 
# easiest for now is compare check_all years logger period to edi daily logger, note what is different and replace as needed
sdlcr_qad_sitevisit <- read_csv("../long-term-trends/extended_summer/analysis/output_data/prep_data/qa_sdlcr_temp.csv") %>%
  mutate(met = gsub("airtemp_", "", met))
check_updates <-  subset(sdl_logger_temp, year(date) < 2019 & !grepl("hmp", instrument), select = c(date, logger, met, airtemp, flag)) %>%
  rename_at(names(.)[grepl("airt|flag", names(.))], function(x) paste0("rawlogger_", x)) %>%
  left_join(check_allyrs) %>%
  left_join(sdlcr_qad_sitevisit[c("date", "met", "logger", "cr_temp", "qa_temp", "qa_flag")]) %>%
  rename(sitevis_crtemp = cr_temp, crqa_temp = qa_temp, crqa_flag = qa_flag)

View(subset(check_updates, rawlogger_airtemp != sitevis_crtemp)) # only one obs that was flagged as a sensor fail on 2017-02-21
# it's -4 vs. -10 for tmin, so see what happened at c1 and d1
tminfeb <- as.Date("2017-02-21")
feb17_window <- seq.Date(tminfeb - 30, tminfeb + 30, 1)

ggplot(subset(check_updates, grepl("min", met) & date %in% feb17_window)) +
  geom_line(aes(date, rawlogger_airtemp), col = "black") +
  geom_line(aes(date, finaltemp), col = "orchid2") +
  geom_line(data = subset(d1_logger_edi, date %in% feb17_window), aes(date, airtemp_min), col = "blue") +
  geom_line(data = subset(ameriforest, met == "min" & date %in% feb17_window), aes(date, airtemp), col = "forestgreen")
# definitely go with raw updated value
# add cr qa flagging too for info
tminrow <- which(sdl_out$date == tminfeb & sdl_out$met == "min")
sdl_out[tminrow,] # correct row
# grab replacement row
tminreplace <- which(check_updates$rawlogger_airtemp != check_updates$sitevis_crtemp)
check_updates[tminreplace, ] # correct row

sdl_out$finaltemp[tminrow] <- check_updates$rawlogger_airtemp[tminreplace]
sdl_out$infilled[tminrow] <- FALSE
sdl_out$infill_method[tminrow] <- sdl_out$logger[tminrow]
# check row
sdl_out[tminrow,] # good


# 3. stack mean hmps 2019-2020 ----
# prep hmp daily means to stack with sdl_out
sdlhmps <- mutate(alldates2, infilled = is.na(daily),
                  infill_method = ifelse(is.na(daily), "sdl hmp metric mean ~ d1 hmp metric mean, 60d window", "sdl hmp metric mean"),
                  logger = paste0(logger, " hmps")) %>%
  rename(hmps_used = hmps,
         source_instrument = logger,
         doy = jday, yr = year, finaltemp = airtemp) %>%
  mutate(mon = month(date))


sdl_out2 <- rename(sdl_out, source_instrument = logger) %>%
  mutate(source_instrument = ifelse(grepl("cr21x-sdl", infill_method), "sdl chart", source_instrument),
         hmps_used = NA) %>%
  rbind(sdlhmps[names(.)]) %>%
  arrange(date, met)
# note: have cr1000 and hmps values in here for all of 2018  

# 4. quick homogenize for instrument changes -----
sdl_homog <- left_join(sdl_out2, ameriforest[c("date", "met", "airtemp")]) %>%
  rename(ameriflux = airtemp) %>%
  mutate(diff_sdlflux = finaltemp-ameriflux)

# what is the difference by instrument period?
with(sdl_homog, sapply(split(diff_sdlflux, source_instrument), summary))
# create winter/summer to look for seasonal differences
sdl_homog$season <- with(sdl_homog, ifelse(mon %in% c(10:12, 1:5), "Nov-May", "Jun-Sep"))
# calculate mean diffs
meandiffs_sdlflux <- subset(sdl_homog, !is.na(diff_sdlflux)) %>%
  group_by(source_instrument, met,) %>%
  mutate(mean_globaldiff = mean(diff_sdlflux),
         se_globaldiff = (sd(diff_sdlflux))/(sqrt(length(diff_sdlflux)))) %>%
  group_by(source_instrument, met, season) %>%
  mutate(mean_seasondiff = mean(diff_sdlflux),
         se_seasondiff = (sd(diff_sdlflux))/(sqrt(length(diff_sdlflux)))) %>%
  ungroup() #%>%
# dplyr::select(grep("^source|^season|^mean_|^se_|^met$", names(.))) %>%
# distinct() %>%
# mutate(source_instrument = factor(source_instrument, levels = c("cr21x", "cr23x", "cr1000", "cr1000 hmps"))) %>%
# arrange(source_instrument, season, met)

# also start at 1999-07-02 so comparable to decomposed
meandiffs_sdlflux1999 <- subset(sdl_homog, !is.na(diff_sdlflux)) %>%
  # trend ends at 2020-07-02
  filter(date >= as.Date("1999-07-02") & date <= as.Date("2020-07-02")) %>%
  group_by(source_instrument, met,) %>%
  mutate(mean_globaldiff = mean(diff_sdlflux),
         se_globaldiff = (sd(diff_sdlflux))/(sqrt(length(diff_sdlflux)))) %>%
  group_by(source_instrument, met, season) %>%
  mutate(mean_seasondiff = mean(diff_sdlflux),
         se_seasondiff = (sd(diff_sdlflux))/(sqrt(length(diff_sdlflux)))) %>%
  ungroup() %>%
  dplyr::select(source_instrument, season, met, grep("^mean|^se", names(.))) %>%
  distinct()

ggplot(subset(sdl_homog, !is.na(diff_sdlflux)), aes(date, diff_sdlflux, col = paste(source_instrument, season))) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(alpha = 0.5, lty = 2) +
  #geom_smooth(aes(fill = paste(source_instrument, season)), method = "lm") +
  #geom_hline(data = meandiffs_sdlflux, aes(yintercept = mean_seasondiff, col = paste(source_instrument, season))) +
  geom_line(data = meandiffs_sdlflux, aes(date, mean_seasondiff, col =paste(source_instrument, season) )) +
  facet_grid(~met)

# add d1 kittel for comparison (note: there are some periods of drift in d1 chart)
d1k_long <- dplyr::select(d1_kittel, c(date, max_temp, min_temp)) %>%
  gather(met, d1_temp, max_temp:min_temp) %>%
  mutate(met = gsub("_temp", "", met))

sdl_homog <- left_join(sdl_homog, d1k_long) %>%
  mutate(diff_sdld1 = finaltemp - d1_temp,
         diff_fluxd1 = d1_temp - ameriflux)

meandiffs_sdld1 <- subset(sdl_homog, !is.na(diff_sdld1)) %>%
  group_by(source_instrument, met,) %>%
  mutate(mean_globaldiff = mean(diff_sdld1),
         se_globaldiff = (sd(diff_sdld1))/(sqrt(length(diff_sdld1)))) %>%
  group_by(source_instrument, met, season) %>%
  mutate(mean_seasondiff = mean(diff_sdld1),
         se_seasondiff = (sd(diff_sdld1))/(sqrt(length(diff_sdld1)))) %>%
  ungroup()

ggplot(subset(sdl_homog, !is.na(diff_sdld1)), aes(date, diff_sdld1)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(col = paste(source_instrument, season)), alpha = 0.5, lty = 2) +
  geom_smooth(aes(fill = paste(source_instrument, season)), method = "lm") +
  # look for cyclic trend to explain undulations
  geom_smooth(aes(fill = paste(source_instrument, season)), method = "lm") +
  #geom_hline(data = meandiffs_sdld1, aes(yintercept = mean_seasondiff, col = paste(source_instrument, season))) +
  geom_line(data = meandiffs_sdld1, aes(date, mean_seasondiff, col =paste(source_instrument, season) )) +
  # look for cyclic trend to explain undulations
  geom_smooth(col = "grey50") +
  facet_grid(season~met)


# try detrended comparison
# need complete year
sdlallyrs_cr1000 <- subset(sdl_homog,!(source_instrument == "cr1000 hmps" & yr == 2018) & yr > 1981)
sdlallyrs_hmps <- subset(sdl_homog,!(source_instrument == "cr1000" & yr == 2018) & yr > 1981)
tmax_sdl_allyrs_cr1000 <- ts(sdlallyrs_cr1000$finaltemp[sdlallyrs_cr1000$met == "max"], start = c(1982,1), frequency = 365.24)
tmin_sdl_allyrs_cr1000 <- ts(sdlallyrs_cr1000$finaltemp[sdlallyrs_cr1000$met == "min"], start = c(1982,1), frequency = 365.24)
tmax_sdl_cr1000_components <- decompose(tmax_sdl_allyrs_cr1000)
tmin_sdl_cr1000_components <- decompose(tmin_sdl_allyrs_cr1000)
plot(tmax_sdl_cr1000_components)
plot(tmin_sdl_cr1000_components)

tmax_sdl_allyrs_hmps <- ts(sdlallyrs_hmps$finaltemp[sdlallyrs_hmps$met == "max"], start = c(1982,1), frequency = 365.24)
tmin_sdl_allyrs_hmps <- ts(sdlallyrs_hmps$finaltemp[sdlallyrs_hmps$met == "min"], start = c(1982,1), frequency = 365.24)
tmax_sdl_hmps_components <- decompose(tmax_sdl_allyrs_hmps)
tmin_sdl_hmps_components <- decompose(tmin_sdl_allyrs_hmps)
plot(tmax_sdl_hmps_components)
plot(tmin_sdl_hmps_components)

# put in dataframe
sdl_decomposed <- subset(sdlallyrs_cr1000, met == "max", select = c(date, yr, season, source_instrument, met, finaltemp)) %>%
  cbind(list2DF(tmax_sdl_cr1000_components)) %>%
  rbind(cbind(subset(sdlallyrs_cr1000, met == "min", select = c(date, yr, season, source_instrument, met, finaltemp)), 
              list2DF(tmin_sdl_cr1000_components))) %>%
  # rename for combining with sdl homog dataset
  rename_at(names(.)[grep("^final|^tren|^seas|^rando", names(.))], function(x) paste0("sdl_", x)) %>%
  mutate(version = "cr1000 2018")
# check everything matched properly
summary(sdl_decomposed$sdl_finaltemp == sdl_decomposed$x) #yup

sdl_decomposed_hmps <- subset(sdlallyrs_hmps, met == "max", select = c(date, yr, season, source_instrument, met, finaltemp)) %>%
  cbind(list2DF(tmax_sdl_hmps_components)) %>%
  rbind(cbind(subset(sdlallyrs_hmps, met == "min", select = c(date, yr, season, source_instrument, met, finaltemp)), 
              list2DF(tmin_sdl_hmps_components))) %>%
  # rename for combining with sdl homog dataset
  rename_at(names(.)[grep("^final|^tren|^seas|^rando", names(.))], function(x) paste0("sdl_", x))  %>%
  mutate(version = "hmps 2018")
summary(sdl_decomposed_hmps$sdl_finaltemp == sdl_decomposed_hmps$x) #yes

sdl_decomposed <- rbind(sdl_decomposed, sdl_decomposed_hmps)

# try with c1 ameriflux 1999-2020
tmax_ameriforest <- ts(ameriforest$airtemp[ameriforest$met == "max" & year(ameriforest$date) > 1998], start = c(1999,1), frequency = 365.24)
tmin_ameriforest <- ts(ameriforest$airtemp[ameriforest$met == "min" & year(ameriforest$date) > 1998], start = c(1999,1), frequency = 365.24)
tmax_ameriforest_components <- decompose(tmax_ameriforest)
tmin_ameriforest_components <- decompose(tmin_ameriforest)
plot(tmax_ameriforest_components)
plot(tmin_ameriforest_components)

ameriforest_decomposed <- subset(ameriforest, year(date) > 1998 & met == "min", select = c(date, met, airtemp)) %>%
  cbind(list2DF(tmin_ameriforest_components)) %>%
  rbind(cbind(subset(ameriforest, year(date) > 1998 & met == "max", select = c(date, met, airtemp)),
              list2DF(tmax_ameriforest_components))) %>%
  # rename for combining with sdl
  rename_at(names(.)[grep("^air|^tren|^seas|^rando", names(.))], function(x) paste0("c1flux_", x))
# check everything matched properly
summary(ameriforest_decomposed$c1flux_airtemp == ameriforest_decomposed$x) #yup

# sdl_homog2 <- left_join(sdl_homog, sdl_decomposed[!grepl("^x$|^fig|type", names(sdl_decomposed))]) %>%
#   left_join(ameriforest_decomposed[!grepl("^x$|^fig|type", names(ameriforest_decomposed))]) %>%
#   mutate(diff_sdlflux_trend = sdl_trend - c1flux_trend)

sdl_homog2 <- left_join(sdl_decomposed[!grepl("^x$|^fig|type", names(sdl_decomposed))],
                        ameriforest_decomposed[!grepl("^x$|^fig|type", names(ameriforest_decomposed))]) %>%
  mutate(diff_sdlflux_trend = sdl_trend - c1flux_trend)

ggplot(subset(sdl_homog2, yr > 1998), aes(date, diff_sdlflux_trend, col = source_instrument)) +
  geom_line(aes(lty = sdl_season), alpha = 0.5) +
  geom_smooth(method = "lm") +
  #scale_shape_manual(values = c(1,8)) +
  facet_grid(met~., scales = "free_y")

# needs more of a lens on early period.. compare against d1 kittel but just to 2010 (to avoid weird shifts)
# have d1 tmax decomposed from above
d1_kittel_1982 <- subset(d1_kittel, year >= 1982)
d1k_tmax_1982 <- ts(d1_kittel_1982$max_temp, start = c(1982,1), frequency = 365.24)
d1k_tmax_components_1982 <- decompose(d1k_tmax_1982)
d1k_tmin_1982 <- ts(d1_kittel_1982$min_temp, start = c(1982,1), frequency = 365.24)
d1k_tmin_components_1982 <- decompose(d1k_tmin_1982)
plot(d1k_tmin_components_1982)

d1k_decomposed <- cbind(met = "max", subset(d1_kittel_1982, select = c(date, max_temp)), list2DF(d1k_tmax_components_1982)) %>%
  rename(min_temp = max_temp) %>%
  rbind(cbind(met = "min", subset(d1_kittel_1982, select = c(date, min_temp)), list2DF(d1k_tmin_components_1982))) %>%
  rename(d1k_airtemp = min_temp) %>%
  rename_at(names(.)[grep("^tren|^seas|^rando", names(.))], function(x) paste0("d1k_", x))

sdl_homog2 <- left_join(sdl_homog2, subset(d1k_decomposed[!grepl("x|type|figure", names(d1k_decomposed))])) %>%
  mutate(diff_sdld1k_trend = sdl_trend - d1k_trend,
         diff_d1kflux_trend = d1k_trend - c1flux_trend)

ggplot(subset(sdl_homog2), aes(date)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(y = diff_sdlflux_trend), alpha = 0.5, col = "grey30") +
  geom_line(aes(y = diff_sdld1k_trend), alpha = 0.5, col = "orchid") +
  geom_line(aes(y = diff_d1kflux_trend), alpha = 0.5, col = "pink") +
  #geom_smooth(method = "lm") +
  #scale_shape_manual(values = c(1,8)) +
  labs(subtitle = "diffed trends (deseasoned): orchid = sdl - d1k, grey = sdl - c1 flux, pink = d1k - c1 flux") +
  facet_grid(met~factor(source_instrument, levels = c("sdl chart", "cr21x", "cr23x", "cr1000", "cr1000 hmps")), scales = "free")


# try with c1
c1_kittel_1982 <- subset(c1_kittel, year >= 1982)
c1k_tmax_1982 <- ts(c1_kittel_1982$max_temp, start = c(1982,1), frequency = 365.24)
c1k_tmax_components_1982 <- decompose(c1k_tmax_1982)
c1k_tmin_1982 <- ts(c1_kittel_1982$min_temp, start = c(1982,1), frequency = 365.24)
c1k_tmin_components_1982 <- decompose(c1k_tmin_1982)
plot(c1k_tmin_components_1982)

c1k_decomposed <- cbind(met = "max", subset(c1_kittel_1982, select = c(date, max_temp)), list2DF(c1k_tmax_components_1982)) %>%
  rename(min_temp = max_temp) %>%
  rbind(cbind(met = "min", subset(c1_kittel_1982, select = c(date, min_temp)), list2DF(c1k_tmin_components_1982))) %>%
  rename(c1k_airtemp = min_temp) %>%
  rename_at(names(.)[grep("^tren|^seas|^rando", names(.))], function(x) paste0("c1k_", x))


sdl_homog3 <- left_join(sdl_homog2, subset(c1k_decomposed[!grepl("x|type|figure", names(c1k_decomposed))])) %>%
  mutate(diff_sdlc1k_trend = sdl_trend - c1k_trend,
         diff_c1kflux_trend = c1k_trend - c1flux_trend)

ggplot(subset(sdl_homog3), aes(date)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(y = diff_sdlflux_trend), alpha = 0.5, col = "grey30") +
  geom_line(aes(y = diff_sdld1k_trend), alpha = 0.5, col = "orchid") +
  geom_line(aes(y = diff_d1kflux_trend), alpha = 0.5, col = "pink") +
  geom_line(aes(y = diff_sdlc1k_trend), alpha = 0.5, col = "dodgerblue2") +
  geom_line(aes(y = diff_c1kflux_trend), alpha = 0.5, col = "lightblue") +
  #geom_smooth(method = "lm") +
  #scale_shape_manual(values = c(1,8)) +
  labs(subtitle = "diffed trends (deseasoned):\norchid = sdl - d1k, grey = sdl - c1 flux, pink = d1k - c1 flux, dark blue = sdl - c1k, light blue = c1k - flux") +
  facet_grid(met~factor(source_instrument, levels = c("sdl chart", "cr21x", "cr23x", "cr1000", "cr1000 hmps")), scales = "free")

# plot c1 chart trend vs. c1 ameriflux trend for direct comparison (chart makes it seem things changing at c1 1990-2018, but ameriflux says no)
ggplot(sdl_homog3, aes(date)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_line(aes(y = c1flux_trend, lty = met), col = "grey50") +
  geom_line(aes(y = c1k_trend, lty = met), col = "purple") + # it's similar
  labs(subtitle = "purple = c1 kittel trend, grey = c1 ameriflux trend")
# look at actual temps
ggplot(distinct(sdl_homog3[c("date", "c1flux_airtemp", "c1k_airtemp", "met")]), aes(date)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_line(aes(y = c1k_airtemp), col = "purple",lwd = 1, alpha = 0.5) +
  geom_line(aes(y = c1flux_airtemp), col = "grey50", alpha = 0.5) +
  facet_wrap(~met, nrow = 2)
# c1 chart is colder than ameriflux mins, and warmer than ameriflux tmax
# > which explains difference in trend lines in preceding graph

# look at means of time series for tmax and tmin
with(sdl_homog3, sapply(split(c1flux_airtemp, met), function(x) mean(x, na.rm = T)))
# max       min 
# 6.081225 -1.326337 
with(sdl_homog3, sapply(split(c1k_airtemp, met), function(x) mean(x, na.rm = T)))
# max       min 
# 8.444753 -4.404542 


# calculate mean diff for each period sdl and ameriflux, both detrended and raw (should be similar since it's the mean)
meandiffs_sdlflux2 <- meandiffs_sdlflux %>%
  group_by(source_instrument) %>%
  mutate(start_year = min(yr)) %>%
  ungroup() %>%
  dplyr::select(grep("^start|^mean|^source|^se|met$", names(.))) %>%
  distinct() %>%
  # tidy
  gather(difftype, val, mean_globaldiff:se_seasondiff) %>%
  mutate(metric = ifelse(grepl("^mean", difftype), "mean", "se"),
         difftype = gsub("mean_|se_", "", difftype),
         season = ifelse(grepl("global", difftype), "global", season)) %>%
  distinct() %>%
  data.frame() %>%
  # diff means from instrument to instrument
  group_by(met, metric, season) %>%
  mutate(instrument_shift = val - lag(val))

instrument_dates <- distinct(sdl_out2[c("date", "source_instrument")]) %>%
  group_by(source_instrument) %>%
  summarise(date = min(date))

dplyr::select(meandiffs_sdlflux2, -instrument_shift) %>%
  ungroup() %>%
  spread(metric, val) %>%
  ggplot(aes(start_year, mean, col = paste(difftype, season))) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), width = 0) +
  labs(subtitle = "mean diff (sdl - c1 ameriflux) per logger period [points at start date for instrument]") +
  facet_wrap(~met)


# compare with means based on diffed deseasoned trends
meandiffs_trends_sdl <- dplyr::select(sdl_homog3, date, sdl_season, version, source_instrument, met, grep("^diff.*end$", names(sdl_homog3))) %>%
  rename(season = sdl_season) %>%
  gather(difftype, val, grep("^diff", names(.))) %>%
  subset(!is.na(val)) %>%
  group_by(version, source_instrument, difftype, met) %>%
  mutate(mean_global = mean(val),
         se_global = sd(val)/sqrt(length(val))) %>%
  group_by(version, season, source_instrument, difftype, met, mean_global, se_global) %>%
  summarise(mean_season = mean(val),
            se_season = sd(val)/sqrt(length(val)),
            .groups = "drop_last") %>%
  gather(type, val, mean_global:ncol(.)) %>%
  mutate(season = ifelse(grepl("global", type), "global", season),
         type = gsub("_global|_season", "", type),
         version = gsub(" ","_", version),
         val = round(val, 6)) %>%
  left_join(instrument_dates) %>%
  #unite(type, type, version) %>%
  data.frame() %>%
  distinct() %>%
  arrange(difftype, season, met, date) %>%
  # diff instrument to instrument change
  group_by(version, season, difftype, met, type) %>%
  mutate(instrument_shift = val - lag(val)) %>%
  ungroup()

# plot all means for comparison
meandiffs_trends_sdl%>%
  dplyr::select(-instrument_shift) %>%
  # change start date for any diff involving flux to 01-01-1999 (start for detrended flux data)
  mutate(date2 = ifelse((grepl("flux", difftype) & source_instrument == "cr21x"), "1999-01-01", as.character(date)),
         date2= as.Date(date2)) %>%
  spread(type, val) %>%
  ggplot(aes(date2, mean, col = difftype, lty = season)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), width = 0.25) +
  labs(subtitle = "Mean diff per sdl instrument period (pts at new instrument date) on *detrended* site temps") +
  facet_wrap(~version + met)

# calculate against c1 kittel also
meandiffs_sdlc1k <- dplyr::select(sdl_homog3, version, date:sdl_finaltemp, c1k_airtemp) %>%
  mutate(diff_sdlc1k = sdl_finaltemp - c1k_airtemp) %>%
  filter(!is.na(diff_sdlc1k)) %>%
  group_by(version, source_instrument, met) %>%
  mutate(start_year = min(yr),
         mean_global = mean(diff_sdlc1k),
         se_global = (sd(diff_sdlc1k))/(sqrt(length(diff_sdlc1k)))) %>%
  ungroup() #%>%
  # distinct() %>%
  # # tidy
  # gather(difftype, val, mean_globaldiff:se_seasondiff) %>%
  # mutate(metric = ifelse(grepl("^mean", difftype), "mean", "se"),
  #        difftype = gsub("mean_|se_", "", difftype),
  #        season = ifelse(grepl("global", difftype), "global", season)) %>%
  # distinct() %>%
  # data.frame() %>%
  # # diff means from instrument to instrument
  # group_by(met, metric, season) %>%
  # mutate(instrument_shift = val - lag(val))

# 5. assess hourly jennings et al. infilled nwt logger dataset -----
# compare 2018-2020 airtemp_avg vals derivedfrom jennings et al. to mean hmps
#sdl_means_hmps <- subset(jennings, metric == "airtemp_avg" & yr >= 2018 & local_site== "sdl", select = c(data_source:temp))
sdl_means_hmps  <- subset(alldates2, met == "avg") %>%
  ungroup() %>%
  mutate(infilled = ifelse(airtemp != fit | is.na(fit), FALSE, TRUE),
         source_instrument = "cr1000 hmps",
         mon = month(date),
         infill_method = ifelse(infilled, "sdl hmp metric mean ~ d1 hmp metric mean,, 60d window", 
                                "sdl hmp metric mean")) %>%
  rename(yr = year, doy = jday, finaltemp = airtemp, hmps_used = hmps) %>%
  dplyr::select(names(sdl_out2)) %>%
  # compare daily means derived from jennings et al.
  left_join(subset(jennings, metric == "airtemp_avg" & local_site == "sdl", select = c(date, temp, qaflag_hrly_spike, hrly2014_adjusted))) %>%
  rename(jennings = temp) %>%
  mutate(diff_hmpj = finaltemp - jennings)

summary(sdl_means_hmps)
# plot
ggplot(subset(sdl_means_hmps, abs(diff_hmpj) > 5), aes(date)) + #negate abs to see when mostly agree
  # add in sdl min and max t
  geom_point(data = subset(sdl_out2, year(date) >= 2018 & grepl("hmp", source_instrument)), aes(date, finaltemp, col = met), alpha = 0.5) +
  geom_point(aes(y = finaltemp), alpha = 0.5, size = 1) +
  geom_point(aes(y = jennings), col = "purple", size = 1, alpha = 0.5) +
  facet_wrap(~yr, scales = "free_x", nrow = 3)
# for the most part true dailies line up well with derived dailies from hrly, and for the latter some values that disagree exceed daily tmax
# go with true dailies for 2018-2020


# ^ 11/24/21: added comparative code above after code below; adding in code to swap out 2018-2020 values (will look clunky to future caitlin)
# clean up later
mean_jennings <- subset(jennings, metric == "airtemp_avg", select = c(data_source:temp)) %>%
  # code to swap 2018-2020
  left_join(cbind(sdl_means_hmps[c("date", "finaltemp", "jennings")], local_site = "sdl"), by = c("date","local_site", "temp" = "jennings")) %>%
  mutate(temp = ifelse(is.na(finaltemp), temp, finaltemp)) %>%
  dplyr::select(-finaltemp) %>%
  spread(local_site, temp) %>%
  left_join(subset(ameriforest, met == "mean", select = c(date, airtemp))) %>%
  rename(c1flux= airtemp) %>%
  mutate(diff_sdlc1 = sdl - c1,
         diff_sdld1 = sdl-d1,
         diff_d1c1 = d1-c1,
         diff_sdlc1flux = sdl - c1flux,
         diff_c1c1flux = c1 - c1flux,
         diff_d1c1flux = d1 - c1flux)

# how close is c1flux mean to daily mean of hourly mean at c1?
mean_jennings %>%
  dplyr::select(grep("date|yr|^diff", names(.))) %>%
  gather(difftype, val, diff_sdlc1:ncol(.)) %>%
  left_join(distinct(sdl_homog2[c("date", "source_instrument")])) %>%
  ggplot(aes(date, val, col = source_instrument)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = as.Date("2014-01-01"))) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~difftype)
# facet_wrap(~grepl("c1flu", difftype))

# calculate mean diffs per instrument period for sdl
jennings_meandiffs <- mean_jennings %>%
  dplyr::select(grep("date|yr|^diff", names(.))) %>%
  gather(difftype, val, diff_sdlc1:ncol(.)) %>%
  left_join(distinct(sdl_homog2[c("date", "source_instrument")])) %>%
  subset(grepl("sdl|c1c1flux", difftype)) %>%
  # take out cr1000 in 2018 since using hmps
  subset(!(yr == 2018 & source_instrument == "cr1000")) %>%
  group_by(source_instrument, difftype) %>%
  subset(!is.na(val)) %>%
  summarise(start_year = min(yr),
            meandiff = mean(val),
            sediff = sd(val)/sqrt(length(val))) %>%
  ungroup() %>%
  arrange(start_year, difftype) %>%
  #what are the shifts?
  group_by(difftype) %>%
  mutate(instrument_shift = meandiff - lag(meandiff))

ggplot(jennings_meandiffs, aes(start_year, meandiff, col = difftype)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax = meandiff + sediff, ymin = meandiff- sediff), width = 0)

# decompose mean temps
sdl_meantemp_decompose <- decompose(ts(mean_jennings$sdl, start = c(1990,1), frequency = 365.24))
d1_meantemp_decompose <- decompose(ts(mean_jennings$d1, start = c(1990,1), frequency = 365.24))
c1_meantemp_decompose <- decompose(ts(mean_jennings$c1, start = c(1990,1), frequency = 365.24))
c1flux_meantemp_decompose <- list2DF(decompose(ts(mean_jennings$c1flux[mean_jennings$yr >= 1999], 
                                                  start = c(1999,1), frequency = 365.24))) %>%
  cbind(date = mean_jennings$date[mean_jennings$yr >= 1999]) %>%
  rename_all(function(x)paste0("c1flux_",x))

jennings_decomp <- cbind(mean_jennings[c("date", "yr", "sdl")], sdl_trend = sdl_meantemp_decompose$trend) %>%
  cbind(d1 = d1_meantemp_decompose$x, d1_trend = d1_meantemp_decompose$trend) %>%
  cbind(c1 = c1_meantemp_decompose$x, c1_trend = c1_meantemp_decompose$trend) %>%
  left_join(c1flux_meantemp_decompose[c("c1flux_date", "c1flux_x", "c1flux_trend")], by = c("date" = "c1flux_date")) %>%
  # calculate diffs on trends to compare with raw diffs
  mutate(diff_sdlc1flux = sdl_trend - c1flux_trend,
         diff_c1c1flux = c1_trend - c1flux_trend,
         diff_sdlc1 = sdl_trend - c1_trend,
         diff_sdld1 = sdl_trend - d1_trend)

dplyr::select(jennings_decomp, grep("date|^diff", names(jennings_decomp))) %>%
  gather(difftype, val, grep("^diff", names(.))) %>%
  ggplot(aes(date, val, col = difftype)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_vline(aes(xintercept = as.Date("2014-01-01")), lty = 3) +
  geom_vline(data = instrument_dates, aes(xintercept = date)) +
  geom_line()

# calculate mean diff for decomp
jennings_meandiffs_decomp <- jennings_decomp %>%
  dplyr::select(grep("date|yr|^diff", names(.))) %>%
  gather(difftype, val, grep("^diff", names(.))) %>%
  mutate(difftype = paste0(difftype, "_trend")) %>%
  left_join(distinct(sdl_homog2[c("date", "source_instrument")])) %>%
  subset(!is.na(val)) %>%
  group_by(source_instrument, difftype) %>%
  summarise(start_year = min(yr),
            meandiff = mean(val),
            sediff = sd(val)/sqrt(length(val))) %>%
  ungroup() %>%
  arrange(start_year, difftype) %>%
  #what are the shifts?
  group_by(difftype) %>%
  mutate(instrument_shift = meandiff - lag(meandiff))


# 6. adjust instrument periods to homogenize with hmps ----
# > adjustments based on *detrended* mean diffs between sdl and c1 flux and c1 kittel (for chart adjustment) 
# > intuitively based on mean relationships between sdl and c1 in the jennings et al. dataset (most apples:apples comparison) -- they should be fairly consistent over time
# adjustments are rounded to nearest tenth and error on being conservative/leaving some noise in

# max first
tmax_adjustments <- c(0.9, 0, -1.4, -1.5, 0)
names(tmax_adjustments) <- paste0("max_", c("sdl chart", "cr21x", "cr23x", "cr1000", "cr1000 hmps"))
tmin_adjustments <- c(0, +0.25, -0.75, -0.9, 0)
names(tmin_adjustments) <- paste0("min_", c("sdl chart", "cr21x", "cr23x", "cr1000", "cr1000 hmps"))
tmean_adjustments <- c(0, +0.25, -0.9, -0.8, 0)
names(tmean_adjustments) <- paste0("avg_", c("sdl chart", "cr21x", "cr23x", "cr1000", "cr1000 hmps"))
adjustments <- c(tmax_adjustments, tmin_adjustments, tmean_adjustments)
# check all looks good
adjustments

# create adjusted dat col and adjustment note
# pull jennings 1990-2017 to rbind to sdl_out2
jennings_sdlmeans2017 <- subset(jennings, metric == "airtemp_avg" & local_site == "sdl" & yr < 2018) %>%
  mutate(infilled = ifelse(yr> 2013 & hrly2014_adjusted, TRUE,
                           ifelse(yr < 2014 & flag != "qc0in0", TRUE, FALSE)),
         metric = "avg",
         hmps_used = NA) %>%
  rename(infill_method = flag, met = metric, finaltemp = temp) %>%
  left_join(distinct(sdl_out2[c("date", "source_instrument")])) %>%
  dplyr::select(names(sdl_out2))
       

sdl_out3 <- sdl_out2 %>%
  rbind(jennings_sdlmeans2017) %>%
  arrange(date, met) %>%
  mutate(adjustment_to_2018hmps = as.numeric(NA), adjusted_airtemp = as.numeric(NA))

for(i in 1:length(adjustments)){
  a <- adjustments[i]
  tempmetric <- str_extract(names(a), "^m[a-z]{2}|^avg")
  tempinst <- gsub("max_|min_|avg_", "", names(a))
  temprows <- with(sdl_out3[c("date", "met", "source_instrument")], which(met == tempmetric & source_instrument == tempinst))
  # adjust
  sdl_out3$adjustment_to_2018hmps[temprows] <- a
  sdl_out3$adjusted_airtemp[temprows] <- sdl_out3$finaltemp[temprows] + a
}

# calculate math means for start -> 1999-12-31 for tmean
sdl_80s_means <- subset(sdl_out3, met != "avg" & yr < 1990, select = c(date:infill_method, finaltemp)) %>%
  grouped_df(names(.)[!grepl("final|infill", names(.))]) %>%
  summarise(finaltemp = mean(finaltemp),
            nobs = length(doy),
            infilled = str_flatten(unique(infilled)),
            infill_method = str_flatten(unique(infill_method), collapse = ", and tmin: "),
            .groups = "drop_last") %>%
  ungroup() %>%
  mutate(infilled = ifelse(infilled == "TRUE", TRUE, ifelse(infilled == "FALSE", FALSE, TRUE)),
         hmps_used = NA,
         adjustment_to_2018hmps = 0,
         met = "avg",
         adjusted_airtemp = finaltemp)

# rbind to sdl_out3
sdl_out3 <- rbind(sdl_out3, sdl_80s_means[names(sdl_out3)]) %>%
  arrange(date, met) %>%
  # take out cr1000 source for 2018 -- use hmps
  subset(!(source_instrument == "cr1000" & yr == 2018))
with(sdl_out3, sapply(split(finaltemp, met), length))



# 7. review + run through quick QA to see if any crazy values ----
# how does it look?
ggplot(subset(sdl_out3), aes(date, adjusted_airtemp)) +
  geom_line(aes(col = source_instrument), alpha = 0.5) +
  geom_smooth(col = "black") +
  facet_wrap(~met)
# what about DTR?
subset(sdl_out3, met != "avg", select = c(date, source_instrument, met, adjusted_airtemp)) %>%
  spread(met, adjusted_airtemp) %>%
  ggplot(aes(date, max - min)) +
  geom_line(aes(col = source_instrument), alpha = 0.5) +
  geom_smooth(col = "black")
subset(sdl_out3, met != "avg", select = c(date, source_instrument, met, finaltemp, adjusted_airtemp)) %>%
  gather(type, temp, finaltemp, adjusted_airtemp) %>%
  spread(met, temp) %>%
  ggplot(aes(date, max - min)) +
  geom_line(aes(col = source_instrument), alpha = 0.5) +
  geom_smooth(col = "black") +
  facet_wrap(~type)

# annual DTR comparison
cowplot::plot_grid(
  subset(sdl_out3, met != "avg", select = c(date, yr, source_instrument, met, finaltemp, adjusted_airtemp)) %>%
    gather(type, temp, finaltemp, adjusted_airtemp) %>%
    spread(met, temp) %>%
    mutate(DTR = max-min) %>%
    group_by(yr) %>%
    ggplot(aes(yr, DTR)) +
    stat_summary() +
    geom_smooth(col = "black") +
    labs(subtitle = "sdl") +
    facet_wrap(~type),
  
  cowplot::plot_grid(
    # compare with d1 kittel
    ggplot(group_by(subset(d1_kittel, year >= 1981), year), aes(year, DTR)) +
      stat_summary() +
      geom_smooth() +
      labs(subtitle = "d1 kittel"),
    
    ggplot(group_by(subset(c1_kittel, year >= 1981), year), aes(year, DTR)) +
      stat_summary() +
      geom_smooth() +
      labs(subtitle = "c1 kittel"),
    nrow = 1
  ),
  nrow = 2
)

# look at means
subset(sdl_out3, yr >1981, select = c(date, yr, source_instrument, met, adjusted_airtemp)) %>%
  group_by(yr, met) %>%
  ggplot() +
  stat_summary(aes(x = yr, y = adjusted_airtemp)) +
  geom_smooth(aes(x = yr, y = adjusted_airtemp), method = "lm") +
  facet_wrap(~met)

# look at all stations
cowplot::plot_grid(
  
  subset(d1_kittel, year >= 1981, select = c(date, year, max_temp, min_temp, mean_temp)) %>%
    gather(met, val, max_temp:mean_temp) %>%
    group_by(year, met) %>%
    ggplot() +
    stat_summary(aes(x = year, y = val)) +
    geom_smooth(aes(x = year, y = val)) +
    labs(subtitle = "d1 kittel annual means") +
    facet_wrap(~met),
  
  subset(sdl_out3, yr >1981, select = c(date, yr, source_instrument, met, adjusted_airtemp)) %>%
    mutate(met = gsub("avg", "mean", met)) %>%
    group_by(yr, met) %>%
    ggplot() +
    stat_summary(aes(x = yr, y = adjusted_airtemp)) +
    geom_smooth(aes(x = yr, y = adjusted_airtemp)) +
    labs(subtitle = "sdl annual means") +
    facet_wrap(~met),
  
  subset(c1_kittel, year >= 1981, select = c(date, year, max_temp, min_temp, mean_temp)) %>%
    gather(met, val, max_temp:mean_temp) %>%
    group_by(year, met) %>%
    ggplot() +
    stat_summary(aes(x = year, y = val)) +
    geom_smooth(aes(x = year, y = val)) +
    #geom_vline(aes(xintercept = 1999), lty = 2) +
    labs(subtitle = "c1 kittel annual means") +
    facet_wrap(~met),
  
  mutate(ameriforest, yr = year(date)) %>%
    subset(yr > 1998) %>%
    group_by(yr, met) %>%
    ggplot() +
    stat_summary(aes(x = yr, y = airtemp)) +
    geom_smooth(aes(x = yr, y = airtemp)) +
    scale_x_continuous(limits = c(1980, 2020)) +
    labs(subtitle = "c1 ameriflux annual means") +
    facet_wrap(~met),
  nrow = 4
)

# summer means for sdl
plot_grid(
subset(sdl_out3, yr >1981 & mon %in% 6:9, select = c(date, yr, source_instrument, met, adjusted_airtemp)) %>%
  mutate(met = gsub("avg", "mean", met)) %>%
  group_by(yr, met) %>%
  ggplot() +
  stat_summary(aes(x = yr, y = adjusted_airtemp)) +
  geom_smooth(aes(x = yr, y = adjusted_airtemp)) +
  facet_wrap(~met),
subset(sdl_out3, yr >1981 & mon %in% 6:9, select = c(date, yr, source_instrument, met,finaltemp)) %>%
  mutate(met = gsub("avg", "mean", met)) %>%
  group_by(yr, met) %>%
  ggplot() +
  stat_summary(aes(x = yr, y = finaltemp)) +
  geom_smooth(aes(x = yr, y = finaltemp)) +
  facet_wrap(~met)
)

# summer maxes for sdl
plot_grid(
  subset(sdl_out3, yr >1981 & mon %in% 6:9, select = c(date, yr, source_instrument, met, adjusted_airtemp)) %>%
    mutate(met = gsub("avg", "mean", met)) %>%
    group_by(yr, met) %>%
    ggplot() +
    stat_summary(aes(x = yr, y = adjusted_airtemp), fun= max) +
    #geom_smooth(aes(x = yr, y = max(adjusted_airtemp, na.rm = T))) +
    facet_wrap(~met),
  subset(sdl_out3, yr >1981 & mon %in% 6:9, select = c(date, yr, source_instrument, met,finaltemp)) %>%
    mutate(met = gsub("avg", "mean", met)) %>%
    group_by(yr, met) %>%
    ggplot() +
    stat_summary(aes(x = yr, y = finaltemp), fun = max) +
    #geom_smooth(aes(x = yr, y = max(finaltemp, na.rm = T))) +
    facet_wrap(~met)
)

# annual mins for sdl
plot_grid(
  subset(sdl_out3, yr >1981, select = c(date, yr, source_instrument, met, adjusted_airtemp)) %>%
    mutate(met = gsub("avg", "mean", met)) %>%
    group_by(yr, met) %>%
    ggplot() +
    stat_summary(aes(x = yr, y = adjusted_airtemp), fun= min) +
    #geom_smooth(aes(x = yr, y = max(adjusted_airtemp, na.rm = T))) +
    facet_wrap(~met),
  subset(sdl_out3, yr >1981, select = c(date, yr, source_instrument, met,finaltemp)) %>%
    mutate(met = gsub("avg", "mean", met)) %>%
    group_by(yr, met) %>%
    ggplot() +
    stat_summary(aes(x = yr, y = finaltemp), fun = min) +
    #geom_smooth(aes(x = yr, y = max(finaltemp, na.rm = T))) +
    facet_wrap(~met)
)

# reality check that tmean value is between tmax and tmin
tmean_check <- dplyr::select(sdl_out3, date, yr, source_instrument, met, adjusted_airtemp) %>%
  spread(met, adjusted_airtemp) %>%
  mutate(mathmean = (max + min)/2,
         diff_avgmath = avg-mathmean,
         avgmax = avg > max,
         avgmin = avg < min,
         # make sure all max > min
         maxmin = max > min) %>%
  subset(avgmax | avgmin)

# for dates that got flagged, use the math mean instead
sdl_out3$draft_qa_note <- NA
sdl_out4 <- sdl_out3
for(d in tmean_check$date){
  #find row
  temprow <- with(sdl_out4, which(date == d & met == "avg"))
  stopifnot(sdl_out4$adjusted_airtemp[temprow] == with(tmean_check, avg[date == d]))
  sdl_out4$adjusted_airtemp[temprow] <- with(tmean_check, mathmean[date == d])
  if(tmean_check$avgmax[tmean_check$date == d]){
    sdl_out4$draft_qa_note[temprow] <- "use math mean, derived mean from knb-lter-nwt.168 > daily tmax"
  }else{
    sdl_out4$draft_qa_note[temprow] <- "use math mean, derived mean from knb-lter-nwt.168 < daily tmin"
  }
}
# note math mean used for 1981-1989
sdl_out4$draft_qa_note[which(sdl_out4$yr < 1990 & sdl_out4$met == "avg")] <- "use math mean of tmax, tmin"
#rename final temp
names(sdl_out4)[names(sdl_out4)== "finaltemp"] <- "qa_airtemp"



# -- ATTACH RAW DATA -----
# read in sdl dats prepped for qa (already in appropriate format)
qadat <- paste0(datpath, "data/prep_data/ready_for_qa/")
sdl_chart_tidy <- read_csv(paste0(qadat, "sdl_chart_tidy.csv"))
sdlcr_tidy <- read.csv(paste0(qadat, "sdlcr_tidy.csv"))
sdl_hmps_tidy <- read_csv(paste0(qadat, "sdl_hmps_tidy.csv"))

chart_prep <- subset(sdl_chart_tidy) %>%
  mutate(source_instrument = "sdl chart",
         metric = gsub("airtemp_", "",metric),
         hmp1_raw_airtemp = NA, hmp2_raw_airtemp = NA, hmp3_raw_airtemp = NA) %>%
  rename(raw_airtemp = temp, raw_flag = flag)

cr_prep <- subset(sdlcr_tidy, !metric == "DTR", select = -flag) %>%
  mutate(metric = gsub("airtemp_", "", metric),
         hmp1_raw_airtemp = NA, hmp2_raw_airtemp = NA, hmp3_raw_airtemp = NA) %>%
  rename(raw_airtemp = temp, source_instrument = logger)
  
hmp_prep <- subset(sdl_hmps_tidy) %>%
  # bring in flag and temp
  gather(thing, val, temp:flag) %>%
  mutate(thing = gsub("te", "airte", thing),
         metric = gsub("airtemp_", "", metric),
         logger = "cr1000 hmps") %>%
  unite(thing, sensor, thing, sep = "_raw_") %>%
  spread(thing, val) %>%
  mutate_at(.vars = names(.)[grepl("_airtemp", names(.))], as.numeric) %>%
  mutate(raw_airtemp = NA) %>%
  rename(source_instrument = logger)
  

raw_stack <- subset(chart_prep, metric != "DTR", select = -c(local_site, data_source, raw_flag)) %>%
  rbind(cr_prep[names(.)], hmp_prep[names(.)])

sdl_out_stitched <- left_join(rename(sdl_out4, metric = met), raw_stack)

# check all dates there
with(sdl_out_stitched, lapply(split(date, metric), length))

# final look
ggplot(rename(sdl_out_stitched, met = metric), aes(date, col = source_instrument)) +
  # compare with c1 ameriflux
  geom_line(data = mutate(ameriforest, met= gsub("mean", "avg", met)), aes(date, airtemp), col = "grey50", lwd = 1) +
  geom_line(aes(y = adjusted_airtemp), alpha = 0.5) +
  facet_wrap(~met, nrow = 3) # good enough

# -- quick adjust hi tmax 1990s ----
# compare adjust_temps to raw -- not  hmp period bc of data structure and already addressed
subset(sdl_out_stitched, source_instrument != "cr1000 hmps", select = c(date, source_instrument, metric, qa_airtemp, adjusted_airtemp, raw_airtemp)) %>%
  gather(temp, val, qa_airtemp:raw_airtemp) %>%
  mutate(source_instrument = factor(source_instrument, levels = c("sdl chart", "cr21x", "cr23x", "cr1000"))) %>%
  ggplot(aes(date, val, col = temp)) +
  geom_line(alpha = 0.5) +
  facet_grid(metric ~source_instrument, scales = "free", space = "free_x") 

# closer look
# tmax
subset(sdl_out_stitched, source_instrument != "cr1000 hmps", select = c(date, source_instrument, metric, qa_airtemp, adjusted_airtemp, raw_airtemp)) %>%
  gather(temp, val, qa_airtemp:raw_airtemp) %>%
  subset(metric == "max") %>%
  mutate(source_instrument = factor(source_instrument, levels = c("sdl chart", "cr21x", "cr23x", "cr1000")),
         temp = factor(temp, levels = c("raw_airtemp", "qa_airtemp", "adjusted_airtemp"))) %>%
  ggplot(aes(date, val, col = temp)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~source_instrument+temp, scales = "free_x",nrow = 4) 

# tmean
subset(sdl_out_stitched, source_instrument != "cr1000 hmps", select = c(date, source_instrument, metric, qa_airtemp, adjusted_airtemp, raw_airtemp)) %>%
  gather(temp, val, qa_airtemp:raw_airtemp) %>%
  subset(metric == "avg") %>%
  mutate(source_instrument = factor(source_instrument, levels = c("sdl chart", "cr21x", "cr23x", "cr1000")),
         temp = factor(temp, levels = c("raw_airtemp", "qa_airtemp", "adjusted_airtemp"))) %>%
  ggplot(aes(date, val, col = temp)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~source_instrument+temp, scales = "free_x",nrow = 4) 

# tmin
subset(sdl_out_stitched, source_instrument != "cr1000 hmps", select = c(date, source_instrument, metric, qa_airtemp, adjusted_airtemp, raw_airtemp)) %>%
  gather(temp, val, qa_airtemp:raw_airtemp) %>%
  subset(metric == "min") %>%
  mutate(source_instrument = factor(source_instrument, levels = c("sdl chart", "cr21x", "cr23x", "cr1000")),
         temp = factor(temp, levels = c("raw_airtemp", "qa_airtemp", "adjusted_airtemp"))) %>%
  ggplot(aes(date, val, col = temp)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~source_instrument+temp, scales = "free_x",nrow = 4) 

# last qa checks
sdl_out_devs <- flag_deviations(rename(sdl_out_stitched, met = metric), metric = "adjusted_airtemp", groupvars =c("mon", "met", "source_instrument"))
sdl_out_spikes <- flag_spikes(rename(sdl_out_stitched, met = metric), metric = "adjusted_airtemp", groupvars =c("mon", "met", "source_instrument"), abs_limit = 35, returnworking = T)

# make a note for tmin 2011-02-01 since was global tmin and lowered from adjustment
tmin_date <- with(sdl_out_stitched, which(adjusted_airtemp == min(adjusted_airtemp)))
sdl_out_stitched[tmin_date, ] # correct
tmin_note <- "sdl global tmin, use qa_temp rather than adjusted on this date; d1 tmin was -33.77 logger, -35 chart; 10th coldest day since 1950s"
sdl_out_stitched$adjusted_airtemp[tmin_date] <- sdl_out_stitched$qa_airtemp[tmin_date]
sdl_out_stitched$adjustment_to_2018hmps[tmin_date] <- 0
sdl_out_stitched$draft_qa_note[tmin_date] <- tmin_note

# look at tmax in outgoing dataset vs. sdl chart in late 80s, early 90s (june 9 1990 has a high value)
# > just eyeballing, looks like logger tmax was 5 degrees warmer than chart for a period; gets close again by late june 1990

compare_sdl <- subset(sdl_chart_qa2018, yr %in% c(1989:1994)) %>%
  mutate(met = gsub("airtemp_", "", met),
         date = as.Date(date)) %>%
  left_join(subset(sdl_out_stitched, select = c(date, metric, source_instrument, infill_method, qa_airtemp, adjusted_airtemp, raw_airtemp, adjustment_to_2018hmps)), by = c("date", "met" = "metric")) %>%
  mutate(diff_chartadjust = sdl_qatemp - qa_airtemp)

ggplot(compare_sdl, aes(date, diff_chartadjust)) +
  geom_hline(aes(yintercept = 0), lty = 2, color = "grey50") +
  geom_line() +
  geom_point(aes(col = infill_method), alpha = 0.5) +
  facet_grid(met~yr, scales = "free_x") +
  scale_x_date(date_labels = "%b")

# focus on 1990
ggplot(subset(compare_sdl, yr %in% c(1989:1990)), aes(date, diff_chartadjust)) +
  geom_hline(aes(yintercept = 0), lty = 2, color = "grey50") +
  geom_line() +
  geom_point(aes(col = infill_method), alpha = 0.5) +
  facet_grid(met~., scales = "free_x") +
  scale_x_date(date_labels = "%b")

ggplot(subset(compare_sdl, yr ==1990 & mon %in% 1:7), aes(date, diff_chartadjust)) +
  geom_hline(aes(yintercept = 0), lty = 2, color = "grey50") +
  geom_line() +
  geom_point(aes(col = infill_method), alpha = 0.5) +
  geom_smooth(col = "grey30") +
  scale_x_date(breaks = "2 weeks", date_labels = "%b %d") +
  facet_grid(met~., scales = "free_x")

# for quick adjustment, make mean diff for window equal to window where it seems like diff more in range (this is subjective, but placeholder for now)
# pick apr 1 - jun 15 as window to adjust
# look at mean diff for cr21x source period jan 1 - feb and jun 16- jul 16
adjust90 <- subset(compare_sdl, yr == 1990 & date <= "1990-07-16") #%>%
adjust90$rolldiff[adjust90$met == "max"] <-c(NA, NA, c(zoo::rollmean(adjust90$diff_chartadjust[adjust90$met == "max"], k = 3)))
adjust90$rolldiff[adjust90$met == "min"] <-c(NA, NA, c(zoo::rollmean(adjust90$diff_chartadjust[adjust90$met == "min"], k = 3)))
ggplot(adjust90, aes(doy, diff_chartadjust)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,200,10)) +
  #scale_x_date(breaks= "1 week", date_labels = "%m-%d") +
  facet_wrap(~met, nrow = 2) # choose doy 80 - 165

ggplot(adjust90, aes(doy, diff_chartadjust)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_vline(aes(xintercept = 80), col = "red") +
  geom_vline(aes(xintercept = 165), col = "red") +
  geom_line() +
  scale_x_continuous(breaks = seq(0,200,10)) +
  #scale_x_date(breaks= "1 week", date_labels = "%m-%d") +
  facet_wrap(~met, nrow = 2)

# look at 3-d rolling mean
ggplot(adjust90, aes(doy, rolldiff)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_vline(aes(xintercept = 80), col = "red") +
  geom_vline(aes(xintercept = 165), col = "red") +
  geom_line() +
  scale_x_continuous(breaks = seq(0,200,10)) +
  #scale_x_date(breaks= "1 week", date_labels = "%m-%d") +
  facet_wrap(~met, nrow = 2)

adjust_means <- mutate(adjust90, section = ifelse(doy %in% 80:165, "to_adjust", "reference")) %>%
  group_by(section, met) %>%
  summarise(meanroll = mean(rolldiff, na.rm = T),
            sdroll = sd(rolldiff, na.rm = T),
            nobs_roll = length(rolldiff[!is.na(rolldiff)]),
            meandiff = mean(diff_chartadjust, na.rm = T),
            sddiff = sd(diff_chartadjust, na.rm = T),
            nobs_diff = length(diff_chartadjust[!is.na(diff_chartadjust)])) %>%
  ungroup()#

# what would it look like to adjust?
adjust90 <- mutate(adjust90, section = ifelse(doy %in% 80:165, "to_adjust", "reference"))
adjust90 %>%
  mutate(newtemp = ifelse(section == "to_adjust" & met == "max", adjusted_airtemp - 3, adjusted_airtemp)) %>%
  ggplot(aes(doy, newtemp)) +
  #geom_hline(aes(yintercept = 0), lty = 2) +
  geom_vline(aes(xintercept = 80), col = "grey", lty = 2) +
  geom_vline(aes(xintercept = 165), col = "grey", lty = 2) +
  geom_line(aes(doy, adjusted_airtemp), col ="purple", alpha = 0.5, lwd = 1) +
  geom_line(aes(doy, sdl_qatemp), col ="red", alpha = 0.75) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,200,10)) +
  labs(subtitle = "red = chart, purple = sdl stitch (as is), black = new adjustment") +
  #scale_x_date(breaks= "1 week", date_labels = "%m-%d") +
  facet_wrap(~met, nrow = 2)

# go through stitched dataset and adjust: qa_airtmep, adjusted_airtemp, and draft_qa_note for tmax relevant dates
# subtracting 3 degrees from tmax only
for(d in as.character(unique(adjust90$date[adjust90$section == "to_adjust"]))){
  d <- as.Date(d)
  # id row in stitched dataset
  temprow <- with(sdl_out_stitched, which(date == d & metric == "max"))
  tempref <- with(adjust90, which(date == d & met == "max"))
  # be sure it's correct row
  stopifnot(sdl_out_stitched$qa_airtemp[temprow] == adjust90$qa_airtemp[tempref])
  sdl_out_stitched$qa_airtemp[temprow] <- (sdl_out_stitched$qa_airtemp[temprow] -3)
  sdl_out_stitched$adjusted_airtemp[temprow] <- (sdl_out_stitched$qa_airtemp[temprow] + sdl_out_stitched$adjustment_to_2018hmps[temprow])
  sdl_out_stitched$draft_qa_note[temprow] <- "logger drift from sdl chart in tmax, adjust qa_airtemp -3 based on mean diff before and after drift period"
}



# -- recheck early record homogenization -----
# before adjusting c1 logger tmax, mean diff in homog above suggested sdl chart tmax was rel lower, but could be closer now than cr21x logger tmax lowered a bit
# recheck DTR too
sdl_early_tmax <- subset(sdl_out_stitched, metric == "max", select = c("date", "source_instrument", "qa_airtemp")) %>%
  left_join(rename(instrument_dates, start_date = date)) %>%
  #complete years only
  subset(year(date) >= 1982)
sdl_early_tmax_decomposed <- list2DF(decompose(ts(sdl_early_tmax$qa_airtemp, start = c(1982,1), frequency = 365.24)))
names(sdl_early_tmax_decomposed) <- paste0("sdl_", names(sdl_early_tmax_decomposed))
sdl_early_tmax2 <- cbind(sdl_early_tmax, sdl_early_tmax_decomposed[c("sdl_seasonal", "sdl_trend")]) %>%
  #join c1 and d1 kittel decomposed
  left_join(subset(c1k_decomposed, met == "max", select = c("date", "c1k_airtemp", "c1k_trend"))) %>%
  left_join(subset(d1k_decomposed, met == "max", select = c("date", "d1k_airtemp", "d1k_trend"))) %>%
  mutate(diff_sdld1k = sdl_trend - d1k_trend,
         diff_sdlc1k = sdl_trend - c1k_trend,
         diff_d1kc1k = d1k_trend - c1k_trend)

sdl_early_tmax2 %>%
  dplyr::select(names(.)[grep("date|source|trend", names(.))]) %>%
  gather(trend, val, sdl_trend:ncol(.)) %>%
  ggplot(aes(date, val, col = trend)) +
  geom_vline(aes(xintercept = start_date), lty = 2) +
  geom_line()

sdl_early_tmax2 %>%
  dplyr::select(names(.)[grep("date|source|diff", names(.))]) %>%
  gather(trend, val, diff_sdld1k:ncol(.)) %>%
  ggplot(aes(date, val, col = trend)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_vline(aes(xintercept = start_date), lty = 2) +
  geom_line()

# calculate means diffs
earlymeans <- sdl_early_tmax2 %>%
  dplyr::select(names(.)[grep("date|start|source|diff", names(.))]) %>%
  gather(trend, val, diff_sdld1k:ncol(.)) %>%
  group_by(source_instrument, start_date, trend) %>%
  subset(!is.na(val)) %>%
  summarise(meandiff = mean(val),
            nobs = length(val)) %>%
  arrange(start_date, trend)

sdl_early_tmax2 %>%
  subset(year(date)<2019) %>%
  dplyr::select(names(.)[grep("date|source|qa_air|c1k_air|d1k_air", names(.))]) %>%
  gather(trend, val, grep("airt", names(.))) %>%
  ggplot(aes(date, val, col = trend)) +
  geom_vline(aes(xintercept = start_date), lty = 2) +
  geom_line()

# don't adjust anything in chart period to be conservative -- not sure if cr21x logger is drifting since c1-d1 mean diff relationship stays constant
copy <- sdl_out_stitched
sdl_out_stitched <- sdl_out_stitched %>%
  mutate(adjustment_to_2018hmps = ifelse(source_instrument == "sdl chart" & metric == "max", 0, adjustment_to_2018hmps),
         adjusted_airtemp = ifelse(source_instrument == "sdl chart" & metric == "max", qa_airtemp, adjusted_airtemp))

# -- final review temps -----
ggplot(sdl_out_stitched, aes(date, adjusted_airtemp, col = source_instrument)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_line() +
  geom_smooth(color = "black", method = "lm") +
  facet_wrap(~factor(metric, levels = c("max", "avg", "min")),
             nrow = 3)

subset(sdl_out_stitched, metric == "max") %>%
  mutate(decade = ifelse(yr < 1990, 1980, ifelse(yr < 2000, 1990, ifelse(yr < 2010, 2000, 2010)))) %>%
  ggplot(aes(date, adjusted_airtemp)) +
  geom_line() +
  geom_point(aes(col = date %in% adjust90$date[adjust90$section == "to_adjust"])) +
  theme(legend.position = "none") +
  facet_wrap(~decade, scales = "free_x", nrow = 4)

ggplot(subset(sdl_out_stitched, metric == "max" & yr %in% c(1989:1991)), aes(doy, adjusted_airtemp, col = factor(yr))) +
  geom_line(aes(group= yr), alpha = 0.5, lwd = 1) +
  geom_point(data = subset(sdl_out_stitched, date %in% adjust90$date[adjust90$section == "to_adjust"] & metric == "max")) +
  scale_color_viridis_d()

# look at winter/summer
ggplot(sdl_out_stitched, aes(doy, adjusted_airtemp)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_line(aes(col = yr), alpha = 0.5) +
  scale_color_viridis_c() +
  #geom_smooth(color = "black", method = "lm") +
  facet_wrap(~factor(metric, levels = c("max", "avg", "min")),
             nrow = 3)

# look at winter/summer
sdl_out_stitched %>%
  mutate(period = ifelse(mon %in% 6:9, "summer", "winter")) %>%
  subset(yr > 1981) %>% # complete yrs
  group_by(yr, period) %>%
  ggplot(aes(yr, adjusted_airtemp, col = yr >= 1987)) + # col = yr >= 1987
  geom_hline(aes(yintercept = 0), lty = 2) +
  stat_summary() +
  geom_smooth(method = "lm") +
  labs(subtitle = "sdl annual temp trends, summer  (jun-sep) vs. winter (oct-may)") +
  facet_grid(period~factor(metric, levels = c("max", "avg", "min")), scales = "free_y")

#4 seasons
sdl_out_stitched %>%
  mutate(period = ifelse(mon %in% 6:8, "summer", ifelse(mon %in% 9:11, "fall",
                                                        ifelse(mon %in% c(12,1,2), "winter", "spring"))),
         period = factor(period, levels = c("spring", "summer", "fall", "winter"))) %>%
  
  subset(yr > 1981) %>% # complete yrs
  group_by(yr, period) %>%
  ggplot(aes(yr, adjusted_airtemp)) + # col = yr >= 1987
  geom_hline(aes(yintercept = 0), lty = 2) +
  stat_summary() +
  geom_smooth() +
  labs(subtitle = "sdl annual temp trends, by season (sp = MAM, sum = JJA, f = SON, w = DJF)") +
  facet_grid(factor(metric, levels = c("max", "avg", "min"))~period, scales = "free_y")



# -- WRITE OUT -----
# write out temp dat for sce/renewal drafts dec 2020
# clean up:
# > avg => mean (so plots in correct order)
# > add note for tmean for 1990-2017 derived from jennings et al hrly infilled dataset

sdl_out_stitched2 <- cbind(LTER_site = "NWT", local_site = "sdl", sdl_out_stitched) %>%
  mutate(draft_qa_note = ifelse(yr %in% c(1990:2017) & metric == "avg" & is.na(draft_qa_note),
                                "derived daily mean from hourly qa'd, gap-filled knb-lter-nwt.168",
                                draft_qa_note),
         metric = gsub("avg", "mean", metric))

str(sdl_out_stitched2)
summary(sdl_out_stitched2)
View(subset(sdl_out_stitched2, !infilled & (raw_airtemp != qa_airtemp) &is.na(draft_qa_note) & metric != "mean"))
# add note in data_qa for temp values shifted 1 day in cr21x series (in case anyone wonders why raw != qa)
shift_dates <- with(sdl_out_stitched2, which(!infilled & (raw_airtemp != qa_airtemp) & is.na(draft_qa_note) & metric != "mean"))
View(sdl_out_stitched2[shift_dates,]) # appropriate rows
sdl_out_stitched2$draft_qa_note[shift_dates] <- "qa_airtemp shifted 1 day from raw_airtemp during spatial congruence check"
# check unique draft flags
unique(sdl_out_stitched2$draft_qa_note)
#clean up math mean note
sdl_out_stitched2$draft_qa_note <- gsub("use math mean, derived mean", "use math mean of tmax, tmin; derived daily mean", sdl_out_stitched2$draft_qa_note)
sdl_out_stitched2 <- arrange(sdl_out_stitched2, date, metric)

write_csv(sdl_out_stitched2, paste0(datpath, "data/sdl_temp_1981-2020_draft.csv"))
