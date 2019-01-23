#' ---
#' title: Compare precip and temp from chart, Emily's data, and Jennings et al.
#' author: CTW
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' 
#' **Goal**: Figure out how best to infill precip and temp for years 2015, 2016, 2017

#+ script setup, echo = FALSE, message = FALSE, warning = FALSE
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
# set working directory to ctw climate infilling subfolder on NWT_climate_infilling github repo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load needed libraries
library(tidyverse)
library(lubridate)

# read in NWT climate dataset used in NSF proposal
# > read in from EDI data portal
sdl_charttemp <- read_csv("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.413.10&entityid=afc48b6fab15649c9f91a9367debd2e0",
                       trim_ws=TRUE, na = c("", "NA", ".", "NaN")) %>%
  mutate(source="chart")

sdl_loggerdat <- read_csv("http://niwot.colorado.edu/data_csvs/sdlcr23x-cr1000.daily.ml.data.csv",
                             na = c("", "NA", "Nan"))
# keep only temp data from logger for this
sdl_loggerdat <- sdl_loggerdat[,1:12] # should keep "LTER_site" through "flag_airtemp_avg 

# read in precip chart data from EDI
# > read in from EDI data portal
sdl_chartpcp <- read_csv("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.416.8&entityid=c1c73287f2e10039c0d89dba8fd44993",
                         trim_ws = TRUE,
                         na = c("", "NA", "NaN")) %>%
  mutate(source = "chart")

# read in C1 and D1 to compare for quick infilling
# all chart data, all from NWT data portal
c1_charttemp <- read_csv("http://niwot.colorado.edu/data_csvs/c-1tdayv.ml.data.csv",
                   na = c("", "NA", "NaN"))
c1_charttemp$source <- "chart"

c1_chartpcp <- read_csv("http://niwot.colorado.edu/data_csvs/c-1pdayv.ml.data.csv",
                     na = c("", "NA", "NaN"))
c1_chartpcp$source <- "chart"

d1_charttemp <- read_csv("http://niwot.colorado.edu/data_csvs/d-1tdayv.ml.data.csv",
                   na = c("", "NA", "NaN"))
d1_charttemp$source <- "chart"

d1_chartpcp <- read_csv("http://niwot.colorado.edu/data_csvs/d-1pdayv.ml.data.csv",
                     na = c("", "NA", "NaN"))
d1_chartpcp$source <- "chart"

# temp and precip data EF used for NSF proposal climate dataset
temp_pcp_ef <- read_csv("./Saddle_precip_temp_formoisturedeficit.csv")
temp_pcp_ef$source <- "NSF proposal data"

# Keith Jennings et al. infilled *hourly* data
Jennings_infill <- read_csv("http://niwot.colorado.edu/data_csvs/infillclimate_c1_d1_sdl.hourly.kj.data.csv")
Jennings_infill$source <-  "Jennings et al."

# summarise Jennings et al. data to daily totals (ppt) or averages (temp)
Jennings_summarized <- Jennings_infill %>%
  group_by(LTER_site, local_site, year, date,jday, source) %>%
  summarise(ppt_tot = sum(ppt_tot),
            airtemp_max = mean(airtemp_avg),
            airtemp_min = min(airtemp_avg),
            airtemp_avg = mean(airtemp_avg))

# remove hourly infilled dataset bc 65 MB and eats up memory
rm(Jennings_infill)

#' ### Time ranges of each dataset..
#' 

#+ compare raw datasets
# combine datasets first

# what are structures of each dataset?
glimpse(sdl_chartpcp)
glimpse(c1_chartpcp)
glimpse(d1_chartpcp)
glimpse(sdl_charttemp)
glimpse(c1_charttemp)
glimpse(d1_charttemp)
glimpse(sdl_loggerdat) # only air temperature values kept; wind speed, rel. humid, soil moisture, etc. dropped (not needed for this)
glimpse(Jennings_summarized)
glimpse(temp_pcp_ef)

# correct d1 chart precip ppt_tot name to match others
names(d1_chartpcp)[4] <- "ppt_tot"

pcp_all <- rbind(d1_chartpcp, sdl_chartpcp, c1_chartpcp)
Jennings_summarized$qdays <- 1 #reflects daily sum
Jennings_summarized$flag_ppt_tot <- NA
pcp_all <-  rbind(pcp_all,
                  Jennings_summarized[c("LTER_site", "local_site", "date", "ppt_tot", "qdays", "flag_ppt_tot", "source")])
# give Emily's dataset the same fields as other chart data
temp_pcp_ef <- temp_pcp_ef %>%
  mutate(qdays = 1,
         flag_ppt_tot = NA,
         date = as.Date(paste(Year,Month,Day, sep = "-")),
         LTER_site = "NWT",
         local_site = "sdl") %>%
  dplyr::rename(ppt_tot = PCP)

pcp_all <- rbind(pcp_all, 
                 temp_pcp_ef[c("LTER_site", "local_site", "date", "ppt_tot", "qdays", "flag_ppt_tot", "source")])

# combine temperature data
temp_all <- rbind(d1_charttemp, sdl_charttemp, c1_charttemp)

# give Jennings dataset same fields as chart data
Jennings_summarized$flag_airtemp_max <- NA # lose flag data when summarise hourly values
Jennings_summarized$flag_airtemp_min <- NA # lose flag data when summarise hourly values

# add in Jennings et al. summarized daily temp
temp_all <- rbind(temp_all,
                  Jennings_summarized[c("LTER_site", "local_site", "date", 
                                        "airtemp_max", "flag_airtemp_max", "airtemp_min", "flag_airtemp_min",
                                        "airtemp_avg", "source")])
# add in Emily's dataset
# give Emily's dataset the same fields as other chart data
temp_pcp_ef <- temp_pcp_ef %>%
  mutate(date = as.Date(paste(Year,Month,Day, sep = "-")),
         LTER_site = "NWT",
         local_site = "sdl",
         flag_airtemp_max = NA,
         flag_airtemp_min = NA,
         airtemp_avg = NA) %>%
  dplyr::rename(airtemp_max = TMAX,
                airtemp_min = TMIN)

# add in Emily's data
temp_all <- rbind(temp_all, 
                  temp_pcp_ef[c("LTER_site", "local_site", "date", "airtemp_max", "flag_airtemp_max",
                                "airtemp_min", "flag_airtemp_min", "airtemp_avg", "source")])

# add in logger data
sdl_loggerdat$source <- "logger"

temp_all <- rbind(temp_all, sdl_loggerdat[c("LTER_site", "local_site", "date", 
                                          "airtemp_max", "flag_airtemp_max", "airtemp_min", "flag_airtemp_min",
                                          "airtemp_avg", "source")])
# -----------------------
# plot to compare!

# raw data - precip
ggplot(pcp_all) +
  geom_point(aes(date, ppt_tot, fill = source), pch=21, alpha = 0.6) +
  scale_fill_brewer(palette ="Set2") +
  theme_bw() +
  facet_grid(source~local_site)

# data subsetted to period of NSF proposal and corrected for blowing snow
# snow correction: if Jun-Aug, do nothing; otherwise multiple ppt * 0.39
pcp_all %>%
  mutate(yr = year(date),
         mon = month(date),
         ppt_tot = ifelse(source == "chart" & 
                            #local_site == "sdl" & 
                            mon %in% c(1:5,9:12), ppt_tot*0.39, ppt_tot)) %>%
  filter(yr >= 1982 & local_site == "sdl") %>%
  ggplot() +
  geom_point(aes(date, ppt_tot, fill = source), pch=21, alpha = 0.6) +
  scale_fill_brewer(palette ="Set2") +
  theme_bw() +
  facet_grid(source~local_site)

pcp_all %>%
  mutate(yr = year(date),
         mon = month(date),
         ppt_tot = ifelse(source == "chart" & 
                            local_site == "sdl" & 
                            mon %in% c(1:5,9:12), ppt_tot*0.39, ppt_tot)) %>%
  filter(yr >= 1982 & local_site != "sdl") %>%
  ggplot() +
  geom_point(aes(date, ppt_tot, fill = source), pch=21, alpha = 0.6) +
  scale_fill_brewer(palette ="Set2") +
  theme_bw() +
  facet_grid(source~local_site)

pcp_all %>%
  mutate(yr = year(date),
         mon = month(date),
         ppt_tot = ifelse(source == "chart" & 
                            local_site == "sdl" & 
                            mon %in% c(1:5,9:12), ppt_tot*0.39, ppt_tot)) %>%
  filter(yr %in% 2007:2008 & local_site != "sdl") %>%
  ggplot() +
  geom_point(aes(date, ppt_tot, col = source), alpha = 0.6) +
  scale_color_brewer(palette ="Set2") +
  theme_bw() +
  facet_grid(.~local_site)

pcp_all <- pcp_all %>%
  mutate(yr = year(date),
         mon = month(date),
         doy = yday(date),
         ppt_tot2 = ifelse(source == "chart" & 
                            local_site == "sdl" & 
                            mon %in% c(1:5,9:12), ppt_tot*0.39, ppt_tot))

# compare all, busy plot
ggplot() +
  geom_point(data = subset(pcp_all,yr %in% 2011:2014 & local_site == "sdl" & source == "Jennings et al."),
             aes(doy, ppt_tot2, col=source), size=3, alpha = 0.4) +
  geom_point(data = subset(pcp_all,yr %in% 2011:2014 & local_site == "sdl" & source == "chart"),
             aes(doy, ppt_tot2, col=source), size=2, alpha = 0.55) +
  geom_point(data = subset(pcp_all,yr %in% 2011:2014 & local_site == "sdl" & source == "NSF proposal data"),
             aes(doy, ppt_tot2, col=source), size =1, alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  ggtitle("Comparison of daily precip for select years, colored by data source") +
  theme_bw() + 
  facet_wrap(~yr, scales = "free_y")

# show how data diverges on qualifying days > 1
qdays_dates <- pcp_all$date[pcp_all$qdays >1]

ggplot(data = subset(pcp_all,yr %in% 2006:2014 & local_site == "sdl" & date %in% qdays_dates)) +
  geom_point(aes(doy, ppt_tot2, col=source), size=1.5, alpha = 0.75) +
  # geom_point(data = subset(pcp_all,yr %in% 2011:2014 & local_site == "sdl" & source == "chart"),
  #            aes(doy, ppt_tot2, col=source), size=2, alpha = 0.55) +
  # geom_point(data = subset(pcp_all,yr %in% 2011:2014 & local_site == "sdl" & source == "NSF proposal data"),
  #            aes(doy, ppt_tot2, col=source), size =1, alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Comparison of daily precip for select years where qualifying days >1, colored by data source",
       y = "Daily total precipitation (mm)",
       x = "Day of year") +
  theme_bw() + 
  facet_wrap(~yr, scales = "free_y")

pcp_all %>%
  filter(local_site == "sdl") %>%
  left_join(d1_chartpcp, by = "date") %>%
  #filter(source.x != "chart") %>%
  ggplot(aes(ppt_tot2, ppt_tot.y, col=year(date)>2014)) +
  geom_point(alpha=0.6) +
  #geom_smooth(method="lm") +
  #geom_abline(aes(intercept = 0, slope = 1), col="red") +
  ggtitle("Saddle daily precip (x) by D1 chart data (y), arrayed by source") +
  facet_grid(.~source.x)

pcp_all %>%
  filter(local_site == "sdl") %>%
  left_join(c1_chartpcp, by = "date") %>%
  #filter(source.x != "chart") %>%
  ggplot(aes(ppt_tot2, ppt_tot.y, col=year(date)>2014)) +
  geom_point(alpha=0.6) +
  #geom_smooth(method="lm") +
  #geom_abline(aes(intercept = 0, slope = 1), col="red") +
  ggtitle("Saddle daily precip (x) by C1 chart data (y), arrayed by source") +
  facet_grid(.~source.x)




#------------------
# temp comparisons

# raw data - temp
ggplot(temp_all) +
  geom_point(aes(date, airtemp_max, fill = source), pch=21, alpha = 0.6) +
  scale_fill_brewer(palette ="Set2") +
  theme_bw() +
  facet_grid(source~local_site)

# raw data - subsetted to period covered in NSF proposal
ggplot(subset(temp_all, year(date)>1981)) +
  geom_point(aes(date, airtemp_max, fill = source), pch=21, alpha = 0.6) +
  scale_fill_brewer(palette ="Set2") +
  #geom_smooth(aes(date, airtemp_max), method="lm") +
  theme_bw() +
  facet_grid(source~local_site)

ggplot(subset(temp_all, year(date) %in% 2008:2011 & local_site == "sdl")) +
  geom_hline(aes(yintercept = 0), col="black") +
  geom_point(aes(date, airtemp_max, fill = source), pch=21, alpha = 0.6) +
  scale_fill_brewer(palette ="Set2") +
  geom_smooth(aes(date, airtemp_max), method="lm") +
  theme_bw() +
  facet_grid(source~local_site)

ggplot(subset(temp_all, year(date) %in% 2008:2011 & local_site == "sdl")) +
  geom_hline(aes(yintercept = 0), col="black") +
  geom_point(aes(date, airtemp_min, fill = source), pch=21, alpha = 0.6) +
  scale_fill_brewer(palette ="Set2") +
  geom_smooth(aes(date, airtemp_min), method="lm") +
  theme_bw() +
  facet_grid(source~local_site)

ggplot(subset(temp_all, year(date) %in% 2008:2017 & local_site == "sdl" & source %in% c("chart", "logger"))) +
  geom_hline(aes(yintercept = 0), col="black") +
  geom_point(aes(date, airtemp_avg, col = source), alpha = 0.3) +
  scale_color_brewer(palette ="Dark2") +
  #geom_smooth(aes(date, airtemp_min), method="lm") +
  theme_bw()
  #facet_grid(.~source)

temp_all %>%
  filter(local_site == "sdl") %>%
  left_join(sdl_charttemp, by = "date") %>%
  filter(source.x != "chart") %>%
  ggplot(aes(airtemp_max.x, airtemp_max.y)) +
  geom_point(alpha=0.6) +
  geom_abline(aes(intercept = 0, slope = 1), col="red") +
  ggtitle("Saddle max temp [non-chart source] (x) by Saddle chart data (y), arrayed by source on x") +
  facet_grid(.~source.x)

temp_all %>%
  filter(local_site == "sdl") %>%
  left_join(sdl_charttemp, by = "date") %>%
  filter(source.x != "chart") %>%
  ggplot(aes(airtemp_avg.x, airtemp_avg.y)) +
  geom_point(alpha=0.6) +
  geom_abline(aes(intercept = 0, slope = 1), col="red") +
  ggtitle("Saddle max temp [non-chart source] (x) by Saddle chart data (y), arrayed by source on x") +
  facet_grid(.~source.x)

temp_all %>%
  filter(local_site == "sdl") %>%
  left_join(d1_charttemp, by = "date") %>%
  #filter(source.x != "chart") %>%
  ggplot(aes(airtemp_max.x, airtemp_max.y)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm") +
  #geom_abline(aes(intercept = 0, slope = 1), col="red") +
  ggtitle("Saddle max temp (x) by D1 chart data (y), arrayed by source") +
  facet_grid(.~source.x)

temp_all %>%
  filter(local_site == "sdl") %>%
  left_join(c1_charttemp, by = "date") %>%
  #filter(source.x != "chart") %>%
  ggplot(aes(airtemp_max.x, airtemp_max.y)) +
  geom_point(alpha = 0.6) +
  geom_abline(aes(intercept = 0, slope = 1), col="red") +
  ggtitle("Saddle max temp (x) by C1 chart data (y), arrayed by source") +
  facet_grid(.~source.x)

# where don't NSF data and chart data overlap in temp?
# answer: nowhere if chart data available.. infilling data is the problem
temp_all%>%
  filter(source %in% c("NSF proposal data", "chart") &
           local_site == "sdl" &
           year(date) %in% 2010:2014) %>%
  ggplot() +
  geom_point(aes(date, airtemp_max, col =source), alpha = 0.5) +
  theme_bw()

# compare Jennings infill at C1 and D1 to raw chart data
Jennings_summarized %>%
  filter(local_site=="d1") %>%
  left_join(d1_charttemp, by =c("LTER_site", "local_site", "date")) %>%
  filter(!is.na(flag_airtemp_max.y)) %>%
  ggplot(aes(airtemp_max.x, airtemp_max.y)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope =1), col="red")

# ------------------
# Notes on infilling temp:
# #
# Niwot data files used in compilation of summary:
#   Temperature data:
#   1 Climate report data            --->
#   2 Chart recorder temperature data    --->
#   3 Dp211 data                --->
#   4 Cr21x data
# (Method flag 1) regression methodology:
#   Regression was performed using the chart recorder data at the missing site and the adjacent site using a 14 day window prior to and after the missing day. The regression equation was applied to the value at the known site to determine the value for infilling the missing site. An r2 value above 0.6 was considered acceptable. If the r2 was below 0.6 or missing, the standard deviation method was used to fill in the missing value (see Method Flag 2).
# #
# (Method flag 2) standard deviation methodology:
#   Ratios of known site values and standard deviations were used in order to determine the replacement values. The standard deviation was taken for the day (mm-dd) in question throughout the climate record at both the known site and the unknown site and applied as follows:
#   #
#   #     known site date value (yyyy-mm-dd)                                                   (x)
#   # ______________________________________________  :  __________________________________________________
#   # std. dev. of the day (mm-dd) at the known site                        std. dev. of the day (mm-dd) at the unknown site
#   #
#   Temperature data adjacency hierarchy (if value missing from charts):
#   (1) Saddle dp211 data logger
# (2) Saddle cr21x data logger
# (3) D1 chart recorder


## write for loop to use method 1 for infilling years 2015, 2016 off of logger data
max_temp_missing <- sdl_charttemp$date[is.na(sdl_charttemp$airtemp_max)]
min_temp_missing <- sdl_charttemp$date[is.na(sdl_charttemp$airtemp_min)] 
avg_temp_missing <- sdl_charttemp$date[is.na(sdl_charttemp$airtemp_avg)]

# join sdl chart and sdl logger data for regressions
regress_sdl_temp <- sdl_charttemp[c("date", "airtemp_max", "airtemp_min", "airtemp_avg")] %>%
  dplyr::rename(chart_max = airtemp_max,
                chart_min = airtemp_min,
                chart_avg = airtemp_avg) %>%
  left_join(sdl_loggerdat[c("LTER_site", "local_site", "date", "logger", "year", "jday", 
                            "airtemp_max", "airtemp_min", "airtemp_avg")]) %>%
  dplyr::rename(logger_max = airtemp_max,
                logger_min = airtemp_min,
                logger_avg = airtemp_avg) %>%
  filter(date > as.Date("2009-12-15"))

#initialize df
avg_temp_infill <- data.frame(missing_date= avg_temp_missing[avg_temp_missing <= max(sdl_loggerdat$date)], 
                              nobs_x = NA, nobs_y = NA, x = NA, tmean_infill = NA, adj_r2 = NA, pval = NA )

#avg_temp_missing_test <- as.Date(avg_temp_missing[1:10])

for(i in avg_temp_missing){
  temp_row <- which(avg_temp_missing == i)
  begin_date <- i - 14
  end_date <-  i + 14
  temp_df = subset(regress_sdl_temp,date >= begin_date & date <=end_date)
  avg_temp_infill[temp_row, "nobs_x"] <- sum(!is.na(temp_df$chart_avg))
  avg_temp_infill[temp_row, "nobs_y"] <- sum(!is.na(temp_df$logger_avg))
  #avg_temp_infill[temp_row, "missing_date"] <- temp_df$date[temp_df$date == i]
  temp_model <- lm(chart_avg ~ logger_avg, data=temp_df)
  temp_x <- temp_df$logger_avg[temp_df$date == i]
  avg_temp_infill[temp_row, "x"] <- temp_x
  avg_temp_infill[temp_row, "tmean_infill"] <- temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x)
  avg_temp_infill[temp_row, "adj_r2"] <- summary(temp_model)[[9]] 
  avg_temp_infill[temp_row, "pval"] <- summary(temp_model)$coefficients[8] 
}
 

# compare average values to Jennings infill
mean_infill <- avg_temp_infill %>%
  left_join(Jennings_summarized)



#initialize df
max_temp_infill <- data.frame(missing_date= max_temp_missing[max_temp_missing <= max(sdl_loggerdat$date)], 
                              nobs_x = NA, nobs_y = NA, x = NA, tmax_infill = NA, adj_r2 = NA, pval = NA )

#avg_temp_missing_test <- as.Date(avg_temp_missing[1:10])

for(i in max_temp_missing){
  temp_row <- which(max_temp_missing == i)
  begin_date <- i - 30
  end_date <-  i + 30
  temp_df = subset(regress_sdl_temp,date >= begin_date & date <=end_date)
  max_temp_infill[temp_row, "nobs_x"] <- sum(!is.na(temp_df$chart_avg))
  max_temp_infill[temp_row, "nobs_y"] <- sum(!is.na(temp_df$logger_avg))
  #max_temp_infill[temp_row, "missing_date"] <- temp_df$date[temp_df$date == i]
  temp_model <- lm(chart_max ~ logger_max, data=temp_df)
  temp_x <- temp_df$logger_max[temp_df$date == i]
  max_temp_infill[temp_row, "x"] <- temp_x
  max_temp_infill[temp_row, "tmax_infill"] <- temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x)
  max_temp_infill[temp_row, "adj_r2"] <- summary(temp_model)[[9]] 
  max_temp_infill[temp_row, "pval"] <- summary(temp_model)$coefficients[8] 
}


# visual test
infill_test <-sdl_charttemp %>%
  filter(year(date) > 2009) %>%
  left_join(max_temp_infill, by = c("date" = "missing_date")) %>%
  mutate(max_infill = ifelse(is.na(airtemp_max) & adj_r2 >= 0.6 & pval < 0.05, tmax_infill, airtemp_max)) %>%
  left_join(temp_pcp_ef[c("LTER_site", "local_site", "date", "airtemp_max")], by = c("LTER_site", "local_site", "date"))
  

ggplot(subset(infill_test, !is.na(tmax_infill) & tmax_infill > -30)) +
  geom_point(aes(date, tmax_infill), col="dodgerblue", size=2, alpha = 0.7) +
  geom_point(aes(date, airtemp_max.y), col = "goldenrod", size = 1, alpha = 0.7) +
  labs(title = "Infill with logger data, tmax comparison (blue = infill, yellow = NSF proposal data)",
       subtitle = "Infilled via moving 30 days window regression using Saddle logger data",
       y = "Daily max temp (C)", x = "Date (where chart data missing)")  +
  theme_bw()

dogs <- infill_test %>%
  left_join(Jennings_summarized[c("LTER_site", "local_site", "date", "airtemp_max")]) %>%
  filter(!is.na(max_infill)) %>%
  ggplot() +
  #geom_point(aes(airtemp_max, tmax_infill), col = "navyblue", alpha = 0.5) +
  #geom_point(aes(airtemp_max.y, tmax_infill), col = "orchid", alpha = 0.5) +
  #geom_abline(aes(intercept=0, slope= 1)) +
  #geom_point(aes(date, airtemp_max), col = "chocolate1", alpha = 0.5) +
  geom_histogram(aes(tmax_infill - airtemp_max.y, na.rm=TRUE)) +
  theme_bw()
  


### try with D1

# join sdl chart and sdl logger data for regressions
regress_sdl_temp_d1 <- sdl_charttemp[c("date", "airtemp_max", "airtemp_min", "airtemp_avg")] %>%
  dplyr::rename(chart_max = airtemp_max,
                chart_min = airtemp_min,
                chart_avg = airtemp_avg) %>%
  left_join(d1_charttemp[c("LTER_site", "date", 
                            "airtemp_max", "airtemp_min", "airtemp_avg")]) %>%
  dplyr::rename(d1_max = airtemp_max,
                d1_min = airtemp_min,
                d1_avg = airtemp_avg) %>%
  filter(date > as.Date("2009-12-15"))

#initialize df
avg_temp_infill_d1 <- data.frame(missing_date= avg_temp_missing[avg_temp_missing <= max(sdl_loggerdat$date)], 
                              nobs_x = NA, nobs_y = NA, x = NA, tmean_infill = NA, adj_r2 = NA, pval = NA )

#avg_temp_missing_test <- as.Date(avg_temp_missing[1:10])

for(i in avg_temp_missing){
  temp_row <- which(avg_temp_missing == i)
  begin_date <- i - 30
  end_date <-  i + 30
  temp_df = subset(regress_sdl_temp_d1,date >= begin_date & date <=end_date)
  avg_temp_infill[temp_row, "nobs_x"] <- sum(!is.na(temp_df$chart_avg))
  avg_temp_infill[temp_row, "nobs_y"] <- sum(!is.na(temp_df$logger_avg))
  #avg_temp_infill[temp_row, "missing_date"] <- temp_df$date[temp_df$date == i]
  temp_model <- lm(chart_avg ~ logger_avg, data=temp_df)
  temp_x <- temp_df$logger_avg[temp_df$date == i]
  avg_temp_infill[temp_row, "x"] <- temp_x
  avg_temp_infill[temp_row, "tmean_infill"] <- temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x)
  avg_temp_infill[temp_row, "adj_r2"] <- summary(temp_model)[[9]] 
  avg_temp_infill[temp_row, "pval"] <- summary(temp_model)$coefficients[8] 
}


#initialize df
max_temp_infill_d1 <- data.frame(missing_date= max_temp_missing[max_temp_missing <= max(sdl_loggerdat$date)], 
                              nobs_x = NA, nobs_y = NA, x = NA, tmax_infill = NA, adj_r2 = NA, pval = NA )

#avg_temp_missing_test <- as.Date(avg_temp_missing[1:10])

for(i in max_temp_missing){
  temp_row <- which(max_temp_missing == i)
  begin_date <- i - 30
  end_date <-  i + 30
  temp_df = subset(regress_sdl_temp_d1,date >= begin_date & date <=end_date)
  max_temp_infill_d1[temp_row, "nobs_x"] <- sum(!is.na(temp_df$chart_max))
  max_temp_infill_d1[temp_row, "nobs_y"] <- sum(!is.na(temp_df$d1_max))
  #max_temp_infill[temp_row, "missing_date"] <- temp_df$date[temp_df$date == i]
  temp_model <- lm(chart_max ~ d1_max, data=temp_df)
  temp_x <- temp_df$d1_max[temp_df$date == i]
  max_temp_infill_d1[temp_row, "x"] <- temp_x
  max_temp_infill_d1[temp_row, "tmax_infill"] <- temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x)
  max_temp_infill_d1[temp_row, "adj_r2"] <- summary(temp_model)[[9]] 
  max_temp_infill_d1[temp_row, "pval"] <- summary(temp_model)$coefficients[8] 
}


# visual test
infill_test_d1 <-sdl_charttemp %>%
  filter(year(date) > 2009) %>%
  left_join(max_temp_infill_d1, by = c("date" = "missing_date")) %>%
  mutate(max_infill = ifelse(is.na(airtemp_max) & adj_r2 >= 0.6 & pval < 0.05, tmax_infill, airtemp_max)) %>%
  left_join(temp_pcp_ef[c("LTER_site", "local_site", "date", "airtemp_max")], by = c("LTER_site", "local_site", "date"))

ggplot(subset(infill_test_d1, !is.na(tmax_infill))) +
  geom_point(aes(date, tmax_infill), col="dodgerblue", size=2) +
  geom_point(aes(date, airtemp_max.y), col = "goldenrod", size = 1) +
  ggtitle("infill with D1 chart data, tmax comparison (blue = infill, yellow = NSF data)")  +
  theme_bw()

boxplot(infill_test_d1$tmax_infill - infill_test_d1$airtemp_max.y, na.rm=T)
boxplot(infill_test$tmax_infill - infill_test$airtemp_max.y, na.rm=T)


dogs2 <- infill_test_d1 %>%
  left_join(Jennings_summarized[c("LTER_site", "local_site", "date", "airtemp_max")]) %>%
  filter(!is.na(max_infill)) %>%
  ggplot() +
  #geom_point(aes(airtemp_max, tmax_infill), col = "navyblue", alpha = 0.5) +
  #geom_point(aes(airtemp_max.y, tmax_infill), col = "orchid", alpha = 0.5) +
  #geom_abline(aes(intercept=0, slope= 1)) +
  #geom_point(aes(date, airtemp_max), col = "chocolate1", alpha = 0.5) +
  geom_histogram(aes(tmax_infill - airtemp_max, na.rm=TRUE)) +
  theme_bw()


## try via method 2 
# tmax with saddle
glimpse(regress_sdl_temp)

ratio_sdl_temp <- regress_sdl_temp %>%
  group_by(jday) %>%
  mutate(sd_doy_logger = sd(logger_max, na.rm=T),
         sd_doy_chart = sd(chart_max, na.rm=T)) %>%
  ungroup() %>%
         mutate(ratio_log = logger_max/sd_doy_logger,
                tmax_infill = ratio_log * sd_doy_chart)

plot(ratio_sdl_temp$chart_max, ratio_sdl_temp$tmax_infill)
abline(a=0, b=1, col="red")

ratio_sdl_temp_d1 <- regress_sdl_temp_d1 %>%
  mutate(doy=yday(date),
         mon = month(date)) %>%
  #filter(!mon %in% c(11,12,1,2)) %>%
  group_by(doy) %>%
  mutate(sd_doy_d1 = sd(d1_max, na.rm=T),
         sd_doy_chart = sd(chart_max, na.rm=T)) %>%
  ungroup(doy) %>%
  mutate(ratio_d1 = d1_max/sd_doy_d1,
         tmax_infill = ratio_d1 * sd_doy_chart)

plot(ratio_sdl_temp_d1$chart_max, ratio_sdl_temp_d1$tmax_infill)
abline(a=0, b=1, col="red")
abline(a=0, b=0, col="blue")
abline(v=0, b=0, col="blue")

### Estimating GL4 ice off..
gl4_phytos <- read_csv("http://pasta.lternet.edu/package/data/eml/knb-lter-nwt/161/1/9b37e40db2134f4f163cdac258d54560")
iceoff <- read_csv("http://niwot.colorado.edu/data_csvs/glakeice.nc.data.csv",
                   trim_ws = TRUE,
                   na = c("NaN", NA, "NA ", " ", ""))

# After eyeballing dates, will use first date of GL4 sample - 1 week (1 week prior to first sample date)

## Which stations have higher predictive power for Saddle chart data?
# try by monthly regressions, all data available, all years

# pair raw Saddle data with logger data
sdl_chartlogger <- sdl_charttemp %>%
  left_join(sdl_loggerdat, by = c("LTER_site", "local_site", "date")) %>%
  mutate(yr = year(date),
         mon = month(date),
         doy = yday(date)) %>%
  filter(date >= as.Date("2000-07-01") & date <= as.Date("2016-12-31") & # limit to complete months available in logger
           logger == "cr1000") # consider most recent logger only (in case any influence in values from switching data loggers)

lm_chartlogger_jan <- lm(airtemp_max.x ~ airtemp_max.y, data= subset(sdl_chartlogger, mon == 1))
lm_chartlogger_jun <- lm(airtemp_max.x ~ airtemp_max.y, data= subset(sdl_chartlogger, mon == 6))
lm_chartlogger_sep <- lm(airtemp_max.x ~ airtemp_max.y, data= subset(sdl_chartlogger, mon == 9))
