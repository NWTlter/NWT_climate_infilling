# compile niwot snotel data

# script purpose:
# read in and tidy snotel temp data, earliest historic to present date


# -- SETUP ----
library(dplyr)
library(lubridate)
options(stringsAsFactors = F)

# read in C1 Snotel (Station 663)
tmin <- read_csv("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/663:CO:SNTL|id=%22%22|name/POR_BEGIN,POR_END/TMIN::value", skip = 57)
tmax <- read_csv("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/663:CO:SNTL|id=%22%22|name/POR_BEGIN,POR_END/TMAX::value", skip = 57)
ppt <- read_csv("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport,metric/daily/start_of_period/663:CO:SNTL%7Cid=%22%22%7Cname/-14456,0/PREC::value,PRCP::value,PRCP::qcFlag,PRCP::qaFlag,PRCPSA::value,PRCPSA::qcFlag,PRCPSA::qaFlag?fitToScreen=false", 
                skip = 64)


# read in Snotel University Camp (near Albion)
uc_ppt <- read_csv("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport,metric/daily/start_of_period/838:CO:SNTL%7Cid=%22%22%7Cname/-21488,0/PREC::value,PREC::qcFlag,PREC::qaFlag,PRCP::value,PRCP::qcFlag,PRCP::qaFlag,PRCPSA::value,PRCPSA::qcFlag,PRCPSA::qaFlag?fitToScreen=false",
                   skip = 66)


# -- PREP DATA ------
# temp
# stack, rename temp col, and convert degrees from F to C
colnames(tmin)[2] <- "tempF"
tmin$met = "airtemp_min"
colnames(tmax)[2] <- "tempF"
tmax$met = "airtemp_max"

snotel_temp <- data.frame(rbind(tmin, tmax)) %>%
  mutate(date = as.Date(Date, format = "%Y-%m-%d"),
         tempC = round((tempF - 32) * (5/9),2),
         local_site = "Niwot Snotel (663)",
         # add yr, month and day of yr,
         yr = year(date),
         mon = month(date),
         doy = yday(date),
         LTER_site = "NWT") %>%
  dplyr::select(LTER_site, local_site, date, yr, mon, doy, met, tempC)


# -- PRECIP -----
# just need to rename cols to something shorter
snotel_ppt <- ppt %>%
  rename(ppt_mm = `Niwot (663) Precipitation Increment (mm)`,
         ppt_qcflag = `Niwot (663) Precipitation Increment QC Flag`,
         ppt_qaflag = `Niwot (663) Precipitation Increment QA Flag`) %>%
  mutate(date = as.Date(Date, format = "%Y-%m-%d"),
         local_site = "Niwot Snotel (663)",
         # add yr, month and day of yr,
         yr = year(date),
         mon = month(date),
         doy = yday(date),
         LTER_site = "NWT") %>%
  dplyr::select(LTER_site, local_site, date, yr, mon, doy,ppt_mm, ppt_qcflag, ppt_qaflag)


# just need to rename cols to something shorter
uc_ppt2 <- uc_ppt[,c(1, 5:7)]
colnames(uc_ppt2) <- c("date", "ppt_mm", "ppt_qcflag", "ppt_qaflag")
uc_ppt2 <- uc_ppt2 %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         local_site = "University Camp Snotel (838)",
         # add yr, month and day of yr,
         yr = year(date),
         mon = month(date),
         doy = yday(date),
         LTER_site = "NWT") %>%
  dplyr::select(LTER_site, local_site, date, yr, mon, doy,ppt_mm, ppt_qcflag, ppt_qaflag)

  
# -- FINISHING -----
# write out
datout <- "c1_d1_sdl_clim/homogenize_climdat/data/prep_data/"
write_csv(snotel_temp, paste0(datout,"nwt_snotel_temp.csv"))
write_csv(snotel_ppt,  paste0(datout,"nwt_snotel_ppt.csv"))
write_csv(uc_ppt2,  paste0(datout,"nwt_univcamp_ppt.csv"))
