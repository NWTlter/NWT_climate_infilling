#' Format Temp and PPT datasets for EDI
#' Author: CTW
#' Change log:
#'  Miles 2023 - Commented out what doesn't apply for this year, changed pathing


################################################################################
# Set up
################################################################################
rm(list=ls())

library(tidyverse)
library(lubridate)
source("daily_met/R/fetch_data_functions.R")
options(stringsAsFactors = FALSE)

#Path to where data have been written to
datpath <- "~/OneDrive - UCB-O365/NWT_Infilling_2023/data/"
################################################################################
# Read in data 
################################################################################

c1_temp <- read.csv("daily_met/Infilling_2022/c1/c1_chart_infilled_v1.csv") |> 
  dplyr::mutate(LTER_site = "NWT") |> dplyr::select(-local_site.1) |> 
  dplyr::filter(yr > 2021)
c1_ppt <- readRDS(paste0(datpath,"infill/c1PPT_infilled_draft.rds")) |> 
  dplyr::filter(year > 2021)

# pull in all sites for saddle (temp and precip)
sitesppt <- readRDS(paste0(datpath, "qc/siteinfoPPT_qc.rds"))
sitestemp <- readRDS(paste0(datpath, "qc/siteinfoTEMP_qc.rds"))

# We need to read in all the stacked data so we have info in the environment for
# describing which source stations were available for infilling target stations.
# (e.g., GHCND not available for HMPs and no GHCND data during this period)
qcdats <- list.files(paste0(datpath, "qc"), full.names = T)
ghcndtemp <- readRDS(qcdats[grepl("cndTEMP_re", qcdats)])
ghcndppt <- readRDS(qcdats[grepl("cndPPT_qc", qcdats)])
fluxtemp <- readRDS(qcdats[grepl("fluxTEMP_re", qcdats)])
fluxppt <- readRDS(qcdats[grepl("fluxPPT_qc", qcdats)])
snoteltemp <- readRDS(qcdats[grepl("telTEMP_re", qcdats)])
snotelppt <- readRDS(qcdats[grepl("telPPT_qc", qcdats)])
nwtlogtemp <- readRDS(qcdats[grepl("loggerTEMP_re", qcdats)])
allppt <- readRDS(paste0(datpath, "qc/siteinfoPPT_qc.rds"))

# Read in TK datasets - mostly so we have a starting point for formatting final dat
tkd1_temp <- getTabular(185, datanum = 1)
tkd1_ppt <- getTabular(186)

# Read in Keith Jennings dataset for naming conventions as well
jennings <- getTabular(168)

# Read in currently posted temp version for aligning
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/185/2/3a2e8db4a0211a097f28917a6f5d4c95" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

posted_temp <-read.csv(infile1) |> 
  dplyr::mutate(
    max_temp = as.numeric(max_temp),
    min_temp = as.numeric(min_temp),
    DTR = as.numeric(DTR)
  )
unlink(infile1)

# Read in currently posted ppt version for aligning
inUrl1  <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.184.6&entityid=be2f9ce2465dddeec1e42538cbf626d8" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

posted_ppt <-read.csv(infile1)
unlink(infile1)

################################################################################
# Pretty Datasets!
################################################################################

#Note from CTW:
# make sure colnames match naming convention for C1 and D1 gap-filled
# pval = pvalue, r2 = rsquared
# > going to leave airtemp_max/min/avg as is and dtr lowcase 
# (not following C1 and D1 colnames, will suggest to sarah we adjust those)
# > not going to put "num_obs...regression_equation" because will be too long 
# for hmps (end it at "..regression")

# names(tkd1_temp)
sapply(tkd1_temp[grepl("LTER|local|flag|source", names(tkd1_temp))], 
       function(x) sort(unique(x)))
# names(jennings)
sapply(jennings[grepl("LTER|local|flag", names(jennings))], 
       function(x) sort(unique(x)))

# keith's flag explanations:
# The flagging structure has the format qcXinY, 
# where X indicates the quality control protocol applied, if any, 
# and Y indicates the infilling protocol applied, if any

# X can have the following values: 
## 0 (observation passed all quality control checks); 
## 1 (observation removed because it falls outside of the maximum/minimum 
# threshold for that variable); 
## 2 (observation removed because it exceeded the rate of change threshold); 
## 3 (observation removed because the sensor was stuck); 
## 4 (variable missing from raw data due to power outage, missing files, 
# datalogger issue, etc.).

# Y can have the following values: 
## 0 (no infilling applied); 
## 1 (gap = 1 h, infilled by using linear interpolation between previous and 
# following observations);
## 2 (gap > 1 h and <= 24, infilled by taking average value of observation 24 
# hours before and after missing value); 
## 3 (gap > 24 and <= 72 h, infilled with forecasted and backcasted ARIMA model); 
## 4 (infilled using multiple linear regression of observations from both o
# ther met stations); 
## 5 (infilled using linear regression from closest met station); 
## 6 (infilled using linear regression from farthest met station); 
## 7 (infilled using climatic mean for that month and 3 h time block); 
## ns (quality flags not specified) [ctw note: for 2014 onwards when kehoe 
# infilled the data]


# -- 1. PRETTY c1 TEMP --------------------------------------------------------
# Init a new dataframe to pretty up
c1_temp_pretty <- c1_temp

# Change names of pval, r2, and year columns
names(c1_temp_pretty) <- gsub("pval$|pval(?=_)" , "pvalue", 
                               names(c1_temp), perl = T)
names(c1_temp_pretty) <- gsub("r2" , "rsquared", names(c1_temp_pretty))
names(c1_temp_pretty)[names(c1_temp_pretty) == "yr"] <- "year"

# i guess put year before date to match c1 and d1 infilled? and in raw 
# datasets and tk infilled, max, min and avg is the order
# order temps: max, min, avg

#Note from Miles: no adjustment columns because
# only HMPS here. Adjustment column contains the value
# applied to max, min, avg from non-hmp sources to 
#cross calibrate them to extend HMPs. 

c1_temp_pretty <- c1_temp_pretty |> 
  dplyr::select(LTER_site, local_site, year, date,everything(), -mon, -doy)

# -- Populate flag columns based on criteria in methods of TK/CTW packages
c1_temp_pretty <- c1_temp_pretty %>%
  mutate(
    # Flagging based on method for flag_1
    flag_1 = case_when(
      is.na(method) ~ "A",
      grepl("multi", method) & airtemp_avg_pvalue <= 0.05 ~ "B",
      grepl("multi", method) & airtemp_avg_pvalue > 0.05 ~ "C",
      grepl("moving", method) & airtemp_avg_pvalue <= 0.05 ~ "D",
      grepl("moving", method) & airtemp_avg_pvalue > 0.05 ~ "E",
      TRUE ~ NA_character_
    ),
    
    # Flagging for tmax/tmin derivation for flag_2
    flag_2 = case_when(
      is.na(method) ~ "A",
      airtemp_max_method == "predicted" & airtemp_min_method == "predicted" ~ "B",
      grepl("failed", airtemp_max_method) ~ "B*",
      grepl("adjusted$", airtemp_max_method) & airtemp_min_method == "raw" ~ "C",
      airtemp_max_method == "raw" & grepl("dtr adjusted$", airtemp_min_method) ~ "D",
      TRUE ~ NA_character_
    ),
    
    # Flagging for dtr pval for flag_3
    flag_3 = case_when(
      is.na(method) ~ "A",
      dtr_pvalue <= 0.05 ~ "B",
      dtr_pvalue > 0.05 ~ "C",
      TRUE ~ NA_character_
    )
  ) |> mutate(date = lubridate::date(date))

#Inspect Flagging
c1_temp_pretty |>
  ggplot()+
    geom_line(aes(date, airtemp_avg))+
    geom_point(aes(date, airtemp_avg), alpha = 0.5)+
    geom_point(data = c1_temp_pretty |> subset(flag_1 != 'A'),
               aes(date, airtemp_avg, shape = flag_1), color = 'red') +
  xlim(c(lubridate::date("2020-01-01"), lubridate::date("2020-12-31")))

# -- Inspect regression flags & metadata ----
# revisit c1 flags again..
sapply(c1_temp_pretty[grepl("flag", names(c1_temp_pretty))], 
       function(x) sort(unique(x)))

sapply(c1_temp_pretty[grepl("method", names(c1_temp_pretty))], 
       function(x) sort(unique(x)))


# QAQC notes should be intelligible and standardized
c1_temp_flagging <- subset(c1_temp) %>%
  subset(select = grepl("date|yr|mon|flag", names(.)))

# -- clean up station names ----
# source station names should be clear
sapply(c1_temp_pretty[grepl("source", names(c1_temp_pretty))], 
       function(x) sort(unique(x))) 

# create station LUT, iterate through each source col and replace as did for qc notes
# > naming convention on EDI for loggers is SITE data logger CR[x]
# > D1 and C1 gap-filled have lowcase loggers, but will follow how it's shown on EDI
# Snotel has: Name (number)
# GHCDN is last 5 digits of station ID
unique(c1_temp_pretty$source_station)

temp_station_LUT <- distinct(sitestemp[c("station_id", "station_name", "local_site", "source")]) %>%
  mutate(
    pretty_name = dplyr::case_match(source,
                                    'nwt lter' ~ gsub("_", " ", station_id) |> toupper(),
                                    'snotel' ~ paste0(station_name, " (", station_id, ")" ),
                                    .default = station_id),
    # little adjustments for nwt
    pretty_name = gsub("CHART", "chart", pretty_name),
    pretty_name = gsub(" CR", " logger CR", pretty_name),
    pretty_name = gsub("HMP ", "HMP-", pretty_name))


# Iterate through cr qc_flag notes and replace

source_columns <- names(c1_temp_pretty)[grepl("source", names(c1_temp_pretty))]
for(n in source_columns){
  for(i in 1:nrow(temp_station_LUT)){
    c1_temp_pretty[[n]][(c1_temp_pretty[[n]] == temp_station_LUT$local_site[i]) & 
                           !is.na(c1_temp_pretty[[n]])] <- temp_station_LUT$pretty_name[i]
  }
}

# check names
sapply(c1_temp_pretty[grepl("source", names(c1_temp_pretty))], unique) # looks good

# Calculate DTR
c1_temp_pretty <- c1_temp_pretty |> 
  dplyr::mutate(
    DTR = ifelse(!is.na(dtr_infill), dtr_infill,
                 airtemp_max - airtemp_min)
  )


# -- finalize dataset -----
# check cols and arrangement: remove mon, doy, pay attention to letter casing
str(c1_temp_pretty) # colnames look okay

# final check of ranges and char values
sapply(c1_temp_pretty[grepl("LTER|local|logg|flag|source",names(c1_temp_pretty))],
       function(x) sort(unique(x)))

# Rename some variables to match what is posted_temp on EDI now.
c1_temp_pretty <- c1_temp_pretty |> 
  mutate(
    local_site = local_site |> toupper(),
    infill_QAnote = NA,
    Tmax_QAflag = NA,
    Tmin_QAflag = NA
  ) |> 
  dplyr::rename(
    mean_temp = airtemp_avg,
    max_temp = airtemp_max,
    min_temp = airtemp_min,
    num_obs_in_t_mean_regression_equation = airtemp_avg_n.obs,
    num_obs_in_TDTR_regression_equation = dtr_n.obs,
    source_station = source.station,
    raw_Tmean = raw_airtemp_avg,
    raw_Tmax = raw_airtemp_max,
    raw_Tmin = raw_airtemp_min
  )

names(c1_temp_pretty) <- gsub('airtemp_avg', 't_mean', names(c1_temp_pretty))
names(c1_temp_pretty) <- gsub('dtr_', 'TDTR_', names(c1_temp_pretty))
names(c1_temp_pretty) <- gsub('_equation', '_regression_equation', names(c1_temp_pretty))


name_alignment <- data.frame(posted_temp = c(names(posted_temp), 
                                        rep(NA, length(names(c1_temp_pretty)) - 
                                              length(names(posted_temp)))),
                             new = names(c1_temp_pretty))
c1_temp_pretty <- c1_temp_pretty |> 
  dplyr::select(LTER_site, local_site, year, date, max_temp, mean_temp, min_temp,
                DTR, flag_1, flag_2, flag_3, source_station, t_mean_pvalue, t_mean_rsquared,
                num_obs_in_t_mean_regression_regression_equation, t_mean_regression_equation,
                TDTR_pvalue, TDTR_rsquared, num_obs_in_TDTR_regression_regression_equation,
                TDTR_regression_equation, infill_QAnote, Tmax_QAflag, Tmin_QAflag,
                raw_Tmean, raw_Tmax, raw_Tmin)

name_alignment <- data.frame(posted_temp = c(names(posted_temp), NA),
                             new = names(c1_temp_pretty))

# be sure no duplicate dates and all dates accounted for
summary(seq.Date(min(c1_temp_pretty$date), max(c1_temp_pretty$date), 1) %in% c1_temp_pretty$date)
summary(duplicated(c1_temp_pretty$date)) #looks good!


# -- write out pretty temp -----
# for NWT long term datasets:
# make NA NaN instead (tell sce NaN in this case = no value, not data are missing)
# write out csv with utf-8 and \r\n eol (end of line)
# (need to change this in code settings, check it looks good after write out)
write.csv(c1_temp_pretty, paste0(datpath, "publish/c1_daily_airtemp_gapfilled_ongoing.csv"),
          row.names = F, na = "NaN", quote = T)

# -- 2. PRETTY c1 PPT -----
# pretty all dates, but only write out summer months pre-oct 1988 (show in fig to meagan and sarah that c1 ppt )

# check flags/notes:
sapply(c1_ppt[grepl("note|flag", names(c1_ppt))], unique)

# regression flags already fine, just standardize qc notes

# notes I have:
# 1) qdays flag/note, 2) infill qc note/flag, 3) qc and post-infill qc note
# keep all three cols rather than condense (in case any data user interested), just clean up notes

# -- clean up QC notes -----
c1_ppt_pretty<- c1_ppt %>%
  # change ppt to precip
  mutate_at(.vars = names(.)[grepl("_qc", names(.))], function(x) gsub("ppt", "precip", x)) %>%
  # capitalize first letter of note
  mutate_at(.vars = names(.)[grepl("_qc", names(.))], 
            function(x) ifelse(!is.na(x), 
                               # if not present, paste capitalized first letter, then the rest of string
                               paste0(casefold(substr(x, 1,1), upper = T), substr(x, 2, nchar(x))),
                               # else leave as is
                               x)
  )

# check notes again
sapply(c1_ppt_pretty[grepl("note|flag", names(c1_ppt_pretty))], unique)

# clarify notes:
# gapfilling notes
c1_ppt_pretty$infill_qcnote[grepl("^JM flag",c1_ppt_pretty$infill_qcnote)] <- "J. Morse flagged raw value as questionable, remove raw and gapfill"

# comparative qc notes:
# check notes now
sapply(c1_ppt_pretty[grepl("note|flag", names(c1_ppt_pretty))], unique) #ok


# -- clean up station names -----
unique(c1_ppt_pretty$source_station) # this matches tim's station name convention.. don't need to change, esp if SCE is going to recycle this for C1, D1
# but.. I prefer clarity. treat ghcnd and snotel stations the same as did for temp

# iterate through to partial match value (should work) and assign pretty name from temp_LUT
ppt_sites_LUT <- distinct(c1_ppt_pretty,source_station) %>%
  mutate(source = ifelse(grepl("Belfort", source_station), "nwt lter", 
                         ifelse(grepl("US-NR", source_station), "ameriflux",
                                ifelse(grepl("[0-9]{5}", source_station), "ghcnd", "snotel")))) %>%
  mutate(pretty_name = ifelse(grepl("nwt lt|ameri", source), source_station, ""))
ppt_sites_LUT <- subset(temp_station_LUT, grepl("snot|ghc", source), select = -local_site) %>%
  distinct() %>%
  mutate(keycol = ifelse(source == "snotel", station_name,
                         # drop beginning of ghcnd name otherwise to get last 5 chars
                         gsub("US.000", "", station_id)))

# join pretty source names to c1 to replace ghcnd and snotel
c1_ppt_pretty <- left_join(c1_ppt_pretty, ppt_sites_LUT[c("keycol", "pretty_name")], by = c("source_station" = "keycol"))
# review before replace
#View(subset(c1_ppt_pretty, !is.na(pretty_name))) # looks fine
c1_ppt_pretty <- mutate(c1_ppt_pretty, source_station = ifelse(!is.na(pretty_name), pretty_name, source_station))


# -- finalize dataset -----
# final review
summary(c1_ppt_pretty)
# are all dates accounted for?
summary(c1_ppt_pretty$date %in% seq.Date(min(c1_ppt_pretty$date), max(c1_ppt_pretty$date), 1))
summary(seq.Date(min(c1_ppt_pretty$date), max(c1_ppt_pretty$date), 1) %in% c1_ppt_pretty$date) # good

# check unique vals for all but regression eqs
sapply(c1_ppt_pretty[grepl("LTER|local|flag|source|qc", names(c1_ppt_pretty))], function(x) unique(sort(x))) # ok

# reorganize cols, and slight tweak to colnames
names(c1_ppt_pretty)
names(tkd1_ppt) # put winteradj before precip

#drop pretty_name column
c1_ppt_pretty <- c1_ppt_pretty |> dplyr::select(-pretty_name)

c1_ppt_pretty <- c1_ppt_pretty %>%
  rename(compare_qcnote = compare_qcflag)

c1_ppt_pretty <- c1_ppt_pretty[names(posted_ppt)]

# names(posted_ppt)[!names(posted_ppt) %in% names(c1_ppt_pretty)]
# names(c1_ppt_pretty)[!names(c1_ppt_pretty) %in% names(posted_ppt)]

# -- write out pretty ppt -----

# make NA NaN instead (tell sce NaN in this case = no value, not data are missing)
# write out csv with utf-8 and \r\n eol (end of line)
# (need to change this in code settings, check it looks good after write out)
write.csv(c1_ppt_pretty, paste0(datpath, "publish/c1_daily_precip_gapfilled_ongoing.csv"), 
          row.names = F, na = "NaN", quote = T, fileEncoding = 'utf-8')

