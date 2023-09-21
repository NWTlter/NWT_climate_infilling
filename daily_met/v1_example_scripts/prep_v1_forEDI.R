# format infill temp and ppt dats for EDI

# notes:
# for ppt, include june-sep infilled (i think i did that?). winter months pre-1989 will probably never be usable unless NWT finds something to explain winter precip discrep (esp why saddle drier than c1 in winter when d1 is not)


# -- SETUP ----
library(tidyverse)
library(lubridate)
options(stringsAsFactors = F)

source("~/github/nwt-data-munging/nwt_climate/R/fetch_data_functions.R")
datpath <- "/Users/scarlet/Documents/nwt_lter/nwt_climate/data/"

# read in saddle dats used for jhux and mo papers
# > qc'd, gapfilled, homogenized/date-cutoff daily datasets

sdl_temp <- readRDS(paste0(datpath, "homogenize/nwt_sdl_homogenized_temperature_draft.rds"))
sdl_ppt <- readRDS(paste0(datpath, "infill/sdlPPT_infilled_draft.rds"))

# pull in all sites for saddle (temp and precip)
sitesppt <- readRDS(paste0(datpath, "qc/siteinfoPPT_qc.rds"))
sitestemp <- readRDS(paste0(datpath, "qc/siteinfoTEMP_qc.rds"))

# need to look at all sites stacked data to know which sites to describe in codes for source stations (e.g., no ghcnd stations for hmp period if those stations had stopped)
qcdats <- list.files(paste0(datpath, "qc"), full.names = T)
ghcndtemp <- readRDS(qcdats[grepl("cndT", qcdats)])
ghcndppt <- readRDS(qcdats[grepl("cndP", qcdats)])
fluxtemp <- readRDS(qcdats[grepl("fluxT", qcdats)])
fluxppt <- readRDS(qcdats[grepl("fluxP", qcdats)])
snoteltemp <- readRDS(qcdats[grepl("telT", qcdats)])
snotelppt <- readRDS(qcdats[grepl("telP", qcdats)])
nwtlogtemp <- readRDS(qcdats[grepl("loggerT", qcdats)])
allppt <- readRDS(paste0(datpath, "qc/siteinfoPPT_qc.rds"))

# read in kittel datasets to check colnames and station names
tkd1_temp <- getTabular(185, datanum = 1)
tkd1_ppt <- getTabular(186)

# read in keith's dataset to see colname convention and infill station name convention
jennings <- getTabular(168)
# > also notice in metadata for that dataset they only used 1952-2010 infilled c1 and d1 precip.. revisit how summed hourly values compare to my infilled dailies

# what sites were temporally available to use for sdl temp?
# 1986-2017 = loggers
# 2018 - present = HMP sensor period

subset(ghcndtemp, yr %in% 1986:2017, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # berthoud only available for 1981-1985

subset(ghcndtemp, yr > 2017, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() 

subset(fluxtemp, yr > 2017, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # all available, only forest available for ppt

subset(snoteltemp, yr < 2017, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # all available post and before 2017

# check loggers
subset(nwtlogtemp, yr< 2017, select = c(station_id, station_name, local_site)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # gl4 cr850 in operation 2017-2019, so available for hmp infilling. all hmps at all stations were there in 2018
# c1 hmps all began in jan 2017, but only c1 had hmps in 2017

# look at sites used for sdl
subset(sitesppt, station_name %in% sitesppt$paired_site[sitesppt$station_name == "sdl"], select = c(station_id:station_name)) %>% 
  distinct() %>%
  data.frame()

subset(sitesppt, station_id %in% sitesppt$paired_site[sitesppt$station_name == "sdl"], select = c(station_id:station_name)) %>% 
  distinct() %>%
  data.frame() # not all there because of white space difference

# temp
subset(sitestemp, station_name %in% sitesppt$paired_site[grepl("sdl", sitesppt$station_name)], select = c(station_id:station_name)) %>% 
  distinct() %>%
  data.frame()

subset(sitestemp, station_id %in% sitesppt$paired_site[grepl("sdl", sitesppt$station_name)], select = c(station_id:station_name)) %>% 
  distinct() %>%
  arrange(station_id)


# -- SCREEN PPT ----
# check that all flags present, no weird values in columns
glimpse(sdl_ppt)

summary(is.na(sdl_ppt))
# columns that have NAs:
# pvalue, r2, regression (all same amount of NAs)
# raw ppt and raw qdays (okay to not have same # NAs)
# qc notes
# > all okay

# look at unique values
sapply(sdl_ppt[grepl("site|ye|flag|note", names(sdl_ppt))], function(x) sort(unique(x)))
# okay to release infilled dataset oct 01 1987 onwards -- need to subset dataset

# maybe need to clarify...
# "outdated method" and "JM flagged" in infill_qcnote. explain previous method and QA-flagged or flagged by NWT climate lead during QA review
# also if "raw" values present for outdated method row, explain "raw" is actually old-method infilled

# all comparative qc flags need to be clarified (e.g., "scaled limit", z-score, 15% threshold)
# 


# repeat for ppt
# what sites were temporally available to use for sdl ppt?
# 1987-present 

subset(ghcndppt, yr > 1986, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # berthoud only available for 1981-1985


subset(snoteltemp, yr > 1986, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # all available 1987 onwards


# -- PRETTY DATASETS ----
# make sure colnames match naming convention for C1 and D1 gap-filled
# pval = pvalue, r2 = rsquared
# > going to leave airtemp_max/min/avg as is and dtr lowcase (not following C1 and D1 colnames, will suggest to sarah we adjust those)
# > not going to put "num_obs...regression_equation" because will be too long for hmps (end it at "..regression")

names(tkd1_temp)
sapply(tkd1_temp[grepl("LTER|local|flag|source", names(tkd1_temp))], function(x) sort(unique(x)))
names(jennings)
sapply(jennings[grepl("LTER|local|flag", names(jennings))], function(x) sort(unique(x)))

# keith's flag explanations:
# The flagging structure has the format qcXinY, 
# where X indicates the quality control protocol applied, if any, 
# and Y indicates the infilling protocol applied, if any

# X can have the following values: 
## 0 (observation passed all quality control checks); 
## 1 (observation removed because it falls outside of the maximum/minimum threshold for that variable); 
## 2 (observation removed because it exceeded the rate of change threshold); 
## 3 (observation removed because the sensor was stuck); 
## 4 (variable missing from raw data due to power outage, missing files, datalogger issue, etc.).

# Y can have the following values: 
## 0 (no infilling applied); 
## 1 (gap = 1 h, infilled by using linear interpolation between previous and following observations);
## 2 (gap > 1 h and <= 24, infilled by taking average value of observation 24 hours before and after missing value); 
## 3 (gap > 24 and <= 72 h, infilled with forecasted and backcasted ARIMA model); 
## 4 (infilled using multiple linear regression of observations from both other met stations); 
## 5 (infilled using linear regression from closest met station); 
## 6 (infilled using linear regression from farthest met station); 
## 7 (infilled using climatic mean for that month and 3 h time block); 
## ns (quality flags not specified) [ctw note: for 2014 onwards when kehoe infilled the data]


# -- 1. PRETTY SDL TEMP -----
sdl_temp_pretty <- sdl_temp
names(sdl_temp_pretty) <- gsub("pval$|pval(?=_)" , "pvalue", names(sdl_temp), perl = T)
names(sdl_temp_pretty) <- gsub("r2" , "rsquared", names(sdl_temp_pretty))
names(sdl_temp_pretty)[names(sdl_temp_pretty) == "yr"] <- "year"

# i guess put year before date to match c1 and d1 infilled? and in raw datasets and tk infilled, max, min and avg is the order
# order temps: max, min, avg
sdl_temp_pretty <- subset(sdl_temp_pretty, select = c(LTER_site:logger, year, date,
                                     airtemp_max_homogenized, airtemp_min_homogenized, airtemp_avg_homogenized, dtr_homogenized,
                                     airtemp_max_adjustment, airtemp_avg_adjustment, airtemp_min_adjustment,
                                     airtemp_max_gapfilled, airtemp_min_gapfilled, airtemp_avg_gapfilled, dtr_gapfilled,
                                     flag_1:ncol(sdl_temp_pretty)))
                          
                                     
# -- clean up regression flags ----
# revisit sdl flags again..
sapply(sdl_temp[grepl("flag|note", names(sdl_temp))], function(x) sort(unique(x)))
# > change B* in flag_2 for temp to B. Doesn't matter to user than the raw adjustment failed. Either way used predicted DTR and predicted mean to derive tmax and tmin.
# maybe note in qc_flag that prediction chosen over observed-adjusted
# which rows have B*
View(subset(sdl_temp_pretty, flag_2 == "B*"))
# these both happened in observed tmax cases. for one, tmin pulled bc outside sensor range, the other date's tmin was absent
sdl_temp_pretty$airtemp_max_qcflag[grepl("B[*]", sdl_temp_pretty$flag_2)] <- "Predicted daily temperatures chosen over values derived from observed Tmax"
# change asterisk to B
sdl_temp_pretty$flag_2[grepl("B[*]", sdl_temp_pretty$flag_2)] <- "B"
sapply(sdl_temp_pretty[grepl("flag_[0-9]", names(sdl_temp_pretty))], function(x) sort(unique(x))) # all regression flags good now


# -- clean up QC notes -----
sapply(sdl_temp_pretty[grepl("qcflag|note", names(sdl_temp_pretty))], function(x) sort(unique(x)))
# look at observations where tmean removed due to max or min not recorded
View(subset(sdl_temp_pretty, grepl("not recorded", airtemp_avg_qcflag)))
# > funny: the predicted means are close in val to observed tmeans, esp for 1994-04-08 (both 11C), even tho tmax and tmin not recorded those days

# QAQC notes should be intelligible and standardized
sdl_temp_flagging <- subset(sdl_temp) %>%
  subset(select = grepl("date|yr|mon|logger|flag", names(.)))
# separate qc flagging
sdl_temp_qcflagging <- sdl_temp_flagging %>%
  subset(select = grepl("date|yr|mon|logger|qc", names(.))) %>%
  gather(met, val, airtemp_max_qcflag:ncol(.)) %>%
  mutate(airtemp = str_extract(met, "airtemp_min|airtemp_max|airtemp_avg"),
         met = gsub("^a.*_max_|^a.*_avg_|^a.*_min_", "", met)) %>%
  distinct() %>%
  # keep only records with flags (value NA'd)
  subset(!is.na(val))
# review:
with(sdl_temp_qcflagging, sapply(split(val, paste(logger, met)), function(x) summary(factor(x))))

# > at this time, can't differentiate within-site from cross-site sd violation. within site compared triplicate sensors at some station, cross site compared local_sites (sdl v d1 v c1) 
# or because there are so few hmp flags, maybe just see what they all are

subset(sdl_temp_qcflagging, grepl("or spatial coh", val)) # these should be clarified, pre-hmp will probably be spatial coherency; hmp could be either
# for time's sake, run gapfill_sdlts.R to find intermediate data frame that shows exact issue
# > create hmp_dailysds to execute following code:

View(subset(hmp_dailysds, station_id == "sdl" & date %in% c(sdl_temp_qcflagging$date[grepl("hmp1", sdl_temp_qcflagging$met)])))
# hmp1 flagged for xs violation
# 2021-02-09: flagged for top 15 xs rate change within tmax across all sites (including its own hmps)
# 2021-08-26: flagged for xs temp and xs rate change in tmin (every other data point 1.6-3.4 and hmp1 was -3.6)

View(subset(hmp_dailysds, station_id == "sdl" & date %in% c(sdl_temp_qcflagging$date[grepl("hmp3", sdl_temp_qcflagging$met)])))
# hmp 3 also flagged for xs:
# 2012-12-23: top 15 rate change on tmin
# 2021-05-21: top 15 rate change on tmax (except predicted value made rate change worse -- need to write checks for this in the future. amount off compared to others is ~0.5 so whatever. predicion made delta +1.34 instead of original +1.39 [others were +1.8-1.9])
# > maybe if value is within 0.5 [or 1?] of original, keep original
# 2021-06-06: top 15 xs daily temp violation on tmin (legit): everything else c1 to d1 and other sdl hmps was around 3-3.5, hmp3 was +6
# > but prediction is a bit low of others (1.6 from source sdl hmp2.. averaged tmin for the day ends up being 2.5.. which, other sensors were around 3)

# conversions:
# comp deviance & spatial coherence dev = "Value removed for spatial coherence deviation in rate change and/or daily value" [I actually don't know if 'comp deviance' is for daily value or rate change]
# sensor fail = "Value removed, sensor failure"
# high value = "High value, value removed for spatial coherence deviation"
# within-site = "Value removed for same-site triplicate sensor deviation in rate change and/or daily value"

# need to do manual corrections since flags are in wide-col format
# make a data frame with each unique flag, and it's cleaned up name
cr_qcflags <- sapply(sdl_temp_pretty[grepl("qcflag$", names(sdl_temp_pretty))], function(x) sort(unique(x)))
cr_qcflags <- unlist(cr_qcflags)
cr_qcflags <- unname(cr_qcflags)
# remove Predicted value selected bc I just added that (it's fine)
cr_qcflags <- unique(cr_qcflags[!grepl("Predicted daily temp", cr_qcflags)])
cr_qcflags <- data.frame(original = cr_qcflags)

# make clean flag LUT
## flag for comp deviance & any cr logger that has within-site or spatial coherence
cr_qcflags$replacement <- "Value removed for spatial coherence deviation in rate change and/or daily value"
# high value
cr_qcflags$replacement[grepl("^high", cr_qcflags$original)] <- "High value, removed for spatial coherence deviation"
# shifting temp by x days
cr_qcflags$replacement[grepl("shifted", cr_qcflags$original)] <- gsub("shifted temp", "Temperature value shifted", cr_qcflags$original[grepl("shifted", cr_qcflags$original)])
# sensor fail
cr_qcflags$replacement[grepl("^sensor", cr_qcflags$original)] <- "Value removed, sensor failure"
# mean temp removed bc daily extreme failed, not recorded (keep as is just capitalize first letter in note)
cr_qcflags$replacement[grepl("daily average temp ", cr_qcflags$original)] <-  gsub("daily average temp ", "Average temperature value ", cr_qcflags$original[grepl("daily average temp", cr_qcflags$original)])
cr_qcflags$replacement[grepl("not recorded", cr_qcflags$original)] <-  "Average temperature value removed: Tmax and Tmin not recorded"
# capitalize 't' in Tmin, Tmax 
cr_qcflags$replacement <- gsub(" tm", " Tm", cr_qcflags$replacement)
# see how it looks
cr_qcflags # okay
# can clean up hmp1 and 3 manually since only 5 cases

# iterate through cr qc_flag notes and replace
for(t in c("max", "min", "avg")){
  tempcol <- paste0("airtemp_", t, "_qcflag")
  for(i in 1:nrow(cr_qcflags)){
    sdl_temp_pretty[[tempcol]][grepl(cr_qcflags$original[i], sdl_temp_pretty[[tempcol]])] <- cr_qcflags$replacement[i]
  }
}

# check flags
sapply(sdl_temp_pretty[grepl("qcflag|note", names(sdl_temp_pretty))], function(x) sort(unique(x))) # seems like it's okay

# manually correct hmps
## hmp1
sapply(sdl_temp_pretty[grepl("qcflag_hmp1", names(sdl_temp_pretty))], function(x) sort(unique(x)))
# hmp1 flagged for xs violation:
# > 2021-02-09: flagged for top 15 xs rate change within tmax across all sites (including its own hmps)
sdl_temp_pretty$airtemp_max_qcflag_hmp1[grepl("within", sdl_temp_pretty$airtemp_max_qcflag_hmp1)] <- "Value removed for spatial coherence deviation in rate change"

# > 2021-08-26: flagged for xs temp and xs rate change in tmin (every other data point 1.6-3.4 and hmp1 was -3.6)
sdl_temp_pretty$airtemp_min_qcflag_hmp1[grepl("within", sdl_temp_pretty$airtemp_min_qcflag_hmp1)] <- "Value removed for spatial coherence deviation in rate change and daily value"

## hmp3
# hmp 3 flagged for xs:
# > 2020-12-23: top 15 rate change on tmin
sdl_temp_pretty$airtemp_min_qcflag_hmp3[grepl("within", sdl_temp_pretty$airtemp_min_qcflag_hmp3) & sdl_temp_pretty$date == as.Date("2020-12-23")] <- "Value removed for spatial coherence deviation in rate change"

# > 2021-02-05: top 15 rate change on tmax
sdl_temp_pretty$airtemp_max_qcflag_hmp3[grepl("within", sdl_temp_pretty$airtemp_max_qcflag_hmp3) & sdl_temp_pretty$date == as.Date("2021-02-05")] <- "Value removed for spatial coherence deviation in rate change"

# > 2021-06-06: top 15 xs daily temp violation on tmin
sdl_temp_pretty$airtemp_min_qcflag_hmp3[grepl("within", sdl_temp_pretty$airtemp_min_qcflag_hmp3) & sdl_temp_pretty$date == as.Date("2021-06-06")] <- "Value removed for spatial coherence deviation in daily value"

# check flags again
sapply(sdl_temp_pretty[grepl("flag|note", names(sdl_temp_pretty))], function(x) sort(unique(x))) # looks okay



# -- clean up station names ----
# source station names should be clear
sapply(sdl_temp_pretty[grepl("source", names(sdl_temp_pretty))], function(x) sort(unique(x))) 

# create station LUT, iterate through each source col and replace as did for qc notes
# > naming convention on EDI for loggers is SITE data logger CR[x]
# > D1 and C1 gap-filled have lowcase loggers, but will follow how it's shown on EDI
# Snotel has: Name (number)
# GHCDN is last 5 digits of station ID
unique(sdl_temp_pretty$source_station)

temp_station_LUT <- distinct(sitestemp[c("station_id", "station_name", "local_site", "source")]) %>%
  mutate(pretty_name = ifelse(source == "nwt lter", casefold(gsub("_", " ", station_id), upper = T),
                              ifelse(source == "snotel", paste0(station_name, " (", station_id, ")" ),
                                     ## gsub("US.00", "", station_id))),
         # I don't think I'm gonna sub out the first part of ghcnd stations like TK did because otherwise it will convert them to numeric
         station_id)),
         # little adjustments for nwt
         pretty_name = gsub("CHART", "chart", pretty_name),
         pretty_name = gsub(" CR", " logger CR", pretty_name),
         pretty_name = gsub("HMP ", "HMP-", pretty_name))


# iterate through cr qc_flag notes and replace
for(n in names(sdl_temp_pretty)[grepl("source", names(sdl_temp_pretty))]){
  for(i in 1:nrow(temp_station_LUT)){
    sdl_temp_pretty[[n]][(sdl_temp_pretty[[n]] == temp_station_LUT$local_site[i]) & !is.na(sdl_temp_pretty[[n]])] <- temp_station_LUT$pretty_name[i]
  }
}

# check names
sapply(sdl_temp_pretty[grepl("source", names(sdl_temp_pretty))], unique) # looks good



# -- finalize dataset -----
# check cols and arrangement: remove mon, doy, pay attention to letter casing
str(sdl_temp_pretty) # colnames look okay

# final check of ranges and char values
summary(sdl_temp_pretty)
summary(is.na(sdl_temp_pretty))
View(subset(sdl_temp_pretty, !is.na(raw_airtemp_avg_flag_hmp3))) # I forgot hmps have raw flags of "n" when no flagging assigned (why more non-NA flags than there are non-NA data values)
sapply(sdl_temp_pretty[grepl("LTER|local|logg|flag|source",names(sdl_temp_pretty))], function(x) sort(unique(x)))
# still need to capitalize local_site and logger

sdl_temp_pretty$local_site <- casefold(sdl_temp_pretty$local_site, upper = T)
sdl_temp_pretty$logger <- casefold(sdl_temp_pretty$logger, upper = T)
sdl_temp_pretty$logger <- gsub("_HMP", " HMP", sdl_temp_pretty$logger)

# be sure no duplicate dates and all dates accounted for
summary(seq.Date(min(sdl_temp_pretty$date), max(sdl_temp_pretty$date), 1) %in% sdl_temp_pretty$date)
summary(duplicated(sdl_temp_pretty$date)) # good

#check decimal places on saddle temp pretty.. (noticing weird running decimal places on homogenized DTR)
sapply(sdl_temp_pretty[grepl("_homogenized$", names(sdl_temp_pretty))], function(x) head(x, n = 20))
sapply(sdl_temp_pretty[grepl("_gapfilled$", names(sdl_temp_pretty))], function(x) head(x, n = 20))

# I think temp ready to go
sapply(sdl_temp_pretty, class) # col classes seem right
summary(is.na(sdl_temp_pretty)) # some flag columns only have a few flag values. read_csv reads in these as logical classes and it throws errors, so just let sarah know
# i don't want to quote NA cells bc then numerics will read in as character and that's annoying. just tell data users to pay attn to col types in metadata
sapply(sdl_temp_pretty[grepl("source", names(sdl_temp_pretty))], unique)
View(subset(sdl_temp_pretty, source_station_hmp2 == "University Camp (838)")) # just to see why hmp1 and 3 were't used (answer: they were also out)
# looks fine overall


# -- write out pretty temp -----
# for NWT long term datasets:
# make NA NaN instead (tell sce NaN in this case = no value, not data are missing)
# write out csv with utf-8 and \r\n eol (end of line)
# (need to change this in code settings, check it looks good after write out)
write.csv(sdl_temp_pretty, paste0(datpath, "publish/sdl_daily_airtemp_gapfilled_ongoing.csv"), row.names = F, na = "NaN", quote = T)


# -- 2. PRETTY SDL PPT -----
# pretty all dates, but only write out summer months pre-oct 1988 (show in fig to meagan and sarah that sdl ppt )

# check flags/notes:
sapply(sdl_ppt[grepl("note|flag", names(sdl_ppt))], unique)

# regression flags already fine, just standardize qc notes

# notes I have:
# 1) qdays flag/note, 2) infill qc note/flag, 3) qc and post-infill qc note
# keep all three cols rather than condense (in case any data user interested), just clean up notes

# -- clean up QC notes -----
sdl_ppt_pretty<- sdl_ppt %>%
  # capitalize sdl, d1, nwt site names
  mutate_at(.vars = names(.)[grepl("_qc", names(.))], function(x) gsub("sdl", "SDL", x)) %>%
  mutate_at(.vars = names(.)[grepl("_qc", names(.))], function(x) gsub("d1", "D1", x)) %>%
  mutate_at(.vars = names(.)[grepl("_qc", names(.))], function(x) gsub("nwt", "NWT", x)) %>%
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
sapply(sdl_ppt_pretty[grepl("note|flag", names(sdl_ppt_pretty))], unique)

# clarify notes:
# gapfilling notes
sdl_ppt_pretty$infill_qcnote[grepl("^Infilled by outdated",sdl_ppt_pretty$infill_qcnote)] <- "'Raw' value infilled by outdated method; remove and redo gapfill with current methods"
sdl_ppt_pretty$infill_qcnote[grepl("^JM flag",sdl_ppt_pretty$infill_qcnote)] <- "J. Morse flagged raw value as questionable, remove raw and gapfill"
# move snow even where overcatch applied to infill col
sdl_ppt_pretty$infill_qcnote[grepl("post-infilling", sdl_ppt_pretty$compare_qcflag)] <- unique(sdl_ppt_pretty$compare_qcflag[grepl("post-infilling", sdl_ppt_pretty$compare_qcflag)])
# clarify winter overcatch and it's a shoulder event (these are in june)
sdl_ppt_pretty$infill_qcnote <- gsub("overcatch", "winter overcatch", sdl_ppt_pretty$infill_qcnote)
sdl_ppt_pretty$infill_qcnote <- gsub("Snow event,", "Large shoulder snow event,", sdl_ppt_pretty$infill_qcnote)
# remove from comparative qc col once moved
sdl_ppt_pretty$compare_qcflag[grepl("post-infilling", sdl_ppt_pretty$infill_qcnote)] <- NA

# comparative qc notes:
sdl_ppt_pretty$compare_qcflag[grepl("^SDL 0,",sdl_ppt_pretty$compare_qcflag)] <- "SDL recorded 0 precip; all other NWT sites have positive precip, at least one > 2 z-score and D1 > 10mm or NA"
sdl_ppt_pretty$compare_qcflag[grepl("^SDL >= 10mm precip,",sdl_ppt_pretty$compare_qcflag)] <- "SDL >= 10mm precip; all other NWT sites no precip or NA, only 1 to 3 regional sites with precip (<15% regional sites have positive precip)"
sdl_ppt_pretty$compare_qcflag[grepl("^SDL >= 25in precip,",sdl_ppt_pretty$compare_qcflag)] <- "SDL >= 0.25in precip, only site at NWT with positive precip, only 1 of 1 to 3 sites in region with positive precip"
sdl_ppt_pretty$compare_qcflag[grepl("^Only site",sdl_ppt_pretty$compare_qcflag)] <- "SDL only site in region with positive precip, value exceeds 2.5 z-score (likely blowing snow event)"      

# average diff with all other sites exceeded 5 in scale(diff)
sdl_ppt_pretty$compare_qcflag[grepl("^Shoulder snow",sdl_ppt_pretty$compare_qcflag)] <- "Shoulder snow event or blowing snow; SDL precip exceeds all NWT sites, average pairwise difference with all regional sites exceeds 5 z-score"
sdl_ppt_pretty$compare_qcflag[grepl("^SDL precip exceeds",sdl_ppt_pretty$compare_qcflag)] <- "SDL precip exceeds all NWT sites, average pairwise difference with all regional sites exceeds 5 z-score"

# check notes now
sapply(sdl_ppt_pretty[grepl("note|flag", names(sdl_ppt_pretty))], unique) #ok


# -- clean up station names -----
unique(sdl_ppt_pretty$source_station) # this matches tim's station name convention.. don't need to change, esp if SCE is going to recycle this for C1, D1
# but.. I prefer clarity. treat ghcnd and snotel stations the same as did for temp

# iterate through to partial match value (should work) and assign pretty name from temp_LUT
ppt_sites_LUT <- distinct(sdl_ppt_pretty,source_station) %>%
  mutate(source = ifelse(grepl("Belfort", source_station), "nwt lter", 
                         ifelse(grepl("US-NR", source_station), "ameriflux",
                                ifelse(grepl("[0-9]{5}", source_station), "ghcnd", "snotel")))) %>%
  mutate(pretty_name = ifelse(grepl("nwt lt|ameri", source), source_station, ""))
ppt_sites_LUT <- subset(temp_station_LUT, grepl("snot|ghc", source), select = -local_site) %>%
  distinct() %>%
  mutate(keycol = ifelse(source == "snotel", station_name,
                         # drop beginning of ghcnd name otherwise to get last 5 chars
                         gsub("US.000", "", station_id)))

# join pretty source names to sdl to replace ghcnd and snotel
sdl_ppt_pretty <- left_join(sdl_ppt_pretty, ppt_sites_LUT[c("keycol", "pretty_name")], by = c("source_station" = "keycol"))
# review before replace
View(subset(sdl_ppt_pretty, !is.na(pretty_name))) # looks fine
sdl_ppt_pretty <- mutate(sdl_ppt_pretty, source_station = ifelse(!is.na(pretty_name), pretty_name, source_station))


# -- finalize dataset -----
# final review
summary(sdl_ppt_pretty)
# are all dates accounted for?
summary(sdl_ppt_pretty$date %in% seq.Date(min(sdl_ppt_pretty$date), max(sdl_ppt_pretty$date), 1))
summary(seq.Date(min(sdl_ppt_pretty$date), max(sdl_ppt_pretty$date), 1) %in% sdl_ppt_pretty$date) # good
# check non-winter v winter ppt to be sure
with(sdl_ppt_pretty, sapply(split(precip, lubridate::month(date) %in% 6:9), summary))
with(sdl_ppt_pretty, sapply(split(precip_winteradj, lubridate::month(date) %in% 6:9), summary)) # okay

# check unique vals for all but regression eqs
sapply(sdl_ppt_pretty[grepl("LTER|local|flag|source|qc", names(sdl_ppt_pretty))], function(x) unique(sort(x))) # ok

# reorganize cols, and slight tweak to colnames
names(sdl_ppt_pretty)
names(tkd1_ppt) # put winteradj before precip
# be sure to NA winter months ppt 1981 until Sep 1987 (all okay to leave after that)
sdl_ppt_pretty$remove <- with(sdl_ppt_pretty, month(date) %in% c(1:5, 10:12) & date < as.Date("1987-09-01"))

sdl_ppt_pretty %>%
  mutate(mon = month(date)) %>%
  group_by(mon, year) %>%
  summarize(ppt = sum(precip_winteradj)) %>%
  ungroup() %>%
  mutate(plot_date = as.Date(paste(year, mon, 1, sep = "-")),
         decade = paste0(substr(year,1,3), "0")) %>%
  ggplot(aes(plot_date, ppt)) +
  geom_line() +
  facet_wrap(~decade, scales = "free_x", nrow = 5)

sdl_ppt_pretty %>%
  mutate(mon = month(date)) %>%
  group_by(mon, year) %>%
  summarize(ppt = sum(precip_winteradj)) %>%
  ungroup() %>%
  mutate(plot_date = as.Date(paste(year, mon, 1, sep = "-")),
         decade = paste0(substr(year,1,3), "0")) %>%
  ggplot(aes(mon, ppt)) +
  geom_boxplot(aes(group = mon)) +
  geom_point(aes(col = substr(year,4,4), group = year), position = position_dodge(width = 0.3)) +
  geom_smooth(aes(col = substr(year,4,4), group = year), position = position_dodge(width = 0.3), se = F) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_viridis_d(name = "Month") +
  facet_wrap(~decade, scales = "free_x", nrow = 5)

sdl_check <- sdl_ppt_pretty %>%
  mutate(mon = month(date)) %>%
  group_by(local_site, mon, year) %>%
  summarize(ppt = sum(precip_winteradj)) %>%
  ungroup() %>%
  mutate(plot_date = as.Date(paste(year, mon, 1, sep = "-")),
         decade = paste0(substr(year,1,3), "0"))

tkd1_ppt %>%
  mutate(mon = month(date)) %>%
  group_by(local_site, mon, year) %>%
  summarize(ppt = sum(precip)) %>%
  ungroup() %>%
  mutate(plot_date = as.Date(paste(year, mon, 1, sep = "-")),
         decade = paste0(substr(year,1,3), "0")) %>%
  rbind(sdl_check) %>%
  subset(year %in% 1981:2000) %>%
  ggplot(aes(mon, ppt, col = local_site)) +
  #geom_boxplot(aes(group = mon)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year)

tkd1_ppt %>%
  mutate(mon = month(date)) %>%
  group_by(local_site, mon, year) %>%
  summarize(ppt = sum(precip)) %>%
  ungroup() %>%
  mutate(plot_date = as.Date(paste(year, mon, 1, sep = "-")),
         decade = paste0(substr(year,1,3), "0")) %>%
  rbind(sdl_check) %>%
  subset(year %in% 1982:1992) %>%
  ggplot(aes(mon, ppt)) +
  #geom_boxplot(aes(group = mon)) +
  geom_point(aes(group = year), position = position_dodge(width = 0.3)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  scale_color_viridis_d(name = "Month") +
  facet_grid(year~local_site, scales = "free_x") # start sdl all days at sep 1987, before that jun-sep only. sort of looks like winter correction got applied earlier, but idk

sdl_ppt_pretty <- sdl_ppt_pretty %>%
  # remove winter months. people can get the raw data if they want it
  # reorder cols and put raw after qc notes
  subset(!remove, select = c(LTER_site:date, precip_winteradj, precip,flag_1:regression_equation, 
                             infill_qcnote, compare_qcflag, qdays_qcnote, raw_ppt_tot, raw_qdays)) %>%
  rename(compare_qcnote = compare_qcflag)

glimpse(sdl_ppt_pretty)

ggplot(sdl_ppt_pretty, aes(day(date), precip_winteradj, col = year)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_c() +
  facet_wrap(~month(date), nrow = 2)

mutate(sdl_ppt_pretty,decade = paste0(substr(year,1,3), 0),) %>%
ggplot(aes(month(date), precip, group = month(date))) +
  geom_boxplot() +
  geom_jitter(aes(col = substr(year,4,4)), alpha = 0.5) +
  scale_color_viridis_d() +
  facet_wrap(~decade, nrow = 2) # okay.. just make a note in methods we're unsure about early record winter precip (but late 80s looks like early 2020s .. so maybe)



# -- write out pretty ppt -----
# make NA NaN instead (tell sce NaN in this case = no value, not data are missing)
# write out csv with utf-8 and \r\n eol (end of line)
# (need to change this in code settings, check it looks good after write out)
write.csv(sdl_ppt_pretty, paste0(datpath, "publish/sdl_daily_precip_gapfilled_ongoing.csv"), 
          row.names = F, na = "NaN", quote = T)

# make sure it reads back in okay
testread_base <- read.csv(paste0(datpath, "publish/sdl_daily_precip_gapfilled_ongoing.csv"))
testread_tidy <- read_csv(paste0(datpath, "publish/sdl_daily_precip_gapfilled_ongoing.csv")) # looks good!
