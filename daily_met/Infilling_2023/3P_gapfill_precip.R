#' ---
#' title: Gap-fill precipitation data 
#' author: CTW  
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#' 
#' ### Precipitation gap-filling and post-infill review
#' 
#' Test report to:
#' 1. Read in NWT and neighbor station temperature datasets
#' 2. Check qualifying days don't overlap a non-NA day for NWT precip
#' 3. Check for breaks in time series
#' 4. Wide-format data for gap filling
#' 
#' All code will be displayed to show procedure and work out bugs

# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

source("daily_met/R/fetch_data_functions.R") # to read in prepped tidy datasets
source("daily_met/R/dataviz_functions.R")
source("daily_met/R/ppt_infill_functions.R")

# set path to prepared data
datpath <- "~/OneDrive - UCB-O365/NWT_Infilling_2023/data/"
# list all rds files in qc'd data folder (full path directory)
rdsfiles_qc <- list.files(paste0(datpath, "qc"), pattern = "rds", full.names = T)

# read in prepared precip data
chartppt <- get_tidydat("chartPPT", rdsfiles_qc, "ppt")
ameriflux <- get_tidydat("amerifluxP", rdsfiles_qc, "ppt")
snotel <- get_tidydat("snotelP", rdsfiles_qc, "ppt")
ghcnd <- get_tidydat("ghcndP", rdsfiles_qc, "ppt")
allsites <- readRDS(rdsfiles_qc[grep("siteinfoP", rdsfiles_qc)])

# -- IMPROMPTU FUNCTIONS -----
idInfillDates <- function(dat, site, startyr){
  infilldates <- with(dat, date[local_site == site & yr >= startyr & is.na(measurement)])
  return(infilldates)
}

# -- PREP DATA FOR REGRESSIONS -----
# stack all dats for regressions, be sure qdays > 1 for NWT datasets are set to NA
sort(unique(chartppt$measurement[chartppt$qdays > 1]))
chartppt$measurement[chartppt$qdays > 1] <- NA

# need date, yr, mon, doy, local_site, and measurement (qc'd)
alldats <- subset(chartppt, select = c(date, yr, mon, doy, local_site, metric, measurement)) %>%
  rbind(ghcnd[names(.)]) %>%
  rbind(snotel[names(.)]) %>%
  rbind(ameriflux[names(.)]) %>%
  arrange(local_site, date) %>%
  mutate(metric = "ppt_tot") # snotel stil has snow_adj
  
# make sure allsites ordered by pair rank
allsites <- arrange(allsites, local_site, combo_rank)
allsites$local_site <- gsub("-", "_", allsites$local_site) 
allsites$paired_site <- gsub("-", "_", allsites$paired_site)
alldats$local_site <- gsub("-", "_", alldats$local_site)

# -- INFILL C1 -----
# specify station hierarchies
c1_order <- with(allsites, paired_site[local_site == "c1" & paired_site != "c1"])
# id missing dates needing infill
c1missing <- idInfillDates(alldats, "c1", 1980)

c1_seasonfill <- tk_ppt_seasonalfill(alldats, "c1", c1missing, c1_order)
c1_historicfill <- tk_ppt_historicfill(alldats, "c1", c1missing, c1_order) # ask SCE about how to find perfect fits
c1chosen <- choose_best(c1missing, c1_seasonfill, c1_historicfill)

# -- backfill c1 based on best regression chosen ----
c1ppt_out <- subset(chartppt, local_site == "c1" & yr >= 1980) %>%
  # now make sure qdays > 1 have the raw measurement unless there is a qc note on it
  mutate(ppt_tot = ifelse(qdays > 1, raw, measurement))

c1ppt_out <- left_join(c1ppt_out, c1chosen) %>% mutate(exp.ppt = exp(infill))
c1ppt_out <- backfill_ppt(c1ppt_out)

# several 0-accumulated backfill periods to take care of
# grab dates that need another round of regressions for 0 accumulated ppt
c1missing_0ac <- with(subset(c1ppt_out, grepl("^0 ppt", Flag.2)), date[!is.na(qdays)])

# join qdays and raw to re-run models, model selection, and backfilling for backfill periods where selected model had 0 ppt accum but c1 has positive
alldats_qdays <- left_join(alldats, chartppt[c("date", "local_site", "qdays", "raw")])
# re-run each model type with qdays check = T
c1_historicfill_qdays <- tk_ppt_historicfill(alldats_qdays,target_site = "c1", missing_dates = c1missing_0ac, site_order = c1_order, qdayscheck = T)
c1_seasonfill_qdays <- tk_ppt_seasonalfill(alldats_qdays,target_site = "c1", missing_dates = c1missing_0ac, site_order = c1_order, qdayscheck = T)
# choose the best model
c1_chosen_qdays <- choose_best(c1missing_0ac, seasonfill = c1_seasonfill_qdays, historicfill = c1_historicfill_qdays)

# because best chosen is based on date where qdays > 1, apply the best model's equation to the backfill period to predict ln'd infill values
c1_backfillexpanded_chosen <- expand_backfill(dat = alldats_qdays, bestmodels = c1_chosen_qdays, target_site = "c1")

# remove new backfill period dates from c1chosen [original models selected] and sub in non-0 ppt models chosen
c1_complete_predicted <- subset(c1chosen, !date %in% c1_backfillexpanded_chosen$date) %>%
  rbind(c1_backfillexpanded_chosen) %>%
  arrange(date)
# make sure everything accounted for (no NAs in infill column)
summary(c1_complete_predicted) # no NAs
summary(c1missing %in% c1_complete_predicted$date)


c1ppt_out_complete <- subset(chartppt, local_site == "c1" & yr >= 1980) %>%
  # now make sure qdays > 1 have the raw measurement unless there is a qc note on it
  mutate(ppt_tot = ifelse(qdays > 1, raw, measurement)) %>%
  left_join(c1_complete_predicted) %>%
  mutate(exp.ppt = exp(infill))

# calculate exponentiated backfilled values + infill single missing days
c1ppt_out_complete <- backfill_ppt(c1ppt_out_complete)
# fix flagging
c1ppt_out_complete$Flag.2[grepl("^H$", c1ppt_out_complete$method)] <- "H" # accumulated amt divided by # event days
c1ppt_out_complete$Flag.1[grepl("^H$", c1ppt_out_complete$method)] <- "A" # data is from target station
c1ppt_out_complete$Flag.2[grepl(", I$", c1ppt_out_complete$method)] <- "I"

# review for completeness
summary(c1ppt_out_complete)

# -- INFILL SDL ----
# id missing dates needing infill
sdlmissing <- idInfillDates(alldats, "sdl", 1980) 
# specify station hierarchies
sdl_order <- with(allsites, paired_site[local_site == "sdl" & paired_site != "sdl"])
# move c1 ahead of snotel and us-nr1 for site order (looks like us nr1 infilled from c1 and function will choose us nr1 over c1)
sdl_order_c1up <- sdl_order[!sdl_order == "c1"] 
sdl_order_c1up <- c(sdl_order_c1up[1:2], "c1", sdl_order_c1up[3:length(sdl_order_c1up)])

sdl_seasonfill <- tk_ppt_seasonalfill(alldats, target_site = "sdl", missing_dates = sdlmissing, site_order = sdl_order_c1up)
sdl_historicfill <- tk_ppt_historicfill(alldats, "sdl", sdlmissing, sdl_order_c1up)
# try sdl infill with overcatch correction factor applied to winter months to see if higher r2s for winter months
alldats_overcatch <- mutate(alldats, measurement = ifelse(local_site == "sdl" & !mon %in% 6:9, measurement*0.39, measurement))
sdl_historicfill_overcatch <- tk_ppt_historicfill(alldats_overcatch, "sdl", sdlmissing, sdl_order)
sdlchosen <- choose_best(sdlmissing, sdl_seasonfill, sdl_historicfill)

# compare infilling saddle when start it in mid 1990s

alldats1996 <- subset(alldats, yr > 1995)
sdl_historicfill96 <- tk_ppt_historicfill(alldats1996, "sdl", sdlmissing[year(sdlmissing) > 1995], sdl_order_c1up)

# how different are the infill values when start at post 1995
ggplot(data = sdl_historicfill96, aes(date, exp(infill))) +
  geom_point(aes(size = r2), col = "green3", alpha = 0.5) +
  geom_point(data = subset(sdl_historicfill, year(date) > 1995), aes(date, exp(infill), size = r2), col = "purple", alpha = 0.25) +
  facet_wrap(~month(date)) # nov-feb are the worst correlative months, gets a little better 3 +4

ggplot() +
  geom_point(data = subset(sdl_historicfill, year(date) > 1995), aes(exp(infill), exp(sdl_historicfill96$infill)), alpha = 0.25) +
  facet_wrap(~month(date), scales = "free") # splitting the data yields higher predictions for 1996 onwards period, overall they aren't that different

ggplot() +
  geom_line(data = subset(sdl_historicfill, year(date) > 1995), aes(date, exp(infill) - exp(sdl_historicfill96$infill)), alpha = 0.5) +
  facet_wrap(~month(date), scales = "free") # use the full (all years) historic fill

# and how different are season v. historic fill values
ggplot(data = sdl_seasonfill, aes(exp(sdl_seasonfill$infill), exp(sdl_historicfill$infill))) +
  geom_point()

ggplot(data = sdl_seasonfill, aes(date, exp(sdl_seasonfill$infill) - exp(sdl_historicfill$infill))) +
  geom_point()

ggplot(data = sdl_seasonfill, aes(date, exp(infill))) +
  geom_point(aes(size = r2), alpha = 0.5) +
  geom_point(data = sdl_historicfill, aes(date, exp(infill), size = r2), col = "blue", alpha = 0.5) +
  geom_smooth(col = "red", fill = "red", method = "lm") +
  facet_wrap(~month(date), scales = "free_x")

ggplot(data = sdl_seasonfill, aes(year(date), exp(infill), group = year(date))) +
  geom_boxplot(fill = "red", alpha = 0.5) +
  #geom_point(aes(size = r2), alpha = 0.5) +
  geom_boxplot(data = sdl_historicfill, aes(year(date), exp(infill), group = year(date)), fill = "blue", alpha = 0.25) +
  #geom_smooth(col = "red", fill = "red", method = "lm") +
  facet_wrap(~month(date), scales = "free_x")

test <- group_by(sdl_historicfill, year(date)) %>% summarise(tot = sum(exp(infill))) %>% data.frame()
test2 <- group_by(sdl_seasonfill, year(date)) %>% summarise(totseas = sum(exp(infill))) %>% data.frame()
test <- left_join(test, test2)
ggplot(test, aes(tot, totseas)) +geom_point() + geom_abline(aes(slope = 1, intercept = 0))
ggplot(test, aes(year.date., tot-totseas)) +geom_point() # choosing one method over the other can change total predicted ppt for that yr by 40mm, which is only 1.57in. not too bad

# clean up
rm(test, test2)

# -- backfill sdl based on best regression chosen ----

sdlppt_out <- subset(chartppt, local_site == "sdl") %>%
  # now make sure qdays > 1 have the raw measurement unless there is a qc note on it
  mutate(ppt_tot = ifelse(qdays > 1, raw, measurement))

sdlppt_out <- left_join(sdlppt_out, sdlchosen) %>% mutate(exp.ppt = exp(infill))
sdlppt_out <- backfill_ppt(sdlppt_out)
# some missing dates need model selection re-run

# grab dates that need another round of regressions for 0 accumulated ppt
sdlmissing_0ac <- with(subset(sdlppt_out, grepl("^0 ppt", Flag.2)), date[!is.na(qdays)])

# join qdays and raw to re-run models, model selection, and backfilling for backfill periods where selected model had 0 ppt accum but sdl has positive
alldats_qdays <- left_join(alldats, chartppt[c("date", "local_site", "qdays", "raw")])
# re-run each model type with qdays check = T
sdl_historicfill_qdays <- tk_ppt_historicfill(alldats_qdays,target_site = "sdl", missing_dates = sdlmissing_0ac, site_order = sdl_order_c1up, qdayscheck = T)
sdl_seasonfill_qdays <- tk_ppt_seasonalfill(alldats_qdays,target_site = "sdl", missing_dates = sdlmissing_0ac, site_order = sdl_order_c1up, qdayscheck = T)
# choose the best model
sdl_chosen_qdays <- choose_best(sdlmissing_0ac, seasonfill = sdl_seasonfill_qdays, historicfill = sdl_historicfill_qdays)

# because best chosen is based on date where qdays > 1, apply the best model's equation to the backfill period to predict ln'd infill values
sdl_backfillexpanded_chosen <- expand_backfill(dat = alldats_qdays, bestmodels = sdl_chosen_qdays, target_site = "sdl")

# remove new backfill period dates from sdlchosen [original models selected] and sub in non-0 ppt models chosen
sdl_complete_predicted <- subset(sdlchosen, !date %in% sdl_backfillexpanded_chosen$date) %>%
  rbind(sdl_backfillexpanded_chosen) %>%
  arrange(date)
# make sure everything accounted for (no NAs in infill column)
summary(sdl_complete_predicted) # no NAs
summary(sdlmissing %in% sdl_complete_predicted$date)


sdlppt_out_complete <- subset(chartppt, local_site == "sdl") %>%
  # now make sure qdays > 1 have the raw measurement unless there is a qc note on it
  mutate(ppt_tot = ifelse(qdays > 1, raw, measurement)) %>%
  left_join(sdl_complete_predicted) %>%
  mutate(exp.ppt = exp(infill))

# calculate exponentiated backfilled values + infill single missing days
sdlppt_out_complete <- backfill_ppt(sdlppt_out_complete)
# fix flagging
sdlppt_out_complete$Flag.2[grepl("^H$", sdlppt_out_complete$method)] <- "H" # accumulated amt divided by # event days
sdlppt_out_complete$Flag.1[grepl("^H$", sdlppt_out_complete$method)] <- "A" # data is from target station
sdlppt_out_complete$Flag.2[grepl(", I$", sdlppt_out_complete$method)] <- "I"

# review for completeness
summary(sdlppt_out_complete)


# -- INFILL D1 -----
# specify station hierarchies
d1_order <- with(allsites, paired_site[local_site == "d1" & paired_site != "d1"])
# id missing dates needing infill
d1missing <- idInfillDates(alldats, "d1", 1980)
# move c1 ahead of snotel and usnr1
d1_order_c1up <- d1_order[!d1_order == "c1"] 
d1_order_c1up <- c(d1_order_c1up[1:2], "c1", d1_order_c1up[3:length(d1_order_c1up)])

d1_seasonfill <- tk_ppt_seasonalfill(alldats, "d1", d1missing, d1_order_c1up)
d1_historicfill <- tk_ppt_historicfill(alldats, "d1", d1missing, d1_order_c1up)
d1chosen <- choose_best(d1missing, d1_seasonfill, d1_historicfill)


# -- backfill d1 based on best regression chosen ----

d1ppt_out <- subset(chartppt, local_site == "d1" & yr >= 1980) %>%
  # now make sure qdays > 1 have the raw measurement unless there is a qc note on it
  mutate(ppt_tot = ifelse(qdays > 1, raw, measurement))
# fix qdays value where qdays > 1 but no accumulated ppt present (note to self: this should be NA'd in QC code)
d1ppt_out$qdays[d1ppt_out$qdays> 1 & !is.na(d1ppt_out$qdays) & is.na(d1ppt_out$raw)] <- NA
d1ppt_out <- left_join(d1ppt_out, d1chosen) %>% mutate(exp.ppt = exp(infill))
d1ppt_out <- backfill_ppt(d1ppt_out)

# several 0-accumulated backfill periods to take care of
# grab dates that need another round of regressions for 0 accumulated ppt
d1missing_0ac <- with(subset(d1ppt_out, grepl("^0 ppt", Flag.2)), date[!is.na(qdays)])

# join qdays and raw to re-run models, model selection, and backfilling for backfill periods where selected model had 0 ppt accum but d1 has positive
alldats_qdays <- left_join(alldats, chartppt[c("date", "local_site", "qdays", "raw")])
# re-run each model type with qdays check = T
d1_historicfill_qdays <- tk_ppt_historicfill(alldats_qdays,target_site = "d1", missing_dates = d1missing_0ac, site_order = d1_order_c1up, qdayscheck = T)
d1_seasonfill_qdays <- tk_ppt_seasonalfill(alldats_qdays,target_site = "d1", missing_dates = d1missing_0ac, site_order = d1_order_c1up, qdayscheck = T)
# choose the best model
d1_chosen_qdays <- choose_best(d1missing_0ac, seasonfill = d1_seasonfill_qdays, historicfill = d1_historicfill_qdays)

# because best chosen is based on date where qdays > 1, apply the best model's equation to the backfill period to predict ln'd infill values
d1_backfillexpanded_chosen <- expand_backfill(dat = alldats_qdays, bestmodels = d1_chosen_qdays, target_site = "d1")

# remove new backfill period dates from d1chosen [original models selected] and sub in non-0 ppt models chosen
d1_complete_predicted <- subset(d1chosen, !date %in% d1_backfillexpanded_chosen$date) %>%
  rbind(d1_backfillexpanded_chosen) %>%
  arrange(date)
# make sure everything accounted for (no NAs in infill column)
summary(d1_complete_predicted) # no NAs
summary(d1missing %in% d1_complete_predicted$date)


d1ppt_out_complete <- subset(chartppt, local_site == "d1" & yr >= 1980) %>%
  # now make sure qdays > 1 have the raw measurement unless there is a qc note on it
  mutate(ppt_tot = ifelse(qdays > 1, raw, measurement)) %>%
  left_join(d1_complete_predicted) %>%
  mutate(exp.ppt = exp(infill))
# fix qdays value where qdays > 1 but no accumulated ppt present (note to self: this should be NA'd in QC code)
d1ppt_out_complete$qdays[d1ppt_out_complete$qdays> 1 & !is.na(d1ppt_out_complete$qdays) & is.na(d1ppt_out_complete$raw)] <- NA

# calculate exponentiated backfilled values + infill single missing days
d1ppt_out_complete <- backfill_ppt(d1ppt_out_complete)
# fix flagging
d1ppt_out_complete$Flag.2[grepl("^H$", d1ppt_out_complete$method)] <- "H" # accumulated amt divided by # event days
d1ppt_out_complete$Flag.1[grepl("^H$", d1ppt_out_complete$method)] <- "A" # data is from target station
d1ppt_out_complete$Flag.2[grepl(", I$", d1ppt_out_complete$method)] <- "I"

# review for completeness
summary(d1ppt_out_complete)
summary(is.na(d1ppt_out_complete))


# -- CLEAN UP FOR WRITE OUT -----
# clean up for final review

# cleanup source names
clean_sources <- function(dat, target_site){
  dat$source_station <- gsub("LakeEl", "Lake El", dat$source_station)
  dat$source_station <- gsub("HighLo", "High Lo", dat$source_station)
  dat$source_station <- gsub("sityCam", "sity Cam", dat$source_station)
  dat$source_station <- gsub("NR1_.*$", "NR1", dat$source_station)
  dat$source_station <- gsub("US_NR1", "US-NR1", dat$source_station)
  dat$source_station <- gsub("USC00053116_.*", "USC00053116", dat$source_station)
  dat$source_station <- gsub("sdl$", "SDL Belfort Shielded", dat$source_station)
  dat$source_station <- gsub("c1$", "C1 Belfort Shielded", dat$source_station)
  dat$source_station <- gsub("d1$", "D1 Belfort Shielded", dat$source_station)
  dat$source_station <- gsub("USC000", "", dat$source_station)
  dat$source_station <- gsub("USW000", "", dat$source_station)
  dat$local_site <- target_site
  print(unique(dat$source_station))
  return(dat)
}


# read in d1 infilled for reminder on colnames and structure
d1tkppt <- getTabular(186)

names_out <- names(d1tkppt)
names_current <- names(d1ppt_out_complete)
d1_out_complete_prep <- d1ppt_out_complete %>%
  mutate(precip = ifelse(!is.na(backfill), backfill, measurement),
         Flag.1 = ifelse(is.na(equation) & raw == measurement, "A", Flag.1),
         Flag.2 = ifelse(is.na(equation) & raw == measurement, "A", Flag.2),
         source.station = ifelse((is.na(equation) & is.na(source.station)) | method == "H", "D1 Belfort Shielded", source.station)) %>%
  rename_all(casefold) %>%
  rename_all(function(x) gsub(".", "_", x, fixed = T))

# check completeness
summary(d1_out_complete_prep)
sapply(d1_out_complete_prep[c("flag_1", "flag_2","method")], unique) # looks okay

# add A flags to test
d1_out <- d1_out_complete_prep[unique(c(names_out[names_out %in% names(d1_out_complete_prep)], 
                                        names(d1_out_complete_prep)[grepl("raw|yr|pval|r2|n_obs|equ|qc", names(d1_out_complete_prep))]))] %>%
  rename(raw_ppt_tot = raw, year = yr,
         rsquared = r2, pvalue = pval, regression_equation = equation,
         num_obs_in_regression_equation = n_obs) %>%
  mutate(LTER_site = "NWT")
d1_out <- d1_out[c(names_out, names(d1_out)[grepl("qc", names(d1_out))])]
# stuff that was never infilled needs A, A flags, and round ppt to same decimal value as recorded? raw has 3 decimal places at most
d1_out <- d1_out %>%
  mutate_at(c("precip", "raw_ppt_tot"), function(x) round(x,3))
# clean up names
d1_out <- clean_sources(d1_out, "D1")

# review one more time
sapply(d1_out[c("flag_1", "flag_2","source_station")], function(x) summary(as.factor(x)))

# same treatment to c1
c1tkppt <- getTabular(184)
names_current <- names(c1ppt_out_complete)
c1_out_complete_prep <- c1ppt_out_complete %>%
  mutate(precip = ifelse(!is.na(backfill), backfill, measurement),
         Flag.1 = ifelse(is.na(equation) & raw == measurement, "A", Flag.1),
         Flag.2 = ifelse(is.na(equation) & raw == measurement, "A", Flag.2),
         source.station = ifelse(is.na(equation) & is.na(source.station), "C1 Belfort Shielded", source.station)) %>%
  rename_all(casefold) %>%
  rename_all(function(x) gsub(".", "_", x, fixed = T))

# add A flags to test
c1_out <- c1_out_complete_prep[unique(c(names_out[names_out %in% names(c1_out_complete_prep)], 
                                        names(c1_out_complete_prep)[grepl("raw|yr|pval|r2|n_obs|equ|qc", names(c1_out_complete_prep))]))] %>%
  rename(raw_ppt_tot = raw, year = yr,
         rsquared = r2, pvalue = pval, regression_equation = equation,
         num_obs_in_regression_equation = n_obs) %>%
  mutate(LTER_site = "NWT")
c1_out <- c1_out[c(names_out, names(c1_out)[grepl("qc", names(c1_out))])]
# stuff that was never infilled needs A, A flags, and round ppt to same decimal value as recorded? raw has 3 decimal places at most
c1_out <- c1_out %>%
  mutate_at(c("precip", "raw_ppt_tot"), function(x) round(x,3))
# clean up names
c1_out <- clean_sources(c1_out, "C1")



# sdl gets mostly the same except need to compare predicted summer to overcatch adjusted for flagged summer snow events
# also need to add winter adjusted col
names_current <- names(sdlppt_out_complete)
sdl_out_complete_prep <- sdlppt_out_complete %>%
  mutate(precip = ifelse(!is.na(backfill), backfill, measurement),
         # assign "A" flags to nonfilled dates
         Flag.1 = ifelse(is.na(method) & raw == measurement, "A", Flag.1),
         Flag.2 = ifelse(is.na(method) & raw == measurement, "A", Flag.2),
         # if was not infilled or method was just to divide accumulated precip by number of days, assign SDL Belfort
         source.station = ifelse((is.na(method) & is.na(source.station)) | method == "H", "SDL Belfort Shielded", source.station)) %>%
  rename_all(casefold) %>%
  rename_all(function(x) gsub(".", "_", x, fixed = T))

# check that everything now has flag and source stations
summary(is.na(sdl_out_complete_prep)) # yes

# add A flags to test
sdl_out <- sdl_out_complete_prep[unique(c(names_out[names_out %in% names(sdl_out_complete_prep)], 
                                        names(sdl_out_complete_prep)[grepl("raw|yr|pval|r2|n_obs|equ|qc", names(sdl_out_complete_prep))]))] %>%
  rename(raw_ppt_tot = raw, year = yr,
         rsquared = r2, pvalue = pval, regression_equation = equation,
         num_obs_in_regression_equation = n_obs) %>%
  mutate(LTER_site = "NWT")
sdl_out <- sdl_out[c(names_out, names(sdl_out)[grepl("qc", names(sdl_out))])]
# stuff that was never infilled needs A, A flags, and round ppt to same decimal value as recorded? raw has 3 decimal places at most
sdl_out <- sdl_out %>%
  mutate_at(c("precip", "raw_ppt_tot"), function(x) round(x,3))
# clean up names
sdl_out <- clean_sources(sdl_out, "SDL")

# create winter adjusted and insert next to precip column
sdl_out <- mutate(sdl_out, precip_winteradj = ifelse(!month(date) %in% 6:9, round(precip * 0.39,3), precip)) %>%
  dplyr::select(LTER_site:precip, precip_winteradj, flag_1:ncol(.))

# assess summer snow events
summersnow <- subset(sdl_out, grepl("^summer snow", compare_qcflag))
# > plot(summersnow$precip, summersnow$raw_ppt_tot*0.39)
# > unique(summersnow$compare_qcflag)
# [1] "summer snow event, apply overcatch correction; sdl ppt exceeds all nwt sites, mean regional difference > 5 scaled limit"
# > sum(summersnow$raw_ppt_tot)
# [1] 597
# > sum(summersnow$precip)
# [1] 60.064

# keep predicted value but clean up flag to not cause confusion
sdl_out$compare_qcflag <- with(sdl_out, gsub("summer snow event, apply overcatch correction", "shoulder snow event or blowing snow", compare_qcflag))


# plot all to review
ggplot(sdl_out, aes(date, precip_winteradj)) +
  geom_line() +
  geom_point(data = subset(sdl_out, !is.na(regression_equation)), col = "red")
# two highest points in the dataset are infilled values from d1 and c1
# june 1995 is a snow storm that occurred late may - early june (backfilled)
# may portion got overcatch treatment, so will apply to early june portion as well

# 2011-07-17 is the other date (infilled from c1, 3 day event UC latter two and c1 on predicted high value day)
# denver post says in early july this year snow was still waist high in spots in the high country. apply correction factor here as well.

# can fuss with later and send jared 0.39 adjusted now bc total amount for event will stay the same
june95 <- seq.Date(as.Date("1995-06-01"), as.Date("1995-06-05"), 1)
sdl_out$precip_winteradj[sdl_out$date %in% june95] <- (sdl_out$precip[sdl_out$date %in% june95] * 0.39)
sdl_out$compare_qcflag[sdl_out$date %in% june95] <- "snow event, adjusted by overcatch factor in post-infilling review"

jul11 <- seq.Date(as.Date("2011-07-17"), as.Date("2011-07-19"), 1)
sdl_out$precip_winteradj[sdl_out$date %in% jul11] <- (sdl_out$precip[sdl_out$date %in% jul11] * 0.39)
sdl_out$compare_qcflag[sdl_out$date %in% jul11] <- "snow event, adjusted by overcatch factor in post-infilling review"


ggplot(sdl_out, aes(date, precip)) +
  geom_line() +
  geom_point(data = subset(sdl_out, !is.na(regression_equation)), col = "red")

# double check for any missing values
# > anything infilled should have equations
summary(sdl_out) # infill values there for 2017 Apr, but H flags not there.. why?
sapply(sdl_out[c("flag_1", "flag_1", "source_station", "infill_qcnote", "compare_qcflag")], function(x) summary(as.factor(x)))


# double check flagging
sapply(subset(sdl_out, flag_1 != "A" & flag_2 == "A", select = c(local_site, flag_1:compare_qcflag)), function(x) length(unique(x)))
View(subset(sdl_out, flag_1 == "A" & flag_2 != "A")) # all G or H cases, should have SDL Belfort and no regression info
View(subset(sdl_out, flag_1 != "A" & flag_2 == "A")) # these are cases where SDL just needed a single day infill
summary(is.na(subset(sdl_out, flag_1 != "A" & flag_2 != "A")))

# check same for d1 and c1
View(subset(d1_out, flag_1 == "A" & flag_2 != "A")) # same deal. anything that was 0 accum needs D1 assigned and all regression info removed
View(subset(d1_out, flag_1 != "A" & flag_2 == "A"))
# check c1
View(subset(c1_out, flag_1 == "A" & flag_2 != "A")) # same deal. anything that was 0 accum needs D1 assigned and all regression info removed
View(subset(c1_out, flag_1 != "A" & flag_2 == "A"))


## clean up flagging
sdl_out$source_station[sdl_out$flag_2 %in% c("G", "H")] <- "SDL Belfort Shielded"
sdl_out[grepl("^SDL", sdl_out$source_station), grep("pval|rsquar|regression", names(sdl_out))] <- NA
View(sdl_out)
# add note for H that regression source stations had 0 accumulated during period
sdl_out$infill_qcnote[sdl_out$flag_2 == "H"] <- "Available source stations had 0 accumulated during qualifying days period; D1 station (nearest) did not record data during period"
# clean up d1
d1_out$source_station[d1_out$flag_2 %in% c("G", "H")] <- "D1 Belfort Shielded"
d1_out[grepl("^D1", d1_out$source_station), grepl("pvalu|rsquar|regression", names(d1_out))] <- NA
c1_out$source_station[c1_out$flag_2 %in% c("G", "H")] <- "C1 Belfort Shielded"
c1_out[grepl("^C1", c1_out$source_station), grep("pval|rsquar|regression", names(c1_out))] <- NA


# stack c1 and d1 with tkdats, but start over at 2011-01-01
c1_out_tkctw <- subset(c1tkppt, year <= 2010) %>%
  #mutate(qdays_qcnote = NA, infill_qcnote = NA, compare_qcflag = NA) %>%
  dplyr::bind_rows(subset(c1_out, year > 2010)) %>%
  arrange(date)

d1_out_tkctw <- subset(d1tkppt, year <= 2010) %>%
  #mutate(qdays_qcnote = NA, infill_qcnote = NA, compare_qcflag = NA) %>%
  dplyr::bind_rows(subset(d1_out, year > 2010)) %>%
  arrange(date)


# to follow Tim's methods, just change D1 and C1 and SDL to those when they are sources for another station in D1 and C1 datasets
# > can stay belfort shielded in SDL because that's what it was
d1_out_tkctw$source_station[grepl("C1", d1_out_tkctw$source_station)] <- "C1"
d1_out_tkctw$source_station[grepl("SDL", d1_out_tkctw$source_station)] <- "SDL"

c1_out_tkctw$source_station[grepl("D1", c1_out_tkctw$source_station)] <- "D1"
c1_out_tkctw$source_station[grepl("SDL", c1_out_tkctw$source_station)] <- "SDL"


# -- WRITE OUT ----
# prelim dats out for jrad
write.csv(c1_out_tkctw, paste0(datpath, "/infill/c1PPT_infilled_draft.csv"), row.names = F)
write.csv(d1_out_tkctw, paste0(datpath, "/infill/d1PPT_infilled_draft.csv"), row.names = F)
write.csv(sdl_out, paste0(datpath, "/infill/sdlPPT_infilled_draft.csv"), row.names = F)

# write out as rds for me to inspect
saveRDS(c1_out_tkctw, paste0(datpath, "/infill/c1PPT_infilled_draft.rds"))
saveRDS(d1_out_tkctw, paste0(datpath, "/infill/d1PPT_infilled_draft.rds"))
saveRDS(sdl_out, paste0(datpath, "/infill/sdlPPT_infilled_draft.rds"))

# write out predicted values for comparison and ctw's recalculated c1 and d1 infilled as well
saveRDS(sdl_historicfill, paste0(datpath, "/infill/sdl_historicfill.rds"))
saveRDS(sdl_seasonfill, paste0(datpath, "/infill/sdl_seasonfill.rds"))
saveRDS(sdlchosen, paste0(datpath, "/infill/sdlchosen.rds"))
saveRDS(sdl_backfillexpanded_chosen, paste0(datpath, "/infill/sdl_backfillexpanded_chosen.rds"))
saveRDS(c1_out, paste0(datpath, "/infill/c1PPT_infilled_ctw1980.rds"))
saveRDS(d1_out, paste0(datpath, "/infill/d1PPT_infilled_ctw1980.rds"))

# and the all stacked dat
saveRDS(alldats_qdays, paste0(datpath, "/infill/allPPTdats_wNWTqdays.rds"))

# write out special Saddle Sep 1987 onwards for Meagan
write.csv(subset(sdl_out, date >= as.Date("1987-09-01")), paste0(datpath, "/infill/sdlPPT_infilled_draft_Sep1987.csv"), row.names = F)