# Build full-record files for EDI: the posted EDI data before each dataset's re-infill start,
# followed by the 2025 re-infill (step 5 `_ongoing` files), in the posted column format.
# Run after the three step-5 scripts, from the repo root.
#
# Re-infill starts (earlier rows are kept exactly as posted):
#   C1/D1 temp and precip  1980-01-01  first year with neighbor sources (2T drops GHCN before 1980)
#   SDL temp               1986-12-06  whole record is new: CTW's homogenized 1986-2017 series re-infilled
#                                      (2T_sdl_homogenized.R, 3T) and the HMPs from 2018
#   SDL precip             1987-06-01  whole record is new; posted 1981-1987 rows are dropped (3P)
#
# The NWT_metadata reformat scripts append only rows after the last posted date, so these
# files are written separately, to data/publish/full_record/, named like the EDI tables.

rm(list=ls())
library(tidyverse)
source("daily_met/R/fetch_data_functions.R")

datpath <- "daily_met/Infilling_2025/data/publish/"
out_dir <- paste0(datpath, "full_record/")
dir.create(out_dir, showWarnings = FALSE)
# last day to publish (outputs run into the partial next year)
last_date <- as.Date("2025-12-31")

datasets <- tribble(
  ~edi_id, ~new_file,                                   ~new_start,               ~keep_posted, ~out_file,
  185,     "c1_daily_airtemp_gapfilled_ongoing.csv",    as.Date("1980-01-01"),   TRUE,         "c1_infilled_temp_daily.tk.data.csv",
  187,     "d1_daily_airtemp_gapfilled_ongoing.csv",    as.Date("1980-01-01"),   TRUE,         "d1_infilled_temp_daily.tk.data.csv",
  184,     "c1_daily_precip_gapfilled_ongoing.csv",     as.Date("1980-01-01"),   TRUE,         "c1_infilled_precip_daily.tk.data.csv",
  186,     "d1_daily_precip_gapfilled_ongoing.csv",     as.Date("1980-01-01"),   TRUE,         "d1_infilled_precip_daily.tk.data.csv",
  314,     "sdl_daily_airtemp_gapfilled_ongoing.csv",   as.Date("1986-12-06"),   FALSE,        "sdl_daily_airtemp_gapfilled.cw.data.csv",
  315,     "sdl_daily_precip_gapfilled_ongoing.csv",    as.Date("1987-06-01"),   FALSE,        "sdl_daily_precip_gapfilled.cw.data.csv"
)

# read as character so posted values are written back exactly as on EDI
read_chr <- function(x) {
  dat <- if (is.data.frame(x)) x else read.csv(x, colClasses = "character")
  dat[] <- lapply(dat, as.character)
  dat[dat == "NaN"] <- NA
  dat$date <- as.character(as.Date(dat$date))
  dat
}

# C1/D1 temp: CTW's QA flags and notes (e.g. "Artificial drop, shifted +1C") are not made by this
# workflow. Carry them from the posted rows, by date, only where the new value is still the posted
# (raw or shifted) value; otherwise they don't describe the new value.
carry_qa <- function(new, posted) {
  pst <- posted[match(new$date, posted$date), ]
  same <- function(a, b) !is.na(a) & !is.na(b) & abs(as.numeric(a) - as.numeric(b)) < 1e-6
  max_same <- same(new$max_temp, pst$max_temp) | same(new$max_temp, pst$raw_Tmax)
  min_same <- same(new$min_temp, pst$min_temp) | same(new$min_temp, pst$raw_Tmin)
  new$Tmax_QAflag <- ifelse(max_same, pst$Tmax_QAflag, new$Tmax_QAflag)
  new$Tmin_QAflag <- ifelse(min_same, pst$Tmin_QAflag, new$Tmin_QAflag)
  new$infill_QA_note <- ifelse(max_same & min_same, pst$infill_QA_note, new$infill_QA_note)
  # CTW's "Predicted infill chosen over known temperature-adjusted infill" days (2011-2018): 2T NAs the chart
  # values on these days so both tmax and tmin are predicted again, so the note still applies where both are
  # predicted (flag_2 B). Keeping it in the posted table also lets next year's 2T find these days
  ctw_pred <- grepl("Predicted infill chosen", pst$infill_QA_note) & new$flag_2 %in% "B"
  new$infill_QA_note <- ifelse(ctw_pred, pst$infill_QA_note, new$infill_QA_note)
  new
}

summary_out <- data.frame()
for (i in seq_len(nrow(datasets))) {
  d <- datasets[i, ]
  posted <- read_chr(getTabular(d$edi_id, col_class = readr::cols(.default = "c")))
  new <- read_chr(paste0(datpath, d$new_file))

  # step-5 names -> posted names (same fixes as the NWT_metadata reformat scripts)
  names(new) <- gsub("regression_regression", "regression", names(new))
  names(new)[names(new) == "infill_QAnote"] <- "infill_QA_note"
  new <- new[names(new) != "raw_Tmean"]
  stopifnot(all(names(new) %in% names(posted)))
  # columns step 5 doesn't write are left blank
  for (col in setdiff(names(posted), names(new))) new[[col]] <- NA_character_
  new$local_site <- unique(posted$local_site)
  if (d$edi_id %in% c(185, 187)) new <- carry_qa(new, posted)

  full <- bind_rows(subset(posted, d$keep_posted & as.Date(date) < d$new_start),
                    subset(new, as.Date(date) >= d$new_start & as.Date(date) <= last_date)) %>%
    select(all_of(names(posted))) %>%
    arrange(date)

  # temp: no source station on measured days (flag_1 A), as in the step-5 rows; the posted pre-1980
  # C1/D1 rows have "C1"/"D1" there. Precip keeps its gauge name on measured days (e.g. "C1 Belfort Shielded")
  if (d$edi_id %in% c(185, 187, 314)) {
    for (src in grep("^source_station", names(full), value = TRUE)) {
      flag <- sub("source_station", "flag_1", src)
      full[[src]][full[[flag]] %in% "A"] <- NA
    }
  }

  # temps, DTR, precip, adjustments, raw values and r2 to 3 decimals (infilled values carry up to 17).
  # Only values with more than 3 decimals are rewritten, so e.g. posted "12.5" stays "12.5".
  # p-values, observation counts and regression equations are left as is
  num_cols <- grep("temp|DTR|dtr|precip|adjustment|homogenized|gapfilled|^raw_|rsquared", names(full), value = TRUE)
  num_cols <- grep("pvalue|num_obs|equation|flag|note|qdays|source", num_cols, value = TRUE, invert = TRUE)
  for (col in num_cols) {
    x <- full[[col]]
    long <- !is.na(x) & (grepl("\\.[0-9]{4,}", x) | grepl("e", x, ignore.case = TRUE))
    full[[col]][long] <- as.character(round(as.numeric(x[long]), 3))
  }

  # every date once, no gaps
  dates <- as.Date(full$date)
  stopifnot(!any(duplicated(dates)), all(seq.Date(min(dates), max(dates), 1) %in% dates))

  # EDI layout: every value quoted except date and NaN
  write.csv(full, paste0(out_dir, d$out_file), row.names = FALSE, na = "NaN", quote = which(names(full) != "date"),
            fileEncoding = "UTF-8", eol = "\r\n")
  summary_out <- rbind(summary_out, data.frame(
    file = d$out_file, edi = paste0("knb-lter-nwt.", d$edi_id), first = min(dates), last = max(dates),
    posted_rows_kept = sum(dates < d$new_start), new_rows = sum(dates >= d$new_start),
    qa_flags_new_rows = sum(!is.na(full$Tmax_QAflag[dates >= d$new_start])) + sum(!is.na(full$Tmin_QAflag[dates >= d$new_start]))))
}
print(summary_out, row.names = FALSE)
