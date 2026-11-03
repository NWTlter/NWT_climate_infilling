# ============================================================================
# QC: find dates where a chart-recorder site's infilled precip/temp files
# show an INFILLED value even though the raw source ("*.ml.data.csv") record
# for that date looks complete and was not flagged questionable -- i.e.
# infilling that, per the documented methods, should not have happened.
#
# Works for either D1 or C1 -- both infilled packages (knb-lter-nwt.186 for
# D1 precip; the equivalent C1 packages) document the same scheme:
#   flag_1 == "A"  -> "data recorded at target station"
#   flag_1 in B/C/D/E -> infilled via regression method 1/2, at given p-value
#   The infilled files also carry raw_ppt_tot/raw_Tmax/raw_Tmin columns,
#   described as "raw (not infilled)" values -- these should be NA exactly
#   when flag_1 != "A", and non-NA when flag_1 == "A".
#
# This script checks that invariant two ways:
#   1. Internally, within each infilled file (fast, no join needed).
#   2. Against the actual raw source files (<site>-1pdayv.ml.data.csv /
#      <site>-1tdayv.ml.data.csv), which are the authoritative record and
#      may have been updated since a given year's infilling was run.
#
# Precip has one extra wrinkle the temp file doesn't: the raw file's `qdays`
# column. When qdays > 1, the value recorded on that date is a multi-day
# ACCUMULATED total (chart recorder read weekly/etc.), not a same-day
# observation -- per methods step (C), parsing that total into daily values
# is expected and legitimate, even though flag_1 will show a non-"A" method.
# So "raw present but qdays > 1" is reported separately from "raw present,
# qdays == 1" (a clean single-day reading that was overridden for no
# documented reason -- the strongest evidence of a real problem).
#
# The raw PRECIP file's flag_ppt_tot codes are documented in the raw
# package's own EML (knb-lter-nwt.415 for D1):
#   1 = value infilled using method 1 (daily ratio)
#   2 = value infilled using method 2 (known total)
#   q = data questionable
#   n = no quality issues
# Codes 1 and 2 matter a lot here: they mean the "raw" value is itself an
# estimate, not an observation, so the infilled product is *expected* to
# re-infill that date with its own method rather than copy it. A mismatch on
# those dates is correct behavior, not a finding.
#
# The raw TEMP file's flag_airtemp_max / flag_airtemp_min codes are
# documented the same way in knb-lter-nwt.412:
#   1 = infilled using method 1 (regression)
#   2 = infilled using method 2 (standard deviation)
#   q = data questionable
#   n = no flag
# so 1/2 disqualify a date the same way they do for precip. Note that the
# data also contains "3" (29 dates) and "m" (77 dates), neither of which is
# in that package's enumerated code list -- "m" reads as missing by the
# convention used elsewhere in NWT files (and is documented that way in the
# SDL airtemp package), "3" is anyone's guess. Both are treated as "not a
# clean observation" and named individually in the expected_reason column so
# the call is visible rather than buried.
#
# knb-lter-nwt.187 (infilled temp) also documents flag_2 codes that matter
# here: C = "infilled adjusted for known Tmin", D = "adjusted for known
# Tmax". On a "C" date Tmin was the known anchor the infill was built from,
# so raw Tmin being present is expected, not a finding -- and likewise Tmax
# on a "D" date. Confirmed in the data: where flag_2 == "D", published
# max_temp matches raw Tmax for 44 of 49 dates (median difference 0), and
# where flag_2 == "C", published min_temp matches raw Tmin for 60 of 67.
#
# Usage:
#   cd /path/to/this/repo     # run from the repo ROOT: the script uses the
#                             # relative paths data/ and qc_out/
#   Rscript R/qc_infilled_vs_raw.R d1     # or: c1   (default: d1)
#
# Before running, download the 2025 versions of these files from EDI into
# data/. Only the four for the site you're running are needed.
#
#   file                                    EDI package
#   -------------------------------------   ---------------------------------
#   d1_infilled_precip_daily.tk.data.csv    knb-lter-nwt.186
#   d1_infilled_temp_daily.tk.data.csv      knb-lter-nwt.187
#   d-1pdayv.ml.data.csv                    knb-lter-nwt.415
#   d-1tdayv.ml.data.csv                    knb-lter-nwt.412
#
#   c1_infilled_precip_daily.tk.data.csv    knb-lter-nwt.184 or .185 *
#   c1_infilled_temp_daily.tk.data.csv      knb-lter-nwt.184 or .185 *
#   c-1pdayv.ml.data.csv                    knb-lter-nwt.414
#   c-1tdayv.ml.data.csv                    knb-lter-nwt.411
#
# The D1 numbers and 411/414 are confirmed against the EML documents
# themselves. (*) The two C1 infilled packages are somewhere in the 184-187
# range but which is precip and which is temp hasn't been verified here --
# check before relying on it. The infilled packages are the "infilled"
# series (184-187); the raw chart-recorder series is 411-415.
#
# Writes one CSV per check to qc_out/<site>/.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
})

args <- commandArgs(trailingOnly = TRUE)
site <- if (length(args) >= 1) tolower(args[[1]]) else "d1"
stopifnot("site must be 'd1' or 'c1'" = site %in% c("d1", "c1"))
site_letter <- substr(site, 1, 1)  # "d" or "c" -- used in the raw filenames' "<letter>-1..." pattern

data_dir <- "data"
outdir   <- file.path("qc_out", site)
if(!dir.exists(outdir)){
  dir.create(outdir, recursive = TRUE)
}

# Raw-file quality flags treated as "not a clean, trustworthy observation"
# and therefore excluded from the highest-confidence tier below. See the
# header comment for what's documented vs. inferred.
RAW_ALREADY_INFILLED_PRECIP_FLAGS <- c("1", "2")  # documented in knb-lter-nwt.415
QUESTIONABLE_PRECIP_FLAGS         <- c("q")
RAW_ALREADY_INFILLED_TEMP_FLAGS   <- c("1", "2")  # documented in knb-lter-nwt.412
QUESTIONABLE_TEMP_FLAGS           <- c("q", "m")  # q documented in .412; m is not in its code list
UNDOCUMENTED_TEMP_FLAGS           <- c("3")       # present in the data, absent from .412's code list

# knb-lter-nwt.187 flag_2: the raw value for this variable was the known
# anchor the infill was built from, so its presence is expected.
TMIN_ANCHOR_FLAG_2 <- "C"   # "infilled adjusted for known Tmin"
TMAX_ANCHOR_FLAG_2 <- "D"   # "infilled adjusted for known Tmax"

# flag_2 codes on the infilled file meaning the daily value came from a
# period total recorded at the target station, so no per-day raw value is
# expected to exist (knb-lter-nwt.186):
#   G = period total of zero recorded at target, all days in period set to 0
#   H = period total recorded at target, divided by number of days in period
PERIOD_TOTAL_FLAG_2 <- c("G", "H")

# Infilled-minus-raw differences at or below these are treated as rounding
# noise, not a real discrepancy, and are broken out into their own
# "negligible_rounding_diff" tier rather than the priority list -- a date
# where infilling reproduced the raw value to within a tenth of a degree or
# a millimeter isn't worth the same scrutiny as one off by 10+.
PRECIP_ROUNDING_TOL <- 1    # mm
TEMP_ROUNDING_TOL    <- 0.1 # deg C

na_codes <- c("NA", "NaN", "")

infilled_precip <- read_csv(file.path(data_dir, paste0(site, "_infilled_precip_daily.tk.data.csv")), na = na_codes, show_col_types = FALSE)
# infill_QA_note / Tmax_QAflag / Tmin_QAflag are almost all blank, so readr's
# type-guessing (based on the first 1000 rows) mis-detects them as logical and
# silently discards the handful of free-text values they actually hold -- and
# those values turn out to matter (see below). Force them to character so
# nothing gets dropped.
infilled_temp <- read_csv(
  file.path(data_dir, paste0(site, "_infilled_temp_daily.tk.data.csv")),
  na = na_codes, show_col_types = FALSE,
  col_types = cols(infill_QA_note = col_character(), Tmax_QAflag = col_character(),
                    Tmin_QAflag = col_character(), .default = col_guess())
)
raw_precip <- read_csv(file.path(data_dir, paste0(site_letter, "-1pdayv.ml.data.csv")), na = na_codes, show_col_types = FALSE)
raw_temp   <- read_csv(file.path(data_dir, paste0(site_letter, "-1tdayv.ml.data.csv")), na = na_codes, show_col_types = FALSE)

stopifnot(!anyDuplicated(raw_precip$date), !anyDuplicated(raw_temp$date))
stopifnot(!anyDuplicated(infilled_precip$date), !anyDuplicated(infilled_temp$date))

issues <- list()

# ============================================================================
# PRECIP
# ============================================================================

# ---- 1. internal consistency: flag_1 says infilled, but the file's own
#         raw_ppt_tot column (meant to be NA exactly when infilled) is present
issues$precip_internal_infilled_but_raw_col_present <- infilled_precip %>%
  filter(flag_1 != "A", !is.na(raw_ppt_tot)) %>%
  select(date, precip, flag_1, flag_2, raw_ppt_tot, raw_qdays, source_station)

# ---- 2. does the infilled file's stored raw_ppt_tot match the CURRENT
#         source file? (they should always agree; a mismatch means the raw
#         file was edited/corrected after this date was infilled)
precip_joined <- infilled_precip %>%
  left_join(
    raw_precip %>% select(date, src_ppt_tot = ppt_tot, src_qdays = qdays, src_flag = flag_ppt_tot),
    by = "date"
  )

issues$precip_stored_raw_vs_current_source_drift <- precip_joined %>%
  filter(
    (is.na(raw_ppt_tot) != is.na(src_ppt_tot)) |
      (!is.na(raw_ppt_tot) & !is.na(src_ppt_tot) & abs(raw_ppt_tot - src_ppt_tot) > 0.01)
  ) %>%
  select(date, precip, flag_1, raw_ppt_tot, src_ppt_tot, src_qdays, src_flag)

# ---- 3. main check: flag_1 says infilled, but the CURRENT raw source has a
#         value for that date.
#
# Three situations make an infilled value *expected* rather than a finding,
# so they're separated into an "expected_infill" bucket with the reason
# recorded, leaving only genuinely unexplained dates in the priority list:
#   - the raw value is itself infilled (flag_ppt_tot 1/2) -- the target
#     package re-infills those with its own method, so it won't match
#   - the raw value is flagged questionable (q) -- legitimately overridden
#   - the date falls in a multi-day accumulation period (qdays != 1) -- the
#     daily value is parsed from the period total by design
precip_candidates <- precip_joined %>%
  filter(flag_1 != "A", !is.na(src_ppt_tot)) %>%
  mutate(
    value_diff  = precip - src_ppt_tot,
    raw_already_infilled = !is.na(src_flag) & src_flag %in% RAW_ALREADY_INFILLED_PRECIP_FLAGS,
    raw_is_questionable  = !is.na(src_flag) & src_flag %in% QUESTIONABLE_PRECIP_FLAGS,
    is_period_total      = is.na(src_qdays) | src_qdays != 1,
    expected_reason = sub(";\\s*$", "", paste0(
      if_else(raw_already_infilled, "raw value itself infilled (flag 1/2); ", ""),
      if_else(raw_is_questionable,  "raw flagged questionable (q); ", ""),
      if_else(is_period_total,      "multi-day accumulation period (qdays != 1); ", "")
    )),
    tier = case_when(
      expected_reason != ""                  ~ "expected_infill",
      abs(value_diff) <= PRECIP_ROUNDING_TOL ~ "negligible_rounding_diff",
      TRUE                                   ~ "high_confidence_should_not_be_infilled"
    )
  ) %>%
  select(date, year, precip, src_ppt_tot, value_diff, flag_1, flag_2, source_station,
         pvalue, rsquared, src_qdays, src_flag, expected_reason, tier) %>%
  arrange(tier, desc(abs(value_diff)))

issues$precip_infilled_but_raw_available_high_confidence <- precip_candidates %>%
  filter(tier == "high_confidence_should_not_be_infilled") %>% select(-tier, -expected_reason)
issues$precip_infilled_but_raw_available_negligible_rounding_diff <- precip_candidates %>%
  filter(tier == "negligible_rounding_diff") %>% select(-tier, -expected_reason)
issues$precip_infilled_but_raw_available_expected_infill <- precip_candidates %>%
  filter(tier == "expected_infill") %>% select(-tier)

# ---- 4. reverse check (secondary): flag_1 == "A" (says recorded), but the
#         current raw source has no value for that date at all. flag_2 G/H
#         are excluded: those daily values come from a period total recorded
#         at the target station, so there is no per-day raw value to find.
issues$precip_flagged_recorded_but_raw_missing <- precip_joined %>%
  filter(flag_1 == "A", is.na(src_ppt_tot), !flag_2 %in% PERIOD_TOTAL_FLAG_2) %>%
  select(date, precip, flag_1, flag_2, raw_ppt_tot)

# ============================================================================
# TEMPERATURE  (max_temp / min_temp checked in parallel; no qdays concept)
# ============================================================================

issues$temp_internal_infilled_but_raw_col_present <- infilled_temp %>%
  filter(flag_1 != "A", !is.na(raw_Tmax) | !is.na(raw_Tmin)) %>%
  select(date, max_temp, min_temp, flag_1, flag_2, flag_3, raw_Tmax, raw_Tmin, source_station)

temp_joined <- infilled_temp %>%
  left_join(
    raw_temp %>% select(date, src_tmax = airtemp_max, src_tmax_flag = flag_airtemp_max,
                         src_tmin = airtemp_min, src_tmin_flag = flag_airtemp_min),
    by = "date"
  )

issues$temp_stored_raw_vs_current_source_drift <- temp_joined %>%
  filter(
    (is.na(raw_Tmax) != is.na(src_tmax)) | (!is.na(raw_Tmax) & !is.na(src_tmax) & abs(raw_Tmax - src_tmax) > 0.01) |
    (is.na(raw_Tmin) != is.na(src_tmin)) | (!is.na(raw_Tmin) & !is.na(src_tmin) & abs(raw_Tmin - src_tmin) > 0.01)
  ) %>%
  select(date, max_temp, min_temp, flag_1, raw_Tmax, src_tmax, raw_Tmin, src_tmin)

# Tmax and Tmin are infilled/recorded independently but share flag_1/flag_3 in
# this file (flag_1 covers t_mean, flag_3 covers TDTR/derived Tmax&Tmin per
# the regression columns) -- check each series against its own raw column.
#
# Two ways an infilled value is explained rather than suspect:
#   - the infilled file documents the override itself via Tmax_QAflag /
#     Tmin_QAflag (e.g. "pen stuck", "temp shifted 4C warmer, chart dropped
#     artificially") or infill_QA_note -> documented_or_flagged_raw
#   - the raw value was never a clean observation to begin with (itself
#     infilled, flagged questionable/missing, or carrying the undocumented
#     "3" code), or it was the known anchor the infill was built from
#     (flag_2 C/D) -> expected_infill, with the reason recorded
temp_candidates <- function(value_col, src_col, src_flag_col, qaflag_col, anchor_flag_2) {
  temp_joined %>%
    filter(flag_1 != "A", !is.na(.data[[src_col]])) %>%
    mutate(
      value_diff = .data[[value_col]] - .data[[src_col]],
      raw_already_infilled  = !is.na(.data[[src_flag_col]]) & .data[[src_flag_col]] %in% RAW_ALREADY_INFILLED_TEMP_FLAGS,
      raw_is_questionable   = !is.na(.data[[src_flag_col]]) & .data[[src_flag_col]] %in% QUESTIONABLE_TEMP_FLAGS,
      raw_flag_undocumented = !is.na(.data[[src_flag_col]]) & .data[[src_flag_col]] %in% UNDOCUMENTED_TEMP_FLAGS,
      raw_is_anchor         = !is.na(flag_2) & flag_2 == anchor_flag_2,
      documented            = !is.na(.data[[qaflag_col]]) | !is.na(infill_QA_note),
      expected_reason = sub(";\\s*$", "", paste0(
        if_else(raw_already_infilled,  "raw value itself infilled (flag 1/2); ", ""),
        if_else(raw_is_questionable,   "raw flagged questionable or missing (q/m); ", ""),
        if_else(raw_flag_undocumented, "raw flag '3' (not in the raw package's documented code list); ", ""),
        if_else(raw_is_anchor,         paste0("raw value was the known anchor for the infill (flag_2 == '", anchor_flag_2, "'); "), "")
      )),
      tier = case_when(
        documented                           ~ "documented_or_flagged_raw",
        expected_reason != ""                ~ "expected_infill",
        abs(value_diff) <= TEMP_ROUNDING_TOL ~ "negligible_rounding_diff",
        TRUE                                 ~ "high_confidence_undocumented"
      )
    ) %>%
    select(date, year, infilled = all_of(value_col), raw = all_of(src_col), value_diff,
           flag_1, flag_2, flag_3, source_station, raw_flag = all_of(src_flag_col),
           qaflag = all_of(qaflag_col), infill_QA_note, expected_reason, tier) %>%
    arrange(tier, desc(abs(value_diff)))
}

temp_tmax_candidates <- temp_candidates("max_temp", "src_tmax", "src_tmax_flag", "Tmax_QAflag", TMAX_ANCHOR_FLAG_2)
temp_tmin_candidates <- temp_candidates("min_temp", "src_tmin", "src_tmin_flag", "Tmin_QAflag", TMIN_ANCHOR_FLAG_2)

for (v in c("tmax", "tmin")) {
  cand <- if (v == "tmax") temp_tmax_candidates else temp_tmin_candidates
  issues[[paste0("temp_", v, "_infilled_but_raw_available_UNDOCUMENTED")]] <-
    cand %>% filter(tier == "high_confidence_undocumented") %>% select(-tier, -expected_reason)
  issues[[paste0("temp_", v, "_infilled_but_raw_available_documented_or_flagged")]] <-
    cand %>% filter(tier == "documented_or_flagged_raw") %>% select(-tier, -expected_reason)
  issues[[paste0("temp_", v, "_infilled_but_raw_available_negligible_rounding_diff")]] <-
    cand %>% filter(tier == "negligible_rounding_diff") %>% select(-tier, -expected_reason)
  issues[[paste0("temp_", v, "_infilled_but_raw_available_expected_infill")]] <-
    cand %>% filter(tier == "expected_infill") %>% select(-tier)
}

issues$temp_flagged_recorded_but_raw_missing <- temp_joined %>%
  filter(flag_1 == "A", is.na(src_tmax) | is.na(src_tmin)) %>%
  select(date, max_temp, min_temp, flag_1, raw_Tmax, raw_Tmin)

# ============================================================================
# write outputs + console summary
# ============================================================================

iwalk(issues, function(df, nm) {
  if (nrow(df) > 0) write_csv(df, file.path(outdir, paste0("qc_", nm, ".csv")))
})

cat(sprintf("==== site: %s ====\n", toupper(site)))
cat("==== issue counts ====\n")
for (nm in names(issues)) cat(sprintf("%-58s %d\n", nm, nrow(issues[[nm]])))

cat(sprintf(
  "\nprecip: %d high-confidence 'infilled but raw available' dates (clean single-day observation, infilled anyway, no explanation; excludes %d within %gmm rounding tolerance and %d where infilling was expected -- raw itself infilled, raw flagged q, or multi-day accumulation period).\n",
  nrow(issues$precip_infilled_but_raw_available_high_confidence),
  nrow(issues$precip_infilled_but_raw_available_negligible_rounding_diff),
  PRECIP_ROUNDING_TOL,
  nrow(issues$precip_infilled_but_raw_available_expected_infill)
))
cat(sprintf(
  "temp Tmax: %d UNDOCUMENTED (a clean raw observation, overwritten, nothing explaining it) + %d documented/flagged + %d expected (raw itself infilled/flagged, or the known anchor) + %d within %gC rounding tolerance. Tmin: %d / %d / %d / %d.\n",
  nrow(issues$temp_tmax_infilled_but_raw_available_UNDOCUMENTED),
  nrow(issues$temp_tmax_infilled_but_raw_available_documented_or_flagged),
  nrow(issues$temp_tmax_infilled_but_raw_available_expected_infill),
  nrow(issues$temp_tmax_infilled_but_raw_available_negligible_rounding_diff),
  TEMP_ROUNDING_TOL,
  nrow(issues$temp_tmin_infilled_but_raw_available_UNDOCUMENTED),
  nrow(issues$temp_tmin_infilled_but_raw_available_documented_or_flagged),
  nrow(issues$temp_tmin_infilled_but_raw_available_expected_infill),
  nrow(issues$temp_tmin_infilled_but_raw_available_negligible_rounding_diff)
))

# ---- plot: magnitude of value discrepancy for the highest-priority tiers ---
plot_df <- bind_rows(
  issues$precip_infilled_but_raw_available_high_confidence %>%
    transmute(variable = "precip (mm)", value_diff),
  issues$temp_tmax_infilled_but_raw_available_UNDOCUMENTED %>%
    transmute(variable = "Tmax (deg C)", value_diff),
  issues$temp_tmin_infilled_but_raw_available_UNDOCUMENTED %>%
    transmute(variable = "Tmin (deg C)", value_diff)
)

if (nrow(plot_df) > 0) {
  p <- ggplot(plot_df, aes(x = value_diff)) +
    geom_histogram(bins = 60) +
    facet_wrap(~variable, scales = "free", ncol = 1) +
    labs(
      title = sprintf("%s: infilled value minus raw observation (precip: high-confidence tier; temp: undocumented overrides)", toupper(site)),
      x = "infilled - raw (0 = infilling reproduced the raw value exactly)",
      y = "count of dates"
    ) +
    theme_minimal()
  ggsave(file.path(outdir, "qc_infilled_vs_raw_diff_hist.png"), p, width = 9, height = 7, dpi = 150)
}

cat(sprintf("\nQC outputs written to: %s/\n", normalizePath(outdir, mustWork = FALSE)))
