################################################################################
# D1 temperature series (chart, HMPs 1-3, older CR1000 logger), Jan 2018 - Jun 2019,
# each vs its best-matching neighbors: the same plots as qc_2025_vs_neighbors.R.
# For deciding whether to NA the D1 HMPs over ~2018-07-25 to ~2019-03-26 (dashed lines).
# History = 2010-2024 minus this window; the dashed HMP period is also never used as a
# neighbor (so it cannot pull the D1 chart's estimate) or as history.
# Outputs: data/plots/qc_review/d1_2018_*  Run from the repo root (~10 s).
################################################################################

qc_start <- as.Date("2018-01-01")
qc_end <- as.Date("2019-06-30")
target_pattern <- "^d1"
do_precip <- FALSE
n_col <- 1
out_dir <- "daily_met/Infilling_2025/data/plots/qc_review/"
out_prefix <- "d1_2018_"
source("daily_met/Infilling_2025/qc_2025_vs_neighbors.R")
