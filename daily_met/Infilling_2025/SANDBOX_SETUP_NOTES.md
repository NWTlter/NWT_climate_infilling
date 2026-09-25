# Infilling_2025 — sandbox setup & debugging notes

Working notes from getting `1_prepare_climate_data.R` to run on a fresh Linux VM
(2026-09-24, branch `AI_infilling_2025`). Keep or delete as you see fit — the
package-install section is the part likely to save the next person real time.

## 1. R package setup (required on a fresh machine)

R 4.5.2. Three packages the script needs are NOT installable the obvious way:

```r
install.packages(c("gridExtra", "remotes"))

# amerifluxr has been REMOVED from CRAN (only in the archive). Install from source:
remotes::install_github("chuhousen/amerifluxr", upgrade = "never")

# EDIutils moved to rOpenSci. `EDIorg/EDIutils` is now only a docs/pkgdown page
# (README.md + index.html, no DESCRIPTION), so install_github("EDIorg/EDIutils")
# fails with a misleading HTTP 404.
remotes::install_github("ropensci/EDIutils", upgrade = "never")
```

Already-present deps used by the sourced helpers: tidyverse, lubridate,
data.table, readxl, stringi, dplyr, tidyr, purrr, magrittr.

## 2. Manual data step

Create the data tree first — `create_subfolders()` in the script is not recursive,
so it silently warns and does nothing if `data/` itself is missing (which then
surfaces much later as AmeriFlux's `out_dir not valid...` error):

```
daily_met/Infilling_2025/data/raw/AmeriFlux/
daily_met/Infilling_2025/data/raw/GHCNd/
daily_met/Infilling_2025/data/raw/NPN/
daily_met/Infilling_2025/data/{prep,qc,infill,homogenize}/
```

Then manually download `PRECIP_CO90_DAILY.csv` into `data/raw/NPN/` from
<https://nadp.slh.wisc.edu/networks/nadp-precipitation-network/> (select daily,
then CO90). Without it, `npn` reads as an empty tibble and the failure appears as
a confusing `rename()` error: ``Column `qualifiedPrecipInches` doesn't exist``.

## 3. EDI credentials — not needed

All `knb-lter-nwt.*` packages fetched fine unauthenticated via EDIutils. No
env var / token was required for this script.

## 4. Memory ceiling (the main blocker)

The test VM had **3.8 GB RAM and zero swap**; the script was OOM-killed
(exit 137) three times at `tidyAmeriflux()`.

Why it's memory-hungry: `fetch_data_functions.R:65` reads the AmeriFlux
half-hourly CSVs with `colClasses = "character"`. Those files are ~522 MB
(US-NR1 alone is 310 MB), and character is R's most expensive representation,
so each copy is GBs. The script then held three copies alive at once
(`ameriflux`, `ameriflux_prepped`, `ameriflux_daily`) and died in
`tidyAmeriflux()`'s `gather`/`unite`/`spread` chain. Row-wise `apply()` calls in
`prepAmeriflux()` (`prep_data_functions.R:219,224`) add further full copies.

**Fix applied** (only code change made; affects no numerical results): free the
two spent intermediates in `1_prepare_climate_data.R` —
`rm(ameriflux); gc()` after `ameriflux_prepped` is built, and
`rm(ameriflux_prepped); gc()` after `ameriflux_daily` is built. This got the run
past the OOM point.

Caveat: the commented-out `view_plots` blocks referencing `ameriflux_prepped`
(lines ~192, ~197) would need that `rm()` removed before they'd work.

If memory is still tight on a small box, add swap (needs sudo):
```
sudo fallocate -l 4G /swapfile && sudo chmod 600 /swapfile
sudo mkswap /swapfile && sudo swapon /swapfile
```

## 5. Open question

`colClasses = "character"` in `fetchAmeriFlux()` is the root memory amplifier.
Reading the numeric met columns as numeric would cut usage dramatically, but
that helper is shared by the 2022/2023/2024 scripts — unclear whether the
character read is deliberate (preserving flag columns / avoiding type coercion).
Not changed for that reason. Worth deciding before running on a small machine.

## 6. Status: full workflow runs clean

All scripts run end to end from the repo root (exit 0):

| Script | Runtime | Output |
|---|---|---|
| `1_prepare_climate_data.R` | ~5 min, ~2.7 GB | `data/prep/` |
| `2T_qc_temp_data.R` | ~40 s | `data/qc/*TEMP_qc.rds`, `*TEMP_ready.rds` |
| `2P_qc_precip_data.R` | ~30 s | `data/qc/*PPT_qc.rds` |
| `3P_gapfill_precip.R` | ~14 min | `data/infill/*PPT_infilled_draft.*` |
| `3T_gapfill.R` | ~8 min | `data/infill/*_chart_infilled_v1.*`, `sdlhmp_infilled_2025.rds` |
| `4T_homogenize_sdlts.R` | ~3 s | `data/homogenize/` |
| `5_prep_v1_forEDI.R`, `c1/`, `d1/` | ~25 s each | `data/publish/*_gapfilled_ongoing.csv` |
| `infilled_viz_check.R` | ~25 s | `data/plots/qc/` |

Changes needed to get there (beyond sections 1, 2 and 4):

- `geodist` must be installed from CRAN (used by 2T).
- 2T reads six previously QC'd files from the `long-term-trends` repo. They are now
  copied into `long-term-trends-data-copy/` (see its README) and committed via a
  `.gitignore` exception, so no sibling repo is needed.
- New D1 temperature sensors (`airtemp_hv1_*`, `airtemp_hv2_*`, from 2025-09-10)
  are split into sites `d1_cr1000_hv_1` / `_hv_2` by `tidytemp()` in
  `R/prep_data_functions.R`, and handled like HMPs in 2T.
- 2T double-counted `d1_cr1000` from 2020 on (`yr > 2018` vs `yr == 2019` in two
  filters), crashing once EDI added 2020+ rows; fixed to `yr == 2019`.
- C1/D1 step-5 scripts downloaded pinned old EDI revisions by URL; those now return
  HTTP 403, so they use `getTabular()` (newest revision) instead.
- 3T kept any metric matching `"avg|DTR"`, which let an all-NA `airtemp_s_avg` logger
  metric into `alldats`. `tk_temp_historicfill()` skips a candidate if any of its
  columns is NA on the target date, so the multi-year method silently excluded every
  NWT logger (HMPs, aspirated, hv, CRs). Now filtered to exactly `airtemp_avg`/`DTR`;
  e.g. SDL hmp_3 went from 56 to 670 of 671 days infilled from on-site sensors.
- GHCN co-op station USC00052761's whole record (temp max/min and precip) is dated
  one day late relative to calendar-day stations (r2 vs Boulder max 0.958 shifted
  vs 0.806 as dated). 2T and 2P now shift it back 1 day. The other co-op stations
  (USC00053496, USC00053116_1600, USC00053500) look like afternoon observers: max
  aligned, min partly a day late; left unshifted on purpose. `time_observed` is empty
  for temperature, so observation times could not be confirmed. The station ends
  Jan 2021, so published (2022+) outputs are unaffected.
- Units: GHCN-Daily stores PRCP in tenths of mm and TMAX/TMIN/TOBS in tenths of C
  (the NCEI "access" CSVs used by `fetchGHCND()` since 2024; CTW's original CDO
  downloads were metric). 2P and 2T now divide by 10 on read. NPN (ETI gauge) was
  converted inches -> cm in script 1; now inches -> mm. Converting precip changed
  results: 2P's cross-station check pools all stations into one z-score, so ~128
  lone large Saddle days (1985-2026) are now flagged and infilled (e.g. 127 mm on
  2019-01-22 -> 10.8 mm); 18 D1 days and 1 C1 day also changed.
- 2T SNOTEL QC: CTW's manual sensor-failure windows (Niwot tmax 2005-06-01..2007-02-01,
  University Camp tmax 2010-05-01..2011-09-01) only NA'd a working copy; only `qcflag`
  is joined back, so ~970 bad values survived. `sensor_fail` now sets `qcflag`.
- 2T AmeriFlux QC: flags were applied to `amerigl4` instead of `ameriflux` (typo); fixed.
- Review plots (not part of the pipeline, run after 2T/2P):
  `qc_review_suspect_periods.R` -> `data/plots/qc_review/` (Niwot/Univ Camp SNOTEL,
  D1 HMPs Jul 2018-Mar 2019, still undecided); `qc_2025_sources.R` ->
  `data/plots/qc_2025/` (each 2025 source vs own-site others, C1/D1 means, Boulder 14 W
  and Daymet, against its usual 2018-2024 monthly offset; precip cumulative ratios).
  Daymet downloads are cached in `data/raw/Daymet/`.
- C1 aspirated sensors on the CR1000X (2025+, `c1_cr1000x_asp_N`) are deliberately
  kept as separate sites from the CR1000 ones (`c1_cr1000_asp_N`), per decision on
  2026-09-24; they will build multi-year history on their own.
- All OneDrive paths and `2024` output names now point at `data/` / `2025`;
  script 1 also creates `data/publish/` and `data/plots/`.

Not changed on purpose:

- C1 temp output keeps the odd names `..._regression_regression_equation` and
  `infill_QAnote`, and both C1/D1 temp keep `raw_Tmean`: the NWT_metadata
  reformat scripts (`project.185`, `project.187`) expect and fix exactly these.
- Publish-window filters in step 5 (`year >= 2023` for SDL, `yr > 2021` for C1/D1)
  are unchanged; outputs run into 2026 (partial year), so clip as needed in the
  NWT_metadata reformat step.

Not yet reviewed: whether the infilled values are scientifically correct.

Running non-interactively: 2T/2P call `View()`, which needs stubbing (e.g.
`View <- function(...) invisible(NULL)`) when run via `Rscript`.

## 7. Runtime notes

- Every run re-downloads everything: all EDI packages, plus ~100 MB of AmeriFlux
  zips (`fetchAmeriFlux` has no cache/skip-if-present check). Budget ~5 min per
  attempt; a skip-if-present check would make iteration much faster.
- `view_plots` is `FALSE`; plotting was never exercised.
- Latent bug spotted but NOT hit by this script: the qa function in
  `common_qaqc_functions.R` references undefined `maxval`/`minval` in its
  `stopifnot()` (its params are `maxlim`/`minlim`). It would error if called —
  likely reachable from the 2T/2P QC scripts.
