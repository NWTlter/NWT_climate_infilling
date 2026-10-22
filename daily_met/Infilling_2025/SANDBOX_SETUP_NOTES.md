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
  `stopifnot()` (its params are `maxlim`/`minlim`). It would error if called, but
  `flag_maxmin()` is never called anywhere, so it is not reachable.

## 8. Plan for resuming (as of 2026-09-25, commit 43d9fe3)

The whole workflow runs end to end (1 -> 2T/2P -> 3P/3T -> 4T -> 5 sdl/c1/d1 ->
infilled_viz_check). Remaining work is data decisions, then a final rerun and review.

### First
0. Run `qc_2025_vs_neighbors.R` (~15 s): every 2025 series (charts included) minus its
   best-matching neighbors, over gray bands of its usual 2010-2024 range; flagged days go to
   `data/plots/qc_2025_vs_neighbors/flagged_days_*.csv`. Check these before the decisions
   below, since they may add items to the list.
   Run 2026-10-19 after the 412.18 rerun: 321 temp / 52 precip flagged days (was 329 / 52).
   D1 chart Tmax summer flags gone; new D1 chart Tmin flags 2025-08-01/02 (1 C, just
   below the usual band) and 2025-12-03 (-13 C vs ~-20 C neighbors) -- not yet reviewed.

### Decisions to make (review plots first)
1. **D1 HMPs 1-3, ~2018-07-25 to ~2019-03-26**: stop tracking temperature (HMP =
   6.2 + 0.38 x chart; min up to +12 C vs Boulder 14 W); not flagged in EDI, so they
   pass 2T QC. They feed the multi-year regressions for D1 chart / Saddle HMPs.
   Plots: `data/plots/qc_review/3_d1_hmps_2018-2019.png`, `zoom_d1_hmps_start/end.png`,
   and `d1_2018_temp_airtemp_max/min.png` (vs neighbors; `qc_review_d1_2018.R`). Those
   confirm 2018-07-25 through 2019-03-26 (partly off that day; fine from 03-27) for all
   three HMPs, which read nearly identical values throughout; D1 chart and old CR1000 fine.
   **Resolved upstream (2026-10-19):** these periods are now `q`-flagged in the EDI
   source data. 2T NAs any NWT logger value whose flag is not `n` (`# Clean out flagged`,
   ~line 167), so they drop out once script 1 re-downloads; no 2T change needed.
   Confirmed in the 2026-10-19 rerun: all 3 HMPs fully `q`-flagged over the window.
2. **D1 chart daily max, ~May-Aug 2025, ~2 C cold** vs own-site sensors, Boulder 14 W
   and Daymet (HMP 1 is steady over the same months). The chart is the infill target,
   so its raw values are kept as measured. Decide: NA and infill, or keep and note.
   Plots: `data/plots/qc_2025/temp_d1_airtemp_max.png`, `temp_monthly_departure_heatmap.png`.
   **Resolved (2026-10-19):** now `q`-flagged in the EDI source data, and 2T changed so
   ALL `q`-flagged chart values are NA'd and infilled (previously kept unless they failed
   later checks). Two places in 2T: the `charttemp_out` flag filter (~line 124) and the
   `charttemp_c1d1` raw restore for 2019+ (~line 545), which had put raw back regardless
   of flag. In the pre-redownload data this NAs ~680 c1/d1 chart values from 2019 on
   (d1: 250 in 2019, 184 in 2024), plus 6 `m`-flagged values that had raw readings
   (also NA'd and infilled, decided 2026-10-19).
   Confirmed in the 2026-10-19 rerun with D1 chart knb-lter-nwt.412.18: 91 of 123
   May-Aug 2025 Tmax days `q`-flagged, and no May-Aug 2025 day is published as raw
   (flag_2: 83 C = Tmax adjusted from raw Tmin, 35 B = both predicted, 33 of them
   because the chart has no Tmin that day, 5 B*). Needs a look in the scientific review.
   2011-2018 C1/D1 come from the CTW QC files and are unaffected.
3. **Boulder 14 W (USW00094075) precip 0.0 mm on 2025-12-03** while Niwot 20.3, NPN 20.1,
   C1 16: possible missed snow event (US-NR1 carries the same 0.0, see below).
   **Decided 2026-10-19: keep as 0.0** -- precip is patchy, so it may really have been 0.
   - US-NR1 precip is independent only 2003-2010 (own gauge, r 0.89-0.98, ~10-20% lower
     totals). 2011-2022 it is AmeriFlux gap-fill and 2023+ it reports the same gauge as
     USW00094075 (r = 1, identical annual totals). Decided 2026-09-25: keep US-NR1 as a
     precip source in case it returns to an independent gauge (comment in 2P section 3).
   - US-NR1 temperature is its own sensor (Tmax ~1.7 C cooler than Boulder 14 W, Tmin
     ~2.4 C warmer); no issue.
4. **Co-op afternoon observers** (USC00053496, USC00053116_1600, USC00053500): daily min
   partly a day late; left unshifted (only USC00052761 is shifted). USC00059175
   (Winter Park, 08:00) precip aligns best shifted -1 day; not shifted. Revisit only
   if they start being chosen as sources.

### Then
5. Rerun from script 1 to pick up the new EDI `q` flags (1 -> 2T -> 3T -> 4T -> 5 x3 ->
   infilled_viz_check; ~20 min; add 2P/3P if precip changes, ~20 min more).
   Scripts must run from the repo root; 2T/2P call `View()`, so stub it when using Rscript.
6. Rerun `qc_2025_sources.R` and `qc_review_suspect_periods.R`; check the heatmap and
   the new infill plots in `data/plots/qc/`. Rerun `qc_2025_vs_neighbors.R` (step 0)
   to confirm any NA'd periods no longer flag.
7. Scientific review of the 2025 infilled values: first pass 2026-10-19 with
   `qc_review_2025_infill.R` (~1 min) -> `data/plots/qc_review_2025_infill/`. Infilled
   temp tracks independent neighbors (SNOTEL, Boulder 14 W, US-NR1) as well as raw days
   with no monthly bias at C1/D1/SDL. Open points: D1 chart Tmin kept raw Apr-Aug while
   Tmax was bad (Apr raw Tmin ~1.5 C warm vs usual); D1 DTR fits weak (median r2 ~0.3);
   D1 2025-12-13..16 15.2 mm accumulation while neighbors dry; SDL 2025-02-20 infilled
   from a negative-slope r2 0.17 fit; SDL/D1 2025 precip 2nd lowest since 1990 (raw, not
   infill; region dry too). Then hand
   `data/publish/*_gapfilled_ongoing.csv` to the NWT_metadata reformat scripts
   (projects 184-187 and SDL). Note outputs run into 2026 (partial year) and the
   step-5 publish filters (SDL `year >= 2023`, C1/D1 `yr > 2021`) are unchanged.

### Worth knowing
- Unit, QC and date fixes in this branch also change pre-2025 values (e.g. ~128 Saddle
  precip days since 1985) relative to what was published in earlier years.
- Daymet 2025 runs cool vs 2018-2024 for every sensor, so use it as a weak reference.


## 9. Method changes and re-infill comparison (2026-10-19)

- `reject_neg_slope` option added to the precip (`tk_ppt_*fill`) and temp (`tk_temp_*fill`)
  infill functions in `daily_met/R/`; default `F` (2022-2024 unchanged), `T` in the 2025
  3P/3T calls. Temp rejects a source if either the tmean or DTR fit has slope <= 0.
  Effect: SDL precip 128 days changed (net +118 mm over 1980-2025; 110 negative fits gone);
  D1 temp 6 published days changed; C1 none.
- Bug fixes for all years: precip seasonal tie-break picked the highest p-value (now lowest);
  `mean_and_diurnalT()` now computes tmean for sites that never report one (21x loggers).
- 2T keeps 21x values (their packages have no flags); CTW's QC flags are applied via
  `logger_prep` as for other loggers. 21x is a usable source for 1986-2000.
- 3T now infills C1/D1 from 1980 (was 2020); SDL HMPs still from 2020 (record starts 2018).
  Published C1/D1 temp still starts 2022, so this only feeds the comparison.
- `qc_compare_old_infill.R` compares with the published EDI infill (knb-lter-nwt.184-187),
  1980-2024 -> `data/plots/qc_compare_old_infill/`. Raw days identical in both. Days infilled
  in both: same source sd of diff ~0.8 C, different source ~2.1 C, no bias. Precip before
  2011 was stitched from the old TK/CTW series; now `new_infill_start <- 1980` in 3P (~line 480;
  set to 2011 to restore). 1980-2010 C1/D1 precip: raw days identical; annual totals within 5%;
  D1 raw 1983-01-14 (33 mm), 1986-04-22 (20 mm), 2010-01-11 (15 mm) now removed by 2P QC and
  infilled as 0 -- user to review.
- Pre-1980 re-infill would need Estes Park (USC00052759), Fraser (USC00053113), Berthoud
  Pass (USC00050674), Caribou Ranch (USC00051342) added to script 1 and the 1980 cut removed
  in 2T. DP211 loggers and Loch Vale are not available.

**CTW 2011-2018 "predicted chosen over adjusted" days (2026-10-22).** 59 C1/D1 days (24 C1, 35 D1) that the old
published data infilled with both tmax/tmin predicted (`infill_QA_note` = "Predicted infill chosen over known
temperature-adjusted infill"; methods II(D) in knb-lter-nwt.185/187) were being published raw by the new pipeline.
2T now NAs the 2011-2018 CTW chart values on those days so 3T predicts both sides, matching CTW's choice. Only those
59 days changed; |new - old| median 0.8 C (was 1.0 C with the raw values). CTW's artificial-drop shifts (+1/+3/+4 C)
were already applied via `qa_temp`, so no extra adjustment was needed.
