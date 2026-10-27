# How to update the daily met infill through 2026 (run in 2027)

Step-by-step guide for extending the C1, D1 and Saddle daily temperature and precipitation
infill through 2026-12-31, starting from this folder (the 2025 run). Written 2026-10-26.
Setup details and the reasons behind each 2025 change are in `SANDBOX_SETUP_NOTES.md`
and `CHANGES_2025.md`.

Outputs (same six EDI packages as 2025):

| EDI | Dataset | Published file |
|---|---|---|
| knb-lter-nwt.185 | C1 temp | `c1_infilled_temp_daily.tk.data.csv` |
| knb-lter-nwt.187 | D1 temp | `d1_infilled_temp_daily.tk.data.csv` |
| knb-lter-nwt.184 | C1 precip | `c1_infilled_precip_daily.tk.data.csv` |
| knb-lter-nwt.186 | D1 precip | `d1_infilled_precip_daily.tk.data.csv` |
| knb-lter-nwt.314 | Saddle temp | `sdl_daily_airtemp_gapfilled.cw.data.csv` |
| knb-lter-nwt.315 | Saddle precip | `sdl_daily_precip_gapfilled.cw.data.csv` |

---

## 0. Before you start

1. **Check that the 2026 source data are on EDI.** Each NWT chart and logger package should
   run through 2026-12-31: charts 411–416 (C1/D1/SDL temp and precip) and the C1, D1 and
   SDL daily logger packages. Ask the IM team about any `q` (questionable) flags still to
   be added for 2026. The workflow NAs and infills `q`-flagged chart values, so flags added
   after the run won't be picked up.
2. **Decide whether to append 2026 or republish the whole record.**
   - **Append (recommended unless the methods change):** the rows posted in 2025 stay
     exactly as they are, and only 2026 is added. The regressions are still refit on the
     full record, so 2026 benefits from it.
   - **Full republish:** as in 2025, every row from 1980 is replaced (1986-12-06 for
     Saddle temp, 1987-06-01 for Saddle precip). Use this only if a method or QC fix
     should reach back through the record. Values for earlier infilled days will shift a
     little because the regressions are refit with one more year.

   Step 3.4 sets this.
3. **Machine.** R ≥ 4.5 and at least 4 GB RAM (script 1 peaks at ~2.7 GB; add swap on a
   small VM). Package setup is in `SANDBOX_SETUP_NOTES.md` §1. `amerifluxr` and `EDIutils`
   install from GitHub, not CRAN. `geodist` is also needed.
4. **Branch.** Make a new branch off the merged 2025 work, e.g. `infilling_2026`.

## 1. Make the 2026 folder

Each year has its own folder (`daily_met/Infilling_2022` … `Infilling_2025`). From the repo root:

```bash
cd daily_met
mkdir Infilling_2026
# scripts and docs only, not data/ or the long-term-trends copy
cp Infilling_2025/*.R Infilling_2025/readme.md Infilling_2026/
mkdir Infilling_2026/c1 Infilling_2026/d1
cp Infilling_2025/c1/*.R Infilling_2026/c1/
cp Infilling_2025/d1/*.R Infilling_2026/d1/
# data tree (create_subfolders() in script 1 is not recursive; it fails quietly if data/ is missing)
mkdir -p Infilling_2026/data/raw/{AmeriFlux,GHCNd,NPN} \
         Infilling_2026/data/{prep,qc,infill,homogenize,publish/full_record} \
         Infilling_2026/data/plots/{qc,qc_review,qc_2025,qc_2025_vs_neighbors,qc_review_2025_infill,qc_compare_old_infill}
# point every path at the new folder
grep -rl "Infilling_2025" Infilling_2026 | xargs sed -i 's/Infilling_2025/Infilling_2026/g'
```

Then:

- **Keep reading the long-term-trends QC copy from the 2025 folder.** It is 12 MB and
  doesn't change. In `Infilling_2026/2T_qc_temp_data.R` (~line 484), set it back:
  `lttcopy <- "daily_met/Infilling_2025/long-term-trends-data-copy/"`
- **Ignore the new data folder in git.** Add `daily_met/Infilling_2026/data/` to
  `.gitignore`, next to the 2025 line.
- **Add the ETI gauge file by hand.** Download `PRECIP_CO90_DAILY.csv` into
  `Infilling_2026/data/raw/NPN/`. Get it from
  <https://nadp.slh.wisc.edu/networks/nadp-precipitation-network/>: choose daily, then
  CO90. This is the only manual download. Without it, script 1 fails with
  ``Column `qualifiedPrecipInches` doesn't exist``.
- **Check the AmeriFlux credentials** in `1_prepare_climate_data.R` (~line 68). They are
  currently Miles Moore's (`MilesMooreNWT`). Replace them with your own account if needed.

## 2. Reading the posted EDI tables (no edits needed)

Several scripts read the **posted** EDI tables (newest revision) to learn which values were
infilled or adjusted before. Examples: 2T reads C1/D1 temp (185/187), 2P reads C1/D1 precip
(184/186), and `2T_sdl_homogenized.R` reads Saddle temp (314). In 2025 these were still
TK's and CTW's tables. Once the 2025 republish is on EDI, they will be the 2025 output.

This works without pinning the reads to the old revisions:

- `6_build_full_record.R` writes CTW's "Predicted infill chosen over known
  temperature-adjusted infill" note (2011–2018: 25 C1, 41 D1 days) into the republished
  C1/D1 temp tables. That is the one thing 2T looks for that isn't in the flags.
- Tested 2026-10-26: 2T, `2T_sdl_homogenized.R` and 2P were rerun with those five tables
  replaced by the 2025 full-record files, with all other inputs the same. Results:
  - **Precip:** identical.
  - **Saddle temp:** two measured days the 2025 infill had already replaced (1994-02-09
    max, 2003-11-21 min) were NA'd up front instead. Since the switch to the per-logger fill
    (2026-10-27) the 2025 infill keeps both measured values, so this should no longer
    happen (not retested).
  - **C1/D1 temp:** 46 of ~100,000 measured chart values change. 30 are measured values the 2025
    infill had already replaced (flag `B*`), so they are infilled either way. The other 16
    are raw values that the rank-based spatial QC in 2T now removes, because the neighbouring
    values changed. Adding a year of data moves those ranks anyway, so a few raw days changing
    each year is expected.
- The step-5 scripts and 3P read 184–187 only for column names and order, which don't change.

Check: `2T_sdl_homogenized.R` prints `table(metric, ctw_infilled)`. Compared with the 2025
run, it should show 2–3 more infilled per metric (the days above) and nothing else.

If something here goes wrong, the tables as TK and CTW posted them are still on EDI: 184
revision 8, 185 revision 5, 186 revision 6, 187 revision 5, 314 revision 4. Reading one
would need a `version` argument added to `getTabular()` in `daily_met/R/fetch_data_functions.R`,
which currently always reads the newest revision. EDIutils reads old revisions fine.

## 3. Year edits

### 3.1 Pipeline (needed for correct output)

| File | Line (approx.) | Change |
|---|---|---|
| `6_build_full_record.R` | 22 | `last_date <- as.Date("2026-12-31")` |
| `6_build_full_record.R` | 25–30 | `new_start` / `keep_posted`: see 3.4 |
| `3T_gapfill.R` | 315–316 | output names `sdlhmp_infilled_2025.rds`, `sdlhomog_infilled_2025.rds` → `_2026` |
| `4T_homogenize_sdlts.R` | 28, 31 | match those names: `sdlhmp_infilled_2026`, `sdlhomog_infilled_2026` |

**Leave these alone:**
- The 1980, 2018, 1986 and 1987-06-01 infill starts.
- The 2025 QC review list in `2T_sdl_homogenized.R` (`reviewed_remove`) and its
  "(2025 QC)" flag text, which records when those values were removed.
- The D1 2012-01-16 qdays fix in `2P` and `3P`.
- The SNOTEL sensor-failure windows in `2T`.

These are fixes to the historical record, not settings for the run year.

Nothing else needs a year change for the infill itself. 3T and 3P infill every missing
day from the start year through the last day of data, and step 5 writes everything.

### 3.2 Diagnostic plots inside the pipeline (needed only so the plots show 2026)

| File | Line (approx.) | Change |
|---|---|---|
| `2T_qc_temp_data.R` | 72–80, 163, 229 | `2024` → `2025`, `2025-01-01` → `2026-01-01` (drawn only if `generate_plots`) |
| `3T_gapfill.R` | 118, 160 | `xlim` end `2025-12-31` → `2026-12-31` |
| `3T_gapfill.R` | 211–220 | `yr == 2025` → `yr == 2026` |
| `c1/5_prep_v1_forEDI_c1.R` | 196 | `xlim` end → `2026-12-31` |
| `infilled_viz_check.R` | 102, 144, 146, 220, 224, 250, 254, 256, 328, 336, 359, 395 | `2025` → `2026` in the year loops (`c(2025)` → `c(2026)`; `c(2024, 2025)` → `c(2025, 2026)`), the `xlim` ranges and the file names |

### 3.3 Review scripts

| File | Change |
|---|---|
| `qc_2025_sources.R` | `yr_qc <- 2026` (line 28). Optionally rename the script and output folder to `qc_2026_*`. |
| `qc_2025_vs_neighbors.R` | `qc_start` / `qc_end` → 2026-01-01 / 2026-12-31; `ref_range` end → 2025-12-31 (lines 41–43). In `proxy_hist` (~line 62), the C1 CR1000X sensors borrow CR1000 history only because they had none. With 2025 history of their own, consider removing that line. |
| `qc_review_2025_infill.R` | `review_yr <- 2026`, `clim_yrs <- 2010:2025` (lines 19–20). Labels and column names still hard-code 2024/2025 (lines 92, 134, 148, 223, 226, 229, 259, 275): update them to match. |
| `qc_compare_republish.R` | For an append run, set every `new_start` (line ~18) to `2026-01-01`. That only moves the dotted line; every earlier year should then show no difference. For a full republish, leave it as is. |
| `qc_compare_old_infill.R` | Compares against the published infill for 1980–2024 (from 2027, the 2025 republish). Optional; `qc_compare_republish.R` covers the same ground. |

### 3.4 Append vs full republish (`6_build_full_record.R`, `datasets` table)

- **Append:** for all six rows, `new_start = as.Date("2026-01-01")` and `keep_posted = TRUE`.
  The posted 1952–2025 rows (including the 2025 republish) stay byte-identical; they are
  already at ≤3 decimals, so the script's 3-decimal rounding only touches the new rows.
- **Full republish:** keep the 2025 settings, which are already in the file.

## 4. Run the scripts, in order

All scripts run from the **repo root**. `2T` and `2P` call `View()`, so run them
non-interactively through a wrapper that stubs it. Several scripts call `rm(list = ls())`,
so the stub must survive that. Save this as e.g. `~/run_one.R` (outside the repo):

```r
# usage: Rscript ~/run_one.R <script>   (run from repo root)
f <- commandArgs(TRUE)[1]
exprs <- parse(f)
env <- new.env(parent = globalenv())
assign("View", function(...) invisible(NULL), envir = globalenv())
for (e in exprs) {
  eval(e, envir = env)
  if (!exists("View", envir = globalenv(), inherits = FALSE))
    assign("View", function(...) invisible(NULL), envir = globalenv())
}
cat("DONE", f, "\n")
```

With `D=daily_met/Infilling_2026`:

| # | Script | Time | Writes | Check before moving on |
|---|---|---|---|---|
| 1 | `$D/1_prepare_climate_data.R` | ~5 min | `data/prep/`, `data/raw/` | All EDI, GHCN, SNOTEL and AmeriFlux downloads succeed. The `data/prep/*.rds` files run into 2026. Look for any **new site names** (step 5). |
| 2 | `$D/2T_qc_temp_data.R` | ~1 min | `data/qc/*TEMP_qc.rds`, `*TEMP_ready.rds`, `siteinfoTEMP_qc.rds` | Console summaries; no stop. |
| 3 | `$D/2T_sdl_homogenized.R` | ~1 min | `data/qc/sdlhomogTEMP_qc.rds`; `plots/qc_review/sdl_homog_qc_candidates.png/.csv` | Counts as in section 2. The candidates should be the same as in 2025, already reviewed. Anything new needs review. |
| 4 | `$D/2P_qc_precip_data.R` | ~30 s | `data/qc/*PPT_qc.rds` | Console summaries. |
| 5 | **QC review of the 2026 source data**: see section 5 | | | **Stop here and review** before infilling. |
| 6 | `$D/3T_gapfill.R` | ~8–10 min | `data/infill/*_chart_infilled_v1.*`, `sdlhmp_infilled_2026.rds`, `sdlhomog_infilled_2026.rds` | Runs in the background fine. |
| 7 | `$D/3P_gapfill_precip.R` | ~14 min | `data/infill/*PPT_infilled_draft.*` | Can run alongside 3T (they don't share outputs). |
| 8 | `$D/4T_homogenize_sdlts.R` | ~5 s | `data/homogenize/` | The `stopifnot` passes (CR series ends before the HMPs start). |
| 9 | `$D/5_prep_v1_forEDI.R` (Saddle) | ~25 s | `data/publish/sdl_*_ongoing.csv` | |
| 10 | `$D/c1/5_prep_v1_forEDI_c1.R` | ~25 s | `data/publish/c1_*_ongoing.csv` | |
| 11 | `$D/d1/5_prep_v1_forEDI_d1.R` | ~25 s | `data/publish/d1_*_ongoing.csv` | |
| 12 | `$D/6_build_full_record.R` | ~1 min | `data/publish/full_record/*.csv` | Its summary table: `first`/`last` dates, posted rows kept, new rows (append: 365 each), no duplicate or missing dates (it stops if there are). |
| 13 | Review scripts: see section 6 | | | |

```bash
cd /path/to/NWT_climate_infilling
D=daily_met/Infilling_2026
for s in 1_prepare_climate_data.R 2T_qc_temp_data.R 2T_sdl_homogenized.R 2P_qc_precip_data.R; do
  Rscript ~/run_one.R $D/$s || break
done
# ...review (section 5), then:
for s in 3T_gapfill.R 3P_gapfill_precip.R 4T_homogenize_sdlts.R 5_prep_v1_forEDI.R \
         c1/5_prep_v1_forEDI_c1.R d1/5_prep_v1_forEDI_d1.R 6_build_full_record.R; do
  Rscript ~/run_one.R $D/$s || break
done
```

To rerun only temp after a QC change: 2T → 2T_sdl_homogenized → 3T → 4T → step 5 ×3 → 6.
To rerun only precip: 2P → 3P → step 5 ×3 → 6. Step 5 writes temp and precip together,
so it always runs whole.

## 5. QC review of 2026 source data (between 2P and 3T)

Plot suspect periods and decide with the team **before** removing any values. Prefer
getting a `q` flag added upstream on EDI over NA-ing values in code.

1. `qc_2025_vs_neighbors.R` (~15 s) → `plots/qc_2025_vs_neighbors/`
   - `temp_airtemp_max.png`, `temp_airtemp_min.png`: every 2026 series minus its
     neighbors, over its usual band. Look for drift, steps or a sensor that stops
     tracking (like the D1 HMPs in 2018–19).
   - `precip_daily.png`, `precip_30day_ratio.png`: gauges reading persistently
     high or low against neighbors.
   - `flagged_days_temp.csv`, `flagged_days_precip.csv`: the days outside the band.
2. `qc_2025_sources.R` → `plots/qc_2025/`
   - `temp_monthly_departure_heatmap.png`: which sensor or month departs from its usual offset.
   - `temp_{c1,d1,saddle,regional}_airtemp_{max,min}.png`: drill down.
   - `precip_cumulative_ratio.png`: gauge undercatch or overcatch over the year.
3. Re-check the decisions from 2025 that may change in 2026:
   - **Does US-NR1 precip have its own gauge again?** It is used only for 1998–2010
     (`2P` ~line 656) because from 2011 it copies Boulder 14 W.
   - **Do the co-op stations still report?** USC00052761 ended Jan 2021. If a co-op
     station starts being chosen as a source, check its observation time (§9 of the notes).
   - **Is the US-NR1 Boulder 14 W precip still identical** (r = 1, same annual totals)?
4. **New sensors or loggers.** Script 1 maps logger columns to sites in `tidytemp()`
   (`daily_met/R/prep_data_functions.R`). In 2025, the D1 hv sensors (`_hv1/_hv2`) and C1
   CR1000X aspirated sensors needed handling there. `2T` then treats anything matching
   `hmp|_as|_hv` as an HMP-type series (~line 590). A new naming pattern needs adding in
   both places, or it is silently dropped. Sources are ranked automatically in `2T`, so
   nothing else needs editing.

## 6. Review the results (after step 6, before handing to EDI)

1. **`infilled_viz_check.R`** (~25 s) → `plots/qc/{c1,d1,sdl}/`. Infilled days are red
   points over the other NWT sites. Check:
   - `temp/airtemp_{max,min,avg}_*_infilled_2026.png`: infilled stretches follow the neighbors.
   - `sdl/hmps/hmp{1,2,3}/…_2026.png`: each Saddle HMP.
   - `ppt/precip_*_ppt_infilled_2026{1..12}.png`: month by month, infilled precip
     against the other two gauges.
2. **`qc_review_2025_infill.R`** (~1 min) → `plots/qc_review_2025_infill/`.
   - `temp_anomaly_vs_region.png`, `temp_monthly_offset.png`: infilled days track the region
     as well as raw days do.
   - `temp_infilled_outliers.csv`: infilled days > 3 sd from the region.
   - `temp_regression_strength.csv`: weak fits (D1 DTR was weak in 2025, median r² ~0.3).
   - `precip_infilled_days.csv`: large infills while the neighbors were dry (e.g. D1
     2025-12-13..16).
   - `precip_annual.csv`: where 2026 ranks.
3. **`qc_compare_republish.R`** →
   `plots/qc_review/republish_vs_posted_{c1,d1,saddle}.png`. Annual and seasonal means
   (temp) and totals (precip), posted vs new.
   - **Append run:** the two lines should overlap exactly before 2026.
   - **Full republish:** see where the new infill moves the record.

   Also check the **2025/2026 boundary.** A precip reading in early January 2026 can cover
   days back into December 2025. In an append run those December rows keep their posted
   values, so check that the accumulation isn't double counted. Search `qdays_qcnote` /
   `raw_qdays` in the new rows for the first weeks of January.
4. Read the infill notes and flags in the new rows of each `full_record/` file.
   C1/D1 temp `Tmax_QAflag`/`Tmin_QAflag` are carried from posted rows only; 2026 rows
   have none unless added by hand.

## 7. Hand off

1. Update a `CHANGES_2026.md` (start from the template of `CHANGES_2025_by_dataset.md`).
   It covers decisions made, new flags, new sensors, and anything left open. Also review the
   "**To ponder for next year**" section in `CHANGES_2025.md` §6: the Saddle CR21x
   adjustment, early CR21x Tmax running warm. That fix would go in `2T_sdl_homogenized.R`
   and needs a full Saddle temp republish.
2. Give the six `data/publish/full_record/*.csv` files to the IM team as replacement
   tables. The NWT_metadata reformat scripts only append after the last posted date and
   expect the step-5 column names. `6_build_full_record.R` already applies their column
   fixes, so use the `full_record/` files directly.
   - **Append:** update the package temporal coverage to 2026-12-31.
   - **Republish:** also update the methods text.
3. Commit the `Infilling_2026` scripts and docs (not `data/`) on the feature branch.
