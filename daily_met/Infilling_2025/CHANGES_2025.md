# 2025 infilling: all code changes and what was reprocessed

Branch `AI_infilling_2025`. Compares `daily_met/Infilling_2025/` and `daily_met/R/`
with the 2024 workflow (`daily_met/Infilling_2024/`, commit `acd4271`). Per-dataset
summaries are in `CHANGES_2025_by_dataset.md`; working notes and reasoning are in
`SANDBOX_SETUP_NOTES.md`.

## Contents

1. [What was reprocessed](#1-what-was-reprocessed)
2. [Changes that alter results](#2-changes-that-alter-results)
   - [2.1 Data inputs and units](#21-data-inputs-and-units)
   - [2.2 Temperature QC (2T)](#22-temperature-qc-2t)
   - [2.3 Precipitation QC (2P)](#23-precipitation-qc-2p)
   - [2.4 Temperature infilling (3T, temp_infill_functions.R)](#24-temperature-infilling-3t-temp_infill_functionsr)
   - [2.5 Precipitation infilling (3P, ppt_infill_functions.R)](#25-precipitation-infilling-3p-ppt_infill_functionsr)
   - [2.6 Homogenization and publishing (4T, step 5)](#26-homogenization-and-publishing-4t-step-5)
3. [Changes that only make the workflow run](#3-changes-that-only-make-the-workflow-run)
4. [New review scripts (not part of the pipeline)](#4-new-review-scripts-not-part-of-the-pipeline)
5. [Not changed on purpose](#5-not-changed-on-purpose)
6. [Open items](#6-open-items)
7. [Commits](#7-commits)

---

## 1. What was reprocessed

The whole workflow was rerun from script 1, with fresh downloads of all EDI, SNOTEL,
GHCN-Daily, AmeriFlux and NADP inputs:

1 → 2T / 2P → 3T / 3P → 4T → step 5 (SDL, C1, D1) → `6_build_full_record.R` → `infilled_viz_check.R`

**2025 republishes each dataset from the start of its re-infill**, so the fixes reach
back through the record instead of applying only to the newest years. Earlier rows are
kept exactly as posted on EDI.

| Dataset (EDI) | Kept as posted | New this year | 2024 workflow published |
|---|---|---|---|
| C1 temp (185) | 1952 – 1979 | 1980 – 2025 | 2022 on, appended |
| D1 temp (187) | 1952 – 1979 | 1980 – 2025 | 2022 on, appended |
| C1 precip (184) | 1952 – 1979 | 1980 – 2025 | 2022 on, appended |
| D1 precip (186) | 1952 – 1979 | 1980 – 2025 | 2022 on, appended |
| SDL temp (314) | none (CTW's measured values reused, see 2.4) | 1986-12-06 – 2025 | 2023 on, appended |
| SDL precip (315) | none (posted 1981 – May 1987 dropped) | 1987-06-01 – 2025 | 2023 on, appended |

- **Why 1980 for C1/D1:** it is the earliest year the current neighbor sources support.
  2T drops GHCN before 1980, and SNOTEL starts 1979–80. Going earlier would need older
  co-op stations added to script 1: Estes Park USC00052759, Fraser USC00053113, Berthoud
  Pass USC00050674, Caribou Ranch USC00051342. The 1980 cut in 2T would also need removing.
- **SDL temp, whole record:** CTW's homogenization of the pre-HMP loggers (CR21x
  1986–2000, CR23x 2000–2012, CR1000 2012–2017: raw + a constant per logger and variable,
  CR21x 0, plus manual fixes) is kept. Only CTW's infill is replaced (2.4). The HMPs are
  infilled from 2018, when they were installed.
- **Why 1987-06-01 for SDL precip:** see 2.5. The posted Jun–Sep 1981–86 values are dropped too.
- **Files:**
  - `data/publish/full_record/`: one file per dataset, named and laid out exactly like
    the EDI table, ending 2025-12-31.
  - `data/publish/*_ongoing.csv`: the new period only, still in the step-5 layout. The
    NWT_metadata reformat scripts append only rows after the last posted date, so they
    would ignore the earlier years.
- **Comparison with the old series:** `qc_compare_old_infill.R` compares 1980–2024
  against what is on EDI (knb-lter-nwt.184–187).

---

## 2. Changes that alter results

### 2.1 Data inputs and units

| Change | Where | Affects | Effect |
|---|---|---|---|
| GHCN-Daily precip converted from tenths of mm to mm (÷10) | 2P | All precip | The NCEI "access" CSVs used by `fetchGHCND()` since 2024 store tenths. The log-scale regressions are not affected by the scale, but the 2P cross-station check is: it pools all stations into one z-score, so the oversized GHCN values had hidden outliers. 15 more SDL days, 6 D1 and 1 C1 are now flagged (see 2.3). |
| GHCN-Daily Tmax/Tmin/TOBS converted from tenths of °C to °C (÷10) | 2T | All temp | 2T's range limits (e.g. −60 to 50) assume °C. In tenths, most GHCN values fell outside them and were dropped, so the co-op stations were largely unused as temperature sources. |
| NADP/NPN ETI gauge (CO90, at C1) converted inches → mm (was inches → cm) | 1 | All precip | Now in the same units as the other gauges. |
| ETI gauge (`NPN`) added as a precip infill source | 3P | All precip | It was commented out in 2024. It is an independent gauge (daily r ≈ 0.55 with Boulder 14 W and C1). Record starts Dec 2012, with full years from 2016. Chosen on 16 C1 and 44 SDL days. |
| `PRECIP_CO90_DAILY.csv` read with `skip = 1` | 1 | All precip | The file's first line is metadata. It must be downloaded by hand from NADP (see `SANDBOX_SETUP_NOTES.md` §2). |
| ETI gauge coordinates offset very slightly from C1 | 2P | All precip | With identical coordinates the site-distance ranking dropped it as a duplicate of C1. |
| GHCN co-op USC00052761 shifted back 1 day | 2T, 2P | Pre-2021 temp and precip | Its whole record is dated one day late (r² vs Boulder Tmax 0.958 shifted vs 0.806 as dated). The station ends Jan 2021, so this affects 1980–2020 values. Other co-op stations were checked and left as is. |
| New D1 temperature sensors `airtemp_hv1_*` / `hv2_*` (from 2025-09-10) | `R/prep_data_functions.R` (`tidytemp()`), 2T | D1 temp, SDL temp | Split into sites `d1_cr1000_hv_1` / `_hv_2` and handled like HMPs. They are sources after the D1 HMPs end (Aug 2025). |
| C1 aspirated sensors on the CR1000X (2025+) kept as separate sites (`c1_cr1000x_asp_N`) | 2T (by design), viz | C1 temp | Decided 2026-09-24: they build their own multi-year history rather than joining the CR1000 ones. |
| CTW's 2018 QC'd temperature files copied into the repo | 2T, `long-term-trends-data-copy/` | All temp | Same files as before, so no change in values. They were read from a sibling `long-term-trends` checkout and are now in this repo (see its README). |
| EDI packages read with `EDIutils` (newest revision) | `R/fetch_data_functions.R`, C1/D1 step 5 | All | Pinned pasta URLs to old revisions now return HTTP 403. This picked up upstream EDI changes, notably the new `q` flags (2.2). |

### 2.2 Temperature QC (2T)

| Change | Affects | Effect |
|---|---|---|
| **Chart values flagged `q` (questionable) are now NA'd and infilled.** Before, they were kept unless they failed later checks. Two places: the `charttemp_out` flag filter, and the 2019+ raw-restore step, which put raw back regardless of flag. `m`-flagged values that have readings are also NA'd. | C1/D1 temp | About 680 C1/D1 chart values from 2019 on. Includes D1 chart Tmax May–Aug 2025 (≈2 °C cold; 91 of 123 days `q`-flagged upstream), now infilled rather than published raw. |
| D1 HMPs 1–3, 2018-07-25 to 2019-03-26: stopped tracking temperature. Now `q`-flagged in the EDI source data, and dropped by the existing "NA any logger value not flagged `n`" rule. | D1, SDL, C1 temp (as a source) | Removes a bad source from the multi-year regressions. No code change; picked up by re-downloading. |
| 21x loggers kept. Their EDI packages carry no flags, so the "not `n`" rule dropped every value. They are now treated as unflagged, and CTW's QC flags still apply via `logger_prep`. | Re-infill 1986–2000 | The 21x loggers are a usable source for the early years again. |
| CTW's 2011–2018 "Predicted infill chosen over known temperature-adjusted infill" days are NA'd. CTW replaced the raw value with a fully predicted pair (methods II(D), knb-lter-nwt.185/187); the new pipeline now does the same. | C1/D1 2011–2018 | 59 days (24 C1, 35 D1). Median \|new − old\| 0.8 °C. CTW's +1/+3/+4 °C artificial-drop shifts were already in `qa_temp`. |
| SNOTEL manual sensor-failure windows now actually applied. They only NA'd a working copy, and only `qcflag` was joined back. `sensor_fail` now sets `qcflag`. | All temp (source) | About 970 bad Niwot / University Camp Tmax values removed. With Tmax gone, DTR is missing too, so those days are excluded from all fits. |
| Niwot SNOTEL Tmax window extended 2007-02-01 → 2007-03-10 | All temp (source) | Tmax still ran ~1.3 °C warm vs Boulder 14 W in that stretch. Effect of the extension alone: 1 C1 day (1991) by ≤0.03 °C. |
| AmeriFlux temperature QC flags applied to `ameriflux` (was a typo, `amerigl4`) | All temp (source) | US-NR1/NR3/NR4 outliers flagged by 2T are now removed. |
| D1 CR1000 2019 filter fixed (`yr == 2019`; was `yr > 2018` in one of two places) | D1 temp | It double-counted D1 CR1000 from 2020 on and crashed once EDI added 2020+ rows. |
| Chart outlier QC now reaches 1980–2010 | C1/D1 1980–2010 | A consequence of re-infilling from 1980. 37 pre-2011 days that were raw in the old series are now flagged and infilled. |

### 2.3 Precipitation QC (2P)

| Change | Affects | Effect |
|---|---|---|
| Cross-station check now sees GHCN in mm (2.1). The rules themselves are CTW's, unchanged since v1. | All precip | 109 SDL days flagged in total, 94 of them already flagged in earlier years. New: 15 SDL days (1990–2019, 33–127 mm, e.g. 2019-01-22 127 → 10.8 mm), D1 1983-01-14, 1986-04-22, 2010-01-11 (plus 3 pre-1980), and C1 2024-03-16 (15 → 0 mm). Reviewed and kept (`data/plots/qc_review/ppt_new_flags_sdl_c1.png`). |
| US-NR1 precip written out for infilling only for 1998–2010. From 2011 it is Boulder 14 W (USW00094075) re-reported (r = 0.999–1.000, same annual totals). It duplicated that source and let days NA'd at Boulder 14 W back in. It still feeds the cross-station check, which CTW's rules need (`ameri_pos`). | All precip | Rerun 2026-10-22: net −6 mm C1, −22 mm D1, −4 mm SDL over the whole record. QC flags unchanged. |
| The "infilled by outdated method" note now uses only the pre-2011 raw flags `1`/`2` (old infill). From 2011, EDI 416 marks every day `n` (no flag), `m` (missing) or `q`, and `n`/`m` had been read as old infill. | All precip | Notes: about 4,550 raw SDL days from 2011 on had been labelled "'Raw' value infilled by outdated method…" in the published file, although they are raw. These notes, and the matching C1/D1 notes, are removed. Values: one D1 day changes. On 2011-12-29, 13 mm falls inside the 19-day accumulation read on 2012-01-16, so the qdays check removes it. The old note had made 2P put the raw value back; this was the `#!BUG` in 3P. The day is now infilled (6.5 mm). Over the record, 18 other D1 and 16 SDL infilled days move by less than 2 mm. |

### 2.4 Temperature infilling (3T, `temp_infill_functions.R`)

| Change | Affects | Effect |
|---|---|---|
| **Multi-year fill no longer skips every NWT logger.** 3T kept any metric matching `"avg\|DTR"`, which let an all-NA `airtemp_s_avg` logger metric into the data. `tk_temp_historicfill()` skips a candidate if any of its columns is NA, so every logger (HMPs, aspirated, hv, CRs) was silently excluded. Now exactly `airtemp_avg` / `DTR`. | All temp | Largest temperature change. E.g. SDL HMP 3 went from 56 to 670 of 671 infilled days drawing on on-site sensors. |
| `mean_and_diurnalT()` builds Tmean/DTR per site. Before, if any site in a dataset reported a mean, sites that only report max/min (e.g. 21x loggers) got no mean and could not be used. | Mostly pre-2001 re-infill | 21x loggers usable as sources. Sites that do report a mean keep their NAs (they can be QC removals). |
| `reject_neg_slope = T`: a source is rejected if either its Tmean or DTR fit has slope ≤ 0. The option defaults to `F`, so the 2022–2024 scripts behave as before. | All temp | Removes non-physical fits (mostly weak DTR fits). D1 temp: 6 published days changed; C1: none. |
| C1/D1 infill start 2020 → 1980 | C1/D1 temp | 1980 is the first year with neighbor sources (2T drops GHCN before 1980). Now published from 1980 (see 1). |
| **SDL pre-HMP series re-infilled (new `2T_sdl_homogenized.R`, 3T).** Reads CTW's homogenized 1986-12-06 – 2017-12-31 series from 314, keeps only measured values (raw + CTW adjustment, plus CTW's 60 values shifted −1 day in 1996–97) and blanks CTW's infill (~1,900–2,050 values per variable, mostly 1987–1991; 1988–89 have no logger data). Re-screens it with the 2025 all-logger QC pass (5 candidates; after review, CR21x Tmin 1992-08-30 and -31 removed, 4–8 °C below the SDL chart and D1). 3T infills it as one target (`sdl_homog`), with the CR21x neighbor order minus the raw SDL loggers. | SDL temp | Measured values unchanged except 2 days (1994-02-09, 2003-11-21) where `calculate_minmax()` rejects a measured mean inconsistent with the missing extreme, as for every target. Re-infilled days vs CTW: Tmax −0.05, Tmin +0.27, Tmean +0.09 °C (sd ~1). Annual means change mainly 1987–1991 (Tmean 0 to +0.2, Tmin +0.2 to +0.4). Trend 1987–2019: Tmax 0.22 → 0.24, Tmin 0.70 → 0.64, Tmean 0.47 → 0.45 °C/decade. |
| SDL HMP infill start 2020 → 2018 | SDL temp | 2018–2019 re-infilled (they were CTW's). Annual means unchanged to 0.03 °C. |

### 2.5 Precipitation infilling (3P, `ppt_infill_functions.R`)

| Change | Affects | Effect |
|---|---|---|
| **SDL precip record starts 1987-06-01.** SDL rows before then are dropped from `alldats`, so SDL is neither infilled nor used as a source before that date. The SDL:D1 winter ratio is ~1 in water years 1982–87 and ~2 from WY1988. knb-lter-nwt.315 already hid Oct–May 1981–87 for "unresolved data quality issues", but those years were still infilled and fed D1. Jun–Sep 1981–86 are dropped too (decided 2026-10-23). | SDL, D1 precip | Rerun 2026-10-23: SDL net +141 mm over 1987–2026, because its regressions no longer fit to 1981–87. D1 −30 mm (SDL no longer a source 1980–87). C1 unchanged. |
| Seasonal-fill tie-break: an r² tie is now broken by the lowest p-value (it picked the highest). | All precip | Bug fix. |
| `reject_neg_slope = T` for all C1/D1/SDL precip fits. The option defaults to `F`. | All precip | SDL: 128 days changed, net +118 mm over 1980–2025 (110 negative-slope fits removed). |
| `tk_ppt_historicfill()` check before `lm()` now requires positive source values on shared days, as `tk_ppt_seasonalfill()` already did. | All precip | Prevents a crash when a source has only zeros on the shared days. It cannot change any model that already ran. |
| C1/D1 write-out sets the raw "A" flags by testing `is.na(method)`, as SDL already did (it tested `is.na(equation)`). On H days (C1's own multi-day total divided evenly) raw is NA, so the old test was NA and wiped the A/H flags that `backfill_ppt()` had set. | C1/D1 precip | 10 C1 days (2013-06-19..25, 2014-06-15..17) now carry A/H. Values unchanged. D1 has no H days from 1980 on. |
| C1/D1 stitch year 2011 → 1980 (`new_infill_start`). Before, 1980–2010 came from the TK/CTW published series. | C1/D1 precip | 1980–2010 raw days are identical. Annual totals are within 5%. Set back to 2011 to restore the old stitch. |

### 2.6 Homogenization and publishing (4T, step 5)

- **4T:** reads `sdlhmp_infilled_2025.rds` and, new, `sdlhomog_infilled_2025.rds`. The
  pre-HMP rows (1986–2017) get the posted 314 columns: homogenized = the re-infilled
  series, CTW's adjustment, gapfilled = homogenized − adjustment, flags (same rules as
  the HMPs, now a function `infill_flags()`), sources and regressions from 3T, and CTW's
  qc flags and raw values (the 1992 removals noted in the qc flag).
- **Step 5:**
  - C1/D1 read the currently posted EDI versions via `getTabular()` instead of pinned URLs.
  - SDL step 5 still contains the 1981–Sep 1987 winter-removal filter. It now removes
    nothing, because the SDL record starts 1987-06-01.
  - Publish windows widened. C1/D1 go from `yr > 2021` to `yr >= 1980`. SDL temp is no longer filtered
    (1986-12-06 on), and keeps the adjustment, flag, regression, qc and raw columns for
    the pre-HMP rows; the H/A summary flags are set only on HMP rows. SDL precip is no longer filtered, so it starts
    1987-06-01. Output columns and flags are unchanged.
- **New `6_build_full_record.R`:**
  - Reads each posted EDI table.
  - Keeps its rows before the re-infill start, except SDL temp and precip, which keep none.
  - Appends the step-5 rows up to 2025-12-31.
  - Applies the column fixes the reformat scripts make:
    - `regression_regression` → `regression`
    - `infill_QAnote` → `infill_QA_note`
    - drop `raw_Tmean`
    - EDI `local_site`
    - columns step 5 doesn't write left blank (e.g. CTW's adjustment/gap-fill columns on HMP rows)
  - C1/D1 temp: carries CTW's `Tmax_QAflag`, `Tmin_QAflag` and `infill_QA_note` from the
    posted rows, by date, for 1980 on. A flag is carried only where the new value equals the
    posted value or the posted raw value. This workflow doesn't create these flags, and
    without the carry the republished 1980–2024 rows lost them. Shifted chart values, e.g.
    "Artificial drop, shifted +1C…", then had no explanation. EDI's chart data now holds the
    shifted value, so the new `raw_Tmax`/`raw_Tmin` hold it too. The flag is still what
    explains the shift. Carried: C1 1,170 Tmax / 1,239 Tmin of 1,224 / 1,292 posted; D1
    1,459 / 1,448 of 1,483 / 1,486. The rest are days the new run infills, so the old flag
    no longer applies. No `infill_QA_note` carries over, because all 66 are on old-infill days.
  - Checks for duplicate and missing dates, then writes `data/publish/full_record/`.
  - Reads the posted tables as text and writes them in EDI's layout (all values quoted
    except date and NaN), so posted rows are byte-identical to EDI. The first version read
    them as numbers, which trimmed 131 SDL temp p-values by one digit.

---

## 3. Changes that only make the workflow run

None of these change values.

- **Paths:** OneDrive / `setwd()` paths replaced with repo-relative `daily_met/Infilling_2025/data/`.
  Scripts run from the repo root. Output names changed `2024` → `2025`.
- **Folders:** script 1 also creates `data/publish/` and `data/plots/`.
- **Memory:** script 1 frees spent AmeriFlux intermediates (`rm(); gc()`), so it fits on a small VM.
- **`.gitignore`:** ignores `daily_met/Infilling_2025/data/` and `Rplots.pdf`, and keeps the
  `long-term-trends-data-copy/` CSVs.
- **EDI fetch helpers** (`getPackageVersion`, `getEntityId`, `getTabular`): rewritten on
  `EDIutils`, with a clear error if the requested entity does not exist.
- **`infilled_viz_check.R`:**
  - Repo-relative paths, and adds the D1 hv and C1 CR1000X aspirated sensors to the comparison plots.
  - Fixed missing `/` in the D1 output paths.
  - The C1 2019–2025 and D1 2024–2025 precip plots no longer overwrite the 2025 plot.
- **Plot limits:** year limits in diagnostic plots moved to 2025.

## 4. New review scripts (not part of the pipeline)

All write to `daily_met/Infilling_2025/data/plots/`; see `data/plots/README.md` there.

| Script | Run after | Purpose |
|---|---|---|
| `qc_2025_sources.R` | 2T/2P | Each 2025 source vs references (own site, C1, D1, Boulder 14 W, Daymet) against its usual 2018–2024 offset |
| `qc_2025_vs_neighbors.R` | 2T/2P | Every 2025 series minus its best neighbors; flags days outside the usual band |
| `qc_review_suspect_periods.R` | 2T | Niwot / University Camp SNOTEL and D1 HMP suspect periods |
| `qc_review_d1_2018.R` | 2T | D1 HMPs 2018–2019 vs neighbors |
| `qc_review_2025_infill.R` | step 5 | 2025 infilled values vs independent regional neighbors |
| `qc_compare_old_infill.R` | 3T/3P | New C1/D1 1980–2024 vs published knb-lter-nwt.184–187 |

## 5. Not changed on purpose

- **Ambiguous co-op timing:** afternoon co-op observers (USC00053496, USC00053116_1600,
  USC00053500) and Winter Park precip look partly a day late. They are not shifted, since
  observation times can't be confirmed.
- **Boulder 14 W 0.0 mm on 2025-12-03** (neighbors ~16–20 mm): kept as 0.0.
- **US-NR1 temperature:** kept. It is the tower's own sensor, unlike its precip.
- **Column names:** C1 temp output keeps `..._regression_regression_equation`, `infill_QAnote`
  and `raw_Tmean`, because the NWT_metadata reformat scripts expect them.
- **Shared AmeriFlux reader:** `fetchAmeriFlux()` still reads as character, because the
  helper is shared with earlier years.
- **EDI metadata:** the text in NWT_metadata was not touched. The SDL precip package
  description will need to say the record now starts 1987-06-01.

## 6. Open items

- **EDI update:** the `full_record/` files replace the posted tables rather than append
  to them. The NWT_metadata reformat scripts (append-only) don't apply. Package text to
  update:
  - Methods: re-infilled with the 2025 methods from 1980 (C1/D1), from 1986-12-06 (SDL
    temp, CTW's homogenized measured values re-infilled) and from 1987-06-01 (SDL precip).
  - SDL precip record now starts 1987-06-01.
- **2026:** the `_ongoing` files still run into 2026; `full_record/` stops at 2025-12-31
  (`last_date` in `6_build_full_record.R`).
- **2025 review points not yet decided:**
  - D1 chart Tmin kept raw Apr–Aug 2025 while Tmax was bad.
  - Weak D1 DTR fits (median r² ~0.3).
  - D1 2025-12-13 to 16 15.2 mm accumulation while neighbors were dry.
  - SDL CR21x before mid-1992: measured Tmax runs ~1–3 °C warmer relative to the SDL chart
    and D1 than in later CR21x years (largest in 1990). CTW's CR21x adjustment is 0, and the
    re-infill (fit mostly to later years) doesn't share the offset. Not changed.

## 7. Commits

| Commit | Summary |
|---|---|
| `82874f8` | 2025 scripts added (copied from 2024 with small edits, incl. ETI gauge as a source); EDIutils fetch helpers |
| `cd73343` | Script 1 memory fix |
| `bed003c` | Workflow runs end to end: paths, GHCN units, USC00052761 shift, SNOTEL/AmeriFlux QC fixes, avg/DTR filter, hv sensors, long-term-trends copies, step-5 EDI reads |
| `43d9fe3` | Review scripts for suspect periods and 2025 neighbors; US-NR1 note; setup notes |
| `ba80464` | Re-infill C1/D1 from 1980, `q`/`m` flag handling, negative-slope rejection, CTW predicted days |
| `8155232` | Niwot window extended, viz paths fixed |
| `d9cdf19` | US-NR1 precip used only 1998–2010 |
| `d621d4b` | SDL precip starts 1987-06-01; historic-fill `lm` check |
