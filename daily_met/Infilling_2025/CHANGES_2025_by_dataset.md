# 2025 changes by output dataset

Short, high-level summary for each published file. The full list, with details and
numbers, is in `CHANGES_2025.md`.

Applies to every dataset:

- Rerun end to end on fresh downloads of all sources.
- Republished from the start of each re-infill, so the fixes apply back through the
  record: C1/D1 from 1980, Saddle temp from 1986-12-06, Saddle precip from 1987-06-01.
  Earlier rows are kept exactly as posted. Files are in `data/publish/full_record/` and
  end 2025-12-31.

---

## C1 temperature (knb-lter-nwt.185, `c1_infilled_temp_daily.tk.data.csv`)

- **Questionable chart values:** chart values flagged `q` in EDI are now infilled instead of kept.
- **More and cleaner sources:** two bugs had removed sources from the regressions.
  - GHCN co-op temperatures had been read in tenths of °C, so most failed QC.
  - The multi-year regressions had been skipping every NWT logger.

  Both are fixed. SNOTEL and AmeriFlux QC fixes also remove known-bad source values.
- **Negative-slope fits rejected:** regressions where warmer at the source predicts colder at C1 are no longer used.
- **New C1 sensors:** the CR1000X aspirated sensors (2025+) are kept as their own sources.
- **CTW's QA flags kept:** the 1980–2024 `Tmax_QAflag`/`Tmin_QAflag` (e.g. "Artificial drop, shifted +1C") are carried from the posted data wherever the value is unchanged. CTW's 2011–2018 "Predicted infill chosen over known temperature-adjusted infill" note is also kept; those days are still fully predicted.
- **Republished from 1980; 1952–1979 unchanged.** Raw days match the old series. Days infilled in both versions differ by ~1–2 °C (sd), with no bias.

## D1 temperature (knb-lter-nwt.187, `d1_infilled_temp_daily.tk.data.csv`)

- **Questionable chart values:** chart values flagged `q` in EDI are now infilled instead of kept. This includes the ~2 °C-cold chart Tmax of May–Aug 2025, now infilled rather than published raw.
- **Bad D1 HMP period removed:** the D1 HMPs failed Jul 2018 – Mar 2019. That period is now `q`-flagged upstream and no longer used as a source.
- **New D1 sensors:** the hv sensors (from Sep 2025) are added as sources after the HMPs end.
- **Same source and regression fixes as C1:** GHCN units, NWT loggers in the multi-year regressions, SNOTEL/AmeriFlux QC, and negative-slope rejection.
- **CTW's QA flags kept:** the 1980–2024 `Tmax_QAflag`/`Tmin_QAflag` (e.g. "temp shifted 4C warmer") are carried from the posted data wherever the value is unchanged. CTW's 2011–2018 "Predicted infill chosen over known temperature-adjusted infill" note is also kept; those days are still fully predicted.
- **Republished from 1980; 1952–1979 unchanged.** About 230 days per variable that were raw in the old series are now infilled, mostly because of `q` flags.

## Saddle temperature (knb-lter-nwt.314, `sdl_daily_airtemp_gapfilled.cw.data.csv`)

- **Main change:** the Saddle HMP infill now draws on other NWT loggers in the multi-year regressions. A bug had excluded them all. For example, HMP 3 went from 56 to 670 of 671 infilled days using on-site sensors.
- **Whole record re-infilled from 1986-12-06:**
  - CTW's homogenized pre-HMP series (CR21x, CR23x, CR1000; 1986–2017) is kept for every measured day, including CTW's adjustments and manual fixes.
  - Only CTW's infilled days are re-infilled with the 2025 methods, mostly 1987–1991. 1988–89 had no logger data.
  - The HMPs are infilled from 2018, when they were installed.
- **2025 QC on the old series:** two CR21x minimums (1992-08-30 and -31) removed after review.
- **Effect:** annual means change mainly in 1987–1991 (Tmean up to +0.2 °C, Tmin up to +0.4 °C). The 1987–2019 Tmean trend is 0.45 °C/decade (was 0.47).
- **Same source fixes as C1/D1:** GHCN units, SNOTEL/AmeriFlux QC, the D1 HMP 2018–19 removal, and negative-slope rejection.

## C1 precipitation (knb-lter-nwt.184, `c1_infilled_precip_daily.tk.data.csv`)

- **New source:** the NADP ETI gauge at C1 (CO90, from Dec 2012) is now used as an infill source, in mm.
- **US-NR1 limited:** US-NR1 precip is used only for 1998–2010. Later it is a copy of Boulder 14 W.
- **More days flagged:** GHCN precip is now in mm, so CTW's cross-station QC flags slightly more outliers. In the published window this is 2024-03-16 (15 mm → 0, likely late snowmelt in the gauge).
- **Regression fixes:** negative-slope fits are rejected, and a tie-break bug in model choice is fixed.
- **Flag fix:** 10 days in Jun 2013 and Jun 2014, where C1's own multi-day total is split evenly, now carry flags A/H; they had been blank.
- **Republished from 1980; 1952–1979 unchanged.** 1980–2010 was previously the TK/CTW series. Raw days are identical and annual totals are within 5%.

## D1 precipitation (knb-lter-nwt.186, `d1_infilled_precip_daily.tk.data.csv`)

- **Same source and regression changes as C1:** ETI gauge added, US-NR1 limited to 1998–2010, GHCN in mm, negative slopes rejected, tie-break fix.
- **Saddle source cut:** Saddle is no longer used as a source before 1987-06-01 (see Saddle precipitation below).
- **Republished from 1980; 1952–1979 unchanged:**
  - 1980–2010 re-infilled.
  - Three large isolated raw days (1983-01-14, 1986-04-22, 2010-01-11) are now flagged by the cross-station QC and infilled as 0.

## Saddle precipitation (knb-lter-nwt.315, `sdl_daily_precip_gapfilled.cw.data.csv`)

- **Record now starts 1987-06-01.** Earlier winters are not comparable with the rest of the record and were already hidden in the published data. The whole 1981–May 1987 period is now neither infilled nor used as a source.
- **More large isolated days flagged:** CTW's cross-station QC flags 15 more days (1990–2019, 33–127 mm) now that GHCN is in mm. These were reviewed and kept as flagged; the infilled values replace them.
- **Notes corrected:** from 2011, raw days had been labelled "'Raw' value infilled by outdated method". They are raw, and the note is removed.
- **Regression changes:** negative-slope fits are rejected (net +118 mm over the record). Fits no longer use the 1981–87 years (net +141 mm). The ETI gauge is added and US-NR1 limited, as for C1/D1.
- **Whole record re-infilled and republished from 1987-06-01.** The posted 1981 – May 1987 rows, including the Jun–Sep values, are dropped.
