# 2025 scripts vs 2024

Unified diffs of each `Infilling_2025` script against its `Infilling_2024` counterpart
(`-` = 2024, `+` = 2025), made 2026-10-26 (C1/D1 step 5 updated after the flag_1 fix). The reasons for each change
are in `../CHANGES_2025.md`.

Regenerate from `daily_met/`:

```bash
diff -u --label Infilling_2024/<script> --label Infilling_2025/<script> Infilling_2024/<script> Infilling_2025/<script>
git diff master -- R/
```

| Diff | 2025 script | Lines added / removed |
|---|---|---|
| `1_prepare_climate_data.diff` | `1_prepare_climate_data.R` | +14 / −5 |
| `2T_qc_temp_data.diff` | `2T_qc_temp_data.R` | +55 / −23 |
| `2P_qc_precip_data.diff` | `2P_qc_precip_data.R` | +35 / −7 |
| `3T_gapfill.diff` | `3T_gapfill.R` | +55 / −27 |
| `3P_gapfill_precip.diff` | `3P_gapfill_precip.R` | +44 / −31 |
| `4T_homogenize_sdlts.diff` | `4T_homogenize_sdlts.R` | +61 / −22 |
| `5_prep_v1_forEDI.diff` | `5_prep_v1_forEDI.R` (Saddle) | +17 / −11 |
| `c1_5_prep_v1_forEDI_c1.diff` | `c1/5_prep_v1_forEDI_c1.R` | +15 / −20 |
| `d1_5_prep_v1_forEDI_d1.diff` | `d1/5_prep_v1_forEDI_d1.R` | +14 / −19 |
| `infilled_viz_check.diff` | `infilled_viz_check.R` | +44 / −30 |
| `R_shared_functions_vs_master.diff` | `daily_met/R/*.R` (shared by all years; diff against `master`) | +92 / −27 |

New in 2025, with no 2024 counterpart (nothing to diff):

- Pipeline: `2T_sdl_homogenized.R`, `6_build_full_record.R`
- Review only: `qc_2025_sources.R`, `qc_2025_vs_neighbors.R`, `qc_compare_old_infill.R`,
  `qc_compare_republish.R`, `qc_review_2025_infill.R`, `qc_review_d1_2018.R`,
  `qc_review_suspect_periods.R`, `daily_met/infill_qc_check/qc_infilled_vs_raw.R`
- Data: `long-term-trends-data-copy/` (QC files copied from the long-term-trends repo)

`readme.md` is unchanged from 2024.
