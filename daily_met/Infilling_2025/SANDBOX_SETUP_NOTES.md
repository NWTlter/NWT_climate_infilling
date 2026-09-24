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

## 6. Status: script runs clean

With sections 1, 2 and 4 in place, `1_prepare_climate_data.R` ran end to end
(exit 0, no errors, ~5 min, ~2.7 GB peak RSS) and wrote all 14 expected outputs
to `data/prep/` (7 datasets, `.csv` + `.rds` each):

```
ameriflux_prep  c1loggerPPT_prep  ghcnd_prep  nwtchartPPT_prep
nwtchartTemp_prep  nwtloggerTemp_prep  snotel_prep
```

Not yet reviewed: whether those values are scientifically correct. Only that the
script completes. Scripts 2T/2P onward have not been run.

## 7. Runtime notes

- Every run re-downloads everything: all EDI packages, plus ~100 MB of AmeriFlux
  zips (`fetchAmeriFlux` has no cache/skip-if-present check). Budget ~5 min per
  attempt; a skip-if-present check would make iteration much faster.
- `view_plots` is `FALSE`; plotting was never exercised.
- Latent bug spotted but NOT hit by this script: the qa function in
  `common_qaqc_functions.R` references undefined `maxval`/`minval` in its
  `stopifnot()` (its params are `maxlim`/`minlim`). It would error if called —
  likely reachable from the 2T/2P QC scripts.
