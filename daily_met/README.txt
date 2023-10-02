# README: NWT CLIMATE WORKFLOW
# created: 2023-08-24, ctw
# last update: 2023-09-2
# follow-up questions?: caitlin.t.white@colorado.edu (or via NWT LTER/Suding Lab Slack or Github repo)


# README contents:
# 1. Workflow requirements
# 2. General workflow steps
# 3. Precipitation-specific steps
# 4. Temperature-specific steps
# 5. Unfinished workflow needs
# 6. Options for tweaking workflow and outputs

# Notes:
I initially thought I might make this workflow an R package for NWT, so functions live in an 'R' folder (akin to how they would in a package).


# -- 1. REQUIREMENTS ------

R package dependencies:
dplyr
tidyr
lubridate

R source scripts:
utility_functions.R
fetch_data_functions.R
prep_data_functions.R
ppt_infill_functions.R
temp_infill_functions.R



# -- 2. GENERAL STEPS ------ 

The general sequential workflow for preparing NWT daily climate gap-filled datasets is the following:

[1]. Fetch and prepare (standardize data structure and formats) all datasets for running through the workflow; write out prepared datasets
- Step 1 includes generating a station hierachy table for each site using x,y,z coordinates and manual adjustments afterwards to prioritize any stations based on user's knowledge of a site and instrumentation (e.g., NWT C1 station should be prioritized to gap-fill SDL and D1 over Niwot Snotel and Ameriflux US-NR1 Forest, which are both near C1)
[2]. QC prepared datasets individually; can write out or not, but be sure use individually QC'd datasets in next step
Individual (within-station) QC checks include:
- Plausibility (e.g., values outside detection limit)
- Internal consistency (e.g., day-to-day rate changes or same day values outside of user-defined threshold; check min and max values globally and monthly by default)
- Internal temporal coherency (e.g., all exptected dates/timestamps present and not duplicated)
[3]. Comparative QC; write out as qc'd data
Comparative checks are used to determine whether values flagged in step 2 should be removed (e.g., if multiple comparative stations recorded relative extreme high or low temperatures around the same time target )
[4]. Infill QC'd data, review infilling; append new gap-filled to continuing time series dataset
- While many stations are considered as source stations for infillling a given missing-value date, linear regression models use one source station only, the best model is chosen, and ultimately one source station is selected per date for gap-filling.
- Important: don't use any gapfilled data to gapfill data (NA any infilled data in source datasets before running regressions on the target station to infill).
[5]. Optional/as needed: homogenize time series
- Use other R packages or software to check for inhomogeneities along with any internal NWT notes (e.g., from Lead Climate Technicians); document any adjustments made, adjust conservatively and consider bias you are introducing in the time series by making adjustments
- Be careful of other packages suggesting statistical breaks around the dates of true events. Rely on station metadata, and when none are available check news archives (e.g., for mentions of blizzards or other weather events), and consider patterns at nearby weather stations
[6]. Pretty dataset for EDI publication (e.g., standardize flagging and QC notes, clean up column names)

Except for the first datasets read in for step 1 and the final dataset to publish on EDI, the workflow uses .rdata files for intermediate steps to preserve column classes and any date-time formatting.



# -- 3. PRECIPITATION-SPECIFIC STEPS ------

The code developed in ppt_infill_functions.R already accounts for these but so the user knows, precipitation infilling differs from temperature in the following ways:

- For infill data (for target and source stations), regressions only use non-0 data. All 0 values are NA'd prior to the linear regression models for-loop starting.
- Data are log-transformed for the regressions, so gap-filled values must be exponentiated and then rounded to the original precision of raw data.
- For observations missing, a sizeable portion fall in missing-data windows that have an accumulated precip value on the day non-NA data resume. Missing-data days in the window need to be "backfilled" from the accumulate precip value and predictions are used to apportion the total accumulated precip to days in the backfill window. In other words, the data aren't completely missing. Sometimes the accumulated value is 0, and in this case all days in the backfill window are assigned 0. In other cases, data are missing *and* there is no accumulated amount and gap-filled values are best predictions from whatever sensors get selected to gap-fill.



# -- 4. TEMPERATURE-SPECIFIC STEPS ------

- All sensors (target and sources) should be qc'd before moving to running gap-filling regressions. Some data, like GHCNd already have qc done, values are flagged accordingly, and you just need to look at flagged values to see if they should be removed as comparison data (GHCNd has a lots of different flag codes and I don't think I removed all flagged values, but any value that had a certain flag). Even when dataset metadata note data have been QC'd, still plot the data for odd values because I have found them in QC'd data.
- For daily temp, regressions predict mean temp and diurnal temp and derive tmax and tmin from those.
-- For Saddle specifically: the Saddle daily mean temp *should* match the daily mean one would calculate using hourly values for the day. Electronic logger tmax and tmin are instantaneous so the match between the daily and hourly datasets only applies to mean temp and not tmax, tmin, and diurnal. However, for version 1 of Saddle, I followed Tim Kittel's methods for C1 and D1 chart and didn't use the gap-filled hourly C1/D1/SDL dataset in any way. I did this in part for consistency in daily methods, but also because the hourly dataset is not QC'd for 2013 (or 2014?) onwards and there are odd values present.
- If either tmin or tmax is present on a day with other missing temp values, you can use the present daily extreme value to derive the missing extreme value. I usually used the present extreme with the predicted tmean rather than predicted diurnal temp because the tmean R2s are typically better than the diurnal temp R2s.
- Do plot the predicted data with the time series to see if gap-filled values visually seem to make sense or influence trends in the record (e.g. is the record tmax or tmin a predicted value?).
- Because there are two regression equations per day on days with missing values (one equation for tmean, one for diurnal), this means there are regression columns in the final dataset out for each and each needs to get flagged accordingly.



# -- 5. UNFINISHED NEEDS ------
(Some of these are already posted as issues in the repository)

1. Temp QC functions: different iterations of temp QC functions resides in the v0_pre_edi_example_scripts subfolders (different instances of infilling daily temp datasets for NWT). Some functions work on wide-format data (e.g., tmin, tmax, tmean are in their own columns, or different stations are in their own columns), and some work on the long-format (tidy) data (e.g., everything stacked with a column to indicate what's what). The data prep functions prepare data for long-format and I think this is the way to go, but ran out of time for making functions play nicely.

2. Dataviz flagged values (to dataviz or not to dataviz?): The first time I infilled the daily temp datasets, I made a function to plot flagged values with comparative station values. For simplicity, it may be easier on the NWT Data IM's life to write rules to remove values if they fail certain checks or fail a set of checks. But visualizing potential errors with comparative data is nice, especially for flagged relatively extreme values (as a daily value or rate change) that appear okay when plotted in the context of other stations.

3. Functions to prepare the gapfilled dataset for publication on EDI. Some examples of things functions could do:
- apply the correct flag label
- append the new time period of gapfilled data to the ongoing dataset
- check all dates expected are present (and there are no duplicates) and that there are no NAs in the gapfilled data
- round final gapfilled (and homogenized if applicable) values to the precision of the raw data

4. Function to compare gap-filled data to removed flagged data: if the values are about the same, keep the raw data. Make a similar check for predicted values vs. tmax and tmin derived from one present daily extreme. Sometimes the predicted values end up making more sense/reveal that the present extreme value should have been removed also.

5. Function to screen for consecutive days of faulty values: If there is an artificial spike in the data, sometimes values following that spike are also in a shifted range but they won't necessarily be caught by any existing qc functions I've written. This is the sort of error that is caught visualizing data (e.g. you can see in comparison with other stations, when a temp spike happens the values following are depressed or elevated artificially until there is a complete break in the data [not uncommon for chart data]). I started a check using chanepoint detection functions, but didn't finish it. I'll copy paste into the temp_qc_functions.R scripts.

6. Gapfilling and adjusting Saddle chart to append to the long-term gap filled Saddle temp dataset. V1 begins in 1987 when the loggers start recoding at NWT (albeit the first handful of years in the logger records are missing and gapfilled). The start of Saddle record-keeping for temp begins July 1981 (or '82?). The chart recorder functions in a way that makes its temp values recorded a little different than electronic loggers (chart daily extremes are often more muted so diurnal T may be smaller, mean may be comparable but not as precise as logger tmean [I think chart temp was recorded to the nearest 0.5C? But the chart paper is gridded in 1C intervals]). There are papers on transitioning from chart to logger record-keeping (I found example related to museums, but they have to exist for outdoor met stations as well), but I ran out of time for identifying the best method.



# -- 6. OPTIONS ------
(some of these are also already issues in the repo)

- Read and prep Ameriflux, SNOTEL, and GHCNd data using R packages made for each
- Add Berthoud Pass SNOTEL to the source stations read in (I only used Berthoud GHCNd because that was the station noted by Tim, but I later saw there is a SNOTEL station at Berthoud Pass that begins around the time the GHCNd record ends). This station would be useful because it's one of the more higher elevation stations in the region (still lower than D1 though).
