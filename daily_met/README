# README: NWT CLIMATE WORKFLOW
# 2023-08-24, ctw


# Contents:
# 1. Workflow requirements
# 2. General workflow steps
# 3. Precipitation-specific steps
# 4. Temperature-specific steps
# 5. Unfinished workflow needs
# 6. Options for tweaking workflow and outputs



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
- While many stations are considered as source stations for infillling a given missing-value date, linear regression models use one source station only, the best model is chosen, and ultimately one source station is selected per date for gap-filling
[5]. Optional/as needed: homogenize time series
- Use other R packages or software to check for inhomogeneities along with any internal NWT notes (e.g., from Lead Climate Technicians); document any adjustments made, adjust conservatively and consider bias you are introducing in the time series by making adjustments
- Be careful of other packages suggesting statistical breaks around the dates of true events. Rely on station metadata, and when none are available check news archives (e.g., for mentions of blizzards or other weather events), and consider patterns at nearby weather stations
[6]. Pretty dataset for EDI publication (e.g., standardize flagging and QC notes, clean up column names)

Except for the first datasets read in for step 1 and the final dataset to publish on EDI, the workflow uses .rdata files for intermediate steps to preserve column classes and any date-time formatting.


# -- 3. PRECIPITATION-SPECIFIC STEPS ------

The code developed in ppt_infill_functions.R already accounts for these but so the user knows, precipitation infilling differs from temperature in the following ways:

- 
- for infill data (for target and source stations), regressions only use non-0 data. All 0 values are NA'd prior to linear models



# -- 4. TEMPERATURE-SPECIFIC STEPS ------




# -- 5. UNFINISHED NEEDS ------




# -- 6. OPTIONS ------


