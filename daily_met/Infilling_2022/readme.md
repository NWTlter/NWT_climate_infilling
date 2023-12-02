## Miles used the scripts include here to infilling c1 data from 2019 - 2022.

`1_prepare_climate_data.R` - The code in this fall was almost entire
unchanged from the example daily_met workflow included in
../v1_example_scripts/. It is necessary to run this to pull at the
latest data from EDI and the various nearby stations.

`2T_qc_temp_data.R` - This code is sue to read in, clean, and visually
inspect the data that will ultimately be used to infill c1 temperature.
Much of this code is well written and easily reused, some additions were
added to clean out specific things in the datasets this go around.

`3T_gapfill.R` - This is where the gapfilling takes place! Easily
recyclable across years and sites.

*Note* 4T_homogenize_sdlts.R is not required for c1 and d1 since we do
not need to cross calibrate across loggers and sensors at these other
stations for these years.

`c1_infilled_viz_check.R` - This is the code used to produce the plots
and conduct a cross-site comparison of the final infilled data from
2019 - 2022. These plots can be found in Infilled/c1/plots on OneDrive
for now. These should be moved to a SCE or NWT owned one drive folder
eventually.
