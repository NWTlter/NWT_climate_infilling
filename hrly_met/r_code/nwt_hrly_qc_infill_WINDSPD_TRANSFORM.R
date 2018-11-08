#Test different transforms of the wind data
#Must be transformed to meet normality assumption of linear regression
#Transform performed after QC3 and before spatial infilling

#Load THRUFILL1 workspace
load("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/nwt_hrly_qc_infill_WINDSPD_V2_THRUFILL1.RData")

#Load reshape2 package
library(reshape2)

#Make data frame of wind values
wind_transform <- data.frame("c1" = wind[["c1"]]$wind_FILL1,
                             "d1" = wind[["d1"]]$wind_FILL1,
                             "sdl" = wind[["sdl"]]$wind_FILL1)

wind_transform_melt <- melt(wind_transform)

#Plot distros of the QCd, FILL1 data
savepath = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/wind_speed_transform/"

ggplot(wind_transform_melt, aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Site", y = expression("Wind speed (m "*s^-1*")"), title = "Raw wind speed distributions")
ggsave(path = savepath, 
       filename = "nwt_windspd_allsites_boxplot.png")

ggplot(wind_transform_melt, aes(value)) + 
  geom_histogram() + 
  facet_wrap(~variable, ncol = 3, scales = "free") + 
  labs(x = expression("Wind speed (m "*s^-1*")"), y = "Frequency" , title = "Raw wind speed distributions")
ggsave(path = savepath, 
       filename = "nwt_windspd_allsites_histogram_freescale.png")

#Run different transforms
lamda <- seq(0.1, 1.0, by = 0.05)
for (i in 1: length(lamda)){
  wind_transform_melt[ , paste0("trans_lamda", lamda[i])] <-
    wind_transform_melt$value ^ lamda[i]
}
colnames(wind_transform_melt)[1:2] <- c("site", "notrans")
wind_transform_melt2 <- melt(wind_transform_melt, id.vars = "site")

#Plot and save all transforms
ggplot(subset(wind_transform_melt2, site == "c1"), aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Transform", y = expression("Wind speed (m "*s^-1*")"), title = "C1 Wind Speed Transforms")
ggsave(path = savepath, 
       filename = "nwt_windspd_c1_boxplot_alltransforms.png")
ggplot(subset(wind_transform_melt2, site == "d1"), aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Transform", y = expression("Wind speed (m "*s^-1*")"), title = "D1 Wind Speed Transforms")
ggsave(path = savepath, 
       filename = "nwt_windspd_d1_boxplot_alltransforms.png")
ggplot(subset(wind_transform_melt2, site == "sdl"), aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Transform", y = expression("Wind speed (m "*s^-1*")"), title = "SDL Wind Speed Transforms")
ggsave(path = savepath, 
       filename = "nwt_windspd_sdl_boxplot_alltransforms.png")

#The above plots indicate:
#O.35 is the best lamda for C1
#0.5 is the best lamda for D1 and SDL

#Use box-cox to estimate best lamda
library(MASS)
wind_transform_pos <- subset(wind_transform, c1 > 0 & d1 > 0 & sdl > 0)
setwd(savepath)

#C1
png(filename = "nwt_windspd_c1_boxcox_lambdas.png", width = 7.76, height = 6.39, units = "in", res = 300)
par(mfrow = c(3,1))
boxcox(lm(c1 ~ d1 + sdl, data = wind_transform_pos))
title( "C1 ~ D1 + SDL")
boxcox(lm(c1 ~ d1, data = wind_transform_pos))
title( "C1 ~ D1")
boxcox(lm(c1 ~ sdl, data = wind_transform_pos))
title( "C1 ~ SDL")
dev.off()

#D1
png(filename = "nwt_windspd_d1_boxcox_lambdas.png", width = 7.76, height = 6.39, units = "in", res = 300)
par(mfrow = c(3,1))
boxcox(lm(d1 ~ c1 + sdl, data = wind_transform_pos))
title( "D1 ~ C1 + SDL")
boxcox(lm(d1 ~ c1, data = wind_transform_pos))
title( "D1 ~ C1")
boxcox(lm(d1 ~ sdl, data = wind_transform_pos))
title( "D1 ~ SDL")
dev.off()

#SDL
png(filename = "nwt_windspd_sdl_boxcox_lambdas.png", width = 7.76, height = 6.39, units = "in", res = 300)
par(mfrow = c(3,1))
boxcox(lm(sdl ~ c1 + d1, data = wind_transform_pos))
title( "SDL ~ C1 + D1")
boxcox(lm(sdl ~ c1, data = wind_transform_pos))
title( "SDL ~ C1")
boxcox(lm(sdl ~ d1, data = wind_transform_pos))
title( "SDL ~ D1")
dev.off()
