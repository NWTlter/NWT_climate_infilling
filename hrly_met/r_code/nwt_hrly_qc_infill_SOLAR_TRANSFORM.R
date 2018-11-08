#Test different transforms of the solar data
#Must be transformed to meet normality assumption of linear regression
#Transform performed after temporal infilling and before spatial infilling

#Load THRUFILL1 workspace
load("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/nwt_hrly_qc_infill_solar_V2_THRUFILL1.RData")

#Load reshape2 package
library(reshape2)

#Make data frame of solar values
solar_transform <- data.frame("c1" = solar[["c1"]]$solar_FILL1,
                              "d1" = solar[["d1"]]$solar_FILL1,
                              "sdl" = solar[["sdl"]]$solar_FILL1)

solar_transform_melt <- melt(solar_transform)

#Plot distros of the QCd, FILL1 data
savepath = "~/Documents/research/site_data/niwot/nwt_hrly_qc/plots/solar_transform/"

ggplot(solar_transform_melt, aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Site", y = expression("Total incoming solar (MJ "*m^-2*")"), title = "Raw solar distributions")
ggsave(path = savepath, 
       filename = "nwt_solar_allsites_boxplot.png")

ggplot(solar_transform_melt, aes(value)) + 
  geom_histogram() + 
  facet_wrap(~variable, ncol = 3, scales = "free") + 
  labs(x = expression("Total incoming solar (MJ "*m^-2*")"), y = "Frequency" , title = "Raw solar distributions")
ggsave(path = savepath, 
       filename = "nwt_solar_allsites_histogram_freescale.png")

#Run different transforms
lamda <- seq(0.1, 1.0, by = 0.05)
for (i in 1: length(lamda)){
  solar_transform_melt[ , paste0("trans_lamda", lamda[i])] <-
    solar_transform_melt$value ^ lamda[i]
}
colnames(solar_transform_melt)[1:2] <- c("site", "notrans")
solar_transform_melt2 <- melt(solar_transform_melt, id.vars = "site")

#Plot and save all transforms
ggplot(subset(solar_transform_melt2, site == "c1"), aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Transform", y = expression("Total incoming solar (MJ "*m^-2*")"), title = "C1 solar Transforms")
ggsave(path = savepath, 
       filename = "nwt_solar_c1_boxplot_alltransforms.png")
ggplot(subset(solar_transform_melt2, site == "d1"), aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Transform", y = expression("Total incoming solar (MJ "*m^-2*")"), title = "D1 solar Transforms")
ggsave(path = savepath, 
       filename = "nwt_solar_d1_boxplot_alltransforms.png")
ggplot(subset(solar_transform_melt2, site == "sdl"), aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Transform", y = expression("Total incoming solar (MJ "*m^-2*")"), title = "SDL solar Transforms")
ggsave(path = savepath, 
       filename = "nwt_solar_sdl_boxplot_alltransforms.png")


#Plot and save all transforms where solar != 0
ggplot(subset(solar_transform_melt2, site == "c1" & value > 0), aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Transform", y = expression("Total incoming solar (MJ "*m^-2*")"), title = "C1 Solar Transforms (> 0)")
ggsave(path = savepath, 
       filename = "nwt_solar_c1_boxplot_alltransforms_positive.png")
ggplot(subset(solar_transform_melt2, site == "d1" & value > 0), aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Transform", y = expression("Total incoming solar (MJ "*m^-2*")"), title = "D1 Solar Transforms (> 0)")
ggsave(path = savepath, 
       filename = "nwt_solar_d1_boxplot_alltransforms_positive.png")
ggplot(subset(solar_transform_melt2, site == "sdl" & value > 0), aes(variable, value)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Transform", y = expression("Total incoming solar (MJ "*m^-2*")"), title = "SDL Solar Transforms (> 0)")
ggsave(path = savepath, 
       filename = "nwt_solar_sdl_boxplot_alltransforms_positive.png")


#The above plots indicate:
#0.55 is the best lamda

#Use box-cox to estimate best lamda
library(MASS)
solar_transform_pos <- subset(solar_transform, c1 > 0 & d1 > 0 & sdl > 0)
setwd(savepath)

#C1
png(filename = "nwt_solar_c1_boxcox_lambdas.png", width = 7.76, height = 6.39, units = "in", res = 300)
par(mfrow = c(3,1))
boxcox(lm(c1 ~ d1 + sdl, data = solar_transform_pos))
title( "C1 ~ D1 + SDL")
boxcox(lm(c1 ~ d1, data = solar_transform_pos))
title( "C1 ~ D1")
boxcox(lm(c1 ~ sdl, data = solar_transform_pos))
title( "C1 ~ SDL")
dev.off()

#D1
png(filename = "nwt_solar_d1_boxcox_lambdas.png", width = 7.76, height = 6.39, units = "in", res = 300)
par(mfrow = c(3,1))
boxcox(lm(d1 ~ c1 + sdl, data = solar_transform_pos))
title( "D1 ~ C1 + SDL")
boxcox(lm(d1 ~ c1, data = solar_transform_pos))
title( "D1 ~ C1")
boxcox(lm(d1 ~ sdl, data = solar_transform_pos))
title( "D1 ~ SDL")
dev.off()

#SDL
png(filename = "nwt_solar_sdl_boxcox_lambdas.png", width = 7.76, height = 6.39, units = "in", res = 300)
par(mfrow = c(3,1))
boxcox(lm(sdl ~ c1 + d1, data = solar_transform_pos))
title( "SDL ~ C1 + D1")
boxcox(lm(sdl ~ c1, data = solar_transform_pos))
title( "SDL ~ C1")
boxcox(lm(sdl ~ d1, data = solar_transform_pos))
title( "SDL ~ D1")
dev.off()


detach("package:MASS", unload = TRUE)
detach("package:reshape2", unload = TRUE)
