## This is a script for visualizing and quality controlling Miles' first try at
# infilling c1 data. I ran the prepare climate data scirpt, the 2T_qc script and
# then the 3T_gapfill.R script.

rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library( tidyverse )

out_dir <- "~/OneDrive - UCB-O365/INSTAAR/infilling/"
datpath <- "~/OneDrive - UCB-O365/nwt_climate/data/"

################################################################################
# Reading in Data and Pivoting
################################################################################

#temp
c1_chart_infilled <- read.csv("c1/c1_chart_infilled_v1.csv") |>
  dplyr::mutate(date = lubridate::date(date))
d1_chart_infilled <- read.csv("d1/d1_chart_infilled_v1.csv") |>
  dplyr::mutate(date = lubridate::date(date))
# alldats <- read.csv("other_dats/alldats.csv")
sdl_homogenized <- read.csv(paste0(datpath, "publish/sdl_daily_airtemp_gapfilled_ongoing.csv"))
sdl_homogenized <- sdl_homogenized |> 
  mutate(date = lubridate::date(date))

# ppt
c1_ppt <- readRDS(paste0(datpath, "/infill/c1PPT_infilled_draft.rds"))
d1_ppt <- readRDS(paste0(datpath, "/infill/d1PPT_infilled_draft.rds"))
sdl_ppt <- readRDS(paste0(datpath, "publish/sdl_daily_precip_gapfilled_ongoing.csv"))
# getting DFs of mean hmps at sdl, d1, c1 for visual comparison across sites
# in the plotting code below
sdl_hmps <- alldats %>% dplyr::filter(local_site %in% c('sdl_cr1000_hmp_1',
                                                        'sdl_cr1000_hmp_2',
                                                        'sdl_cr1000_hmp_3')) %>% 
  spread(key = metric, value = measurement) %>% group_by(date) %>% 
  dplyr::summarise(airtemp_avg = mean(airtemp_avg, na.rm = TRUE)) %>% 
  filter(lubridate::year(date) > 2018) %>% 
  mutate(date = lubridate::date(date))

d1_hmps <- alldats %>% dplyr::filter(local_site %in% c('d1_cr1000_hmp_1',
                                                        'd1_cr1000_hmp_2',
                                                        'd1_cr1000_hmp_3')) %>% 
  spread(key = metric, value = measurement) %>% group_by(date) %>% 
  dplyr::summarise(airtemp_avg = mean(airtemp_avg, na.rm = TRUE)) %>% 
  filter(lubridate::year(date) > 2018) %>% 
  mutate(date = lubridate::date(date))

c1_hmps <- alldats %>% dplyr::filter(local_site %in% c('c1_cr1000_hmp_1',
                                                       'c1_cr1000_hmp_2',
                                                       'c1_cr1000_hmp_3')) %>% 
  spread(key = metric, value = measurement) %>% group_by(date) %>% 
  dplyr::summarise(airtemp_avg = mean(airtemp_avg, na.rm = TRUE)) %>% 
  filter(lubridate::year(date) > 2018) %>% 
  mutate(date = lubridate::date(date))

################################################################################
# C1
################################################################################

if(!dir.exists(paste0(out_dir,"c1/plots/temp/"))){
  dir.create((paste0(out_dir,"c1/plots/temp/")))
}

plot_out <- paste0(out_dir,"c1/plots/temp/")

for (col in c("airtemp_max", "airtemp_min", "airtemp_avg")){

  p <- ggplot()+
    geom_line(data = c1_chart_infilled, aes(date, .data[[col]]))+
    geom_point(data = c1_chart_infilled |>
                 dplyr::filter(.data[[paste0(col,"_method")]] != 'raw'),
               aes(date, .data[[col]], shape = .data[[paste0(col,"_method")]]),
               color = 'red', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('C1 Infilled Full Timeseries')+
    labs(y= col, x = 'Date')

  ggsave(paste0(plot_out, col, "_infilled_allyrs.png"), plot = p, device = 'png')

  p <- ggplot()+
    geom_line(data = sdl_hmps, aes(date, airtemp_avg, color = 'sdl_hmps_mean'))+
    geom_line(data = d1_hmps, aes(date, airtemp_avg, color = 'd1_hmps_mean'))+
    geom_line(data = c1_chart_infilled |> filter( yr > 2018), 
              aes(date, .data[[col]], color = 'c1_chart_infilled'))+
    geom_point(data = c1_chart_infilled |>
                 dplyr::filter(yr > 2018 & .data[[paste0(col,"_method")]] != 'raw'),
               aes(date, .data[[col]], shape = .data[[paste0(col,"_method")]]),
               color = 'firebrick', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('C1 Infilled Cross Site Comparison')+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')

  ggsave(paste0(plot_out, col, "_c1_infilled_2019_2022.png"), plot = p, device = 'png')

  for (y in 2019:2022){
    p <- ggplot()+
      geom_line(data = sdl_hmps |> filter( lubridate::year(date) == y),
                aes(date, airtemp_avg, color = 'sdl_hmps_mean'))+
      geom_line(data = d1_hmps |> filter( lubridate::year(date) == y),
                aes(date, airtemp_avg, color = 'd1_hmps_mean'))+
      geom_line(data = c1_chart_infilled |> filter( lubridate::year(date) == y), 
                aes(date, .data[[col]], color = 'c1_chart_infilled'))+
      geom_point(data = c1_chart_infilled |>
                   dplyr::filter(lubridate::year(date) == y & 
                                   .data[[paste0(col,"_method")]] != 'raw'),
                 aes(date, .data[[col]], shape = .data[[paste0(col,"_method")]]),
                 color = 'firebrick', size = 2)+
      theme(legend.position = 'bottom')+
      ggtitle(paste('C1 Infilled Cross Site Comparison', y))+
      scale_color_brewer(palette = 'Set2')+
      labs(y= col, x = 'Date')
      
    ggsave(paste0(plot_out, col, "_c1_infilled_",y,".png"), plot = p, device = 'png')

  }


}

#ppt
if(!dir.exists(paste0(out_dir,"c1/plots/ppt/"))){
  dir.create((paste0(out_dir,"c1/plots/ppt/")))
}

plot_out <- paste0(out_dir,"c1/plots/ppt/")

for (col in c("precip")){
  
  p <- ggplot()+
    geom_line(data = c1_ppt, aes(date, .data[[col]]))+
    geom_point(data = c1_ppt |>
                 dplyr::filter(.data[['flag_2']] != 'A'),
               aes(date, .data[[col]], color = .data[['flag_2']]),
               size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('c1 PPT Infilled Full Timeseries')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_c1_ppt_infilled_allyrs.png"), plot = p, device = 'png')
  
  p <- ggplot()+
    geom_line(data = d1_ppt |> filter( year > 2018), 
              aes(date, precip, color = 'd1_pt'))+
    geom_line(data = c1_ppt |> filter( year > 2018), 
              aes(date, precip, color = 'c1_ppt'))+
    geom_line(data = sdl_ppt |> filter( year > 2018),
              aes(date, .data[[col]], color = 'sdl_homogenized'))+
    geom_point(data = c1_ppt |>
                 dplyr::filter(year > 2018 & .data[['flag_1']] != 'A'),
               aes(date, .data[[col]], shape = .data[['flag_1']]),
               color = 'red', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('C1 PPT Infilled Cross Site Comparison')+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_c1_ppt_infilled_2019_2022.png"), plot = p, device = 'png')
  
  for (y in 2019:2022){
    p <- ggplot()+
      geom_line(data = d1_ppt |> filter( lubridate::year(date) == y),
                aes(date, precip, color = 'd1_ppt'))+
      geom_line(data = c1_ppt |> filter( lubridate::year(date) == y),
                aes(date, precip, color = 'c1_ppt'))+
      geom_line(data = sdl_ppt |> filter( lubridate::year(date) == y), 
                aes(date, .data[[col]], color = 'sdl_ppt'))+
      geom_point(data = c1_ppt |>
                   dplyr::filter(lubridate::year(date) == y & .data[['flag_1']] != 'A'),
                 aes(date, .data[[col]], shape = .data[['flag_1']]),
                 color = 'red', size = 2)+
      theme(legend.position = 'bottom')+
      ggtitle(paste('c1 PPT Infilled Cross Site Comparison', y))+
      scale_color_brewer(palette = 'Set2')+
      labs(y= col, x = 'Date')
    
    ggsave(paste0(plot_out, col, "_c1_ppt_infilled_",y,".png"), plot = p, device = 'png')
    
    for (mon in 1:12){
      p <- ggplot()+
        geom_line(data = d1_ppt |> filter( lubridate::year(date) == y &
                                             lubridate::month(date) == mon),
                  aes(date, precip, color = 'd1_ppt'))+
        geom_line(data = c1_ppt |> filter( lubridate::year(date) == y &
                                             lubridate::month(date) == mon),
                  aes(date, precip, color = 'c1_ppt'))+
        geom_line(data = sdl_ppt |> filter( lubridate::year(date) == y &
                                              lubridate::month(date) == mon), 
                  aes(date, .data[[col]], color = 'sdl_ppt'))+
        geom_point(data = c1_ppt |>
                     dplyr::filter(lubridate::year(date) == y &
                                     lubridate::month(date) == mon &
                                     .data[['flag_2']] != 'A'),
                   aes(date, .data[[col]], shape = .data[['flag_2']]),
                   color = 'red', size = 2)+
        theme(legend.position = 'bottom')+
        ggtitle(paste('c1 PPT Infilled Cross Site Comparison', y))+
        scale_color_brewer(palette = 'Set2')+
        labs(y= col, x = 'Date')
      
      ggsave(paste0(plot_out, col, "_c1_ppt_infilled_",y,mon,".png"), plot = p, device = 'png')
    }
    
  }
}
################################################################################
# D1
################################################################################

if(!dir.exists(paste0(out_dir,"d1/plots/temp/"))){
  dir.create((paste0(out_dir,"d1/plots/temp/")))
}

plot_out <- paste0(out_dir,"d1/plots/temp/")

for (col in c("airtemp_max", "airtemp_min", "airtemp_avg")){
  
  p <- ggplot()+
    geom_line(data = d1_chart_infilled, aes(date, .data[[col]]))+
    geom_point(data = d1_chart_infilled |>
                 dplyr::filter(.data[[paste0(col,"_method")]] != 'raw'),
               aes(date, .data[[col]], shape = .data[[paste0(col,"_method")]]),
               color = 'red', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('D1 Infilled Full Timeseries')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_d1_infilled_allyrs.png"), plot = p, device = 'png')
  
  p <- ggplot()+
    geom_line(data = sdl_hmps, aes(date, airtemp_avg, color = 'sdl_hmps_mean'))+
    geom_line(data = c1_hmps, aes(date, airtemp_avg, color = 'c1_hmps_mean'))+
    geom_line(data = d1_chart_infilled |> filter( yr > 2018),
              aes(date, .data[[col]], color = 'd1_chart_infilled'))+
    geom_point(data = d1_chart_infilled |>
                 dplyr::filter(yr > 2018 & .data[[paste0(col,"_method")]] != 'raw'),
               aes(date, .data[[col]], shape = .data[[paste0(col,"_method")]]),
               color = 'firebrick', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('D1 Infilled Cross Site Comparison')+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_d1_infilled_2019_2022.png"), plot = p, device = 'png')
  
  for (y in 2019:2022){
    p <- ggplot()+
      geom_line(data = sdl_hmps |> filter( lubridate::year(date) == y),
                aes(date, airtemp_avg, color = 'sdl_hmps_mean'))+
      geom_line(data = c1_hmps |> filter( lubridate::year(date) == y),
                aes(date, airtemp_avg, color = 'c1_hmps_mean'))+
      geom_line(data = d1_chart_infilled |> filter( lubridate::year(date) == y), 
                aes(date, .data[[col]], color = 'd1_chart_infilled'))+
      geom_point(data = d1_chart_infilled |>
                   dplyr::filter(lubridate::year(date) == y & 
                                   .data[[paste0(col,"_method")]] != 'raw'),
                 aes(date, .data[[col]], shape = .data[[paste0(col,"_method")]]),
                 color = 'firebrick', size = 2)+
      theme(legend.position = 'bottom')+
      
      ggtitle(paste('D1 Infilled Cross Site Comparison', y))+
      scale_color_brewer(palette = 'Set2')+
      labs(y= col, x = 'Date')
    
    ggsave(paste0(plot_out, col, "_infilled_",y,".png"), plot = p, device = 'png')
    
  }
}

#ppt 
if(!dir.exists(paste0(out_dir,"d1/plots/ppt/"))){
  dir.create((paste0(out_dir,"d1/plots/ppt/")))
}

plot_out <- paste0(out_dir,"d1/plots/ppt/")

for (col in c("precip")){
  
  p <- ggplot()+
    geom_line(data = d1_ppt, aes(date, .data[[col]]))+
    geom_point(data = d1_ppt |>
                 dplyr::filter(.data[['flag_2']] != 'A'),
               aes(date, .data[[col]], color = .data[['flag_2']]),
               size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('SDL PPT Infilled Full Timeseries')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_d1_ppt_infilled_allyrs.png"), plot = p, device = 'png')
  
  p <- ggplot()+
    geom_line(data = d1_ppt |> filter( year > 2018), aes(date, precip, color = 'd1_pt'))+
    geom_line(data = c1_ppt |> filter( year > 2018), aes(date, precip, color = 'c1_ppt'))+
    geom_line(data = sdl_ppt |> filter( year > 2018),
              aes(date, .data[[col]], color = 'sdl_homogenized'))+
    geom_point(data = d1_ppt |>
                 dplyr::filter(year > 2018 & .data[['flag_1']] != 'A'),
               aes(date, .data[[col]], shape = .data[['flag_1']]),
               color = 'red', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('d1 PPT Infilled Cross Site Comparison')+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_d1_ppt_infilled_2019_2022.png"), plot = p, device = 'png')
  
  for (y in 2019:2022){
    p <- ggplot()+
      geom_line(data = d1_ppt |> filter( lubridate::year(date) == y),
                aes(date, precip, color = 'd1_ppt'))+
      geom_line(data = c1_ppt |> filter( lubridate::year(date) == y),
                aes(date, precip, color = 'c1_ppt'))+
      geom_line(data = sdl_ppt |> filter( lubridate::year(date) == y), 
                aes(date, .data[[col]], color = 'sdl_ppt'))+
      geom_point(data = d1_ppt |>
                   dplyr::filter(lubridate::year(date) == y & .data[['flag_1']] != 'A'),
                 aes(date, .data[[col]], shape = .data[['flag_1']]),
                 color = 'red', size = 2)+
      theme(legend.position = 'bottom')+
      ggtitle(paste('d1 PPT Infilled Cross Site Comparison', y))+
      scale_color_brewer(palette = 'Set2')+
      labs(y= col, x = 'Date')
    
    ggsave(paste0(plot_out, col, "_d1_ppt_infilled_",y,".png"), plot = p, device = 'png')
    
    for (mon in 1:12){
      p <- ggplot()+
        geom_line(data = d1_ppt |> filter( lubridate::year(date) == y &
                                             lubridate::month(date) == mon),
                  aes(date, precip, color = 'd1_ppt'))+
        geom_line(data = c1_ppt |> filter( lubridate::year(date) == y &
                                             lubridate::month(date) == mon),
                  aes(date, precip, color = 'c1_ppt'))+
        geom_line(data = sdl_ppt |> filter( lubridate::year(date) == y &
                                              lubridate::month(date) == mon), 
                  aes(date, .data[[col]], color = 'sdl_ppt'))+
        geom_point(data = d1_ppt |>
                     dplyr::filter(lubridate::year(date) == y &
                                     lubridate::month(date) == mon &
                                     .data[['flag_2']] != 'A'),
                   aes(date, .data[[col]], shape = .data[['flag_2']]),
                   color = 'red', size = 2)+
        theme(legend.position = 'bottom')+
        ggtitle(paste('d1 PPT Infilled Cross Site Comparison', y))+
        scale_color_brewer(palette = 'Set2')+
        labs(y= col, x = 'Date')
      
      ggsave(paste0(plot_out, col, "_d1_ppt_infilled_",y,mon,".png"), plot = p, device = 'png')
    }
    
  }
}

################################################################################
# Saddle
################################################################################

if(!dir.exists(paste0(out_dir,"sdl/plots/temp/"))){
  dir.create((paste0(out_dir,"sdl/plots/temp/")))
}

plot_out <- paste0(out_dir,"sdl/plots/temp/")

for (col in c("airtemp_max_homogenized", "airtemp_min_homogenized",
              "airtemp_avg_homogenized")){
  
  p <- ggplot()+
    geom_line(data = sdl_homogenized, aes(date, .data[[col]]))+
    geom_point(data = sdl_homogenized |>
                 dplyr::filter(.data[['flag_2']] != 'A'),
               aes(date, .data[[col]], shape = .data[['flag_2']]),
               color = 'orchid3', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('SDL Homogenized / Infilled Full Timeseries')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_sdl_infilled_allyrs.png"), plot = p, device = 'png')
  
  p <- ggplot()+
    geom_line(data = d1_hmps, aes(date, airtemp_avg, color = 'd1_hmps_mean'))+
    geom_line(data = c1_hmps, aes(date, airtemp_avg, color = 'c1_hmps_mean'))+
    geom_line(data = sdl_homogenized |> filter( yr > 2018),
              aes(date, .data[[col]], color = 'sdl_homogenized'))+
    geom_point(data = sdl_homogenized |>
                 dplyr::filter(yr > 2018 & .data[['flag_1']] != 'A'),
               aes(date, .data[[col]], shape = .data[['flag_1']]),
               color = 'red', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('SDL Homogenized / Infilled Cross Site Comparison')+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_sdl_infilled_2019_2022.png"), plot = p, device = 'png')
  
  for (y in 2019:2022){
    p <- ggplot()+
      geom_line(data = d1_hmps |> filter( lubridate::year(date) == y),
                aes(date, airtemp_avg, color = 'd1_hmps_mean'))+
      geom_line(data = c1_hmps |> filter( lubridate::year(date) == y),
                aes(date, airtemp_avg, color = 'c1_hmps_mean'))+
      geom_line(data = sdl_homogenized |> filter( lubridate::year(date) == y), 
                aes(date, .data[[col]], color = 'sdl_homogenized'))+
      geom_point(data = sdl_homogenized |>
                   dplyr::filter(yr > 2018 & .data[['flag_1']] != 'A'),
                 aes(date, .data[[col]], shape = .data[['flag_1']]),
                 color = 'red', size = 2)+
      theme(legend.position = 'bottom')+
      ggtitle(paste('SDL Homogenized / Infilled Cross Site Comparison', y))+
      scale_color_brewer(palette = 'Set2')+
      labs(y= col, x = 'Date')
    
    ggsave(paste0(plot_out, col, "_sdl_infilled_",y,".png"), plot = p, device = 'png')
    
  }
}


if(!dir.exists(paste0(out_dir,"sdl/plots/ppt/"))){
  dir.create((paste0(out_dir,"sdl/plots/ppt/")))
}

plot_out <- paste0(out_dir,"sdl/plots/ppt/")

for (col in c("precip")){
  
  p <- ggplot()+
    geom_line(data = sdl_ppt, aes(date, .data[[col]]))+
    geom_point(data = sdl_ppt |>
                 dplyr::filter(.data[['flag_2']] != 'A'),
               aes(date, .data[[col]], color = .data[['flag_2']]),
               size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('SDL PPT Infilled Full Timeseries')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_sdl_ppt_infilled_allyrs.png"), plot = p, device = 'png')
  
  p <- ggplot()+
    geom_line(data = d1_ppt |> filter( year > 2018), aes(date, precip, color = 'd1_pt'))+
    geom_line(data = c1_ppt |> filter( year > 2018), aes(date, precip, color = 'c1_ppt'))+
    geom_line(data = sdl_ppt |> filter( year > 2018),
              aes(date, .data[[col]], color = 'sdl_homogenized'))+
    geom_point(data = sdl_ppt |>
                 dplyr::filter(year > 2018 & .data[['flag_1']] != 'A'),
               aes(date, .data[[col]], shape = .data[['flag_1']]),
               color = 'red', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('SDL PPT Infilled Cross Site Comparison')+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_sdl_ppt_infilled_2019_2022.png"), plot = p, device = 'png')
  
  for (y in 2019:2022){
    p <- ggplot()+
      geom_line(data = d1_ppt |> filter( lubridate::year(date) == y),
                aes(date, precip, color = 'd1_ppt'))+
      geom_line(data = c1_ppt |> filter( lubridate::year(date) == y),
                aes(date, precip, color = 'c1_ppt'))+
      geom_line(data = sdl_ppt |> filter( lubridate::year(date) == y), 
                aes(date, .data[[col]], color = 'sdl_ppt'))+
      geom_point(data = sdl_ppt |>
                   dplyr::filter(lubridate::year(date) == y & .data[['flag_1']] != 'A'),
                 aes(date, .data[[col]], shape = .data[['flag_1']]),
                 color = 'red', size = 2)+
      theme(legend.position = 'bottom')+
      ggtitle(paste('SDL PPT Infilled Cross Site Comparison', y))+
      scale_color_brewer(palette = 'Set2')+
      labs(y= col, x = 'Date')
    
    ggsave(paste0(plot_out, col, "_sdl_ppt_infilled_",y,".png"), plot = p, device = 'png')
    
    for (mon in 1:12){
      p <- ggplot()+
        geom_line(data = d1_ppt |> filter( lubridate::year(date) == y &
                                             lubridate::month(date) == mon),
                  aes(date, precip, color = 'd1_ppt'))+
        geom_line(data = c1_ppt |> filter( lubridate::year(date) == y &
                                             lubridate::month(date) == mon),
                  aes(date, precip, color = 'c1_ppt'))+
        geom_line(data = sdl_ppt |> filter( lubridate::year(date) == y &
                                              lubridate::month(date) == mon), 
                  aes(date, .data[[col]], color = 'sdl_ppt'))+
        geom_point(data = sdl_ppt |>
                     dplyr::filter(lubridate::year(date) == y &
                                     lubridate::month(date) == mon &
                                     .data[['flag_2']] != 'A'),
                   aes(date, .data[[col]], shape = .data[['flag_2']]),
                   color = 'red', size = 2)+
        theme(legend.position = 'bottom')+
        ggtitle(paste('SDL PPT Infilled Cross Site Comparison', y))+
        scale_color_brewer(palette = 'Set2')+
        labs(y= col, x = 'Date')
      
      ggsave(paste0(plot_out, col, "_sdl_ppt_infilled_",y,mon,".png"), plot = p, device = 'png')
    }
    
  }
}

# End of Document #