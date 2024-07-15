## This is a script for visualizing and quality controlling Miles' first try at
# infilling c1 data. I ran the prepare climate data scirpt, the 2T_qc script and
# then the 3T_gapfill.R script.

rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library( tidyverse )

out_dir <- "~/OneDrive - UCB-O365/NWT_Infilling_2023/plots/"
datpath <- "~/OneDrive - UCB-O365/NWT_Infilling_2023/data/publish/"
infill_path <- "~/NWTLTER/NWT_climate_infilling/daily_met/Infilling_2023/"

################################################################################
# Reading in Data and Pivoting
################################################################################
#temp
c1_chart_infilled <- read.csv(file.path(infill_path, "c1/c1_chart_infilled_v1.csv")) |>
  dplyr::mutate(date = lubridate::date(date))

d1_chart_infilled <- read.csv(file.path(infill_path, "d1/d1_chart_infilled_v1.csv")) |>
  dplyr::mutate(date = lubridate::date(date))

alldats <- read.csv("~/NWTLTER/NWT_climate_infilling/daily_met/Infilling_2023/other_dats/alldats.csv")

sdl_homogenized <- read.csv(paste0(datpath, "sdl_daily_airtemp_gapfilled_ongoing.csv"))
sdl_homogenized <- sdl_homogenized |> 
  mutate(date = lubridate::date(date),
         # Join all flag 1s for easy plotting (AAA = none infilled)
         flag_1 = paste0(flag_1_hmp1,flag_1_hmp2,flag_1_hmp3))

# ppt
c1_ppt <- read.csv(paste0(datpath, "c1_daily_precip_gapfilled_ongoing.csv")) |>
  dplyr::mutate(date = lubridate::date(date))
d1_ppt <- read.csv(paste0(datpath, "d1_daily_precip_gapfilled_ongoing.csv")) |>
  dplyr::mutate(date = lubridate::date(date))
sdl_ppt <- read.csv(paste0(datpath, "sdl_daily_precip_gapfilled_ongoing.csv")) |>
  dplyr::mutate(date = lubridate::date(date))
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

if(!dir.exists(paste0(out_dir,"qc/c1/temp"))){
  dir.create((paste0(out_dir,"qc/c1/temp/")))
}

plot_out <- paste0(out_dir,"qc/c1/temp/")

for (col in c("airtemp_max", "airtemp_min", "airtemp_avg")){
  
  p <- ggplot()+
    geom_line(data = sdl_hmps, aes(date, airtemp_avg, color = 'sdl_hmps_mean'))+
    geom_line(data = d1_hmps, aes(date, airtemp_avg, color = 'd1_hmps_mean'))+
    geom_line(data = c1_chart_infilled |> filter( yr > 2018), 
              aes(date, .data[[col]], color = 'c1_chart_infilled'))+
    geom_point(data = c1_chart_infilled |>
                 dplyr::filter(yr > 2022 & .data[[paste0(col,"_method")]] != 'raw'),
               aes(date, .data[[col]], shape = .data[[paste0(col,"_method")]]),
               color = 'firebrick', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('C1 Infilled Cross Site Comparison')+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_c1_infilled_2023.png"), plot = p, device = 'png')
  
  
}

#ppt
if(!dir.exists(paste0(out_dir,"qc/c1/ppt"))){
  dir.create((paste0(out_dir,"qc/c1/ppt/")))
}

plot_out <- paste0(out_dir,"qc/c1/ppt/")

for (col in c("precip")){
  
  # p <- ggplot()+
  #   geom_line(data = c1_ppt, aes(date, .data[[col]]))+
  #   geom_point(data = c1_ppt |>
  #                dplyr::filter(.data[['flag_1']] != 'A'),
  #              aes(date, .data[[col]], color = .data[['flag_1']]),
  #              size = 2)+
  #   theme(legend.position = 'bottom')+
  #   ggtitle('c1 PPT Infilled Full Timeseries')+
  #   labs(y= col, x = 'Date')
  
  # ggsave(paste0(plot_out, col, "_c1_ppt_infilled_allyrs.png"), plot = p, device = 'png')
  
  p <- ggplot()+
    geom_line(data = d1_ppt |> filter( year > 2018), 
              aes(date, precip, color = 'd1_pt'))+
    geom_line(data = c1_ppt |> filter( year > 2018),
              aes(date, precip, color = 'c1_ppt'))+
    geom_line(data = sdl_ppt |> filter( year > 2018),
              aes(date, .data[[col]], color = 'sdl_homogenized'))+
    geom_point(data = c1_ppt |>
                 dplyr::filter(year > 2022 & .data[['flag_1']] != 'A'),
               aes(date, .data[[col]], shape = .data[['flag_1']]),
               color = 'red', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('C1 PPT Infilled Cross Site Comparison')+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_c1_ppt_infilled_2023.png"), plot = p, device = 'png')
  
  for (y in 2023){
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
                                     .data[['flag_1']] != 'A'),
                   aes(date, .data[[col]], shape = .data[['flag_1']]),
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

if(!dir.exists(paste0(out_dir,"qc/d1/temp"))){
  dir.create((paste0(out_dir,"qc/d1/temp")))
}

plot_out <- paste0(out_dir,"qc/d1/temp")

for (col in c("airtemp_max", "airtemp_min", "airtemp_avg")){
  
  
  p <- ggplot()+
    geom_line(data = sdl_hmps, aes(date, airtemp_avg, color = 'sdl_hmps_mean'))+
    geom_line(data = c1_hmps, aes(date, airtemp_avg, color = 'c1_hmps_mean'))+
    geom_line(data = d1_chart_infilled |> filter( yr > 2018),
              aes(date, .data[[col]], color = 'd1_chart_infilled'))+
    geom_point(data = d1_chart_infilled |>
                 dplyr::filter(yr > 2022 & .data[[paste0(col,"_method")]] != 'raw'),
               aes(date, .data[[col]], shape = .data[[paste0(col,"_method")]]),
               color = 'firebrick', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('D1 Infilled Cross Site Comparison')+
    xlim(lubridate::date("2023-01-01"), lubridate::date("2023-12-31"))+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_d1_infilled_2023.png"), plot = p, device = 'png')
  
  
}

#ppt 
if(!dir.exists(paste0(out_dir,"qc/d1/ppt"))){
  dir.create((paste0(out_dir,"qc/d1/ppt")))
}

plot_out <- paste0(out_dir,"qc/d1/ppt")

for (col in c("precip")){
  
  
  p <- ggplot()+
    geom_line(data = d1_ppt |> filter( year > 2022), aes(date, precip, color = 'd1_pt'))+
    geom_line(data = c1_ppt |> filter( year > 2022), aes(date, precip, color = 'c1_ppt'))+
    geom_line(data = sdl_ppt |> filter( year > 2022),
              aes(date, .data[[col]], color = 'sdl_homogenized'))+
    geom_point(data = d1_ppt |>
                 dplyr::filter(year > 2022 & .data[['flag_1']] != 'A'),
               aes(date, .data[[col]], shape = .data[['flag_1']]),
               color = 'red', size = 2)+
    theme(legend.position = 'bottom')+
    ggtitle('d1 PPT Infilled Cross Site Comparison')+
    xlim(lubridate::date("2023-01-01"), lubridate::date("2023-12-31"))+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_d1_ppt_infilled_2023.png"), plot = p, device = 'png')
  
  for (y in 2023){
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
                                     .data[['flag_1']] != 'A'),
                   aes(date, .data[[col]], shape = .data[['flag_1']]),
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

#temp
if(!dir.exists(paste0(out_dir,"qc/sdl/temp"))){
  dir.create((paste0(out_dir,"qc/sdl/temp")))
}

plot_out <- paste0(out_dir,"qc/sdl/temp/")

for (col in c("airtemp_max_homogenized", "airtemp_min_homogenized",
              "airtemp_avg_homogenized")){
  
  
  p <- ggplot()+
    geom_line(data = d1_hmps, aes(date, airtemp_avg, color = 'd1_hmps_mean'))+
    geom_line(data = c1_hmps, aes(date, airtemp_avg, color = 'c1_hmps_mean'))+
    geom_line(data = sdl_homogenized |> filter( year > 2018),
              aes(date, .data[[col]], color = 'sdl_homogenized'))+
    geom_point(data = sdl_homogenized |>
                 dplyr::filter(year > 2018 & .data[['flag_1']] != 'AAA'),
               aes(date, .data[[col]], shape = .data[['flag_1']] == 'AAA'),
               color = 'red', size = 2)+
    xlim(lubridate::date("2023-01-01"), lubridate::date("2023-12-31"))+
    theme(legend.position = 'bottom')+
    
    ggtitle('SDL Homogenized / Infilled Cross Site Comparison')+
    scale_color_brewer(palette = 'Set2')+
    labs(y= col, x = 'Date')
  
  ggsave(paste0(plot_out, col, "_sdl_infilled_2023.png"), plot = p, device = 'png')
  
}

# -
# sdl hmps only (from homogenized dataset)
if(!dir.exists(paste0(out_dir,"qc/sdl/hmps/"))){
  dir.create((paste0(out_dir,"qc/sdl/hmps/")))
}

for (rep in 1:3){
  sensor = paste0('hmp',rep)
  if(!dir.exists(paste0(out_dir,"qc/sdl/hmps/hmp",rep,'/'))){
    dir.create((paste0(out_dir,"qc/sdl/hmps/hmp",rep,'/')))
  }
  
  plot_out <- paste0(out_dir,"qc/sdl/hmps/hmp",rep,'/')
  
  for (metric in c("airtemp_max", "airtemp_min", "airtemp_avg")){
    col = paste0(metric,"_gapfilled_",sensor)
    
    print(paste('Plotting', col, '...'))
    
    for (y in 2023){
      p <- ggplot()+
        # geom_line(data = sdl_hmps |> filter( lubridate::year(date) == y),
        #           aes(date, airtemp_avg, color = 'sdl_hmps_mean'))+
        # geom_line(data = c1_hmps |> filter( lubridate::year(date) == y),
        #           aes(date, airtemp_avg, color = 'c1_hmps_mean'))+
        geom_line(data = sdl_homogenized |> filter( lubridate::year(date) == y), 
                  aes(date, .data[[col]]))+
        geom_point(data = sdl_homogenized |> filter(year  == y) |> 
                     dplyr::filter(.data[[paste0("flag_1_",sensor)]] != 'A'),
                   aes(date, .data[[col]], shape = .data[[paste0("flag_1_",sensor)]]),
                   color = 'red', size = 2)+
        theme(legend.position = 'bottom')+
        
        ggtitle(paste('SDL', sensor, 'only - Infilled', y))+
        scale_color_brewer(palette = 'Set2')+
        labs(y= col, x = 'Date')
      
      ggsave(paste0(plot_out, col, "_sdl_infilled_",y,".png"), plot = p, device = 'png')
      
    }
  }
}


# ppt
if(!dir.exists(paste0(out_dir,"qc/sdl/ppt/"))){
  dir.create((paste0(out_dir,"qc/sdl/ppt/")))
}

plot_out <- paste0(out_dir,"qc/sdl/ppt/")

for (col in c("precip")){
  
  
  
  for (y in 2023){
    p <- ggplot()+
      geom_line(data = d1_ppt |> filter( lubridate::year(date) == y),
                aes(date, precip, color = 'd1_ppt'))+
      geom_line(data = c1_ppt |> filter( lubridate::year(date) == y),
                aes(date, precip, color = 'c1_ppt'))+
      geom_line(data = sdl_ppt |> filter( lubridate::year(date) == y), 
                aes(date, .data[[col]], color = 'sdl_ppt'))+
      geom_point(data = sdl_ppt |>
                   dplyr::filter(lubridate::year(date) == y & .data[['flag_1']] != 'A'),
                 aes(date, .data[[col]], shape = .data[['flag_1']] == 'A'),
                 color = 'red', size = 2)+
      theme(legend.position = 'bottom')+
      ggtitle(paste('SDL PPT Infilled Cross Site Comparison', y))+
      scale_color_brewer(palette = 'Set2', c('t', 'f'))+
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
                                     .data[['flag_1']] != 'A'),
                   aes(date, .data[[col]], shape = .data[['flag_1']]),
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
