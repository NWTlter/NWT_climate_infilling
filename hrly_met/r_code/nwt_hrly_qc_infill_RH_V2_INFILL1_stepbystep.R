#####################################################################
#####################################################################
#Note, the below needs to be run by manually entering the loop numbers
#There is an issue with the arima for c1 ARIMA #4
#And SDL ARIMA 10
#Run i individually
#1) i = 1 and q = 1, then k in 1:3
#2) then k in 5:length(rh_gap_ARIMA_l)
#3) then q = 2 and k in 1:3
#4) then k in 5:length(rh_gap_ARIMA_l)
#5) then i = 2 (run all)
#6) then i = 3 and k in 1:9


#The below has how the code was run in the console

> load("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/nwt_hrly_qc_infill_RH_V2_THRUQC3.RData")
> version = c("a", "b")
> i = 1
> q =1
> rh_gap <- #subset to all missing observations
  +     rh[[i]][is.na(rh[[i]][, paste0("rh_QC3", version[q]) ]) , ]
> 
  > rh_gap$time_diff <- #calculate time diff in hours
    +     c(0, diff.POSIXt(rh_gap$datetime)) 
  > 
    > tmp <-
      +     head(diff.POSIXt(rh_gap$datetime))
    > 
      > if(attr(tmp, which = "units") == "secs") {
        +     #Divide time_diff by 3600 if differences are in seconds not hours
          +     #That diff.posix units can't be controlled is a known error in R
          +     rh_gap$time_diff <- rh_gap$time_diff / 3600
          + }
    > 
      > #assign dummy gap number and initiate gap counter (for numbering gaps)
      > rh_gap$gap_num <- 0
      > gap_counter = 1
      > 
        > for (j in 1:length(rh_gap$time_diff)){
          +     if (rh_gap[j, "time_diff"] > 1) { 
            +         gap_counter = #increase gap counter by 1 if gap longer than 1 h
              +             gap_counter + 1
            +         rh_gap[j, "gap_num"] = #assign new gap#
              +             gap_counter
            +     } else {
              +         rh_gap[j, "gap_num"] = #assign gap number
                +             gap_counter
              +     }
          + }
      > 
        > if(q == 1){
          +     rh_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
            +         rh_gap %>% 
            +         group_by(gap_num) %>% 
            +         mutate(gap_length = length(rh_QC3a)) } else {
              +             rh_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
                +                 rh_gap %>% 
                +                 group_by(gap_num) %>% 
                +                 mutate(gap_length = length(rh_QC3b)) 
              +         }
      > 
        > rh_gap$fill_type <- #assign gap categorization
          +     fun_FILLTYPE(rh_gap$gap_length)
        > 
          > rh[[i]] <- #Merge gap information with complete dataset
            +     merge(rh[[i]], rh_gap[ , c("datetime", "gap_num", "gap_length", "fill_type")],
                        +           by = "datetime", all.x = TRUE)
          > 
            > rh[[i]] <- #make sure df is ordered by datetime
              +     arrange(rh[[i]], datetime)
            > 
              > #Add time shifted values for linear interpolation (filltype = INTERP)
              > rh[[i]]$shiftpos1 <- 
                +     c(NA, rh[[i]][1: (length(rh[[i]]$datetime) - 1), paste0("rh_QC3", version[q])])
              > rh[[i]]$shiftneg1 <- 
                +     c(rh[[i]][2: (length(rh[[i]]$datetime)), paste0("rh_QC3", version[q])], NA)
              > 
                > #fill gaps = 1 h
                > rh[[i]][, paste0("rh_FILL1", version[q])] <- 
                  +     ifelse(is.na(rh[[i]]$fill_type) == TRUE,
                               +            rh[[i]][, paste0("rh_QC3", version[q])], #if no fill needed, use rh_QC3
                               +            ifelse(rh[[i]]$fill_type == "INTERP", 
                                                   +                   (rh[[i]]$shiftpos1 + rh[[i]]$shiftneg1)/2,
                                                   +                   rh[[i]][, paste0("rh_QC3", version[q])]))
                > 
                  > #Add time shifted values for 24 h pre/post averaging (filltype = AVG24)
                  > rh[[i]]$shiftpos24 <- 
                    +     c(rep(NA, times = 24), rh[[i]][1: (length(rh[[i]]$datetime) - 24), paste0("rh_FILL1", version[q])])
                  > rh[[i]]$shiftneg24 <- 
                    +     c(rh[[i]][25: (length(rh[[i]]$datetime)), paste0("rh_FILL1", version[q])], rep(NA, times = 24))
                  > 
                    > #fill gaps > 1 h and <= 24 h
                    > rh[[i]][, paste0("rh_FILL1", version[q])] <- 
                      +     ifelse(is.na(rh[[i]]$fill_type) == TRUE,
                                   +            rh[[i]][, paste0("rh_FILL1", version[q])], #if no fill needed, use previously filled value (i.e. QC3)
                                   +            ifelse(rh[[i]]$fill_type == "AVG24",
                                                       +                   (rh[[i]]$shiftpos24 + rh[[i]]$shiftneg24)/2,
                                                       +                   rh[[i]][, paste0("rh_FILL1", version[q])]))
                    > 
                      > 
                      > rh_gap_ARIMA_l <- #Put all ARIMA filltypes into a list
                        +     split(filter(rh[[i]], fill_type == "ARIMA"), 
                                    +           f = filter(rh[[i]], fill_type == "ARIMA")$gap_num)
                      > 
                        > #Run ARIMAs for each 25-72 h set of missing observations
                        > for(k in 1:3){
                          +     gap = #calculate gap length of data to be filled
                            +         length(rh_gap_ARIMA_l[[k]]$datetime)
                          +     
                            +     #Acquire data before and after missing obs of equal length to missing data
                            +     tmp_gap_pre <- 
                              +         subset(rh[[i]], datetime < min(rh_gap_ARIMA_l[[k]]$datetime) &
                                                 +                    datetime >= (min(rh_gap_ARIMA_l[[k]]$datetime) - gap * 3600))
                            +     tmp_gap_post <- 
                              +         subset(rh[[i]], datetime > max(rh_gap_ARIMA_l[[k]]$datetime) &
                                                 +                    datetime <= (max(rh_gap_ARIMA_l[[k]]$datetime) + gap * 3600))
                            +     tmp_gap_post <- #reverse post data for backcasting
                              +         arrange(tmp_gap_post, desc(datetime))
                            +     
                              +     #Skip ARIMA if too many missing values in pre or post data
                              +     if(length(tmp_gap_pre[is.na(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1 | 
                                       +        length(tmp_gap_post[is.na(tmp_gap_post[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1) {
                                +         print(paste(names(rh[i]), k, version[q], "fail"))
                                +         next
                                +     }
                            +     
                              +     #Make forecast and backcast ARIMA predictions
                              +     tmp_gap_pre_fit <- 
                                +         arima(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                +               seasonal = list(order = c(0,0,1), period = 24))
                              +     tmp_gap_pre_forecast <- 
                                +         predict(tmp_gap_pre_fit, n.ahead = gap)
                              +     
                                +     tmp_gap_post_fit <- 
                                  +         arima(tmp_gap_post[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                  +               seasonal = list(order=c(0,0,1),period=24))
                                +     tmp_gap_post_backcast <- 
                                  +         predict(tmp_gap_post_fit, n.ahead = gap)
                                +     
                                  +     #Calculate the weights to be applied to the forecast and backcast
                                  +     wt1 <- ((gap - 1):0)/gap
                                  +     wt2 <- (0:(gap - 1))/gap
                                  +     
                                    +     #Predict missing data using through weighted forecast and backcast
                                    +     rh_gap_ARIMA_l[[k]][, paste0("rh_FILL1", version[q]) ] <-  
                                      +         (tmp_gap_pre_forecast$pred * wt1) +
                                      +         (rev(tmp_gap_post_backcast$pred)* wt2)
                                    +     
                                      +     #Remove missing rows from data
                                      +     rh[[i]] <- 
                                        +         subset(rh[[i]], datetime %in% rh_gap_ARIMA_l[[k]]$datetime == FALSE)
                                      +     
                                        +     #And add infilled data in its place
                                        +     rh[[i]] <- 
                                          +         rbind(rh[[i]], rh_gap_ARIMA_l[[k]])
                                        + }
                      [1] "c1 2 a fail"
                      > 
                        > 
                        > #Run ARIMAs for each 25-72 h set of missing observations
                        > for(k in 5:length(rh_gap_ARIMA_l)){
                          +     gap = #calculate gap length of data to be filled
                            +         length(rh_gap_ARIMA_l[[k]]$datetime)
                          +     
                            +     #Acquire data before and after missing obs of equal length to missing data
                            +     tmp_gap_pre <- 
                              +         subset(rh[[i]], datetime < min(rh_gap_ARIMA_l[[k]]$datetime) &
                                                 +                    datetime >= (min(rh_gap_ARIMA_l[[k]]$datetime) - gap * 3600))
                            +     tmp_gap_post <- 
                              +         subset(rh[[i]], datetime > max(rh_gap_ARIMA_l[[k]]$datetime) &
                                                 +                    datetime <= (max(rh_gap_ARIMA_l[[k]]$datetime) + gap * 3600))
                            +     tmp_gap_post <- #reverse post data for backcasting
                              +         arrange(tmp_gap_post, desc(datetime))
                            +     
                              +     #Skip ARIMA if too many missing values in pre or post data
                              +     if(length(tmp_gap_pre[is.na(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1 | 
                                       +        length(tmp_gap_post[is.na(tmp_gap_post[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1) {
                                +         print(paste(names(rh[i]), k, version[q], "fail"))
                                +         next
                                +     }
                            +     
                              +     #Make forecast and backcast ARIMA predictions
                              +     tmp_gap_pre_fit <- 
                                +         arima(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                +               seasonal = list(order = c(0,0,1), period = 24))
                              +     tmp_gap_pre_forecast <- 
                                +         predict(tmp_gap_pre_fit, n.ahead = gap)
                              +     
                                +     tmp_gap_post_fit <- 
                                  +         arima(tmp_gap_post[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                  +               seasonal = list(order=c(0,0,1),period=24))
                                +     tmp_gap_post_backcast <- 
                                  +         predict(tmp_gap_post_fit, n.ahead = gap)
                                +     
                                  +     #Calculate the weights to be applied to the forecast and backcast
                                  +     wt1 <- ((gap - 1):0)/gap
                                  +     wt2 <- (0:(gap - 1))/gap
                                  +     
                                    +     #Predict missing data using through weighted forecast and backcast
                                    +     rh_gap_ARIMA_l[[k]][, paste0("rh_FILL1", version[q]) ] <-  
                                      +         (tmp_gap_pre_forecast$pred * wt1) +
                                      +         (rev(tmp_gap_post_backcast$pred)* wt2)
                                    +     
                                      +     #Remove missing rows from data
                                      +     rh[[i]] <- 
                                        +         subset(rh[[i]], datetime %in% rh_gap_ARIMA_l[[k]]$datetime == FALSE)
                                      +     
                                        +     #And add infilled data in its place
                                        +     rh[[i]] <- 
                                          +         rbind(rh[[i]], rh_gap_ARIMA_l[[k]])
                                        + }
                      [1] "c1 5 a fail"
                      [1] "c1 6 a fail"
                      [1] "c1 9 a fail"
                      [1] "c1 17 a fail"
                      Warning message:
                        In arima(tmp_gap_pre[, paste0("rh_FILL1", version[q])], order = c(1,  :
                                                                                            possible convergence problem: optim gave code = 1
                                                                                          > 
                                                                                            > rh[[i]] <- #make sure df is ordered by datetime
                                                                                            +       arrange(rh[[i]], datetime)
                                                                                          >     
                                                                                            >     rh[[i]][, paste0("rh_FLAG4", version[q]) ] <- 
                                                                                            +       ifelse(is.na(rh[[i]][, paste0("rh_QC3", version[q]) ]) == is.na(rh[[i]][, paste0("rh_FILL1", version[q]) ]) ,
                                                                                                           +              NA,
                                                                                                           +              4) #flag 4 means filled with same station time series data (concatenate with fill type for more info)
                                                                                          >     
                                                                                            >     #remove the "gap_num", "gap_length", "fill_type" columns (to prevent issues when going between vers a and b)
                                                                                            >     if(q == 1){
                                                                                              +       rh[[i]][, c("gap_num", "gap_length", "fill_type")] <- NULL
                                                                                              +     }
                                                                                          > q = 2
                                                                                          > rh_gap <- #subset to all missing observations
                                                                                            +     rh[[i]][is.na(rh[[i]][, paste0("rh_QC3", version[q]) ]) , ]
                                                                                          > 
                                                                                            > rh_gap$time_diff <- #calculate time diff in hours
                                                                                            +     c(0, diff.POSIXt(rh_gap$datetime)) 
                                                                                          > 
                                                                                            > tmp <-
                                                                                            +     head(diff.POSIXt(rh_gap$datetime))
                                                                                          > 
                                                                                            > if(attr(tmp, which = "units") == "secs") {
                                                                                              +     #Divide time_diff by 3600 if differences are in seconds not hours
                                                                                                +     #That diff.posix units can't be controlled is a known error in R
                                                                                                +     rh_gap$time_diff <- rh_gap$time_diff / 3600
                                                                                                + }
                                                                                          > 
                                                                                            > #assign dummy gap number and initiate gap counter (for numbering gaps)
                                                                                            > rh_gap$gap_num <- 0
                                                                                          > gap_counter = 1
                                                                                          > 
                                                                                            > for (j in 1:length(rh_gap$time_diff)){
                                                                                              +     if (rh_gap[j, "time_diff"] > 1) { 
                                                                                                +         gap_counter = #increase gap counter by 1 if gap longer than 1 h
                                                                                                  +             gap_counter + 1
                                                                                                +         rh_gap[j, "gap_num"] = #assign new gap#
                                                                                                  +             gap_counter
                                                                                                +     } else {
                                                                                                  +         rh_gap[j, "gap_num"] = #assign gap number
                                                                                                    +             gap_counter
                                                                                                  +     }
                                                                                              + }
                                                                                          > 
                                                                                            > if(q == 1){
                                                                                              +     rh_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
                                                                                                +         rh_gap %>% 
                                                                                                +         group_by(gap_num) %>% 
                                                                                                +         mutate(gap_length = length(rh_QC3a)) } else {
                                                                                                  +             rh_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
                                                                                                    +                 rh_gap %>% 
                                                                                                    +                 group_by(gap_num) %>% 
                                                                                                    +                 mutate(gap_length = length(rh_QC3b)) 
                                                                                                  +         }
                                                                                          > 
                                                                                            > rh_gap$fill_type <- #assign gap categorization
                                                                                            +     fun_FILLTYPE(rh_gap$gap_length)
                                                                                          > 
                                                                                            > rh[[i]] <- #Merge gap information with complete dataset
                                                                                            +     merge(rh[[i]], rh_gap[ , c("datetime", "gap_num", "gap_length", "fill_type")],
                                                                                                        +           by = "datetime", all.x = TRUE)
                                                                                          > 
                                                                                            > rh[[i]] <- #make sure df is ordered by datetime
                                                                                            +     arrange(rh[[i]], datetime)
                                                                                          > 
                                                                                            > #Add time shifted values for linear interpolation (filltype = INTERP)
                                                                                            > rh[[i]]$shiftpos1 <- 
                                                                                            +     c(NA, rh[[i]][1: (length(rh[[i]]$datetime) - 1), paste0("rh_QC3", version[q])])
                                                                                          > rh[[i]]$shiftneg1 <- 
                                                                                            +     c(rh[[i]][2: (length(rh[[i]]$datetime)), paste0("rh_QC3", version[q])], NA)
                                                                                          > 
                                                                                            > #fill gaps = 1 h
                                                                                            > rh[[i]][, paste0("rh_FILL1", version[q])] <- 
                                                                                            +     ifelse(is.na(rh[[i]]$fill_type) == TRUE,
                                                                                                         +            rh[[i]][, paste0("rh_QC3", version[q])], #if no fill needed, use rh_QC3
                                                                                                         +            ifelse(rh[[i]]$fill_type == "INTERP", 
                                                                                                                             +                   (rh[[i]]$shiftpos1 + rh[[i]]$shiftneg1)/2,
                                                                                                                             +                   rh[[i]][, paste0("rh_QC3", version[q])]))
                                                                                          > 
                                                                                            > #Add time shifted values for 24 h pre/post averaging (filltype = AVG24)
                                                                                            > rh[[i]]$shiftpos24 <- 
                                                                                            +     c(rep(NA, times = 24), rh[[i]][1: (length(rh[[i]]$datetime) - 24), paste0("rh_FILL1", version[q])])
                                                                                          > rh[[i]]$shiftneg24 <- 
                                                                                            +     c(rh[[i]][25: (length(rh[[i]]$datetime)), paste0("rh_FILL1", version[q])], rep(NA, times = 24))
                                                                                          > 
                                                                                            > #fill gaps > 1 h and <= 24 h
                                                                                            > rh[[i]][, paste0("rh_FILL1", version[q])] <- 
                                                                                            +     ifelse(is.na(rh[[i]]$fill_type) == TRUE,
                                                                                                         +            rh[[i]][, paste0("rh_FILL1", version[q])], #if no fill needed, use previously filled value (i.e. QC3)
                                                                                                         +            ifelse(rh[[i]]$fill_type == "AVG24",
                                                                                                                             +                   (rh[[i]]$shiftpos24 + rh[[i]]$shiftneg24)/2,
                                                                                                                             +                   rh[[i]][, paste0("rh_FILL1", version[q])]))
                                                                                          > 
                                                                                            > 
                                                                                            > rh_gap_ARIMA_l <- #Put all ARIMA filltypes into a list
                                                                                            +     split(filter(rh[[i]], fill_type == "ARIMA"), 
                                                                                                        +           f = filter(rh[[i]], fill_type == "ARIMA")$gap_num)
                                                                                          > 
                                                                                            > #Run ARIMAs for each 25-72 h set of missing observations
                                                                                            > for(k in 1:3){
                                                                                              +     gap = #calculate gap length of data to be filled
                                                                                                +         length(rh_gap_ARIMA_l[[k]]$datetime)
                                                                                              +     
                                                                                                +     #Acquire data before and after missing obs of equal length to missing data
                                                                                                +     tmp_gap_pre <- 
                                                                                                  +         subset(rh[[i]], datetime < min(rh_gap_ARIMA_l[[k]]$datetime) &
                                                                                                                     +                    datetime >= (min(rh_gap_ARIMA_l[[k]]$datetime) - gap * 3600))
                                                                                                +     tmp_gap_post <- 
                                                                                                  +         subset(rh[[i]], datetime > max(rh_gap_ARIMA_l[[k]]$datetime) &
                                                                                                                     +                    datetime <= (max(rh_gap_ARIMA_l[[k]]$datetime) + gap * 3600))
                                                                                                +     tmp_gap_post <- #reverse post data for backcasting
                                                                                                  +         arrange(tmp_gap_post, desc(datetime))
                                                                                                +     
                                                                                                  +     #Skip ARIMA if too many missing values in pre or post data
                                                                                                  +     if(length(tmp_gap_pre[is.na(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1 | 
                                                                                                           +        length(tmp_gap_post[is.na(tmp_gap_post[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1) {
                                                                                                    +         print(paste(names(rh[i]), k, version[q], "fail"))
                                                                                                    +         next
                                                                                                    +     }
                                                                                                +     
                                                                                                  +     #Make forecast and backcast ARIMA predictions
                                                                                                  +     tmp_gap_pre_fit <- 
                                                                                                    +         arima(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                                                                                    +               seasonal = list(order = c(0,0,1), period = 24))
                                                                                                  +     tmp_gap_pre_forecast <- 
                                                                                                    +         predict(tmp_gap_pre_fit, n.ahead = gap)
                                                                                                  +     
                                                                                                    +     tmp_gap_post_fit <- 
                                                                                                      +         arima(tmp_gap_post[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                                                                                      +               seasonal = list(order=c(0,0,1),period=24))
                                                                                                    +     tmp_gap_post_backcast <- 
                                                                                                      +         predict(tmp_gap_post_fit, n.ahead = gap)
                                                                                                    +     
                                                                                                      +     #Calculate the weights to be applied to the forecast and backcast
                                                                                                      +     wt1 <- ((gap - 1):0)/gap
                                                                                                      +     wt2 <- (0:(gap - 1))/gap
                                                                                                      +     
                                                                                                        +     #Predict missing data using through weighted forecast and backcast
                                                                                                        +     rh_gap_ARIMA_l[[k]][, paste0("rh_FILL1", version[q]) ] <-  
                                                                                                          +         (tmp_gap_pre_forecast$pred * wt1) +
                                                                                                          +         (rev(tmp_gap_post_backcast$pred)* wt2)
                                                                                                        +     
                                                                                                          +     #Remove missing rows from data
                                                                                                          +     rh[[i]] <- 
                                                                                                            +         subset(rh[[i]], datetime %in% rh_gap_ARIMA_l[[k]]$datetime == FALSE)
                                                                                                          +     
                                                                                                            +     #And add infilled data in its place
                                                                                                            +     rh[[i]] <- 
                                                                                                              +         rbind(rh[[i]], rh_gap_ARIMA_l[[k]])
                                                                                                            + }
                                                                                          [1] "c1 2 b fail"
                                                                                          > 
                                                                                            > 
                                                                                            > #Run ARIMAs for each 25-72 h set of missing observations
                                                                                            > for(k in 5:length(rh_gap_ARIMA_l)){
                                                                                              +     gap = #calculate gap length of data to be filled
                                                                                                +         length(rh_gap_ARIMA_l[[k]]$datetime)
                                                                                              +     
                                                                                                +     #Acquire data before and after missing obs of equal length to missing data
                                                                                                +     tmp_gap_pre <- 
                                                                                                  +         subset(rh[[i]], datetime < min(rh_gap_ARIMA_l[[k]]$datetime) &
                                                                                                                     +                    datetime >= (min(rh_gap_ARIMA_l[[k]]$datetime) - gap * 3600))
                                                                                                +     tmp_gap_post <- 
                                                                                                  +         subset(rh[[i]], datetime > max(rh_gap_ARIMA_l[[k]]$datetime) &
                                                                                                                     +                    datetime <= (max(rh_gap_ARIMA_l[[k]]$datetime) + gap * 3600))
                                                                                                +     tmp_gap_post <- #reverse post data for backcasting
                                                                                                  +         arrange(tmp_gap_post, desc(datetime))
                                                                                                +     
                                                                                                  +     #Skip ARIMA if too many missing values in pre or post data
                                                                                                  +     if(length(tmp_gap_pre[is.na(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1 | 
                                                                                                           +        length(tmp_gap_post[is.na(tmp_gap_post[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1) {
                                                                                                    +         print(paste(names(rh[i]), k, version[q], "fail"))
                                                                                                    +         next
                                                                                                    +     }
                                                                                                +     
                                                                                                  +     #Make forecast and backcast ARIMA predictions
                                                                                                  +     tmp_gap_pre_fit <- 
                                                                                                    +         arima(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                                                                                    +               seasonal = list(order = c(0,0,1), period = 24))
                                                                                                  +     tmp_gap_pre_forecast <- 
                                                                                                    +         predict(tmp_gap_pre_fit, n.ahead = gap)
                                                                                                  +     
                                                                                                    +     tmp_gap_post_fit <- 
                                                                                                      +         arima(tmp_gap_post[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                                                                                      +               seasonal = list(order=c(0,0,1),period=24))
                                                                                                    +     tmp_gap_post_backcast <- 
                                                                                                      +         predict(tmp_gap_post_fit, n.ahead = gap)
                                                                                                    +     
                                                                                                      +     #Calculate the weights to be applied to the forecast and backcast
                                                                                                      +     wt1 <- ((gap - 1):0)/gap
                                                                                                      +     wt2 <- (0:(gap - 1))/gap
                                                                                                      +     
                                                                                                        +     #Predict missing data using through weighted forecast and backcast
                                                                                                        +     rh_gap_ARIMA_l[[k]][, paste0("rh_FILL1", version[q]) ] <-  
                                                                                                          +         (tmp_gap_pre_forecast$pred * wt1) +
                                                                                                          +         (rev(tmp_gap_post_backcast$pred)* wt2)
                                                                                                        +     
                                                                                                          +     #Remove missing rows from data
                                                                                                          +     rh[[i]] <- 
                                                                                                            +         subset(rh[[i]], datetime %in% rh_gap_ARIMA_l[[k]]$datetime == FALSE)
                                                                                                          +     
                                                                                                            +     #And add infilled data in its place
                                                                                                            +     rh[[i]] <- 
                                                                                                              +         rbind(rh[[i]], rh_gap_ARIMA_l[[k]])
                                                                                                            + }
                                                                                          [1] "c1 5 b fail"
                                                                                          [1] "c1 6 b fail"
                                                                                          [1] "c1 9 b fail"
                                                                                          [1] "c1 17 b fail"
                                                                                          Warning message:
                                                                                            In arima(tmp_gap_pre[, paste0("rh_FILL1", version[q])], order = c(1,  :
                                                                                                                                                                possible convergence problem: optim gave code = 1
                                                                                                                                                              > 
                                                                                                                                                                > rh[[i]] <- #make sure df is ordered by datetime
                                                                                                                                                                +       arrange(rh[[i]], datetime)
                                                                                                                                                              >     
                                                                                                                                                                >     rh[[i]][, paste0("rh_FLAG4", version[q]) ] <- 
                                                                                                                                                                +       ifelse(is.na(rh[[i]][, paste0("rh_QC3", version[q]) ]) == is.na(rh[[i]][, paste0("rh_FILL1", version[q]) ]) ,
                                                                                                                                                                               +              NA,
                                                                                                                                                                               +              4) #flag 4 means filled with same station time series data (concatenate with fill type for more info)
                                                                                                                                                              >     
                                                                                                                                                                >     #remove the "gap_num", "gap_length", "fill_type" columns (to prevent issues when going between vers a and b)
                                                                                                                                                                >     if(q == 1){
                                                                                                                                                                  +       rh[[i]][, c("gap_num", "gap_length", "fill_type")] <- NULL
                                                                                                                                                                  +     }
                                                                                                                                                              > save.image("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/nwt_hrly_qc_infill_RH_V2_THRUFILL1_C1.RData")
                                                                                                                                                              > i = 2
                                                                                                                                                              > for (q in 1: length(version)){
                                                                                                                                                                +     rh_gap <- #subset to all missing observations
                                                                                                                                                                  +       rh[[i]][is.na(rh[[i]][, paste0("rh_QC3", version[q]) ]) , ]
                                                                                                                                                                +     
                                                                                                                                                                  +     rh_gap$time_diff <- #calculate time diff in hours
                                                                                                                                                                    +       c(0, diff.POSIXt(rh_gap$datetime)) 
                                                                                                                                                                  +     
                                                                                                                                                                    +     tmp <-
                                                                                                                                                                      +       head(diff.POSIXt(rh_gap$datetime))
                                                                                                                                                                    +     
                                                                                                                                                                      +     if(attr(tmp, which = "units") == "secs") {
                                                                                                                                                                        +       #Divide time_diff by 3600 if differences are in seconds not hours
                                                                                                                                                                          +       #That diff.posix units can't be controlled is a known error in R
                                                                                                                                                                          +       rh_gap$time_diff <- rh_gap$time_diff / 3600
                                                                                                                                                                          +     }
                                                                                                                                                                    +     
                                                                                                                                                                      +     #assign dummy gap number and initiate gap counter (for numbering gaps)
                                                                                                                                                                      +     rh_gap$gap_num <- 0
                                                                                                                                                                      +     gap_counter = 1
                                                                                                                                                                      +     
                                                                                                                                                                        +     for (j in 1:length(rh_gap$time_diff)){
                                                                                                                                                                          +       if (rh_gap[j, "time_diff"] > 1) { 
                                                                                                                                                                            +         gap_counter = #increase gap counter by 1 if gap longer than 1 h
                                                                                                                                                                              +           gap_counter + 1
                                                                                                                                                                            +         rh_gap[j, "gap_num"] = #assign new gap#
                                                                                                                                                                              +           gap_counter
                                                                                                                                                                            +       } else {
                                                                                                                                                                              +         rh_gap[j, "gap_num"] = #assign gap number
                                                                                                                                                                                +           gap_counter
                                                                                                                                                                              +       }
                                                                                                                                                                          +     }
                                                                                                                                                                      +     
                                                                                                                                                                        +     if(q == 1){
                                                                                                                                                                          +       rh_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
                                                                                                                                                                            +         rh_gap %>% 
                                                                                                                                                                            +         group_by(gap_num) %>% 
                                                                                                                                                                            +         mutate(gap_length = length(rh_QC3a)) } else {
                                                                                                                                                                              +           rh_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
                                                                                                                                                                                +             rh_gap %>% 
                                                                                                                                                                                +             group_by(gap_num) %>% 
                                                                                                                                                                                +             mutate(gap_length = length(rh_QC3b)) 
                                                                                                                                                                              +         }
                                                                                                                                                                      +     
                                                                                                                                                                        +     rh_gap$fill_type <- #assign gap categorization
                                                                                                                                                                          +       fun_FILLTYPE(rh_gap$gap_length)
                                                                                                                                                                        +     
                                                                                                                                                                          +     rh[[i]] <- #Merge gap information with complete dataset
                                                                                                                                                                            +       merge(rh[[i]], rh_gap[ , c("datetime", "gap_num", "gap_length", "fill_type")],
                                                                                                                                                                                          +             by = "datetime", all.x = TRUE)
                                                                                                                                                                          +     
                                                                                                                                                                            +     rh[[i]] <- #make sure df is ordered by datetime
                                                                                                                                                                              +       arrange(rh[[i]], datetime)
                                                                                                                                                                            +     
                                                                                                                                                                              +     #Add time shifted values for linear interpolation (filltype = INTERP)
                                                                                                                                                                              +     rh[[i]]$shiftpos1 <- 
                                                                                                                                                                                +       c(NA, rh[[i]][1: (length(rh[[i]]$datetime) - 1), paste0("rh_QC3", version[q])])
                                                                                                                                                                              +     rh[[i]]$shiftneg1 <- 
                                                                                                                                                                                +       c(rh[[i]][2: (length(rh[[i]]$datetime)), paste0("rh_QC3", version[q])], NA)
                                                                                                                                                                              +     
                                                                                                                                                                                +     #fill gaps = 1 h
                                                                                                                                                                                +     rh[[i]][, paste0("rh_FILL1", version[q])] <- 
                                                                                                                                                                                  +       ifelse(is.na(rh[[i]]$fill_type) == TRUE,
                                                                                                                                                                                                 +              rh[[i]][, paste0("rh_QC3", version[q])], #if no fill needed, use rh_QC3
                                                                                                                                                                                                 +              ifelse(rh[[i]]$fill_type == "INTERP", 
                                                                                                                                                                                                                       +                     (rh[[i]]$shiftpos1 + rh[[i]]$shiftneg1)/2,
                                                                                                                                                                                                                       +                     rh[[i]][, paste0("rh_QC3", version[q])]))
                                                                                                                                                                                +     
                                                                                                                                                                                  +     #Add time shifted values for 24 h pre/post averaging (filltype = AVG24)
                                                                                                                                                                                  +     rh[[i]]$shiftpos24 <- 
                                                                                                                                                                                    +       c(rep(NA, times = 24), rh[[i]][1: (length(rh[[i]]$datetime) - 24), paste0("rh_FILL1", version[q])])
                                                                                                                                                                                  +     rh[[i]]$shiftneg24 <- 
                                                                                                                                                                                    +       c(rh[[i]][25: (length(rh[[i]]$datetime)), paste0("rh_FILL1", version[q])], rep(NA, times = 24))
                                                                                                                                                                                  +     
                                                                                                                                                                                    +     #fill gaps > 1 h and <= 24 h
                                                                                                                                                                                    +     rh[[i]][, paste0("rh_FILL1", version[q])] <- 
                                                                                                                                                                                      +       ifelse(is.na(rh[[i]]$fill_type) == TRUE,
                                                                                                                                                                                                     +              rh[[i]][, paste0("rh_FILL1", version[q])], #if no fill needed, use previously filled value (i.e. QC3)
                                                                                                                                                                                                     +              ifelse(rh[[i]]$fill_type == "AVG24",
                                                                                                                                                                                                                           +                     (rh[[i]]$shiftpos24 + rh[[i]]$shiftneg24)/2,
                                                                                                                                                                                                                           +                     rh[[i]][, paste0("rh_FILL1", version[q])]))
                                                                                                                                                                                    +     
                                                                                                                                                                                      +     
                                                                                                                                                                                      +     rh_gap_ARIMA_l <- #Put all ARIMA filltypes into a list
                                                                                                                                                                                        +       split(filter(rh[[i]], fill_type == "ARIMA"), 
                                                                                                                                                                                                      +             f = filter(rh[[i]], fill_type == "ARIMA")$gap_num)
                                                                                                                                                                                      +     
                                                                                                                                                                                        +     #Run ARIMAs for each 25-72 h set of missing observations
                                                                                                                                                                                        +     for(k in 1:length(rh_gap_ARIMA_l)){
                                                                                                                                                                                          +       gap = #calculate gap length of data to be filled
                                                                                                                                                                                            +         length(rh_gap_ARIMA_l[[k]]$datetime)
                                                                                                                                                                                          +       
                                                                                                                                                                                            +       #Acquire data before and after missing obs of equal length to missing data
                                                                                                                                                                                            +       tmp_gap_pre <- 
                                                                                                                                                                                              +         subset(rh[[i]], datetime < min(rh_gap_ARIMA_l[[k]]$datetime) &
                                                                                                                                                                                                                 +                  datetime >= (min(rh_gap_ARIMA_l[[k]]$datetime) - gap * 3600))
                                                                                                                                                                                            +       tmp_gap_post <- 
                                                                                                                                                                                              +         subset(rh[[i]], datetime > max(rh_gap_ARIMA_l[[k]]$datetime) &
                                                                                                                                                                                                                 +                  datetime <= (max(rh_gap_ARIMA_l[[k]]$datetime) + gap * 3600))
                                                                                                                                                                                            +       tmp_gap_post <- #reverse post data for backcasting
                                                                                                                                                                                              +         arrange(tmp_gap_post, desc(datetime))
                                                                                                                                                                                            +       
                                                                                                                                                                                              +       #Skip ARIMA if too many missing values in pre or post data
                                                                                                                                                                                              +       if(length(tmp_gap_pre[is.na(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1 | 
                                                                                                                                                                                                         +          length(tmp_gap_post[is.na(tmp_gap_post[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1) {
                                                                                                                                                                                                +         print(paste(names(rh[i]), k, version[q], "fail"))
                                                                                                                                                                                                +         next
                                                                                                                                                                                                +       }
                                                                                                                                                                                            +       
                                                                                                                                                                                              +       #Make forecast and backcast ARIMA predictions
                                                                                                                                                                                              +       tmp_gap_pre_fit <- 
                                                                                                                                                                                                +         arima(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                                                                                                                                                                                +               seasonal = list(order = c(0,0,1), period = 24))
                                                                                                                                                                                              +       tmp_gap_pre_forecast <- 
                                                                                                                                                                                                +         predict(tmp_gap_pre_fit, n.ahead = gap)
                                                                                                                                                                                              +       
                                                                                                                                                                                                +       tmp_gap_post_fit <- 
                                                                                                                                                                                                  +         arima(tmp_gap_post[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                                                                                                                                                                                  +               seasonal = list(order=c(0,0,1),period=24))
                                                                                                                                                                                                +       tmp_gap_post_backcast <- 
                                                                                                                                                                                                  +         predict(tmp_gap_post_fit, n.ahead = gap)
                                                                                                                                                                                                +       
                                                                                                                                                                                                  +       #Calculate the weights to be applied to the forecast and backcast
                                                                                                                                                                                                  +       wt1 <- ((gap - 1):0)/gap
                                                                                                                                                                                                  +       wt2 <- (0:(gap - 1))/gap
                                                                                                                                                                                                  +       
                                                                                                                                                                                                    +       #Predict missing data using through weighted forecast and backcast
                                                                                                                                                                                                    +       rh_gap_ARIMA_l[[k]][, paste0("rh_FILL1", version[q]) ] <-  
                                                                                                                                                                                                      +         (tmp_gap_pre_forecast$pred * wt1) +
                                                                                                                                                                                                      +         (rev(tmp_gap_post_backcast$pred)* wt2)
                                                                                                                                                                                                    +       
                                                                                                                                                                                                      +       #Remove missing rows from data
                                                                                                                                                                                                      +       rh[[i]] <- 
                                                                                                                                                                                                        +         subset(rh[[i]], datetime %in% rh_gap_ARIMA_l[[k]]$datetime == FALSE)
                                                                                                                                                                                                      +       
                                                                                                                                                                                                        +       #And add infilled data in its place
                                                                                                                                                                                                        +       rh[[i]] <- 
                                                                                                                                                                                                          +         rbind(rh[[i]], rh_gap_ARIMA_l[[k]])
                                                                                                                                                                                                        +     }
                                                                                                                                                                                      +     rh[[i]] <- #make sure df is ordered by datetime
                                                                                                                                                                                        +       arrange(rh[[i]], datetime)
                                                                                                                                                                                      +     
                                                                                                                                                                                        +     rh[[i]][, paste0("rh_FLAG4", version[q]) ] <- 
                                                                                                                                                                                          +       ifelse(is.na(rh[[i]][, paste0("rh_QC3", version[q]) ]) == is.na(rh[[i]][, paste0("rh_FILL1", version[q]) ]) ,
                                                                                                                                                                                                         +              NA,
                                                                                                                                                                                                         +              4) #flag 4 means filled with same station time series data (concatenate with fill type for more info)
                                                                                                                                                                                        +     
                                                                                                                                                                                          +     #remove the "gap_num", "gap_length", "fill_type" columns (to prevent issues when going between vers a and b)
                                                                                                                                                                                          +     if(q == 1){
                                                                                                                                                                                            +       rh[[i]][, c("gap_num", "gap_length", "fill_type")] <- NULL
                                                                                                                                                                                            +     }
                                                                                                                                                                                        +   }
                                                                                                                                                              [1] "d1 2 a fail"
                                                                                                                                                              [1] "d1 3 a fail"
                                                                                                                                                              [1] "d1 4 a fail"
                                                                                                                                                              [1] "d1 5 a fail"
                                                                                                                                                              [1] "d1 6 a fail"
                                                                                                                                                              [1] "d1 7 a fail"
                                                                                                                                                              [1] "d1 10 a fail"
                                                                                                                                                              [1] "d1 12 a fail"
                                                                                                                                                              [1] "d1 13 a fail"
                                                                                                                                                              [1] "d1 2 b fail"
                                                                                                                                                              [1] "d1 3 b fail"
                                                                                                                                                              [1] "d1 4 b fail"
                                                                                                                                                              [1] "d1 5 b fail"
                                                                                                                                                              [1] "d1 6 b fail"
                                                                                                                                                              [1] "d1 7 b fail"
                                                                                                                                                              [1] "d1 10 b fail"
                                                                                                                                                              [1] "d1 12 b fail"
                                                                                                                                                              [1] "d1 13 b fail"
                                                                                                                                                              > save.image("~/Documents/research/site_data/niwot/nwt_hrly_qc/r_code_workspaces/nwt_hrly_qc_infill_RH_V2_THRUFILL1_D1.RData")
                                                                                                                                                              > i = 3
                                                                                                                                                              > for (q in 1: length(version)){
                                                                                                                                                                +     rh_gap <- #subset to all missing observations
                                                                                                                                                                  +       rh[[i]][is.na(rh[[i]][, paste0("rh_QC3", version[q]) ]) , ]
                                                                                                                                                                +     
                                                                                                                                                                  +     rh_gap$time_diff <- #calculate time diff in hours
                                                                                                                                                                    +       c(0, diff.POSIXt(rh_gap$datetime)) 
                                                                                                                                                                  +     
                                                                                                                                                                    +     tmp <-
                                                                                                                                                                      +       head(diff.POSIXt(rh_gap$datetime))
                                                                                                                                                                    +     
                                                                                                                                                                      +     if(attr(tmp, which = "units") == "secs") {
                                                                                                                                                                        +       #Divide time_diff by 3600 if differences are in seconds not hours
                                                                                                                                                                          +       #That diff.posix units can't be controlled is a known error in R
                                                                                                                                                                          +       rh_gap$time_diff <- rh_gap$time_diff / 3600
                                                                                                                                                                          +     }
                                                                                                                                                                    +     
                                                                                                                                                                      +     #assign dummy gap number and initiate gap counter (for numbering gaps)
                                                                                                                                                                      +     rh_gap$gap_num <- 0
                                                                                                                                                                      +     gap_counter = 1
                                                                                                                                                                      +     
                                                                                                                                                                        +     for (j in 1:length(rh_gap$time_diff)){
                                                                                                                                                                          +       if (rh_gap[j, "time_diff"] > 1) { 
                                                                                                                                                                            +         gap_counter = #increase gap counter by 1 if gap longer than 1 h
                                                                                                                                                                              +           gap_counter + 1
                                                                                                                                                                            +         rh_gap[j, "gap_num"] = #assign new gap#
                                                                                                                                                                              +           gap_counter
                                                                                                                                                                            +       } else {
                                                                                                                                                                              +         rh_gap[j, "gap_num"] = #assign gap number
                                                                                                                                                                                +           gap_counter
                                                                                                                                                                              +       }
                                                                                                                                                                          +     }
                                                                                                                                                                      +     
                                                                                                                                                                        +     if(q == 1){
                                                                                                                                                                          +       rh_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
                                                                                                                                                                            +         rh_gap %>% 
                                                                                                                                                                            +         group_by(gap_num) %>% 
                                                                                                                                                                            +         mutate(gap_length = length(rh_QC3a)) } else {
                                                                                                                                                                              +           rh_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
                                                                                                                                                                                +             rh_gap %>% 
                                                                                                                                                                                +             group_by(gap_num) %>% 
                                                                                                                                                                                +             mutate(gap_length = length(rh_QC3b)) 
                                                                                                                                                                              +         }
                                                                                                                                                                      +     
                                                                                                                                                                        +     rh_gap$fill_type <- #assign gap categorization
                                                                                                                                                                          +       fun_FILLTYPE(rh_gap$gap_length)
                                                                                                                                                                        +     
                                                                                                                                                                          +     rh[[i]] <- #Merge gap information with complete dataset
                                                                                                                                                                            +       merge(rh[[i]], rh_gap[ , c("datetime", "gap_num", "gap_length", "fill_type")],
                                                                                                                                                                                          +             by = "datetime", all.x = TRUE)
                                                                                                                                                                          +     
                                                                                                                                                                            +     rh[[i]] <- #make sure df is ordered by datetime
                                                                                                                                                                              +       arrange(rh[[i]], datetime)
                                                                                                                                                                            +     
                                                                                                                                                                              +     #Add time shifted values for linear interpolation (filltype = INTERP)
                                                                                                                                                                              +     rh[[i]]$shiftpos1 <- 
                                                                                                                                                                                +       c(NA, rh[[i]][1: (length(rh[[i]]$datetime) - 1), paste0("rh_QC3", version[q])])
                                                                                                                                                                              +     rh[[i]]$shiftneg1 <- 
                                                                                                                                                                                +       c(rh[[i]][2: (length(rh[[i]]$datetime)), paste0("rh_QC3", version[q])], NA)
                                                                                                                                                                              +     
                                                                                                                                                                                +     #fill gaps = 1 h
                                                                                                                                                                                +     rh[[i]][, paste0("rh_FILL1", version[q])] <- 
                                                                                                                                                                                  +       ifelse(is.na(rh[[i]]$fill_type) == TRUE,
                                                                                                                                                                                                 +              rh[[i]][, paste0("rh_QC3", version[q])], #if no fill needed, use rh_QC3
                                                                                                                                                                                                 +              ifelse(rh[[i]]$fill_type == "INTERP", 
                                                                                                                                                                                                                       +                     (rh[[i]]$shiftpos1 + rh[[i]]$shiftneg1)/2,
                                                                                                                                                                                                                       +                     rh[[i]][, paste0("rh_QC3", version[q])]))
                                                                                                                                                                                +     
                                                                                                                                                                                  +     #Add time shifted values for 24 h pre/post averaging (filltype = AVG24)
                                                                                                                                                                                  +     rh[[i]]$shiftpos24 <- 
                                                                                                                                                                                    +       c(rep(NA, times = 24), rh[[i]][1: (length(rh[[i]]$datetime) - 24), paste0("rh_FILL1", version[q])])
                                                                                                                                                                                  +     rh[[i]]$shiftneg24 <- 
                                                                                                                                                                                    +       c(rh[[i]][25: (length(rh[[i]]$datetime)), paste0("rh_FILL1", version[q])], rep(NA, times = 24))
                                                                                                                                                                                  +     
                                                                                                                                                                                    +     #fill gaps > 1 h and <= 24 h
                                                                                                                                                                                    +     rh[[i]][, paste0("rh_FILL1", version[q])] <- 
                                                                                                                                                                                      +       ifelse(is.na(rh[[i]]$fill_type) == TRUE,
                                                                                                                                                                                                     +              rh[[i]][, paste0("rh_FILL1", version[q])], #if no fill needed, use previously filled value (i.e. QC3)
                                                                                                                                                                                                     +              ifelse(rh[[i]]$fill_type == "AVG24",
                                                                                                                                                                                                                           +                     (rh[[i]]$shiftpos24 + rh[[i]]$shiftneg24)/2,
                                                                                                                                                                                                                           +                     rh[[i]][, paste0("rh_FILL1", version[q])]))
                                                                                                                                                                                    +     
                                                                                                                                                                                      +     
                                                                                                                                                                                      +     rh_gap_ARIMA_l <- #Put all ARIMA filltypes into a list
                                                                                                                                                                                        +       split(filter(rh[[i]], fill_type == "ARIMA"), 
                                                                                                                                                                                                      +             f = filter(rh[[i]], fill_type == "ARIMA")$gap_num)
                                                                                                                                                                                      +     
                                                                                                                                                                                        +     #Run ARIMAs for each 25-72 h set of missing observations
                                                                                                                                                                                        +     for(k in 1:9){
                                                                                                                                                                                          +       gap = #calculate gap length of data to be filled
                                                                                                                                                                                            +         length(rh_gap_ARIMA_l[[k]]$datetime)
                                                                                                                                                                                          +       
                                                                                                                                                                                            +       #Acquire data before and after missing obs of equal length to missing data
                                                                                                                                                                                            +       tmp_gap_pre <- 
                                                                                                                                                                                              +         subset(rh[[i]], datetime < min(rh_gap_ARIMA_l[[k]]$datetime) &
                                                                                                                                                                                                                 +                  datetime >= (min(rh_gap_ARIMA_l[[k]]$datetime) - gap * 3600))
                                                                                                                                                                                            +       tmp_gap_post <- 
                                                                                                                                                                                              +         subset(rh[[i]], datetime > max(rh_gap_ARIMA_l[[k]]$datetime) &
                                                                                                                                                                                                                 +                  datetime <= (max(rh_gap_ARIMA_l[[k]]$datetime) + gap * 3600))
                                                                                                                                                                                            +       tmp_gap_post <- #reverse post data for backcasting
                                                                                                                                                                                              +         arrange(tmp_gap_post, desc(datetime))
                                                                                                                                                                                            +       
                                                                                                                                                                                              +       #Skip ARIMA if too many missing values in pre or post data
                                                                                                                                                                                              +       if(length(tmp_gap_pre[is.na(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1 | 
                                                                                                                                                                                                         +          length(tmp_gap_post[is.na(tmp_gap_post[, paste0("rh_FILL1", version[q]) ]) , ]$datetime) > 1) {
                                                                                                                                                                                                +         print(paste(names(rh[i]), k, version[q], "fail"))
                                                                                                                                                                                                +         next
                                                                                                                                                                                                +       }
                                                                                                                                                                                            +       
                                                                                                                                                                                              +       #Make forecast and backcast ARIMA predictions
                                                                                                                                                                                              +       tmp_gap_pre_fit <- 
                                                                                                                                                                                                +         arima(tmp_gap_pre[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                                                                                                                                                                                +               seasonal = list(order = c(0,0,1), period = 24))
                                                                                                                                                                                              +       tmp_gap_pre_forecast <- 
                                                                                                                                                                                                +         predict(tmp_gap_pre_fit, n.ahead = gap)
                                                                                                                                                                                              +       
                                                                                                                                                                                                +       tmp_gap_post_fit <- 
                                                                                                                                                                                                  +         arima(tmp_gap_post[, paste0("rh_FILL1", version[q]) ], order = c(1,1,1), 
                                                                                                                                                                                                                  +               seasonal = list(order=c(0,0,1),period=24))
                                                                                                                                                                                                +       tmp_gap_post_backcast <- 
                                                                                                                                                                                                  +         predict(tmp_gap_post_fit, n.ahead = gap)
                                                                                                                                                                                                +       
                                                                                                                                                                                                  +       #Calculate the weights to be applied to the forecast and backcast
                                                                                                                                                                                                  +       wt1 <- ((gap - 1):0)/gap
                                                                                                                                                                                                  +       wt2 <- (0:(gap - 1))/gap
                                                                                                                                                                                                  +       
                                                                                                                                                                                                    +       #Predict missing data using through weighted forecast and backcast
                                                                                                                                                                                                    +       rh_gap_ARIMA_l[[k]][, paste0("rh_FILL1", version[q]) ] <-  
                                                                                                                                                                                                      +         (tmp_gap_pre_forecast$pred * wt1) +
                                                                                                                                                                                                      +         (rev(tmp_gap_post_backcast$pred)* wt2)
                                                                                                                                                                                                    +       
                                                                                                                                                                                                      +       #Remove missing rows from data
                                                                                                                                                                                                      +       rh[[i]] <- 
                                                                                                                                                                                                        +         subset(rh[[i]], datetime %in% rh_gap_ARIMA_l[[k]]$datetime == FALSE)
                                                                                                                                                                                                      +       
                                                                                                                                                                                                        +       #And add infilled data in its place
                                                                                                                                                                                                        +       rh[[i]] <- 
                                                                                                                                                                                                          +         rbind(rh[[i]], rh_gap_ARIMA_l[[k]])
                                                                                                                                                                                                        +     }
                                                                                                                                                                                      +     rh[[i]] <- #make sure df is ordered by datetime
                                                                                                                                                                                        +       arrange(rh[[i]], datetime)
                                                                                                                                                                                      +     
                                                                                                                                                                                        +     rh[[i]][, paste0("rh_FLAG4", version[q]) ] <- 
                                                                                                                                                                                          +       ifelse(is.na(rh[[i]][, paste0("rh_QC3", version[q]) ]) == is.na(rh[[i]][, paste0("rh_FILL1", version[q]) ]) ,
                                                                                                                                                                                                         +              NA,
                                                                                                                                                                                                         +              4) #flag 4 means filled with same station time series data (concatenate with fill type for more info)
                                                                                                                                                                                        +     
                                                                                                                                                                                          +     #remove the "gap_num", "gap_length", "fill_type" columns (to prevent issues when going between vers a and b)
                                                                                                                                                                                          +     if(q == 1){
                                                                                                                                                                                            +       rh[[i]][, c("gap_num", "gap_length", "fill_type")] <- NULL
                                                                                                                                                                                            +     }
                                                                                                                                                                                        +   }
                                                                                                                                                              [1] "sdl 1 a fail"
                                                                                                                                                              [1] "sdl 2 a fail"
                                                                                                                                                              [1] "sdl 3 a fail"
                                                                                                                                                              [1] "sdl 4 a fail"
                                                                                                                                                              [1] "sdl 5 a fail"
                                                                                                                                                              [1] "sdl 6 a fail"
                                                                                                                                                              [1] "sdl 1 b fail"
                                                                                                                                                              [1] "sdl 2 b fail"
                                                                                                                                                              [1] "sdl 3 b fail"
                                                                                                                                                              [1] "sdl 4 b fail"
                                                                                                                                                              [1] "sdl 5 b fail"
                                                                                                                                                              [1] "sdl 6 b fail"