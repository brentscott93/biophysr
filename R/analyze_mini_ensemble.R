

#' Analyze Mini Ensemble Data - Finds Event Duration, Force, and Time Offs
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
analyze_mini_ensemble <- function(data, mv2nm, detrend){

#PROCESS DATA
#convert to mV to nm
raw_data_4_processing <- tibble(raw_trap = data*mv2nm,
                   index = 1:length(trap))


#estimate baseline mean

loess_smoothed_data <- loess(trap ~ index, data = raw_data_4_processing, span = 0.05)

loess_prediction_line <- predict(loess_smoothed_data)

low_pts <- find_peaks(-loess_prediction_line, m = 12500)

baseline_mean <- vector("list")
for(i in seq_along(low_pts)){

  baseline_data_chunk <- raw_data_4_processing$raw_trap[(low_pts[[i]] - 25) : (low_pts[[i]] + 25)]
  baseline_mean[[i]] <- mean(baseline_data_chunk)

}

esimated_baseline <- mean(unlist(baseline_mean))

#center to 0

zero_centered <- raw_data_4_processing$raw_trap - estimated_baseline

#detrend

processed <- if(detrend == TRUE){

  detrend_trap(zero_centered)

} else if(detrend == FALSE){

  zero_centered

}

#processing done

raw_data <- raw_data_4_processing %>%
mutate(trap = processed)

#calculate running mean

run_mean <- running(raw_data$trap, fun = mean, width = 50, by = 1)
run_mean0 <- ifelse(run_mean < 0, 0, run_mean)

#Determine if run_mean is in an event or baseline noise by using >8 as event
on_off <- ifelse(run_mean > 8, 2, 1)

rle_object<- as_tibble(do.call("cbind", rle(on_off)))

#find initial event start/stop
#If the rle_object's last row is in state 1, get rid of that last row. This needs to end in state 2 to capture the end of the last event
mini_rle_object <- if(tail(rle_object, 1)$values == 1){
  slice(rle_object, -length(rle_object$values))
} else {
  rle_object
}

split_data <- mini_rle_object %>%
  dplyr::mutate(cumsum = cumsum(lengths)) %>%
  dplyr::group_by(values) %>%
  split(mini_rle_object$values)

#data is recombined in a state_1 column and a state_2
#the values in these columns represent the last data point in either state 1 or state 2
#So the range of values between the end of state 1 (or start of state 2) and the end of state 2 is the event duration
regroup_data <- bind_cols(state_1_end = split_data[[1]]$cumsum, state_2_end = split_data[[2]]$cumsum) %>%
  mutate(event_duration_dp = state_2_end - state_1_end)

#filter out state 2s that are less than 10 ms (50 data points)
events <- regroup_data %>%
  filter(event_duration_dp > 50)




scale_by_event_index <- data.frame(state_1_start = c(0, events$state_2_end[-length(events$state_2_end)] + 1),
                                   state_2_end = events$state_2_end)

prior_noise_plus_event <- vector("list")
for(i in 1:nrow(scale_by_event_index)){
  prior_noise_plus_event[[i]] <- raw_data$trap[scale_by_event_index$state_1_start[i]:scale_by_event_index$state_2_end[i]]
}

state_1_index <-  data.frame(state_1_start = scale_by_event_index$state_1_start,
                             state_1_end = events$state_1_end)

state_1_means <- vector("list")
for(i in 1:nrow(state_1_index)){
  state_1_means[[i]] <- mean(raw_data$trap[state_1_index$state_1_start[i]:state_1_index$state_1_end[i]])

}

rescaled_vectors <- vector("list")
for(i in 1:length(prior_noise_plus_event)){
  rescaled_vectors[[i]] <- prior_noise_plus_event[[i]] - state_1_means[[i]]
}

##### FIND BETTER START OF EVENT########

end_of_last_event <- max(length(events$state_2_end))
last_s1_start <- events$state_2_end[end_of_last_event]+ 1
end_raw <- length(raw_data$trap)

rescaled_raw_data <- tibble(trap = c(unlist(rescaled_vectors), raw_data$trap[last_s1_start : end_raw]),
                            index = seq(1, length(trap)))


run_mean_rescaled <- running(rescaled_raw_data$trap, fun = mean, width = 50, by = 1)

rescaled_events <- identify_mini_events(raw_data, run_mean_rescaled)

##### FIND OFF TIMES #######

minus1 <- rescaled_events$state_1_end[-1]
minus2 <- rescaled_events$state_2_end[-length(rescaled_events$state_2_end)]

off_time_index <- bind_cols(state_1_start = minus2 + 1, state_1_end = minus1) %>%
  mutate(off_time_dp = (state_1_end - state_1_start) +1,
         off_time_sec = off_time_dp/5000,
         off_time_ms = off_time_sec*1000)

###### FORCES #####

forces <- vector()
for(i in 1:nrow(rescaled_events)){
  temp_vector <- rescaled_raw_data$trap[(rescaled_events$state_1_end[i] + 1) : (rescaled_events$state_2_end[i])]

  forces[i] <- max(running(temp_vector, fun = mean, width = 10, by = 1))

}

converted_force = forces*0.04

##### COMBINE ALL EVENT DATA ####

rescaled_events$force <- converted_force



final_events <-  rescaled_events %>%
  mutate(off_time_prior_dp = c(NA, off_time_index$off_time_dp),
         off_time_prior_sec = off_time_prior_dp/5000,
         off_time_prior_ms = off_time_prior_sec*1000,
         raw_event_duration_dp = state_2_end - state_1_end,
         raw_event_duration_sec = raw_event_duration_dp/5000,
         raw_event_duration_ms = raw_event_duration_sec * 1000,
         peak_nm = forces) %>%
  dplyr::select(state_1_end, state_2_end, raw_event_duration_ms, off_time_prior_ms, force, everything()) %>%
  rename(end_s1 = state_1_end,
         end_s2 = state_2_end)



#plot

filter_final_events1 <- filter(final_events, end_s2 < 20000)
filter_final_events2 <- filter(final_events, end_s2 > 20000 & end_s2 < 40000)
filter_final_events3 <- filter(final_events, end_s2 > max(final_events$end_s2) - 20000)

run_mean_rescaled0 <- ifelse(run_mean_rescaled < 0 , 0, run_mean_rescaled)
#1

p1 <- ggplot()+
  geom_line(aes(x = 1:20000, y = rescaled_raw_data$trap[1: 20000 ]))+
  geom_line(aes(x = 1: 20000, y = run_mean_rescaled0[1:20000]),color = "lightskyblue")+
  geom_line(aes(x = 1:20000, y = rep(8, 20000)), color = "gray50")+
  geom_point(aes(x = filter_final_events1$end_s2, y = filter_final_events1$peak_nm), color = "gold")+
  geom_point(aes(x = filter_final_events1$end_s1, y = run_mean_rescaled0[filter_final_events1$end_s1]), color = "green", shape = 17, size = 2)+
  geom_point(aes(x = filter_final_events1$end_s2, y = run_mean_rescaled0[filter_final_events1$end_s2]), color = "red", shape = 4)+
  theme_bw()+
  ylab("Discplacement (nm)")+
  xlab("Time (data points)")



#2

p2 <- ggplot()+
  geom_line(aes(x = 20001:40000, y = rescaled_raw_data$trap[20001: 40000 ]))+
  geom_line(aes(x = 20001: 40000, y = run_mean_rescaled0[20001:40000]),color = "lightskyblue")+
  geom_line(aes(x = 20001:40000, y = rep(8, 20000)), color = "gray50")+
  geom_point(aes(x = filter_final_events2$end_s2, y = filter_final_events2$peak_nm), color = "gold")+
  geom_point(aes(x = filter_final_events2$end_s1, y = run_mean_rescaled0[filter_final_events2$end_s1]), color = "green", shape = 17, size = 2)+
  geom_point(aes(x = filter_final_events2$end_s2, y = run_mean_rescaled0[filter_final_events2$end_s2]), color = "red", shape = 4)+
  theme_bw()+
  ylab("Discplacement (nm)")+
  xlab("Time (data points)")


#3
p3 <- ggplot()+
  geom_line(aes(x = (max(final_events$end_s2) - 20000):max(final_events$end_s2), y = rescaled_raw_data$trap[(max(final_events$end_s2) - 20000):max(final_events$end_s2)]))+
  geom_line(aes(x = (max(final_events$end_s2) - 20000):max(final_events$end_s2), y = run_mean_rescaled0[(max(final_events$end_s2) - 20000):max(final_events$end_s2)]),color = "lightskyblue")+
  geom_line(aes(x = (max(final_events$end_s2) - 20000):max(final_events$end_s2), y = rep(8, 20001)), color = "grey50")+
  geom_point(aes(x = filter_final_events3$end_s2, y = filter_final_events3$peak_nm), color = "gold")+
  geom_point(aes(x = filter_final_events3$end_s1, y = run_mean_rescaled0[filter_final_events3$end_s1]), color = "green", shape = 17, size = 2)+
  geom_point(aes(x = filter_final_events3$end_s2, y = run_mean_rescaled0[filter_final_events3$end_s2]), color = "red", shape = 4)+
  theme_bw()+
  ylab("Discplacement (nm)")+
  xlab("Time (data points)")


plots <-  arrangeGrob(p1, p2, p3, ncol = 1)

return_list <- list(data = final_events,
                    plot = plots)

return(return_list)


}
