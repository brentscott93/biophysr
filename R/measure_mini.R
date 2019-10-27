

#' Measure ton and toff for mini ensemble data
#'
#' @param run_mean
#'
#' @return
#' @export
#'
#' @examples
measure_mini <- function(raw_data, run_mean){
#Determine if run_mean is in an event or baseline noise by using >10 as event
on_off <- ifelse(run_mean > 10, 2, 1)

rle_object<- as_tibble(do.call("cbind", rle(on_off)))

#### FIND EVENT DURATIONS ####
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

#data is recmombined in a state_1 column and a state_2
#the values in these columns represent the last data point in either state 1 or state 2
#So the range of values between the end of state 1 (or start of state 2) and the end of state 2 is the event duration
regroup_data <- bind_cols(state_1_end = split_data[[1]]$cumsum, state_2_end = split_data[[2]]$cumsum) %>%
  mutate(filtered_event_duration_dp = state_2_end - state_1_end)

#filter out state 2s that are less than 10 ms (50 data points)
events <- regroup_data %>%
  filter(filtered_event_duration_dp > 50)



event_vector <- vector("list")
for(i in 1:nrow(events)){

  event_vector[[i]] <- raw_data$trap[events$state_2_end[i] : length(raw_data$trap)]

}

#find first data point not in event
dp1_not_event <- sapply(event_vector, mini_find_true_end)

events$dp1_not_event <- dp1_not_event

events <- events %>%
  mutate(true_end = state_2_end + dp1_not_event,
         raw_event_duration_dp = true_end - (state_1_end + 1),
         raw_event_duration_sec = raw_event_duration_dp/5000,
         raw_event_duration_ms = raw_event_duration_sec * 1000)


##### FIND OFF TIMES #######

minus1 <- events$state_1_end[-1]
minus2 <- events$state_2_end[-length(events$state_2_end)]

off_time_index <- bind_cols(state_1_start = minus2 + 1, state_1_end = minus1) %>%
  mutate(off_time_dp = (state_1_end - state_1_start) +1,
         off_time_sec = off_time_dp/5000,
         off_time_ms = off_time_sec*1000)

forces <- vector()
for(i in 1:nrow(events)){
  temp_vector <- raw_data$trap[(events$state_1_end[i] + 1) : (events$state_2_end[i])]

  forces[i] <- max(running(temp_vector, fun = mean, width = 10, by = 1))

}
events$forces <- forces

final_events <- events %>%
  mutate(off_time_prior_dp = c(NA, off_time_index$off_time_dp),
         off_time_prior_sec = off_time_prior_dp/5000,
         off_time_prior_ms = off_time_prior_sec*1000)


}
