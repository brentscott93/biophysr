
#' Identifies mini ensemble events (>10nm, >10ms)
#'
#' @param run_mean
#'
#' @return
#' @export
#'
#' @examples
identify_mini_events <- function(raw_data, run_mean){
  #Determine if run_mean is in an event or baseline noise by using >10 as event
  on_off <- ifelse(run_mean > 8, 2, 1)

  rle_object<- as_tibble(do.call("cbind", rle(on_off)))

  #if starts in state2/event get rid of it
  if(head(rle_object, 1)$values == 2){
    rle_object %<>% slice(2:nrow(rle_object))
  }

  #find initial event start/stop
  #If the rle_object's last row is in state 1, get rid of that last row. This needs to end in state 2 to capture the end of the last event
  mini_rle_object <- if(tail(rle_object, 1)$values == 1){
    slice(rle_object, -length(rle_object$values))
  } else {
    rle_object
  }

 # mini_rle_object <- if(head(mini_rle_object, 1)$values == 2){
   # slice(mini_rle_object, -1)
 # } else {
 #   mini_rle_object
 # }


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


  return(events)
}
