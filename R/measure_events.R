
#' Measure single molecule events detected by Hidden Markov Model
#'
#' @param data
#' @param conversion
#'
#' @return a tibble with event duration and step size for each event
#' @export
#'
#' @examples
#'
#' measure_events(data = depmix_posterior,
#' conversion = raw_trace_length/running_calcs_length,
#'  run_mean = running_mean_object_given_2_hmm)

measure_events <- function(data, conversion, run_mean_data){

#setup

  #convert running mean object to tibble
  run_mean_tibble <- tibble::enframe(run_mean_data)

  #finds lengths of events in number of running windows
  run_length_encoding <- rle(data$state)

  #converting to a tibble
  rle_object <- as_tibble(do.call("cbind", run_length_encoding))

  #make a copy of data for event duration analysis
  rle_object_4_duration <- rle_object %>% dplyr::filter(values == 2)

  #calculates the events durations
  on_times <- rle_object_4_duration %>%
    dplyr::mutate(n_event = 1:nrow(rle_object_4_duration),
                  length_5kHz = lengths*conversion,
                  event_duration_ms = (length_5kHz/5000)*1000) %>%
    dplyr::select(n_event,values, everything()) %>%
    dplyr::rename("num_windows" = lengths,
                  "hmm_state" = values)

  #calculate event displacement

  #If the rle_object's last row is in state 1, get rid of that last row. This needs to end in state 2 to capture the end of the last event
  rle_object_4_step_sizes <- if(tail(rle_object, 1)$values == 1) slice(rle_object, -length(rle_object$values))

  #Calculate the cumulative sum of the run length encoder
  #And splits the tibble into two seperate tables to isolate state 1 info from state 2

  split_data <- rle_object_4_step_sizes %>%
    dplyr::mutate(cumsum = cumsum(lengths)) %>%
    dplyr::group_by(values) %>%
    split(rle_object_4_step_sizes$values)

  #data is recmombined in a state_1 column and a state_2
  #the values in these columsn represent the last data point in either state 1 or state 2
  #So the range of values between the end of state 1 (or start of state 2) and the end of state 2 is the event duration
  regroup_data <- bind_cols(state_1_end = split_data[[1]]$cumsum, state_2_end = split_data[[2]]$cumsum)

  #loop over regrouped data to find the mean of the events displacements
  step_sizes <- vector("list", length = nrow(regroup_data)) #allocate space for output storage of loop
  for(i in seq_along(1:nrow(regroup_data))){
    step_sizes[[i]] <- mean(run_mean_tibble$value[(regroup_data$state_1_end[i]+1) : (regroup_data$state_2_end[i])])
  }


  #add step sizes to the on_times table
  measured_events <- on_times %>% mutate(step_size_nm = unlist(step_sizes))

  return(measured_events)

}
