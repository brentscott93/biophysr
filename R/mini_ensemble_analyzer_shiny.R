#' Mini Ensemble analyzer for shiny
#'
#' @param parent_dir
#' @param mv2nm
#' @param nm2pn
#' @param run_mean_color
#'
#' @return
#' @export
#'
#' @examples
mini_ensemble_analyzer_shiny <- function(input, detrend, baseline_start_sec, baseline_end_sec, mv2nm, nm2pn, run_mean_color){


  miniFile <- input
  #Load data and convert mV to nm
  dat <- read.delim(miniFile, header = FALSE) %>%
    mutate(nm_converted = V1*mv2nm) %>%
    dplyr::pull(nm_converted)

  #PROCESS DATA
  #detrends data by either performing a piecewise linear detrend or simply removing baseline mean from all points (i.e. constant detrend)
  #both of these will center the mean around 0. It just depends if there needs to be long linear drift corrected or not

  processed <- if(detrend == "TRUE"){

    break_pts <- seq(25000, length(dat), by = 25000)

    pracma::detrend(dat, tt = "linear", bp = break_pts)

  } else if(detrend == "FALSE"){

    get_mean <- mean(dat[input$mini_baseline_start : input$mini_baseline_end])
    dat - get_mean

  }



  #build table for analysis

  raw_data <- tibble(index = time(processed),
                     trap = processed)


  #calculate running mean

  run_mean <- as.vector(rollmean(raw_data$trap, k = 50, align = "left"))
  run_mean0 <- ifelse(run_mean < 0, 0, run_mean)

  #Determine if run_mean is in an event or baseline noise by using >8 as event threshold

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


  dygraph_raw_data[[folder]] <- rescaled_raw_data$trap

  run_mean_rescaled <- as.vector(rollmean(rescaled_raw_data$trap, k = 50, align = "left"))



  run_mean_rescaled <- tibble(run_mean = run_mean_rescaled,
                              index = time(run_mean_rescaled))

  rescaled_events <- identify_mini_events(raw_data, run_mean_rescaled$run_mean)

  ##### FIND OFF TIMES #######

  minus1 <- rescaled_events$state_1_end[-1]
  minus2 <- rescaled_events$state_2_end[-length(rescaled_events$state_2_end)]

  off_time_index <- bind_cols(state_1_start = minus2 + 1, state_1_end = minus1) %>%
    mutate(off_time_dp = (state_1_end - state_1_start) +1,
           off_time_sec = off_time_dp/5000,
           off_time_ms = off_time_sec*1000)

  ###### FORCES #####

  peak_displacement_df <- tibble(run_mean = NA,
                                 index = NA)

  for(i in 1:nrow(rescaled_events)){
    temp_df <- run_mean_rescaled[(rescaled_events$state_1_end[i] + 1) : (rescaled_events$state_2_end[i]),]

    find_event_peak <- max(find_peaks(temp_df$run_mean, m = length(temp_df$run_mean)))

    peak_displacement_df[i,] <- temp_df[find_event_peak,]

  }

  peak_displacement_df <- peak_displacement_df %>%
    rename(displacement_nm = run_mean)%>%
    mutate(converted_force = displacement_nm*nm2pn)

  ##### COMBINE ALL EVENT DATA ####

  final_events <-  rescaled_events %>%
    mutate(off_time_prior_dp = c(NA, off_time_index$off_time_dp),
           off_time_prior_sec = off_time_prior_dp/5000,
           time_off_prior_ms = off_time_prior_sec*1000,
           raw_event_duration_dp = state_2_end - state_1_end,
           raw_event_duration_sec = raw_event_duration_dp/5000,
           time_on_ms = raw_event_duration_sec * 1000,
           displacement_nm = peak_displacement_df$displacement_nm,
           conditions = read_directions$condition[[folder]],
           observation = read_directions$folder[[folder]],
           event_num = 1:nrow(rescaled_events),
           force = peak_displacement_df$converted_force)%>%
    dplyr::select(event_num, conditions, observation, time_on_ms, time_off_prior_ms, displacement_nm, force)

  final_events_4_plot <-  rescaled_events %>%
    mutate(off_time_prior_dp = c(NA, off_time_index$off_time_dp),
           off_time_prior_sec = off_time_prior_dp/5000,
           time_off_prior_ms = off_time_prior_sec*1000,
           raw_event_duration_dp = state_2_end - state_1_end,
           raw_event_duration_sec = raw_event_duration_dp/5000,
           time_on_ms = raw_event_duration_sec * 1000,
           peak_nm = peak_displacement_df$displacement_nm,
           conditions = read_directions$condition[[folder]],
           observation = read_directions$folder[[folder]],
           event_num = 1:nrow(rescaled_events),
           force = peak_displacement_df$converted_force,
           peak_nm_index = peak_displacement_df$index)%>%
    rename(end_s1 = state_1_end,
           end_s2 = state_2_end)




  #plot

  filter_final_events1 <- filter(final_events_4_plot, end_s2 < 20000)
  filter_final_events2 <- filter(final_events_4_plot, end_s2 > 20000 & end_s2 < 40000)
  filter_final_events3 <- filter(final_events_4_plot, end_s2 > max(final_events_4_plot$end_s2) - 40000 & end_s2 < max(final_events_4_plot$end_s2) - 20000)
  filter_final_events4 <- filter(final_events_4_plot, end_s2 > max(final_events_4_plot$end_s2) - 20000)


  run_mean_rescaled0 <- ifelse(run_mean_rescaled$run_mean < 0 , 0, run_mean_rescaled$run_mean)


  if(length(dat) > 100001){
    #1

    p1 <- ggplot()+
      geom_line(aes(x = 1:20000, y = rescaled_raw_data$trap[1: 20000 ]))+
      geom_line(aes(x = 1: 20000, y = run_mean_rescaled0[1:20000]),color = run_mean_color)+
      geom_line(aes(x = 1:20000, y = rep(8, 20000)), color = "gray50")+
      geom_point(aes(x = filter_final_events1$peak_nm_index, y = filter_final_events1$peak_nm), color = "gold")+
      geom_point(aes(x = filter_final_events1$end_s1, y = run_mean_rescaled0[filter_final_events1$end_s1]), color = "green", shape = 17, size = 2)+
      geom_point(aes(x = filter_final_events1$end_s2, y = run_mean_rescaled0[filter_final_events1$end_s2]), color = "red", shape = 4)+
      theme_bw()+
      ggtitle(paste0(read_directions$condition[[folder]], "_", read_directions$folder[[folder]]))+
      ylab("Discplacement (nm)")+
      xlab("")



    #2

    p2 <- ggplot()+
      geom_line(aes(x = 20001:40000, y = rescaled_raw_data$trap[20001: 40000 ]))+
      geom_line(aes(x = 20001: 40000, y = run_mean_rescaled0[20001:40000]),color = run_mean_color)+
      geom_line(aes(x = 20001:40000, y = rep(8, 20000)), color = "gray50")+
      geom_point(aes(x = filter_final_events2$peak_nm_index, y = filter_final_events2$peak_nm), color = "gold")+
      geom_point(aes(x = filter_final_events2$end_s1, y = run_mean_rescaled0[filter_final_events2$end_s1]), color = "green", shape = 17, size = 2)+
      geom_point(aes(x = filter_final_events2$end_s2, y = run_mean_rescaled0[filter_final_events2$end_s2]), color = "red", shape = 4)+
      theme_bw()+
      ylab("Discplacement (nm)")+
      xlab("")

    #3

    p3 <- ggplot()+
      geom_line(aes(x = (max(final_events_4_plot$end_s2) - 40000):max(final_events_4_plot$end_s2 - 20000), y = rescaled_raw_data$trap[(max(final_events_4_plot$end_s2) - 40000): (max(final_events_4_plot$end_s2) - 20000)]))+
      geom_line(aes(x = (max(final_events_4_plot$end_s2) - 40000):max(final_events_4_plot$end_s2 - 20000), y = run_mean_rescaled0[(max(final_events_4_plot$end_s2) - 40000): (max(final_events_4_plot$end_s2)- 20000)]), color = run_mean_color)+
      geom_line(aes(x = (max(final_events_4_plot$end_s2) - 40000):max(final_events_4_plot$end_s2 - 20000), y = rep(8, 20001)), color = "grey50")+
      geom_point(aes(x = filter_final_events3$peak_nm_index, y = filter_final_events3$peak_nm), color = "gold")+
      geom_point(aes(x = filter_final_events3$end_s1, y = run_mean_rescaled0[filter_final_events3$end_s1]), color = "green", shape = 17, size = 2)+
      geom_point(aes(x = filter_final_events3$end_s2, y = run_mean_rescaled0[filter_final_events3$end_s2]), color = "red", shape = 4)+
      theme_bw()+
      ylab("Discplacement (nm)")+
      xlab("")



    #4
    p4 <- ggplot()+
      geom_line(aes(x = (max(final_events_4_plot$end_s2) - 20000):max(final_events_4_plot$end_s2), y = rescaled_raw_data$trap[(max(final_events_4_plot$end_s2) - 20000):max(final_events_4_plot$end_s2)]))+
      geom_line(aes(x = (max(final_events_4_plot$end_s2) - 20000):max(final_events_4_plot$end_s2), y = run_mean_rescaled0[(max(final_events_4_plot$end_s2) - 20000):max(final_events_4_plot$end_s2)]),color = run_mean_color)+
      geom_line(aes(x = (max(final_events_4_plot$end_s2) - 20000):max(final_events_4_plot$end_s2), y = rep(8, 20001)), color = "grey50")+
      geom_point(aes(x = filter_final_events4$peak_nm_index, y = filter_final_events4$peak_nm), color = "gold")+
      geom_point(aes(x = filter_final_events4$end_s1, y = run_mean_rescaled0[filter_final_events4$end_s1]), color = "green", shape = 17, size = 2)+
      geom_point(aes(x = filter_final_events4$end_s2, y = run_mean_rescaled0[filter_final_events4$end_s2]), color = "red", shape = 4)+
      theme_bw()+
      ylab("Discplacement (nm)")+
      xlab("Time (data points)")


    plots <-  arrangeGrob(p1, p2, p3, p4, ncol = 1)




  } else if(length(dat) < 100000){

    #1

    p1 <- ggplot()+
      geom_line(aes(x = 1:20000, y = rescaled_raw_data$trap[1: 20000 ]))+
      geom_line(aes(x = 1: 20000, y = run_mean_rescaled0[1:20000]),color = run_mean_color)+
      geom_line(aes(x = 1:20000, y = rep(8, 20000)), color = "gray50")+
      geom_point(aes(x = filter_final_events1$peak_nm_index, y = filter_final_events1$peak_nm), color = "gold")+
      geom_point(aes(x = filter_final_events1$end_s1, y = run_mean_rescaled0[filter_final_events1$end_s1]), color = "green", shape = 17, size = 2)+
      geom_point(aes(x = filter_final_events1$end_s2, y = run_mean_rescaled0[filter_final_events1$end_s2]), color = "red", shape = 4)+
      theme_bw()+
      ggtitle(paste0(read_directions$condition[[folder]], "_", read_directions$folder[[folder]]))+
      ylab("Discplacement (nm)")+
      xlab("Time (data points")

  }



  mini_analysis_list <- list(analysis = final_events,
                             dygraph = final_events_4_plot,
                             run_mean = run_mean_rescaled0,
                             plot = plots
  )

  return(mini_analysis_list)
}
