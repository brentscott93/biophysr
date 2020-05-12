#' Hidden Markov for Shiny
#'
#' @param trap_selected_date
#' @param mv2nm
#' @param nm2pn
#' @param overlay_color
#'
#' @return
#' @export
#'
#' @examples
shiny_hidden_markov_analysis <- function(trap_selected_date, trap_selected_conditions, mv2nm, nm2pn, overlay_color, file_type, hm_emcontrol){

  withProgress(message = 'HMM Analysis in Progress', value = 0, max = 1, min = 0, {
    incProgress(amount = .01, detail = "Reading Data")

     observation_folders <- list_dir(trap_selected_date) %>%
      dplyr::filter(str_detect(name, "obs")) %>%
      pull(name)

    grouped4r_files <- list_dir(trap_selected_date, recursive = TRUE) %>%
      dplyr::filter(str_detect(name, "grouped")) %>%
      pull(path)

    directions <- list_dir(trap_selected_date) %>%
      dplyr::filter(name == "directions.csv") %>%
      pull(path)


    if(file_type == "csv"){
    read_directions <- suppressMessages(read_csv(directions)) %>%
      mutate(folder = observation_folders,
             grouped_file = grouped4r_files,
             condition = trap_selected_conditions)%>%
      filter(include == "yes")

    } else {
      read_directions <- suppressMessages(read_csv(directions)) %>%
        mutate(folder = observation_folders,
               grouped_file = grouped4r_files,
               condition = trap_selected_conditions) %>%
        filter(include == "yes")


    }

    read_directions$baseline_start_sec <- as.numeric(read_directions$baseline_start_sec)
    read_directions$baseline_start_sec <- read_directions$baseline_start_sec*5000

    read_directions$baseline_stop_sec <- as.numeric(read_directions$baseline_stop_sec)
    read_directions$baseline_stop_sec <- read_directions$baseline_stop_sec*5000

    #create results folders for output on dropbox
    results_folder <- paste0(trap_selected_date, "/results")
    dir.create(results_folder)
    events_folder <- paste0(trap_selected_date, "/results/events")
    dir.create(events_folder)
    plots_folder <- paste0(trap_selected_date, "/results/plots")
    dir.create(plots_folder)
    model_folder <- paste0(trap_selected_date, "/results/model_summary")
    dir.create(model_folder)
    ensemble_folder <- paste0(trap_selected_date, "/results/ensemble")
    dir.create(ensemble_folder)


hmm_initial_parameters <- c(0.98, 0.02,        #Initial state probabilities
                            0.98, 0.02,         #transition probs s1 to s1/s2. These are guesses knowing they are stable states
                            0.02, 0.98)       #transition probs s2 to s1/s2. Again a guess
                            #transition probs s3 to s1/s2. Again a guess




## LOAD IN DATA ##
report <- vector("list")
var_signal_to_noise <- vector("list")
error_file <- file(paste0(trap_selected_date, "/error_log.txt"), open = "a")
var_signal_to_noise_directions <- vector("list")

#loop will start here
for(folder in seq_along(read_directions$folder)){
  tryCatch({
    inc_prog_bar <-  nrow(read_directions)*4
    setProgress(1/inc_prog_bar, paste("Analyzing", read_directions$condition[[folder]], read_directions$folder[[folder]]))



    report[[folder]] <- paste0("failed_to_initialize!_", read_directions$folder[[folder]])

    dat <-  if(file_type == "txt"){

    #Load data and convert mV to nm

      read_tsv(read_directions$grouped_file[[folder]], col_names = c("bead", "trap"))%>%
      mutate(nm_converted = bead*mv2nm) %>%
      dplyr::pull(nm_converted)

    } else if(file_type == "csv") {


    #Load data and convert mV to nm
    read_csv(read_directions$grouped_file[[folder]]) %>%
    mutate(nm_converted = bead*mv2nm) %>%
    dplyr::pull(nm_converted)

    }


   #PROCESS DATA
   #detrends data by either performing a piecewise linear detrend or simply removing baseline mean from all points (i.e. constant detrend)
   #both of these will center the mean around 0. It just depends if there needs to be long linear drift corrected or not

   processed_data <- if(read_directions$detrend[[folder]] == "yes"){

      break_pts <- seq(25000, length(dat), by = 25000)

      as.vector(pracma::detrend(dat, tt = "linear", bp = break_pts))

   } else if(read_directions$detrend[[folder]] == "no"){

      get_mean <- mean(dat[read_directions$baseline_start_sec[[folder]] : read_directions$baseline_stop_sec[[folder]]])
      dat - get_mean

   }

   ## RUNNING MEAN & VAR ##
   w_width <- 150
   incProgress(detail = "Calculating Running Mean")
   run_mean <- running(processed_data, fun = mean, width = w_width, by = w_width/2)


   incProgress(detail = "Calculating Running Variance")
   run_var <- running(processed_data, fun = var, width = w_width, by = w_width/2)


   running_table <- tibble(run_mean = run_mean,
                           run_var = run_var)

   ## HMM ##

   report[[folder]]  <- paste0("failed_HMM!_", read_directions$folder[[folder]])

   seed <- floor(runif(1, 0, 1e6))

   hmm <- depmixS4::depmix(list(run_var~1,
                                run_mean~1),
                 data = running_table,
                 nstates = 2,
                 family = list(stats::gaussian(),
                               stats::gaussian()))





   sd_run_mean <- sd(run_mean)

   mean_run_var <- mean(run_var)
   sd_run_var <- sd(run_var)

   if(hm_emcontrol == T){
      hmm_fit <- depmixS4::fit(hmm, emcontrol = em.control(random.start = TRUE))
   } else {
      estimate_hmm_gaussians <- c(mean_run_var, sd_run_var, 0, sd_run_mean,
                                  mean_run_var/2, sd_run_var, 2, sd_run_mean*2)
      hmm <- depmixS4::setpars(hmm, c(hmm_initial_parameters, estimate_hmm_gaussians))
      set.seed(seed)
      hmm_fit <- depmixS4::fit(hmm, emcontrol = em.control(random.start = FALSE))
   }

   hmm_posterior <- depmixS4::posterior(hmm_fit)

   #make sure HMM starts in state 2 this will reset seed and try to refit 10 times
   #should really never have to do this with the em.control set

   hmm_repeat <- 0

   while(hmm_repeat < 10){

      if(hmm_posterior$state[[1]] == 1){
         writeLines("HMM starts in state 1")
         hmm_repeat <- 11

      } else if(hmm_posterior$state[[1]] == 2){
         writeLines(paste("Refitting HMM", hmm_repeat))

         seed <- floor(runif(1, 0, 1e6))

         set.seed(seed)

         hmm_fit <- depmixS4::fit(hmm, emcontrol = em.control(random.start = hm_emcontrol))

         hmm_posterior <- depmixS4::posterior(hmm_fit)

         hmm_repeat <- hmm_repeat + 1
      }
   }

   report[[folder]] <- paste0("error-HMM_starts_in_state_2!_", read_directions$folder[[folder]])

   if(hmm_posterior$state[[1]] == 2){
      writeLines(c("Skipping",
                   read_directions$folder[[folder]],
                   "HMM starts in State 2."), error_file);
      next
   } else {

      incProgress(detail = "Continuing On!")

   }



    hmm_file <- file(paste0(trap_selected_date,
                           "/results/model_summary/",
                           read_directions$condition[[folder]],
                           "_",
                           read_directions$folder[[folder]],
                           "_",
                           "model.txt"),
                    open =  "a")

   capture.output(depmixS4::summary(hmm_fit), file = hmm_file)
   close(hmm_file)


   sum_fit <- depmixS4::summary(hmm_fit)
   base_var <- sum_fit[[1]]
   event_var <- sum_fit[[2]]

   var_signal_to_noise[[folder]] <- base_var/event_var
   var_signal_to_noise_directions[[folder]] <- paste0(read_directions$folder[[folder]], "!_", base_var/event_var)

   ## Calculate conversion between window length and data points



   ## COUNT EVENTS ##
   incProgress(detail = "Measuring Events")

   counter <- matrix(table(paste0(head(hmm_posterior$state,-1),tail(hmm_posterior$state,-1))), nrow = 1)


   dimnames(counter) <- list(c("#_transitions"),
                             c("1-1", "1-2", "2-1", "2-2"))

   count_events <- as.data.frame(counter, row.names = "#_transitions", make.names = TRUE) %>%
      mutate(condition = paste0( read_directions$condition[[folder]]))




   #save running mean, var, & state for dygraph
   hmm_identified_events <- tibble(run_mean = unname(run_mean),
                                   run_var = unname(run_var),
                                   state = hmm_posterior$state)


   report[[folder]] <- paste0("error_measureing_events!_", read_directions$folder[[folder]])

   ## MEASURE EVENTS ##
   ## Calculate conversion between window length and data points
   #setup
   conversion <- length(processed_data)/length(run_mean)

   #convert running mean object to tibble
   run_mean_tibble <- tibble::enframe(run_mean) %>%
      mutate(index = seq(1, length(run_mean), length.out = length(run_mean)))

   #finds lengths of events in number of running windows
   run_length_encoding <- rle(hmm_posterior$state)

   #converting to a tibble
   rle_object <- as_tibble(do.call("cbind", run_length_encoding))

   #make a copy of data for time on analysis
   rle_object_4_duration <- rle_object %>%
      dplyr::filter(values == 2)


   if(hmm_posterior$state[[length(hmm_posterior$state)]] == 2){
      #make a copy of data for time off analysis
      time_offs <- rle_object %>%
         dplyr::filter(values == 1) %>%
         tail(-1) %>% #this gets rid of the first state 1 when that begins with the start of the trace recording
         # head(-1) %>% #this gets rid of the last state 1 that only ends because we stopped collecting
         mutate(off_length_5kHz = lengths*conversion,
                time_off_ms = (off_length_5kHz/5000)*1000)
   } else {
      #make a copy of data for time off analysis
      time_offs <- rle_object %>%
         dplyr::filter(values == 1) %>%
         tail(-1) %>% #this gets rid of the first state 1 when that begins with the start of the trace recording
         head(-1) %>% #this gets rid of the last state 1 that only ends because we stopped collecting
         mutate(off_length_5kHz = lengths*conversion,
                time_off_ms = (off_length_5kHz/5000)*1000)
   }


   #calculates the events durations
   on_off_times <- rle_object_4_duration %>%
      dplyr::mutate(n_event = 1:nrow(rle_object_4_duration),
                    length_5kHz = lengths*conversion,
                    time_on_ms = (length_5kHz/5000)*1000,
                    time_off_ms = c(NA, time_offs$time_off_ms)) %>%
      dplyr::select(n_event,values, everything()) %>%
      dplyr::rename("num_windows" = lengths,
                    "hmm_state" = values)


   #calculate event displacement

   #If the rle_object's last row is in state 1, get rid of that last row. This needs to end in state 2 to capture the end of the last event
   rle_object_4_step_sizes <- if(tail(rle_object, 1)$values == 1){
      slice(rle_object, -length(rle_object$values))
   } else {
      rle_object
   }
   #Calculate the cumulative sum of the run length encoder
   #And splits the tibble into two seperate tables to isolate state 1 info from state 2

   split_data <- rle_object_4_step_sizes %>%
      dplyr::mutate(cumsum = cumsum(lengths)) %>%
      dplyr::group_by(values) %>%
      split(rle_object_4_step_sizes$values)

   #data is recmombined in a state_1 column and a state_2
   #the values in these columns represent the last data point (in window lengths) in either state 1 or state 2
   #So the range of values between the end of state 1 (or start of state 2) and the end of state 2 is the event duration
   regroup_data <- bind_cols(state_1_end = split_data[[1]]$cumsum, state_2_end = split_data[[2]]$cumsum)



   #loop over regrouped data to find the mean of the events displacements
   step_sizes <- vector("list", length = nrow(regroup_data)) #allocate space for output storage of loop
   peak_nm_index <- vector()
   for(i in 1:nrow(regroup_data)){

      win_values_t <- run_mean_tibble[(regroup_data$state_1_end[i]+1) : (regroup_data$state_2_end[i]),]
      max_step_index <- win_values_t$index[which.max(abs(win_values_t$value))]
      peak_nm_index[i] <- max_step_index
      step_sizes[[i]] <-   win_values_t$value[which(win_values_t$index == max_step_index)]


   }

   #do opposite to get means of state 1 to subtract from s2 means.
   # need to subtract first s1 value and last s2 value of the cumsum to align properly
   #don't need the end point of the first s1 because we don't know when the last event ended because we only have knowledge
   #of events we observe when data collection starts

   minus1 <- split_data[[1]]$cumsum[-1]
   minus2 <- split_data[[2]]$cumsum[-length(split_data[[2]]$cumsum)]


   s1_regroup_data <- bind_cols(state_2_end = minus2, state_1_end = minus1)

   #loop over s1_regrouped data to find the mean of state 1
   state_1_avg <- vector("list", length = nrow(regroup_data)) #allocate space for output storage of loop
   state_1_avg[[1]] <- mean(run_mean_tibble$value[1:regroup_data$state_1_end[1]]) #get everage of first state 1
   if(nrow(s1_regroup_data) > 1){
      for(i in seq_along(1:nrow(s1_regroup_data))){
         state_1_avg[[i+1]] <- mean(run_mean_tibble$value[(s1_regroup_data$state_2_end[i]+1) : (s1_regroup_data$state_1_end[i])])
      }
   }

   calculate_mean_differences <- tibble(avg_s1 = unlist(state_1_avg),
                                        avg_s2 = unlist(step_sizes),
                                        diff = avg_s2 - avg_s1)
   ## DIRECTION CORRECTION ##

   positive_events <- sum(calculate_mean_differences$diff > 0)
   negative_events <- sum(calculate_mean_differences$diff < 0)

   #if there are more negative step sizes than positive, actin filament assumed backward and all events flipped (multipled by -1)
   #also raw trace, state 1 averages, and step sizes flipped for hmm overlay
   direction_correction <- if(negative_events > positive_events){
      calculate_mean_differences$diff * -1
   } else {
      calculate_mean_differences$diff
   }


   flip_raw <- if(negative_events > positive_events){
      processed_data * -1
   } else {
      processed_data
   }

   flip_state_1_avg <- if(negative_events > positive_events){
      unlist(state_1_avg) * -1
   } else {
      unlist(state_1_avg)
   }

   flip_step_sizes <- if(negative_events > positive_events){
      unlist(step_sizes) * -1
   } else {
      unlist(step_sizes)
   }


   #add step sizes and forces to the on_off_times table
   measured_events <- on_off_times %>%
      dplyr::mutate(displacement_nm = direction_correction,
                    condition = paste0(read_directions$condition[[folder]]),
                    force = displacement_nm*nm2pn)



   #find better time on

   forward_data <- tibble(s1_end = floor((regroup_data$state_1_end - 1.75)*conversion),
                              s2_end = ceiling(regroup_data$state_2_end*conversion))

   backwards_data <- tibble(s1_end = floor(regroup_data$state_1_end*conversion),
                          s2_end = ceiling((regroup_data$state_2_end + 1.75)*conversion))

  # flip_raw_run_mean <- rollmean(as.vector(flip_raw), k = 50, align = "left")
   processed_data_tibble <- tibble(data = as.vector(flip_raw),
                                   index = seq(1, length(flip_raw), length.out = length(flip_raw)))

   did_it_flip <- negative_events > positive_events
   is_positive <- calculate_mean_differences$diff > 0
   if(did_it_flip == TRUE){
      is_positive <- ifelse(is_positive == TRUE, FALSE, TRUE)
   }

   ensemble_length <- 300 #data points, 60ms, 0.06 seconds
   better_time_on_starts <- vector()
   forward_ensemble_average_data <- vector("list")
   ensemble_keep1 <- vector()
   for(c in 1:nrow(forward_data)){
      print(c)
     forward_chunk <- processed_data_tibble[forward_data$s1_end[[c]] : forward_data$s2_end[[c]],]

     has_na <- table(is.na(forward_chunk$data))

     if(length(has_na) > 1){
       better_time_on_starts[[c]] <- NA
       ensemble_keep1[[c]] <- FALSE

       next

     }

      if(nrow(forward_chunk) > 3000){
         forward_chunk %<>%
            slice(1:3000)
      }

      run_var_chunk <- running(forward_chunk$data, fun = var, width = 50, align = "left")
      chunk_length <- length(run_var_chunk)*0.7
      run_var_chunk <- run_var_chunk[1:(chunk_length)]


      forward_cpt_obj <- cpt.mean(run_var_chunk, method = "AMOC")
      forward_chunk %<>%
         #mutate(data = smth.gaussian(forward_chunk$data, window = 50)) %>%
        slice(1:chunk_length)

      event_on <- cpts(forward_cpt_obj)

     if(identical(cpts(forward_cpt_obj), numeric(0)) == TRUE){

       forward_chunk <- processed_data_tibble[(forward_data$s1_end[[c]] - (1.5*w_width)): forward_data$s2_end[[c]],]

       if(nrow(forward_chunk) > 3000){
          forward_chunk %<>%
             slice(1:3000)
       }

       run_var_chunk <- running(forward_chunk$data, fun = var, width = 50, align = "left")
       chunk_length <- length(run_var_chunk)*0.7
       run_var_chunk <- run_var_chunk[1:(chunk_length)]

       forward_cpt_obj <- cpt.mean(run_var_chunk, method = "AMOC")

       forward_chunk %<>%
          #mutate(data = smth.gaussian(forward_chunk$data, window = 50)) %>%
         slice(1:chunk_length)

       event_on <- cpts(forward_cpt_obj)

     }

     if(identical(cpts(forward_cpt_obj), numeric(0)) == TRUE){

        better_time_on_starts[[c]] <- NA
        ensemble_keep1[[c]] <- FALSE

        next

     } else {

       better_time_on_starts[[c]] <- forward_chunk$index[event_on]
       ensemble_keep1[[c]] <- TRUE

     }

       start_ensemble <- 1 - event_on

       forward_chunk %<>%
         mutate(forward_index = seq(start_ensemble, by = 1, length.out = nrow(forward_chunk)),
                event = c)


      filter_forward_chunk <- forward_chunk %>%
                                dplyr::filter(forward_index >= 0) %>%
                                dplyr::select(-index)

       if(nrow(filter_forward_chunk) < ensemble_length){
         mean_data <- ceiling(length(filter_forward_chunk$data)*.95)
         diff_length <- length(filter_forward_chunk$data) -  mean_data
         mean_chunk <- mean(filter_forward_chunk$data[diff_length : length(filter_forward_chunk$data) ])

         rep_num <- 300 - nrow(filter_forward_chunk)
         forward_extension <- rep(mean_chunk, rep_num)
         forward_60ms <- tibble(data = c(filter_forward_chunk$data, forward_extension),
                                    forward_index = 0:299,
                                    event = c)

       } else {

         forward_60ms <- filter_forward_chunk %>% slice(1:300)
      }

      before_event <-  forward_chunk %>%
        dplyr::filter(forward_index < 0) %>%
        dplyr::select(-index)

      forward_60ms %<>% rbind(before_event) %>%
         mutate(is_positive = is_positive[[c]],
                conditions = trap_selected_conditions,
                date = trap_selected_date) %>%
         arrange(forward_index)

      forward_ensemble_average_data[[c]] <- forward_60ms
     }


   ####backwards ensemble
   better_time_on_stops <- vector()
   backwards_ensemble_average_data <- vector("list")
   ensemble_keep2 <- vector()
   for(c in 1:nrow(backwards_data)){
    try({
    print(c)
     backwards_chunk <- processed_data_tibble[backwards_data$s1_end[[c]] : backwards_data$s2_end[[c]],]

     has_na <- table(is.na(backwards_chunk$data))

    if(length(has_na) > 0){
      better_time_on_stops[[c]] <- NA
      ensemble_keep2[[c]] <- FALSE

      next

    }



     if(nrow(backwards_chunk) > 3000){
        lb <- nrow(backwards_chunk)
        backwards_chunk %<>%
           slice((lb-3000):lb)
     }

     bward_run_var_chunk <- running(backwards_chunk$data, fun = var, width = 50, align = "right", allow.fewer = F, pad = T)
     bward_chunk_length <- length(bward_run_var_chunk)
     bward_run_var_chunk <- bward_run_var_chunk[(bward_chunk_length*0.3):bward_chunk_length]


     backwards_cpt_obj <- cpt.mean(bward_run_var_chunk, method = "AMOC")
     backwards_chunk %<>%
        #mutate(data = smth.gaussian(backwards_chunk$data, window = 50)) %>%
       slice((bward_chunk_length * 0.3):bward_chunk_length)
     event_off <- cpts(backwards_cpt_obj)


     if(identical(cpts(backwards_cpt_obj), numeric(0)) == TRUE){
      #  if(is_positive[[c]] == TRUE){

         backwards_chunk <- processed_data_tibble[backwards_data$s1_end[[c]]: (backwards_data$s2_end[[c]] + (1.5*w_width)),]


         if(nrow(backwards_chunk) > 3000){
            lb <- nrow(backwards_chunk)
            backwards_chunk %<>%
               slice((lb-3000):lb)
         }

         bward_run_var_chunk <- running(backwards_chunk$data, fun = var, width = 50, align = "right", allow.fewer = F, pad = T)
         bward_chunk_length <- length(bward_run_var_chunk)
         bward_run_var_chunk <- bward_run_var_chunk[(bward_chunk_length*0.3):bward_chunk_length]


         backwards_cpt_obj <- cpt.mean(bward_run_var_chunk, method = "AMOC")
         backwards_chunk %<>%
            #mutate(data = smth.gaussian(backwards_chunk$data, window = 50)) %>%
            slice((bward_chunk_length * 0.3):bward_chunk_length)
         event_off <- cpts(backwards_cpt_obj)

     }

     if(identical(cpts(backwards_cpt_obj), numeric(0)) == TRUE){
        better_time_on_stops[[c]] <- NA
        ensemble_keep2[[c]] <- FALSE

      next

     } else {

     better_time_on_stops[[c]] <- backwards_chunk$index[event_off]
     ensemble_keep2[[c]] <- TRUE

     }

     start_backward_ensemble <- 1 - event_off
     backwards_chunk %<>%
       mutate(backward_index = seq(start_backward_ensemble, by = 1, length.out = nrow(backwards_chunk)),
              event = c)


     filter_backwards_chunk <- backwards_chunk %>%
       dplyr::filter(backward_index <= 0) %>%
       dplyr::select(-index)

     if(nrow(filter_backwards_chunk) < ensemble_length){

       mean_back_chunk <- mean(filter_backwards_chunk$data[1:(nrow( filter_backwards_chunk)*0.95)])

       rep_num2 <- 300 - nrow(filter_backwards_chunk)
       backward_extension <- rep(mean_back_chunk, rep_num2)
       backward_60ms <- tibble(data = c(backward_extension, filter_backwards_chunk$data),
                               backward_index = -299:0,
                               event = c)

     } else {

       backward_60ms <- filter_backwards_chunk %>%
         dplyr::filter(backward_index <= 0 & backward_index >= -299)
     }

     after_event <-  backwards_chunk %>%
       dplyr::filter(backward_index > 0) %>%
       dplyr::select(-index)

     backward_60ms %<>% rbind(after_event) %>%
        mutate(is_positive = is_positive[[c]],
               conditions = trap_selected_conditions,
               date = trap_selected_date)

     backwards_ensemble_average_data[[c]] <- backward_60ms
   })
   }



   better_time_on <- tibble(start = better_time_on_starts,
                            stop = better_time_on_stops,
                            keep1 = ensemble_keep1,
                            keep2 = ensemble_keep2,
                            n_event = 1:length(start),
                            is_positive = is_positive) %>%
     mutate(better_time_on_dp = stop - start,
            better_time_on_ms = (better_time_on_dp/5000)*1000)



   dygraph_periods <- cbind(better_time_on, regroup_data) %>%
      mutate(state_2_start = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
                      state_1_end*conversion,
                      start),
             state_2_stop = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE,
                            state_2_end*conversion,
                            stop)) %>%
      rename("run_from" = state_1_end,
             "run_to" = state_2_end) %>%
      dplyr::select(state_2_start, state_2_stop, run_from, run_to)




   ###add better on times to final table
   measured_events %<>% full_join(better_time_on) %>%
      mutate(final_time_ons_ms = ifelse(is.na(start) == TRUE | is.na(stop) == TRUE | better_time_on_dp <= 0,
                                        time_on_ms,
                                        better_time_on_ms)) %>%
      dplyr::select(condition, time_off_ms, final_time_ons_ms,  displacement_nm, force) %>%
      rename("time_on_ms" = final_time_ons_ms)

   #######better step sizes###########
   #loop over regrouped data to find the mean of the events displacements
  # better_step <- vector() #allocate space for output storage of loop
   #for(i in 1:nrow(measured_events)){
    #  if(is.na(measured_events$start[[i]]) == T | is.na(measured_events$stop[[i]]) == T){
     #    better_step[[i]] <- measured_events$displacement_nm[[i]]
#      } else {
 #        e_start <- measured_events$start[[i]]
  #       e_stop <- measured_events$stop[[i]]
   #      l_event <- length(e_start:e_stop)*0.05/2 #get length of event in data points, get 5% of that, and divide by 2 to subtract/add
    #     e_start %<>% sum(l_event)
     #    e_stop <- e_stop - l_event
      #   event_chunk <- processed_data_tibble %>% slice(e_start:e_stop)
       #  better_step[[i]] <- mean(event_chunk$data)
#      }

#   }


   # final selectionsdplyr::select(condition, time_off_ms, final_time_ons_ms,  displacement_nm, force) %>%

   ## SAVE OUTPUT ##
   incProgress(detail = "Saving Events")
   measured_events_path <-  paste0(trap_selected_date,
                                   "/results/events/",
                                   read_directions$condition[[folder]],
                                   "_",
                                   read_directions$folder[[folder]],
                                   "_hmm_events.csv")

   write_csv(measured_events, measured_events_path)

   #save ensemble data
   forward_ensemble_df <- bind_rows(forward_ensemble_average_data)

   forward_ensemble_path <-  paste0(trap_selected_date,
                                   "/results/ensemble/",
                                   read_directions$condition[[folder]],
                                   "_",
                                   read_directions$folder[[folder]],
                                   "_forward_ensemble_data.csv")

   write_csv(forward_ensemble_df, forward_ensemble_path)

   #backwards

   backwards_ensemble_df <- bind_rows(backwards_ensemble_average_data)

   backwards_ensemble_path <-  paste0(trap_selected_date,
                                    "/results/ensemble/",
                                    read_directions$condition[[folder]],
                                    "_",
                                    read_directions$folder[[folder]],
                                    "_backwards_ensemble_data.csv")

   write_csv(backwards_ensemble_df, backwards_ensemble_path)

   #makae hmm overlay for dygraph
   #dp2plot <- 10*5000

   s1_avg_4plot <- tibble(state_order = seq(from = 1, length.out = length(state_1_avg), by = 2),
                          avg = flip_state_1_avg)



   s2_avg_4plot <- tibble(state_order = seq(from = 2, length.out = length(step_sizes), by = 2),
                          avg = flip_step_sizes)


   hmm_overlay <- bind_rows(s1_avg_4plot, s2_avg_4plot) %>%
      arrange(state_order)




   if(hmm_posterior$state[[length(hmm_posterior$state)]] == 2){

      overlay <- vector("list")
      for(i in seq_along(1:(nrow(hmm_overlay)-1))){

         overlay[[i]] <- rep(hmm_overlay$avg[i],
                             (round(conversion)*rle_object$lengths[-length(rle_object$lengths)][i]))
      }
   } else {

      overlay <- vector("list")
      for(i in seq_along(1:nrow(hmm_overlay))){
         overlay[[i]] <- rep(hmm_overlay$avg[i],
                             (round(conversion)*rle_object$lengths[-length(rle_object$lengths)][i]))
      }
   }

   overlay <- unlist(overlay)


   #save data for dygraph


   temp_dir <-  chartr("\\", "/", paste0(tempdir(), "/", squysh_time(),"_", read_directions$condition[[folder]], "_", read_directions$folder[[folder]]))

   dir.create(temp_dir)


   rdata_temp_file <- tempfile(pattern = "rdata", tmpdir = temp_dir)
   rdata_temp_file <-  chartr("\\", "/", rdata_temp_file)

   dygraph_master_list <- list(raw_data = flip_raw,
                               run_mean = overlay,
                               final_events = measured_events,
                               count_events = count_events,
                               periods = dygraph_periods,
                               peak_nm_index = peak_nm_index * conversion,
                               rdata_temp_file = rdata_temp_file,
                               hmm_identified_events = hmm_identified_events,
                               var_signal_to_noise = var_signal_to_noise[[folder]])



   save("dygraph_master_list", file =  rdata_temp_file)


   #make dygraph .R file
   writeLines(c(
      "#+ echo=FALSE",
      "suppressPackageStartupMessages(library(tidyverse))
   suppressPackageStartupMessages(library(dygraphs))
   suppressPackageStartupMessages(library(rmarkdown))
   suppressPackageStartupMessages(library(RColorBrewer))
   suppressPackageStartupMessages(library(gridExtra))
  ",
      paste0("rdata_temp_file <- ","'", rdata_temp_file, "'"),
      paste0("run_mean_color <- ", "'",overlay_color, "'"),
      "
   #+ echo=FALSE, message = FALSE, fig.width = 10, fig.height = 2

  load(rdata_temp_file)

  tibble('Number of Events' = dygraph_master_list$count_events$'1-2',
          'Signal (V1/V2)' = dygraph_master_list$var_signal_to_noise)


#' This is an interactive plot of the running variance and running mean.
#' The algorithm for event detection is a Hidden Markov Model and these are the data the model receives as input. The model is fitted with the EM algorithm and the
#' gray shaded regions are the binding events identified through state sequence decoding via the Viterbi alogorithm.
#'
#+ echo=FALSE, message = FALSE, fig.width = 10, fig.height = 2



  dy_rv <- tibble(window = 1:nrow(dygraph_master_list$hmm_identified_events),
                  rv = dygraph_master_list$hmm_identified_events$run_var)

  dy_rm <- tibble(Window = 1:nrow(dygraph_master_list$hmm_identified_events),
                  rm = dygraph_master_list$hmm_identified_events$run_mean)

  shades_df <- data.frame(start = dygraph_master_list$periods$run_from,
                           stop = dygraph_master_list$periods$run_to)


  add_shades <- function(x, periods, ...){
    for(p in 1:nrow(periods)){
      x <- dyShading(x, from = periods$start[[p]], to = periods$stop[[p]], ...)
    }
    x
  }

c <- brewer.pal(8, 'Dark2')
shade_col <- '#E2E2E2'

dygraph(dy_rv, group = 'group') %>%
  dySeries('rv', color = c[[1]], strokeWidth = 2) %>%
  dyAxis('x', axisLineColor = '#FFFFFF', drawGrid = FALSE, axisLabelColor = '#FFFFFF') %>%
  dyAxis('y', label = 'Running Variance', drawGrid = FALSE,) %>%
  add_shades(periods = shades_df, color = shade_col) %>%
  dyUnzoom()
dygraph(dy_rm, group = 'group') %>%
  dySeries('rm', color = c[[2]],  strokeWidth = 2) %>%
  dyAxis('x', label = 'Window', drawGrid = FALSE) %>%
  dyAxis('y', label = 'Running Mean (nm)', drawGrid = FALSE) %>%
  add_shades(periods = shades_df) %>%
  add_shades(periods = shades_df, color = shade_col) %>%
  dyRangeSelector(fillColor =c[[3]], strokeColor = c[[8]])
#'
#'
#'
#' Below is an interactive plot of the raw data (model does not use this data).
#' The overlay is the Hidden Markov Model 'state' prediction multipled by several conversion factors.
#' These convert the x-axis (time) from 'windows' to 'data points' to 'seconds'.
#' The HMM state, baseline mean, and measured step size are used to scale the model overlay to each step and subsequent baseline level.

#+ echo=FALSE, message=FALSE, fig.width = 10, fig.height = 4


  d <- data.frame(index = (1:length(dygraph_master_list$run_mean)/5000),
                  raw = dygraph_master_list$raw_data[1:length(dygraph_master_list$run_mean)],
                  model = dygraph_master_list$run_mean
                  )

  events <- dygraph_master_list$final_events
  peak_nm_index = dygraph_master_list$peak_nm_index/5000

  periods_df <- data.frame(start = dygraph_master_list$periods$state_2_start/5000,
                           stop = dygraph_master_list$periods$state_2_stop/5000)

   add_labels <- function(x, events, ...){
    for(event in 1:length(peak_nm_index)){
      x <- dyEvent(x, peak_nm_index[[event]], paste(round(events$time_on_ms[[event]], digits = 0), 'ms,', round(events$displacement_nm[[event]], digits = 1), 'nm'), ...)
    }
    x
  }

  dygraph(d) %>%
    dySeries('raw', color = '#242424', strokeWidth = 2) %>%
    dySeries('model', color = run_mean_color,  strokeWidth = 2) %>%
    dyRangeSelector() %>%
    add_shades(periods_df, color = 'lightpink') %>%
    add_labels(events, labelLoc = 'bottom') %>%
    dyAxis('x', label = 'seconds', drawGrid = FALSE) %>%
    dyAxis('y', label = 'nm') %>%
    dyUnzoom()


#'
#'
#' Plots of the running mean vs. running variance.
#' This provides insight into how the model divided data into either the baseline or event populations.
#+ echo=FALSE, message = FALSE, fig.width = 16, fig.height = 6

mean_var_tib <- tibble(rm = dygraph_master_list$hmm_identified_events$run_mean,
                       rv = dygraph_master_list$hmm_identified_events$run_var,
                       state = paste('State',dygraph_master_list$hmm_identified_events$state))



mv1 <- ggplot(mean_var_tib)+
  geom_jitter(aes(x = rm, y = rv, color = state), size = 3, alpha = 0.5)+
  scale_color_manual(values = c)+
  ggtitle('Mean-Variance (overlayed)')+
  ylab('Running Variance (nm)')+
  xlab('Running Mean (nm)')+
  theme_linedraw(base_size = 18)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

mv2 <- ggplot(mean_var_tib)+
  geom_jitter(aes(x = rm, y = rv, color = state), size = 3, alpha = 0.5)+
  scale_color_manual(values = c)+
  facet_wrap(~state)+
  ggtitle('Mean-Variance (separate by state)')+
  ylab('')+
  xlab('Running Mean (nm)')+
  theme_linedraw(base_size = 18)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

grid.arrange(mv1, mv2, nrow = 1)







  "),
      paste0(temp_dir,
             "/",
             read_directions$condition[[folder]],
             "_",
             read_directions$folder[[folder]],
             "_dygraph.R")

   )





   incProgress(detail = "Render to HTML")

   #render to HTML
   report[[folder]] <- paste0("failed_to_render_dygraph!_", read_directions$folder[[folder]])


   #rendered <-  tempfile(pattern = "rendered", fileext = ".html", tmpdir = temp_dir)

   #rendered <- chartr("\\", "/", rendered)


   rmarkdown::render(input = paste0(temp_dir,
                                    "/",
                                    read_directions$condition[[folder]],
                                    "_",
                                    read_directions$folder[[folder]],
                                    "_dygraph.R"),
                     output_file = paste0(trap_selected_date,
                                          "/results/plots/",
                                          read_directions$condition[[folder]],
                                          "_",
                                          read_directions$folder[[folder]],
                                          "_plots.html"),
                     envir = new.env())



   report[[folder]] <- paste0("success!_", read_directions$folder[[folder]])






  }, error=function(e){
    writeLines(paste0("Analysis error in ",
                      read_directions$folder[[folder]],
                      "with error: ",
                      as.character(e)), error_file)
    cat("ERROR :",conditionMessage(e), "\n")
  })

}

close(error_file)

signal <- tibble(sig = unlist(var_signal_to_noise_directions)) %>%
   separate(sig, c("folder", "signal"), sep = "!_")

export_directions <- read_csv(directions) %>%
  dplyr::select(starts_with("obs"), baseline_start_sec, baseline_stop_sec, detrend, include) %>%
  mutate(folder = observation_folders,
         grouped_file = grouped4r_files) %>%
  left_join(signal)



success_report <- tibble(analysis_complete = unlist(report)) %>%
  separate(analysis_complete, c("report", "folder"), sep = "!_") %>%
  right_join(export_directions, by = "folder") %>%
  replace_na(list(report = "user_excluded")) %>%
  arrange(folder) %>%
  dplyr::select(-starts_with("grouped_file"))

write_csv(success_report, paste0(trap_selected_date, "/directions.csv"), append = FALSE)


incProgress(1, detail = "Done!")

  }) #close withProgress


}



