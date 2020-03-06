#' Hidden Markov for Shiny (dropbox)
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
shiny_hidden_markov_analysis <- function(trap_selected_date, mv2nm, nm2pn, overlay_color, file_type){

  withProgress(message = 'HMM Analysis in Progress', value = 0, max = 1, min = 0, {
    incProgress(amount = .01, detail = "Reading Data")



    observation_folders <- drop_dir(trap_selected_date) %>%
      dplyr::filter(str_detect(name, "obs")) %>%
      pull(name)

    grouped4r_files <- drop_dir(trap_selected_date, recursive = TRUE) %>%
      dplyr::filter(str_detect(name, "grouped")) %>%
      pull(path_display)

    directions <- drop_dir(trap_selected_date) %>%
      dplyr::filter(name == "directions.csv") %>%
      pull(path_display)

    read_directions <- suppressMessages(drop_read_csv(directions)) %>%
      mutate(folder = observation_folders,
             grouped_file = grouped4r_files) %>%
      filter(include == "yes")


    read_directions$baseline_start_sec <- if(read_directions$baseline_start_sec == 0){
      1/5000
    } else {
      read_directions$baseline_start_sec*5000
    }

    read_directions$baseline_stop_sec <- read_directions$baseline_stop_sec*5000

    #create results folders for output on dropbox
    results_folder <- paste0(trap_selected_date, "/results")
    drop_create(results_folder)
    events_folder <- paste0(trap_selected_date, "/results/events")
    drop_create(events_folder)
    plots_folder <- paste0(trap_selected_date, "/results/plots")
    drop_create(plots_folder)
    model_folder <- paste0(trap_selected_date, "/results/model")
    drop_create(model_folder)

hmm_initial_parameters <- c(0.98, 0.01, 0.01,        #Initial state probabilities
                            0.98, 0.01, 0.01,        #transition probs s1 to s1/s2. These are guesses knowing they are stable states
                            0.01, 0.98, 0.01,        #transition probs s2 to s1/s2. Again a guess
                            0.01, 0.01, 0.98)       #transition probs s3 to s1/s2. Again a guess




## LOAD IN DATA ##
report <- vector("list")
var_signal_to_noise <- vector("list")
error_file_name <- paste0(tempdir(), "/", "error_log_", squysh_time())
dir.create(error_file_name)
error_file <- file(paste0(error_file_name, "/log.txt"), open = "a")
var_signal_to_noise_directions <- vector("list")

#loop will start here
for(folder in seq_along(read_directions$folder)){
  tryCatch({
    inc_prog_bar <-  nrow(read_directions)*4
    incProgress(1/inc_prog_bar, paste("Analyzing", read_directions$condition[[folder]], read_directions$folder[[folder]]))



    report[[folder]] <- paste0("failed_to_initialize!_", read_directions$folder[[folder]])

   dat <-  if(file_type == "txt"){

    #Load data and convert mV to nm
    dat_dir <- paste0(tempdir(), "/grouped", squysh_time(), round(runif(1, 0, 100000)))
    dir.create(dat_dir)

    drop_download(read_directions$grouped_file[[folder]],
                         local_path = dat_dir)

    read_tsv(paste0(dat_dir, "/grouped4r.txt"), col_names = c("bead", "trap"))%>%
      mutate(nm_converted = bead*mv2nm) %>%
      dplyr::pull(nm_converted)

    } else if(file_type == "csv") {


    #Load data and convert mV to nm
    drop_read_csv(read_directions$grouped_file[[folder]]) %>%
    mutate(nm_converted = bead*mv2nm) %>%
    dplyr::pull(nm_converted)

    }


    #PROCESS DATA
    #detrends data by either performing a piecewise linear detrend or simply removing baseline mean from all points (i.e. constant detrend)
    #both of these will center the mean around 0. It just depends if there needs to be long linear drift corrected or not

    processed_data <- if(read_directions$detrend[[folder]] == "yes"){

      break_pts <- seq(25000, length(dat), by = 25000)

      pracma::detrend(dat, tt = "linear", bp = break_pts)

    } else if(read_directions$detrend[[folder]] == "no"){

      get_mean <- mean(dat[read_directions$baseline_start_sec[[folder]] : read_directions$baseline_stop_sec[[folder]]])
      dat - get_mean

    }

    ## RUNNING MEAN & VAR ##
    run_mean <- running(processed_data, fun = mean, width = 150, by = 75)
    run_var <- running(processed_data, fun = var, width = 150, by = 75)
    running_table <- tibble(run_mean = run_mean,
                            run_var = run_var)

    ## HMM ##

    report[[folder]]  <- paste0("failed_HMM!_", read_directions$folder[[folder]])

    seed <- floor(runif(1, 0, 1e6))

    hmm <- depmix(list(run_var~1,
                       run_mean~1),
                  data = running_table,
                  nstates = 3,
                  family = list(gaussian(),
                                gaussian()))





    sd_run_mean <- sd(run_mean)

    mean_run_var <- mean(run_var)
    sd_run_var <- sd(run_var)


    estimate_hmm_gaussians <- c(mean_run_var, sd_run_var, 0, sd_run_mean,         #s1
                                mean_run_var/2, sd_run_var/2, 3, sd_run_mean,     #s2
                                mean_run_var/2, sd_run_var/2, -3, sd_run_mean)    #s3


    hmm <- setpars(hmm, c(hmm_initial_parameters, estimate_hmm_gaussians))

    set.seed(seed)

    hmm_fit <- fit(hmm, emcontrol = em.control(random.start = FALSE))

    hmm_posterior <- posterior(hmm_fit)

    #make sure HMM starts in state 2 this will reset seed and try to refit 10 times
    #should really never have to do this with the em.control set

    hmm_repeat <- 0

    while(hmm_repeat < 10){

      if(hmm_posterior$state[[1]] == 1){
        writeLines("HMM starts in state 1")
        hmm_repeat <- 11

      } else if(hmm_posterior$state[[1]] == 2 | 3){
        writeLines(paste("Refitting HMM", hmm_repeat))

        seed <- floor(runif(1, 0, 1e6))

        set.seed(seed)

        hmm_fit <- fit(hmm, emcontrol = em.control(random.start = FALSE))

        hmm_posterior <- posterior(hmm_fit)

        hmm_repeat <- hmm_repeat + 1
      }
    }

    report[[folder]] <- paste0("error-HMM_starts_in_state_2!_", read_directions$folder[[folder]])

    if(hmm_posterior$state[[1]] == 2 | 3){
      writeLines(c("Skipping",
                   read_directions$folder[[folder]],
                   "HMM starts in State 2."), error_file);
      next
    }



      hmm_file_path <- paste0(tempdir(),"/model_summary", squysh_time(),  round(runif(1, 0, 100000)))
      dir.create(hmm_file_path)

     hmm_file <- file(paste0(hmm_file_path, "/",
                        read_directions$condition[[folder]],
                        "_",
                        read_directions$folder[[folder]],
                        "_",
                        "model.txt"),
                        open =  "a")


    capture.output(summary(hmm_fit), file = hmm_file)
    close(hmm_file)

    drop_upload(file = paste0(hmm_file_path, "/",
                       read_directions$condition[[folder]],
                       "_",
                       read_directions$folder[[folder]],
                       "_",
                       "model.txt"),
                path = format_dropbox_path(model_folder))

    sum_fit <- summary(hmm_fit)
    base_var <- sum_fit[[1]]
    event_var <- sum_fit[[2]]

    var_signal_to_noise[[folder]] <-base_var/event_var
    var_signal_to_noise_directions[[folder]] <- paste0(read_directions$folder[[folder]], "!_", base_var/event_var)

    ## COUNT EVENTS ##
    #turn all state 3s into 2s

    hmm_posterior$state <- ifelse(hmm_posterior$state == 3, 2, hmm_posterior$state)

    counter <- matrix(table(paste0(head(hmm_posterior$state,-1),tail(hmm_posterior$state,-1))), nrow = 1)
    dimnames(counter) <- list(c("#_transitions"),
                              c("1-1", "1-2", "2-1", "2-2"))
    count_events <- as.data.frame(counter, row.names = "#_transitions", make.names = TRUE) %>%
      mutate(condition = paste0( read_directions$condition[[folder]]))

    #save running mean, var, & state for ensemble averaging & dygraph
    hmm_identified_events <- tibble(run_mean = run_mean,
                                    run_var = run_var,
                                    state = hmm_posterior$state)


    report[[folder]] <- paste0("error_measureing_events!_", read_directions$folder[[folder]])

    ## MEASURE EVENTS ##
    ## Calculate conversion between window length and data points
    #setup
    conversion <- length(processed_data)/length(run_mean)

    #convert running mean object to tibble
    run_mean_tibble <- tibble::enframe(run_mean) %>%
      mutate(index = time(run_mean))

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
      step_sizes[[i]] <-  win_values_t$value[which(win_values_t$index == max_step_index)]


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
                    force = displacement_nm*nm2pn) %>%
      dplyr::select(condition, time_on_ms, time_off_ms, displacement_nm, force)


    ## SAVE OUTPUT ##
   # writeLines("Saving Events")


    temp_events <- write_temp_csv(measured_events, filename = paste0(read_directions$condition[[folder]],
                                                      "_",
                                                      read_directions$folder[[folder]],
                                                      "_hmm_events.csv"))

    drop_upload(temp_events, format_dropbox_path(events_folder))


    #makae hmm overlay for dygraph
    dp2plot <- 10*5000

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


    temp_dir <-  paste0(tempdir(), "/", squysh_time(),"_", read_directions$condition[[folder]], "_", read_directions$folder[[folder]])

    dir.create(temp_dir)


    rdata_temp_file <- tempfile(pattern = "rdata", tmpdir = temp_dir)
    rdata_temp_file <-  chartr("\\", "/", rdata_temp_file)

    dygraph_master_list <- list(raw_data = flip_raw,
                                run_mean = overlay,
                                final_events = measured_events,
                                count_events = count_events,
                                periods = regroup_data %>%
                                  mutate(state_2_start = state_1_end * conversion,
                                         state_2_stop = state_2_end * conversion,
                                         run_from = state_1_end,
                                         run_to = state_2_end) %>%
                                  dplyr::select(state_2_start, state_2_stop, run_from, run_to),
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





  #  writeLines("Render to HTML")

    #render to HTML
    report[[folder]] <- paste0("failed_to_render_dygraph!_", read_directions$folder[[folder]])

     rmarkdown::render(input = paste0(temp_dir,
                                     "/",
                                     read_directions$condition[[folder]],
                                     "_",
                                     read_directions$folder[[folder]],
                                     "_dygraph.R"),
                      envir = new.env())

    drop_upload(paste0(temp_dir,
                       "/",
                       read_directions$condition[[folder]],
                       "_",
                       read_directions$folder[[folder]],
                       "_dygraph.html"),
                path = format_dropbox_path(plots_folder))


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
drop_upload(paste0(error_file_name, "/log.txt"), format_dropbox_path(trap_selected_date))

signal <- tibble(sig = unlist(var_signal_to_noise_directions)) %>%
  separate(sig, c("folder", "signal"), sep = "!_")

export_directions <- drop_read_csv(directions) %>%
  mutate(folder = observation_folders,
         grouped_file = grouped4r_files) %>%
  left_join(signal)



success_report <- tibble(analysis_complete = unlist(report)) %>%
  separate(analysis_complete, c("report", "folder"), sep = "!_") %>%
  right_join(export_directions, by = "folder") %>%
  replace_na(list(report = "user_excluded")) %>%
  arrange(folder) %>%
  dplyr::select(-starts_with("grouped_file"))

success_report_path <- write_temp_csv(success_report, "directions.csv")

drop_delete(directions)
drop_upload(success_report_path, path = format_dropbox_path(trap_selected_date))













incProgress(1, detail = "Done!")

  }) #close withProgress

  sendSweetAlert(session = session,
                 title =  "Hidden Markov Analysis Complete",
                 text = "Results saved to Dropbox",
                 type = "success")
}



