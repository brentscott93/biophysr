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
shiny_mini_ensemble_analyzer <- function(trap_selected_date, mv2nm, nm2pn, color){

    withProgress(message = 'Analyzing Mini Ensemble', value = 0, max = 1, min = 0, {
    incProgress(amount = .01, detail = "Reading Data")


    #setwd(parent_dir)

    observation_folders <- drop_dir(trap_selected_date) %>%
      dplyr::filter(str_detect(name, "obs")) %>%
      pull(name)

    grouped4r_files <- drop_dir(trap_selected_date, recursive = TRUE) %>%
      dplyr::filter(str_detect("grouped")) %>%
      pull(path_display)

    directions <- drop_dir(trap_selected_date) %>%
      dplyr::filter(name == "directions.csv") %>%
      pull(path_display)

    read_directions <- suppressMessages(drop_read_csv(directions)) %>%
      mutate(folder = observation_folders,
             grouped_file = grouped4r_files) %>%
      filter(include == "yes")


    read_directions$baseline_start_sec <- if(baseline_start_sec == 0){
                                            1/5000
                                           } else {
                                           read_directions$baseline_start_sec*5000
                                           }

    read_directions$baseline_stop_sec <- read_directions$baseline_stop_sec*5000

    #create results folders for output
    results_folder <- paste0(trap_selected_date, "/results")
    drop_create(results_folder)
    events_folder <- paste0(trap_selected_date, "/results/events")
    drop_create(events_folder)
    plots_folder <- paste0(trap_selected_date, "/results/plots")
    drop_create(plots_folder)

    error_file <- file("log.txt", open = "a")
    writeLines(paste0("Mini-ensemble anlaysis performed on ", Sys.time(), "\n"), error_file)
    inc_prog_bar <-  nrow(read_directions) * 4
    report <- vector("list")
    #start loop
    for(folder in seq_along(read_directions$folder)){
      tryCatch({
        incProgress(1/inc_prog_bar, paste("Analyzing", read_directions$condition[[folder]], read_directions$folder[[folder]]))

        report[[folder]] <- paste0("failed_to_initialize!_", read_directions$folder[[folder]])

        #Load data and convert mV to nm
        dat <- drop_read_csv(read_directions$grouped_file[[folder]]) %>%
          mutate(nm_converted = bead*mv2nm) %>%
          dplyr::pull(nm_converted)

        #PROCESS DATA
        #centers data around 0 by either performing a QR decomposition or simply removing baseline mean from all points (i.e. constant detrend)
        #both of these will center the mean around 0. It just depends if there needs to be long linear drift corrected or not

        processed <- if(read_directions$detrend[[folder]] == "yes"){

          break_pts <- seq(25000, length(dat), by = 25000)

          pracma::detrend(dat, tt = "linear", bp = break_pts)

        } else if(read_directions$detrend[[folder]] == "no"){

          get_mean <- mean(dat[read_directions$baseline_start_sec[[folder]] : read_directions$baseline_stop_sec[[folder]]])
          dat - get_mean

        }

        #build table for analysis

        raw_data <- tibble(index = time(processed),
                           trap = processed)

         #calculate running mean
        run_mean <- as.vector(rollmean(raw_data$trap, k = 50, align = "left"))
        run_mean0 <- ifelse(run_mean < 0, 0, run_mean)

        report[[folder]]  <- paste0("failed_event_detection!_", read_directions$folder[[folder]])
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

        ##### FIND BETTER START OF EVENT#######
        end_of_last_event <- max(length(events$state_2_end))
        last_s1_start <- events$state_2_end[end_of_last_event]+ 1
        end_raw <- length(raw_data$trap)

        rescaled_raw_data <- tibble(trap = c(unlist(rescaled_vectors), raw_data$trap[last_s1_start : end_raw]),
                                    index = seq(1, length(trap)))


        report[[folder]] <- paste0("error_measureing_events!_", read_directions$folder[[folder]])
        # dygraph_raw_data[[folder]] <- rescaled_raw_data$trap
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


        incProgress(detail = paste("Identified", nrow(final_events), "events in", length(dat)/5000, "seconds"))

        temp_final_events <- write_temp_csv(final_events, file = paste0(events_folder,
                                              "/",
                                              read_directions$condition[[folder]],
                                              "_",
                                              read_directions$folder[[folder]],
                                              "_",
                                              "mini_ensemble_events.csv"))

        drop_upload(temp_final_events, path = events_folder)

        #plot
       # filter_final_events1 <- filter(final_events_4_plot, end_s2 < 20000)
        #filter_final_events2 <- filter(final_events_4_plot, end_s2 > 20001 & end_s2 < 40000)
        #filter_final_events3 <- filter(final_events_4_plot, end_s2 > max(final_events_4_plot$end_s2) - 40000 & end_s2 < max(final_events_4_plot$end_s2) - 20000)
        #filter_final_events4 <- filter(final_events_4_plot, end_s2 > max(final_events_4_plot$end_s2) - 20000)


        run_mean_rescaled0 <- ifelse(run_mean_rescaled$run_mean < 0 , 0, run_mean_rescaled$run_mean)


        ################################ MAKE DYGRAPH ####################################
        incProgress(detail = "Making Dygraph")
        report[[folder]] <- paste0("failed_to_render_dygraph!_", read_directions$folder[[folder]])

        #save dygraph data

        temp_dir <-  paste0(tempdir(), "/", squysh_time(),"_", read_directions$condition[[folder]], "_", read_directions$folder[[folder]])

        dir.create(temp_dir)

        rdata_temp_file <- tempfile(pattern = "rdata", tmpdir = temp_dir)
        rdata_temp_file <-  chartr("\\", "/", rdata_temp_file)

        save("dygraph_master_list", file =  rdata_temp_file)


        dygraph_master_list <- list(raw_data = rescaled_raw_data$trap,
                                    run_mean = run_mean_rescaled0,
                                    final_events = final_events_4_plot,
                                    parent_dir = parent_dir)

        save("dygraph_master_list", file = paste0(read_directions$folder[[folder]],
                                                  "/results/",
                                                  read_directions$condition[[folder]],
                                                  "_",
                                                  read_directions$folder[[folder]],
                                                  "_dygraph_data.RData"))




        #make dygraph .R file
        writeLines(c(
          "#+ echo=FALSE",
          paste0("parent_dir <- ","'", parent_dir, "'"),
          paste0("obs <- ", "'", read_directions$folder[[folder]], "'"),
          paste0("run_mean_color <- ", "'",color, "'"),
          "
#+ echo=FALSE, fig.width = 10, fig.height = 4
  setwd(parent_dir)

  suppressPackageStartupMessages(library(tidyverse))
  library(dygraphs)
  library(rmarkdown)


  directions <- list.files(pattern = 'directions.csv')
  observation_folders <- list.files(pattern = 'obs')

  read_directions <- suppressMessages(read_csv(directions)) %>%
  mutate(folder = observation_folders)%>%
  filter(include == 'yes')

  setwd(paste0(parent_dir, '/', obs, '/results'))
  dg_dat <- list.files(pattern = 'dygraph_data.RData')
  load(dg_dat)

  d <- data.frame(index = 1:length(dygraph_master_list$run_mean),
                  raw = dygraph_master_list$raw_data[1:length(dygraph_master_list$run_mean)],
                  run = dygraph_master_list$run_mean,
                  thresh = rep(8, length(dygraph_master_list$run_mean)))

  events <- dygraph_master_list$final_events

  periods_df <- data.frame(start = events$end_s1,
                           stop = events$end_s2)


  add_shades <- function(x, periods, ...){
    for(p in 1:nrow(periods)){
      x <- dyShading(x, from = periods$start[[p]], to = periods$stop[[p]], ...)
    }
    x
  }

   add_labels <- function(x, events, ...){
    for(event in 1:nrow(events)){
      x <- dyEvent(x, events$peak_nm_index[[event]], paste(events$time_on_ms[[event]], 'ms,', round(events$force[[event]], digits = 2), 'pN'), ...)
    }
    x
  }

  dygraph(d) %>%
    dySeries('raw', color = 'gray30', strokeWidth = 2) %>%
    dySeries('run', color = run_mean_color,  strokeWidth = 2) %>%
    dySeries('thresh', strokeWidth = 3, color = 'lightgrey') %>%
    dyRangeSelector() %>%
    add_shades(periods_df, color = 'lightpink') %>%
    add_labels(events, labelLoc = 'bottom') %>%
    dyAxis('x', drawGrid = FALSE) %>%
    dyUnzoom()

  "),

          paste0(temp_dir,
                 "/",
                 read_directions$condition[[folder]],
                 "_",
                 read_directions$folder[[folder]],
                 "_dygraph.R")
)

#render to HTML


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
                    path = plots_folder)

        report[[folder]] <- paste0("success!_", read_directions$folder[[folder]])

      }, error=function(e){
        writeLines(paste0("Analysis error in ",
                          read_directions$folder[[folder]],
                          " with error: ",
                          as.character(e)), error_file)
        cat("ERROR :",conditionMessage(e), "\n")
        })

    }

    close(error_file)

    export_directions <- drop_read_csv(directions) %>%
      mutate(folder = observation_folders,
             grouped_file = grouped4r_files)

    success_report <- tibble(analysis_complete = unlist(report)) %>%
      separate(analysis_complete, c("report", "folder"), sep = "!_") %>%
      right_join(export_directions, by = "folder") %>%
      replace_na(list(report = "user_excluded")) %>%
      arrange(folder) %>%
      dplyr::select(-starts_with("grouped_file"))

   success_report_path <-  write_temp_csv(success_report, filename = "directions.csv")

    drop_delete(directions)

    drop_upload(success_report_path, path = trap_selected_date)

   incProgress(1, detail = "Done!")

  }) #close withProgress

  sendSweetAlert(session = session,
                title =  "Mini-ensemble analysis complete",
                text = "Results saved to Dropbox",
                   type = "success")
  }
