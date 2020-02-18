#' HMM Analysis
#'
#' @return
#' @export
#'
#' @examples
simulation_drive_hidden_markov_analysis_shiny <- function(date_dribble, overlay_color){

withProgress(message = 'HMM Analysis in Progress', value = 0, max = 1, min = 0, {
    incProgress(amount = .01, detail = "Reading Data")

#make dir on drive

observation_folders <- drive_ls(date_dribble, pattern = "obs_", orderBy = "name")

grouped4r_files <- drive_ls(date_dribble, pattern = "grouped_obs", recursive = TRUE, orderBy = "name")

grouped_files_list <- grouped4r_files %>%
  split(grouped4r_files$name)

directions <- drive_ls(date_dribble, pattern = "directions")

#download directions file from drive
download_directions <- drive_downread(dribble = directions,
                                  col_names = TRUE,
                                  type = "csv")


read_directions <- download_directions %>%
  mutate(folder = observation_folders$name,
         grouped_file = grouped_files_list) %>%
  filter(include == "yes")

read_directions$baseline_start_sec <- read_directions$baseline_start_sec*5000
read_directions$baseline_stop_sec <- read_directions$baseline_stop_sec*5000

hmm_initial_parameters <- c(0.99, 0.01,       #Initial state probabilities
                            0.99, 0.01, #transition probs s1 to s1/s2. These are guesses knowing they are stable states
                            0.01, 0.99) #transition probs s2 to s1/s2. Again a guess

#make "results" folder on google drive to upload output to

results_dribble <- drive_mkdir(name = "results",
                              path = date_dribble,
                              overwrite = TRUE)

events_dribble <- drive_mkdir(name = "events",
                             path = results_dribble,
                             overwrite = TRUE)


plots_dribble <- drive_mkdir(name = "plots",
                             path = results_dribble,
                             overwrite = TRUE)


## LOAD IN DATA ##
report <- vector("list")
var_signal_to_noise <- vector("list")
error_temp_file <- tempfile(pattern = "error", fileext = ".txt")
error_file <- file(error_temp_file, open =  "a")

#loop will start here
for(folder in seq_along(read_directions$folder)){
  tryCatch({

    inc_prog_bar <-  nrow(read_directions)*2
    incProgress(1/inc_prog_bar, paste("Analyzing", read_directions$condition[[folder]], read_directions$folder[[folder]]))

report[[folder]] <- paste0("failed_to_initialize!_", read_directions$folder[[folder]])


temp_dir <- paste0(tempdir(), "/", squysh_time(),"_", read_directions$folder[[folder]])
dir.create(temp_dir)

#downlaod and read from drive
dat <- drive_downread(dribble = read_directions$grouped_file[[folder]], col_names = "bead", type = "csv") %>%
  mutate(nm_converted = bead) %>%
  dplyr::pull(nm_converted)



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


run_mean <- running(processed_data, fun = mean, width = 200, by = 100)


run_var <- running(processed_data, fun = var, width = 200, by = 100)


running_table <- tibble(run_mean = run_mean,
                            run_var = run_var)

## HMM ##

report[[folder]]  <- paste0("failed_HMM!_", read_directions$folder[[folder]])

seed <- floor(runif(1, 0, 1e6))

hmm <- depmix(run_var~1,
              data = running_table,
              nstates = 2,
              family = gaussian())

sd_run_mean <- sd(run_mean)

mean_run_var <- mean(run_var)
sd_run_var <- sd(run_var)


estimate_hmm_gaussians <- c(mean_run_var, sd_run_var,
                            mean_run_var/2, sd_run_var/2)

hmm <- setpars(hmm, c(hmm_initial_parameters, estimate_hmm_gaussians))


set.seed(seed)

hmm_fit <- fit(hmm, emcontrol = em.control(random.start = FALSE))
# emcontrol = em.control(random.start = FALSE)
hmm_posterior <- posterior(hmm_fit)

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

    hmm_fit <- fit(hmm, emcontrol = em.control(random.start = FALSE))

    hmm_posterior <- posterior(hmm_fit)

    hmm_repeat <- hmm_repeat + 1
  }
}

report[[folder]] <- paste0("error-HMM_starts_in_state_2!_", read_directions$folder[[folder]])

if(hmm_posterior$state[[1]] == 2){
  writeLines(c("Skipping",
               read_directions$folder[[folder]],
               "HMM starts in State 2."), error_file);
  next
}


hmm_temp_file <- tempfile(pattern = "hmm_summary", tmpdir = temp_dir, fileext = ".txt")
hmm_file <- file(hmm_temp_file, open =  "a")
capture.output(summary(hmm_fit), file = hmm_file)
close(hmm_file)

sum_fit <- summary(hmm_fit)
base_var <- sum_fit[[1]]
event_var <- sum_fit[[2]]

var_signal_to_noise <- base_var/event_var


## Calculate conversion between window length and data points


#old/not necessary
## COUNT EVENTS ##
#writeLines("Counting Events")

counter <- matrix(table(paste0(head(hmm_posterior$state,-1),tail(hmm_posterior$state,-1))), nrow = 1)


dimnames(counter) <- list(c("#_transitions"),
                               c("1-1", "1-2", "2-1", "2-2"))

count_events <- as.data.frame(counter, row.names = "#_transitions", make.names = TRUE) %>%
  mutate(condition = condition)


#write_csv(count_events, paste0(read_directions$folder[[folder]],
                             #  "/results/",
                             #  read_directions$condition[[folder]],
                             #  "_",
                             #  read_directions$folder[[folder]], "_event_count.csv"))


#save running mean, var, & state for ensemble averaging & dygraph
hmm_identified_events <- tibble(run_mean = run_mean,
                                run_var = run_var,
                                state = hmm_posterior$state)

#write_csv(hmm_identified_events, paste0(read_directions$folder[[folder]],
#                                        "/results/",
#                                        read_directions$condition[[folder]],
 #                                       "_",
 #                                       read_directions$folder[[folder]],
 #                                       "_data4_ensemble_average.csv"))


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
for(i in seq_along(1:nrow(s1_regroup_data))){
  state_1_avg[[i+1]] <- mean(run_mean_tibble$value[(s1_regroup_data$state_2_end[i]+1) : (s1_regroup_data$state_1_end[i])])
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
                condition = condition,
                force = displacement_nm) %>%
  dplyr::select(condition, time_on_ms, time_off_ms, displacement_nm, force)


## SAVE OUTPUT ##

write_csv(measured_events, paste0(temp_dir,
                                  "/",
                                  condition,
                                  "_hmm_events.csv"))

drive_upload(media = paste0(temp_dir,
                           "/",
                           read_directions$condition[[folder]],
                           "_",
                           read_directions$folder[[folder]],
                           "_hmm_events.csv"),
             path = events_dribble,
             type = "spreadsheet")


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


rdata_temp_file <- tempfile(pattern = "rdata", tmpdir = temp_dir)

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
                            var_signal_to_noise = var_signal_to_noise)



save("dygraph_master_list", file =  rdata_temp_file)

  #make dygraph .R file
writeLines(c(
  "#+ echo=FALSE",
  "library(tidyverse)
  library(dygraphs)
  library(rmarkdown)
  library(RColorBrewer)
  library(gridExtra)",
  paste0("rdata_temp_file <- ","'", rdata_temp_file, "'"),
  paste0("obs <- ", "'", condition, "'"),
  paste0("run_mean_color <- ", "'",overlay_color, "'"),
  "
   #+ echo=FALSE, message = FALSE, fig.width = 10, fig.height = 2

  load(rdata_temp_file)

  datatable('Number of Events' = dygraph_master_list$count_events$'1-2',
          'Signal (V1/V2)' = dygraph_master_list$var_signal_to_noise)


#' This is an interactive plot of the running variance and running mean (the exact data the model uses).
#' Shaded regions are the events identified by the Hidden Markov Model.
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
#' The overlay is the Hidden Markov Model 'state' prediction (via Viterbi Algorithm) multipled by several conversion factors.
#' These convert the x-axis 'time' from windows to data points to seconds.
#' The HMM state, baseline mean, and measured step size are used to scale the overlay to each step and subsequent baseline.

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
      x <- dyEvent(x, peak_nm_index[[event]], paste(round(events$time_on_ms[[event]], digits = 1), 'ms,', round(events$force[[event]], digits = 2), 'pN'), ...)
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
#' Plots of the running mean and the running variance.
#' This is a useful way to see how the model decides what is an event or baseline.
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
  ggtitle('Mean-Variance (seperate by state)')+
  ylab('')+
  xlab('Running Mean (nm)')+
  theme_linedraw(base_size = 18)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

grid.arrange(mv1, mv2, nrow = 1)







  "),

  paste0(temp_dir,
         "/",
         condition,
         "_dygraph.R")

)





incProgress(detail = "Render to HTML")

#render to HTML
report[[folder]] <- paste0("failed_to_render_dygraph!_", read_directions$folder[[folder]])


rendered <- tempfile(pattern = "rendered", fileext = ".html", tmpdir = temp_dir)

rmarkdown::render(input = paste0(temp_dir,
                         "/",
                         condition,
                         "_dygraph.R"),
                  output_file = paste0(condition, ".html"),
                  envir = new.env())


drive_upload(media = rendered,
             path = plots_dribble,
             name = paste0(read_directions$condition[[folder]],
                            "_",
                            read_directions$folder[[folder]],
                            "_plots"),
             overwrite = TRUE)

report[[folder]] <- paste0("success!_", read_directions$folder[[folder]])




  }, error=function(e){
    writeLines(paste0("Analysis error in ",
           read_directions$folder[[folder]],
           "with error: ",
           as.character(e)), error_file)
    cat("ERROR :",conditionMessage(e), "\n")
    })

}


export_directions <- download_directions %>%
  mutate(folder = observation_folders$name,
         grouped_file = grouped_files_list,
         var_signal_to_noise = unlist(var_signal_to_noise))

success_report <- tibble(analysis_complete = unlist(report)) %>%
  separate(analysis_complete, c("report", "folder"), sep = "!_") %>%
  full_join(export_directions) %>%
  replace_na(list(report = "user_excluded")) %>%
  arrange(folder) %>%
  dplyr::select(-starts_with("grouped_file"))

new_directions_file <- tempfile(pattern = "directions", fileext = ".csv", tmpdir = temp_dir)
write_csv(success_report, new_directions_file)


drive_upload(media = new_directions_file,
             path = date_dribble,
             name = "directions",
             type = "spreadsheet",
             overwrite = TRUE)






#---------
close(error_file)

drive_upload(media = error_temp_file,
             path = date_dribble,
             name = "error_log",
             type = "document",
             overwrite = TRUE)


incProgress(1, detail = "Done!")

})

  showNotification("HMM analysis is complete. Files saved to selected directory on Google Drive.")

}
