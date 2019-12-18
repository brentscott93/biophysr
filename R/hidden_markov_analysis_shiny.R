#' HMM Analysis
#'
#' @return
#' @export
#'
#' @examples
hidden_markov_analysis_shiny <- function(parent_dir, mv2nm, nm2pn, overlay_color ){

withProgress(message = 'HMM Analysis in Progress', value = 0, max = 1, min = 0, {
    incProgress(amount = .01, detail = "Reading Data")

## START OF SCRIPT ##

## LOAD PACKAGES ##

writeLines("Loading Packages")
library(biophysr)
suppressPackageStartupMessages(library(tidyverse))
library(gtools)
suppressPackageStartupMessages(library(depmixS4))
suppressPackageStartupMessages(library(gridExtra))
library(readr)

## SET WD ##

setwd(parent_dir)

observation_folders <- list.files(pattern = "obs")
grouped4r_files <- list.files(pattern = "grouped4r.txt", recursive = TRUE)
directions <- list.files(pattern = "directions.csv")

read_directions <- suppressMessages(read_csv(directions)) %>%
  mutate(folder = observation_folders,
         grouped_file = grouped4r_files) %>%
  filter(include == "yes")

read_directions$baseline_start_sec <- read_directions$baseline_start_sec*5000
read_directions$baseline_stop_sec <- read_directions$baseline_stop_sec*5000

hmm_initial_parameters <- c(0.99, 0.01,       #Initial state probabilities
                            0.99, 0.01, #transition probs s1 to s1/s2. These are guesses knowing they are stable states
                            0.01, 0.99) #transition probs s2 to s1/s2. Again a guess


## LOAD IN DATA ##
report <- vector("list")
error_file <- file("error_log.txt", open =  "a")
#loop will start here
for(folder in seq_along(read_directions$folder)){
  tryCatch({

    inc_prog_bar <-  nrow(read_directions)
    incProgress(1/inc_prog_bar, paste("Analyzing", read_directions$condition[[folder]], read_directions$folder[[folder]]))

report[[folder]] <- paste0("failed_to_initialize!_", read_directions$folder[[folder]])

#writeLines(paste("Analyzing", read_directions$condition[[folder]], read_directions$folder[[folder]]))

dir.create(path = paste0(read_directions$folder[[folder]], "/", "results"))


#Load data and convert mV to nm
dat <- read.delim(read_directions$grouped_file[[folder]], header = FALSE) %>%
  mutate(nm_converted = V1*mv2nm) %>%
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
writeLines("Calculating Running Mean")

run_mean <- running(processed_data, fun = mean, width = 150, by = 75)

writeLines("Calculating Running Variance")

run_var <- running(processed_data, fun = var, width = 150, by = 75)


running_table <- data.frame(run_mean = run_mean,
                            run_var = run_var)

## HMM ##
writeLines("Fitting HMM")
report[[folder]]  <- paste0("failed_HMM!_", read_directions$folder[[folder]])

seed <- floor(runif(1, 0, 1e6))

hmm <- depmix(list(run_var~1,
                   run_mean~1),
                   data = running_table,
                   nstates = 2,
                   family = list(gaussian(),
                                 gaussian()))

sd_run_mean <- sd(run_mean)

mean_run_var <- mean(run_var)
sd_run_var <- sd(run_var)


estimate_hmm_gaussians <- c(mean_run_var, sd_run_var, 0, sd_run_mean,
                            mean_run_var/2, sd_run_var/2, 7, sd_run_mean)

hmm <- setpars(hmm, c(hmm_initial_parameters, estimate_hmm_gaussians))


set.seed(seed)

hmm_fit <- fit(hmm)

hmm_posterior <- posterior(hmm_fit)

#make sure HMM starts in state 2 this will reset seed and try to refit 10 times

hmm_repeat <- 0

while(hmm_repeat < 10){

  if(hmm_posterior$state[[1]] == 1){
    writeLines("HMM starts in state 1")
    hmm_repeat <- 11

  } else if(hmm_posterior$state[[1]] == 2){
    writeLines(paste("Refitting HMM", hmm_repeat))

    seed <- floor(runif(1, 0, 1e6))

    set.seed(seed)

    hmm_fit <- fit(hmm)

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

writeLines("Saving HMM Summary")
sink(paste0(read_directions$folder[[folder]],
                   "/results/",
                   read_directions$condition[[folder]],
                   "_",
                   read_directions$folder[[folder]],
                   "_", "hmm_summary.txt"))

summary(hmm_fit)

paste("set.seed =", seed)
sink()



## Calculate conversion between window length and data points
raw_length <- length(processed_data)
num_windows <- length(run_var)


## COUNT EVENTS ##
writeLines("Counting Events")

counter <- matrix(table(paste0(head(hmm_posterior$state,-1),tail(hmm_posterior$state,-1))), nrow = 1)


dimnames(counter) <- list(c("#_transitions"),
                               c("1-1", "1-2", "2-1", "2-2"))

count_events <- as.data.frame(counter, row.names = "#_transitions", make.names = TRUE) %>%
  mutate(condition = paste0( read_directions$condition[[folder]]))


write_csv(count_events, paste0(read_directions$folder[[folder]],
                               "/results/",
                               read_directions$condition[[folder]],
                               "_",
                               read_directions$folder[[folder]], "_event_count.csv"))


#save running mean, var, & state for ensemble averaging & dygraph
hmm_identified_events <- data.frame(run_mean = run_mean,
                                    run_var = run_var,
                                    state = hmm_posterior$state)

write_csv(hmm_identified_events, paste0(read_directions$folder[[folder]],
                                        "/results/",
                                        read_directions$condition[[folder]],
                                        "_",
                                        read_directions$folder[[folder]],
                                        "_data4_ensemble_average.csv"))


 report[[folder]] <- paste0("error_measureing_events!_", read_directions$folder[[folder]])

## MEASURE EVENTS ##
writeLines("Measuring Events")
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

#make a copy of data for time off analysis
time_offs <- rle_object %>%
  dplyr::filter(values == 1) %>%
  tail(-1) %>% #this gets rid of the first state 1 when that begins with the start of the trace recording
  head(-1) %>% #this gets rid of the last state 1 that only ends because we stopped collecting
  mutate(off_length_5kHz = lengths*conversion,
         time_off_ms = (off_length_5kHz/5000)*1000)


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
for(i in seq_along(1:nrow(regroup_data))){
  win_values <- c(run_mean_tibble$value[(regroup_data$state_1_end[i]+1) : (regroup_data$state_2_end[i])])
  step_sizes[[i]] <- max(win_values)

  win_values_t <- run_mean_tibble[(regroup_data$state_1_end[i]+1) : (regroup_data$state_2_end[i]),]
  peak_nm_index[i] <- win_values_t$index[which.max(win_values_t$value)]


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
                condition = paste0(read_directions$condition[[folder]]),
                force = displacement_nm*nm2pn) %>%
  dplyr::select(condition, time_on_ms, time_off_ms, displacement_nm, force)


## SAVE OUTPUT ##

write_csv(measured_events, paste0(read_directions$folder[[folder]],
                                  "/results/",
                                  read_directions$condition[[folder]],
                                  "_",
                                  read_directions$folder[[folder]],
                                  "_hmm_measured_events.csv"))




#makae hmm overlay for ggplot and dygraph
dp2plot <- 10*5000

s1_avg_4plot <- tibble(state_order = seq(from = 1, length.out = length(state_1_avg), by = 2),
                       avg = flip_state_1_avg)



s2_avg_4plot <- tibble(state_order = seq(from = 2, length.out = length(step_sizes), by = 2),
                       avg = flip_step_sizes)


hmm_overlay <- bind_rows(s1_avg_4plot, s2_avg_4plot) %>%
  arrange(state_order)

overlay <- vector("list")
for(i in seq_along(1:nrow(hmm_overlay))){
  overlay[[i]] <- rep(hmm_overlay$avg[i],
                      (round(conversion)*rle_object$lengths[-length(rle_object$lengths)][i]))
}

overlay <- unlist(overlay)


#save data for dygraph

dygraph_master_list <- list(raw_data = processed_data,
                            run_mean = overlay,
                            final_events = measured_events,
                            periods = regroup_data %>%
                              mutate(state_2_start = (state_1_end + 1) * conversion,
                                     state_2_stop = state_2_end * conversion) %>%
                              dplyr::select(state_2_start, state_2_stop),
                            peak_nm_index = peak_nm_index * conversion,
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
  paste0("run_mean_color <- ", "'",overlay_color, "'"),
  "
#+ echo=FALSE, fig.width = 10, fig.height = 4
  setwd(parent_dir)

  library(tidyverse)
  library(dygraphs)
  library(rmarkdown)


  directions <- list.files(pattern = 'directions.csv')
  observation_folders <- list.files(pattern = 'obs')

  read_directions_dy <- suppressMessages(read_csv(directions)) %>%
  mutate(folder = observation_folders)%>%
  filter(include == 'yes')

  setwd(paste0(parent_dir, '/', obs, '/results'))
  dg_dat <- list.files(pattern = 'dygraph_data.RData')
  load(dg_dat)

  d <- data.frame(index = (1:length(dygraph_master_list$run_mean)/5000),
                  raw = dygraph_master_list$raw_data[1:length(dygraph_master_list$run_mean)],
                  run = dygraph_master_list$run_mean
                  )

  events <- dygraph_master_list$final_events
  peak_nm_index = dygraph_master_list$peak_nm_index/5000

  periods_df <- data.frame(start = dygraph_master_list$periods$state_2_start/5000,
                           stop = dygraph_master_list$periods$state_2_stop/5000)


  add_shades <- function(x, periods, ...){
    for(p in 1:nrow(periods)){
      x <- dyShading(x, from = periods$start[[p]], to = periods$stop[[p]], ...)
    }
    x
  }

   add_labels <- function(x, events, ...){
    for(event in 1:length(peak_nm_index)){
      x <- dyEvent(x, peak_nm_index[[event]], paste(round(events$time_on_ms[[event]], digits = 1), 'ms,', round(events$force[[event]], digits = 2), 'pN'), ...)
    }
    x
  }

  dygraph(d) %>%
    dySeries('raw', color = '#242424', strokeWidth = 2) %>%
    dySeries('run', color = run_mean_color,  strokeWidth = 2) %>%
    dyRangeSelector() %>%
    add_shades(periods_df, color = 'lightpink') %>%
    add_labels(events, labelLoc = 'bottom') %>%
    dyAxis('x', label = 'seconds', drawGrid = FALSE) %>%
    dyAxis('y', label = 'nm') %>%
    dyUnzoom()

  "),

  paste0(read_directions$folder[[folder]],
         "/results/",
         read_directions$condition[[folder]],
         "_",
         read_directions$folder[[folder]],
         "_dygraph.R")

)

report[[folder]] <- paste0("failed_to_plot!_", read_directions$folder[[folder]])

## PLOT OVERLAY ##
writeLines("Plotting Hmm Overlay...")



if(length(processed_data)/5000 > 41){

plot1 <- ggplot()+
  geom_line(aes(x = (1:dp2plot)/5000, y = flip_raw[1:dp2plot]))+
  geom_line(aes(x = (1:dp2plot)/5000, y = overlay[1:dp2plot]), color = overlay_color,  size = 0.75)+
  xlab("")+
  ylab("nm")+
  ggtitle(paste0(read_directions$condition[[folder]],
                 "_",
                 read_directions$folder[[folder]]))+
  theme_bw()

plot2 <- ggplot()+
  geom_line(aes(x = (dp2plot:(2*dp2plot))/5000, y = flip_raw[dp2plot:(2*dp2plot)]))+
  geom_line(aes(x = (dp2plot:(2*dp2plot))/5000, y = overlay[dp2plot:(2*dp2plot)]), color = overlay_color,  size = 0.75)+
  xlab("")+
  ylab("nm")+
  theme_bw()

plot3 <- ggplot()+
  geom_line(aes(x = ((2*dp2plot):(3*dp2plot))/5000, y = flip_raw[(2*dp2plot):(3*dp2plot)]))+
  geom_line(aes(x = ((2*dp2plot):(3*dp2plot))/5000, y = overlay[(2*dp2plot):(3*dp2plot)]), color = overlay_color,  size = 0.75)+
  xlab("")+
  ylab("nm")+
  theme_bw()

plot4 <- ggplot()+
  geom_line(aes(x = ((3*dp2plot):(4*dp2plot))/5000, y = flip_raw[(3*dp2plot):(4*dp2plot)]))+
  geom_line(aes(x = ((3*dp2plot):(4*dp2plot))/5000, y = overlay[(3*dp2plot):(4*dp2plot)]), color = overlay_color, size = 0.75)+
  xlab("Seconds")+
  ylab("nm")+
  theme_bw()


overlay_plots  <- arrangeGrob(plot1, plot2, plot3, plot4, ncol = 1)

ggsave(paste0(read_directions$folder[[folder]],
              "/results/",
              read_directions$condition[[folder]],
              "_",
              read_directions$folder[[folder]], "_hmm_overlay_plot.png"), overlay_plots, width = 12)



} else if(length(processed_data)/5000 < 41 & length(processed_data)/5000 > 21){


  plot1 <- ggplot()+
    geom_line(aes(x = (1:dp2plot)/5000, y = flip_raw[1:dp2plot]))+
    geom_line(aes(x = (1:dp2plot)/5000, y = overlay[1:dp2plot]), color = overlay_color,  size = 0.75)+
    xlab("")+
    ylab("nm")+
    ggtitle(paste0(read_directions$condition[[folder]],
                   "_",
                   read_directions$folder[[folder]]))+
    theme_bw()

  plot2 <- ggplot()+
    geom_line(aes(x = (dp2plot:(2*dp2plot))/5000, y = flip_raw[dp2plot:(2*dp2plot)]))+
    geom_line(aes(x = (dp2plot:(2*dp2plot))/5000, y = overlay[dp2plot:(2*dp2plot)]), color = overlay_color,  size = 0.75)+
    xlab("Seconds")+
    ylab("nm")+
    theme_bw()

  overlay_plots  <- arrangeGrob(plot1, plot2, ncol = 1)

  ggsave(paste0(read_directions$folder[[folder]],
                "/results/",
                read_directions$condition[[folder]],
                "_",
                read_directions$folder[[folder]], "_hmm_overlay_plot.png"), overlay_plots, width = 12)
} else {

  plot1 <- ggplot()+
    geom_line(aes(x = (1:25000)/5000, y = flip_raw[1:25000]))+
    geom_line(aes(x = (1:25000)/5000, y = overlay[1:25000]), color = overlay_color,  size = 0.75)+
    xlab("")+
    ylab("nm")+
    ggtitle(paste0(read_directions$condition[[folder]],
                   "_",
                   read_directions$folder[[folder]]))+
    theme_bw()

  ggsave(paste0(read_directions$folder[[folder]],
                "/results/",
                read_directions$condition[[folder]],
                "_",
                read_directions$folder[[folder]], "_hmm_overlay_plot.png"), plot1, width = 12)

}

incProgress(detail = "Render to HTML")

#render to HTML
report[[folder]] <- paste0("failed_to_render_dygraph!_", read_directions$folder[[folder]])

rmarkdown::render(paste0(parent_dir,
                         "/",
                         read_directions$folder[[folder]],
                         "/results/",
                         read_directions$condition[[folder]],
                         "_",
                         read_directions$folder[[folder]],
                         "_dygraph.R"))

report[[folder]] <- paste0("success!_", read_directions$folder[[folder]])





  }, error=function(e){
    writeLines(paste0("Analysis error in ",
           read_directions$folder[[folder]],
           "with error: ",
           as.character(e)), error_file)
    cat("ERROR :",conditionMessage(e), "\n")
    })

}


#render to HTML
#writeLines("Render to HTML")
#or(r in 1:nrow(read_directions)){
 # tryCatch({

 #   report[[r]] <- paste0("failed_to_render_dygraph!_", read_directions$folder[[r]])

 #   rmarkdown::render(paste0(parent_dir,
   #                          "/",
 #                            read_directions$folder[[r]],
#                             "/results/",
#                             read_directions$condition[[r]],
#                             "_",
 #                            read_directions$folder[[r]],
#                             "_dygraph.R"))

 #   report[[r]] <- paste0("success!_", read_directions$folder[[r]])

  #}, error=function(e){
   # writeLines(paste0("Render to HTML failed on ",
    #                  read_directions$folder[[r]],
     #                 " with error: ",
      #                as.character(e)), error_file)
    #cat("ERROR :",conditionMessage(e), "\n")
  #  })

#}

export_directions <- suppressMessages(read_csv(directions)) %>%
  mutate(folder = observation_folders,
         grouped_file = grouped4r_files)

success_report <- tibble(analysis_complete = unlist(report)) %>%
  separate(analysis_complete, c("report", "folder"), sep = "!_") %>%
  full_join(export_directions) %>%
  replace_na(list(report = "user_excluded")) %>%
  arrange(folder) %>%
  dplyr::select(-starts_with("grouped_file"))

write_csv(success_report, "directions.csv")



#---------
close(error_file)



incProgress(1, detail = "Done!")

})

  showNotification("HMM analysis is complete. Files saved to selected directory. ")

}
