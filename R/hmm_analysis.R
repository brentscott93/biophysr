####################################################  HMM ANALYSIS #########################################################################

## USER INPUTS ##

condition <- "flip2"

obs <- 1

working_directory <- "/Users/brentscott/Desktop/simulation/flip2 copy"
hmm_initial_parameters <- c(0.9, 0.1,       #Initial state probabilities
                            0.99, 0.01, #transition probs s1 to s1/s2. These are guesses knowing they are stable states
                            0.01, 0.99) #transition probs s2 to s1/s2. Again a guess




seed <- 7543

window_look <- 100 #number of windows to plot for sneak peek

seconds_2_plot <- 1

overlay_color <- "lawngreen"

###########################################################################################################################################
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

setwd(working_directory)
file <- list.files(pattern = "grouped4r.txt")

## LOAD IN DATA ##
writeLines(paste("Loading Data for", condition, "Observation", obs))

myo5_data <- read.delim(file, header = FALSE)

#convert data from mV -> nm and standardize mean to 0.
myo5_data <-  as.vector(scale((myo5_data$V1 * 31), scale = FALSE))


## RUNNING MEAN & VAR ##
writeLines("Calculating Running Mean")

run_mean <- running(myo5_data, fun = mean, width = 150, by = 75)

writeLines("Calculating Running Variance")

run_var <- running(myo5_data, fun = var, width = 150, by = 75)


running_table <- data.frame(run_mean = run_mean,
                            run_var = run_var)

## HMM ##
writeLines("Fitting HMM")

myo5_hmm <- depmix(list(run_var~1,
                        run_mean~1),
                    data = running_table,
                    nstates = 2,
                    family = list(gaussian(),
                                         gaussian()))

mean_run_mean <- mean(run_mean)
sd_run_mean <- sd(run_mean)

mean_run_var <- mean(run_var)
sd_run_var <- sd(run_var)


estimate_hmm_gaussians <- c(mean_run_var, sd_run_var, mean_run_mean, sd_run_mean,
                            mean_run_var/2, sd_run_var/2, mean_run_mean*1.25, sd_run_mean/2)

myo5_hmm <- setpars(myo5_hmm, c(hmm_initial_parameters, estimate_hmm_gaussians))


set.seed(seed)

hmm_fit <- fit(myo5_hmm)

hmm_posterior <- posterior(hmm_fit)

writeLines("Saving HMM Summary")
sink("hmm_summary.txt")
summary(hmm_fit)
sink()


## PLOT FIRST AND LAST 1000 windows ~15 seconds @ 150 window width  ##
writeLines("Plotting...")

## Calculate conversion between window length and data points
raw_length <- length(myo5_data)
num_windows <- length(run_var)

## Specify number of windows to look at
plot_conversion <- (raw_length/num_windows)*window_look


## first plots
first_raw <- ggplot()+
  geom_line(aes(x = 1:length(myo5_data[1:plot_conversion]), y = myo5_data[1:plot_conversion]))+
  xlab("")+
  ylab("")+
  ggtitle(paste0(condition, "_", obs))+
  theme_bw()

first_states <- ggplot()+
  geom_line(aes(x = 1:length(hmm_posterior$state[1:window_look]), y = hmm_posterior$state[1:window_look]))+
  xlab("")+
  ylab("")+
  theme_bw()

##last plots
start_x_last <- length(myo5_data) - plot_conversion
end_x_last <- length(myo5_data)


last_raw <- ggplot()+
  geom_line(aes(x = start_x_last:end_x_last, y = myo5_data[start_x_last:end_x_last]))+
  xlab("")+
  ylab("")+
  theme_bw()


last_states <- ggplot()+
  geom_line(aes(x = (length(hmm_posterior$state) - window_look):length(hmm_posterior$state), y = hmm_posterior$state[(length(hmm_posterior$state) - window_look):length(hmm_posterior$state)]))+
  xlab("")+
  ylab("")+
  theme_bw()


#combine first and last plots
final_plot  <- arrangeGrob(first_raw, first_states, last_raw, last_states, ncol = 1)

ggsave("plot_hmm.png", final_plot, width = 12)


## COUNT EVENTS ##
writeLines("Counting Events")

counter <- table(paste0(head(hmm_posterior$state,-1),tail(hmm_posterior$state,-1)))

count_events <- matrix(counter, nrow = 1)

dimnames(count_events) <- list(c("transition counts"),
                               c("1 to 1", "1 to 2", "2 to 1", "2 to 2"))

count_events <- as.data.frame(count_events) %>%
  mutate(condition = paste0(condition, "_", obs))


write.csv(count_events, "event_count.csv")


## MEASURE EVENTS ##
writeLines("Measuring Events")
#setup
conversion <- length(myo5_data)/length(run_mean)

#convert running mean object to tibble
run_mean_tibble <- tibble::enframe(run_mean)

#finds lengths of events in number of running windows
run_length_encoding <- rle(hmm_posterior$state)

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
#the values in these columsn represent the last data point in either state 1 or state 2
#So the range of values between the end of state 1 (or start of state 2) and the end of state 2 is the event duration
regroup_data <- bind_cols(state_1_end = split_data[[1]]$cumsum, state_2_end = split_data[[2]]$cumsum)

#loop over regrouped data to find the mean of the events displacements
step_sizes <- vector("list", length = nrow(regroup_data)) #allocate space for output storage of loop
for(i in seq_along(1:nrow(regroup_data))){
  step_sizes[[i]] <- mean(run_mean_tibble$value[(regroup_data$state_1_end[i]+1) : (regroup_data$state_2_end[i])])
}

#do opposite to get means of state 1 to subtract from s2 means.
# need to subtract first s1 value and last s2 value of the cumsum to align properly

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
direction_correction <- if(negative_events > positive_events){
  calculate_mean_differences$diff * -1
} else {
   calculate_mean_differences$diff
}


flip_raw <- if(negative_events > positive_events){
  myo5_data * -1
} else {
  myo5_data
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


#add step sizes to the on_times table

measured_events <- on_times %>%
  dplyr::mutate(step_size_nm = direction_correction,
         condition = paste0(condition, "_", obs)) %>%
  dplyr::select(condition, everything())


## SAVE OUTPUT ##

write_excel_csv(measured_events, "measured_events.csv")


## PLOT OVERLAY ##
writeLines("Plotting Hmm Overlay...")

dp2plot <- seconds_2_plot*5000

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


plot1 <- ggplot()+
  geom_line(aes(x = 1:dp2plot, y = flip_raw[1:dp2plot]))+
  geom_line(aes(x = 1:dp2plot, y = overlay[1:dp2plot]), color = overlay_color)+
  xlab("")+
  ylab("nm")+
  ggtitle(paste0(condition, "_", obs))+
  theme_bw()

plot2 <- ggplot()+
  geom_line(aes(x = dp2plot:(2*dp2plot), y = flip_raw[dp2plot:(2*dp2plot)]))+
  geom_line(aes(x = dp2plot:(2*dp2plot), y = overlay[dp2plot:(2*dp2plot)]), color = overlay_color)+
  xlab("")+
  ylab("nm")+
  theme_bw()

plot3 <- ggplot()+
  geom_line(aes(x = (2*dp2plot):(3*dp2plot), y = flip_raw[(2*dp2plot):(3*dp2plot)]))+
  geom_line(aes(x = (2*dp2plot):(3*dp2plot), y = overlay[(2*dp2plot):(3*dp2plot)]), color = overlay_color)+
  xlab("")+
  ylab("nm")+
  theme_bw()

plot4 <- ggplot()+
  geom_line(aes(x = (3*dp2plot):(4*dp2plot), y = flip_raw[(3*dp2plot):(4*dp2plot)]))+
  geom_line(aes(x = (3*dp2plot):(4*dp2plot), y = overlay[(3*dp2plot):(4*dp2plot)]), color = overlay_color)+
  xlab("")+
  ylab("nm")+
  theme_bw()


overlay_plots  <- arrangeGrob(plot1, plot2, plot3, plot4, ncol = 1)

ggsave("hmm_overlay.png", overlay_plots, width = 12)

writeLines("Done")

