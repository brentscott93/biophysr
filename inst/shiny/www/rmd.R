#+ echo=FALSE


suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dygraphs))
suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(gridExtra))

rdata_temp_file <- '/var/folders/3g/161bnghn7t17h6vkgxfx68j00000gn/T//RtmpntsfmW/20200224210926_myoV-S217A_pH7.0_Pi_obs_01/rdata3861a05bb7e'
run_mean_color <- 'dodgerblue'

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








