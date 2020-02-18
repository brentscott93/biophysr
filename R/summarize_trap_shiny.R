#' Summarize Trap Analysis for Shiny
#'
#' @param wd
#' @param trap_analysis
#'
#' @return a .csv file to the wd name *_trap_summary.csv
#' @export
#'
#' @examples
#' summarize_trap_shiny("my/directory", "mini")
#'
#'
#'
summarize_trap_shiny <- function(wd, trap_analysis){

  withProgress(message = 'Summarizing Project', value = 0.1, max = 1, min = 0, {



setwd(wd)

if(tolower(trap_analysis) == "mini"){
  #get mini ensemble event filenames
  files <- list.files("/Users/brentscott/trap_data/project_cardiacAzo",
                      pattern = "*_mini_ensemble_events.csv",
                      recursive = TRUE)

  event_files <- suppressMessages(bind_rows(map(files, read_csv)))

} else if(tolower(trap_analysis) == "hmm"){

  #get hmm events filenames
  files <- list.files(pattern = "*_hmm_measured_events.csv",
                      recursive = TRUE)

  event_files <- bind_rows(map(files, read_csv)) %>%
    dplyr::rename(conditions = condition,
          time_off_prior_ms = time_off_ms)

}

incProgress(amount = .40, detail = "Reading Data")

directions <- list.files(pattern = "directions.csv",
                         recursive = TRUE)

raw_data_paths <- list.files(pattern = "grouped4r.txt",
                             recursive = TRUE)

read_exported_directions <- suppressMessages(bind_rows(lapply(directions, read.csv))) %>%
  mutate(grouped_file = raw_data_paths) %>%
  filter(report == "success")

all_success <- suppressMessages(map(read_exported_directions$grouped_file, read_tsv, col_names = FALSE))


for(s in 1:nrow(read_exported_directions)){
  all_success[[s]] <- cbind(all_success[[s]], read_exported_directions$condition[[s]])
}

incProgress(amount = .8, detail = "Calculating")

all_success <- bind_rows(all_success) %>%
  rename(conditions = 'read_exported_directions$condition[[s]]')

get_time <- all_success %>%
  group_by(conditions) %>%
  summarize(minutes = round((length(X1)/5000)/60, 2))


num_events <- event_files %>%
  count(conditions)


summarize_trap <- event_files %>%
  group_by(conditions) %>%
  summarize(time_on_avg = mean(time_on_ms),
            time_on_se = std.error(time_on_ms, na.rm = TRUE),
            time_off_avg = mean(time_off_prior_ms, na.rm = TRUE),
            time_off_se = std.error(time_off_prior_ms, na.rm = TRUE),
            displacement_avg = mean(displacement_nm, na.rm = TRUE),
            displacement_se = std.error(displacement_nm, na.rm = TRUE),
            force_avg = mean(force, na.rm = TRUE),
            force_se = std.error(force, na.rm = TRUE),
            num_events = n()) %>%
  right_join(get_time)


project_name <- tail(unlist(str_split(wd, pattern = "/")), 1)

incProgress(amount = 0.95, detail = "Saving Data")

write_csv(summarize_trap, paste0(wd,
                                 "/",
                                 project_name,
                                 "_trap_summary.csv"))


if(tolower(trap_analysis) == "mini"){
  #get mini ensemble event filenames
  files <- list.files(
                      pattern = "*_mini_ensemble_events.csv",
                      recursive = TRUE)

  plotly_data <- suppressMessages(bind_rows(map(files, read_csv)))

} else if(tolower(trap_analysis) == "hmm"){

  #get hmm events filenames
  files <- list.files( pattern = "*_hmm_measured_events.csv",
                      recursive = TRUE)

  plotly_data <- bind_rows(map(files, read_csv))

}


save("plotly_data", file = paste0("plotly_data.RData"))



writeLines(c(

  "#+ echo=FALSE",
  paste0("wd <- ","'", wd, "'"),
  #paste0("colors <- ", "'",c(colors), "'"),
  "
  #+ echo=FALSE, fig.width = 10, fig.height = 8, warnings = FALSE, message = FALSE

  suppressPackageStartupMessages(library(tidyverse))
  library(plotly)
  library(biophysr)

  setwd(wd)

  plotly_data <-  list.files(pattern = 'plotly_data.RData')
  load(plotly_data)


  time_on_plot <- ggplot(plotly_data)+
    geom_histogram(aes(x = time_on_ms, y = stat(count/sum(count)),  fill = conditions),
                   binwidth = 10, color = 'black')+
    facet_wrap(~conditions)+
    scale_fill_manual(name = '')+
    xlab('10 ms bins')+
    ylab('Proportion')+
    ggtitle('Time On')+
    scale_y_continuous(expand = c(0, 0))+
    scale_x_continuous(breaks = seq(0, nrow(plotly_data), by = 10))+
    expand_limits(x = 0) +
    theme_ppt()+
    theme(axis.line = element_line(size = 1.5),
          strip.background = element_blank(),
          legend.position = 'none')

  ggplotly(time_on_plot)

  #+ echo=FALSE, fig.width = 10, fig.height = 8, warnings = FALSE, message = FALSE

   time_off_plot <- ggplot(plotly_data)+
    geom_histogram(aes(x = time_off_prior_ms, y = stat(count/sum(count)),  fill = conditions),
                   binwidth = 40, color = 'black')+
    facet_wrap(~conditions)+
    scale_fill_manual(name ='')+
    xlab('10 ms bins')+
    ylab('Proportion')+
    ggtitle('Time Off')+
    scale_y_continuous(expand = c(0, 0))+
     scale_x_continuous(breaks = seq(0, nrow(plotly_data), by = 10))+
    expand_limits(x = 0) +
    theme_ppt()+
    theme(axis.line = element_line(size = 1.5),
          strip.background = element_blank(),
          legend.position = 'none')

   ggplotly(time_off_plot)

    #+ echo=FALSE, fig.width = 10, fig.height = 8, warnings = FALSE, message = FALSE

   displacement_plot <- ggplot(plotly_data)+
    geom_histogram(aes(x = displacement_nm, y = stat(count/sum(count)),  fill = conditions),
                   binwidth = 2, color = 'black')+
    facet_wrap(~conditions)+
    scale_fill_manual(name = '')+
    xlab('4 nm bins')+
    ylab('Proportion')+
    ggtitle('Displacement')+
    scale_y_continuous(expand = c(0, 0))+
    expand_limits(x = 0) +
    theme_ppt()+
    theme(axis.line = element_line(size = 1.5),
          strip.background = element_blank(),
          legend.position = 'none')

   ggplotly(displacement_plot)

    #+ echo=FALSE, fig.width = 10, fig.height = 8, warnings = FALSE, message = FALSE

    force_plot <- ggplot(plotly_data)+
    geom_histogram(aes(x = force, y = stat(count/sum(count)),  fill = conditions),
                   binwidth = 0.25, color = 'black')+
    facet_wrap(~conditions)+
    scale_fill_manual(name = '')+
    xlab('Force (pN)')+
    ylab('Proportion')+
    ggtitle('Forces')+
    scale_y_continuous(expand = c(0, 0))+
    expand_limits(x = 0) +
    theme_ppt()+
    theme(axis.line = element_line(size = 1.5),
          strip.background = element_blank(),
          legend.position = 'none')

   ggplotly(force_plot)
  "


),   paste0("histo_plotly.R"))


rmarkdown::render("histo_plotly.R")



incProgress(1, detail = "Done!")

  }) #close withProgress

  showNotification("Trap Summary Complete. File saved to project folder.",
                   type = "message")

}

