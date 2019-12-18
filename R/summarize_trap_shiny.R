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
  files <- list.files("/Users/brentscott/trap_data/project_cardiacAzo",
                      pattern = "*_hmm_measured_events.csv",
                      recursive = TRUE)

  event_files <- bind_rows(map(files, read_csv))

}

incProgress(amount = .40, detail = "Reading Data")

directions <- list.files("/Users/brentscott/trap_data/project_cardiacAzo",
                         pattern = "directions.csv",
                         recursive = TRUE)

raw_data_paths <- list.files("/Users/brentscott/trap_data/project_cardiacAzo",
                             pattern = "grouped4r.txt",
                             recursive = TRUE)

read_exported_directions <- suppressMessages(bind_rows(lapply(directions, read_csv))) %>%
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

incProgress(1, detail = "Done!")

  }) #close withProgress

  showNotification("Trap Summary Complete. File saved to project folder.",
                   type = "message")

}

