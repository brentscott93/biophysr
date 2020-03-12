
#' Summarize trap project (shiny app)
#'
#' @param trap_selected_project
#' @param analyzer
#' @param file_type
#'
#' @return
#' @export
#'
#' @examples
shiny_summarize_trap <- function(trap_selected_project, analyzer, file_type){

  withProgress(min = 0, max = 1, value = 0.1, message = "Summarizing Project", {

directions <- list.files(path = trap_selected_project$path,
                         pattern = "directions.csv",
                         recursive = TRUE,
                         full.names = TRUE)

if(analyzer == "hmm"){
event_paths <- list.files(path = trap_selected_project$path,
                          pattern = "hmm_events.csv",
                          recursive = TRUE,
                          full.names = TRUE)
} else {

  event_paths <- list.files(path = trap_selected_project$path,
                            pattern = "mini_events.csv",
                            recursive = TRUE,
                            full.names = TRUE)
}

raw_data_paths <- list.files(path = trap_selected_project$path,
                             pattern = "grouped",
                             recursive = TRUE,
                              full.names = TRUE)

setProgress(value = 0.2, detail = "Reading Data")
read_exported_directions <- suppressMessages(bind_rows(lapply(directions, read_csv))) %>%
  mutate(grouped_file = raw_data_paths) %>%
  filter(report == "success") %>%
  mutate(event_paths = event_paths) %>%
 # rename("quality_control" = `Quality Control`) %>%
  filter(quality_control == "yes")


all_hmm_events <- suppressMessages(map(read_exported_directions$event_paths, read_csv, col_names = TRUE))


for(s in 1:nrow(read_exported_directions)){
  all_hmm_events[[s]] <- cbind(all_hmm_events[[s]], read_exported_directions$condition[[s]])
}




event_files_filtered <- bind_rows(all_hmm_events) %>%
  rename(conditions = 'read_exported_directions$condition[[s]]') #%>%
 # separate(conditions, c("myo", "ph", "phosphate"), sep = "_")


#####summarise

if(file_type == "csv"){
all_grouped <- suppressMessages(map(read_exported_directions$grouped_file, read_csv, col_names = TRUE))


for(g in 1:nrow(read_exported_directions)){
  all_grouped[[g]] <- cbind(all_grouped[[g]], read_exported_directions$condition[[g]])
}


all_grouped <- bind_rows(all_grouped) %>%
  rename(conditions = 'read_exported_directions$condition[[g]]')

get_time <- all_grouped %>%
  group_by(conditions) %>%
  summarize(minutes = round((length(bead)/5000)/60, 2)) #%>%
  #separate(conditions, c("myo", "ph", "phosphate"), sep = "_")

} else {


  all_grouped <- suppressMessages(map(read_exported_directions$grouped_file, read_csv, col_names = FALSE))


  for(g in 1:nrow(read_exported_directions)){
    all_grouped[[g]] <- cbind(all_grouped[[g]], read_exported_directions$condition[[g]])
  }


  all_grouped <- bind_rows(all_grouped) %>%
    rename(conditions = 'read_exported_directions$condition[[g]]')

  get_time <- all_grouped %>%
    group_by(conditions) %>%
    summarize(minutes = round((length(X1)/5000)/60, 2)) #%>%
  #separate(conditions, c("myo", "ph", "phosphate"), sep = "_")


}

num_events <- event_files_filtered %>%
  group_by(conditions) %>%
  count()

setProgress(value = 0.75, detail = "Calculating...")

summarize_trap <- event_files_filtered %>%
  group_by(conditions) %>%
  summarize(time_on_avg = mean(time_on_ms),
            time_on_se = std.error(time_on_ms, na.rm = TRUE),
            time_off_avg = mean(time_off_ms, na.rm = TRUE),
            time_off_se = std.error(time_off_ms, na.rm = TRUE),
            displacement_avg = mean(displacement_nm, na.rm = TRUE),
            displacement_se = std.error(displacement_nm, na.rm = TRUE),
            force_avg = mean(force, na.rm = TRUE),
            force_se = std.error(force, na.rm = TRUE),
            num_events = n()) %>%
  right_join(get_time) %>%
  mutate_if(is.numeric, round, digits = 2)

setProgress(value = 0.9, detail = "Writing Data")
project_name <- trap_selected_project$name


dir.create(paste0(trap_selected_project$path, "/summary"))

write_csv(summarize_trap, paste0(trap_selected_project$path,
                                 "/summary/",
                                 project_name,
                                 "_trap_summary.csv"))


setProgress(value = 0.9, detail = "Done!")
})
  return(summarize_trap)
}
