#' Move trap files to new observations
#'
#' @param trap_obs
#' @param trap_files
#'
#' @return
#' @export
#'
#' @examples
shiny_move_trap <- function(trap_selected_date, trap_obs, trap_selected_obs,  trap_files, dygraph_clean_date_window_1, dygraph_clean_date_window_2){
  #make destination folder
  withProgress(message = 'Moving Files', value = 0, max = 1, min = 0, {

  number_obs <- length(trap_obs)

  new_obs <- number_obs + 1

  if(new_obs < 10){
    new_folder <- paste0("obs_", 0, new_obs)
  } else {
    new_folder <- paste0("obs_", new_obs)
  }

  new_folder_path <- paste0(trap_selected_date$path, "/", new_folder)
  incProgress(amount = .25, detail = "Creating new folder")
  dir.create(path = new_folder_path)

  #identify folders on drive and move
  start_of_file_indices <- seq(0,
                               by = 5,
                               length.out = nrow(trap_files))

  move_files_from <- round_any(dygraph_clean_date_window_1,
                               5,
                               f = floor)

  from_index <- which(start_of_file_indices == move_files_from)

  end_of_file_indices <- seq(5,
                             by = 5,
                             length.out = nrow(trap_files))

  move_files_to <- round_any(dygraph_clean_date_window_2,
                             5,
                             f = ceiling)

  to_index <- which(end_of_file_indices == move_files_to)

  files_to_move <- dplyr::slice(trap_files, from_index:to_index) %>%
    dplyr::pull(path)

  incProgress(amount = .75, detail = "Moving files")

  purrr::map(files_to_move, move_files, destinations = new_folder_path)

  new_paths <- list.files(new_folder_path, pattern = "Data", full.names = TRUE)

  new_obs_files <- bind_rows(map(new_paths, read_tsv, col_names = FALSE))

  write_tsv(new_obs_files, path = paste0(new_folder_path, "/grouped.txt"), col_names = FALSE)

  #regroup current observation after desired files moved out

  existing_files <- list.files(trap_selected_obs$path, pattern = "Data", full.names = TRUE)

  regroup <- bind_rows(map(existing_files, read_tsv, col_names = FALSE))

  write_tsv(regroup, path = paste0(trap_selected_obs$path, "/grouped.txt"), col_names = FALSE, append = FALSE)


  incProgress(1, detail = "Done")
  })
  showNotification("Files moved to new obs.", type = "message")

}
