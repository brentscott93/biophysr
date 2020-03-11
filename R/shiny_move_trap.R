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

  number_obs <- nrow(trap_obs)

  new_obs <- number_obs + 1

  if(new_obs < 10){
    new_folder <- paste0("obs_", 0, new_obs)
  } else {
    new_folder <- paste0("obs_", new_obs)
  }

  new_folder_path <- paste0(trap_selected_date, "/", new_folder)
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

  files_to_move <- dplyr::slice(trap_files, from_index:to_index)

  files_to_move_paths <- files_to_move %>%
    dplyr::pull(path)

  files_to_move_names <-  files_to_move %>%
    dplyr::pull(name)

  new_files_names <- paste0(new_folder_path, "/", files_to_move_names)

  incProgress(amount = .75, detail = "Moving files")

  purrr::map2(files_to_move_paths, new_files_names, file.rename)

  new_paths <- list_files(new_folder_path) %>%
    dplyr::filter(str_detect(name, "Data")) %>%
    dplyr::pull(path)

  new_obs_files <- bind_rows(map(new_paths, read_csv))

  write_csv(new_obs_files, path = paste0(new_folder_path, "/grouped.csv"), append = FALSE)


  #regroup current observation after desired files moved out

  existing_files <- list_dir(trap_selected_obs) %>%
    dplyr::filter(str_detect(name, "Data"))

  regroup <- bind_rows(map(existing_files$path, read_csv))

  write_csv(regroup, path = paste0(trap_selected_obs, "/grouped.csv"), append = FALSE)

  incProgress(1, detail = "Done")
  })
  showNotification("Files moved to new obs.", type = "message")

}


