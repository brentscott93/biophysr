#' Move trap files to new observations
#'
#' @param trap_obs 
#' @param trap_files 
#'
#' @return
#' @export
#'
#' @examples
shiny_move_trap <- function(trap_obs, trap_files){
  #make destination folder
  
  all_obs <- trap_obs() %>% 
    dplyr::filter(str_detect(name, "obs_"))
  
  number_obs <- nrow(all_obs)
  
  new_obs <- number_obs + 1
  
  if(new_obs < 10){
    new_folder <- paste0("obs_", 0, new_obs)
  } else {
    new_folder <- paste0("obs_", new_obs)
  }
  
  new_folder_path <- paste0(trap_selected_date()$path, "/", new_folder)
  
  dir.create(path = new_folder_path)
  
  #identify folders on drive and move
  start_of_file_indices <- seq(0,
                               by = 5, 
                               length.out = nrow(trap_files()))
  
  move_files_from <- round_any(input$dygraph_clean_shave_date_window[[1]],
                               5, 
                               f = floor)
  
  from_index <- which(start_of_file_indices == move_files_from)
  
  end_of_file_indices <- seq(5, 
                             by = 5, 
                             length.out = nrow(trap_files()))
  
  move_files_to <- round_any(input$dygraph_clean_shave_date_window[[2]],
                             5, 
                             f = ceiling)
  
  to_index <- which(end_of_file_indices == move_files_to)
  
  files_to_move <- dplyr::slice(trap_files(), from_index:to_index) %>% 
    dplyr::pull(path)
  
  purrr::map(files_to_move, move_files, to = new_folder_path)
  
  
  
}