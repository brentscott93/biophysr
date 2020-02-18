#' Trim trap grouped4r file in shiny
#'
#' @param trap_selected_obs 
#'
#' @return
#' @export
#'
#' @examples
shiny_trim_dygraph <- function(trap_selected_obs){


curr_obs <- trap_selected_obs()$path

grouped_file <- list.files(current_obs, pattern = "grouped4r", full.names = TRUE)

grouped_trim_down <- read_tsv(grouped_file,
                              col_names = c("bead", "trap"))

nrows_group <- 1:nrow(grouped_trim_down)

trim_from <- which.min(abs(grouped_trim_down-input$dygraph_clean_shave_date_window[[1]])) 
trim_to <- which.min(abs(grouped_trim_down-input$dygraph_clean_shave_date_window[[2]])) 

trimmed_tib <- slice(grouped_trim_down, trim_from:trim_to)

write_tsv(data = trimmed_tib,
          path = paste0(curr_obs, "/", "grouped4r.txt"),
          col_names =FALSE)

}