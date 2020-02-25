#' Trim trap grouped4r file in shiny
#'
#' @param trap_selected_obs
#'
#' @return
#' @export
#'
#' @examples
shiny_trim_dygraph <- function(trap_selected_obs, trap_grouped_file, input_dygraph_clean_shave_date_window_1, input_dygraph_clean_shave_date_window_2){

from <- input_dygraph_clean_shave_date_window_1 * 5000
to <- input_dygraph_clean_shave_date_window_2 * 5000

trimmed <- trap_grouped_file[-c(from:to),]

write_tsv(trimmed,
          path = paste0(trap_selected_obs$path, "/", "grouped.txt"),
          col_names =FALSE,
          append = FALSE)

}
