#' Trim trap grouped4r file in shiny
#'
#' @param trap_selected_obs
#'
#' @return
#' @export
#'
#' @examples
shiny_trim_dygraph <- function(trap_selected_obs, trap_grouped_file, input_dygraph_clean_shave_date_window_1, input_dygraph_clean_shave_date_window_2){

withProgress(message = "Trimming Data", min= 0, max = 1, value = 0.1, {
  from <- input_dygraph_clean_shave_date_window_1 * 5000
    to <- input_dygraph_clean_shave_date_window_2 * 5000

  trimmed <- trap_grouped_file[-c(from:to),]
incProgress("Writing new 'grouped' file", value = 0.5)
  trimmed_temp_path <- write_temp_csv(trimmed,
               filename = "grouped.csv")


  old_grouped <- drop_dir(trap_selected_obs) %>%
   dplyr::filter(str_detect(name, "grouped"))

  incProgress("Deleting old file", value = 0.75)
  drop_delete(old_grouped$path_display)

  incProgress("Uploading new file", value = 0.9)
  drop_upload(trimmed_temp_path,
            trap_selected_obs)

})
}
