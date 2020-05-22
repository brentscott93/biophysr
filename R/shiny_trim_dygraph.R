#' Trim trap grouped4r file in shiny
#'
#' @param trap_selected_obs
#'
#' @return
#' @export
#'
#' @examples
shiny_trim_dygraph <- function(trap_selected_obs, trap_grouped_file, input_dygraph_clean_shave_date_window_1, input_dygraph_clean_shave_date_window_2){

#trap_grouped_file <- read.csv("/Users/brentscott/Desktop/myoV-WT_2ndConrtol _obs-01/grouped.csv")
withProgress(message = "Trimming Data", min= 0, max = 1, value = 0.3, {
  #from <- 49.2836 *5000
  #to <- 60 *5000
  from <- as.integer(input_dygraph_clean_shave_date_window_1 * 5000)
    to <- as.integer(input_dygraph_clean_shave_date_window_2 * 5000)

  trimmed <- trap_grouped_file[-c(from:to),]

setProgress("Writing new 'grouped' file", value = 0.8)
  write_csv(trimmed, path = paste0(trap_selected_obs, "/grouped.csv"), append = FALSE)



 setProgress("Done", value = 1)

})
  showNotification("Observation trimmed.", type = "message")
}
