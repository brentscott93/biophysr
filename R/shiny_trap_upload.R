#' Move trap files (.txt) to Box Sync folder
#'
#' @param name
#' @param datapath
#'
#' @return
#' @export
#'
#' @examples
#'

shiny_trap_upload <- function(input_data, trap_selected_date){


withProgress(message = 'Uploading trap data', value = 0, max = 1, min = 0, {

  incProgress(amount = .3, detail = "Reading Files")

  input_data <- arrange(input_data, name)

  trap_txts_paths <-  input_data$datapath
  trap_txts_names <-  input_data$name


  #READ
  trap_txts <- map(trap_txts_paths, read_tsv, col_names = FALSE)

  incProgress(amount = .6, detail = "Moving to 'Box Sync' folder")

  box_sync_location <- paste0(trap_selected_date$path, "/", trap_txts_names)

  map2(trap_txts, box_sync_location, write_tsv, col_names = FALSE)

  incProgress(1, detail = "Done")
})

showNotification("Trap Data Uploaded", type = "message", duration = NULL)

}
