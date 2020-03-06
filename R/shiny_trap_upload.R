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

  #READ
  trap_txts <- map(input_data$datapath, read_tsv, col_names = c("bead", "trap"))

  incProgress(amount = .6, detail = "Moving to Dropbox")
  #write temp files
  new_csv_filename <-  map(input_data$name, str_replace, pattern = "txt", replacement = "csv")
  temp_files <- map2(trap_txts, new_csv_filename, write_temp_csv, col_names  = TRUE)

  #upload to dropbox
  map(temp_files, drop_upload, path = format_dropbox_path(trap_selected_date$path_lower))

  incProgress(1, detail = "Done")
})

showNotification("Trap Data Uploaded", type = "message")

}
