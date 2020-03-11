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

    incProgress(amount = .6, detail = "Moving to 'Box Sync' folder")

    new_csv_filename <-  map(input_data$name, str_replace, pattern = "txt", replacement = "csv")

    box_name <- paste0(trap_selected_date, "/", new_csv_filename)

    map2(trap_txts, box_name, write_csv, col_names = TRUE)

    incProgress(1, detail = "Done")
  })

  showNotification("Trap Data Uploaded", type = "message")

}
