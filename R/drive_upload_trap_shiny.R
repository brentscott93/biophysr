#' Upload trap files (.txt) to google drive as sheets
#'
#' @param name
#' @param datapath
#'
#' @return
#' @export
#'
#' @examples
#'

upload_trap_shiny <- function(input_data, destination){


withProgress(message = 'Uploading to Drive', value = 0, max = 1, min = 0, {

  incProgress(amount = .3, detail = "Reading Files")

  input_data <- arrange(input_data, name)

  trap_txts_paths <-  input_data$datapath
  trap_txts_names <-  input_data$name

  trap_txts_names <- sapply(trap_txts_names, str_remove, pattern = ".txt")

  #READ
  trap_txts <- lapply(trap_txts_paths, read_tsv, col_names = FALSE)

  names(trap_txts) <- trap_txts_names

  temp_dir_path <- paste0(tempdir(), "/gsheets_", str_replace_all(Sys.time(), "[^[:alnum:]]", ""))
  dir.create(temp_dir_path)

  incProgress(amount = .6, detail = "Writing to Shiny")
  #WRITE
  #convert to csv saving locally in shiny somewhere
  trap_txt_shiny_file_paths <- vector("list")
  for(g in seq_along(gsheets)){
    file_path <- paste0(temp_dir_path, "/", trap_txts_names[[g]], ".csv")
    trap_txt_shiny_file_paths[[g]] <- file_path
    write_csv(gsheets[[g]], path = file_path, col_names = FALSE)
  }

  incProgress(amount = .8, detail = "Convert to sheet")
  #UPLOAD
  for(csv in seq_along(trap_txt_shiny_file_paths)){

    drive_upload(trap_txt_shiny_file_paths[[csv]], path = destination , type = "spreadsheet", overwrite = TRUE)
  }

  incProgress(1, detail = "Done")
})

showNotification("Trap Data Uploaded to Drive", type = "message", duration = NULL)

}
