#' Write file to temp. directory and upload to google drive
#'
#' @param data
#' @param drive_location
#' @param name
#' @param type
#' @param overwrite
#' @param col_names
#'
#' @return
#' @export
#'
#' @examples
drive_writeup <- function(data, drive_location, name, type, overwrite, col_names ){

  td <- paste0(tempdir(), "/", "gsheets_upload_", squysh_time())

  tf <- tempfile(pattern = "file", tmpdir = td, fileext = type)

  write_csv(data, path = tf, col_names = col_names)

  upload <- drive_upload(tf,
                         path = drive_location,
                         name = name,
                         type = type,
                         overwrite = overwrite)

  return(upload)

}
