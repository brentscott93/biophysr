#' Download and read google drive file (read_sheets is sooo slow)
#'
#' @param dribble
#' @param col_names
#'
#' @return
#' @export
#'
#' @examples
drive_downread <- function(dribble, col_names, type){

  temp_dir <- paste0(tempdir(),"/drive_downloads_", squysh_time())
  dir.create(temp_dir)
  tf <- tempfile(pattern = "gsheets", tmpdir = temp_dir, fileext = type)


  downloaded <- drive_download(dribble,
                               path = tf,
                               type = type,
                               overwrite = TRUE)

  read_tib <- read_csv(downloaded$local_path, col_names = col_names)

  return(read_tib)

}


