#' Writes temporary (.tsv) file
#'
#' @param x
#' @param filename
#'
#' @return a character string
#' @export
#'
#' @examples
#' tempfile <- write_temp_tsv(grouped, "grouped.txt")

write_temp_tsv <- function(x, filename, ...){
  #create unique temporary folder
  temp_dir <- paste0(tempdir(), "/", squysh_time(), round(runif(1, 0, 100000)))
  dir.create(temp_dir)

  file_path <- paste0(temp_dir, "/", filename)
  #write r object to temporary folder
  write_tsv(x, path = file_path, ...)

  return(file_path)

}
