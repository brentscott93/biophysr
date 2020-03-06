
#' Writes temporary (.csv) file
#'
#' @param x
#' @param filename
#'
#' @return a character string
#' @export
#'
#' @examples
#' tempfile <- write_temp_csv(iris, "iris.csv")

write_temp_csv <- function(x, filename, ...){
  #create unique temporary folder
  temp_dir <- paste0(tempdir(), "/", squysh_time(), round(runif(1, 0, 100000)))
  dir.create(temp_dir)

  file_path <- paste0(temp_dir, "/", filename)

  #write r object to temporary folder
  write_csv(x, path = file_path, ...)

  return(file_path)

}


