#' Formats dropbox path for shiny upload
#'
#' @param x A file path returned by drop_dir()$path_lower.
#'
#' @return
#' @export
#'
#' @examples
format_dropbox_path <- function(x){
  str_sub(x, 2, str_length(x))
}
