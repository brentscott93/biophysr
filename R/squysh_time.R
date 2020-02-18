#' Prints the system date/time in only numbers (no spaces or punctuation)
#'
#' @return
#' @export
#'
#' @examples
squysh_time <- function(){
  str_replace_all(Sys.time(), "[^[:alnum:]]", "")
}
