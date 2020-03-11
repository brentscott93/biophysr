#' Lists files into a tibble
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
list_files <- function(...){
  tibble(name = list.files(full.names = FALSE, ...),
         path = list.files(full.names = TRUE, ...))
}


