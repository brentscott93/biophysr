#' List folders into tibble
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
list_dir <- function(...){
  tibble(name = dir(..., full.names = FALSE),
         path = dir(..., full.names = TRUE))
}
