
#' Find first data point not in mini ensemble event
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
mini_find_true_end <- function(x){
  vector <- ifelse(x > 10, TRUE, FALSE)
  first_less_10 <- min(which(vector == FALSE))
  return(first_less_10)
}

