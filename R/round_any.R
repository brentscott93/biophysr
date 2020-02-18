#' Round to any integer from https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816
#'
#' @param x
#' @param accuracy
#'
#' @return
#' @export
#'
#' @examples
round_any = function(x, accuracy, f=round){

  f(x/accuracy) * accuracy

}
