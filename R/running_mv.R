#' Calulate Running Mean and Variance
#'
#' @param data
#' @param window_width
#'
#' @return
#' @export
#'
#' @examples
running_mv <- function(data, window_width, by){


  run_mean <- running(data, fun = mean, width = window_width, by = by)
  run_var <- running(data, fun = var, width = window_width, by = by)

  running_table <- data.frame(run_mean = run_mean,
                            run_var = run_var)

  return(running_table)
}
