#' Summarize Mini Ensemble Event
#'
#' @param data
#'
#' @return a summary tibble
#' @export
#'
#' @examples

summarize_mini_events <- function(data){

  data %>%
      summarize(time_on_avg = mean(time_on_ms),
            time_on_se = std.error(time_on_ms, na.rm = TRUE),
            time_off_avg = mean(time_off_prior_ms, na.rm = TRUE),
            time_off_se = std.error(time_off_prior_ms, na.rm = TRUE),
            displacement_avg = mean(displacement_nm, na.rm = TRUE),
            displacement_se = std.error(displacement_nm, na.rm = TRUE),
            force_avg = mean(force, na.rm = TRUE),
            force_se = std.error(force, na.rm = TRUE))

}
