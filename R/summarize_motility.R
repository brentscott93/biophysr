#' Summarize output from 'analyze_motility'
#'
#' Summarizes the data from analyze_motility() that can be used for plotting average of all analyzed data..
#'
#' @param analyze_motility_data
#'
#' @return
#' @export
#'
#' @examples

summarize_motility <- function(data){

  summarized_data <- data %>%
    dplyr::summarize(
      condition_average_velocity = mean(average_velocity),
      condition_percent_moving = mean(percent_moving),
      sample_size = length(average_velocity),
      velocity_sd = sd(average_velocity),
      velocity_se = velocity_sd/sqrt(sample_size),
      percent_moving_sd = sd(percent_moving),
      percent_moving_se = percent_moving_sd/sqrt(sample_size)
  )

  return(summarized_data)
}
