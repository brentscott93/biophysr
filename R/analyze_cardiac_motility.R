
#' Analyze cardiac motility
#'
#' Works the same as analyze_motility() but with Distance >=1 inclusion criteria
#'get
#' @export
#'
#' @param data
#'
#' @return a summary dataframe.
#'
#' @examples
analyze_cardiac_motility <- function(data){

  velocity_average <- data %>%
    dplyr::filter(Distance >= 4) %>%
    summarize(average_velocity = mean(AvgSpeed))

  moving_filaments <- data %>%
    dplyr::filter(Distance >= 1) %>%
    count()

  names(moving_filaments)[names(moving_filaments) == 'n'] <- 'moving_filaments'

  all_filament <- data %>%
    count()

  names(all_filament)[names(all_filament) == 'n'] <- 'total_filaments'

  calculate_percent_moving <- left_join(moving_filaments, all_filament) %>%
    mutate(percent_moving = moving_filaments/total_filaments)

  left_join(velocity_average, calculate_percent_moving)
}
