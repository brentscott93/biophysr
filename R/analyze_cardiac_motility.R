
#' Analyze cardiac motility
#'
#' Works the same as analyze_motility() but with Distance >=1 inclusion criteria
#'
#' @export
#'
#' @param data
#'
#' @return a summary dataframe.
#'
#' @examples
analyze_cardiac_motility <- function(data){

  #calculates average velocity
  velocity_average <- data %>%
    dplyr::filter(Distance >= 1) %>%
    summarize(average_velocity = mean(AvgSpeed))

  #gets number of moving filaments
  moving_filaments <- data %>%
    dplyr::filter(Distance >= 1) %>%
    count()

  #renaming output of count()
  names(moving_filaments)[names(moving_filaments) == 'n'] <- 'moving_filaments'

  #counts total number of filaments
  all_filament <- data %>%
    count()

  #renaming output of count()
  names(all_filament)[names(all_filament) == 'n'] <- 'total_filaments'

  #calculate percent moving
  calculate_percent_moving <- right_join(moving_filaments, all_filament) %>%
    mutate(percent_moving = moving_filaments/total_filaments)

  #combine the average velocty and percent moving objects. Replacing NAs with 0s.
  right_join(velocity_average, calculate_percent_moving) %>%
    replace_na(list(average_velocity = 0, moving_filaments = 0, percent_moving = 0))
}
