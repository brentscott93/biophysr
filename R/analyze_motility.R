
#' Analyze motility data
#'
#' Automates the analysis of motility data from the WRMTRCK output
#'
#' @param data Data needs to be a dataframe from WRMTRCK
#'
#' @return A summary of motility data including the average velocity and percent moving.
#'     average_velocity calculated off the mean velocity of the "AvgSpeed" from all filaments moving more than 4um.
#'     Percent moving calculated by dividing the number of filaments that moved (>4um) by the total number of filaments.
#'     NOTE* Use with dplyr "group_by" to get output for each variables.
#'
#' @export
#'
#' @examples analyze_motility()
#'
#' data <- data(motility_test)
#' analyzed_data <- data %>%
#' group_by("tm", "pH", "pCa", "video") %>%
#' analyze_motility()
#'

analyze_motility <- function(data){

#calculates average velocity
  velocity_average <- data %>%
    dplyr::filter(Distance >= 4) %>%
    summarize(average_velocity = mean(AvgSpeed))

#gets number of moving filaments
  moving_filaments <- data %>%
    dplyr::filter(Distance >= 4) %>%
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
