
#' Simulate Laser Trap Data
#'
#' @param number_of_events
#' @param signal_2_noise
#'
#' @return a vector
#' @export
#'
#' @examples
#'
#' simulate_trap(10, 2)
#'
simulate_trap_custom <- function(number_of_events, step_size,  event_duration, signal_2_noise, add_noise){


  #define baseline durations
  baseline_duration <- runif(1000000, 100, 5000)





  simulate_events <- c(replicate(number_of_events, jitter(c(rnorm(sample(baseline_duration, 1), 0, 7.5),
                                                            rnorm(event_duration, step_size, 7.5*signal_2_noise)))))


  combine_events <- unlist(simulate_events, use.names = FALSE)

  final <- c(combine_events, rnorm(5000, 0, 7.5))

}



