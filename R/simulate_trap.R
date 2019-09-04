
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
simulate_trap <- function(number_of_events, signal_2_noise){


  #define baseline durations
  baseline_duration <- runif(1000000, 100, 5000)

  #define stepsizes
  step_size <- rnorm(1000000, 8, 1.5)

  #define event durations
  event_duration <- 80 + rexp(1000000, 0.009)

  simulate_events <- c(replicate(number_of_events, jitter(c(rnorm(sample(baseline_duration, 1), 0, 7.5),
                                                            rnorm(sample(event_duration, 1), sample(step_size, 1), 7.5/signal_2_noise)))))


  combine_events <- unlist(simulate_events, use.names = FALSE)

  final <- c(combine_events, rnorm(5000, 0, 7.5))

}


