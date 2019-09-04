
#' Count HMM Events
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
count_events <- function(data){

  counter <- table(paste0(head(data$state,-1),tail(data$state,-1)))

  count_events <- matrix(counter, nrow = 1)

  dimnames(count_events) <- list(c("transition counts"),
                                 c("From state 1 to 1", "1 to 2", "2 to 1", "2 to 2"))

  events <- tibble("No. Events" = count_events[2])

  return(events)

}
