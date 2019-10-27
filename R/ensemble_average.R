#' Forward & Backward Ensemble Averages
#'
#' @param events
#'
#' @return Both forward and backward ensemble averages
#' @export
#'
#' @examples
ensemble_average <- function(events){


  #emsemble average
  #get longest event
  longest_event <- max(sapply(toy_rmean, length))

  #forward average
  #repeat last data point in each shorter event until event lengths are equal
  forward_events <- vector("list")
  for(i in seq_along(events)){

    forward_events[[i]] <- c(events[[i]], rep(tail(events[[i]], 1), (longest_event - length(events[[i]]))))
  }


  forward_events_df <- data.frame(do.call("cbind", forward_events))

  forward_ensemble <- rowMeans(forward_events_df)

  #backward ensemble
  backward_events <- vector("list")
  for(i in seq_along(events)){

    backward_events[[i]] <- c(rep(head(events[[i]], 1), (longest_event - length(events[[i]]))) , events[[i]] )
  }

  backward_events_df <- data.frame(do.call("cbind", backward_events))

  backward_ensemble <- rowMeans(backward_events_df)

  ensemble_average <- data.frame(forward = forward_ensemble,
                                 backward = backward_ensemble)

  return(ensemble_average)
}
