
#' Fit HMM
#'
#' @param data
#' @param window_width
#' @param setpars_vector
#' @param set_seed
#'
#' @return
#' @export
#'
#' @examples
fit_hmm <- function(data, setpars_vector, set_seed){




  hmm <- depmix(list(run_mean~1,
                     run_var~1),
                     data = data,
                     nstates = 2,
                     family = list(gaussian(), gaussian()))

  hmm <- setpars(hmm, setpars_vector)

set.seed(set_seed)

fit_hmm <- fit(hmm)

return(fit_hmm)

}
