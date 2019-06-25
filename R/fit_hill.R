#' Fits Hill Equation
#'
#' Fits Hill equation using drc::drm(). This function for a single condition in a dataframe. See fit_hill_nest() to fit multiple variables.
#' Data frame must have a "average_velocity" and "pCa" column. If not, use drc::drm() directly. This function simply calls the drm() function and provides some of the parameter inputs for quicker analysis.
#' @param data
#'
#' @return a drm object (list)
#' @export
#'
#' @examples
#'
#' x <- data(motility_test)
#' fit_data <- fit_hill(x)
#'

fit_hill <- function(data){

  model <- drm(average_velocity ~ pCa, data = data, fct = LL.4(names = c("hillslope", "min", "vmax", "ec50")), logDose = 10)


}

