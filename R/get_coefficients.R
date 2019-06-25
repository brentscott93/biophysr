#' Make tibble of Hill Equation parameters from a fitted drm model
#'
#' Use this to extract hill parameters from a drm model like that produced by fit_hill()
#'
#' @param drm_model
#' @param conditions
#'
#' @return a tibble of coefficients with an identifier column
#' @export
#'
#' @examples
get_coefficients <- function(drm_model, conditions){
  coeffs <- coef(drm_model)
  tibble(condition = conditions,
         vmax = coeffs[2],
         pca50 = log10(coeffs[3]),
         hillslope = coeffs[1])
}

