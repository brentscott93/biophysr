#' Fits Hill Equation to percent moving data
#'
#' @param data
#'
#' @return a drm object (list)
#' @export
#'
#' @examples

fit_hill_pm <- function(data){

  model <- drm(percent_moving ~ pCa, data = data, fct = LL.4(names = c("hillslope", "min", "vmax", "ec50")), logDose = 10)


}
