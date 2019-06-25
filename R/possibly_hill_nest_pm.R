
#' Fit Hill Equation to Nested Percent Moving Data
#'
#' @param nest_name
#'
#' @return
#' @export
#'
#' @examples

possibly_hill_nest_pm <- function(data, nest_key){

  nest_key = enquo(nest_key)

  hill_equation <- function(data){

    model <- drm(percent_moving ~ pCa, data = data, fct = LL.4(names = c("hillslope", "min", "vmax", "ec50")), logDose = 10)
  }


  possible_hill_fit <- possibly(hill_equation, otherwise = NA_real_)

  mutate(.data = data, drm_model = map(!!nest_key, possible_hill_fit))

}

