
#' Determime if Hill Equation can be fit
#'
#' This will attempt to fit the Hill Equation to data in a nested data frame. If hill equation cannot be fit NA is reported. You can use dplyr::filter() to remove these and use fit_hill_nest() to remaining data.
#'
#' @param nest_name
#'
#' @return
#' @export
#'
#' @examples

possibly_hill_nest <- function(data, nest_key){

  nest_key = enquo(nest_key)

  hill_equation <- function(data){

    model <- drm(average_velocity ~ pCa, data = data, fct = LL.4(names = c("hillslope", "min", "vmax", "ec50")), logDose = 10)
  }


  possible_hill_fit <- possibly(hill_equation, otherwise = NA_real_)

  mutate(.data = data, drm_model = map(!!nest_key, possible_hill_fit))

}

