
#' Fit Hill Equation to percent moving nested data
#'
#' @param nest_name
#'
#' @return
#' @export
#'
#' @examples

fit_hill_nest_pm <- function(data, nest_key){

  nest_key = enquo(nest_key)


  mutate(.data = data, drm_model = map(!!nest_key, fit_hill_pm)) %>%
    mutate(coeffs = map(!!nest_key, ~fit_hill_pm(data = .) %>%
                          coef %>%
                          as.list %>%
                          as_tibble)) %>%
    unnest(coeffs) %>%
    mutate(pCa50 = log10(`ec50:(Intercept)`)) %>%
    rename(hillslope = `hillslope:(Intercept)`,
           min = 'min:(Intercept)',
           vmax = 'vmax:(Intercept)',
           molar_ec50 = 'ec50:(Intercept)') %>%
    mutate(predictions = map(drm_model, predict_hill))
}
