
#' Fit Hill Equation to nested data
#'
#' @param nest_name
#'
#' @return
#' @export
#'
#' @examples

extract_coef <- function(data, nest_key){

  nest_key = enquo(nest_key)

    mutate(coeffs = coef(drm_model) %>%
                          as.list %>%
                          as_tibble) %>%
    unnest(coeffs) %>%
    mutate(pCa50 = log10(`ec50:(Intercept)`)) %>%
    rename(hillslope = `hillslope:(Intercept)`,
           vmax = 'vmax:(Intercept)',
           molar_ec50 = 'ec50:(Intercept)') %>%
    mutate(predictions = map(drm_model, predict_hill))
}


