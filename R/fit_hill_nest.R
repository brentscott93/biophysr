
#' Fit Hill Equation to Nested Data
#'
#' Fits the Hill Equation using drc::drm() to all nested data and adds a column to the dataframe with model specifics called "drm_model". The Hill Equation parameters are extracted and added as new columns. The last column that is added is "predictions". This is a list of coordinates that can be graphed to show the "fit" of the line to the data.
#'
#' @param nest_key name of the "nest key" used to create the oringal nested data in nest()
#'
#' @return
#' @export
#'
#' @examples
#'
#' fit_motility <- fit_hill_net(motility_test_nest, "pca_velocity")

fit_hill_nest <- function(data, nest_key){

  nest_key = enquo(nest_key)


  mutate(.data = data, drm_model = map(!!nest_key, fit_hill)) %>%
    mutate(coeffs = map(!!nest_key, ~fit_hill(data = .) %>%
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

