
#' Add relative velocity columns to motility data
#'
#' @param summary_data
#' @param condition
#' @param control_name
#' @param ph_value
#'
#' @return
#' @export
#'
#' @examples

add_relative_velocity <- function(summary_dat, condition, control_name, ph_value){

  condition <- enquo(condition)

  control_ref_value_help <- summary_dat %>%
    dplyr::filter(!!condition == control_name, pH == ph_value)

  control_ref_value <- tibble(date = control_ref_value_help$date, reference_velocity = control_ref_value_help$average_velocity)

  add_reference_velocity <- left_join(summary_dat, control_ref_value)

  calculate_relative_vel <- add_reference_velocity %>%
    dplyr::mutate(relative_velocity = average_velocity/reference_velocity) %>%
    dplyr::mutate(rel_prop_velocity = relative_velocity - 1)
}
