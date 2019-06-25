#' Plot a single velocity-pCa curve
#'
#' Data needs to be from analyze_motility() and predict_hill()
#'
#' @param data
#' @param predict_data
#'
#'
#'
#' @return a ggplot scatterplot fit with the hill equation
#' @export
#'
#' @examples
plot_motility <- function(data, predict_data){
  ggplot()+
    geom_point(data = data, aes(x = pCa, y = average_velocity))+
    geom_ribbon(data = predict_data, aes(x = theoretical_pCa, y = predicted_y, ymin = lower_error, ymax = upper_error), alpha = 0.2) +
    geom_line(data = predict_data, aes(x = theoretical_pCa, y = predicted_y)) +
    scale_x_reverse()+
    ylab("Velocity (um/s)")+
    theme_cowplot()
}
