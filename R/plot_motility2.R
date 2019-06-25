#' Plot motility data for 2 velocity-pCa curves
#' @param data_7.4
#' @param predict_7.4
#' @param data_6.8
#' @param predict_6.8
#'
#' @return a ggplot scatterplot fit with the hill equation
#' @export
#'
#' @examples
plot_motility2 <- function(data_7.4, predict_7.4, data_6.8, predict_6.8){
  ggplot()+
    geom_point(data = data_7.4, aes(x = pCa, y = average_velocity))+
    geom_ribbon(data= predict_7.4, aes(x = theoretical_pCa, y=predicted_y, ymin=lower_error, ymax=upper_error), alpha=0.2) +
    geom_line(data = predict_7.4, aes(x = theoretical_pCa, y=predicted_y)) +
    geom_point(data = data_6.8, aes(x = pCa, y = average_velocity), color = "firebrick")+
    geom_ribbon(data = predict_6.8, aes(x = theoretical_pCa, y=predicted_y, ymin=lower_error, ymax=upper_error), alpha=0.2, fill = "firebrick") +
    geom_line(data = predict_6.8, aes(x = theoretical_pCa, y=predicted_y), color = "firebrick") +
    scale_x_reverse()+
    ylab("Velocity (um/s)")+
    theme_cowplot()
}





