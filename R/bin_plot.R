#' Histogram plot from binned velocity data
#'
#' @param data
#' @param desired_bins
#'
#' @return a histogram from continous data that has been binned
#' @export
#'
#' @examples bin_plot(data_from_mass_import)

bin_plot <- function(data){
  ggplot(data=data, aes(AvgSpeed))+
    geom_histogram(aes(y = stat(density), fill = pH), binwidth = 0.5, size = 0.5, color = "black")+
    ylab(label = "Relative Frequency")+
    xlab(expression(RTF~Velocity~("um·s"^1)))+
    theme_linedraw()+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor.x = element_blank())
}
