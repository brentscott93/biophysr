#' Quick histogram plotting
#'
#' @param data
#' @param aes_x
#' @param bw
#' @param xlab
#' @param colors
#'
#' @return a ggplot
#' @export
#'
#' @examples
plot_distr <- function(data, aes_x, bw, xlab, colors){

  ggplot(data)+
    geom_histogram(aes(x = aes_x, fill = conditions), binwidth = bw, color = "black")+
    scale_fill_manual(values = colors, name = "")+
    xlab(xlab)+
    scale_y_continuous(expand = c(0, 0))+
    expand_limits(x = 0) +
    theme_ppt()+
    theme(axis.line = element_line(size = 1.5),
          panel.border = element_blank())
}
