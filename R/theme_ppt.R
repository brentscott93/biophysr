#' ggplot2 theme for powerpoint presentations
#'
#' @param base_size
#' @param base_family
#' @param base_line_size
#' @param base_rect_size
#'
#' @return
#' @export
#'
#' @examples
theme_ppt <- function(base_size = 28,
                      base_family = ""){
  theme_bw(base_size = base_size,
           base_family = base_family) %+replace%

    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5),
      axis.title = element_text(
        size = rel(0.95)),
      axis.text = element_text(
        size = rel(0.75)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill=NA,
        size=1)



      )
}
