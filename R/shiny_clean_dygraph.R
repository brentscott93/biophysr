
#' Generate dygraph for cleaning trap data in shiny
#'
#' @param trap_selected_obs
#'
#' @return
#' @export
#'
#' @examples
shiny_clean_dygraph <- function(trap_selected_obs, curr_clean_graph){


current_obs <- trap_selected_obs$path

grouped_file <- list.files(current_obs, pattern = "grouped", full.names = TRUE)

gf <- read_tsv(grouped_file, col_names = c("bead", "trap"))

data <- tibble(seconds = 1:nrow(gf)/5000,
               bead = gf$bead)

number_files <- nrow(data)/25000

end_file <- seq(5, by = 5, length.out = number_files)

add_labels <- function(x, events, ...){
  for(event in 1:length(events)){
    x <- dyEvent(x, events[[event]], paste0("F", event), ...)
  }
  x
}



dg <- dygraph(data,  ylab = "mV", xlab = "Seconds",  main = curr_clean_graph) %>%
  dySeries("bead", color = "black") %>%
  dyRangeSelector(fillColor ="", strokeColor = "blsck") %>%
  add_labels(events = end_file, labelLoc = 'bottom', color = "black") %>%
  dyUnzoom() %>%
  dyOptions(axisLabelColor = "black",
            gridLineColor = "black",
            axisLineColor = "black",
            axisLineWidth = 3,
            axisLabelFontSize = 15,
            drawGrid = FALSE)

return(dg)

}
