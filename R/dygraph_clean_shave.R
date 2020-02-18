
#' Creates a dygraph to clean trap data in shiny
#'
#' @param data
#' @param first_file_name
#'
#' @return
#' @export
#'
#' @examples
dygraph_clean_shave <- function(data, first_file_name, title){

  data <- data %>% dplyr::pull(bead)

  numbers <- unlist(str_extract_all(first_file_name, "\\d"))


  numbers <- paste0(numbers, collapse = "")

  date <- paste0(str_sub(numbers, 1, 4),
                 "-",
                 str_sub(numbers, 5, 6),
                 "-",
                 str_sub(numbers, 7, 8),
                 " ",
                 str_sub(numbers, 9,10),
                 ":",
                 str_sub(numbers, 11, 12),
                 ":",
                 str_sub(numbers, 13, 14),
                 "+0000")

  dt <- lubridate::as_datetime(date)

  ms <- seq(from = 0.0000,  by = 0.0002, along.with = data)

  dt_ms <- dt + ms



  xts2 <- xts::xts(data, order.by = dt_ms)


  number_files <- length(data)/25000

  end_file <- seq(dt + 5 , by = 5, length.out = number_files)

  add_labels <- function(x, events, ...){
    for(event in 1:length(events)){
      x <- dyEvent(x, events[[event]], event, ...)
    }
    x
  }



  dygraph(xts2,  ylab = "mV", xlab = "Time", group = "group", main = title) %>%
    dySeries("V1", color = "black") %>%
    dyRangeSelector(fillColor ="", strokeColor = "black") %>%
    add_labels(events = end_file, labelLoc = 'bottom', color = "black") %>%
    dyUnzoom() %>%
    dyOptions(useDataTimezone = TRUE) %>%
    dyAxis('x', drawGrid = FALSE)

}

