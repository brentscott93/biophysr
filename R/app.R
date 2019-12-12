#' Launch App
#'
#' @return
#' @export
#'
#' @examples
app <- function() {
  appDir <- system.file("shiny", package = "biophysr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `biophysr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal",  launch.browser = TRUE)
}

