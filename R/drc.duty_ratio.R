
#' Dutio ratio self-starter (fct) for drm
#'
#' @return
#' @export
#'
#' @examples
drc.duty_ratio <- function(){

  duty_ratio_model <- function(N, Vmax, f){
    Vmax * (1-(1-f)^N)
  }

  fct <- function(x, parm) {
    duty_ratio_model(N = x, Vmax = parm[,1], f = parm[,2])
  }
  ssfct <- function(data){
    Vmax <- max(data[,2])
    f <- 0.05

    start <- c(Vmax, f)
    return(start)
  }
  names <- c("Vmax", "f")
  text <- "Duty Ratio (Uyeda 1990)"

  ## Returning the function with self starter and names
  returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
  class(returnList) <- "drcMean"
  invisible(returnList)
}
