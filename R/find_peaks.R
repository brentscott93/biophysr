#' Find Peaks Function from "https://github.com/stas-g/findPeaks"
#'
#' @param x
#' @param m
#'
#' @return
#' @export
#'
#' @examples The function takes an ordered sequence (vector) of values x and a number m and returns a vector of indices of local peaks in x. A (local) peak is defined as a point such that m points either side of it has a lower or equal value to it. Thus, m can be used adjust the sensitivity of the peak detection procedure: larger m will result in fewer peaks, whilst smaller values of m will result in more peaks found.

find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}
