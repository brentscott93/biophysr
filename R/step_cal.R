
#' Performs Step Calibration of the Laser Trap
#'
#' @param vector
#'
#' @return
#' @export
#'
#' @examples
step_cal <- function(vector, step){

  data <- vector


  run_mean <- zoo::rollmean(data, 100, align = "right")



  rolling_lm <- tibbletime::rollify(.f = function(x, y) {
    lm(y~x)
  },
  window = 200,
  unlist = FALSE)

  roll <- rolling_lm(1:length(run_mean), run_mean)
  roll <- roll[!is.na(roll)]


  slopes <- vector()
  for(i in seq_along(roll)){

    slopes[i] <- coef(roll[[i]])[[2]]

  }



  peakr <- biophysr::find_peaks(-slopes, 12500)

  indices <- list(c(1, peakr-1000),
                  c(peakr + 1000, length(run_mean)))

  meanr <- vector()
  for(i in seq_along(indices)){

    meanr[i] <- mean(run_mean[indices[[i]][1]:indices[[i]][2]])

  }

  find_diff <- diff(meanr)

  conversion <- step/find_diff

  #plot

  xdat1 <- seq(indices[[1]][1], indices[[1]][2], by = 1)

  xdat2 <- seq(indices[[2]][1], indices[[2]][2], by = 1)



  raw <- ggplot()+
    geom_line(data= as.data.frame(data), aes(x = 1:length(data), y = data))+
    geom_line(data = as.data.frame(run_mean), aes(x = 1:length(run_mean), y = run_mean), color = "red")+
    geom_line(aes(x = xdat1, y = meanr[[1]]), color = "yellow")+
    geom_line(aes(x = xdat2, y = meanr[[2]]), color = "yellow")+
    theme_bw()



  slopeplot <- ggplot()+
    geom_line(aes(x = 1:length(slopes), y = slopes))+
    scale_x_continuous(limits = c(0, length(data)))+
    geom_point(aes(x = peakr, y = slopes[peakr]),shape = "triangle", color = "green", size = 3)+
    theme_bw()



  plots <- gridExtra::grid.arrange(raw, slopeplot)

  results <- list(mv_diff = find_diff,
                  mv2nm_conversion = conversion,
                  plot = plots)

  return(results)

}
