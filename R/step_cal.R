
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

  find_changepoint <- cpt.mean(data)

  change_point <- cpts(find_changepoint)

  indices <- list(c(1, change_point - 1000),
                  c(change_point + 1000, length(data)))

  meanr <- vector()
  for(i in seq_along(indices)){

    meanr[i] <- mean(data[indices[[i]][1]:indices[[i]][2]])

  }

  find_diff <- diff(meanr)

  conversion <- round(abs(step/find_diff), 2)

  #plot

  xdat1 <- seq(indices[[1]][1], indices[[1]][2], by = 0.5)

  xdat2 <- seq(indices[[2]][1], indices[[2]][2], by = 0.5)



plots <- ggplot()+
    geom_line(data= as.data.frame(data), aes(x = 1:length(data), y = data))+
    geom_line(aes(x = xdat1, y = meanr[[1]]), color = "green2", size = 1.5)+
    geom_line(aes(x = xdat2, y = meanr[[2]]), color = "green2", size = 1.5)+
    xlab("Datapoints")+
    ylab("mV")+
    ggtitle(paste0(abs(round(conversion, 2)), "nm/mV"))+
    theme_bw()


  results <- list(mv_diff = find_diff,
                  mv2nm_conversion = conversion,
                  plot = plots)

  return(results)

}
