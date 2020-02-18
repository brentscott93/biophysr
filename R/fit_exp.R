#' Fit Exponential Distribution
#'
#' @param data
#' @param bw
#' @param color
#' @param zoom
#'
#' @return
#' @export
#'
#' @examples
fit_exp <- function(data, bw, color, zoom){


  x <- data$time_on_ms

  f <- fitdistr(x, "exponential")



  x_val <- seq(min(x), max(x), along.with = x)
  exp_curve <- dexp(x_val, rate = f$estimate)

  c <- 1/f$estimate
  names(c) <- "Expected Value or Mean (1/rate)"


  p <- ggplot()+
    geom_histogram(aes(x = x, y = stat(density)), color = "black", fill =
                     color, binwidth = bw, size = 1)+
    geom_line(aes(x = x_val, y = exp_curve), color = "black", size = 1.5)+
    annotate("text", label = paste0( "Mean = ", round(1/f$estimate, digits = 2)), x = Inf, y = Inf,
             hjust = 1, vjust = 1, size = 5)+
    coord_cartesian(x = c(0, zoom))+
    xlab("Time On (ms)")+
    ggtitle(data$condition[[1]])+
    theme_classic(base_size = 16)

  r <- list(fit = f,
            plot = p,
            mean = c)

  return(r)

}
