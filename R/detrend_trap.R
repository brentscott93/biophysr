
#'Drift Correction for Laser Trapping Data
#' @param x
#'
#'
#' @return A Vector
#' @export
#'
#' @examples
detrend_trap <- function(x){

  #get length of data record
  data_length <- length(x)

  #find number of traces concactenated together
  traces_sampled <- data_length/25000

  #build dataframe of indices
  bp_df <-  data.frame(start = seq(1, data_length, by = 25000),
                      stop = tail(seq(0, data_length, by = 25000), -1))

  #seperate data by interval they were collected at
  data_chunks <- vector("list")
  for(i in 1:nrow(bp_df)){

    data_chunks[[i]] <- x[bp_df$start[[i]] : bp_df$stop[[i]]]

  }

  #fit lm to each delta_t chunk and subtract out fitted values to detrend and "0" out
  detrend_data <- vector("list")
  for(i in seq_along(data_chunks)){

    trend <- lm(data_chunks[[i]] ~ time(data_chunks[[i]]))

    detrend_data[[i]] <- (data_chunks[[i]] - trend$fitted.values)

  }
   #return detrended vector
  detrend_data <- unlist(detrend_data)
}


