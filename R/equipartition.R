
#' Equipartition method for calibrating trap stiffness
#'
#' @param data
#'
#' @return trap stiffness
#' @export
#'
#' @examples equipartition(vector)
equipartition <- function(data){

  kb <-  4.10 #boltzman constant

  signal_var <- var(data) #get variance of data trace

  ep <- signal_var/kb  #equation from you, me, and dupuis

  return(ep)

}


x <- 10e3
y <- 1.23*1e3

x==y
y
