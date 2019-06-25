#' Dilution/Concentration Calculator
#'
#' Calculates unknown concentrations or volumes using (c1 * v1 = c2 * v2). Make sure the numbers you give are in the same units! This function will not know otherwise. Whichever parameter is set to 0, the function will solve for.
#'
#' @param c1 initial concentration
#' @param v1 initial volume
#' @param c2 final concentration
#' @param v2 final volume
#'
#' @return a number
#' @export
#'
#' @examples
#'
#' conc_calc(c1 = 100, v1 = 2, c2 = 0, v2 = 1000)
#'
conc_calc <- function(c1, v1, c2, v2){
  if(c1 == 0)
    (c2*v2)/v1
  else if(v1 == 0)
    (c2*v2)/c1
  else if(c2 == 0)
    (c1*v1)/v2
  else if (v1 == 0)
    (c1*v1)/c2
  else print("nothing to solve for")
}
