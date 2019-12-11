#' Dilution/Concentration Calculator
#'
#' Calculates unknown concentrations or volumes using (c1 * v1 = c2 * v2). Make sure the numbers you give are in the same units! This function will not know otherwise. Whichever parameter is set to "find", the function will solve for.
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
#' c1v1(c1 = 100, v1 = 2, c2 = "find", v2 = 1000)
#'
c1v1 <- function(c1, v1, c2, v2){

    if(c1 == "find"){

    (c2*v2)/v1

  } else if(v1 == "find") {

    (c2*v2)/c1

  } else if(c2 == "find"){

    (c1*v1)/v2

  } else if(v2 == "find"){

    (c1*v1)/c2

  } else {

   print("nothing to solve for")
}
}

