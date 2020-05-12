
#' xDP-release, xTP-binding self-starter (fct) for drm
#'
#' @return
#' @export
#'
#' @examples
drc.xDP_release_xTP_binding <- function(){
  
  binding_rates <- function(molar_conc, k_adp, k_atp){
    (k_adp + (k_atp*molar_conc)) / (k_atp*molar_conc*k_adp)
  }
  
  
  fct <- function(x, parm) {
    binding_rates(molar_conc = x, k_adp = parm[,1], k_atp = parm[,2])
  }
  
  ssfct <- function(data){
    k_adp <- 500
    k_atp <- 2*10^6
    
    start <- c(k_adp, k_atp)
    return(start)
  }
  names <- c("k_adp", "k_atp")
  text <- "xDP-release, xTP-binding"
  
  ## Returning the function with self starter and names
  returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
  class(returnList) <- "drcMean"
  invisible(returnList)
}
