#' Cloud fraction
#'
#' Calculate daily cloud fraction given transmissivity
#'
#' @param Tx Daily maximum temperature
#' @param Tn Daily minimum temperature
#' @param trans Transmissivity. If trans=NULL, it can be estimated by 'transmissivity' function using daily min/max temperature
#' @param transMin Minimum transmissivity (default=0.15)
#' @param transMax Maximum transmissivity (default=0.75)
#' @param opt 'linear' (default) or 'Black'
#'
#' @return Cloud fraction
#' @export
#'
#' @examples
EstCloudiness <- function (Tx=(-999), Tn=(-999), trans=NULL, transMin = 0.15, transMax = 0.75, opt = "linear")
{
  suppressWarnings(if (any(Tx == -999 || Tn == -999) && is.null(trans)){
    print("Error: Please enter either Max&Min temp or transmissivity")
  } else {
    if (is.null(trans))	trans <- transmissivity(Tx, Tn)
    if (opt=="Black") {
      cl <- (0.34 - sqrt(0.34^2 + 4*0.458*(0.803-trans)))/(-2*0.458)
      cl[which(trans > 0.803)] <- 0
    } else {
      cl <- 1 - (trans-transMin) / (transMax-transMin)
    }
    cl[which(cl > 1)] <- 1
    cl[which(cl < 0)] <- 0
    return(cl)
  } )
}
