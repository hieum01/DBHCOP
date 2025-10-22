#' Atmosphere Emissivity
#'
#' Calculating atmosphere emissivity to be used for longwave estimation based on Campbell and Norman (1998)
#'
#' @references
#' Campbell, G.S., Norman, J.M., 1998. An Introduction to Environmental Biophysics, second ed., Springer, New York,p. 286.
#' Todd Walter, M., Brooks, E.S., McCool, D.K., King, L.G., Molnau, M., Boll, J., 2005. Process-based snowmelt modeling: does it require more input data than temperature-index modeling? Journal of Hydrology 300, 65–75. https://doi.org/10.1016/j.jhydrol.2004.05.002
#'
#' @param airtemp Air temperature
#' @param cloudiness Cloudness fraction
#' @param vp vapor pressure in kPa (optional)
#' @param opt Option to choose methodologies (Default='linear'). if opt='Brutsaert, vp is requried.
#'
#' @return Atmosphere Emissivity
#' @export
#'
#' @examples AtmosphericEmissivity(airtemp=-12.8, cloudiness=0.4)
#'
AtmosphericEmissivity <- function (airtemp, cloudiness, vp=NULL, opt="linear")
{
  if (opt == "Brutsaert") {
    if(is.null(vp)){ print("To use Brutsaert's 1975 Clear-Sky Emissivity eqn, enter vapor pressure in kPa")
    } else return((1.24*((vp*10)/(T+273.2))^(1/7)) * (1 - 0.84 * cloudiness) +
                    0.84 * cloudiness)
  } else {
    return((0.72 + 0.005 * airtemp) * (1 - 0.84 * cloudiness) +
             0.84 * cloudiness)
  }
}

