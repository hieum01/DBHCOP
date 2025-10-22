#=============================================================================
#' @title Saturation vapor pressure by the Clausius-Clapeyron equation
#' @description calculate saturation vapor pressure \eqn{E_s} at temperature \eqn{Tair}
#' @param Tair Air temperature in degree Celcius
#' @return Saturation vapor pressure in hectopascal (hPa) or millibar (mb)
#' @references
#' Shaman, J., & Kohn, M., 2009, Absolute humidity modulates influenza survival, transmission, and seasonality. Proceedings of the National Academy of Sciences, 106(9), 3243-3248.
#' Wallace, J. M., & Hobbs, P. V., 2006, Atmospheric science: an introductory survey. 92, Academic press.
#' Specific gas constant of water vapor: https://en.wikipedia.org/wiki/Gas_constant#Specific_gas_constant
#' Latent heat of water vapor: https://en.wikipedia.org/wiki/Latent_heat
#' @seealso \code{\link{SVP}}.
#' @return Saturation vapor pressure [hPa]
#' @export
#' @examples SVP.ClaCla(Tair=25.0)

SVP.ClaCla <- function(Tair) {
  T0 <- 273.15 # absolute zero in Kelvin (K)

  # Saturation vapor pressure at absolute zero (hPa)
  Es.T0 <- 6.11

  # Specific gas constant of water vapor (J/Kg/K)
  Rw <- 461.52

  # Latent heat of water vapor (J/kg)
  L <- 2.5e6
  # check parameter
  stopifnot(is.numeric(Tair))

  T.Kel<-Tair+T0

  A<-Es.T0*exp(L/(Rw*T0))
  B<-L/Rw

  Es<-A*exp(-B/T.Kel)

  return(Es)
}

#' @title Partial water vapor pressure given dew point
#' @description calculate partial water vapor pressure given dew point temperature
#'
#' @param T.dew Dew point in Kelvin (K) or in degree Celsius (°C)
#'
#' @return Partial vapor pressure in hectopascal (hPa)
#' @seealso \code{\link{SVP.ClaCla}}.
#' @return artial water vapor pressure [hPa]
#' @export
#' @examples
WVP.Td <- function(T.dew) {
  # check parameters
  stopifnot(is.numeric(T.dew))

  e <- SVP.ClaCla(T.dew)
  return(e)
}

#=========================================================================================
#' @title Partial water vapor pressure
#'
#' @description Calculate partial water vapor pressure given relative humdity and air temperature
#'
#' @param RH relative humidity (ratio)
#' @param Tair temperature in degree celcius
#' @return Partial water vapor pressure [hPa]
WVP.RH.T<-function(RH,Tair) {
  Es <- SVP.ClaCla(Tair) # saturation water vapor at temperature Tair in hectopascal (hPa)
  e<-RH*Es   # partical daily water vapor
  return(e)
}


