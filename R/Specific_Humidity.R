#' Specific humidity
#'
#' @description
#' Calculate specific humidity given partial water vapor pressure and air temperature
#'
#' @param e partial water vapor pressure in hPa
#' @param Tair Temperature in degree Celcius
#' @param Elev Elevation in meter
#'
#' @seealso \code{\link{WVP.RH.T}}, \code{\link{WVP.Td}}
#' @references
#' Molecular weight of water vapor and dry air: https://en.wikipedia.org/wiki/Molar_mass
#' @return Specific humidity (SH) in ratio
#' @export
#' @examples
#' Tair=25
#' e=WVP.RH.T(RH=0.7,Tair=Tair)
#' SH(e, Tair=Tair,Ele=1011)
#'
SH <- function(e, Tair,Ele) {

  # Molecular weight of water vapor [g/mol]
  Mw <- 18.01528
  # Molecular weight of dry air [g/mol]
  Md <- 28.9634

  # Atmoshphere pressure
  p<-pressure.from.elevation(Ele,Tair)*10 # hPa

  k <- Mw / Md
  Q.air <- k * e / (p - (1 - k) * e)
  return(Q.air)
}
