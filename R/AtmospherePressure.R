#' Atmosphere pressure
#'
#' Calculating atmosphere pressure in hPa
#'
#' @param Elev Elevation in meter
#' @param Tair Temperature in degree Celcius
#' @param VPD Vapor pressure deficit (kPa); optional
#'
#' @return Atmp.pressure in hPa
#' @export
#'
#' @examples
Atmp.Pressure.Diurnal <-
  function(Elev,Tair,VPD=NULL){
    library(bigleaf)
    Atmp.pressure<-pressure.from.elevation(Elev,Tair,VPD) #kPa

    return(Atmp.pressure*10) #hPa
  }
