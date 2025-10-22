#' Solar radiation estimation
#'
#' Estimating daily solar radiation in KJ/m2/day or W/m2
#'
#' @param lat Latitude in degrees
#' @param Jday Julian date
#' @param Tx Daily maximum temperature in degree C
#' @param Tn Daily minimum temperature in degree C
#' @param albedo Albedo (default=0.2)
#' @param forest Forest ratio
#' @param slope Slope derived from DEM
#' @param aspect Aspect derived from DEM
#' @param units Solar radiation unit (default=kJm2d)
#' @param latUnits Unit of latitude: 'degrees' or 'radians' (default=degrees)
#' @param printWarn logical (TRUE or FALSE)
#'
#' @return Solar radiation in units (kJ/m2/day or W/m2)
#' @export
#'
#' @examples
Solar <-  function (lat, Jday, Tx, Tn, albedo=0.2, forest=0, slope=0, aspect = 0, units="kJm2d", latUnits = "degrees", printWarn=FALSE) {
  # library(insol)
  if (latUnits == "degrees" ){
    lat <- lat*pi/180
    if (printWarn==TRUE) {
      print('Converted degree to radians')
    }
  }

  if (units == "kJm2d") convert <- 1 else convert <- 86.4  # can convert to W/m2
  return( signif((1 - albedo) * (1 - forest) * transmissivity(Tx, Tn) *
                   PotentialSolar(lat, Jday) * slopefactor(lat, Jday, slope, aspect) / convert , 5 ))
}
