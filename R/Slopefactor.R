#' Slopefactor
#'
#' Adjusts solar radiation for land slope and aspect relative to the sun, 1=level ground
#'
#' @param lat latitdue [rad]
#' @param Jday Julian date or day of the year [day]
#' @param slope slope of the ground [rad]
#' @param aspect ground aspect [rad from north]
#'
#' @return SF: Slope factors
#' @export
#'
#' @examples slopefactor(57.0,365,0.05,1)
slopefactor <- function(lat,Jday,slope,aspect){

  SolAsp <- rep(pi, length(Jday))  # Average Solar aspect is binary - either north (0) or south (pi) for the day
  SolAsp[which(lat - declination(Jday) < 0)] <- 0   #
  SF <- cos(slope) - sin(slope)*cos(aspect-(pi-SolAsp))/tan(solarangle(lat,Jday))
  SF[which(SF < 0)] <- 0  ## Slope factors less than zero are completely shaded

  return( SF )
}

