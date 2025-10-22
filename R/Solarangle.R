#' Solar angle
#'
#' Angle of solar inclination from horizontal at solar noon [rad]
#'
#' @param lat Latitdue [rad]
#' @param Jday Julian date or day of the year [day]
#'
#' @return Solar angle in radians
#' @export
#'
#' @examples
solarangle <-
  function(lat,Jday){
    # solar declination [rad]
    dec<-declination(Jday)

    return(asin(sin(lat)*sin(dec)+cos(lat)*cos(dec)*cos(0)))
  }

