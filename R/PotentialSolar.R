#' Extraterrestrial solar radiation
#'
#' Calculating potential solar radiation given day of the year and latitude
#'
#' @references
#' Allen, R.G., Pereira, L.S., Raes, D., Smith, M., 1998. Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. Fao, Rome 300, D05109.
#'
#' @param lat Latitude in radian
#' @param Jday Julian date or day of the year
#'
#' @return Potential solar radiation at the edge of the atmosphere [kJ m-2 d-1]
#' @export
#'
#' @examples
PotentialSolar <-
  function(lat,Jday){
    # solar declination [rad]
    #dec<-declination(Jday)
    dec<-0.4102*sin(pi*(Jday-80)/180) # solar declination [rad]
    return(117500*(acos(-tan(dec)*tan(lat))*sin(lat)*sin(dec)+cos(lat)*cos(dec)*sin(acos(tan(dec)*tan(lat))))/pi)
  }

