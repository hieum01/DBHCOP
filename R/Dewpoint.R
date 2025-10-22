#' Dewpoint temperature
#'
#' Calculating dewpoint temperature in degree Celcius given air temperature and relative humidity
#'
#' @references
#' Lawrence, Mark G., 2005: The relationship between relative humidity and the dewpoint temperature
#' in moist air: A simple conversion and applications. Bull. Amer. Meteor. Soc., 86, 225-233.
#' doi: http;//dx.doi.org/10.1175/BAMS-86-2-22
#'
#' @param Tair Air temperature in degree celcius
#' @param RH Relative humidity (ratio)
#'
#' @return Dew point temperature in degree Celcius
#' @export
#'
#' @examples
# Dew.Point<-function(Tair,RH) {
#   Tair.F<- Tair+273.15
#   RH.Perct<-RH*100
#   Td = Tair.F - ((100 - RH.Perct)/5)
#   return(Td.C=Td-273.15)
# }

Dew.Point<-function(Tair,RH) {
  RH.Perct<-RH*100
  TD=243.04*(log(RH.Perct/100)+((17.625*Tair)/(243.04+Tair)))/(17.625-log(RH.Perct/100)-((17.625*Tair)/(243.04+Tair)))
  return(Td.C=TD)
}
