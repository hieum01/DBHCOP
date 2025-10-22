#' Diurnal Longwave calculation
#'
#' Calculate hourly longwave radiation using the Stefan-Boltzmann equation given cloudiness estimated from 'Estcloundiness' with Tmin and Tmax
#' It assumes that the cloudiness is constant thourgh a day.
#'
#' 1 W/m2= 86.4 KJ/m2/day (1*3600*24*10^-3)
#' 1 W/m2= 3.6 KJ/m2/hour (1*3600*10^-3)
#' 1 W/m2= 0.001 KJ/m2/sec
#' 1 W = 1 Joule/sec
#'
#' @param Air.Temp.hour Hourly air temperature (degree Celcius) from 'Temperature_Diurnal' function
#' @param cloudiness Cloud fraction from 'EstCloudiness' function
#' @param Longwave.Day Daily incoming longwave radiation [W/m2]
#' @param emissivity Emissivity derived from a Campbell and Norman method given air temperature (optional)
#'
#' @return Hourly longwave [W/m2]
#' @export
#'
#' @examples
Longwave_Diurnal<-function(Air.Temp.hour,cloudiness,Longwave.Day,emissivity=NULL){
  if(is.null(emissivity)) {
    emissivity<-0.72 + 0.005 * Air.Temp.hour*(1-0.84*cloudiness)+0.84*cloudiness
  }
  STEFAN_B = 5.669e-8      # (W m^-2 K^-4) Stefan Boltzmann constant
  #SBconstant<- STEFAN_B*3600*10^-3 #[kJ/m2 K-4 hour-1]
  tempK<-Air.Temp.hour+273.15 #[degrees K]
  Longwave_diurnal<-emissivity*STEFAN_B*tempK^4
  Fraction.longwave<-Longwave_diurnal/mean(Longwave_diurnal)
  Longwave.W.hour<-Fraction.longwave*Longwave.Day
  return(Longwave.W.hour)
}
