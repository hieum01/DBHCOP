#' Longwave
#'
#' Daily longwave radiation based on the Sephan-Boltzman equation [W/m2]
#' 1 W/m2= 86.4 KJ/m2/day (1*3600*24*10^-3)
#' 1 W/m2= 3.6 KJ/m2/hour (1*3600*10^-3)
#' 1 W/m2= 0.001 KJ/m2/sec
#' 1 W = 1 Joule/sec
#' @references
#' Todd Walter, M., Brooks, E.S., McCool, D.K., King, L.G., Molnau, M., Boll, J., 2005. Process-based snowmelt modeling: does it require more input data than temperature-index modeling? Journal of Hydrology 300, 65–75. https://doi.org/10.1016/j.jhydrol.2004.05.002
#'
#' @param emissivity Emissivity estimated by 'AtmosphericEmissivity' function
#' @param temp Air temperature in degree Celcius
#'
#' @return Daily longwave [W/m2]
#' @export
#'
#' @examples
Longwave <-
  function(emissivity,temp){
    STEFAN_B = 5.669e-8      # (W m^-2 K^-4) Stefan Boltzmann constant
    #SBconstant<-0.00000490 #[kJ m-2 K-4 d-1]
    # SBconstant<-4.89*10^-11 #[kJ m-2 K-4 d-1]
    tempK<-temp+273.15 #[degrees K]

    return(emissivity*STEFAN_B*tempK^4)
  }

