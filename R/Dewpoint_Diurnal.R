#=============================================================================
#' @title
#' Diurnal dew point temperature
#'
#' @description
#' Calculate hourly dew point temperature given daily dew point temperature drived from daily Tmin and RH
#' It assumes that daily dew point occurs at sun rise time and dewpoint diurnal is calucated by linear interpolation as described in Bohn et al. (2013)
#'
#' @references
#' Bohn, T.J., Livneh, B., Oyler, J.W., Running, S.W., Nijssen, B., Lettenmaier, D.P., 2013. Global evaluation of MTCLIM and related algorithms for forcing of ecological and hydrological models. Agricultural and Forest Meteorology 176, 38–49. https://doi.org/10.1016/j.agrformet.2013.03.003
#'
#' @author Hyung Eum, Government of Alberta
#'
#' @param Preday Sunrise time (hour) and dewpoint temperature (Celcius) on previous day, c(sunrise,dewpoint)
#' @param Present Sunrise time (hour) and dewpoint temperature (Celcius) on present day
#' @param Afterday Sunrise time (hour) and dewpoint temperature (Celcius) on after day
#'

#' @return Hourly dew point temperature in degree Celcius (24 points) for the present day

Dew.point.Diurnal<-function(Preday,Present,Afterday){
  x<-c(Preday[1],24+Present[1],48+Afterday[1])
  y<-c(Preday[2],Present[2],Afterday[2])
  xout<-seq(25,48,1)
  Td.hour<-approx(x,y,xout=xout)$y
  return(Td.hour=Td.hour)
}
