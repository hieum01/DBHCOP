#' Diurnal temperature
#'
#' Given daily min/max temperature, hourly temperature is calculated.
#'
#' It assumes that minimum temperature occurs at sunrise time while maximum temperature occurs at T_rise+0.67(T_set-T_rise).
#' Once the times of min/max temperature on previous, currnet and after days are set, hourly temperature is calculated by a spline of 3rd Hermite polynomials.
#'
#' @references
#' Bohn, T.J., Livneh, B., Oyler, J.W., Running, S.W., Nijssen, B., Lettenmaier, D.P., 2013. Global evaluation of MTCLIM and related algorithms for forcing of ecological and hydrological models. Agricultural and Forest Meteorology 176, 38–49. https://doi.org/10.1016/j.agrformet.2013.03.003
#'
#' @param lat Latitdue in degrees
#' @param lon Longitude in degrees
#' @param date Current date (YYYY-MM-DD) in character (ex: '2000-12-31')
#' @param tz Time zone
#' @param Tmin Daily minimum temperature in degree Celcius
#' @param Tmax Daily maximum temperature in degree Celcius
#' @param preday tmin/tmax, sunrise and sunset time (in minute) on the previous day: c(tmin, tmax, sunrise, sunset)
#' @param afterday tmin/tmax, sunrise and sunset time (in minute) on the after day: c(tmin, tmax, sunrise, sunset)
#'
#' @return Hourly temperature
#' @export
#'
#' @examples
Temperature_Diurnal<-function(lat, lon, date,tz=Sys.timezone(),Tmin,Tmax,preday=NULL,afterday=NULL) {
  Min.in.Day<-1440 # total minutes in a day

  lon.lat <- matrix(c(lon, lat), nrow=1)

  # make our sequence - using noon gets us around daylight saving time issues
  day <- as.POSIXct(date, tz=tz)
  #sequence <- seq(from=day, length.out=span , by="days")

  # get our data
  sunrise <- sunriset(lon.lat, day, direction="sunrise", POSIXct.out=TRUE)
  sunset <- sunriset(lon.lat, day, direction="sunset", POSIXct.out=TRUE)
  solar_noon <- solarnoon(lon.lat, day, POSIXct.out=TRUE)

  T_rise<-sunrise$day_frac*24*60 # in minute
  T_set<-sunset$day_frac*24*60

  Time.Tmax<-T_rise+0.67*(T_set-T_rise) # Time (in minute) that Tmax occurs
  Time.Tmin<-T_rise                     # Time (in minute) that Tmin occurs (Sunrise time)
  x_cur<-c(Min.in.Day+Time.Tmin,Min.in.Day+Time.Tmax)
  y_cur<-c(Tmin,Tmax)
  #=========================================
  # Previous day
  if (is.null(preday)) {
    x_pre<-c(Time.Tmin,Time.Tmax)
    y_pre<-c(Tmin,Tmax)
  } else {
    Time.Tmax.preday<-(preday[3]+0.67*(preday[4]-preday[3]))*60 # Time (in minute) that Tmax occurs
    Time.Tmin.preday<-preday[3]*60
    x_pre<-c(Time.Tmin.preday,Time.Tmax.preday)
    y_pre<-preday[1:2]
  }
  #c(tmin, tmax, sunrise, sunset)
  if (is.null(afterday)){
    x_after<-c(Min.in.Day*2+Time.Tmin,Min.in.Day*2+Time.Tmax)
    y_after<-c(Tmin,Tmax)
  } else {
    Time.Tmax.afterday<-(afterday[3]+0.67*(afterday[4]-afterday[3]))*60 # Time (in minute) that Tmax occurs
    Time.Tmin.afterday<-afterday[3]*60
    x_after<-c(Min.in.Day*2+Time.Tmin.afterday,Min.in.Day*2+Time.Tmax.afterday)
    y_after<-afterday[1:2]
  }

  xi<-c(x_pre,x_cur,x_after)
  yi<-c(y_pre,y_cur,y_after)

  x<-seq(Min.in.Day,Min.in.Day*2,60)
  Values<-pchip(xi, yi, x) #Piecewise Cubic Hermitean Interpolation Polynomials

  # plot(x,Values,'l')
  # Temperature<-cbind(xi,yi)
  # points(Temperature)
  return(Temp.hourly=Values[2:length(Values)])
}



