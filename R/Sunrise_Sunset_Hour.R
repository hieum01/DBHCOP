#' Sunrise and sunset time
#' @description
#' Calculating sunrise and sunset time (hour) corresponding to latitude and longitude based on sun-methods {maptools}
#'
#' @references
#' Meeus, J. (1991) Astronomical Algorithms. Willmann-Bell, Inc.
#' Sunrise/Sunset: https://gml.noaa.gov/grad/solcalc/sunrise.html
#'
#' @param lat Latitude in degree
#' @param lon Longitude in degree
#' @param date Date in character (YYYY-MM-DD), ex: '2023-04-30'
#' @param tz Time zone
#' @param span Squence days (default=1)
#'
#' @return Sun.Data: Date, Sunrise time, Sunset time, Lenght of day in hour
#' @export
#'
#' @examples Sun.rise.set.Hour(lat=53.4,lon=-117.5,date='1961-01-01',tz='MST',span=1)
#'
Sun.rise.set.Hour <- function(lat, lon, date, tz=Sys.timezone(),span=1){

  lon.lat <- matrix(c(lon, lat), nrow=1)
  # make our sequence - using noon gets us around daylight saving time issues
  day <- as.POSIXct(date, tz=tz)
  sequence <- seq(from=day, length.out=span , by="days")

  sunrise <- sunriset(lon.lat, sequence, direction="sunrise", POSIXct.out=TRUE)
  sunset <- sunriset(lon.lat, sequence, direction="sunset", POSIXct.out=TRUE)

  sunrise.Hour=as.numeric(format(sunrise$time, "%H"))
  sunset.Hour=as.numeric(format(sunset$time, "%H"))
  Daytime.Hour<-sunset.Hour-sunrise.Hour

  Sun.Data<-cbind.data.frame(as.Date(sequence),sunrise.Hour,sunset.Hour,Daytime.Hour)
  colnames(Sun.Data)<-c('Date','Sunrise','Sunset','Daylength_hour')
  return( Sun.Data=Sun.Data )
}
