#' Diurnal solar radiation
#'
#' Diurnal Solar radiation with daily total solar radiation
#'
#' Using a simple cosine bell (Hirschmann, 1974), dirunal solar radiation is calculated.
#' Given the dirunal solar radition, fractin of hourly solar radition is applied to daily solar ration to calculate hourly solar radiatio.
#'
#' @references
#' Kimball, B.A., Bellamy, L.A., 1986. Generation of diurnal solar radiation, temperature, and humidity patterns. Energy in Agriculture 5, 185–197. https://doi.org/10.1016/0167-5826(86)90018-5
#' Hirschmann, J.R., 1974. The cosine function as a mathematical expression for the processes of solar energy. SOl. Energy, 16, 117-124.
#'
#' @param lat Latitude in degrees
#' @param lon Longitude in degrees
#' @param date Date in characer (ex: '2000-12-31')
#' @param tz Time zone
#' @param Solar.Day Daily solar radiation [W/m2]
#'
#' @return Hourly solar radiation [W/m2]
#' @export
#'
#' @examples Solar_Diurnal(lat=53.4,lon=-117.5,date='1961-01-01',tz='MST',Solar.Day=34.7)
#'
Solar_Diurnal<-function(lat, lon, date, tz=Sys.timezone(),Solar.Day){

  #Solar.Day.KJ<-Solar.Day  # Converting W/m2 to KJ/m2/day
  lon.lat <- matrix(c(lon, lat), nrow=1)
  # make our sequence - using noon gets us around daylight saving time issues
  day <- as.POSIXct(date, tz=tz)
  #sequence <- seq(from=day, length.out=span , by="days")

  # get our data
  sunrise <- sunriset(lon.lat, day, direction="sunrise", POSIXct.out=TRUE)
  sunset <- sunriset(lon.lat, day, direction="sunset", POSIXct.out=TRUE)
  solar_noon <- solarnoon(lon.lat, day, POSIXct.out=TRUE)

  sunrise.Hour=as.numeric(format(sunrise$time, "%H"))
  sunset.Hour=as.numeric(format(sunset$time, "%H"))
  D<-sunset.Hour-sunrise.Hour
  t=seq(1,24)
  # A simple cosine bell Hirschmann (1974)
  Cosine.Bell<-cos(pi*(t-12)/D)
  Cosine.Bell[which(Cosine.Bell<0)]<-0 # take only positive values to calculate fraction of solar radiation
  Cosine.Bell[which(t<sunrise.Hour | t>sunset.Hour)]<-0
  Solar.Fraction<-Cosine.Bell/mean(Cosine.Bell)
  #S.noon<-Solar.Day.KJ*pi/(2*D)

  Solar.Diurnal<-Solar.Day*Solar.Fraction
  return(Solar.Diurnal=Solar.Diurnal)
}

