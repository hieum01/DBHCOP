#' DB-HCOP
#'
#' @description
#' Daily based-Hourly Climate Output Producer (DB-HCOP) disaggregating daily to hourly
#'
#' @details
#' Disaggregation of precipitation and wind speed employs hourly fraction sampled from observed hourly climate data.
#' A list of climate stations (i.e., Prcp.STN.List and Wind.STN.List) is required to find a set of nearest neighbor stations (N.Neighbor) from a target point (lon and lat).
#' With a fragment approach, a set of samples of hourly fraction is collected from the nearest neighbor stations, which has a similarity to daily precipitation and wind speed within upper and lower bounds.
#' Then a hourly fraction is randomly chosen from the populated samples of hourly fractions.
#' For diurnal temperature, radiation, dew point temperature, please refer to 'Temperature_Diurnal', 'Solar_Diurnal','Longwave_Diurnal','Dew.point.Diurnal'.
#'
#' @param lon Logitude in degree
#' @param lat Latitude in degree
#' @param Ele Elevatoin, a.s.m.l [m]
#' @param tz Time zone
#' @param Start.Date Start date in character (YYYY-MM-DD), ex:'2000-01-01'
#' @param End.Date End date in character (YYYY-MM-DD), ex:'2000-12-31'
#' @param Prcp Daily precipitation from Start.Date to End.Date [mm/day]
#' @param Tmin Daily minimum temperature from Start.Date to End.Date [degree Celcius]
#' @param Tmax Daily maximum temperature from Start.Date to End.Date [degree Celcius]
#' @param Wind Daily wind speed from Start.Date to End.Date [m/sec]
#' @param RH Daily relative humidity from Start.Date to End.Date [ratio]
#' @param Solar.Rad Daily solar(shortwave) radiation from Start.Date to End.Date [W/m2]
#' @param Long.Rad Daily longwave incoming radiation from Start.Date to End.Date [W/m2]
#' @param Prcp.STN.List List of precipitation stations providing hourly fraction of precipitation, (Station name,ID, Lat, Lon)
#' @param Wind.STN.List List of wind stations providing hourly fraction of wind speed, (Station name,ID, Lat, Lon)
#' @param Dir.Prcp.ratio Path of a folder that stores precipitation fraction files for all stations listed in Prcp.STN.List
#' @param Dir.Wind.ratio Path of a folder that stores wind fraction files for all stations listed in Wind.STN.List
#' @param N.Neighbor Number of nearest neighbor climate stations (default=3)
#' @param Upper.bound Upper bound for pupulating samples of houlry fractions from nearest neighbor climate stations (default=1.2)
#' @param Lower.bound Lower bound for pupulating samples of houlry fractions from nearest neighbor climate stations (default=0.8)
#'
#' @return Prcp.Diurnal[mm/hour],Air.Temp.Diurnal[degree Celcius],Dew.Temp.Diurnal[degree Celcius],
#' RH.Diurnal[ratio],SH.Diurnal[ratio],Wind.Diurnal[m/sec],Atmosphere.Diurnal[hPa],WVP.Diurnal[hPa],
#' Solar.Rad.Diurnal[W/m2],Long.Rad.Diurnal[W/m2],Cloudness.Diurnal[ratio]
#' @export
#'
#' @examples
Disaggregation <- function(lon,lat,Ele,tz=Sys.timezone(),Start.Date,End.Date,Prcp,Tmin,Tmax,Wind,RH,Solar.Rad=NULL,Long.Rad=NULL,
                           Prcp.STN.List,Wind.STN.List,Dir.Prcp.ratio,Dir.Wind.ratio,N.Neighbor=3, Upper.bound=1.2, Lower.bound=0.8) {
  BD<-as.Date(Start.Date)
  ED<-as.Date(End.Date)
  Date.Seq<-as.Date((seq(BD,ED,by="1 day")))
  Nday<-length(Date.Seq)
  JDay <- strptime(Date.Seq, format="%Y-%m-%d")$yday+1 #Julian day

  Time.seq.Hourly<-seq(as.POSIXct(paste0(Start.Date,' 00:00')), as.POSIXct(paste0(End.Date,' 23:00')), by="hour")

  # Calculate sunrise and sunset time
  Sun.Data<-Sun.rise.set.Hour(lat,lon,Date.Seq[1],tz,span=Nday)

  #=============================================================================================================
  # Calculating daily dew point temperature and sunrise time given daily Tmin and RH (ratio)
  #=============================================================================================================
  T.dew.Day<-Dew.Point(Tmin,RH) #Dew point temperature at a time that minimum temperature occurs
  Data.Dew.sunrise<-cbind.data.frame(Sun.Data$Date,Sun.Data$Sunrise,T.dew.Day)
  colnames(Data.Dew.sunrise)<-c('Date','Sunrise','Dew point')
  #====================================================================================
  # List of ECCC stations in which hourly prcp and wind data are availabe
  #List.ECCC.STN.Prcp<-read.csv(Prcp.STN.List)
  #List.ECCC.STN.Wind<-read.csv(Wind.STN.List)
  #--------------------------------------------------------------------------------------------
  # Folder that fraction files for ECCC stations are located
  Fraction.Folder.Prcp<-Dir.Prcp.ratio
  Fraction.Folder.Wind<-Dir.Wind.ratio
  #====================================================================================
  Prcp.Hour.All<-NULL
  Air.Temp.hour.All<-NULL
  Dew.Temp.hour.All<-NULL
  RH.hour.All<-NULL #Relative humidity
  SH.hour.All<-NULL #Specific humidity
  Wind.Hour.All<-NULL
  Atmosphere.Pressure.hour.All<-NULL
  WVP.hour.All<-NULL
  Solar.W.hour.All<-NULL
  Longwave.W.hour.All<-NULL
  Cloudness.Fraction.All<-NULL

  for (iDay in 1:Nday) {
    Date<-Date.Seq[iDay]
    print(Date)
    Prcp.day<-Prcp[iDay]
    Tmin.day<-Tmin[iDay]
    Tmax.day<-Tmax[iDay]
    Tave.day<-(Tmin.day+Tmax.day)/2
    RH.Daily<-RH[iDay]
    Wind.day<-Wind[iDay]
    if(is.null(Solar.Rad)) {
      Solar.Day<-NULL
    }

    if(is.null(Long.Rad)){
      Longwave.Day<-NULL
    }

    #====================================================================================
    # Assume that cloudness is constant in a day
    #====================================================================================
    cloudiness<-EstCloudiness(Tmax.day,Tmin.day)
    Cloudness.Fraction.hour<-rep(cloudiness,24)
    Emissivity<-AtmosphericEmissivity(Tave.day,cloudiness)
    #====================================================================================
    # Estimating shortwave (solar) and longwave radiation
    if(is.null(Longwave.Day)) {
      Longwave.Day<-Longwave(Emissivity,Tave.day) # Estimated longwave radiation [W/m2]
    }
    if(is.null(Solar.Day)) {
      Solar.Day<-Solar(lat, Jday=JDay[iDay], Tmax.day, Tmin.day, units="W/m2") # Estimated solar radiation [W/m2]
    }
    #
    #=============================================================================================================
    # Calculating daily water vapor in hectopascal (hPa)
    Es <- SVP.ClaCla(Tave.day) # saturation water vapor at temperature t in hectopascal (hPa)
    WVP.Day<-RH.Daily*Es   # partial daily water vapor at temperature Tave in hectopascal (hPa)
    #*************************************************************************************************************|
    #                                                                                                             |
    # Temporal Disaggregation (hourly) given daily meteorological variables                                       |
    # for temperature, relative humidity, water vapor, short/longwave radiations                                  |
    #                                                                                                             |
    #*************************************************************************************************************|
    #===========Diurnal air temperature ==================================================
    # preday and afterday are required except for the first and last days
    # format: preday=c(tmin, tmax, sunrise, sunset)

    if (iDay==1) {
      preday=c(Tmin[iDay], Tmax[iDay], Sun.Data$Sunrise[iDay], Sun.Data$Sunset[iDay])
      afterday=c(Tmin[iDay+1], Tmax[iDay+1], Sun.Data$Sunrise[iDay+1], Sun.Data$Sunset[iDay+1])
    } else if (iDay==Nday) {
      preday=c(Tmin[iDay-1], Tmax[iDay-1], Sun.Data$Sunrise[iDay-1], Sun.Data$Sunset[iDay-1])
      afterday=c(Tmin[iDay], Tmax[iDay], Sun.Data$Sunrise[iDay], Sun.Data$Sunset[iDay])
    } else {
      preday=c(Tmin[iDay-1], Tmax[iDay-1], Sun.Data$Sunrise[iDay-1], Sun.Data$Sunset[iDay-1])
      afterday=c(Tmin[iDay+1], Tmax[iDay+1], Sun.Data$Sunrise[iDay+1], Sun.Data$Sunset[iDay+1])
    }

    Air.Temp.hour<-Temperature_Diurnal(lat, lon, Date,tz,Tmin.day,Tmax.day,preday=preday,afterday=afterday)
    #===========Diurnal solar radiation ==================================================
    Solar.W.hour<-Solar_Diurnal(lat, lon, Date,tz,Solar.Day)            # W/m2
    #===========Diurnal Longwave radiation ===============================================
    Emissivity<-AtmosphericEmissivity(Air.Temp.hour,cloudiness) # Hourly emissivity
    Longwave.W.hour<-Longwave_Diurnal(Air.Temp.hour,cloudiness,Longwave.Day,Emissivity) # W/m2
    #===========Diurnal Dew point temperature ===============================================
    # Preday<-c(sunrise time,dewpoint) sunrise and dewpoint temperature (degree Celsius) for previous day
    # Present<-c(sunrise time,dewpoint)  for present day
    # Afterday<-c(sunrise time,dewpoint)  for after day
    Present<-as.numeric(Data.Dew.sunrise[iDay,c(2,3)])
    if (iDay==1) {
      Preday<-as.numeric(Data.Dew.sunrise[iDay,c(2,3)])
      Afterday<-as.numeric(Data.Dew.sunrise[(iDay+1),c(2,3)])
    } else if (iDay==Nday) {
      Preday<-as.numeric(Data.Dew.sunrise[(iDay-1),c(2,3)])
      Afterday<-as.numeric(Data.Dew.sunrise[(iDay),c(2,3)])
    } else {
      Preday<-as.numeric(Data.Dew.sunrise[(iDay-1),c(2,3)])
      Afterday<-as.numeric(Data.Dew.sunrise[(iDay+1),c(2,3)])
    }

    Dew.Temp.hour<-Dew.point.Diurnal(Preday,Present,Afterday) # hourly dew point temperature
    #===========Diurnal water vapor ===============================================
    WVP.hour<-SVP.ClaCla(Dew.Temp.hour) # hourly water vapor in hectopascal (hPa)
    SVP.hour<-SVP.ClaCla(Air.Temp.hour) # saturation water vapor at diurnal temperatures in hectopascal (hPa)
    #===========Diurnal atmospheric pressure ===============================================
    VPD.hour= (SVP.hour-WVP.hour)/10 #Vapor pressure deficit (kPa) 1kPa=10hPa
    Atmosphere.Pressure.hour<-Atmp.Pressure.Diurnal(Ele,Air.Temp.hour,VPD.hour) # Atmosphere pressure in hectopascal (hPa)
    #===========Diurnal Relative and specific humidity ===============================================
    RH.hour.Calc<-WVP.hour/SVP.hour          # Relative humidity (ratio)
    Fraction.RH<-RH.hour.Calc/mean(RH.hour.Calc)
    RH.hour<-Fraction.RH*RH.Daily
    RH.hour[RH.hour>1.0]<-0.999
    RH.hour[RH.hour<0.0]<-0.001

    e.hour=WVP.RH.T(RH.hour,Tair=Air.Temp.hour)
    SH.Hour<-SH(e.hour,Atmosphere.Pressure.hour) # hourly specific humidity [kg/kg]
    #===========Diurnal Precipitation and wind speed ===============================================

    # Diurnal Prcpitation
    Upper.Ratio=Upper.bound;Lower.Ratio=Lower.bound

    if(Prcp.day>0){
      Prcp.Hour<-Prcp_Wind_Diurnal(lat,lon,Date,Prcp.day,Upper.Ratio,Lower.Ratio,N.Neighbor=N.Neighbor,
                                   List.ECCC.STN=Prcp.STN.List,Fraction.Folder=Fraction.Folder.Prcp,Fraction.format='xlsx')
    } else {
      Prcp.Hour<-rep(0,24)
    }
    # plot(x=1:24,y=Prcp.Hour,'l',main='Precipitation',xlab='Hour',ylab='mm/hour')
    # Diurnal wind speed

    if(is.na(Wind.day)){
      Wind.Hour<-rep(NA,24)
    } else {
      Wind.Hour<-Prcp_Wind_Diurnal(lat,lon,Date,Wind.day,Upper.Ratio,Lower.Ratio,N.Neighbor=N.Neighbor,
                                   List.ECCC.STN=Wind.STN.List,Fraction.Folder=Fraction.Folder.Wind,Fraction.format='xlsx')
    }
    #Wind.Hour<-Wind.Hour*10^3/3600 #convert km/h to m/sec
    #===============================================================================================
    Prcp.Hour.All<-c(Prcp.Hour.All,Prcp.Hour) # [mm/hour]
    Air.Temp.hour.All<-c(Air.Temp.hour.All,Air.Temp.hour)  # degree Celcius
    Dew.Temp.hour.All<-c(Dew.Temp.hour.All,Dew.Temp.hour)  # degree Celcius
    RH.hour.All<-c(RH.hour.All,RH.hour) # [ratio]
    SH.hour.All<-c(SH.hour.All,SH.Hour) # [ratio]
    Wind.Hour.All<-c(Wind.Hour.All,Wind.Hour) # [m/sec]
    Atmosphere.Pressure.hour.All<-c(Atmosphere.Pressure.hour.All,Atmosphere.Pressure.hour) # [hPa]
    WVP.hour.All<-c(WVP.hour.All,WVP.hour)  # [hPa]
    Solar.W.hour.All<-c(Solar.W.hour.All,Solar.W.hour)  # [W/m2]
    Longwave.W.hour.All<-c(Longwave.W.hour.All,Longwave.W.hour)   # [W/m2]
    Cloudness.Fraction.All<-c(Cloudness.Fraction.All,Cloudness.Fraction.hour)

    } #iDay
  Prcp.Diurnal<-cbind.data.frame(Time.seq.Hourly,Prcp.Hour.All)
  Air.Temp.Diurnal<-cbind.data.frame(Time.seq.Hourly,Air.Temp.hour.All)
  Dew.Temp.Diurnal<-cbind.data.frame(Time.seq.Hourly,Dew.Temp.hour.All)
  RH.Diurnal<-cbind.data.frame(Time.seq.Hourly,RH.hour.All)
  SH.Diurnal<-cbind.data.frame(Time.seq.Hourly,SH.hour.All)
  Wind.Diurnal<-cbind.data.frame(Time.seq.Hourly,Wind.Hour.All)
  Atmosphere.Diurnal<-cbind.data.frame(Time.seq.Hourly,Atmosphere.Pressure.hour.All)
  WVP.Diurnal<-cbind.data.frame(Time.seq.Hourly,WVP.hour.All)
  Solar.Rad.Diurnal<-cbind.data.frame(Time.seq.Hourly,Solar.W.hour.All)
  Long.Rad.Diurnal<-cbind.data.frame(Time.seq.Hourly,Longwave.W.hour.All)
  Cloudness.Diurnal<-cbind.data.frame(Time.seq.Hourly,Cloudness.Fraction.All)


  list(Prcp.Diurnal=Prcp.Diurnal,Air.Temp.Diurnal=Air.Temp.Diurnal,Dew.Temp.Diurnal=Dew.Temp.Diurnal,
         RH.Diurnal=RH.Diurnal,SH.Diurnal=SH.Diurnal,Wind.Diurnal=Wind.Diurnal,
         Atmosphere.Diurnal=Atmosphere.Diurnal,WVP.Diurnal=WVP.Diurnal,Solar.Rad.Diurnal=Solar.Rad.Diurnal,
         Long.Rad.Diurnal=Long.Rad.Diurnal,Cloudness.Diurnal=Cloudness.Diurnal)

  }
