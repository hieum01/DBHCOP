#' Hourly Fraction
#'
#' Calculating hourly fraction of pecipitation and wind speed from hourly climate data
#'
#' The hourly fractions are employed into 'Disaggregation' function to disaggre daily to hourly for precipitation and wind speed.
#' Hourly Fraction generates a fraction file named by 'STN.name'_'STN.ID'_Hourly_Fraction_'Var.Name'.csv (or xlsx).
#'
#' @param STN.name Station name in character
#' @param STN.ID Station ID in character
#' @param Hourly.Data Hourly data (time and data), time format='YYYY-MM-DD hh:mm:ss'. Please refer to 'Hourly.Data.Sample.Prcp'
#' @param Var.Name Variable name ('Prcp' or 'Wind')
#' @param Dir.Output Output folder to store hourly fraction file generated
#' @param Output.format Output format: csv or xlsx (default='csv')
#'
#' @return Fraction.Var.calc.avail
#' @export
#'
#' @examples
#' data(Hourly.Data.Sample.Wind)
#' STN.name="WILLOW CREEK 1"
#' STN.ID="306GE70"
#' Hourly.Data=Hourly.Data.Sample.Wind
#' Var.Name='Wind'
#' Dir.Output='Ouput'
#' Output.format='csv'
#' Fraction(STN.name,STN.ID,Hourly.Data,Var.Name,Dir.Output,Output.format)
Fraction<-function(STN.name,STN.ID,Hourly.Data,Var.Name='Prcp',Dir.Output,Output.format='csv') {
  Date.Data.Hourly<-as.Date(Hourly.Data[,1])
  Date.First<-as.Date(Hourly.Data[1,1])
  Date.End<-as.Date(Hourly.Data[nrow(Hourly.Data),1])
  Date.STN<-as.Date((seq(Date.First,Date.End,by="1 day")))
  N.Day<-length(Date.STN)

  Fraction.Var.calc<-array(NA,dim=c(N.Day,25)) #hourly

  for (iDay in 1:N.Day) {
    Date.Cur<-Date.STN[iDay]
    print(Date.Cur)
    Hourly.data.day<-Hourly.Data[which(Date.Data.Hourly==Date.Cur),2]

    #Wind.data[which(Wind.data==0)]<-NA  # Set zero Wind speed to NA
    # Counting times when hourly precipitation/wind speed is available in a day
    Not.NA.Data<-length(which(!is.na(Hourly.data.day)))

    if(Var.Name=='Prcp') {
      if(Not.NA.Data > 12) {
        Sum.Day<-sum(Hourly.data.day,na.rm = T)
        if(Total.prcp>0){
          Fraction.Cal<-Hourly.data.day/Sum.Day
          Fraction.Cal[is.na(Fraction.Cal)]<-0
          Fraction.Var.calc[iDay,1:24]<-Fraction.Cal
          Fraction.Var.calc[iDay,25]<-Sum.Day
        }
      }
    } else {
      if(Not.NA.Data > 12) {
        mean.Day<-mean(Hourly.data.day,na.rm = T)
        Fraction.Cal<-Hourly.data.day/mean.Day
        Fraction.Cal[is.na(Fraction.Cal)]<-1
        Fraction.Var.calc[iDay,1:24]<-Fraction.Cal
        Fraction.Var.calc[iDay,25]<-mean.Day
      }
    }

  } # N.Day

  Fraction.Var.calc<-cbind.data.frame(Date.STN,Fraction.Var.calc)
  #=====================================================================================
  # Extracting Date and fraction on days when hourly fraction and ratio are calculated
  #=====================================================================================
  Fraction.Var.calc.avail<-Fraction.Var.calc[which(!is.na(Fraction.Var.calc[,2])),]
  if(Var.Name=='Prcp') {
    colnames(Fraction.Var.calc.avail)<-c('Date',paste0(seq(1,24),'hr'),'Total Prcp')
  } else {
    colnames(Fraction.Var.calc.avail)<-c('Date',paste0(seq(1,24),'hr'),'WS_Ave')
  }
  if(Output.format=='csv') {
    write.csv(Fraction.Var.calc.avail,file=paste0(Dir.Output,'/',STN.name,'_',STN.ID,'_Hourly_Fraction_',Var.Name,'.csv'),row.names = F)
  } else {
    write.xlsx(Fraction.Var.calc.avail,file=paste0(Dir.Output,'/',STN.name,'_',STN.ID,'_Hourly_Fraction_',Var.Name,'.xlsx'))
  }

  #=====================================================================================
  return(Fraction.Var.calc.avail=Fraction.Var.calc.avail)
}
