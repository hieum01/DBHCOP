#===================================================================================================================
#' @title
#' Disaggregation of daily to hourly
#' @description
#' Disaggregating daily to hourly for precipiation and wind using hourly fraction of precipitation drived from ECCC stations
#' @details
#' Hourly fraction files are named by Station name and ID described in List.ECCC.STN.
#' If station name and ID are 'ABEE AGDM' and '3010010', for example, hourly fraction file name should be 'ABEE AGDM_3010010_Hourly_Fraction_Prcp.xlsx'
#' The format of List.ECCC.STN and hourly fraction is available in this package by data('List.ECCC.STN.Prcp')
#' @author
#' Hyung Eum (hieum01@gov.ab.ca)
#'
#' @param lat Latitude in degree
#' @param lon Longitude in degree
#' @param Date Current date, format("YYYY-MM-DD")
#' @param Var.Day Daily precipitation(mm/day) or wind speed(m/s)
#' @param Upper.Ratio Upper bound ratio to select hourly fractions from nearest neighbor stations
#' @param Lower.Ratio Lower bound ratio to select hourly fractions from nearest neighbor stations
#' @param N.Neighbor Number of nearest neighbor climate stations (default=3)
#' @param List.ECCC.STN List of ECCC stations that hourly data is available
#' @param Fraction.Folder Location (Folder path) where hourly fraction files are located
#' @param Fraction.format File format in character: xlsx or csv (default=xlsx)

Prcp_Wind_Diurnal<-function(lat, lon, Date,Var.Day,Upper.Ratio=1.2,Lower.Ratio=0.8,N.Neighbor=3,List.ECCC.STN,Fraction.Folder,Fraction.format='xlsx'){

  Date<-as.Date(Date)
  Month<-as.numeric(format(Date,"%m")) # Current month
  #pic.data<-as.numeric(format(Fraction.Wind.avail[,1],"%m"))==Month
  Lat.Lon.ECCC<-cbind(List.ECCC.STN$Lon,List.ECCC.STN$Lat)

  lon.lat.Grid <- matrix(c(lon, lat), nrow=1)

  #======================================================================================
  # Finding the nearest ECCC station
  Distance<-distm(lon.lat.Grid, Lat.Lon.ECCC) # Distance in meter between two points
  Distance.list<-cbind(seq(1,length(Distance),1),Distance[1,])
  Distance.list.Sort<-Distance.list[order(Distance.list[,2],decreasing = F),]

  #STN.Selected<-List.ECCC.STN[which(Distance==min(Distance)),]
  STN.Selected<-List.ECCC.STN[Distance.list.Sort[1:N.Neighbor,1],]
  #======================================================================================
  Climate_Name<-STN.Selected$STN.Name
  Climate.ID<-STN.Selected$ID

  flist<-sapply(1:N.Neighbor, function(i){
    list.files(Fraction.Folder, pattern = glob2rx(paste0("*",Climate_Name[i],"*",Climate.ID[i],"*")), full.names = F)
  })

  Fraction.Data<-NULL
  if (Fraction.format=='xlsx') {
    for(iFile in 1:N.Neighbor) {
      Fraction.file<-read.xlsx(paste0(Fraction.Folder,'/',flist[iFile]),detectDates =T)
      Fraction.Data<-rbind.data.frame(Fraction.Data,Fraction.file)
    }

  } else {
    for(iFile in 1:N.Neighbor) {
      Fraction.file<-read.csv(paste0(Fraction.Folder,'/',flist))
      Fraction.Data<-rbind.data.frame(Fraction.Data,Fraction.file)
    }
  }

  if(Month==12) {
    pic.data<-as.numeric(format(Fraction.Data[,1],"%m"))==Month | as.numeric(format(Fraction.Data[,1],"%m"))==1 | as.numeric(format(Fraction.Data[,1],"%m"))==2
  } else if (Month==1) {
    pic.data<-as.numeric(format(Fraction.Data[,1],"%m"))==Month | as.numeric(format(Fraction.Data[,1],"%m"))==12 | as.numeric(format(Fraction.Data[,1],"%m"))==Month+1
  } else {
    pic.data<-as.numeric(format(Fraction.Data[,1],"%m"))==Month | as.numeric(format(Fraction.Data[,1],"%m"))==Month+1 | as.numeric(format(Fraction.Data[,1],"%m"))==Month-1
  }


  Fraction.Candidates<-Fraction.Data[pic.data,]

  #Diff.Var.Day<-abs(Var.Day-Fraction.Candidates[,26])
  # Upper and Lower bounds of a daily value
  Var.Upper.bound<-Var.Day*Upper.Ratio
  Var.Lower.bound<-Var.Day*Lower.Ratio
  #======================================================================================
  # Random selection from the nearest neighbors
  Loc.Neighbors<-which(Fraction.Candidates[,ncol(Fraction.Candidates)]>=Var.Lower.bound & Fraction.Candidates[,ncol(Fraction.Candidates)]<=Var.Upper.bound)
  Fraction.Candidates.Bound<-Fraction.Candidates[Loc.Neighbors,]
  if(length(which(Fraction.Candidates.Bound$Date==Date))>0){  # hourly fraction on the same date with target date
    Loc.Fraction<-which(Fraction.Candidates.Bound$Date==Date)
    Fration.selected<-Fraction.Candidates.Bound[Loc.Fraction[1],2:25]
  } else {
    N.Fraction.sample<-nrow(Fraction.Candidates.Bound)
    Selected.hourly.fraction<-floor(runif(1, min=1, max=N.Fraction.sample))
    # Fraction or ratio selected
    Fration.selected<-Fraction.Candidates.Bound[Selected.hourly.fraction,2:25]
  }

  #======================================================================================
  Var.Diurnal<-as.numeric(Fration.selected*Var.Day)

  return(Var.Diurnal=Var.Diurnal)
}

