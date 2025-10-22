#' Julian Day
#'
#' Converting date(YYYY-MM-DD) to Julian day
#'
#' @param Date.Seq Date (format="%Y-%m-%d") in "Date"
#'
#' @return Julian day
#' @export
#'
#' @examples
#' BD<-as.Date('2000-01-01')
#' ED<-as.Date('2000-12-31')
#' Date.Seq<-as.Date((seq(BD,ED,by="1 day")))
#' Julian_Day(Date.Seq)

Julian_Day<-function(Date.Seq){
  JDay <- strptime(Date.Seq, format="%Y-%m-%d")$yday+1 #Julian day
  return(JDay)
}
