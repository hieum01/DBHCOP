#' Declination
#'
#' Estimating solar declination in radian
#'
#' @param Jday Julian date or day of the year [day] (i.e., 1 to 365 or 366)
#'
#' @return Solar declination [rad]
#' @export
#'
#' @examples
declination <-
  function(Jday){

    return(0.4102*sin(pi*(Jday-80)/180))
  }

