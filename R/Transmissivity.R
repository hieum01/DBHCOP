#' Atmospheric Transmissivity
#'
#' @description
#' Fraction of direct solar radiation passing through the atmosphere
#'
#' @details
#' Atmospheric Transmissivity is estimated by the Bristow-Campbell eqn, AT=A[1-exp(B(Tx-Tn)^C)]
#' B is an empirical coefficient and estimated by 0.036*exp(-0.154*(Tx-Tn))
#' This function is used to estimate solar radiation (refer to 'Solar').
#' @references
#' Bohn, T.J., Livneh, B., Oyler, J.W., Running, S.W., Nijssen, B., Lettenmaier, D.P., 2013. Global evaluation of MTCLIM and related algorithms for forcing of ecological and hydrological models. Agricultural and Forest Meteorology 176, 38–49. https://doi.org/10.1016/j.agrformet.2013.03.003
#' Bristow, K.L., Campbell, G.S., 1984. On the relationship between incoming solar radiation and daily maximum and minimum temperature. Agric. Forest Meteor. 31(2), 159–166.
#' Ndlovu, L.S., 1994. Weather Data Generation and Its Use in Estimating Evapotranspiration. PhD Thesis. Department of Biosystems Engineering (Engineering Science). Washington State University. Pullman, WA. 143pp.
#'
#' @param Tx maximum daily temperature [C]
#' @param Tn minimum daily temperature [C]
#' @param A Coefficient, 0.75
#' @param C Empirical coefficient, 2.4
#' @param opt
#' @param JD
#'
#' @return
#' @export
#'
#' @examples
transmissivity<-function(Tx,Tn, A=0.75, C=2.4, opt="1day", JD=NULL){

  if (any(Tx<Tn)) {print("Warning, Tn larger than Tx and generated NAs in the following rows:")
    print(which(Tx<Tn))
  }
  len<-length(Tx)
  dT <- (Tx-Tn)  # diurnal temperature range just difference between max and min daily temperature
  avDeltaT<-vector(length=len)

  if (opt == "2day"){  ## This is the way Bristow-Campbell originally envisioned it
    for (i in 1:(len-1)){
      dT[i] <- Tx[i] - (Tn[i]+Tn[i+1])/2
    }
  }

  if (opt == "missingdays" & !is.null(JD)){
    for (i in 1:len){
      JDX <- JD[i] + 365
      JDN <- JD[i] - 365
      index1 <- which((JD < (JD[i] + 15) & JD > (JD[i] - 15)) | (JD > (JDX - 15)) |	(JD < (JDN + 15)))
      index2 <- index1[which(index1 > i-14 & index1 < i+15)]
      avDeltaT[i]<-mean(dT[index2])
    }
  } else if(len<30){
    avDeltaT<-mean(dT)
  } else {
    avDeltaT[1:14]<-mean(dT[1:30])
    avDeltaT[(len-14):len]<-mean(dT[(len-30):len])
    for (i in 15:(len-15)){
      avDeltaT[i]<-mean(dT[(i-14):(i+15)])
    }
  }
  B<-0.036*exp(-0.154*avDeltaT)
  return(A*(1-exp(-B*dT^C)))
}

