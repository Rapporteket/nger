#' Preprosesser data fra NGER
#'
#' Denne funksjonen definerer variabler og fjerner ikke-ferdigstilte registreringer
#'
#' @inheritParams NGERFigAndeler
#'
#' @return RegData En data.frame med det preprosesserte datasettet
#'
#' @export
#'
NGERPreprosess <- function(RegData=RegData)
{
  #Kun ferdigstilte registreringer:
  RegData <- RegData[RegData$BasisRegStatus==1, ]

  #For bedre lesbarhet:
  RegData$OpEarlierLaparatomy <- RegData$OpTidlLaparotomi

  #Riktig format på datovariable:
  #RegData$FodselsDato <- as.Date(RegData$FodselsDato, format="%Y-%m-%d")
  RegData$InnDato <- as.Date(RegData$OpDato, format="%Y-%m-%d")
  RegData$HovedDato <- as.Date(RegData$HovedDato, format="%Y-%m-%d")

  #Riktig navn på resh-variabel:
  names(RegData)[which(names(RegData)=='AvdRESH')] <- 'ReshId' #Change var name
  names(RegData)[which(names(RegData)=='PasientAlder')] <- 'Alder' #Change var name


  return(invisible(RegData))
}
