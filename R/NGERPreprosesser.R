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
  RegData$OpEarlierLaparatomy <- RegData$OpEarlierPaparotomy
  #Riktig format på datovariable:
  RegData$BirthDate <- as.Date(RegData$BirthDate, format="%Y-%m-%d")
  RegData$InnDato <- as.Date(RegData$OpDate, format="%Y-%m-%d")
  RegData$HovedDato <- as.Date(RegData$HovedDato, format="%Y-%m-%d")
  #Riktig navn på resh-variabel:
  names(RegData)[which(names(RegData)=='AvdRESH')] <- 'ReshId' #Change var name
  #Beregner alder:
  RegData$Alder <- as.numeric(floor(difftime(RegData$InnDato, RegData$BirthDate, units='days')/365.25))


  return(invisible(RegData))
}
