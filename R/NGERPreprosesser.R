#' Preprosesser data fra NGER
#'
#' Denne funksjonen definerer variabler og fjerner ikke-ferdigstilte registreringer
#'
#' @inheritParams FigAndeler
#'
#' @return Data En list med det filtrerte datasettet og sykehusnavnet som tilsvarer reshID
#'
#' @export
#'
NGERPreprosess <- function(RegData=RegData, reshID=reshID)
{
#Kun ferdigstilte registreringer:
  RegData <- RegData[RegData$BasisRegStatus==1, ]
#For bedre lesbarhet:
  RegData$OpEarlierLaparatomy <- RegData$OpEarlierPaparotomy
#Riktig format på datovariable:
  RegData$BirthDate <- as.POSIXlt(RegData$BirthDate, format="%Y-%m-%d")
  RegData$InnDato <- as.POSIXlt(RegData$OpDate, format="%Y-%m-%d")
  RegData$HovedDato <- as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d")
#Riktig navn på resh-variabel:
  names(RegData)[which(names(RegData)=='AVD_RESH')] <- 'ReshId' #Change var name
#Beregner alder:
  RegData$Alder <- as.numeric(floor(difftime(RegData$InnDato, RegData$BirthDate, units='days')/365.25))


  return(invisible(RegData))
}
