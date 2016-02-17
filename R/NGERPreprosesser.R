#' Preprosesser data fra NGER
#'
#' Denne funksjonen definerer opp sammensatte variabler og fjerner ikke-ferdigstilte registreringer
#'
#' @inheritParams FigAndeler
#'
#' @return Data En list med det filtrerte datasettet og sykehusnavnet som tilsvarer reshID
#'
#' @export
#'
NGERPreprosess <- function(RegData=RegData, reshID=reshID)
{
  RegData <- RegData[RegData$BasisRegStatus==1, ]
  RegData$OpEarlierLaparatomy <- RegData$OpEarlierPaparotomy
  RegData$BirthDate <- as.POSIXlt(RegData$BirthDate, format="%Y-%m-%d")
  RegData$InnDato <- as.POSIXlt(RegData$OpDate, format="%Y-%m-%d")
  RegData$HovedDato <- as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d")
  # RegData$Variabel <- 0	#Fordi LibUtvalg trenger denne variabelen uansett
  names(RegData)[which(names(RegData)=='AVD_RESH')] <- 'ReshId' #Change var name
  RegData$Alder <- as.numeric(floor(difftime(RegData$InnDato, RegData$BirthDate, units='days')/365.25))
  shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$ReshId)])  # Må sjekkes !!!!!!!!!!!!!!!!!!!

  Data <- list(RegData=RegData, shtxt=shtxt)

  return(invisible(Data))
}
