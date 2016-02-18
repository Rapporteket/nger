#' Lag tabell med antall registreringer per avdeling
#'
#' Denne funksjonen lager en tabell over antall registreringer per år per avdeling
#'
#' @inheritParams FigAndeler
#'
#' @return Tabell En tabell over antall registreringer per år per avdeling
#'
#' @export


NGERAntallRegPrAvd  <- function(RegData=RegData, datoFra='2014-01-01', datoTil='2050-12-31')

{

  Data <- NGERPreprosess(RegData=RegData, reshID=110734)
  RegData <- Data$RegData
  rm(Data)

  RegData <- RegData[which(RegData$HovedDato >= as.POSIXlt(datoFra) & RegData$HovedDato <= as.POSIXlt(datoTil)),]

  RegData <- RegData[RegData$BasisRegStatus==1, ]
  RegData$OpAar <- as.factor(format(RegData$HovedDato, '%Y'))

  Tabell <- plyr::ddply(RegData[,c('Organisasjon', 'OpAar')], c('Organisasjon', 'OpAar'), nrow)

  Tabell <- reshape2::dcast(Tabell, Organisasjon~OpAar, fill=0)

  return(invisible(Tabell))
}

