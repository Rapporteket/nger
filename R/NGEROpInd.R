#' Genererer figur over de 20 vanligste operasjonsindikatorene.
#'
#' Denne funksjonen lager en figur over de 20 vanligste operasjonsindikatorene.
#' I tellingen slås variablene OpInd1, OpInd2 og OpInd3 sammen.
#'
#' @inheritParams NGERFigAndeler
#'
#' @return Tabell med de 20 vanligste operasjonsindikatorene
#'
#' @export
#'
NGEROpInd <- function(RegData=0, datoFra='2000-04-01', datoTil='2050-12-31', minald=0, maxald=130, outfile='',
                     reshID, enhetsUtvalg=1, MCEType=99, preprosess=0, hentData=0)
{
  ## Hvis spørring skjer fra R på server. ######################
  if(hentData==1){
    RegData <- NGERRegDataSQL(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  RegData$Variabel <- RegData$OpInd1
  NGERUtvalg <- NGERLibUtvalg(RegData = RegData, minald = minald, maxald = maxald, datoFra = datoFra, datoTil = datoTil, MCEType = MCEType)
  RegData <- NGERUtvalg$RegData
  utvalgTxt <- NGERUtvalg$utvalgTxt


  AlleOpInd <- unique(c(unique(toupper(as.character(RegData$OpInd1))),
                        unique(toupper(as.character(RegData$OpInd2))),
                        unique(toupper(as.character(RegData$OpInd3)))))

  aux <- apply(apply(RegData[, c('OpInd1', 'OpInd2', 'OpInd3')], 2, function(x){
    return(as.character(toupper(x)))}), 2, factor, levels = sort(AlleOpInd))
  Tabell <- table(aux)
  Tabell <- sort(Tabell[-which(names(Tabell)=='')], decreasing = TRUE)[1:20]

  return(invisible(Tabell))
}
