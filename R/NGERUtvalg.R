#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams NGERFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export
#'
NGERUtvalg <- function(RegData, datoFra, datoTil, fargepalett='BlaaOff', minald, maxald, MCEType, AlvorlighetKompl, Hastegrad)
{
  # Definer intersect-operator
  "%i%" <- intersect

  Ninn <- dim(RegData)[1]

  #Utvalg på alder:
  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  #Utvalg på dato:
  indDato <- which(RegData$InnDato >= as.Date(datoFra) & RegData$InnDato <= as.Date(datoTil))
  #Operasjonstype:
  indMCE <- if (MCEType %in% c(1:3)){which(RegData$OpMetode == MCEType)
  } else {indMCE <- 1:Ninn}
  #Alvorlighetsgrad, flervalgsutvalg
  indAlvor <- if (AlvorlighetKompl[1] != '') {which(RegData$Opf0AlvorlighetsGrad %in% as.numeric(AlvorlighetKompl)) %i%
      which(RegData$Opf0Status == 1)} else {indAlvor <- 1:Ninn}
  #Hastegrad  1:3 'Elektiv', 'Akutt', 'Ø-hjelp'
  indHastegrad <- if (Hastegrad[1] != '') {which(RegData$OpKategori %in% as.numeric(Hastegrad))
                  } else {indHastegrad <- 1:Ninn}


  #utvalg:
  indMed <- indAld %i% indDato %i% indMCE %i% indAlvor %i% indHastegrad

  RegData <- RegData[indMed,]

  N <- dim(RegData)[1]


  utvalgTxt <- c(paste('Operasjonsdato: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra},
                       ' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}, sep='' ),
                 if ((minald>0) | (maxald<130))
                 {paste('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
                        ' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år', sep='')},
                 if (MCEType %in% c(1:3)){paste('Operasjonsmetode: ', c('Laparoskopi', 'Hysteroskopi', 'Begge')[MCEType], sep='')},
                 if (Hastegrad[1] != ''){paste('Hastegrad: ', paste(c('Elektiv', 'Akutt', 'Ø-hjelp')[as.numeric(Hastegrad)], collapse=','), sep='')},
                 if (AlvorlighetKompl[1] != ''){paste('Alvorlighetsgrad: ', paste(c('Liten', 'Middels', 'Alvorlig', 'Dødelig')
                                                         [as.numeric(AlvorlighetKompl)], collapse=','), sep='')})

  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
  return(invisible(UtData))
}
