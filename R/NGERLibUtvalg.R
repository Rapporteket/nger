#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams FigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export
#'
NGERLibUtvalg <- function(RegData, datoFra, datoTil, fargepalett='BlaaOff', minald, maxald, MCEType, AlvorlighetKompl)
{
    # Definer intersect-operator
    "%i%" <- intersect

    Ninn <- dim(RegData)[1]

    #Utvalg på alder:
    indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
    #Utvalg på dato:
    indDato <- which(RegData$InnDato >= as.Date(datoFra) & RegData$InnDato <= as.Date(datoTil))
    #Operasjonstype:
    indMCE <- if (MCEType %in% c(1:3)){which(RegData$MCEType == MCEType)
              } else {indMCE <- 1:Ninn}
	indAlvor <- if (AlvorlighetKompl %in% c(1:4)){which(RegData$FollowupSeriousness == AlvorlighetKompl) %i% which(RegData$OppflgRegStatus==2)
              } else {indAlvor <- 1:Ninn}


    #utvalg:
    indMed <- indAld %i% indDato %i% indMCE %i% indAlvor

    RegData <- RegData[indMed,]

    N <- dim(RegData)[1]


    utvalgTxt <- c(paste('Operasjonsdato: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra},
                         ' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}, sep='' ),
                   if ((minald>0) | (maxald<130))
                   {paste('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
                          ' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år', sep='')},
                   if (MCEType %in% c(1:3)){paste('Operasjonsmetode: ', c('Laparoskopi', 'Hysteroskopi', 'Begge')[MCEType], sep='')},
                   if (AlvorlighetKompl %in% c(1:4)){paste('Alvorlighetsgrad: ', c('Liten', 'Middels', 'Alvorlig', 'Dødelig')[AlvorlighetKompl], sep='')})

    UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
    return(invisible(UtData))
}
