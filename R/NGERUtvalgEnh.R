#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams NGERFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export
#'
NGERUtvalgEnh <- function(RegData, datoFra, datoTil, fargepalett='BlaaOff', minald=0, maxald=130,
                       MCEType='', AlvorlighetKompl='', Hastegrad='')
{
  # Definer intersect-operator
  "%i%" <- intersect


 hovedgrTxt <- switch(as.character(enhetsUtvalg),
                  '0' = 'Hele landet',
                  '1' = as.character(RegData$ShNavn[match(reshID, RegData$ReshId)]),
                  '2' = as.character(RegData$ShNavn[match(reshID, RegData$ReshId)]))

  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$ReshId == reshID), ]}


  Ninn <- dim(RegData)[1]

  #Utvalg på alder:
  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  #Utvalg på dato:
  indDato <- which(RegData$InnDato >= as.Date(datoFra) & RegData$InnDato <= as.Date(datoTil))
  #Operasjonstype:
  indMCE <- if (MCEType %in% c(1:3)){which(RegData$OpMetode %in% c(MCEType,3))
    } else {indMCE <- 1:Ninn}
  if (MCEType %in% 4:6) {
      ProsLap <- c('LapProsedyre1', 'LapProsedyre2', 'LapProsedyre3')
      RegData$LapProsedyre1 <- toupper(RegData$LapProsedyre1)
      RegData$LapProsedyre2 <- toupper(RegData$LapProsedyre2)
      RegData$LapProsedyre3 <- toupper(RegData$LapProsedyre3)
      indMCE <- switch(as.character(MCEType),
              '4' = unique(c(which(RegData[,ProsLap] == 'LCD01', arr.ind = TRUE)[,1],
                                         which(RegData[,ProsLap] == 'LCD04', arr.ind = TRUE)[,1])), #LCD01 + LCD04: total laparoskopisk hysterektomi
              '5' = which(RegData[,ProsLap] == 'LCC11', arr.ind = TRUE)[,1], #LCC11: laparoskopisk subtotal hysterektomi)

              '6' = which(RegData[,ProsLap] == 'LCD11', arr.ind = TRUE)[,1] #LCD11: laparoskopisk assistert vaginal hysterektomi).
      )
  }


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


  utvalgTxt <- c(paste0('Operasjonsdato: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra},
                       ' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}),
                 if ((minald>0) | (maxald<130))
                    {paste0('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
                        ' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
                 if (MCEType %in% c(1:6)){paste0('Operasjonsmetode: ',
                                                c('Laparoskopi', 'Hysteroskopi', 'Begge',
                                                  'Tot. lap. hysterektomi (LCD01/LCD04)',
                                                  'Lap. subtotal hysterektomi (LCC11)',
                                                  'Lap. ass. vag. hysterektomi (LCD11)')[MCEType])},
                 if (Hastegrad[1] != ''){paste0('Hastegrad: ', paste0(c('Elektiv', 'Akutt', 'Ø-hjelp')[as.numeric(Hastegrad)], collapse=','))},
                 if (AlvorlighetKompl[1] != ''){paste0('Alvorlighetsgrad: ', paste(c('Liten', 'Middels', 'Alvorlig', 'Dødelig')
                                                         [as.numeric(AlvorlighetKompl)], collapse=','))})
  #Generere hovedgruppe og sammenlikningsgruppe
  #Trenger indeksene før genererer tall for figurer med flere variable med ulike utvalg
  if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
    hovedgrTxt <- as.character(RegData$ShNavn[match(reshID, RegData$ReshId)])
  } else {hovedgrTxt <- 'Hele landet'}

    ind <- list(Hoved=0, Rest=0)
	if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
	smltxt <- 'Ingen sml'
    ind$Hoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    ind$Rest <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg == 1) {
      ind$Hoved <-which(as.numeric(RegData$ReshId)==reshID)
      smltxt <- 'Landet forøvrig'
      ind$Rest <- which(as.numeric(RegData$ReshId) != reshID)
    }
  }



  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett,
				ind=ind, medSml=medSml, hovedgrTxt=hovedgrTxt, smltxt=smltxt)
  return(invisible(UtData))
}