#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams NGERFigAndeler
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren).
#' @param minald Alder, fra og med (Standardverdi: 0)
#' @param maxald Alder, til og med (Standardverdi: 130)
#' @param OpMetode  1: Laparoskopi
#'                 2: Hysteroskopi
#'                 3: Begge # 21.okt. 2022: Fjernes som eget valg. Blir med i både laparaskopi og hysteroskopi
#'                 4: LCD01 eller LCD04 (total laparoskopisk hysterektomi)
#'                 5: LCC11 (laparoskopisk subtotal hysterektomi)
#'                 6: LCD11 (laparoskopisk assistert vaginal hysterektomi)
#'                 7: Robotassisert inngrep
#'                 8: Kolpopeksiene
#'                 9: Hysterectomier (alle)
#' @param velgDiag 0: Alle
#'                 1: Ovarialcyster (N83.0, N83.1, N83.2 og D27)
#'                 2: Endometriose, livmorvegg (N80.0)
#'                 3: Endometriose, unntatt livmorvegg.
#' @param Hastegrad Hastegrad av operasjon.
#'                1: Elektiv
#'                2: Akutt
#'                3: Ø-hjelp
#' @param dagkir Dagkirurgi? 0-nei, 1-ja
#' @param AlvorlighetKompl  Alvorlighetsgrad for postoperative komplikasjoner (Flervalg)
#'                          Angis som en vektor av tall som tekst, f.eks. c('1','2')
#'                          1: Lite alvorlig
#'                          2: Middels alvorlig
#'                          3: Alvorlig
#'                          4: Dødelig
#' @param enhetsUtvalg Lag figur for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Standard)
#'                 2: Egen enhet
#' @param velgAvd Velge hvilke avdelinger som skal vises i figurer med avdelingsvise resultater.
#' IKKE tatt høyde for sammenlikning mot "resten".
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og
#' tekststreng som angir fargepalett
#'
#' @export
#'
NGERUtvalgEnh <- function(RegData, datoFra='2016-01-01', datoTil='3000-12-31', fargepalett='BlaaOff',
                          minald=0, maxald=110, OpMetode=0, AlvorlighetKompl=0, Hastegrad=0, dagkir=9,
                          enhetsUtvalg=0, velgAvd=0, velgDiag=0, reshID=0)
{
  # Definer intersect-operator
  "%i%" <- intersect

  #Velge hva som er eget sykehus
  if ((reshID!=0) & (length(velgAvd)==1) & (velgAvd != 0)) {
    reshID <- velgAvd}

  #Velge hvilke sykehus som skal være med:
  if (velgAvd[1] != 0 & reshID==0) {
    #if (enhetsUtvalg !=0) {stop("enhetsUtvalg må være 0 (alle)")}
    #Utvalg på avdelinger:
    RegData <- RegData[which(as.numeric(RegData$ReshId) %in% as.numeric(velgAvd)),]
    RegData$ShNavn <- as.factor(RegData$ShNavn)
  }

  indEgen1 <- match(reshID, RegData$ReshId)
  enhetsUtvalg <- ifelse(reshID==0 | is.na(indEgen1), 0, enhetsUtvalg )

  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$ReshId == reshID), ]}

  hovedgrTxt <- switch(as.character(enhetsUtvalg),
                       '0' = 'Hele landet',
                       '1' = as.character(RegData$ShNavn[indEgen1]),
                       '2' = as.character(RegData$ShNavn[indEgen1]))

  Ninn <- dim(RegData)[1]

  #Utvalg på alder:
  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  #Utvalg på dato:
  indDato <- which(as.Date(RegData$InnDato) >= datoFra & as.Date(RegData$InnDato) <= datoTil)  #as.Date(datoFra)
  #Operasjonstype:
  indMCE <- if (OpMetode %in% c(1:3)){which(RegData$OpMetode %in% c(OpMetode,3))
    } else {indMCE <- 1:Ninn}
  #if (OpMetode == 7) {indMCE <- which(RegData$LapRobotKirurgi == 1)} #ROBOT_KIRURGI==TRUE
  if (OpMetode %in% c(4:6,8,9)) {
      ProsLap <- c('LapProsedyre1', 'LapProsedyre2', 'LapProsedyre3')
      indMCE <- switch(as.character(OpMetode),
              '4' = unique(c(which(RegData[,ProsLap] == 'LCD01', arr.ind = TRUE)[,1],
                                         which(RegData[,ProsLap] == 'LCD04', arr.ind = TRUE)[,1])), #LCD01 + LCD04: total laparoskopisk hysterektomi
              '5' = which(RegData[,ProsLap] == 'LCC11', arr.ind = TRUE)[,1], #LCC11: laparoskopisk subtotal hysterektomi)

              '6' = which(RegData[,ProsLap] == 'LCD11', arr.ind = TRUE)[,1], #LCD11: laparoskopisk assistert vaginal hysterektomi).
              '7' = which(RegData$LapRobotKirurgi == 1), #ROBOT_KIRURGI==TRUE
              '8' = which(RegData[,ProsLap] == 'LEF51', arr.ind = TRUE)[,1], #LCC11: Kolpopeksiene)
      #        '9' = rowSums(RegData[,ProsLap] == "M017" | RegData[,ProsLap] == "M018", na.rm = TRUE) > 0L%in% LCD00, LCD01,LCD04,LCD11, LCC1
              '9' = unique(c(which(RegData$LapProsedyre1 %in% c('LCD00', 'LCD01','LCD04','LCD11', 'LCC11')),
                           which(RegData$LapProsedyre2 %in% c('LCD00', 'LCD01','LCD04','LCD11', 'LCC11')), #Egentlig ikke nødvendig å sjekke pros2 og 3
                           which(RegData$LapProsedyre3 %in% c('LCD00', 'LCD01','LCD04','LCD11', 'LCC11'))))
      )
  }


if (velgDiag !=0) {
  indDiag <- NULL
  diagTxt <- c('Godartede ovarialcyster', 'Endometriose, livmorvegg', 'Endometriose utenom livmorvegg',
               'Onkologi', 'Generell gynekologi', 'Gravide', 'Komplikasjoner', 'Infertilitet')
  DiagVar <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3', 'HysDiagnose1','HysDiagnose2', 'HysDiagnose3')
  if (velgDiag ==1) {
    koder <- c('N830', 'N831', 'N832', 'D27')
    for (var in DiagVar) {indDiag <- union(indDiag, grep(paste(koder, collapse = "|"), RegData[ ,var]))}  #(Se også på pmatch, carmatch
  }
  if (velgDiag==2) { #Endometriose , livmorvegg
    koder <- 'N800'
    for (var in DiagVar) {indDiag <- union(indDiag, grep(koder, RegData[ ,var]))}
  }
	if (velgDiag == 3) {#endometriose, «u/livmorvegg»: N80.1-N80.9
	  koder <- paste0('N80', 1:9)
    for (var in DiagVar) {indDiag <- union(indDiag, grep(paste(koder, collapse = "|"), RegData[ ,var]))}  #(Se også på pmatch, carmatch
	}
  if (velgDiag == 4){#Onkologi C52	C53	C54	C55	C56	C57	C58	C76	C77	C79.6
    koder <- c(paste0('C5', 2:8), 'C76',	'C77',	'C79.6')
    for (var in DiagVar) {indDiag <- union(indDiag, grep(paste(koder, collapse = "|"), RegData[ ,var]))}  #(Se også på pmatch, carmatch
  }
  if (velgDiag == 5){#Generell gynekologi	D25	D26	D27	D28	D39	N70	N71	N72	N73	N74	N80	N81	N82	N83	N84	N85	N86	N87	N88	N91	N92	N93	N94	N95	N96	N97	N98	N99	R10
    koder <- c(paste0('D', c(25:28, 39)), paste0('N7',0:4), paste0('N8', 0:8), paste0('N9', 1:9), 'R10', 'Q50',	'Q51',	'Q52')
    for (var in DiagVar) {indDiag <- union(indDiag, grep(paste(koder, collapse = "|"), RegData[ ,var]))}
  }
  if (velgDiag == 6){#Gravide	O00	O01	O02	O03	O04	O05	O07	Q50	Q51	Q52
    koder <- paste0('O0', c(0:5, 7)) #, 'Q50',	'Q51',	'Q52')
    for (var in DiagVar) {indDiag <- union(indDiag, grep(paste(koder, collapse = "|"), RegData[ ,var]))}
  }
  if (velgDiag == 7){ # Komplikasjoner	T81	T83
    koder <- c('T81',	'T83')
    for (var in DiagVar) {indDiag <- union(indDiag, grep(paste(koder, collapse = "|"), RegData[ ,var]))}
  }
  if (velgDiag == 8){ # Infertilitet	N96	N97	N98
    koder <- c('N96',	'N97',	'N98')
    for (var in DiagVar) {indDiag <- union(indDiag, grep(paste(koder, collapse = "|"), RegData[ ,var]))}
  }
} else {  indDiag <- 1:Ninn}

  #Alvorlighetsgrad, flervalgsutvalg
  indAlvor <- if (AlvorlighetKompl[1] %in% 1:4) {
    which(RegData$Opf0AlvorlighetsGrad %in% as.numeric(AlvorlighetKompl)) %i%
      which(RegData$Opf0Status == 1)} else {indAlvor <- 1:Ninn}
  #Hastegrad  1:3 'Elektiv', 'Akutt', 'Ø-hjelp'
  indHastegrad <- if (Hastegrad[1] %in% 1:3) {which(RegData$OpKategori %in% as.numeric(Hastegrad))
                  } else {indHastegrad <- 1:Ninn}
  #Dagkirurgi 0-nei, 1-ja
  indDagkir <- if (dagkir %in% 0:1) {which(RegData$OpDagkirurgi == as.numeric(dagkir))
  } else {indDagkir <- 1:Ninn}


  #utvalg:
  indMed <- indAld %i% indDato %i% indMCE %i% indAlvor %i% indHastegrad %i% indDiag %i% indDagkir

  RegData <- RegData[indMed,]

  N <- dim(RegData)[1]


  utvalgTxt <- c(paste0('Operasjonsdato: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra},
                       ' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}),
                 if ((minald>0) | (maxald<110))
                    {paste0('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
                        ' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
                 if (OpMetode %in% c(1:9)){paste0('Operasjonstype: ',
                                                c('Laparoskopi', 'Hysteroskopi', 'Begge',
                                                  'Tot. lap. hysterektomi (LCD01/LCD04)',
                                                  'Lap. subtotal hysterektomi (LCC11)',
                                                  'Lap. ass. vag. hysterektomi (LCD11)',
                                                  'Robotassisert inngrep',
                                                  'Kolpopeksiene',
                                                  'Hysterektomier')[OpMetode])},
                 if (Hastegrad[1] %in% 1:3){
                   paste0('Hastegrad: ',
                          paste0(c('Elektiv', 'Akutt', 'Ø-hjelp')[as.numeric(Hastegrad)], collapse=','))},
                 if (dagkir %in% 0:1){c('Ikke dagkirurgi', 'Dagkirurgi')[as.numeric(dagkir)+1]},
                 if (AlvorlighetKompl[1] %in% 1:4){
                   paste0('Alvorlighetsgrad: ', paste(c('Liten', 'Middels', 'Alvorlig', 'Dødelig')
                                                         [as.numeric(AlvorlighetKompl)], collapse=','))},
                 if (velgDiag != 0) {paste0('Diagnose: ', diagTxt[velgDiag])},
                 if (velgAvd[1] != 0 & reshID==0) {'Viser valgte sykehus'})
  #Generere hovedgruppe og sammenlikningsgruppe
  #Trenger indeksene før genererer tall for figurer med flere variable med ulike utvalg
  if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
    hovedgrTxt <- as.character(RegData$ShNavn[match(reshID, RegData$ReshId)])}
  if ((velgAvd[1] != 0) & (reshID==0)) {hovedgrTxt <-'Valgte sykehus'}


    ind <- list(Hoved=0, Rest=0)
	if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
	  smltxt <- 'Ingen sml'
    ind$Hoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    ind$Rest <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg == 1) {
      ind$Hoved <-which(as.numeric(RegData$ReshId) == reshID)
      smltxt <- 'Landet forøvrig'
      ind$Rest <- which(as.numeric(RegData$ReshId) != reshID)
    }
  }

  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett,
				ind=ind, medSml=medSml, hovedgrTxt=hovedgrTxt, smltxt=smltxt)
  return(invisible(UtData))
}
