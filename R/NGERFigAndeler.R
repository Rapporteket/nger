#' Lag søylediagram eller stabelplott som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram ellersom viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel skal visualiseres
#'     Alder: Pasientens alder, 5-årige aldersgrupper
#'     Education: Pasientens utdanning
#'     LapNumHjelpeinnstikk: Antall hjelpeinnstikk
#'     MaritalStatus: Sivilstand
#'     MCEType: Operasjonsmetode
#'     PatientNorwegian: Pasientens norskkunnskaper
#'     OpBMICategory: BMI-kategori
#'     Opcat: Hastegrad av operasjon
#'     OpEarlierVaginal: Tidligere vaginale inngrep
#'     OpEarlierLaparoscopy: Tidligere laparoskopi
#'     OpEarlierLaparatomy: Tidligere laparatomi
#'     OpOpcatOutsideDaytime: Operasjon i legens vakttid
#'     OpDaySurgery: Dagkirurgiske inngrep
#'     OpType: Primæroperasjon eller reoperasjon
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren).
#' @param minald Alder, fra og med (Standardverdi: 0)
#' @param maxald Alder, til og med (Standardverdi: 130)
#' @param outfile Navn på fil figuren skrives til. Default: '' (Figur skrives
#'    til systemets standard utdataenhet (som regel skjerm))
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Lag figur for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Standard)
#'                 2: Egen enhet
#' @param preprosess Preprosesser data - foreslår vi alltid gjør dette. Ellers må vi ta høyde for det i funksjonen også
#'                 FALSE: Nei
#'                 TRUE: Ja (Standard)
#' @param hentData Gjør spørring mot database
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#' @param MCEType  1: Laparoskopi
#'                 2: Hysteroskopi
#'                 3: Begge
#'                 99: Alle
#'
#' @return En figur med søylediagram (eller et stabelplot) av ønsket variabel
#'
#' @export
#'
FigAndeler  <- function(RegData, valgtVar, datoFra='2013-01-01', datoTil='2050-12-31', minald=0, maxald=130,
                        outfile='', reshID, enhetsUtvalg=1, MCEType=99, hentData=0, preprosess=TRUE)
{
  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERHentRegData(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- NGERPreprosess(RegData=RegData, reshID=reshID)
  }

  ###----------- Figurparametre ------------------------------
  retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
  cexgr <- 1	#Kan endres for enkeltvariable
  grtxt <- ''		#Spesifiseres for hver enkelt variabel
  grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
  subtxt <- ''	#Benevning
  flerevar <- 0
  antDes <- 1

  ###############
  ### Variable
  ###############

  ### Kommentar:
  #Hvis vi definerer kategoriske variable først, kan følgende linjer flyttes ut av def. for kategoriske:
  #		RegData$Variabel <- 99
  #		indVar <- which(RegData[ ,valgtVar] %in% 1:5)	#Må da definere koder <- 1:5 i variabeldef.
  #		RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
  #		RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:5,9), labels = grtxt) #levels=c(nivaa,9)
  ############
  ### Kategoriske variable:
  grtxt <- ''
  koder <- ''

  if (valgtVar == 'Education') {
    # 1:Grunnskole, 2:VG, 3:Fagskole, 4:Universitet<4 år, 5:Universitet>4 år, 6:Ukjent
    Tittel <- 'Utdanningsnivå'
    grtxt <- c('Grunnskole', 'Videregående', 'Fagskole', 'Universitet < 4 år', 'Universitet > 4 år', 'Ukjent')
    koder <- 1:5
    #		RegData$Variabel <- 99
    #		indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder <- 1:5 i variabeldef.
    #		RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    #		RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
    retn <- 'H'
  }

  if (valgtVar == 'MaritalStatus') {
    # 1:Enslig, 2:Særboer, 3:Samboer, 4:Gift, 5:Skilt, 6:Enke, 9:Ukjent
    Tittel <- 'Sivilstatus'
    grtxt <- c('Enslig', 'Særboer', 'Samboer', 'Gift', 'Skilt', 'Enke', 'Ukjent')
    koder <- 1:6
    #		RegData$Variabel <- 9	#For å ta med manglende registreringer i "Ukjent"
    #		indVar <- which(RegData[ ,valgtVar] %in% 1:6)
    #		RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    #		RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:6,9), labels=grtxt)
    retn <- 'H'
  }

  if (valgtVar == 'MCEType') {
    #1:Laparoskopi, 2:Hysteroskopi, 3:Begge
    Tittel <- 'Operasjonsmetode'
    koder <- 1:3
    grtxt <- c('Laparoskopi', 'Hysteroskopi', 'Begge', 'Ukjent')
    #RegData$Variabel <- 9	#0 og NA blir "Ukjent"
    #indVar <- which(RegData[ ,valgtVar] %in% 1:3)
    #RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    #RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
    retn <- 'H'
  }

  if (valgtVar == 'PatientNorwegian') {
    # 0:Nei, 1:Ja, 2:Delvis, 9:Ukjent
    Tittel <- 'Patient forstår og gjør seg forstått på norsk'
    grtxt <- c('Nei', 'Ja', 'Delvis', 'Ukjent')
    koder <- 0:2
    #RegData$Variabel <- 9	#For å ta med manglende registreringer i "Ukjent"
    #indVar <- which(RegData[ ,valgtVar] %in% 0:2)
    #RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    #RegData$VariabelGr <- factor(RegData$Variabel, levels = c(0:2,9), labels = grtxt)
    retn <- 'H'
  }

  if (valgtVar == 'OpBMICategory') {
    # 1:Alvorlig undervekt,2:moderat undervekt, 3:mild undervekt, 4:normal vekt, 5:overvekt,
    # 6:fedme kl.I, 7:fedme kl.II, 8:fedme kl.III
    Tittel <- 'BMI-kategorier, slå sammen noen???'
    grtxt <- c('Alvorlig undervekt','moderat undervekt', 'mild undervekt', 'normal vekt', 'overvekt',
               'fedme kl.I', 'fedme kl.II', 'fedme kl.III', 'Ukjent')
    koder <- 1:8
    #RegData$Variabel <- 9	#For å ta med manglende registreringer i "Ukjent"
    #indVar <- which(RegData[ ,valgtVar] %in% 1:8)
    #RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    #RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:8, labels = grtxt)
    retn <- 'H'
  }

  if (valgtVar == 'Opcat') {
    # 1:Elektiv, 2:Akutt, 3:Øyeblikkelig hjelp
    Tittel <- 'Operasjonskategori'
    grtxt <- c('Elektiv', 'Akutt', 'Ø-hjelp', 'Ukjent')
    koder <- 1:3
    #RegData$Variabel <- 9	#For å ta med manglende registreringer i "Ukjent"
    #indVar <- which(RegData[ ,valgtVar] %in% 1:3)
    #RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    #RegData$VariabelGr <- factor(RegData$Variabel, levels = c(1:3,9), labels = grtxt)
  }
  if (valgtVar %in% c('OpEarlierVaginal', 'OpEarlierLaparoscopy', 'OpEarlierLaparatomy')) {
    # 0: Nei, 1: Ja, 9: Vet ikke
    Tittel <- sprintf('Tidligere %s', switch(as.character(valgtVar),
                                             'OpEarlierVaginal' = 'vaginale inngrep',
                                             'OpEarlierLaparoscopy' = 'laparoskopiske inngrep',
                                             'OpEarlierLaparatomy' = 'laparatomi'))
    grtxt <- c('Nei', 'Ja', 'Vet ikke')
    koder <- 0:1
    #RegData$Variabel <- 9	#For å ta med manglende registreringer i "Vet ikke"
    #indVar <- which(RegData[ ,valgtVar] %in% 0:1)
    #RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    #RegData$VariabelGr <- factor(RegData$Variabel, levels = c(0,1,9), labels = grtxt)
  }


  if (valgtVar %in% c('OpOpcatOutsideDaytime', 'OpDaySurgery')) {
    #0: Nei, 1: Ja
    Tittel <- sprintf('%s', switch(as.character(valgtVar),
                                   'OpOpcatOutsideDaytime' = 'Operasjon i vakttid',
                                   'OpDaySurgery' = 'Dagkirurgiske Inngrep'))
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    koder <- 0:1
    #RegData$Variabel <- 9	#For å ta med manglende som "Ukjent"
    #indVar <- which(RegData[ ,valgtVar] %in% 0:1)
    #RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    #RegData$VariabelGr <- factor(RegData$Variabel, levels = c(0:1,9), labels = grtxt)
  }

  if (valgtVar == 'OpType') {
    # 1:Primærinngrep, 2:Reoperasjon
    Tittel <- 'Operasjonstype'
    grtxt <- c('Primærinngrep', 'Reoperasjon', 'Ukjent')
    koder <- 1:2
    #RegData$Variabel <- 9	#For å ta med manglende som "Ukjent"
    #indVar <- which(RegData[ ,valgtVar] %in% 1:2)
    #RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    #RegData$VariabelGr <- factor(RegData$Variabel, levels = c(1:2,9), labels = grtxt)
  }

  #Likt for alle kategoriske variable TEST ut!!!
  RegData$Variabel <- 99
  indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder <- 1:5 i variabeldef.
  RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
  RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)

  ### Numeriske variable:

  if (valgtVar == 'Alder') {
    Tittel <- 'Aldersfordeling'
    gr <- c(0, seq(15, 80, 5), 120)
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = FALSE)
    grtxt <- c('<15', levels(RegData$VariabelGr)[2:(length(gr)-2)], '80+')
    subtxt <- 'Aldersgrupper'
    retn <- 'H'
  }
  if (valgtVar == 'LapNumHjelpeinnstikk') {
    # Velge antall fra 0 til 6
    Tittel <- 'Antall hjelpeinnstikk'
    grtxt <- 0:6
    RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = grtxt)
  }


  ###Gjør utvalg (LibUtvalg)
  ###Kjører denne etter variabeldefinisjon for at utvalgTxt skal bli riktig
  NGERUtvalg <- NGERLibUtvalg(RegData = RegData, minald = minald, maxald = maxald, datoFra = datoFra,
                              datoTil = datoTil, MCEType = MCEType)
  RegData <- NGERUtvalg$RegData
  utvalgTxt <- NGERUtvalg$utvalgTxt


  shtxt <- switch(as.character(enhetsUtvalg),
                  '0' = 'Hele landet',
                  '1' = as.character(RegData$SykehusNavn[match(reshID, RegData$ReshId)]),
                  '2' = as.character(RegData$SykehusNavn[match(reshID, RegData$ReshId)]))

  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$ReshId == reshID), ]}


  ###--------------- Gjøre beregninger ------------------------------
  #       medSml <- 0
  #       utvalg <- c('Sh', 'Rest')	#Sh vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
  #       Andeler <- list(Sh = 0, Rest =0)#

  ##Hvis det skal gjøres sammenligning:
  #        if (enhetsUtvalg == 1) {
  #            indSh <-which(RegData$ReshId == reshID)
  #            indRest <- which(RegData$ReshId != reshID)
  #            RegDataLand <- RegData
  #            ind <- list(Sh=indSh, Rest=indRest)
  #            medSml <- 1
  #        }

  #Generere hovedgruppe og sammenlikningsgruppe
  #Trenger indeksene før genererer tall for figurer med flere variable med ulike utvalg
  if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$ReshId)])
  } else {shtxt <- 'Hele landet'}

  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    indRest <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg == 1) {
      indHoved <-which(as.numeric(RegData$ReshId)==reshID)
      smltxt <- 'landet forøvrig'
      indRest <- which(as.numeric(RegData$ReshId) != reshID)
    }
  }




  #	for (teller in 1:(medSml+1)) {
  #		if (medSml == 1) {
  #			RegData <- RegDataLand[switch(utvalg[teller], Sh = ind$Sh, Rest=ind$Rest), ]
  #           }
  #        if (teller == 1) {
  #			Andeler$Sh <- 100*table(RegData$VariabelGr)/length(RegData$VariabelGr)
  #           Nsh <- dim(RegData)[1]}
  #      if (teller == 2) {
  #			Andeler$Rest <- 100*table(RegData$VariabelGr)/length(RegData$VariabelGr)
  #           Nrest <- dim(RegData)[1]}
  #    }	#for-løkke

  #Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
  Andeler <- list(Hoved = 0, Rest =0)
  NRest <- 0
  AntRest <- 0

  if (flerevar == 0 ) {
    AntHoved <- table(RegData$VariabelGr[indHoved])
    NHoved <- sum(AntHoved)
    Andeler$Hoved <- 100*AntHoved/NHoved
    if (medSml==1) {
      AntRest <- table(RegData$VariabelGr[indRest])
      NRest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
      Andeler$Rest <- 100*AntRest/NRest
    }
  }




  #FIGURER SATT SAMMEN AV FLERE VARIABLE, ULIKT TOTALUTVALG
  if (valgtVar %in% c('Komorbiditet', 'KomplOpr', 'Kompl3mnd', 'OprIndik', 'OprIndikSmerter',
                      'OprIndikMyelopati', 'Radiologi')){
    flerevar <-  1
    utvalg <- c('Hoved', 'Rest')	#Hoved vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
    RegDataLand <- RegData
    NHoved <-length(indHoved)
    NRest <- length(indRest)

    for (teller in 1:(medSml+1)) {
      #  Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.
      RegData <- RegDataLand[switch(utvalg[teller], Hoved = indHoved, Rest=indRest), ]


      if (valgtVar=='OprIndik') {
        #Fyll inn...
      }

      #Fyll inn


      #Generell beregning for alle figurer med sammensatte variable:
      if (teller == 1) {
        AntHoved <- AntVar
        NHoved <- max(NVar, na.rm=T)
        Andeler$Hoved <- 100*AntVar/NVar
      }
      if (teller == 2) {
        AntRest <- AntVar
        NRest <- max(NVar,na.rm=T)	#length(indRest)- Kan inneholde NA
        Andeler$Rest <- 100*AntVar/NVar
      }
    } #end medSml (med sammenligning)
  }	#end sjekk om figuren inneholder flere variable




  ###-----------Figur---------------------------------------
  ##Hvis for få observasjoner..
  #    if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg == 1)) {
  #      FigTypUt <- figtype(outfile)
  #      farger <- FigTypUt$farger
  #      plot.new()
  #      title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
  #      legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
  #      text(0.5, 0.6, 'Færre enn 5 egne registreringer eller færre 10 totalt', cex=1.2)

  if ( NHoved %in% 1:5 | 	(medSml ==1 & NRest<10)) {
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=Tittel)	#
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 "egne" registreringer eller færre enn 10 totalt', cex=1.2)
    if ( outfile != '') {dev.off()}

  } else {


    ###Innparametre til evt. funksjon: subtxt, grtxt, grtxt2, Tittel, Andeler, utvalgTxt, retn, cexgr
    FigTypUt <- figtype(outfile, fargepalett=NGERUtvalg$fargepalett)
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    antDesTxt <- paste('%.', antDes, 'f', sep='')
    if (length(grtxt2) == 1) {grtxt2 <- paste('(', sprintf(antDesTxt, Andeler$Hoved), '%)', sep='')}
    grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)', sep='')
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	#tykkelse på linja som repr. landet
    cexleg <- 1	#Størrelse på legendtekst

    #Horisontale søyler
    if (retn == 'H') {
      xmax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
      pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=Tittel,
                     col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
      if (NHoved>0) {mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)}

      if (medSml == 1) {
        points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''),
                        paste(smltxt, ' (N=', NRest,')', sep='')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
               lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
      } else {
        legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      }
    }

    if (retn == 'V' ) {
      #Vertikale søyler eller linje
      ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
      pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
                     xlab=subtxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=subtxt,
      mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
      if (medSml == 1) {
        points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
               lwd=lwdRest, ncol=2, cex=cexleg)
      } else {
        legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      }
    }


    title(Tittel, line=1, font.main=1)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    if ( outfile != '') {dev.off()}
  }

  #Beregninger som returneres fra funksjonen.
  AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
  rownames(AndelerUt) <- c('Hoved', 'Rest')
  AntallUt <- rbind(AntHoved, AntRest)
  rownames(AntallUt) <- c('Hoved', 'Rest')

  UtData <- list(paste(toString(Tittel),'.', sep=''), AndelerUt, AntallUt, grtxt )
  names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
  return(invisible(UtData))

}
