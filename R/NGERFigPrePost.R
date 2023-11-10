#' Søylediagram som viser resultat av valgt variabel, målt ved tre tidspunkter
#'
#' Funksjon som genererer en figur med som viser endring i en variabels fordeling ved tre ulike tidspunkter.
#' Man kan velge om det skal vises andeler eller antall
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams NGERFigAndeler
#' @param valgtVar Hvilken variabel som skal visualiseres
#'
#' @return Søylediagram som fordeling av valgt variabel, ved operasjon, samt 1 og 3 år etter.
#'
#' @export
NGERFigPrePost  <- function(RegData, valgtVar='ScoreGeneral', datoFra='2019-01-01', datoTil=Sys.Date(),
                            enhetsUtvalg = 0, reshID = 0, velgAvd=0,
                            minald=0, maxald=130, OpMetode=99, velgDiag=0,
                            AlvorlighetKompl = 0, Hastegrad=99, dagkir=9,
                            Ngrense=10, outfile='', preprosess=0, hentData=0,...)
{

  if (hentData == 1) {
    RegData <- NGERRegDataSQL(datoFra, datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert.
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  RANDvar <- c('ScorePhys',	'ScoreRoleLmtPhy',
               'ScoreRoleLmtEmo', 'ScoreEnergy',
               'ScoreEmo', 'ScoreSosial',
               'ScorePain',	'ScoreGeneral')
                #Benytter denne til å filtrere
  TittelAlle <- c('Fysisk funksjon',	'Fysisk rollebegrensning',
                  'Følelsesmessig rollebegrensning', 'Energinivå/vitalitet',
                  'Mental helse', 'Sosial funksjon',
                  'Smerte',	'Generell helsetilstand',
                  'RAND, alle dimensjoner')

  varNr <- match(valgtVar, c(RANDvar, 'AlleRANDdim'))
  PrePostVar <- paste0(c('R0', 'R1', 'R3'), c(RANDvar, 'Metode')[varNr])
  RegData$VarPre <- RegData[ ,PrePostVar[1]]
  RegData$VarPost1 <- RegData[ ,PrePostVar[2]]
  RegData$VarPost3 <- RegData[ ,PrePostVar[3]] #!Endres
  #Tar ut de med manglende registrering av valgt variabel og gjør utvalg
  indSvar <- which(!(is.na(RegData$VarPre) | is.na(RegData$VarPost1) | is.na(RegData$VarPost3))) #
  RegData <- RegData[indSvar, ]

  tittel <- TittelAlle[varNr]
  retn <- c(rep('V',8),'H')[varNr]

  NGERUtvalg <- NGERUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                              enhetsUtvalg = enhetsUtvalg, reshID = reshID, velgAvd=velgAvd,
                              minald=minald, maxald=maxald, OpMetode=OpMetode, velgDiag=velgDiag,
                              Hastegrad = Hastegrad, dagkir = dagkir, AlvorlighetKompl = AlvorlighetKompl)
  RegData <- NGERUtvalg$RegData
  utvalgTxt <- NGERUtvalg$utvalgTxt


  #-----------Definisjoner----------------------------
  cexgr <- 1
  txtretn <- 1
  grtxt <- ''
  subtxt <- ''

  #---------------BEREGNINGER --------------------------

  if (valgtVar == 'AlleRANDdim') {
    GjsnPre <-colMeans(RegData[ , paste0('R0',RANDvar)], na.rm=T)
    Gjsn1aar <- colMeans(RegData[ , paste0('R1',RANDvar)], na.rm=T)
    Gjsn3aar <-colMeans(RegData[ , paste0('R3',RANDvar)], na.rm=T)
    N <- dim(RegData)[1]
    GjsnPP <- cbind(GjsnPre, Gjsn1aar, Gjsn3aar)
    grtxt <- c('Fysisk funksjon',	'Fysisk\n rollebegrensning',
                          'Følelsesmessig\n rollebegrensning', 'Energinivå/vitalitet',
                          'Mental helse', 'Sosial funksjon',
                          'Smerte',	'Generell\n helsetilstand')
    rownames(GjsnPP) <- grtxt
    tittel <- c(tittel, NGERUtvalg$hovedgrTxt)

  } else {
    GjsnPre <-tapply(RegData$VarPre, RegData$ShNavn, FUN = 'mean', na.rm=T)
    Gjsn1aar <- tapply(RegData$VarPost1, RegData$ShNavn, FUN = 'mean', na.rm=T)
    Gjsn3aar <- tapply(RegData$VarPost3, RegData$ShNavn, FUN = 'mean', na.rm=T)
    Ngr <- table(RegData$ShNavn)
    N <- sum(Ngr)
    GjsnPP <- cbind(GjsnPre, Gjsn1aar, Gjsn3aar)
  }

  #-----------Figur---------------------------------------
  FigTypUt <- rapFigurer::figtype(outfile)
  farger <- FigTypUt$farger
  NutvTxt <- length(utvalgTxt)
  vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

  if (dim(RegData)[1] < 10 ) {
    title(main=tittel)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {

    #-----------Figur---------------------------------------

    if (retn == 'V' ) { #Benytter denne til å vise ulike grupper av en variabel
      #Vertikale søyler eller linje
      ymax <- min(max(c(GjsnPP),na.rm=T)*1.25, 110)
      pos <- barplot(t(GjsnPP), beside=TRUE, horiz=FALSE, las=txtretn, ylab="",
                     sub='Gjennomsnittlig sumskår (høyest er best)',
                     cex.names=cexgr, col=farger[1:3], border='white', ylim=c(0, ymax))
    }

    if (retn == 'H') { #Benytte denne til å flere variabler samtidig
      #Horisontale søyler
      xmax <- min(max(GjsnPP,na.rm=T)*1.25, 100)
      pos <- barplot(t(GjsnPP), beside=TRUE, horiz=TRUE, main='', las=1,
                     col=farger[1:3], border='white', font.main=1,  xlim=c(0,xmax),
                     cex.names=cexgr, xlab='Gjennomsnittlig sumskår (høyest er best)')
      # legend('top', c('Før', 'Etter',paste0('N=',N)), bty='n',
      #        fill=farger[c(1:3,NA)], border=NA, ncol=3, cex=0.9)
    }

    legend('top', y.intersp=0, bty='n',
           c('perop.', '1 år', '3 år', paste0('N=', N)),
           fill=farger[c(1:3,NA)], border=NA, ncol=4, cex=0.9)

    title(tittel, font.main=1)	#line=0.5,
    #Tekst som angir hvilket utvalg som er gjort
    utvpos <- 3+length(tittel)-1	#Startlinje for teksten
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

  }
}
