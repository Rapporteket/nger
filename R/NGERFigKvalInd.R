#' S?ylediagram som viser flere kvalitetsindikatorer
#'
#' Funksjon som genererer en figurer med ei gruppe kvalitetsindikatorer
#'
#' Detajer: Her b?r man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams NGERFigAndeler
#' @inheritParams NGERFigAndelerGrVar
#' @inheritParams NGERUtvalgEnh
#'
#' Argumentet \emph{valgtVar} har f?lgende valgmuligheter:
#'    \itemize{
#'     \item RAND0: Alle dimensjonene i RAND36 ved oppf?lging etter 4-6uker. Gjennomsnitt
#'     \item TSS20: Alle sp?rsm?lene i TSS2 ved oppf?lging etter 4-6uker. Andel av beste svaralternativ
#'     \item kvalInd: Samling av kvalitetsindikatorer
#'    }
#'
#' Detajer: Her b?r man liste opp hvilke variable funksjonen benytter.
#'
#' @return S?ylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export


NGERFigGjsnGrVar <- function(RegData, datoFra='2013-01-01', datoTil='3000-12-31',
                             valgtVar, minald=0, maxald=130, MCEType=99, Hastegrad=99,
                             hentData=0, preprosess=1, velgDiag=0, Ngrense=10,outfile='') {

  if (hentData == 1) {
    RegData <- NGERRegDataSQL(datoFra, datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert. (I samledokument gj?res dette i samledokumentet)
  if (preprosess == 1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  '%i%' <- intersect



  #------- Gjøre utvalg
#Utvalg fra variable:
  RegData <- switch(valgtVar,
                    RAND0 = RegData[which(RegData$R0Status==1) %i% which(RegData$R0Metode %in% 1:2)
                                    %i% which(RegData$InnDato >= '2016-01-01'), ],
                    TSS0 = RegData[which(RegData$Tss2Status==1) %i% which(RegData$Tss2Type %in% 1:2)
                                   %i% which(RegData$InnDato >= '2016-01-01'), ])

  NGERUtvalg <- NGERUtvalgEnh(RegData = RegData, minald = minald, maxald = maxald, datoFra = datoFra,
                              datoTil = datoTil, MCEType = MCEType, Hastegrad=Hastegrad, velgDiag=velgDiag)
  smltxt <- NGERUtvalg$smltxt
  medSml <- NGERUtvalg$medSml
  utvalgTxt <- NGERUtvalg$utvalgTxt
  hovedgrTxt <- NGERUtvalg$hovedgrTxt
  RegData <- NGERUtvalg$RegData


    KImaal <- switch(valgtVar, #dummym?l!!
                   RAND0 = 80,
                   TSS20 = 80,
                   kvalInd = 1:4)

  ind <- NGERUtvalg$ind
  if (medSml == 0) {ind$Rest <- 0}
  N <- list(Hoved = length(ind$Hoved), Rest = length(ind$Rest))
  Ngr <- N


  ########RAND36
  if (valgtVar == 'RAND0'){

    tittel <- 'RAND36, alle dimensjoner'
    Rand0var <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
                  'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
    grtxt <- c('Fysisk funksjon',	'Fysisk rollebegrensning',	'Følelsesmessig \n rollebegrensning',
               'Energinivå/vitalitet',	'Mental helse', 'Sosial funksjon',
               'Smerte',	'Generell helsetilstand')
    AggVerdier <- list(Hoved = colMeans(RegData[ind$Hoved, Rand0var], na.rm = T),
                       Rest = colMeans(RegData[ind$Rest, Rand0var], na.rm=T)) #AggVerdier <- list(Hoved=Midt, Rest=0, KIned=KIned, KIopp=KIopp, KIHele=KIHele)
    xakseTxt <- 'Gjennomsnittlig skår (høyest er best)'
  }

  if (valgtVar == 'TSS0') {

    tittel <- 'TSS2, alle spørsmål'
    variable <- c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',	'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')
    grtxt <- c('Svært godt møte \n med gyn. avd.',
      'Svært bra tilpasset \n behandling/opplegg',
    'I svært stor grad lyttet \n og forsto behandlerne',
    'Svært stor grad av tillit \n til behandlerne',
    'Svært stor grad av enighet \n om målsetn. for behandlinga',
    'Svært positiv oppfatning \n av gyn. avd.')
    xakseTxt <- 'Andel (%)'
    Ngr$Hoved <- apply(RegData[ind$Hoved,variable], MARGIN=2,
                                    FUN=function(x) sum(x == 3, na.rm=T))
    Ngr$Hoved['Tss2Generelt'] <- sum(RegData$Tss2Generelt[ind$Hoved]==4, na.rm=T)
    AggVerdier$Hoved <- 100*Ngr$Hoved/N$Hoved

    if (NGERUtvalg$medSml==1) {
      Ngr$Rest <- apply(RegData[ind$Rest,variable], MARGIN=2,
                                     FUN=function(x) sum(x == 3, na.rm=T))
      Ngr$Rest['Tss2Generelt'] <- sum(RegData$Tss2Generelt[ind$Rest]==4, na.rm=T)
      AggVerdier$Rest <- 100*Ngr$Rest/N$Rest
    }
  }

  if (valgtVar == 'kvalInd') {
    #  Reoperasjon for komplikasjoner innen 4 uker: intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1))
    #	Konvertering til laparoskopi (ut fra hysteroskopi) /laparotomi (ut fra hysteroskopi, laparoskopi).
    # SJEKK:      Bare denne fra før: Konvertert til laparotomi?, intersect(which(RegData$LapKonvertert %in% 0:1), which(RegData$LapStatus == 1))
    #	Intraoperative komplikasjoner. - både lap og hys? Blir ikke det "urettferdig" for de som gjør relativt mer hysteroskopi?
    #	Oppfølging etter 4 uker, (postoperative) komplikasjoner): RegData$Variabel[RegData$Opf0Status==1] <- 1.
    #               Ønsker å heller benytte RegData$Variabel[RegData$Opf0Metode %in% 1:2] <- 1
    tittel <- 'Kvalitetsindikatorer, prosessmål'
    grtxt <- c('Postop. komplikasjon: \n Reoperasjon', 'Konvertering hys->lap, \n hys/lap->laparatomi',
               'Intraoperative \n komplikasjoner', 'Oppfølging etter 4 uker')
    variable <- c('Opf0Reoperasjon', 'LapKonvertert', 'Må sjekkes','Opf0Status')




      varTxt <- 'av postoperativ oppfølging'
      tittel <- 'Pasienter som har fått oppfølging etter 6-8 uker'




    Ngrtxt <- paste0(' (', as.character(Ngr),')')
    indGrUt <- which(Ngr < Ngrense)
    if (length(indGrUt)==0) { indGrUt <- 0}
    Ngrtxt[indGrUt] <- paste0(' (<', Ngrense,')')
    N <- dim(RegData)[1]

    indUT <- which(is.na(Midt))  #Rydd slik at bare benytter indGrUt
    soyletxt[indUT] <- ''
    Ngr <- list(Hoved=Ngr[sortInd], Rest=0)

}

  Nfig <- list(Hoved=NULL, Rest=NULL)
  Nfig$Hoved <- ifelse(min(N$Hoved)==max(N$Hoved),
                       min(N$Hoved[1]),
                       paste0(min(N$Hoved),'-',max(N$Hoved)))
  Nfig$Rest <- ifelse(min(N$Rest)==max(N$Rest),
                      min(N$Rest[1]),
                      paste0(min(N$Rest),'-',max(N$Rest)))


  soyletxt <- sprintf(paste0('%.1f'), AggVerdier$Hoved)
  antDesTxt <- paste0('%.', 1, 'f')
  #grtxt <- paste0(rev(grtxt), '\n (', rev(sprintf(antDesTxt, AggVerdier$Hoved)), '%)')

  cexgr <- 1-ifelse(length(soyletxt)>20, 0.25*length(soyletxt)/60, 0)
  grtxt2 <- paste0(sprintf('%.1f',AggVerdier$Hoved), '%') #paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')


  ###-----------Figur---------------------------------------
  if ( max(Nfig$Hoved) < 5 | 	(NGERUtvalg$medSml ==1 & Nfig$Rest<10)) {
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=tittel)	#
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 "egne" registreringer eller færre enn 10 totalt', cex=1.2)
    if ( outfile != '') {dev.off()}

  } else {


    ###Innparametre til evt. funksjon: subtxt, grtxt, grtxt2, tittel, AggVerdier, utvalgTxt, retn, cexgr
    FigTypUt <- figtype(outfile, fargepalett=NGERUtvalg$fargepalett)
    #Tilpasse marger for ? kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.65)
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	#tykkelse p? linja som repr. landet
    cexleg <- 1	#St?rrelse p? legendtekst
    cexgr <- 1

      xmax <- 100 #max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.15
      pos <- barplot(as.numeric(AggVerdier$Hoved), horiz=TRUE, beside=TRUE, las=1, xlab=xakseTxt, #main=tittel,
                     col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
      if (Nfig$Hoved>0) {mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25)}
      soyleXpos <- 1.12*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr
      text(x=soyleXpos+xmax*0.02, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[4])	#AggVerdier, hvert sykehus

      if (NGERUtvalg$medSml == 1) {
        points(as.numeric(rev(AggVerdier$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste0(NGERUtvalg$hovedgrTxt, ' (N=', Nfig$Hoved,')'),
                        paste0(NGERUtvalg$smltxt, ' (N=', Nfig$Rest,')')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
               lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
      } else {
        legend('top', paste0(NGERUtvalg$hovedgrTxt, ' (N=', Nfig$Hoved,')'),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      }

    title(tittel, line=1, font.main=1)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    if ( outfile != '') {dev.off()}
  }

  UtData <- list(paste0(toString(tittel),'.'), AggVerdier, N, grtxt )
  names(UtData) <- c('tittel', 'AggVerdier', 'Antall', 'GruppeTekst')
  return(invisible(UtData))
}

