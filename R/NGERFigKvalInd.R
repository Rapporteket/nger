#' Søylediagram som viser flere kvalitetsindikatorer
#'
#' Funksjon som genererer en figurer med ei gruppe kvalitetsindikatorer
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item RAND0: Alle dimensjonene i RAND36 ved operasjonstidspunkt. Gjennomsnitt
#'     \item RAND1: Alle dimensjonene i RAND36 ett år etter operasjon. Gjennomsnitt
#'     \item TSS0: Alle sp?rsm?lene i TSS2 ved oppf?lging etter 4-6uker. Andel av beste svaralternativ
#'     \item kvalInd: Samling av kvalitetsindikatorer
#'    }
#'
#' @inheritParams NGERFigAndeler
#' @inheritParams NGERFigAndelerGrVar
#' @inheritParams NGERUtvalgEnh
#'
#'
#' @return Søylediagram samling av kvalitetsindikatorer
#'
#' @export


NGERFigKvalInd <- function(RegData, reshID=0, velgAvd=0, datoFra='2013-01-01', datoTil=Sys.Date(),
                           valgtVar='kvalInd', enhetsUtvalg=0, minald=0, maxald=130, OpMetode=99,
                           AlvorlighetKompl = 0,
                           Hastegrad=99, dagkir=9, hentData=0, preprosess=1, velgDiag=0, Ngrense=10,
                           outfile='', ...) {

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0('FigKvalInd: ',valgtVar))
  }
  if (hentData == 1) {
    RegData <- NGERRegDataSQL(datoFra, datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert.
  if (preprosess == 1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  '%i%' <- intersect



  #------- Gjøre utvalg
#Utvalg fra variable:
  RegData <- switch(valgtVar,
                    RAND0 = RegData[which(RegData$R0Metode %in% 1:2) #which(RegData$R0Status==1) %i%
                                    %i% which(RegData$InnDato >= '2016-01-01'), ],
                    RAND1 = RegData[ which(RegData$RY1metode %in% 1:3) #which(RegData$RY1Status==1) %i%
                                    %i% which(RegData$InnDato >= '2018-01-01'), ],
                    TSS0 = RegData[which(RegData$Tss2Type %in% 1:3) #which(RegData$Tss2Status==1) %i%
                                   %i% which(RegData$InnDato >= '2016-01-01'), ],
                    kvalInd = RegData)
  #NGERUtvalg <- NGERUtvalgEnh(RegData = RegData, reshID=reshID, enhetsUtvalg=enhetsUtvalg)
  NGERUtvalg <- NGERUtvalgEnh(RegData = RegData, reshID=reshID,  minald = minald, maxald = maxald, datoFra = datoFra,
                              datoTil = datoTil, OpMetode = OpMetode, Hastegrad=Hastegrad, velgDiag=velgDiag,
                              AlvorlighetKompl = AlvorlighetKompl,
                              dagkir = dagkir, enhetsUtvalg=enhetsUtvalg, velgAvd=velgAvd)
  smltxt <- NGERUtvalg$smltxt
  medSml <- NGERUtvalg$medSml
  utvalgTxt <- NGERUtvalg$utvalgTxt
  hovedgrTxt <- NGERUtvalg$hovedgrTxt
  RegData <- NGERUtvalg$RegData


    KImaal <- switch(valgtVar,
                   RAND0 = 80,
                   TSS0 = 80,
                   kvalInd = 1:4)

  ind <- NGERUtvalg$ind
  if (medSml == 0) {ind$Rest <- 0}
  N <- list(Hoved = length(ind$Hoved), Rest = length(ind$Rest))
  Nfig <- N
  Ngr <- N
  AggVerdier <- list(Hoved = 0, Rest = 0)
  xakseTxt <- 'Andel (%)'
  xmax <- 100
  indUtHoved <- NULL
  indUtRest <- NULL


  ########RAND36
  if (valgtVar %in% c('RAND0', 'RAND1')){

    grtxt <- c('Fysisk funksjon',	'Fysisk \n rollebegrensning',	'Følelsesmessig \n rollebegrensning',
               'Energinivå/vitalitet',	'Mental helse', 'Sosial funksjon',
               'Smerte',	'Generell \n helsetilstand')
    if (valgtVar =='RAND0') {
      RANDvar <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',
                  'R0ScoreEnergy',	'R0ScoreEmo', 'R0ScoreSosial',
                  'R0ScorePain',	'R0ScoreGeneral')
    tittel <- 'RAND36 ved operasjon, alle dimensjoner'
    }
    if (valgtVar =='RAND1')  {
      #mangler <- c(-2, -7)
      RANDvar <- c('R1ScorePhys',	'R1ScoreRoleLmtPhy',	'R1ScoreRoleLmtEmo',
                      'R1ScoreEnergy',	'R1ScoreEmo', 'R1ScoreSosial',
                      'R1ScorePain',	'R1ScoreGeneral') #[mangler]
      #grtxt <- grtxt[mangler]
      tittel <- 'RAND36 ett år etter, alle dimensjoner'
                  }
    AggVerdier <- list(Hoved = colMeans(RegData[ind$Hoved, RANDvar], na.rm = T),
                       Rest = colMeans(RegData[ind$Rest, RANDvar], na.rm=T)) #AggVerdier <- list(Hoved=Midt, Rest=0, KIned=KIned, KIopp=KIopp, KIHele=KIHele)
    xakseTxt <- 'Gjennomsnittlig skår (høyest er best)'
  }


  if (valgtVar == 'TSS0') {

    tittel <- 'TSS2, alle spørsmål'
    variable <- rev(c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',	'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt'))
    grtxt <- rev(c('Svært godt møte \n med gyn. avd.',
      'Svært bra tilpasset \n behandling/opplegg',
    'I svært stor grad lyttet \n og forsto behandlerne',
    'Svært stor grad av tillit \n til behandlerne',
    'Svært stor grad av enighet \n om målsetn. for behandlinga',
    'Svært positiv oppfatning \n av gyn. avd.'))
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
    # Reoperasjon for komplikasjoner innen 4 uker:
    #     intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1))
    #	Konvertering til laparoskopi (ut fra hysteroskopi) /laparotomi (ut fra hysteroskopi, laparoskopi).
    #   LapKonvertert og HysKonvertert gir konvertering for hhv både lap/hys og begge (og ingenting annet)
    #	Intraoperative kompl.: HysKomplikasjoner/LapKomplikasjoner er NA hvis ikke hys/lap el. begge er utf.
    #	Oppfølging etter 4 uker, kun de som faktisk har fått oppfølging:
    #               Ønsker å heller benytte RegData$Variabel[RegData$Opf0Metode %in% 1:2] <- 1

    #postop.kompl. lap og - hys
    tittel <- 'Kvalitetsindikatorer, prosessmål'
    grNavn <- c('Postop. komplikasjon: \n Reoperasjon',
                'Postop. komp., middels/alvorlig, \n laparoskopi', #NY
                'Postop. komp., middels/alvorlig, \n hysteroskopi', #NY
               'Intraop. komplikasjon ved \n laparoskopi',
               'Intraop. komplikasjon ved \n hysteroskopi',
               'Konvertert lap. til laparotomi \n ', #"LapKonvertert":
               'Konvertert hysteroskopi til \n laparaskopi/-tomi') #"HysKonvertert":
               # 'Ikke utført oppfølging \n etter 4 uker')
               # 'Ikke ferdistilt registrering \n innen 6 uker')
    variable <- c('PostOpKomplReop', 'PostKomplLap', 'PostKomplHys', 'LapKomplikasjoner', 'HysKomplikasjoner',
                  'LapKonvertert', 'HysKonvertert') #, 'Opf0') #, 'Innen6uker')

    indKompl <- which(RegData$Opf0Komplikasjoner %in% 0:1)
    #Postop.kompl. laparoskopi
    #NB: Det er et bevisst valg at vi også har med OpMetode=3
    indLap <- which(RegData$OpMetode==1 | RegData$OpMetode == 3)
    RegData$PostKomplLap <- NA
    RegData$PostKomplLap[intersect(indLap, indKompl)] <- 0
    RegData$PostKomplLap[intersect(which(RegData$Opf0AlvorlighetsGrad %in% 2:4), indLap) ] <- 1
    #sum(RegData$PostKomplLap[ind$Hoved], na.rm = T)
    #table(RegData$ShNavn[ind$Hoved])
    #table(RegData$PostKomplLap)

    #Postop.kompl. hysteroskopi
    indHys <- which(RegData$OpMetode==2 | RegData$OpMetode == 3)
    RegData$PostKomplHys <- NA
    RegData$PostKomplHys[intersect(indHys, indKompl)] <- 0
    RegData$PostKomplHys[intersect(which(RegData$Opf0AlvorlighetsGrad %in% 2:4), indHys)] <- 1

    #Reoperasjon som følge av komplikasjon
    #Kode 0: Nei, 1:Ja
    RegData$PostOpKomplReop <- NA
    RegData$PostOpKomplReop[which(RegData$Opf0Komplikasjoner %in% 0:1)] <- 0
    RegData$PostOpKomplReop[which(RegData$Opf0Reoperasjon == 1)] <- 1


    Ngr$Hoved <- apply(RegData[ind$Hoved,variable], MARGIN=2,
                                    FUN=function(x) sum(x == 1, na.rm=T))
    #N$ gjelder selv om totalutvalget er ulikt for de ulike variablene i flerevar
    N$Hoved <- apply(RegData[ind$Hoved,variable], MARGIN=2,
                                  FUN=function(x) sum(x %in% 0:1, na.rm=T))
    AggVerdier$Hoved <- 100*Ngr$Hoved/N$Hoved

    if (NGERUtvalg$medSml==1) {
      Ngr$Rest <- apply(RegData[ind$Rest,variable], MARGIN=2,
                                     FUN=function(x) sum(x == 1, na.rm=T))
      N$Rest <- apply(RegData[ind$Rest,variable], MARGIN=2,
                                   FUN=function(x) sum(x %in% 0:1, na.rm=T))
      AggVerdier$Rest <- 100*Ngr$Rest/N$Rest
      }

    indUtHoved <- N$Hoved < Ngrense
    indUtRest <- N$Rest < Ngrense
    AggVerdier$Hoved[indUtHoved] <- NA
    AggVerdier$Rest[indUtRest] <- NA

    xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.15
    grtxt <- paste0(grNavn, ' (N=', N$Hoved, ')')
    grtxt[indUtHoved] <-   paste0(grNavn[indUtHoved], ' (N<', Ngrense, ')')

} #end kvalInd
      Nfig$Hoved <- ifelse(min(N$Hoved)==max(N$Hoved),
                           min(N$Hoved[1]),
                           paste0(min(N$Hoved),'-',max(N$Hoved)))
      Nfig$Rest <- ifelse(min(N$Rest)==max(N$Rest),
                          min(N$Rest[1]),
                          paste0(min(N$Rest),'-',max(N$Rest)))

  soyletxt <- sprintf(paste0('%.1f'), AggVerdier$Hoved)
  soyletxt[indUtHoved] <- ''

  cexgr <- 1-ifelse(length(soyletxt)>20, 0.25*length(soyletxt)/60, 0)
  grtxt2 <- paste0(sprintf('%.1f',AggVerdier$Hoved), '%') #paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
  names(AggVerdier$Hoved) <- grtxt

  FigDataParam <- list(AggVerdier=AggVerdier,
                       N=Nfig,
                       Ngr=Ngr,
                       #KImaal <- NIRVarSpes$KImaal,
                       grtxt2=grtxt2,
                       grtxt=grtxt,
                       #grTypeTxt=grTypeTxt,
                       tittel=tittel,
                       retn='H',
                       #subtxt=subtxt,
                       #yAkseTxt=yAkseTxt,
                       utvalgTxt=utvalgTxt,
                       fargepalett=NGERUtvalg$fargepalett,
                       medSml=medSml,
                       hovedgrTxt=hovedgrTxt,
                       smltxt=smltxt)

  ###-----------Figur---------------------------------------
  if ( max(N$Hoved) < Ngrense | 	(NGERUtvalg$medSml ==1 & max(N$Rest)< Ngrense)) {
    FigTypUt <- rapFigurer::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=tittel)	#
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, paste0('Færre enn ', Ngrense, ' "egne" registreringer eller \n
                          færre enn ', Ngrense, ' i sammenligningsgruppe'), cex=1.2)
    if ( outfile != '') {dev.off()}

  } else {


    ###Innparametre til evt. funksjon: subtxt, grtxt, grtxt2, tittel, AggVerdier, utvalgTxt, retn, cexgr
    FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NGERUtvalg$fargepalett)
    #Tilpasse marger for ? kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75)
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	#tykkelse på linja som repr. landet
    cexleg <- 1	#Størrelse på legendtekst
    cexgr <- 1

      pos <- barplot(as.numeric(AggVerdier$Hoved), horiz=TRUE, beside=TRUE, las=1, xlab=xakseTxt, #main=tittel,
                     col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
      if (Nfig$Hoved>0) {mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25)}
      soyleXpos <- 1.12*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr
      text(x=soyleXpos+xmax*0.02, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[4])	#AggVerdier, hvert sykehus

      if (NGERUtvalg$medSml == 1) {
        points(as.numeric(AggVerdier$Rest), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
                        paste0(smltxt, ' (N=', Nfig$Rest,')')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
               lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
      } else {
        legend('top', paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      }

    title(tittel, line=1, font.main=1)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    if ( outfile != '') {dev.off()}
  }

  UtData <- FigDataParam #list(paste0(toString(tittel),'.'), AggVerdier, N, grtxt )
  #names(UtData) <- c('tittel', 'AggVerdier', 'Antall', 'GruppeTekst')
  return(invisible(UtData))
}

