#' Søylediagram som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram som viser andeler (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Pasientens alder, 5-årige aldersgrupper
#'     \item Diagnoser: Hyppigst forekommende diagnoser
#'     \item HysGjforingsGrad: Gjennomføringsgrad av hysteroskopi
#'    		Koder:	1-Fullstendig, 2-Ufullstendig, 3-Mislykket
#'     \item HysKomplikasjoner: Hysteroskopi intrapoerative komplikasjoner
#'     \item KomplPost: Postoperative komplikasjoner
#'     \item KomplPostUtd: Postoperative komplikasjoner for ulike utdanningsgrupper
#'     \item KomplReopUtd: Andel reoperasjoner som følge av komplikasjon for ulike utdanningsgrupper
#'     \item LapKomplikasjoner: Laparoskopiske intrapoerative komplikasjoner
#'     \item LapEkstrautstyr: Laparaskopisk ekstrautstyr - Kommer, NY variabel: koagulasjon og klipping
#'     \item LapIntraabdominell: Laparoskopiske intraabdominale komplikasjoner
#'     \item LapNumHjelpeinnstikk: Antall hjelpeinnstikk
#'     \item LapTeknikk: Laparaskopisk tilgang, teknikk og metode (Tidl LapTilgangsMetode)
#'     \item Norsktalende: Pasientens norskkunnskaper
#'     \item OpAnestesi: Anestesitype
#'     \item OpASA: ASA-grad
#'     \item OpBMI: BMI-kategori
#'     \item Opf0AlvorlighetsGrad: Alvorlighetsgrad, postoperative komplikasjoner
#'			  Kode 1-Lite alvorlig, 2-Middels alvorlig, 3-Alvorlig, 4-Dødelig
#'		 \item Opf0KomplInfeksjon: Type infeksjoner
#'		 \item Opf0metode: Opfølgingsmetode
#'     \item OpDagkirurgi: Dagkirurgiske inngrep
#'     \item OpIVaktTid: Operasjon i legens vakttid
#'     \item OpKategori: Hastegrad av operasjon
#'     \item OpMetode: Operasjonsmetode
#'     \item OpTid: Operasjonstid (minutter)
#'     \item OpTidlVagInngrep: Tidligere vaginale inngrep
#'     \item OpTidlLapsko: Tidligere laparoskopi
#'     \item OpTidlLaparotomi: Tidligere laparatomi
#'     \item OpType: Primæroperasjon eller reoperasjon
#'     \item R0ScoreEmo:
#'     \item R0ScoreEnergy:
#'     \item R0ScoreGeneral:
#'     \item R0ScorePain:
#'     \item R0ScorePhys:
#'     \item R0ScoreRoleLmtPhy:
#'     \item R0ScoreRoleLmtEmo:
#'     \item R0ScoreSosial:
#'     \item RegForsinkelse: Tid fra operasjon til ferdigstilt registrering
#'     \item Prosedyrer: Hyppigst forekommende prosedyrer
#'     \item SivilStatus: Sivilstand
#'     \item Tss2Mott: Hvordan du ble mottatt på avd.
#'     \item Tss2Behandling:
#'     \item Tss2Lytte:
#'     \item Tss2Behandlere:
#'     \item Tss2Enighet:
#'     \item Tss2Generelt:
#'     \item Utdanning: Pasientens utdanning (1:Grunnskole, 2:VG, 3:Fagskole, 4:Universitet<4 år, 5:Universitet>4 år, 6:Ukjent)
#'    }
#'
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel som skal visualiseres
#' @param outfile Navn på fil figuren skrives til. Standard: '' (Figur skrives
#'    til systemets standard utdataenhet (som regel skjerm))
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @inheritParams NGERUtvalgEnh
#' @param preprosess Preprosesser data
#'                 0: Nei (Standard)
#'                 1: Ja
#' @param hentData Gjør spørring mot database
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#' @param AlvorlighetKompl  Alvorlighetsgrad for postoperative komplikasjoner (Flervalg)
#'                          Angis som en vektor av tall som tekst, f.eks. c('1','2')
#'                          1: Lite alvorlig
#'                          2: Middels alvorlig
#'                          3: Alvorlig
#'                          4: Dødelig
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#' @return En figur med søylediagram (fordeling) av ønsket variabel
#'
#' @export
#'
NGERFigAndeler  <- function(RegData=0, valgtVar, datoFra='2013-01-01', datoTil='2050-12-31', minald=0, maxald=130,
                            outfile='', reshID, enhetsUtvalg=0, MCEType=99, Hastegrad='', AlvorlighetKompl='',
                            hentData=0, preprosess=0)
{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERRegDataSQL(datoFra = datoFra) #, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  ###----------- Figurparametre ------------------------------
  cexgr <- 1	#Kan endres for enkeltvariable
  grtxt <- ''		#Spesifiseres for hver enkelt variabel
  grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
  subtxt <- ''	#Benevning
  flerevar <- 0
  antDes <- ifelse(valgtVar %in% c('HysKomplikasjoner', 'LapIntraabdominell', 'LapKomplikasjoner'),2, 1)
  '%i%' <- intersect
 #koder <- NULL

  NGERVarSpes <- NGERVarTilrettelegg(RegData, valgtVar=valgtVar, grVar='', figurtype='andeler')
  RegData <- NGERVarSpes$RegData
  flerevar <- NGERVarSpes$flerevar
  subtxt <- NGERVarSpes$subtxt
  grtxt <- NGERVarSpes$grtxt

  ###Gjør utvalg (NGERUtvalg)
  ###Kjører denne etter variabeldefinisjon for at utvalgTxt skal bli riktig
  NGERUtvalg <- NGERUtvalgEnh(RegData = RegData, minald = minald, maxald = maxald, datoFra = datoFra,
                           datoTil = datoTil, MCEType = MCEType, AlvorlighetKompl=AlvorlighetKompl,
                           Hastegrad=Hastegrad, enhetsUtvalg = enhetsUtvalg)
  RegData <- NGERUtvalg$RegData
  utvalgTxt <- NGERUtvalg$utvalgTxt



#  #Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
#  Andeler <- list(Hoved = 0, Rest =0)
#  N$Rest <- 0
#  AntRest <- 0

#  #     if (flerevar == 0 ) { #pt. alltid 0 når kommer hit...
#  AntHoved <- table(RegData$VariabelGr[indHoved])
#  N$Hoved <- sum(AntHoved)
#  Andeler$Hoved <- 100*AntHoved/N$Hoved
#  if (medSml==1) {
#    AntRest <- table(RegData$VariabelGr[indRest])
#    N$Rest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
#    Andeler$Rest <- 100*AntRest/N$Rest
#  }
#  #    }

#----------- Beregninger ---------------:
      Andeler <- list(Hoved = 0, Rest =0)
      N <- list(Hoved = 0, Rest =0)
      Nfig <- list(Hoved = 0, Rest =0) #figurtekst: N i legend
      Ngr <- list(Hoved = 0, Rest =0)
      ind <- NGERUtvalg$ind
	  variable <- NGERVarSpes$variable

      Ngr$Hoved <- switch(as.character(flerevar),
                          '0' = table(RegData$VariabelGr[ind$Hoved]),
                          # '1' = colSums(sapply(RegData[ind$Hoved ,variable], as.numeric), na.rm=T))
                          '1' = apply(RegData[ind$Hoved,variable], MARGIN=2,
                                      FUN=function(x) sum(x == 1, na.rm=T)))
      #N$ gjelder selv om totalutvalget er ulikt for de ulike variablene i flerevar
     N$Hoved <- switch(as.character(flerevar),
                        '0' = sum(Ngr$Hoved),	#length(ind$Hoved)- Kan inneholde NA
                        '1' = apply(RegData[ind$Hoved,variable], MARGIN=2,
                                 FUN=function(x) sum(x %in% 0:1, na.rm=T)))
      Andeler$Hoved <- 100*Ngr$Hoved/N$Hoved

      if (NGERUtvalg$medSml==1) {
           Ngr$Rest <- switch(as.character(flerevar),
                               '0' = table(RegData$VariabelGr[ind$Rest]),
                               '1' = apply(RegData[ind$Rest,variable], MARGIN=2,
                                           FUN=function(x) sum(x == 1, na.rm=T)))
            N$Rest <- switch(as.character(flerevar),
                             '0' = sum(Ngr$Rest),
                             '1' = apply(RegData[ind$Rest,variable], MARGIN=2,
                                   FUN=function(x) sum(x %in% 0:1, na.rm=T)))
            Andeler$Rest <- 100*Ngr$Rest/N$Rest
      }

      if(flerevar==1) {
            Nfig$Hoved <- ifelse(min(N$Hoved)==max(N$Hoved),
                                 min(N$Hoved[1]),
                                 paste0(min(N$Hoved),'-',max(N$Hoved)))
            Nfig$Rest <- ifelse(min(N$Rest)==max(N$Rest),
                                min(N$Rest[1]),
                                paste0(min(N$Rest),'-',max(N$Rest)))
      } else {
            Nfig <- N}

      grtxt2 <- paste0(sprintf('%.1f',Andeler$Hoved), '%') #paste0('(', sprintf('%.1f',Andeler$Hoved), '%)')



  ###-----------Figur---------------------------------------
  if ( Nfig$Hoved %in% 1:5 | 	(NGERUtvalg$medSml ==1 & Nfig$Rest<10)) {
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=NGERVarSpes$tittel)	#
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 "egne" registreringer eller færre enn 10 totalt', cex=1.2)
    if ( outfile != '') {dev.off()}

  } else {


    ###Innparametre til evt. funksjon: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr
    FigTypUt <- figtype(outfile, fargepalett=NGERUtvalg$fargepalett)
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    antDesTxt <- paste0('%.', antDes, 'f')
    if (length(grtxt2) == 1) {grtxt2 <- paste0('(', sprintf(antDesTxt, Andeler$Hoved), '%)')}
    grtxtpst <- paste0(rev(grtxt), '\n (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)')
    if (valgtVar %in% c('Diagnoser', 'Prosedyrer', 'LapEkstrautstyr') ) {
      grtxtpst <- paste0(rev(grtxt), ' (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)')}
    vmarg <- switch(NGERVarSpes$retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.65))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	#tykkelse på linja som repr. landet
    cexleg <- 1	#Størrelse på legendtekst

    #Horisontale søyler
    if (NGERVarSpes$retn == 'H') {
      xmax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
      pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel,
                     col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
      if (Nfig$Hoved>0) {mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=NGERVarSpes$cexgr, adj=1, line=0.25)}

      if (NGERUtvalg$medSml == 1) {
        points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste0(NGERUtvalg$hovedgrTxt, ' (N=', Nfig$Hoved,')'),
                        paste0(NGERUtvalg$smltxt, ' (N=', Nfig$Rest,')')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
               lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
      } else {
        legend('top', paste0(NGERUtvalg$hovedgrTxt, ' (N=', Nfig$Hoved,')'),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      }
    }

    if (NGERVarSpes$retn == 'V' ) {
      #Vertikale søyler eller linje
      ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
      pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
                     xlab=subtxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=subtxt,
      mtext(at=pos, NGERVarSpes$grtxt, side=1, las=1, cex=NGERVarSpes$cexgr, adj=0.5, line=0.5)
      mtext(at=pos, grtxt2, side=1, las=1, cex=NGERVarSpes$cexgr, adj=0.5, line=1.5)
      if (NGERUtvalg$medSml == 1) {
        points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste0(NGERUtvalg$hovedgrTxt, ' (N=', Nfig$Hoved,')'), paste0(NGERUtvalg$smltxt, ' (N=', Nfig$Rest,')')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
               lwd=lwdRest, ncol=2, cex=cexleg)
      } else {
        legend('top', paste0(NGERUtvalg$hovedgrTxt, ' (N=', Nfig$Hoved,')'),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      }
    }


    title(NGERVarSpes$tittel, line=1, font.main=1)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    if ( outfile != '') {dev.off()}
  }

  #Beregninger som returneres fra funksjonen.
#  AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
#  rownames(AndelerUt) <- c('Hoved', 'Rest')
#  AntallUt <- rbind(AntHoved, AntRest)
#  rownames(AntallUt) <- c('Hoved', 'Rest')

  UtData <- list(paste0(toString(NGERVarSpes$tittel),'.'), Andeler, N, grtxt )
  names(UtData) <- c('tittel', 'Andeler', 'Antall', 'GruppeTekst')
  return(invisible(UtData))
}
