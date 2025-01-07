#' Tidstrend (år) av andel for en gitt variabel.
#'
#' Funksjon som genererer en figur med andeler av en variabel for hvert år.
#'
#' Detaljer:
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'		\item Alder: Pasienter over 70 år
#'		\item Opf0KomplBlodning: Postop. komplikasjon: Blødning
#'		\item Opf0KomplUtstyr: Postop. komplikasjon: Problemer med ustyr
#'		\item Opf0KomplInfeksjon: Postop. komplikasjon: Infeksjon
#'		\item Opf0KomplOrgan: Postop. komplikasjon: Organskade
#'		\item Opf0Reoperasjon: Andel reoperasjon som følge av komplikasjon
#'		\item KomplPostopAlvor: Andel av postoperative komplikasjoner som var alvorlige (3 og 4)
#'		\item KomplIntra: Komplikasjoner ved operasjon. (kombinerer variablene HysKomplikasjoner og LapKomplikasjoner)
#'		\item KomplPostop: Andel postoperative komplikasjoner
#'		\item LapKonvertert: Konvertert til laparoromi?
#'		\item OpAntibProfylakse: Andel som får antibiotika
#'		\item OpASA: ASA-grad > II
#'		\item OpBMI: Pasienter med fedme (BMI>30)
#'		\item Opf0Status: Pasienter som har fått postoperativ oppfølging
#'    }

#' @inheritParams NGERFigAndeler
#' @inheritParams NGERUtvalgEnh
#' @param tidsenhet Oppløsning på tidsaksen. Verdier: Aar (standard), Halvaar, Kvartal, Mnd
#' @return Figur som viser andel av valgt variabel, utvikling over tid.
#'
#' @export

NGERFigAndelTid <- function(RegData=0, valgtVar, preprosess=1, hentData=0, reshID=0,
                            datoFra='2015-01-01', datoTil=Sys.Date(), #Hastegrad='',
                            minald=0, maxald=130,
                            OpMetode=99, AlvorlighetKompl='', velgDiag=0, behNivaa = 0,
                            enhetsUtvalg=0, tidsenhet='Aar', outfile='',
                            ...) {

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0('FigAndelTid: ',valgtVar))
  }

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERRegDataSQL(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  NGERVarSpes <- NGERVarTilrettelegg(RegData, valgtVar=valgtVar,
                                     OpMetode=OpMetode, figurtype='andelTid')
  RegData <- NGERVarSpes$RegData
  flerevar <- NGERVarSpes$flerevar
  subtxt <- NGERVarSpes$subtxt
  grtxt <- NGERVarSpes$grtxt



  #Endrer startdato til å være 12 hele måneder før sluttdato
  #if (tidsenhet == 'Mnd') {datoFra <- paste0(as.numeric(strftime(datoTil, format="%Y"))-1,'-',
  #                                           strftime(datoTil, format="%m"),'-','01')}

  NGERUtvalg <- NGERUtvalgEnh(RegData=RegData, enhetsUtvalg=enhetsUtvalg, reshID = reshID,
                              datoFra=datoFra, datoTil=datoTil,
                               minald=minald, maxald=maxald,
                              velgDiag=velgDiag, behNivaa = behNivaa,
                              OpMetode=OpMetode, AlvorlighetKompl=AlvorlighetKompl) #Hastegrad=Hastegrad)
  RegData <- NGERUtvalg$RegData
  utvalgTxt <- NGERUtvalg$utvalgTxt


  #------------------------Klargjøre tidsenhet--------------
  #if (dim(RegData)[1]>9) {
    RegDataFunk <- SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet = tidsenhet)
    RegData <- RegDataFunk$RegData
    tidtxt <- RegDataFunk$tidtxt
xAkseTxt <- switch(tidsenhet,
                     Aar='Operasjonsår',
                     Halvaar = 'Operasjonsår og -halvår',
                     Kvartal = 'Operasjonsår og -kvartal',
                     Mnd='Operasjonsår og -måned')

  #-------------------------Beregning av andel-----------------------------------------

    AggVerdier <- list(Hoved = 0, Rest =0)
    Ngr <- list(Hoved = 0, Rest =0)
	ind <- NGERUtvalg$ind
    N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))

  #RegData$TidsEnhet <- factor(RegData$TidsEnhet, levels=as.character(tidtxt))
  Ngr$Rest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], length)
  NTidHendRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
  AggVerdier$Rest <- NTidHendRest/Ngr$Rest*100
  Ngr$Hoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], length)
  NTidHendHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T)
  AggVerdier$Hoved <- NTidHendHoved/Ngr$Hoved*100
  #Andeler <- rbind(AggVerdier$Rest, AggVerdier$Hoved)

  medSml <- NGERUtvalg$medSml
  hovedgrTxt <- NGERUtvalg$hovedgrTxt

  FigDataParam <- list(AggVerdier=AggVerdier,
                       N=N,
                       Ngr=Ngr,
                      # KImaal <- KImaal,
                      # KImaaltxt <- KImaaltxt,
                       #soyletxt=soyletxt,
                       grtxt=levels(RegData$TidsEnhet),
                       #grtxt2=grtxt2,
                       varTxt=NGERVarSpes$varTxt,
                       #tidtxt=tidtxt, #NGERVarSpes$grtxt,
                       tittel=NGERVarSpes$tittel,
                       retn='V',
                       xAkseTxt=xAkseTxt,
                      # yAkseTxt=yAkseTxt,
                       utvalgTxt=NGERUtvalg$utvalgTxt,
                       fargepalett=NGERUtvalg$fargepalett,
                       medSml=medSml,
                       hovedgrTxt=hovedgrTxt,
                       smltxt=NGERUtvalg$smltxt)

  #----------FIGUR------------------------------
  #Hvis for få observasjoner..
  if (length(ind$Hoved) < 10 | (medSml ==1 & length(ind$Rest)<10)) {
    #-----------Figur---------------------------------------
    FigTypUt <- rapFigurer::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=paste0('variabel: ', valgtVar))	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {




    #-----------Figur---------------------------------------

    #Plottspesifikke parametre:
    FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NGERUtvalg$fargepalett)
    farger <- FigTypUt$farger
    fargeHoved <- farger[3]
    fargeRest <- farger[1]
    NutvTxt <- length(utvalgTxt)
    hmarg <- 0.04+0.01*NutvTxt
    par('fig' = c(0,1,0,1-hmarg))
    cexleg <- 1	#Størrelse på legendtekst
    cexskala <- switch(tidsenhet, Aar=1, Mnd=0.9)
    xskala <- 1:length(tidtxt)
    ymax <- min(119, 1.25*max(c(AggVerdier$Hoved, AggVerdier$Rest), na.rm=T))

    plot(AggVerdier$Hoved,  font.main=1,  type='o', pch="'", col=fargeHoved, xaxt='n',
         frame.plot = FALSE,  xlim = c(1,length(tidtxt)),
         cex=2, lwd=3, xlab=xAkseTxt, ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i')

    axis(side=1, at = xskala, labels = tidtxt, cex.axis=cexskala) #cex.axis=0.9)
    title(NGERVarSpes$tittel, line=1, font.main=1)
    text(xskala, AggVerdier$Hoved, pos=3, NTidHendHoved, cex=0.9, col=fargeHoved)#pos=1,

    #Legge på linjer i plottet.
    grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")


    Ttxt <- paste0('(Tall ved punktene angir antall ', NGERVarSpes$varTxt, ')')
    if (medSml == 1) {
      lines(xskala, AggVerdier$Rest, col=fargeRest, lwd=3)
      points(xskala, AggVerdier$Rest, pch="'", cex=2, col=fargeRest)	#}
      text(xskala, AggVerdier$Rest, pos=3, NTidHendRest, cex=0.9, col=fargeRest)
      legend('topleft', border=NA, c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'),
                                     paste0(NGERUtvalg$smltxt, ' (N=', N$Rest, ')'), Ttxt), bty='n', ncol=1, cex=cexleg,
             col=c(fargeHoved, fargeRest, NA), lwd=3)
    } else {
      legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'), Ttxt),
             col=c(fargeHoved, NA), lwd=3, bty='n')
    }


    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
    #------------------------------------------------------------------------------

  }	#end else statement
  return(invisible(FigDataParam))
}	#end function



